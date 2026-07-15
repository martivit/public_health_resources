#' IPHRA Nutrition Data Analytics Class
#'
#' The `NutritionDataAnalytics` R6 class extends `DataAnalytics` for
#' Nutrition-specific quantitative analysis, including anthropometric data.
#'
#' @description
#' This class provides Nutrition-specific (including anthropometric) functionality:
#' * Quantitative analysis indicators via analysis_schema
#' * Quality checks via two separate schemas:
#'   - Anthropometric plausibility (`quality_schema_anthro`)
#'   - IYCF plausibility (`quality_schema_iycf`)
#' * All visualizations/tables via outputs_schema
#' * Optional MUAC age-adjusted analysis via \code{post_run_analysis(muac_age_weights = TRUE)}
#'
#' @field quality_schema_anthro Quality check schema for anthropometric data
#' @field quality_schema_iycf Quality check schema for IYCF data
#' @field plausibility_results_anthro Results of anthropometric quality checks
#' @field plausibility_results_iycf Results of IYCF quality checks
#'
#' @seealso [DataAnalytics]
#' @export
NutritionDataAnalytics <- R6::R6Class(
  classname = "NutritionDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Nutrition-specific quality schema fields
    quality_schema_anthro    = NULL,
    quality_schema_iycf      = NULL,

    # Nutrition-specific plausibility result fields
    plausibility_results_anthro = NULL,
    plausibility_results_iycf   = NULL,

    #' @description
    #' Initialize a new NutritionDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean nutrition data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @return A new NutritionDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "NutritionDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL) {

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = variable_map,
        value_map = value_map,
        variable_label = variable_label,
        value_label = value_label
      )

      # Load the two nutrition-specific quality schemas
      self$quality_schema_anthro       <- self$default_quality_anthro_schema()
      self$quality_schema_iycf         <- self$default_quality_iycf_schema()

      # Initialize separate result containers
      self$plausibility_results_anthro <- list()
      self$plausibility_results_iycf   <- list()

      phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as NutritionDataAnalytics object."))
      )
    },

    #' @description
    #' Post-analysis hook for MUAC age-adjustment.
    #'
    #' Called automatically at the end of \code{run_analysis()}.  When
    #' \code{muac_age_weights = TRUE} this method:
    #' \enumerate{
    #'   \item Filters the data analysis plan to rows whose \code{var_name}
    #'         contains \code{"muac"}.
    #'   \item If no such rows exist, returns silently.
    #'   \item Computes MUAC age-adjustment weights (expected proportion of
    #'         0–23 month children defaults to \code{1/3}; 24–59 month
    #'         children receive the complementary expected proportion \code{2/3}).
    #'   \item Multiplies the MUAC age-adjustment weight by the existing
    #'         survey weight (if present) to form a composite weight column.
    #'   \item Creates a temporary survey design object using the composite
    #'         weight.
    #'   \item Runs the filtered analysis plan against the modified design and
    #'         stores results in \code{self$analysis_results[["muac_weighted"]]}.
    #' }
    #'
    #' When \code{muac_age_weights = FALSE} (default) the method returns
    #' immediately without performing any additional analysis.
    #'
    #' @param muac_age_weights Logical (default \code{FALSE}).  Set to
    #'   \code{TRUE} to compute and apply MUAC age-adjustment weights.
    #' @param expected_prop_0_23 Numeric in (0, 1); expected proportion of
    #'   children aged 0–23 months among all 0–59 month children.
    #'   Defaults to \code{1/3} (i.e. 1/3 of children are expected to be 0–23
    #'   months and 2/3 are expected to be 24–59 months).
    #' @return Invisibly returns \code{self}.
    post_run_analysis = function(muac_age_weights = FALSE,
                                 expected_prop_0_23 = 1 / 3) {

      origin <- paste0(self$dataset_name, "$post_run_analysis")

      # ------------------------------------------------------------------
      # 1. Early exit when age-adjustment is not requested
      # ------------------------------------------------------------------
      if (!isTRUE(muac_age_weights)) {
        return(invisible(self))
      }

      phr_message(origin, "Running MUAC age-weighted post-analysis...")

      if (is.null(self$survey_design)) {
        phr_warning(message = "Survey design not set; skipping MUAC post-analysis.", origin = origin)
        return(invisible(self))
      }

      if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
        phr_warning(message = "No data_analysis_plan available; skipping MUAC post-analysis.", origin = origin)
        return(invisible(self))
      }

      # ------------------------------------------------------------------
      # 2. Filter the analysis plan to rows referencing any 'muac' variable
      # ------------------------------------------------------------------
      dap_full  <- self$data_analysis_plan$log_df
      muac_rows <- dap_full[grepl("muac", dap_full$var_name, ignore.case = TRUE), , drop = FALSE]

      if (nrow(muac_rows) == 0) {
        phr_message(origin, "No 'muac' variables found in analysis plan; skipping MUAC post-analysis.")
        return(invisible(self))
      }

      # ------------------------------------------------------------------
      # 3. Compute MUAC age-adjustment weights (local vector, not stored)
      # ------------------------------------------------------------------
      alt_weights <- private$.compute_weights_muac_alt(expected_prop_0_23)

      if (is.null(alt_weights)) {
        phr_warning(
          message = "Could not compute MUAC age-adjustment weights (age column missing or no eligible children). Skipping.",
          origin  = origin
        )
        return(invisible(self))
      }

      # ------------------------------------------------------------------
      # 4. Build composite weight: original_weight * weights_muac_alt
      #    Use survey_design$variables so we access the same weights that
      #    are active in the survey design.
      # ------------------------------------------------------------------
      sd_vars    <- self$survey_design$variables
      weight_col <- self$variable_map[["weight"]]
      if (!is.null(weight_col) && weight_col %in% names(sd_vars)) {
        composite_wt <- sd_vars[[weight_col]] * alt_weights
      } else {
        composite_wt <- alt_weights
      }

      tmp_col       <- ".muac_composite_weight"
      modified_data <- sd_vars
      modified_data[[tmp_col]] <- composite_wt

      # ------------------------------------------------------------------
      # 5. Build temporary survey design with the composite weight
      # ------------------------------------------------------------------
      muac_design <- private$.build_muac_survey_design(modified_data, tmp_col)

      if (is.null(muac_design)) {
        phr_warning(
          message = "Could not create MUAC-weighted survey design. Skipping.",
          origin  = origin
        )
        return(invisible(self))
      }

      # ------------------------------------------------------------------
      # 6. Run analysis and store under 'muac_weighted'
      # ------------------------------------------------------------------
      muac_results <- phr_try(
        phr_calc_survey_from_plan(
          design        = muac_design,
          analysis_plan = muac_rows
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Verify muac variables and composite weight column are valid."
      )

      self$analysis_results[["muac_weighted"]] <- muac_results

      phr_message(origin, glue::glue(
        "MUAC age-weighted post-analysis complete: {nrow(muac_rows)} indicator(s) stored under 'muac_weighted'."
      ))

      invisible(self)
    },

    #' @description
    #' Diagnose issues in both anthropometric and IYCF quality schemas.
    #'
    #' Runs the standard quality_diagnose logic on both \code{quality_schema_anthro}
    #' and \code{quality_schema_iycf} and returns a combined tibble with a
    #' \code{schema_type} column indicating which schema each check belongs to.
    #' Results are stored in \code{self$quality_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per quality check, covering
    #'   both anthropometric and IYCF schemas.
    quality_diagnose = function() {

      origin <- paste0(self$dataset_name, "$quality_diagnose")

      phr_try({

        empty_row <- tibble::tibble(
          schema_type         = character(),
          check_group         = character(),
          check_name          = character(),
          check_label         = character(),
          variables           = character(),
          statistical_test    = character(),
          test_params         = character(),
          n_thresholds        = integer(),
          variables_in_data   = logical(),
          missing_variables   = character(),
          function_available  = logical(),
          thresholds_valid    = logical(),
          status              = character()
        )

        data_cols <- if (!is.null(self$data)) names(self$data) else character(0)

        # Internal helper: diagnose one schema
        diagnose_schema <- function(schema, schema_label) {

          if (is.null(schema) || length(schema) == 0) {
            phr_warning(
              message = glue::glue("No {schema_label} quality schema defined. Skipping."),
              origin  = origin
            )
            return(empty_row)
          }

          rows <- list()

          for (check_name in names(schema)) {
            check <- schema[[check_name]]

            check_group      <- check$check_group      %||% NA_character_
            check_label      <- check$check_label      %||% NA_character_
            statistical_test <- check$statistical_test %||% NA_character_
            variables        <- check$variables        %||% character(0)
            test_params      <- check$test_params      %||% list()
            thresholds       <- check$thresholds       %||% list()

            mapped_vars  <- self$.translate_canonical_to_actual_vars(variables)
            missing_vars <- setdiff(mapped_vars, data_cols)
            vars_in_data <- length(missing_vars) == 0

            func_available <- FALSE
            if (!is.na(statistical_test) && nzchar(statistical_test)) {
              func_name <- paste0("quality_test_", statistical_test)
              if (requireNamespace("phr", quietly = TRUE)) {
                tryCatch({
                  ns <- asNamespace("phr")
                  func_available <- exists(func_name, envir = ns, mode = "function", inherits = FALSE)
                }, error = function(e) {})
              }
              if (!func_available) {
                tryCatch({
                  func_available <- exists(func_name, mode = "function", inherits = TRUE)
                }, error = function(e) {})
              }
            }

            thresholds_valid <- TRUE
            if (length(thresholds) > 0) {
              for (thr in thresholds) {
                expr_str <- thr$threshold_expression %||% thr$expression
                if (!is.null(expr_str) && !is.na(expr_str) && nzchar(expr_str)) {
                  parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
                  if (is.null(parsed)) {
                    thresholds_valid <- FALSE
                    break
                  }
                }
              }
            }

            issues <- character(0)
            if (!vars_in_data)     issues <- c(issues, paste0("missing variables: ", paste(missing_vars, collapse = ", ")))
            if (!func_available)   issues <- c(issues, paste0("function not found: quality_test_", statistical_test %||% "NA"))
            if (!thresholds_valid) issues <- c(issues, "invalid threshold expression(s)")
            status <- if (length(issues) == 0) "ok" else paste(issues, collapse = "; ")

            rows[[length(rows) + 1]] <- tibble::tibble(
              schema_type        = schema_label,
              check_group        = check_group,
              check_name         = check_name,
              check_label        = check_label,
              variables          = paste(variables, collapse = ", "),
              statistical_test   = statistical_test %||% NA_character_,
              test_params        = if (length(test_params) > 0) paste(names(test_params), test_params, sep = "=", collapse = ", ") else NA_character_,
              n_thresholds       = length(thresholds),
              variables_in_data  = vars_in_data,
              missing_variables  = if (length(missing_vars) > 0) paste(missing_vars, collapse = ", ") else NA_character_,
              function_available = func_available,
              thresholds_valid   = thresholds_valid,
              status             = status
            )
          }

          if (length(rows) > 0) dplyr::bind_rows(rows) else empty_row
        }

        anthro_result <- diagnose_schema(self$quality_schema_anthro, "anthropometric")
        iycf_result   <- diagnose_schema(self$quality_schema_iycf,   "iycf")

        result <- dplyr::bind_rows(anthro_result, iycf_result)
        self$quality_issues_log <- result

        n_issues <- sum(result$status != "ok", na.rm = TRUE)
        phr_message(phr_txt(glue::glue(
          "quality_diagnose complete: {nrow(result)} check(s) reviewed ({nrow(anthro_result)} anthropometric, {nrow(iycf_result)} IYCF), {n_issues} issue(s) found for {self$dataset_name}."
        )))

        invisible(result)

      }, on_error = "warn", origin = origin)
    },

    #' @description Load the default anthropometric quality schema from template file
    #' @return A list of anthropometric quality checks
    default_quality_anthro_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_anthropometric_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_anthropometric_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_quality_anthro_schema",
        hint     = "Check that quality_schema_data_quality_anthropometric_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Load the default IYCF quality schema from template file
    #' @return A list of IYCF quality checks
    default_quality_iycf_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_iycf_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_iycf_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_quality_iycf_schema",
        hint     = "Check that quality_schema_data_quality_iycf_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Run quality checks for both anthropometric and IYCF schemas
    #'
    #' Executes all checks defined in `quality_schema_anthro` and
    #' `quality_schema_iycf` separately, storing results in
    #' `plausibility_results_anthro` and `plausibility_results_iycf`
    #' respectively. Combined results are also written to `plausibility_results`
    #' so that inherited helpers (e.g. `calculate_overall_score`,
    #' `results_to_table`) continue to work as expected.
    #'
    #' @return A named list with elements `anthro` and `iycf`, each containing
    #'   the check results for that schema (invisibly)
    run_quality_checks = function() {

      phr_try({

        anthro_results <- private$.run_checks_for_schema(
          schema          = self$quality_schema_anthro,
          table_namespace = "plausibility_anthro",
          schema_label    = "Anthropometric"
        )

        iycf_results <- private$.run_checks_for_schema(
          schema          = self$quality_schema_iycf,
          table_namespace = "plausibility_iycf",
          schema_label    = "IYCF"
        )

        self$plausibility_results_anthro <- anthro_results
        self$plausibility_results_iycf   <- iycf_results

        # Combine into the inherited plausibility_results so that
        # calculate_overall_score() and results_to_table() still work
        self$plausibility_results <- c(anthro_results, iycf_results)
        self$calculate_overall_score()

        phr_message(
          phr_txt(glue::glue("Ran {length(anthro_results)} anthropometric and {length(iycf_results)} IYCF quality checks for {self$dataset_name}."))
        )

        invisible(list(anthro = anthro_results, iycf = iycf_results))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_quality_checks"))
    },

    #' @description Load the default Nutrition analysis schema from template file
    #' @return A tibble containing the Nutrition analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_nutrition_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Nutrition unified outputs schema from template file
    #' @return A list of Nutrition-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_nutrition_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  ),

  private = list(

    # Run all checks in `schema` and generate penalty tables under
    # `self$tables[[table_namespace]]`.  Returns the named results list.
    .run_checks_for_schema = function(schema, table_namespace, schema_label) {

      if (is.null(schema) || length(schema) == 0) {
        phr_warning(
          message = glue::glue("No quality checks defined in {schema_label} schema."),
          origin  = self$dataset_name
        )
        return(list())
      }

      results <- list()

      for (check_name in names(schema)) {
        check            <- schema[[check_name]]
        result           <- self$execute_check(check)
        results[[check_name]] <- result
      }

      # --- Penalty tables --------------------------------------------------
      if (is.null(self$tables[[table_namespace]])) {
        self$tables[[table_namespace]] <- list()
      }

      # Temporarily swap quality_schema so that inherited helpers
      # (results_to_table, .compute_results_by_group) operate on this schema
      original_schema  <- self$quality_schema
      original_results <- self$plausibility_results
      self$quality_schema      <- schema
      self$plausibility_results <- results
      on.exit({
        self$quality_schema       <- original_schema
        self$plausibility_results <- original_results
      }, add = TRUE)

      results_df  <- self$results_to_table()
      penalty_tbl <- table_quality_penalty_summary(
        results_df,
        title_name = glue::glue("{schema_label} Data Quality Penalty Summary")
      )
      if (!is.null(penalty_tbl)) {
        self$tables[[table_namespace]][["penalty_summary"]] <- penalty_tbl
      }

      for (role in c("enum_id", "stratum")) {
        col_name <- self$get_variable(role)
        if (!is.null(col_name) && nzchar(col_name) &&
            col_name %in% names(self$data)) {
          per_group_df <- self$.compute_results_by_group(col_name)
          if (!is.null(per_group_df) && nrow(per_group_df) > 0) {
            tbl_key     <- paste0("penalty_summary_by_", role)
            group_label <- if (role == "enum_id") "Enumerator ID" else "Stratum"
            tbl_title   <- if (role == "enum_id") {
              glue::glue("{schema_label} Data Quality Penalty Summary by Enumerator")
            } else {
              glue::glue("{schema_label} Data Quality Penalty Summary by Stratum")
            }
            per_group_tbl <- table_quality_penalty_summary_by_group(
              per_group_df,
              group_col   = "group_value",
              group_label = group_label,
              title_name  = tbl_title
            )
            if (!is.null(per_group_tbl)) {
              self$tables[[table_namespace]][[tbl_key]] <- per_group_tbl
            }

            group_values <- sort(unique(per_group_df$group_value))
            for (gv in group_values) {
              gv_label   <- as.character(gv)
              gv_safe    <- gsub("[^A-Za-z0-9]", "_", gv_label)
              gv_results <- per_group_df |>
              dplyr::filter(.data$group_value == gv) |>
                dplyr::select(-"group_value")
              gv_title <- if (role == "enum_id") {
                glue::glue("{schema_label} Data Quality Penalty Summary - Enumerator: {gv_label}")
              } else {
                glue::glue("{schema_label} Data Quality Penalty Summary - Stratum: {gv_label}")
              }
              gv_tbl_key <- paste0("penalty_summary_", role, "_", gv_safe)
              gv_tbl <- table_quality_penalty_summary(gv_results, title_name = gv_title)
              if (!is.null(gv_tbl)) {
                self$tables[[table_namespace]][[gv_tbl_key]] <- gv_tbl
              }
            }
          }
        }
      }

      results
    },

    # Compute MUAC age-adjustment weights as a numeric vector (not stored).
    #
    # For children aged 0-23 months:
    #   weights_muac_alt = expected_prop_0_23 / sample_prop_0_23
    # For children aged 24-59 months:
    #   weights_muac_alt = (1 - expected_prop_0_23) / sample_prop_24_59
    # All other rows receive NA.
    #
    # Data are sourced from self$survey_design$variables to ensure the same
    # data (and weights) used in the survey design are referenced.
    #
    # Returns the weight vector (same length as nrow(survey_design$variables))
    # or NULL if the survey design / age column is absent or there are no
    # eligible children.
    .compute_weights_muac_alt = function(expected_prop_0_23 = 1 / 3) {

      origin <- paste0(self$dataset_name, "$post_run_analysis$.compute_weights_muac_alt")

      if (is.null(self$survey_design)) return(NULL)

      sd_vars <- self$survey_design$variables

      # Resolve the age-in-months column via variable_map
      age_col <- self$variable_map[["age_months"]]
      if (is.null(age_col) || !age_col %in% names(sd_vars)) {
        phr_message(
          origin,
          "No age_months column found in survey_design$variables; MUAC age-adjustment weights cannot be computed."
        )
        return(NULL)
      }

      age_vec  <- suppressWarnings(as.numeric(sd_vars[[age_col]]))

      in_0_23  <- !is.na(age_vec) & age_vec >= 0  & age_vec < 24
      in_24_59 <- !is.na(age_vec) & age_vec >= 24 & age_vec < 60

      n_0_23  <- sum(in_0_23)
      n_24_59 <- sum(in_24_59)
      n_total <- n_0_23 + n_24_59

      if (n_total == 0) {
        phr_message(
          origin,
          "No children aged 0-59 months found; MUAC age-adjustment weights cannot be computed."
        )
        return(NULL)
      }

      sample_prop_0_23  <- n_0_23  / n_total
      sample_prop_24_59 <- n_24_59 / n_total

      expected_prop_24_59 <- 1 - expected_prop_0_23

      alt_weights <- rep(NA_real_, nrow(sd_vars))

      if (sample_prop_0_23 > 0) {
        alt_weights[in_0_23]  <- expected_prop_0_23  / sample_prop_0_23
      }
      if (sample_prop_24_59 > 0) {
        alt_weights[in_24_59] <- expected_prop_24_59 / sample_prop_24_59
      }

      phr_message(
        origin,
        glue::glue(
          "MUAC age-adjustment weights computed: {n_0_23} children 0-23 months ",
          "(sample prop: {round(sample_prop_0_23, 3)}, expected: {round(expected_prop_0_23, 3)}), ",
          "{n_24_59} children 24-59 months ",
          "(sample prop: {round(sample_prop_24_59, 3)}, expected: {round(expected_prop_24_59, 3)})."
        )
      )

      return(alt_weights)
    },

    # Build a temporary survey design using `weight_col` as the weight column.
    # Cluster, strata, and FPC are resolved from self$variable_map as usual.
    # Returns NULL on failure.
    .build_muac_survey_design = function(modified_data, weight_col) {

      origin <- paste0(self$dataset_name, "$post_run_analysis$.build_muac_survey_design")

      if (is.null(modified_data)) return(NULL)

      cluster_col <- self$variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% names(modified_data)) {
        cluster_col <- self$variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% names(modified_data)) {
        cluster_col <- NULL
      }

      strata_col <- self$variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% names(modified_data)) strata_col <- NULL

      fpc_col <- self$variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% names(modified_data)) fpc_col <- NULL

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- rlang::sym(weight_col)
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phr_try(
        srvyr::as_survey_design(
          .data   = modified_data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Check that the composite weight column and cluster/strata columns contain valid data."
      )

      return(design)
    }
  )
)
