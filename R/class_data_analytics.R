#' IPHRA DataAnalytics Class
#'
#' The `DataAnalytics` R6 class provides unified data quality checks and
#' quantitative analysis in a single analytics object. It maintains separate
#' schemas for quality checks and quantitative analysis, while sharing common
#' output containers.
#'
#' @description
#' This class provides:
#' * Data quality checks via `run_quality_checks()` using `quality_schema`
#' * Quantitative analysis via `run_analysis()` using `analysis_schema`
#' * All outputs (quality and analysis) via `run_outputs()` using `outputs_schema`
#' * Shared `visualizations` and `tables` containers for all outputs
#'
#' @details
#' Key design principles:
#' * `quality_schema` drives quality checks; `analysis_schema` drives analysis
#' * `outputs_schema` defines all visualizations/tables (quality and analysis combined)
#' * Each output entry uses `dataset_type` to specify the data source
#'   ("data" for quality outputs, "survey_design" for analysis outputs)
#' * `results` stores quality check results; `analysis_results` stores analysis results
#' * All outputs store into the same `visualizations` and `tables` fields
#'
#' @field data Data frame containing the dataset (standardized or clean data, not raw)
#' @field parent_data_object Reference to the parent Data object
#' @field dataset_name Character name for identifying this analytics object
#' @field data_stage_name Name of the data stage (e.g., "standardized", "clean")
#' @field data_hash Hash of the data from parent Data object
#' @field variable_map Variable mappings from Data object
#' @field value_map Value mappings from Data object
#' @field variable_label Named list of variable labels
#' @field value_label Named list of value labels
#' @field quality_schema Quality check schema with test definitions, thresholds, and penalty scores
#' @field analysis_schema Analysis schema (catalog of possible indicators)
#' @field outputs_schema Unified outputs schema for all visualizations and tables
#' @field visualizations List of ggplot2 graphics/visualizations (shared)
#' @field tables List of dataframes or formatted table objects (shared)
#' @field plausibility_results Results of quality checks execution
#' @field analysis_results Results of quantitative analysis (named list: survey_design, base)
#' @field data_analysis_plan QuantDataAnalysisPlanLog object holding the analysis plan
#' @field survey_design srvyr survey design object
#' @field base_survey_design Unfiltered base srvyr survey design object (used internally for analysis)
#' @field analysis_plan_issue_log Tibble of plan validation issues (internal)
#' @field quality_issues_log Tibble of quality schema diagnostic results from quality_diagnose()
#' @field analysis_plan_issues_log Tibble of analysis plan diagnostic results from analysis_diagnose()
#' @field outputs_issues_log Tibble of outputs schema diagnostic results from outputs_diagnose()
#' @field overall_score Overall data quality score
#' @field metadata Free-form metadata list
#'
#' @seealso [FSLDataAnalytics]
#' @export
DataAnalytics <- R6::R6Class(
  classname = "DataAnalytics",

  public = list(

    # Core fields (shared)
    data = NULL,
    parent_data_object = NULL,
    dataset_name = NULL,
    data_stage_name = NULL,
    data_hash = NULL,
    variable_map = NULL,
    value_map = NULL,
    variable_label = NULL,
    value_label = NULL,

    # Quality-specific fields
    quality_schema = NULL,
    plausibility_results = NULL,               # Quality check results
    overall_score = NULL,
    metadata = NULL,

    # Diagnostic logs
    quality_issues_log = NULL,
    analysis_plan_issues_log = NULL,
    outputs_issues_log = NULL,

    # Analysis-specific fields
    data_analysis_plan = NULL,
    survey_design = NULL,
    base_survey_design = NULL,
    analysis_results = NULL,      # Quantitative analysis results
    analysis_plan_issue_log = NULL,
    analysis_schema = NULL,

    # Unified outputs schema
    outputs_schema = NULL,

    # Shared output containers
    visualizations = NULL,
    tables = NULL,



    # Initialization


    #' @description
    #' Initialize a new DataAnalytics object
    #'
    #' @param data A data frame (standardized or clean data, not raw)
    #' @param dap Optional data analysis plan (tibble); auto-generated from
    #'   analysis_schema if not supplied
    #' @param parent_data_object The Data object that generated this analytics object
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @return A new DataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "DataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

      origin <- paste0(dataset_name, "$initialize")
      phr_message(origin, "Initializing DataAnalytics class...")

      # --- Common fields
      self$parent_data_object <- parent_data_object
      self$dataset_name       <- dataset_name
      self$data_stage_name    <- data_stage_name
      self$data_hash          <- data_hash
      self$variable_map       <- variable_map  %||% list()
      self$value_map          <- value_map     %||% list()
      self$variable_label     <- variable_label %||% list()
      self$value_label        <- value_label   %||% list()

      # --- Shared output containers
      self$visualizations         <- list()
      self$tables                 <- list()

      # --- Quality-specific initialization
      if (!is.null(data)) {
        phr_validate_dataframe(data, origin = origin, soft = FALSE)
        self$data <- data
      }

      self$plausibility_results       <- list()
      self$overall_score <- NA_real_

      # Diagnostic logs
      self$quality_issues_log          <- tibble::tibble()
      self$analysis_plan_issues_log    <- tibble::tibble()
      self$outputs_issues_log          <- tibble::tibble()
      self$metadata <- list(
        created_at     = Sys.time(),
        n_records      = if (!is.null(data)) nrow(data) else NULL,
        n_columns      = if (!is.null(data)) ncol(data) else NULL,
        parent_name    = if (!is.null(parent_data_object)) parent_data_object$dataset_name else NULL,
        data_stage_name = data_stage_name,
        data_hash      = data_hash
      )

      # Load or set quality schema
      if (!is.null(quality_schema)) {
        self$set_quality_schema(quality_schema)
      } else {
        self$quality_schema <- self$default_quality_schema()
      }

      # Load quality outputs schema
      self$outputs_schema <- self$default_outputs_schema()

      # --- Analysis-specific initialization
      self$analysis_results        <- list()
      self$analysis_plan_issue_log <- tibble::tibble()

      # 1. Load analysis schema
      self$analysis_schema <- self$default_analysis_schema()

      # 2. Create survey design objects from data + variable_map.
      #    base_survey_design: simple unweighted design (ids = 1, no weights/strata/clusters)
      #    survey_design: full weighted design considering clusters, weights, and strata
      if (!is.null(data)) {
        self$base_survey_design <- phr_try(
          srvyr::as_survey_design(.data = data, ids = 1),
          on_error = "warn",
          origin   = origin,
          hint     = "Could not create base (unweighted) survey design from data."
        )
        self$survey_design <- self$create_survey_design()
      }

      # 3. Populate data_analysis_plan
      if (!is.null(dap)) {
        self$data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df   = dap,
          log_name = "Quant Data Analysis Plan"
        )
      } else {
        self$data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df   = NULL,
          log_name = "Quant Data Analysis Plan"
        )
        if (!is.null(self$survey_design) || !is.null(self$data)) {
          self$generate_dap_from_schema()
        }
      }

      # 4. Load outputs schema (combined quality and analysis outputs)
      # (already loaded above, no separate analysis outputs schema needed)

      phr_message(origin, "Initialization complete.")
      invisible(self)
    },



    # Default schema loaders


    #' @description Load the default quality schema from template file
    #' @return A list of quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "DataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
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

    #' @description Load the default unified outputs schema from template file
    #' @return A list of outputs definitions (quality and analysis combined)
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "DataAnalytics$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_schema_data_analytics_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default analysis schema from template file
    #' @return A tibble containing the analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble(
            indicator_name = character(),
            calculation    = character(),
            var_name       = character(),
            denom_var      = character(),
            disaggregation = character(),
            multiplier     = numeric(),
            indicator_unit = character()
          ))
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "DataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble(
          indicator_name = character(),
          calculation    = character(),
          var_name       = character(),
          denom_var      = character(),
          disaggregation = character(),
          multiplier     = numeric(),
          indicator_unit = character()
        ))
      }

      return(schema_tbl)
    },



    # Quality Schema Management


    #' @description Set the quality check schema
    #' @param schema A list defining quality checks, thresholds, and penalties
    set_quality_schema = function(schema) {

      phr_try({
        self$validate_quality_schema(schema)
        self$quality_schema <- schema
        phr_message(
          phr_txt(glue::glue("Quality schema set for {self$dataset_name}."))
        )
      }, on_error = "abort", origin = paste0(self$dataset_name, "$set_quality_schema"))
    },

    #' @description Get the quality check schema
    #' @return The current quality schema
    get_quality_schema = function() {
      self$quality_schema
    },

    #' @description Validate the quality schema structure
    #' @param schema The schema to validate (a list of quality checks)
    #' @return TRUE if valid, throws error otherwise
    validate_quality_schema = function(schema) {

      phr_try({

        if (is.null(schema) || !is.list(schema)) {
          phr_error(
            message = "Quality schema must be a list.",
            origin  = self$dataset_name
          )
        }

        for (check_name in names(schema)) {
          check <- schema[[check_name]]

          if (!is.list(check)) {
            phr_error(
              message = phr_txt(glue::glue("Check '{check_name}' must be a list.")),
              origin  = self$dataset_name
            )
          }

          required_fields <- c("check_name", "check_label", "statistical_test")
          missing <- setdiff(required_fields, names(check))

          if (length(missing) > 0) {
            phr_warning(
              message = phr_txt(glue::glue(
                "Check '{check_name}' is missing recommended fields: {paste(missing, collapse=', ')}."
              )),
              origin = self$dataset_name
            )
          }
        }

        invisible(TRUE)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$validate_quality_schema"))
    },

    #' @description Set the unified outputs schema
    #' @param schema A named list defining output specifications (quality and analysis combined)
    #' @return Invisibly returns self.
    set_outputs_schema = function(schema) {

      phr_try({
        outputs_validate_schema_to_table(
          outputs_schema = schema,
          origin = paste0(self$dataset_name, "$set_outputs_schema")
        )
        tbl <- outputs_schema_to_table(schema)
        outputs_validate_table_to_schema(df = tbl)
        self$outputs_schema <- schema
        phr_message(
          phr_txt(glue::glue("Outputs schema set for {self$dataset_name}."))
        )
      }, on_error = "abort", origin = paste0(self$dataset_name, "$set_outputs_schema"))

      invisible(self)
    },

    #' @description Get the unified outputs schema
    #' @return The current outputs schema (named list), or NULL.
    get_outputs_schema = function() {
      self$outputs_schema
    },

    #' @description Export outputs schema to a data frame
    #' @return A tibble representing the outputs schema, or NULL
    export_outputs_schema = function() {

      phr_try({
        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(
            phr_txt(glue::glue("No outputs schema defined for {self$dataset_name}.")),
            origin = self$dataset_name
          )
          return(NULL)
        }
        outputs_schema_to_table(self$outputs_schema)
      }, on_error = "abort", origin = paste0(self$dataset_name, "$export_outputs_schema"))
    },

    #' @description Import outputs schema from a data frame
    #' @param df A data frame representing the outputs schema
    #' @return The imported schema (invisibly)
    import_outputs_schema = function(df) {

      phr_try({
        phr_validate_dataframe(df, origin = "import_outputs_schema", soft = FALSE)
        new_schema <- outputs_table_to_schema(df)
        self$outputs_schema <- new_schema
        phr_message(
          phr_txt(glue::glue(
            "Outputs schema imported for {self$dataset_name} ({length(new_schema)} output(s))."
          ))
        )
        invisible(new_schema)
      }, on_error = "abort", origin = paste0(self$dataset_name, "$import_outputs_schema"))
    },

    #' @description Export quality schema to a file
    #' @param path Character; destination file path (including extension).
    #' @param format Character; "xlsx" (default) or "csv".
    #' @return Invisibly returns self.
    export_quality_schema = function(path, format = "xlsx") {
      origin <- paste0(self$dataset_name, "$export_quality_schema")

      phr_try({
        if (is.null(self$quality_schema) || length(self$quality_schema) == 0) {
          phr_warning(origin, "No quality schema to export.")
          return(invisible(self))
        }
        tbl <- quality_schema_to_table(list(checks = self$quality_schema))
        phr_message(origin, paste("Exporting quality schema to:", path))
        if (format == "xlsx") {
          openxlsx::write.xlsx(tbl, path)
        } else if (format == "csv") {
          readr::write_csv(tbl, path)
        } else {
          phr_warning(origin, paste("Unsupported export format:", format))
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check file path and permissions.")

      invisible(self)
    },

    #' @description Import quality schema from a file
    #' @param path Character; path to the schema file (.xlsx, .csv, or .rds).
    #' @return Invisibly returns self.
    import_quality_schema = function(path) {
      origin <- paste0(self$dataset_name, "$import_quality_schema")
      phr_message(origin, paste("Importing quality schema from:", path))

      phr_try({
        if (!file.exists(path)) {
          phr_error(origin, paste("File not found:", path))
          return(invisible(self))
        }

        ext <- tools::file_ext(path)
        schema_tbl <- switch(
          ext,
          "csv"  = readr::read_csv(path, show_col_types = FALSE),
          "xlsx" = readxl::read_xlsx(path),
          "rds"  = readRDS(path),
          {
            phr_error(origin, paste("Unsupported file type:", ext))
            return(invisible(self))
          }
        )

        parsed <- quality_table_to_schema(schema_tbl)
        if (!is.null(parsed$checks)) {
          self$quality_schema <- parsed$checks
          phr_message(origin, paste("Quality schema imported with", length(parsed$checks), "check(s)."))
        } else {
          phr_warning(origin, "Imported file did not produce a valid quality schema.")
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure the file is a valid xlsx, csv, or rds with the required quality schema columns.")

      invisible(self)
    },

    #' @description Set the analysis schema
    #' @param schema A tibble with analysis schema columns (indicator_name, calculation, var_name, etc.)
    #' @return Invisibly returns self.
    set_analysis_schema = function(schema) {
      origin <- paste0(self$dataset_name, "$set_analysis_schema")

      phr_try({
        if (!is.data.frame(schema)) {
          phr_error(origin, "Analysis schema must be a data frame or tibble.")
        }
        self$analysis_schema <- schema
        phr_message(origin, paste("Analysis schema set with", nrow(schema), "row(s)."))
      }, on_error = "abort", origin = origin)

      invisible(self)
    },

    #' @description Get the analysis schema
    #' @return The current analysis schema tibble, or NULL.
    get_analysis_schema = function() {
      self$analysis_schema
    },

    # Quality Checks

    #' @description Run all quality checks defined in the quality schema
    #' @return A list of check results (invisibly)
    run_quality_checks = function() {

      phr_try({

        if (is.null(self$quality_schema) || length(self$quality_schema) == 0) {
          phr_warning(
            message = "No quality checks defined in schema.",
            origin  = self$dataset_name
          )
          return(invisible(list()))
        }

        results <- list()

        for (check_name in names(self$quality_schema)) {
          check  <- self$quality_schema[[check_name]]
          result <- self$execute_check(check)
          results[[check_name]] <- result
        }

        self$plausibility_results <- results
        self$calculate_overall_score()

        # --- Plausibility tables
        if (is.null(self$tables[["plausibility"]])) {
          self$tables[["plausibility"]] <- list()
        }

        results_df  <- self$results_to_table()
        penalty_tbl <- table_quality_penalty_summary(
          results_df,
          title_name = "Data Quality Penalty Summary"
        )
        if (!is.null(penalty_tbl)) {
          self$tables[["plausibility"]][["penalty_summary"]] <- penalty_tbl
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
                "Data Quality Penalty Summary by Enumerator"
              } else {
                "Data Quality Penalty Summary by Stratum"
              }
              per_group_tbl <- table_quality_penalty_summary_by_group(
                per_group_df,
                group_col   = "group_value",
                group_label = group_label,
                title_name  = tbl_title
              )
              if (!is.null(per_group_tbl)) {
                self$tables[["plausibility"]][[tbl_key]] <- per_group_tbl
              }

              group_values <- sort(unique(per_group_df$group_value))
              for (gv in group_values) {
                gv_label   <- as.character(gv)
                gv_safe    <- gsub("[^A-Za-z0-9]", "_", gv_label)
                gv_results <- per_group_df |>
                  dplyr::filter(.data$group_value == gv) |>
                  dplyr::select(-"group_value")
                gv_title <- if (role == "enum_id") {
                  paste0("Data Quality Penalty Summary - Enumerator: ", gv_label)
                } else {
                  paste0("Data Quality Penalty Summary - Stratum: ", gv_label)
                }
                gv_tbl_key <- paste0("penalty_summary_", role, "_", gv_safe)
                gv_tbl <- table_quality_penalty_summary(gv_results, title_name = gv_title)
                if (!is.null(gv_tbl)) {
                  self$tables[["plausibility"]][[gv_tbl_key]] <- gv_tbl
                }
              }
            }
          }
        }

        phr_message(
          phr_txt(glue::glue("Ran {length(results)} quality checks for {self$dataset_name}."))
        )

        invisible(results)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_quality_checks"))
    },

    #' @description Execute a single quality check using statistical tests
    #' @param check A single check definition from the schema
    #' @param survey_design Optional srvyr survey design to use. Defaults to
    #'   \code{self$base_survey_design} (or \code{self$survey_design} as fallback)
    #'   when \code{NULL}.
    #' @return A list containing the check result
    execute_check = function(check, survey_design = NULL) {

      phr_try({

        result <- list(
          check_name            = check$check_name     %||% "unknown",
          check_label           = check$check_label    %||% "",
          check_group           = check$check_group    %||% NA_character_,
          test_statistic        = NA,
          p_value               = NA,
          penalty               = 0,
          max_penalty           = 0,
          threshold_expression  = NA_character_,
          message               = ""
        )

        test_name   <- check$statistical_test
        variables   <- check$variables
        thresholds  <- check$thresholds
        test_params <- check$test_params

        if (is.null(variables) || length(variables) == 0) {
          result$message <- "No variables specified"
          return(result)
        }

        mapped_vars    <- self$.translate_canonical_to_actual_vars(variables)
        available_vars <- intersect(mapped_vars, names(self$data))
        if (length(available_vars) != length(mapped_vars)) {
          missing_vars   <- setdiff(mapped_vars, available_vars)
          result$message <- paste0("Required variables not found in data: ", paste(missing_vars, collapse = ", "))
          return(result)
        }

        test_func_name <- paste0("quality_test_", test_name)
        test_function  <- NULL

        if (requireNamespace("phr", quietly = TRUE)) {
          tryCatch({
            ns <- asNamespace("phr")
            if (exists(test_func_name, envir = ns, mode = "function", inherits = FALSE)) {
              test_function <- get(test_func_name, envir = ns, mode = "function", inherits = FALSE)
            }
          }, error = function(e) {})
        }

        if (is.null(test_function)) {
          tryCatch({
            if (exists(test_func_name, mode = "function", inherits = TRUE)) {
              test_function <- get(test_func_name, mode = "function", inherits = TRUE)
            }
          }, error = function(e) {})
        }

        if (is.null(test_function)) {
          result$message <- paste0("Unknown test: ", test_name, " (function ", test_func_name, " not found)")
          return(result)
        }

        effective_design <- survey_design %||% self$base_survey_design %||% self$survey_design
        test_args   <- list(survey_design = effective_design, variables = available_vars)
        if (!is.null(test_params)) {
          test_args <- private$.resolve_output_params(
            func_args           = test_args,
            test_params         = test_params,
            out_name            = check$check_name %||% "unknown",
            skip_results_table  = TRUE
          )
        }

        test_result <- do.call(test_function, test_args)

        if (is.null(test_result)) {
          result$message <- paste0("Test function '", test_func_name, "' returned NULL (internal error occurred)")
          return(result)
        }

        if (is.list(test_result) && "statistic" %in% names(test_result)) {
          result$test_statistic <- test_result$statistic
          if ("p_value" %in% names(test_result)) {
            result$p_value <- test_result$p_value
          }
        } else {
          if (!is.null(test_result)) {
            result$test_statistic <- test_result
          }
        }

        if (!is.null(thresholds) && length(thresholds) > 0) {
          penalty_result           <- self$evaluate_threshold_expressions(
            test_statistic = result$test_statistic,
            p_value        = result$p_value,
            thresholds     = thresholds
          )
          result$penalty               <- penalty_result$penalty
          result$max_penalty           <- penalty_result$max_penalty
          result$threshold_expression  <- penalty_result$threshold_expression
          result$message               <- penalty_result$message
        }

        return(result)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$execute_check"))
    },

    #' @description Evaluate threshold expressions to determine penalty
    #' @param test_statistic The test statistic value
    #' @param p_value The p-value (if applicable)
    #' @param thresholds List of threshold definitions with expression and penalty
    #' @return List with penalty, max_penalty, threshold_expression, and message
    evaluate_threshold_expressions = function(test_statistic, p_value, thresholds) {

      result <- list(
        penalty              = 0,
        max_penalty          = 0,
        threshold_expression = "none",
        message              = ""
      )

      if (is.null(thresholds) || length(thresholds) == 0) {
        return(result)
      }

      all_penalties  <- sapply(thresholds, function(x) x$penalty %||% x$penalty_score %||% 0)
      result$max_penalty <- max(all_penalties, na.rm = TRUE)

      for (i in seq_along(thresholds)) {
        threshold_def <- thresholds[[i]]
        expression    <- threshold_def$expression %||% threshold_def$threshold_expression
        penalty       <- threshold_def$penalty    %||% threshold_def$penalty_score %||% 0

        if (is.null(expression) || is.na(expression) || !nzchar(expression)) next

        phr_try({
          eval_env               <- new.env()
          eval_env$test_statistic <- test_statistic
          eval_env$p_value       <- p_value
          meets_threshold        <- eval(parse(text = expression), envir = eval_env)

          if (isTRUE(meets_threshold)) {
            result$penalty               <- penalty
            result$threshold_expression  <- expression
            result$message <- paste0(
              "Threshold met: ", expression,
              " (test_statistic=", round(test_statistic, 3),
              if (!is.na(p_value)) paste0(", p_value=", round(p_value, 4)) else "",
              "), penalty=", penalty
            )
            break
          }
        }, on_error = "warn", origin = "evaluate_threshold_expressions")
      }

      if (result$threshold_expression == "none") {
        result$message <- paste0(
          "No threshold met (test_statistic=", round(test_statistic, 3),
          if (!is.na(p_value)) paste0(", p_value=", round(p_value, 4)) else "",
          ")"
        )
      }

      return(result)
    },

    #' @description Calculate the overall quality score based on penalties
    #' @return The overall quality score (0-100)
    calculate_overall_score = function() {

      phr_try({
        if (length(self$plausibility_results) == 0) {
          self$overall_score <- NA_real_
          return(NA_real_)
        }

        total_penalty <- 0
        max_penalty   <- 0

        for (result in self$plausibility_results) {
          if (!is.null(result$penalty))     total_penalty <- total_penalty + result$penalty
          if (!is.null(result$max_penalty)) max_penalty   <- max_penalty   + result$max_penalty
        }

        self$overall_score <- if (max_penalty > 0) {
          max(0, 100 - (total_penalty / max_penalty * 100))
        } else {
          100
        }

        invisible(self$overall_score)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$calculate_overall_score"))
    },

    #' @description Get a summary of the analytics assessment
    #' @return A list with summary information
    summary = function() {
      list(
        dataset_name  = self$dataset_name,
        n_records     = if (!is.null(self$data)) nrow(self$data) else NULL,
        n_columns     = if (!is.null(self$data)) ncol(self$data) else NULL,
        n_checks      = length(self$quality_schema),
        n_results     = length(self$plausibility_results),
        overall_score = self$overall_score,
        parent_object = if (!is.null(self$parent_data_object)) self$parent_data_object$dataset_name else NULL,
        created_at    = self$metadata$created_at
      )
    },

    #' @description Convert quality results to a tabular format
    #' @return A tibble with quality check results
    results_to_table = function() {

      phr_try({
        if (length(self$plausibility_results) == 0) {
          return(tibble::tibble(
            check_name           = character(),
            check_label          = character(),
            check_group          = character(),
            test_statistic       = numeric(),
            p_value              = numeric(),
            penalty              = numeric(),
            max_penalty          = numeric(),
            threshold_expression = character(),
            message              = character()
          ))
        }

        rows <- lapply(names(self$plausibility_results), function(nm) {
          r <- self$plausibility_results[[nm]]
          tibble::tibble(
            check_name           = r$check_name           %||% nm,
            check_label          = r$check_label          %||% NA_character_,
            check_group          = r$check_group          %||% NA_character_,
            test_statistic       = as.numeric(r$test_statistic %||% NA),
            p_value              = as.numeric(r$p_value        %||% NA),
            penalty              = as.numeric(r$penalty        %||% 0),
            max_penalty          = as.numeric(r$max_penalty    %||% 0),
            threshold_expression = r$threshold_expression %||% NA_character_,
            message              = r$message              %||% NA_character_
          )
        })

        dplyr::bind_rows(rows)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$results_to_table"))
    },

    #' @description Convert quality schema to a tabular format
    #' @return A tibble representing the quality schema
    schema_to_table = function() {
      quality_schema_to_table(self$quality_schema)
    },

    #' @description Import quality schema from a table
    #' @param df A data frame representing the quality schema
    import_schema_from_table = function(df) {

      phr_try({
        schema <- quality_table_to_schema(df)
        self$set_quality_schema(schema)
        phr_message(
          phr_txt(glue::glue("Imported quality schema from table for {self$dataset_name}."))
        )
        invisible(TRUE)
      }, on_error = "abort", origin = paste0(self$dataset_name, "$import_schema_from_table"))
    },

    #' @description Placeholder visualization method (to be overridden by subclasses)
    #' @param type The type of visualization
    #' @return A plot object or NULL
    visualize = function(type = "summary") {
      phr_message(
        phr_txt("Visualization not implemented in base DataAnalytics class.")
      )
      invisible(NULL)
    },

    #' @description Get the actual column name for a canonical variable role
    #' @param role Character string with canonical variable name (semantic role)
    #' @return Character string with actual column name, or NULL if role not mapped
    get_variable = function(role) {
      if (is.null(role) || length(role) == 0) {
        return(NULL)
      }
      if (!is.null(self$variable_map) && role %in% names(self$variable_map)) {
        return(self$variable_map[[role]])
      }
      return(NULL)
    },

    #' @description Translate canonical variable names to actual column names using variable_map
    #' @param canonical_vars Character vector of canonical variable names
    #' @return Character vector of actual column names
    .translate_canonical_to_actual_vars = function(canonical_vars) {

      if (is.null(canonical_vars) || length(canonical_vars) == 0) {
        return(character(0))
      }

      if (is.null(self$variable_map) || length(self$variable_map) == 0) {
        return(canonical_vars)
      }

      actual_vars <- sapply(canonical_vars, function(canonical_name) {
        actual_name <- self$get_variable(canonical_name)
        if (!is.null(actual_name) && nzchar(actual_name)) actual_name else canonical_name
      }, USE.NAMES = FALSE)

      return(actual_vars)
    },

    #' @description Run quality checks on data subsets for each unique value of a grouping column
    #' @param group_col Character string. The actual column name in self$data to group by.
    #' @return A tibble with per-group check results, or NULL
    .compute_results_by_group = function(group_col) {

      phr_try({

        if (is.null(group_col) || !nzchar(group_col) ||
            !group_col %in% names(self$data)) {
          return(NULL)
        }

        group_values <- unique(self$data[[group_col]])
        group_values <- group_values[!is.na(group_values)]

        if (length(group_values) == 0) {
          return(NULL)
        }

        full_data <- self$data

        all_group_results <- list()

        for (gv in group_values) {
          subset_data   <- full_data[full_data[[group_col]] == gv, ]
          subset_design <- phr_try(
            srvyr::as_survey_design(.data = subset_data, ids = 1),
            on_error = "warn",
            origin   = paste0(self$dataset_name, "$compute_results_by_group"),
            hint     = paste0("Could not create survey design for group: ", gv)
          )

          group_rows <- lapply(names(self$quality_schema), function(check_name) {
            check  <- self$quality_schema[[check_name]]
            result <- self$execute_check(check, survey_design = subset_design)
            tibble::tibble(
              group_value    = as.character(gv),
              check_name     = result$check_name     %||% check_name,
              check_label    = result$check_label    %||% NA_character_,
              check_group    = result$check_group    %||% NA_character_,
              test_statistic = as.numeric(result$test_statistic %||% NA),
              p_value        = as.numeric(result$p_value        %||% NA),
              penalty        = as.numeric(result$penalty        %||% 0),
              max_penalty    = as.numeric(result$max_penalty    %||% 0)
            )
          })

          all_group_results <- c(all_group_results, group_rows)
        }

        dplyr::bind_rows(all_group_results)

      }, on_error = "warn",
         origin   = paste0(self$dataset_name, "$compute_results_by_group"))
    },

    #' @description Generate a quality report
    #' @return A list containing the full quality report
    generate_report = function() {

      phr_try({
        if (length(self$plausibility_results) == 0) self$run_quality_checks()

        report <- list(
          summary       = self$summary(),
          results       = self$plausibility_results,
          results_table = self$results_to_table(),
          overall_score = self$overall_score,
          metadata      = self$metadata,
          generated_at  = Sys.time()
        )

        phr_message(
          phr_txt(glue::glue("Generated quality report for {self$dataset_name}."))
        )

        report

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_report"))
    },

    #' @description Generate a plausibility report with statistical test results
    #' @return A list containing detailed plausibility assessment
    generate_plausibility_report = function() {

      phr_try({
        if (length(self$plausibility_results) == 0) self$run_quality_checks()

        test_summary <- data.frame(
          check_name           = character(),
          check_label          = character(),
          test_statistic       = numeric(),
          p_value              = numeric(),
          threshold_expression = character(),
          penalty              = numeric(),
          max_penalty          = numeric(),
          message              = character(),
          stringsAsFactors     = FALSE
        )

        for (check_name in names(self$plausibility_results)) {
          result <- self$plausibility_results[[check_name]]
          test_summary <- rbind(test_summary, data.frame(
            check_name           = result$check_name           %||% check_name,
            check_label          = result$check_label          %||% "",
            test_statistic       = result$test_statistic       %||% NA_real_,
            p_value              = result$p_value              %||% NA_real_,
            threshold_expression = result$threshold_expression %||% "none",
            penalty              = result$penalty              %||% 0,
            max_penalty          = result$max_penalty          %||% 0,
            message              = result$message              %||% "",
            stringsAsFactors     = FALSE
          ))
        }

        total_checks      <- nrow(test_summary)
        checks_passed     <- sum(test_summary$penalty == 0, na.rm = TRUE)
        total_penalty     <- sum(test_summary$penalty, na.rm = TRUE)
        total_max_penalty <- sum(test_summary$max_penalty, na.rm = TRUE)

        plausibility_score <- if (total_max_penalty > 0) {
          max(0, 100 - (total_penalty / total_max_penalty * 100))
        } else {
          100
        }

        report <- list(
          dataset_name          = self$dataset_name,
          n_checks              = total_checks,
          n_checks_passed       = checks_passed,
          plausibility_score    = round(plausibility_score, 2),
          total_penalty         = total_penalty,
          max_possible_penalty  = total_max_penalty,
          threshold_distribution = as.list(table(test_summary$threshold_expression)),
          test_results          = test_summary,
          generated_at          = Sys.time(),
          schema_version        = self$quality_schema$metadata$version %||% "3.0.0"
        )

        phr_message(
          phr_txt(glue::glue(
            "Generated plausibility report for {self$dataset_name}. Score: {round(plausibility_score, 2)}"
          ))
        )

        return(report)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_plausibility_report"))
    },


    # Diagnose Methods


    #' @description
    #' Diagnose issues in the quality schema against the current dataset.
    #'
    #' For every check defined in \code{self$quality_schema}, this method
    #' verifies that (a) the required variables exist in \code{self$data},
    #' (b) the statistical test function can be found, and (c) each
    #' threshold expression is syntactically valid. The results mirror the
    #' flat quality schema table and add a \code{status} column ("ok" or a
    #' description of the issue).  Results are stored in
    #' \code{self$quality_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per quality check.
    quality_diagnose = function() {

      origin <- paste0(self$dataset_name, "$quality_diagnose")

      phr_try({

        if (is.null(self$quality_schema) || length(self$quality_schema) == 0) {
          phr_warning(
            message = "No quality_schema defined. Cannot diagnose.",
            origin  = origin
          )
          empty <- tibble::tibble(
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
          self$quality_issues_log <- empty
          return(invisible(empty))
        }

        data_cols <- if (!is.null(self$data)) names(self$data) else character(0)
        rows <- list()

        for (check_name in names(self$quality_schema)) {
          check <- self$quality_schema[[check_name]]

          # --- basic fields
          check_group      <- check$check_group      %||% NA_character_
          check_label      <- check$check_label      %||% NA_character_
          statistical_test <- check$statistical_test %||% NA_character_
          variables        <- check$variables        %||% character(0)
          test_params      <- check$test_params      %||% list()
          thresholds       <- check$thresholds       %||% list()

          # --- 1. variables present in data
          mapped_vars <- self$.translate_canonical_to_actual_vars(variables)
          missing_vars <- setdiff(mapped_vars, data_cols)
          vars_in_data <- length(missing_vars) == 0

          # --- 2. test function available
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

          # --- 3. threshold expressions parseable
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

          # --- build status string
          issues <- character(0)
          if (!vars_in_data)     issues <- c(issues, paste0("missing variables: ", paste(missing_vars, collapse = ", ")))
          if (!func_available)   issues <- c(issues, paste0("function not found: quality_test_", statistical_test %||% "NA"))
          if (!thresholds_valid) issues <- c(issues, "invalid threshold expression(s)")
          status <- if (length(issues) == 0) "ok" else paste(issues, collapse = "; ")

          rows[[length(rows) + 1]] <- tibble::tibble(
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

        result <- if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(
          check_group = character(), check_name = character(), check_label = character(),
          variables = character(), statistical_test = character(), test_params = character(),
          n_thresholds = integer(), variables_in_data = logical(),
          missing_variables = character(), function_available = logical(),
          thresholds_valid = logical(), status = character()
        )

        self$quality_issues_log <- result

        n_issues <- sum(result$status != "ok", na.rm = TRUE)
        phr_message(phr_txt(glue::glue(
          "quality_diagnose complete: {nrow(result)} check(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
        )))

        invisible(result)

      }, on_error = "warn", origin = origin)
    },

    #' @description
    #' Diagnose issues in the data analysis plan against the current dataset.
    #'
    #' For every indicator row in \code{self$data_analysis_plan$log_df}, this
    #' method verifies that the primary variable (\code{var_name}), the
    #' denominator variable (\code{denom_var}), and the disaggregation variable
    #' exist in the dataset (or survey design), and that the \code{calculation}
    #' type is valid. The results mirror the analysis plan tibble and add
    #' diagnostic columns. Results are stored in
    #' \code{self$analysis_plan_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per analysis plan indicator.
    analysis_diagnose = function() {

      origin <- paste0(self$dataset_name, "$analysis_diagnose")

      valid_calcs <- c("prop", "mean", "median", "ratio", "cat", "categorical", "select_multiple_cat")

      empty_result <- tibble::tibble(
        indicator_name          = character(),
        calculation             = character(),
        var_name                = character(),
        denom_var               = character(),
        disaggregation          = character(),
        multiplier              = numeric(),
        indicator_unit          = character(),
        var_name_in_data        = logical(),
        denom_var_in_data       = logical(),
        disaggregation_in_data  = logical(),
        calculation_valid       = logical(),
        status                  = character()
      )

      phr_try({

        schema_df <- if (!is.null(self$analysis_schema)) self$analysis_schema else NULL

        if (is.null(schema_df) || nrow(schema_df) == 0) {
          phr_warning(
            message = "No analysis schema defined. Cannot diagnose.",
            origin  = origin
          )
          self$analysis_plan_issues_log <- empty_result
          return(invisible(empty_result))
        }

        available_vars <- if (!is.null(self$survey_design)) {
          names(self$survey_design$variables)
        } else if (!is.null(self$data)) {
          names(self$data)
        } else {
          character(0)
        }

        rows <- lapply(seq_len(nrow(schema_df)), function(i) {
          row <- schema_df[i, ]

          # Helper: resolve a canonical name through variable_map and check availability
          resolve_var <- function(val) {
            v <- val %||% NA_character_
            if (is.na(v) || !nzchar(v)) return(v)
            resolved <- self$.translate_canonical_to_actual_vars(v)
            if (length(resolved) > 0) resolved[[1]] else v
          }

          # Helper: an optional variable field is ok if blank/NA, or if the col exists in data
          optional_var_ok <- function(val) {
            v <- val %||% NA_character_
            is.na(v) || !nzchar(v) || resolve_var(v) %in% available_vars
          }

          resolved_var_name <- resolve_var(row$var_name)
          var_name_ok  <- !is.na(row$var_name) && nzchar(row$var_name) && resolved_var_name %in% available_vars
          denom_ok     <- optional_var_ok(row$denom_var)
          disagg_ok    <- optional_var_ok(row$disaggregation)
          calc_valid   <- !is.na(row$calculation) && row$calculation %in% valid_calcs

          issues <- character(0)
          if (!var_name_ok)  issues <- c(issues, paste0("var_name '", row$var_name, "' not found in data"))
          if (!denom_ok)     issues <- c(issues, paste0("denom_var '", row$denom_var, "' not found in data"))
          if (!disagg_ok)    issues <- c(issues, paste0("disaggregation '", row$disaggregation, "' not found in data"))
          if (!calc_valid)   issues <- c(issues, paste0("invalid calculation '", row$calculation %||% "NA", "'"))
          status <- if (length(issues) == 0) "ok" else paste(issues, collapse = "; ")

          tibble::tibble(
            indicator_name         = row$indicator_name   %||% NA_character_,
            calculation            = row$calculation      %||% NA_character_,
            var_name               = row$var_name         %||% NA_character_,
            denom_var              = row$denom_var        %||% NA_character_,
            disaggregation         = row$disaggregation   %||% NA_character_,
            multiplier             = as.numeric(row$multiplier %||% NA),
            indicator_unit         = row$indicator_unit   %||% NA_character_,
            var_name_in_data       = var_name_ok,
            denom_var_in_data      = denom_ok,
            disaggregation_in_data = disagg_ok,
            calculation_valid      = calc_valid,
            status                 = status
          )
        })

        result <- dplyr::bind_rows(rows)
        self$analysis_plan_issues_log <- result

        n_issues <- sum(result$status != "ok", na.rm = TRUE)
        phr_message(phr_txt(glue::glue(
          "analysis_diagnose complete: {nrow(result)} indicator(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
        )))

        invisible(result)

      }, on_error = "warn", origin = origin)
    },

    #' @description
    #' Diagnose issues in the unified outputs schema against the current dataset.
    #'
    #' For every output defined in \code{self$outputs_schema}, this method checks
    #' that (a) the output function exists, (b) any \code{variables} listed exist
    #' in the data or survey design, and (c) required schema fields
    #' (\code{output_title}, \code{output_func_name}, \code{output_type}) are
    #' non-empty. The results mirror the outputs schema table and add a
    #' \code{status} column. Results are stored in \code{self$outputs_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per output definition.
    outputs_diagnose = function() {

      origin <- paste0(self$dataset_name, "$outputs_diagnose")

      # Combine column names from both data and survey_design for comprehensive checking
      all_cols <- unique(c(
        if (!is.null(self$data)) names(self$data) else character(0),
        if (!is.null(self$survey_design)) names(self$survey_design$variables) else character(0)
      ))

      private$.diagnose_outputs_schema(
        schema      = self$outputs_schema,
        schema_name = "outputs_schema",
        log_field   = "outputs_issues_log",
        data_cols   = all_cols,
        origin      = origin
      )
    },

    #' @description
    #' Run all outputs defined in the outputs_schema.
    #'
    #' Iterates through the unified outputs schema, resolves \code{@variable_map},
    #' \code{@value_map}, \code{@variable_label}, \code{@value_label}, and
    #' \code{@results_table} references in \code{test_params}, determines the
    #' first positional argument from \code{dataset_type} (defaulting to
    #' \code{"base"}), calls the specified output function, and stores results
    #' in \code{self$visualizations} or \code{self$tables}.
    #'
    #' The \code{output_name} field in each schema entry controls the key used
    #' when storing results in \code{visualizations} or \code{tables}. The
    #' \code{output_title} field (matching the schema list key) and the
    #' language-specific title fields (\code{output_title_english},
    #' \code{output_title_french}, \code{output_title_arabic}) are used to
    #' auto-generate the \code{title_name} argument passed to \code{plot_*} or
    #' \code{table_*} functions. The language-specific title for the chosen
    #' \code{language} takes precedence; if absent the generic
    #' \code{output_title} value is used as a fallback.
    #'
    #' Supported \code{dataset_type} values:
    #' \describe{
    #'   \item{"base"}{uses \code{self$base_survey_design} (unweighted; default for all quality outputs)}
    #'   \item{"survey_design"}{uses \code{self$survey_design} (weighted)}
    #'   \item{"analysis_results_surveydesign"}{uses \code{self$analysis_results$survey_design}}
    #'   \item{"analysis_results_base"}{uses \code{self$analysis_results$base}}
    #' }
    #'
    #' @param language Character string specifying the language for auto-generated
    #'   titles. One of \code{"english"} (default), \code{"french"}, or
    #'   \code{"arabic"}. Falls back to the generic \code{output_title} when the
    #'   language-specific field is absent.
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}
    run_outputs = function(language = "english") {

      phr_try({

        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(
            message = "No outputs defined in outputs_schema.",
            origin  = self$dataset_name
          )
          return(invisible(list(visualizations = self$visualizations, tables = self$tables)))
        }

        phr_message(phr_txt(glue::glue(
          "Running {length(self$outputs_schema)} output(s) for {self$dataset_name}..."
        )))

        for (out_name in names(self$outputs_schema)) {

          out <- self$outputs_schema[[out_name]]

          phr_try({

            func_name <- out$output_func_name
            if (is.null(func_name) || is.na(func_name) || !nzchar(func_name)) {
              phr_warning(
                message = phr_txt(glue::glue(
                  "Output '{out_name}' has no output_func_name specified. Skipping."
                )),
                origin = self$dataset_name
              )
              next
            }

            output_function <- NULL
            if (requireNamespace("phr", quietly = TRUE)) {
              tryCatch({
                ns <- asNamespace("phr")
                if (exists(func_name, envir = ns, mode = "function", inherits = FALSE)) {
                  output_function <- get(func_name, envir = ns, mode = "function", inherits = FALSE)
                }
              }, error = function(e) NULL)
            }
            if (is.null(output_function)) {
              tryCatch({
                if (exists(func_name, mode = "function", inherits = TRUE)) {
                  output_function <- get(func_name, mode = "function", inherits = TRUE)
                }
              }, error = function(e) NULL)
            }
            if (is.null(output_function)) {
              phr_warning(
                message = phr_txt(glue::glue(
                  "Function '{func_name}' for output '{out_name}' not found. Skipping."
                )),
                origin = self$dataset_name
              )
              next
            }

            # Determine first positional argument based on dataset_type column.
            # Supported values:
            #   "base"                          – self$base_survey_design (unweighted)
            #   "survey_design"                 – self$survey_design (weighted)
            #   "analysis_results_surveydesign" – self$analysis_results$survey_design
            #   "analysis_results_base"         – self$analysis_results$base
            dataset_type <- if (!is.null(out$dataset_type) &&
                                  !is.na(out$dataset_type) &&
                                  nzchar(out$dataset_type)) {
              out$dataset_type
            } else "base"

            first_arg <- switch(dataset_type,
              "base"                          = self$base_survey_design,
              "survey_design"                 = self$survey_design,
              "analysis_results_surveydesign" = if (!is.null(self$analysis_results)) self$analysis_results$survey_design else NULL,
              "analysis_results_base"         = if (!is.null(self$analysis_results)) self$analysis_results$base else NULL,
              self$base_survey_design  # default: "base" (unweighted survey design)
            )

            func_args <- list(first_arg)

            # Check if any test_param uses @results_table; if so, replace first arg
            if (!is.null(out$test_params) && length(out$test_params) > 0) {
              for (arg_name_check in names(out$test_params)) {
                arg_val_check <- out$test_params[[arg_name_check]]
                if (is.character(arg_val_check) && arg_val_check == "@results_table") {
                  func_args[[1]] <- self$results_to_table()
                  break
                }
              }
            }

            if (!is.null(out$test_params) && length(out$test_params) > 0) {
              func_args <- private$.resolve_output_params(
                func_args          = func_args,
                test_params        = out$test_params,
                out_name           = out_name,
                skip_results_table = TRUE
              )
            }

            # Storage key: use output_name field; fall back to out_name (schema list key)
            label <- if (!is.null(out$output_name) && !is.na(out$output_name) && nzchar(out$output_name)) {
              out$output_name
            } else {
              out_name
            }

            # Auto-inject title_name unless already supplied in test_params
            if (is.null(func_args[["title_name"]])) {
              title_field <- switch(language,
                "french"  = "output_title_french",
                "arabic"  = "output_title_arabic",
                "output_title_english"
              )
              auto_title <- out[[title_field]]
              if (is.null(auto_title) || is.na(auto_title) || !nzchar(auto_title)) {
                auto_title <- out$output_title
              }
              if (!is.null(auto_title) && !is.na(auto_title) && nzchar(auto_title)) {
                func_args[["title_name"]] <- auto_title
              }
            }

            group <- if (!is.null(out$outputs_group) && !is.na(out$outputs_group) && nzchar(out$outputs_group)) {
              out$outputs_group
            } else NULL

            store_result <- function(result, key) {
              if (!is.null(out$output_type) && out$output_type == "table") {
                if (!is.null(group)) {
                  if (is.null(self$tables[[group]])) self$tables[[group]] <- list()
                  self$tables[[group]][[key]] <- result
                } else {
                  self$tables[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Table '{key}' stored successfully.")))
              } else if (!is.null(out$output_type) && out$output_type == "visualization") {
                if (!is.null(group)) {
                  if (is.null(self$visualizations[[group]])) self$visualizations[[group]] <- list()
                  self$visualizations[[group]][[key]] <- result
                } else {
                  self$visualizations[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Visualization '{key}' stored successfully.")))
              } else {
                phr_warning(
                  message = phr_txt(glue::glue(
                    "Output '{out_name}' has unrecognized output_type '{out$output_type}'. ",
                    "Expected 'visualization' or 'table'. Result not stored."
                  )),
                  origin = self$dataset_name
                )
              }
            }

            per_group_ref <- if (!is.null(out$outputs_per_group) &&
                                 !is.na(out$outputs_per_group) &&
                                 nzchar(out$outputs_per_group)) out$outputs_per_group else NULL

            if (!is.null(per_group_ref)) {

              if (grepl("^@variable_map\\$", per_group_ref)) {
                role      <- sub("^@variable_map\\$", "", per_group_ref)
                col_name  <- self$variable_map[[role]]
                var_label <- role
                if (is.null(col_name)) {
                  phr_warning(
                    message = phr_txt(glue::glue(
                      "outputs_per_group role '{role}' not found in variable_map for output '{out_name}'. Skipping."
                    )),
                    origin = self$dataset_name
                  )
                  col_name <- NULL
                }
              } else {
                col_name  <- per_group_ref
                var_label <- per_group_ref
              }

              # Determine the data frame used for finding unique group values.
              # All survey_design types: extract variables tibble.
              source_df <- switch(dataset_type,
                "analysis_results_surveydesign" = if (!is.null(self$analysis_results)) self$analysis_results$survey_design else NULL,
                "analysis_results_base"         = if (!is.null(self$analysis_results)) self$analysis_results$base else NULL,
                # default: base or survey_design – extract variables tibble
                {
                  design_obj <- if (dataset_type == "survey_design") self$survey_design else self$base_survey_design
                  if (!is.null(design_obj)) {
                    tryCatch(design_obj$variables, error = function(e) NULL)
                  } else NULL
                }
              )

              is_survey_design_type <- !(dataset_type %in% c("analysis_results_surveydesign", "analysis_results_base"))

              if (!is.null(col_name) && !is.null(source_df) && col_name %in% names(source_df)) {
                unique_vals <- unique(source_df[[col_name]])
                unique_vals <- unique_vals[!is.na(unique_vals)]

                phr_message(phr_txt(glue::glue(
                  "Calling {func_name} for output '{out_name}' across {length(unique_vals)} group(s) of '{var_label}'..."
                )))

                for (val in unique_vals) {
                  if (is_survey_design_type) {
                    design_to_filter <- if (dataset_type == "survey_design") self$survey_design else self$base_survey_design
                    filtered_first_arg <- tryCatch(
                      dplyr::filter(design_to_filter, !!rlang::sym(col_name) == val),
                      error = function(e) {
                        phr_warning(
                          message = phr_txt(glue::glue(
                            "Failed to filter survey design for '{var_label}' == '{val}': {e$message}"
                          )),
                          origin = self$dataset_name
                        )
                        NULL
                      }
                    )
                  } else {
                    filtered_first_arg <- tryCatch(
                      source_df |> dplyr::filter(!!rlang::sym(col_name) == val),
                      error = function(e) {
                        phr_warning(
                          message = phr_txt(glue::glue(
                            "Failed to filter data for '{var_label}' == '{val}': {e$message}"
                          )),
                          origin = self$dataset_name
                        )
                        NULL
                      }
                    )
                  }

                  if (is.null(filtered_first_arg)) next

                  per_group_args      <- func_args
                  per_group_args[[1]] <- filtered_first_arg
                  amended_label       <- paste0(label, "-", var_label, ".", val)

                  phr_try({
                    output_result <- do.call(output_function, per_group_args)
                    store_result(output_result, amended_label)
                  }, on_error = "warn",
                     origin = paste0(self$dataset_name, "$run_outputs$", out_name, "$", val))
                }

              } else if (!is.null(col_name)) {
                phr_warning(
                  message = phr_txt(glue::glue(
                    "Column '{col_name}' for outputs_per_group not found in source data ({dataset_type}) for output '{out_name}'. Skipping."
                  )),
                  origin = self$dataset_name
                )
              }

            } else {
              phr_message(phr_txt(glue::glue("Calling {func_name} for output '{out_name}'...")))
              output_result <- do.call(output_function, func_args)
              store_result(output_result, label)
            }

          }, on_error = "warn",
             origin = paste0(self$dataset_name, "$run_outputs$", out_name))
        }

        phr_message(phr_txt(glue::glue(
          "run_outputs complete: {length(self$visualizations)} visualization(s), {length(self$tables)} table(s)."
        )))

        invisible(list(visualizations = self$visualizations, tables = self$tables))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs"))
    },



    # Analysis Methods


    #' @description
    #' Create a survey design object from the stored data and variable_map.
    #' @return A srvyr survey design object, or NULL
    create_survey_design = function() {
      origin <- paste0(self$dataset_name, "$create_survey_design")

      if (!requireNamespace("srvyr", quietly = TRUE)) {
        phr_warning(origin, "Package 'srvyr' must be installed to create survey design objects.")
        return(NULL)
      }

      if (is.null(self$data)) {
        phr_warning(origin, "No data available to create survey design.")
        return(NULL)
      }

      data_cols   <- names(self$data)

      cluster_col <- self$variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- self$variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- self$variable_map[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) weight_col <- NULL

      strata_col <- self$variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) strata_col <- NULL

      fpc_col    <- self$variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) fpc_col <- NULL

      if (is.null(cluster_col)) {
        phr_message(origin, "No cluster column found; using ids = 1 (simple random sample design).")
      }

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phr_try(
        srvyr::as_survey_design(
          .data   = self$data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin = origin,
        hint = "Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data."
      )

      if (!is.null(design)) {
        phr_message(origin, "Survey design created successfully.")
      }

      return(design)
    },

    #' @description Add an indicator to the data analysis plan
    #' @param indicator_name Character; human-readable label for the indicator.
    #' @param calculation Character; one of "prop", "mean", "median", "ratio", or "categorical".
    #' @param var_name Character; dataset column name for the primary variable.
    #' @param denom_var Character or NULL; denominator column name.
    #' @param disaggregation Character or NULL; column name to use for stratified analysis.
    #' @param multiplier Numeric; scale factor for the point estimate.
    #' @param indicator_unit Character; unit label.
    #' @param high_design_complexity Logical; reserved for future use.
    #' @param note Character or NULL; optional free-text note.
    #' @return Invisibly returns self.
    add_indicator_dap = function(indicator_name,
                                 calculation,
                                 var_name,
                                 denom_var = NULL,
                                 disaggregation = NULL,
                                 multiplier = 100,
                                 indicator_unit = "%",
                                 high_design_complexity = FALSE,
                                 note = NULL) {
      origin <- paste0(self$dataset_name, "$add_indicator_dap")

      phr_try({
        self$data_analysis_plan$add_indicator(
          indicator_name = indicator_name,
          calculation    = calculation,
          var_name       = var_name,
          denom_var      = denom_var      %||% NA_character_,
          disaggregation = disaggregation %||% NA_character_,
          multiplier     = multiplier,
          indicator_unit = indicator_unit
        )
        phr_message(origin, paste("Added indicator:", indicator_name))
      },
      on_error = "warn",
      origin = origin,
      hint = "Check variable names and calculation type.")
    },

    #' @description Remove an indicator from the data analysis plan
    #' @param indicator_name Character; label of the indicator to remove.
    #' @return Invisibly returns self.
    remove_indicator_dap = function(indicator_name) {
      origin <- paste0(self$dataset_name, "$remove_indicator_dap")

      phr_try({
        if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
          phr_warning(origin, "No data_analysis_plan loaded.")
          return(invisible(self))
        }
        self$data_analysis_plan$log_df <-
          self$data_analysis_plan$log_df[
            self$data_analysis_plan$log_df$indicator_name != indicator_name, ]
        phr_message(origin, paste("Removed indicator:", indicator_name))
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure indicator_name exists in data_analysis_plan.")
    },

    #' @description Convert the analysis schema tibble to a named list
    #' @return A named list of indicator specification lists
    to_list_schema = function() {
      origin <- paste0(self$dataset_name, "$to_list_schema")
      phr_message(origin, "Converting indicator schema tibble to named list...")

      result <- phr_try({
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "No analysis_schema loaded to convert.")
          return(list())
        }
        purrr::pmap(self$analysis_schema, function(...) list(...)) |>
          purrr::set_names(self$analysis_schema$indicator_name)
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure analysis_schema is a valid tibble with 'indicator_name' column.")

      phr_message(origin, "Conversion complete.")
      return(result)
    },

    #' @description Generate the data analysis plan from the analysis schema
    #' @return Invisibly returns self.
    generate_dap_from_schema = function() {
      origin <- paste0(self$dataset_name, "$generate_dap_from_schema")
      phr_message(origin, "Generating data_analysis_plan from schema...")

      phr_try({
        if (!is.null(self$survey_design)) {
          available_vars <- names(self$survey_design$variables)
        } else if (!is.null(self$data)) {
          available_vars <- names(self$data)
        } else {
          phr_error(origin, "Neither survey_design nor data is available.")
          return(invisible(self))
        }

        translate_var <- function(canonical_name) {
          if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
          if (!is.null(self$variable_map) && canonical_name %in% names(self$variable_map)) {
            actual_name <- self$variable_map[[canonical_name]]
            if (!is.null(actual_name) && actual_name != "") return(actual_name)
          }
          return(canonical_name)
        }

        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "analysis_schema is empty; data_analysis_plan will remain empty.")
          return(invisible(self))
        }

        schema_valid <- self$analysis_schema |>
          dplyr::mutate(
            var_name_actual   = purrr::map_chr(.data$var_name,   translate_var),
            denom_var_actual  = purrr::map_chr(.data$denom_var,  translate_var),
            var_exists        = .data$var_name_actual %in% available_vars,
            denom_exists      = ifelse(!is.na(.data$denom_var_actual),
                                       .data$denom_var_actual %in% available_vars,
                                       TRUE)
          ) |>
          dplyr::mutate(include = .data$var_exists & .data$denom_exists)

        issues <- schema_valid |>
          dplyr::filter(!.data$include) |>
          dplyr::transmute(
            indicator_name = .data$indicator_name,
            issue = paste0(
              "Missing variable(s): ",
              ifelse(!.data$var_exists,
                     paste0(.data$var_name, " (maps to: ", .data$var_name_actual, ")"),
                     ""),
              ifelse(!.data$denom_exists & !is.na(.data$denom_var),
                     paste0(", ", .data$denom_var, " (maps to: ", .data$denom_var_actual, ")"),
                     "")
            )
          )

        dap_df <- schema_valid |>
          dplyr::filter(.data$include) |>
          dplyr::transmute(
            indicator_name = .data$indicator_name,
            calculation    = .data$calculation,
            var_name       = .data$var_name_actual,
            denom_var      = .data$denom_var_actual,
            disaggregation = .data$disaggregation,
            multiplier     = .data$multiplier,
            indicator_unit = .data$indicator_unit
          )

        self$data_analysis_plan$log_df <- dap_df

        if (nrow(issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            issues
          )
          phr_warning(origin, paste0(nrow(issues), " indicators skipped due to missing variables."))
        } else {
          phr_message(origin, "All schema indicators found and added to data_analysis_plan.")
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure survey_design$variables is accessible and schema structure is valid.")

      invisible(self)
    },

    #' @description Validate the analysis schema
    #' @return TRUE if validation passes, FALSE otherwise.
    validate_schema = function() {
      origin <- paste0(self$dataset_name, "$validate_schema")
      phr_message(origin, "Validating indicator schema...")

      issues <- tibble::tibble(indicator_name = character(), issue = character())

      phr_try({
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Indicator schema is empty."
          ))
          self$analysis_plan_issue_log <- issues
          phr_warning(origin, "Schema validation FAILED: schema empty.")
          return(invisible(FALSE))
        }

        required_cols <- c("indicator_name", "calculation", "var_name",
                           "denom_var", "disaggregation", "multiplier", "indicator_unit")
        missing_cols  <- setdiff(required_cols, names(self$analysis_schema))
        if (length(missing_cols) > 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = paste("Schema missing required columns:", paste(missing_cols, collapse = ", "))
          ))
        }

        if (is.null(self$survey_design)) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Survey design not loaded. Cannot validate variables."
          ))
        } else {
          available_vars <- names(self$survey_design$variables)
          var_issues     <- self$analysis_schema |>
            dplyr::mutate(
              var_exists   = .data$var_name %in% available_vars,
              denom_exists = ifelse(
                !is.na(.data$denom_var) & .data$denom_var != "",
                .data$denom_var %in% available_vars,
                TRUE
              )
            ) |>
            dplyr::filter(!.data$var_exists | !.data$denom_exists)

          if (nrow(var_issues) > 0) {
            issues <- dplyr::bind_rows(issues, var_issues |>
              dplyr::transmute(
                indicator_name,
                issue = paste0(
                  "Missing variable(s): ",
                  ifelse(!var_exists, var_name, ""),
                  ifelse(!denom_exists & !is.na(denom_var), paste0(", ", denom_var), "")
                )
              ))
          }
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure schema uses standard DAP fields and variables exist in dataset.")

      self$analysis_plan_issue_log <- issues

      if (nrow(issues) > 0) {
        phr_warning(origin, paste("Schema validation FAILED with", nrow(issues), "issue(s)."))
        return(FALSE)
      }

      phr_message(origin, "Schema validation PASSED.")
      return(TRUE)
    },

    #' @description Validate the data analysis plan
    #' @return Invisibly returns self.
    validate_plan = function() {
      origin <- paste0(self$dataset_name, "$validate_plan")
      phr_message(origin, "Validating analysis plan...")

      issues <- tibble::tibble(indicator_name = character(), issue = character())

      phr_try({
        if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Analysis plan is empty."
          ))
        } else {
          required_cols <- c("indicator_name", "calculation", "var_name",
                             "denom_var", "disaggregation", "multiplier", "indicator_unit")
          missing_cols  <- setdiff(required_cols, names(self$data_analysis_plan$log_df))

          if (length(missing_cols) > 0) {
            issues <- dplyr::bind_rows(issues, tibble::tibble(
              indicator_name = NA_character_,
              issue = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
            ))
          }

          invalid_calc <- self$data_analysis_plan$log_df |>
            dplyr::filter(!.data$calculation %in% c("prop", "mean", "median", "ratio"))

          if (nrow(invalid_calc) > 0) {
            issues <- dplyr::bind_rows(issues, tibble::tibble(
              indicator_name = invalid_calc$indicator_name,
              issue = paste("Invalid calculation type:", invalid_calc$calculation)
            ))
          }
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check data_analysis_plan structure and field names.")

      self$analysis_plan_issue_log <- issues
      if (nrow(issues) > 0) {
        phr_warning(origin, paste("Validation found", nrow(issues), "issue(s)."))
      } else {
        phr_message(origin, "Analysis plan validation passed with no issues.")
      }
      invisible(self)
    },

    #' @description Run the data analysis plan and store results in analysis_results
    #'
    #' Executes every indicator twice: once using the full survey design (weighted)
    #' and once using a simple unweighted design.  Both sets are stored in
    #' \code{self$analysis_results} as a named list with elements
    #' \code{survey_design} and \code{base}.
    #'
    #' @return Invisibly returns self.
    run_analysis = function() {
      origin <- paste0(self$dataset_name, "$run_analysis")
      phr_message(origin, "Running analysis plan...")

      if (is.null(self$survey_design)) {
        phr_error(origin, "Survey design not set.")
        return(invisible(self))
      }

      if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
        phr_error(origin, "No data_analysis_plan provided.")
        return(invisible(self))
      }

      survey_design_results <- phr_try(
        phr_calc_survey_from_plan(
          design       = self$survey_design,
          analysis_plan = self$data_analysis_plan$log_df
        ),
        on_error = "warn",
        origin = origin,
        hint = "Verify all variables exist and analysis plan is valid."
      )

      base_results <- NULL
      if (!is.null(self$data)) {
        base_design <- phr_try(
          srvyr::as_survey_design(.data = self$data, ids = 1),
          on_error = "warn",
          origin = origin,
          hint = "Could not create base (unweighted) survey design from self$data."
        )

        if (!is.null(base_design)) {
          base_results <- phr_try(
            phr_calc_survey_from_plan(
              design       = base_design,
              analysis_plan = self$data_analysis_plan$log_df
            ),
            on_error = "warn",
            origin = origin,
            hint = "Verify all variables exist in the base (unweighted) design."
          )
        }
      }

      self$analysis_results <- list(
        survey_design = survey_design_results,
        base          = base_results
      )

      phr_message(origin, "Analysis completed successfully.")
      invisible(self)
    },

    #' @description Retrieve analysis results
    #' @return A named list with elements survey_design and base, or NULL
    get_results = function() {
      origin <- paste0(self$dataset_name, "$get_results")
      phr_message(origin, "Retrieving analysis results...")

      if (is.null(self$analysis_results) || length(self$analysis_results) == 0) {
        phr_warning(origin, "No analysis results available yet. Run run_analysis() first.")
      }
      return(self$analysis_results)
    },

    #' @description Export analysis results to a file
    #' @param path Character; file path for the output file.
    #' @param format Character; "xlsx" (default) or "csv".
    #' @return Invisibly returns self.
    export_results = function(path, format = "xlsx") {
      origin <- paste0(self$dataset_name, "$export_results")

      phr_try({
        if (is.null(self$analysis_results) || length(self$analysis_results) == 0) {
          phr_warning(origin, "No analysis results to export.")
          return(invisible(self))
        }

        phr_message(origin, paste("Exporting analysis results to:", path))

        if (format == "xlsx") {
          sheets <- list()
          if (!is.null(self$analysis_results$survey_design)) {
            sheets[["survey_design"]] <- self$analysis_results$survey_design
          }
          if (!is.null(self$analysis_results$base)) {
            sheets[["base"]] <- self$analysis_results$base
          }
          openxlsx::write.xlsx(sheets, path)
        } else if (format == "csv") {
          tbl <- self$analysis_results$survey_design %||% tibble::tibble()
          readr::write_csv(tbl, path)
        } else {
          phr_warning(origin, paste("Unsupported export format:", format))
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check file path and permissions.")

      invisible(self)
    },

    #' @description Export the internal state as an R list for session serialisation
    #' @return A named list of internal state fields.
    export_state_object = function() {
      list(
        data_analysis_plan           = self$data_analysis_plan,
        analysis_results             = self$analysis_results,
        plausibility_results         = self$plausibility_results,
        analysis_plan_issue_log      = self$analysis_plan_issue_log,
        analysis_schema              = self$analysis_schema,
        quality_schema               = self$quality_schema,
        outputs_schema               = self$outputs_schema,
        visualizations               = self$visualizations,
        tables                       = self$tables,
        survey_design                = self$survey_design,
        base_survey_design           = self$base_survey_design,
        quality_issues_log           = self$quality_issues_log,
        analysis_plan_issues_log     = self$analysis_plan_issues_log,
        outputs_issues_log           = self$outputs_issues_log
      )
    },

    #' @description Restore internal state from a previously exported state list
    #' @param state A named list as returned by export_state_object().
    #' @return Invisibly returns self.
    load_state_object = function(state) {
      self$data_analysis_plan      <- state$data_analysis_plan
      self$analysis_results        <- state$analysis_results
      self$plausibility_results    <- state$plausibility_results %||% state$results
      self$analysis_plan_issue_log <- state$analysis_plan_issue_log
      self$analysis_schema         <- state$analysis_schema
      self$quality_schema          <- state$quality_schema
      # Support loading state saved with old split schema fields
      if (!is.null(state$outputs_schema)) {
        self$outputs_schema <- state$outputs_schema
      } else if (!is.null(state$quality_outputs_schema) || !is.null(state$analysis_outputs_schema)) {
        # Merge legacy quality and analysis outputs schemas into unified outputs_schema
        merged <- c(
          if (!is.null(state$quality_outputs_schema)) state$quality_outputs_schema else list(),
          if (!is.null(state$analysis_outputs_schema)) state$analysis_outputs_schema else list()
        )
        if (length(merged) > 0) self$outputs_schema <- merged
      }
      self$visualizations          <- state$visualizations
      self$tables                  <- state$tables

      if (!is.null(state$quality_issues_log))          self$quality_issues_log          <- state$quality_issues_log
      if (!is.null(state$analysis_plan_issues_log))    self$analysis_plan_issues_log    <- state$analysis_plan_issues_log
      if (!is.null(state$outputs_issues_log))          self$outputs_issues_log          <- state$outputs_issues_log

      if (!is.null(state$survey_design)) {
        self$survey_design <- state$survey_design
      }
      if (!is.null(state$base_survey_design)) {
        self$base_survey_design <- state$base_survey_design
      }

      invisible(self)
    },

    #' @description Export the analysis schema to a file
    #' @param path Character; destination file path (including extension).
    #' @param format Character; "xlsx" (default) or "csv".
    #' @return Invisibly returns self.
    export_analysis_schema = function(path, format = "xlsx") {
      origin <- paste0(self$dataset_name, "$export_analysis_schema")

      phr_try({
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "No analysis schema to export.")
          return(invisible(self))
        }
        phr_message(origin, paste("Exporting analysis schema to:", path))
        if (format == "xlsx") {
          openxlsx::write.xlsx(self$analysis_schema, path)
        } else if (format == "csv") {
          readr::write_csv(self$analysis_schema, path)
        } else {
          phr_warning(origin, paste("Unsupported export format:", format))
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check file path and permissions.")

      invisible(self)
    },

    #' @description Import an analysis schema from a file
    #' @param path Character; path to the schema file (.xlsx, .csv, or .rds).
    #' @return Invisibly returns self.
    import_analysis_schema = function(path) {
      origin <- paste0(self$dataset_name, "$import_analysis_schema")
      phr_message(origin, paste("Importing analysis schema from:", path))

      phr_try({
        if (!file.exists(path)) {
          phr_error(origin, paste("File not found:", path))
          return(invisible(self))
        }

        ext <- tools::file_ext(path)
        schema_tbl <- switch(
          ext,
          "csv"  = readr::read_csv(path, show_col_types = FALSE),
          "xlsx" = readxl::read_xlsx(path),
          "rds"  = readRDS(path),
          {
            phr_error(origin, paste("Unsupported file type:", ext))
            return(invisible(self))
          }
        )

        required_cols <- c("indicator_name", "calculation", "var_name",
                           "denom_var", "disaggregation", "multiplier", "indicator_unit")
        missing_cols  <- setdiff(required_cols, names(schema_tbl))
        if (length(missing_cols) > 0) {
          phr_warning(origin, paste("Imported schema missing columns:", paste(missing_cols, collapse = ", ")))
        }

        self$analysis_schema <- schema_tbl
        phr_message(origin, paste("Analysis schema imported with", nrow(schema_tbl), "row(s)."))
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure the file is a valid xlsx, csv, or rds with the required schema columns.")

      invisible(self)
    }

  ),


  # Private helpers


  private = list(

    # Resolve @ references in test_params into concrete values for do.call()
    #
    # @param func_args Named list; positional/named args accumulated so far.
    # @param test_params Named list of raw parameter values (may contain @-refs).
    # @param out_name Character; output name used in warning messages.
    # @param skip_results_table Logical; if TRUE, @results_table entries are
    #   skipped as named args (they were already handled as first positional arg).
    # @return Updated func_args list.
    .resolve_output_params = function(func_args, test_params, out_name,
                                     skip_results_table = FALSE) {

      for (arg_name in names(test_params)) {
        arg_value <- test_params[[arg_name]]

        # c(...) vector format
        if (is.character(arg_value) && grepl("^c\\(", arg_value)) {
          vec_content  <- sub("^c\\(", "", sub("\\)$", "", arg_value))
          vec_elements <- .parse_indicator_arguments(vec_content)

          resolved_elements <- character()
          for (elem in vec_elements) {
            elem <- trimws(elem)
            if (grepl("^@variable_map\\$", elem)) {
              role     <- sub("^@variable_map\\$", "", elem)
              resolved <- self$variable_map[[role]]
              if (!is.null(resolved)) {
                resolved_elements <- c(resolved_elements, resolved)
              } else {
                phr_warning(
                  message = phr_txt(glue::glue("Variable map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                  origin = self$dataset_name
                )
              }
            } else if (grepl("^@value_map\\$", elem)) {
              parts <- strsplit(sub("^@value_map\\$", "", elem), "\\$")[[1]]
              if (length(parts) >= 1) {
                role <- parts[1]
                if (!is.null(self$value_map[[role]])) {
                  if (length(parts) == 2) {
                    resolved <- self$value_map[[role]][[parts[2]]]
                    if (!is.null(resolved)) resolved_elements <- c(resolved_elements, resolved)
                  } else {
                    resolved_elements <- c(resolved_elements, unlist(self$value_map[[role]], use.names = FALSE))
                  }
                }
              }
            } else if (grepl("^@variable_label\\$", elem)) {
              role     <- sub("^@variable_label\\$", "", elem)
              resolved <- self$variable_label[[role]]
              if (!is.null(resolved)) {
                resolved_elements <- c(resolved_elements, resolved)
              } else {
                phr_warning(
                  message = phr_txt(glue::glue("Variable label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                  origin = self$dataset_name
                )
              }
            } else if (grepl("^@value_label\\$", elem)) {
              parts <- strsplit(sub("^@value_label\\$", "", elem), "\\$")[[1]]
              if (length(parts) >= 1) {
                role <- parts[1]
                if (!is.null(self$value_label[[role]])) {
                  if (length(parts) == 2) {
                    resolved <- self$value_label[[role]][[parts[2]]]
                    if (!is.null(resolved)) {
                      resolved_elements <- c(resolved_elements, resolved)
                    } else {
                      phr_warning(
                        message = phr_txt(glue::glue("Value label '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                        origin = self$dataset_name
                      )
                    }
                  } else {
                    resolved_elements <- c(resolved_elements, unlist(self$value_label[[role]], use.names = FALSE))
                  }
                } else {
                  phr_warning(
                    message = phr_txt(glue::glue("Value label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                    origin = self$dataset_name
                  )
                }
              }
            } else {
              resolved_elements <- c(resolved_elements, gsub("^['\"]|['\"]$", "", elem))
            }
          }

          numeric_attempt <- suppressWarnings(as.numeric(resolved_elements))
          if (length(resolved_elements) > 0 && !any(is.na(numeric_attempt))) {
            func_args[[arg_name]] <- numeric_attempt
          } else if (length(resolved_elements) > 0 && all(toupper(resolved_elements) %in% c("TRUE", "FALSE"))) {
            func_args[[arg_name]] <- as.logical(resolved_elements)
          } else {
            func_args[[arg_name]] <- resolved_elements
          }

        } else if (is.character(arg_value) && grepl("^@variable_label\\$", arg_value)) {
          role     <- sub("^@variable_label\\$", "", arg_value)
          resolved <- self$variable_label[[role]]
          if (!is.null(resolved)) {
            func_args[[arg_name]] <- resolved
          } else {
            phr_warning(
              message = phr_txt(glue::glue("Variable label role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'.")),
              origin = self$dataset_name
            )
          }

        } else if (is.character(arg_value) && arg_value == "@results_table") {
          if (!skip_results_table) {
            func_args[[arg_name]] <- self$results_to_table()
          }
          # else: already handled as first positional arg in run_outputs

        } else if (is.character(arg_value) && grepl("^@value_label\\$", arg_value)) {
          parts <- strsplit(sub("^@value_label\\$", "", arg_value), "\\$")[[1]]
          if (length(parts) >= 1) {
            role <- parts[1]
            if (!is.null(self$value_label[[role]])) {
              resolved <- if (length(parts) == 2) self$value_label[[role]][[parts[2]]] else self$value_label[[role]]
              func_args[[arg_name]] <- resolved %||% arg_value
            } else {
              func_args[[arg_name]] <- arg_value
            }
          }

        } else if (is.character(arg_value) && grepl("^@variable_map\\$", arg_value)) {
          role     <- sub("^@variable_map\\$", "", arg_value)
          resolved <- self$variable_map[[role]]
          if (!is.null(resolved)) func_args[[arg_name]] <- resolved

        } else if (is.character(arg_value) && grepl("^@value_map\\$", arg_value)) {
          parts <- strsplit(sub("^@value_map\\$", "", arg_value), "\\$")[[1]]
          if (length(parts) >= 1) {
            role <- parts[1]
            if (!is.null(self$value_map[[role]])) {
              resolved <- if (length(parts) == 2) self$value_map[[role]][[parts[2]]] else self$value_map[[role]]
              func_args[[arg_name]] <- resolved %||% arg_value
            } else {
              func_args[[arg_name]] <- arg_value
            }
          }

        } else {
          if (is.character(arg_value)) arg_value <- gsub("^['\"]|['\"]$", "", arg_value)
          func_args[[arg_name]] <- arg_value
        }
      }

      return(func_args)
    },

    # Shared implementation for outputs_diagnose
    #
    # @param schema Named list; the outputs schema to inspect.
    # @param schema_name Character; name used in messages.
    # @param log_field Character; name of the public field to store results in.
    # @param data_cols Character vector; column names available in the relevant data source.
    # @param origin Character; calling method name for error messages.
    # @return A tibble (invisibly).
    .diagnose_outputs_schema = function(schema, schema_name, log_field, data_cols, origin) {

      phr_try({

        empty_result <- tibble::tibble(
          output_title       = character(),
          output_name        = character(),
          output_func_name   = character(),
          output_type        = character(),
          variables          = character(),
          test_params        = character(),
          outputs_group      = character(),
          function_available = logical(),
          variables_in_data  = logical(),
          missing_variables  = character(),
          required_fields_ok = logical(),
          status             = character()
        )

        if (is.null(schema) || length(schema) == 0) {
          phr_warning(
            message = phr_txt(glue::glue("No {schema_name} defined. Cannot diagnose.")),
            origin  = origin
          )
          self[[log_field]] <- empty_result
          return(invisible(empty_result))
        }

        rows <- list()

        for (out_name in names(schema)) {
          out <- schema[[out_name]]

          func_name      <- out$output_func_name %||% NA_character_
          output_type    <- out$output_type      %||% NA_character_
          output_name    <- out$output_name      %||% NA_character_
          outputs_group  <- out$outputs_group    %||% NA_character_
          variables      <- out$variables        %||% character(0)
          test_params    <- out$test_params      %||% list()

          # --- 1. required fields present
          req_ok <- !is.null(func_name) && !is.na(func_name) && nzchar(func_name) &&
                    !is.null(output_type) && !is.na(output_type) && nzchar(output_type)

          # --- 2. function available
          func_available <- FALSE
          if (req_ok) {
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

          # --- 3. variables present in data
          # Skip @variable_map and @value_map refs; resolve canonical names via variable_map
          literal_vars <- variables[!grepl("^@", variables)]
          resolved_vars <- self$.translate_canonical_to_actual_vars(literal_vars)
          missing_vars <- setdiff(resolved_vars, data_cols)
          vars_in_data <- length(missing_vars) == 0

          # --- build status
          issues <- character(0)
          if (!req_ok)        issues <- c(issues, "missing required fields (output_func_name or output_type)")
          if (!func_available && req_ok) issues <- c(issues, paste0("function '", func_name, "' not found"))
          if (!vars_in_data)  issues <- c(issues, paste0("missing variables: ", paste(missing_vars, collapse = ", ")))
          status <- if (length(issues) == 0) "ok" else paste(issues, collapse = "; ")

          rows[[length(rows) + 1]] <- tibble::tibble(
            output_title       = out_name,
            output_name        = output_name %||% NA_character_,
            output_func_name   = func_name %||% NA_character_,
            output_type        = output_type %||% NA_character_,
            variables          = if (length(variables) > 0) paste(variables, collapse = ", ") else NA_character_,
            test_params        = if (length(test_params) > 0) paste(names(test_params), test_params, sep = "=", collapse = ", ") else NA_character_,
            outputs_group      = outputs_group,
            function_available = func_available,
            variables_in_data  = vars_in_data,
            missing_variables  = if (length(missing_vars) > 0) paste(missing_vars, collapse = ", ") else NA_character_,
            required_fields_ok = req_ok,
            status             = status
          )
        }

        result <- if (length(rows) > 0) dplyr::bind_rows(rows) else empty_result
        self[[log_field]] <- result

        n_issues <- sum(result$status != "ok", na.rm = TRUE)
        phr_message(phr_txt(glue::glue(
          "{schema_name} diagnose complete: {nrow(result)} output(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
        )))

        invisible(result)

      }, on_error = "warn", origin = origin)
    }
  )
)
