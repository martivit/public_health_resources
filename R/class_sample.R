#' Sample R6 Class
#'
#' @description
#' Holds the protocol strata/sample table and related workflows such as adding
#' strata rows and calculating sample sizes.
#'
#' @importFrom R6 R6Class
#' @export
Sample <- R6::R6Class(
  "Sample",
  public = list(
    #' @field sample_table Data frame with one row per stratum.
    sample_table = NULL,

    #' @field validated Logical indicating whether the sample table has been validated.
    validated = NULL,

    #' @field metadata List containing sample metadata.
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    #' @description Create a new Sample object.
    #' @param sample_table Optional sample table data frame.
    initialize = function(sample_table = NULL) {
      timestamp <- Sys.time()
      self$metadata$created_datetime <- timestamp
      self$metadata$modified_datetime <- timestamp
      self$validated <- FALSE
      if (is.null(sample_table)) {
        self$sample_table <- NULL
      } else {
        phr_validate_dataframe(
          sample_table,
          origin = "Sample$initialize",
          soft = FALSE
        )
        self$sample_table <- as.data.frame(
          sample_table,
          stringsAsFactors = FALSE
        )
      }
      invisible(self)
    },

    #' @description Replace the current sample table.
    #' @param sample_table Data frame.
    set_sample_table = function(sample_table) {
      phr_validate_dataframe(
        sample_table,
        origin = "Sample$set_sample_table",
        soft = FALSE
      )
      self$sample_table <- as.data.frame(sample_table, stringsAsFactors = FALSE)
      private$..touch()
      invisible(self)
    },

    #' @description Return the current sample table data frame.
    get_sample_table = function() {
      out <- self$sample_table
      out
    },

    #' @description Clear sample table data.
    clear_sample_table = function() {
      self$sample_table <- NULL
      private$..touch()
      invisible(self)
    },

    #' @description Add a stratum row to the sample table.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size = NA_real_,
      total_households = NA_real_,
      sampling_method_site = NULL,
      sampling_method_hh = NULL,
      n_psu = NA_real_,
      cluster_size = NA_real_,
      n_sites = NA_real_,
      pop_indicator = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision = NA_real_,
      pop_nonresponse = NA_real_,
      pop_design_effect = NA_real_,
      pop_fpc = FALSE,
      General_HH_Sample_Size = NA_real_,
      ind_indicator = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision = NA_real_,
      ind_nonresponse = NA_real_,
      ind_design_effect = NA_real_,
      ind_avg_hh_size = NA_real_,
      ind_subpop_prop = NA_real_,
      ind_fpc = FALSE,
      Ind_Sample_Size = NA_real_,
      Ind_HH_Sample_Size = NA_real_,
      rate_indicator = NA_character_,
      rate_expected_rate = NA_real_,
      rate_precision = NA_real_,
      rate_nonresponse = NA_real_,
      rate_design_effect = NA_real_,
      rate_recall_days = NA_real_,
      rate_avg_hh_size = NA_real_,
      rate_fpc = FALSE,
      Rate_Ind_Sample_Size = NA_real_,
      Rate_PT_Sample_Size = NA_real_,
      Rate_HH_Sample_Size = NA_real_,
      teams = NA_real_,
      avg_interview_time = NA_real_,
      clusters_per_day = NA_real_,
      enumerators_per_team = NA_real_,
      avg_rest_time = NA_real_,
      avg_travel_time = NA_real_,
      start_time = NA_character_,
      end_time = NA_character_,
      design_effect = NULL,
      precision = NULL,
      confidence_level = NULL
    ) {
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      phr_assert(
        !is.null(sampling_method_site),
        message = phr_txt("sampling_method_site is required."),
        origin = "Sample$add_stratum"
      )

      site_selection_methods <- c(
        "simple_random",
        "proportional",
        "cluster",
        "systematic",
        "purposive"
      )
      phr_assert(
        sampling_method_site %in% site_selection_methods,
        message = phr_txt(
          "sampling_method_site must be one of: {paste(site_selection_methods, collapse=', ')}."
        ),
        origin = "Sample$add_stratum"
      )

      if (sampling_method_site %in% site_selection_methods) {
        phr_assert(
          !is.null(n_sites) && !is.na(n_sites),
          message = phr_txt(
            "n_sites is required for sampling_method '{sampling_method_site}'."
          ),
          origin = "Sample$add_stratum"
        )
      }

      if (
        sampling_method_hh %in%
          "rlc" &&
          (is.null(cluster_size) || is.na(cluster_size))
      ) {
        cluster_size <- 3L
      }

      hh_selection_methods <- c("simple_random", "systematic", "rlc")
      hh_method <- if (sampling_method_hh %in% hh_selection_methods) {
        sampling_method_hh
      } else {
        "simple_random"
      }

      new_row <- data.frame(
        stratum_id = stratum_id,
        stratum_name = stratum_name,
        total_households = as.numeric(total_households),
        total_population = as.numeric(population_size),
        sampling_method_site = sampling_method_site,
        sampling_method_hh = hh_method,
        n_psu = as.numeric(n_psu),
        cluster_size = as.numeric(cluster_size),
        n_sites = as.numeric(n_sites),
        pop_indicator = pop_indicator,
        pop_expected_prevalence = as.numeric(pop_expected_prevalence),
        pop_precision = as.numeric(pop_precision),
        pop_nonresponse = as.numeric(pop_nonresponse),
        pop_design_effect = as.numeric(pop_design_effect),
        pop_fpc = as.logical(pop_fpc),
        ind_indicator = as.character(ind_indicator),
        ind_expected_prevalence = as.numeric(ind_expected_prevalence),
        ind_precision = as.numeric(ind_precision),
        ind_nonresponse = as.numeric(ind_nonresponse),
        ind_design_effect = as.numeric(ind_design_effect),
        ind_avg_hh_size = as.numeric(ind_avg_hh_size),
        ind_subpop_prop = as.numeric(ind_subpop_prop),
        ind_fpc = as.logical(ind_fpc),
        rate_indicator = as.character(rate_indicator),
        rate_expected_rate = as.numeric(rate_expected_rate),
        rate_precision = as.numeric(rate_precision),
        rate_nonresponse = as.numeric(rate_nonresponse),
        rate_design_effect = as.numeric(rate_design_effect),
        rate_recall_days = as.numeric(rate_recall_days),
        rate_avg_hh_size = as.numeric(rate_avg_hh_size),
        rate_fpc = as.logical(rate_fpc),
        teams = as.numeric(teams),
        avg_interview_time = as.numeric(avg_interview_time),
        clusters_per_day = as.numeric(clusters_per_day),
        enumerators_per_team = as.numeric(enumerators_per_team),
        avg_rest_time = as.numeric(avg_rest_time),
        avg_travel_time = as.numeric(avg_travel_time),
        start_time = as.character(start_time),
        end_time = as.character(end_time),
        General_HH_Sample_Size = as.numeric(General_HH_Sample_Size),
        Ind_Sample_Size = as.numeric(Ind_Sample_Size),
        Ind_HH_Sample_Size = as.numeric(Ind_HH_Sample_Size),
        Rate_Ind_Sample_Size = as.numeric(Rate_Ind_Sample_Size),
        Rate_PT_Sample_Size = as.numeric(Rate_PT_Sample_Size),
        Rate_HH_Sample_Size = as.numeric(Rate_HH_Sample_Size),
        Final_HH_Sample_Size = NA_real_,
        num_interview_per_enum_per_day = NA_real_,
        num_days = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phr_warning(
            message = phr_txt(
              "Stratum ID '{stratum_id}' already exists and will be overwritten."
            ),
            origin = "Sample$add_stratum",
            hint = phr_txt(
              "The existing row for '{stratum_id}' has been replaced with the new values."
            )
          )
          self$sample_table <- self$sample_table[
            self$sample_table$stratum_id != stratum_id,
            ,
            drop = FALSE
          ]
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }

      private$..touch()
      invisible(self)
    },

    #' @description Remove a stratum row by strata name.
    #' @param strata_name Character scalar naming the stratum to remove.
    remove_stratum = function(strata_name) {
      phr_assert(
        is.character(strata_name) &&
          length(strata_name) == 1L &&
          nzchar(strata_name),
        message = phr_txt(
          "strata_name must be a single non-empty character value."
        ),
        origin = "Sample$remove_stratum"
      )

      st <- self$sample_table
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        return(invisible(self))
      }

      stratum_col <- private$..resolve_stratum_name_col(st)
      phr_assert(
        !is.null(stratum_col),
        message = phr_txt(
          "sample_table does not contain a stratum name column."
        ),
        origin = "Sample$remove_stratum"
      )

      keep_rows <- as.character(st[[stratum_col]]) != strata_name
      self$sample_table <- st[keep_rows, , drop = FALSE]
      private$..touch()
      invisible(self)
    },

    #' @description Validate strata table structure.
    #' @return Logical.
    validate_strata_table = function() {
      out <- validate_strata_table(self$sample_table)
      self$validated <- out
      private$..touch()
      out
    },

    #' @description Calculate sample sizes for all strata rows.
    calculate_sample_sizes = function() {
      phr_assert(
        !is.null(self$sample_table) && nrow(self$sample_table) > 0,
        message = phr_txt("sample_table is empty. Call add_stratum() first."),
        origin = "Sample$calculate_sample_sizes"
      )
      self$sample_table <- calculate_sample_size_strata_table(self$sample_table)
      private$..touch()
      invisible(self)
    },

    #' @description Return unique sampling methods in the sample table.

    #' @description Return unique sampling methods in the sample table.
    #' @return Character vector of unique sampling methods.
    get_sampling_methods = function(type = c("site", "household")) {
      type <- match.arg(type)
      col_name <- switch(
        type,
        site = "sampling_method_site",
        household = "sampling_method_hh"
      )
      st <- self$sample_table
      if (is.null(st) || !(col_name %in% names(st))) {
        private$..touch()
        return(character(0))
      }
      m <- as.character(st[[col_name]])
      out <- unique(m[!is.na(m) & nzchar(m)])
      private$..touch()
      out
    },

    #' @description Return stratum names.
    #' @return Character vector of stratum names.
    get_strata_names = function() {
      st <- self$sample_table
      if (is.null(st) || nrow(st) == 0) {
        private$..touch()
        return(character(0))
      }
      col <- private$..resolve_stratum_name_col(st)
      if (is.null(col)) {
        private$..touch()
        return(character(0))
      }
      n <- as.character(st[[col]])
      out <- n[!is.na(n) & nzchar(n)]
      private$..touch()
      out
    }
  ),
  private = list(
    #' @description Update modified timestamp.
    #' @return Invisibly returns NULL.
    ..touch = function() {
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    #' @description Resolve the stratum name column from a sample table.
    #' @param st Data frame sample table.
    #' @return Character scalar naming the column, or NULL if not found.
    ..resolve_stratum_name_col = function(st) {
      if ("stratum_name" %in% names(st)) {
        "stratum_name"
      } else if ("Population_Name" %in% names(st)) {
        "Population_Name"
      } else if ("stratum_id" %in% names(st)) {
        "stratum_id"
      } else {
        NULL
      }
    }
  )
)
