#' SurveyProtocol R6 Class
#'
#' @description
#' Protocol subclass for survey-based assessments. `SurveyProtocol` extends
#' `Protocol` with sampling-frame management, sample-table synchronization,
#' survey sampling metadata, sample-size summaries, and Quarto parameters used
#' in survey Terms of Reference and reporting workflows.
#'
#' @details
#' `SurveyProtocol` maintains a nested `Sample` object and a nested
#' `SamplingFrame` object. It synchronizes key sampling outputs into public
#' fields, including strata names, sampling methods, sampling-frame population
#' summaries, and drawn samples. It also extends coherence checks to compare
#' strata defined in the sample table against strata available in the sampling
#' frame.
#'
#' The class is intended as a reusable base for survey protocol subclasses,
#' including IPHRA-style survey protocols with additional report tables,
#' active bindings, and tool-specific outputs.
#'
#' @section Inheritance:
#' Inherits from [`Protocol`], which itself inherits document generation,
#' framework, tool, metadata, and Quarto parameter functionality.
#'
#' @section Public fields:
#' \describe{
#'   \item{sample_object}{A `Sample` object storing sample table and sample-size workflows.}
#'   \item{sample_table}{Data frame mirror of `sample_object$sample_table`.}
#'   \item{strata_names}{Character vector of strata names synced from the sample object.}
#'   \item{sampling_methods}{Character vector of unique sampling methods.}
#'   \item{sampling_frame}{A `SamplingFrame` object storing and validating frame data.}
#'   \item{sampling_frame_strata_names}{Character vector of strata in the sampling frame.}
#'   \item{sampling_frame_strata_population}{Data frame of stratum-level population totals.}
#'   \item{drawn_sample}{Data frame of selected PSUs.}
#'   \item{drawn_sample_full}{Full sampling frame with sampling outputs.}
#'   \item{drawn_sample_from_frame}{Data-table-style sampled rows from the nested sample object.}
#' }
#'
#' @section Key methods:
#' \describe{
#'   \item{`initialize()`}{Create a new survey protocol.}
#'   \item{`set_sampling_frame()`}{Validate and store a sampling frame.}
#'   \item{`validate_strata_table()`}{Validate the nested sample table.}
#'   \item{`get_sample_table()`}{Return the sample table from the nested `Sample` object.}
#'   \item{`get_sampling_methods()`}{Return unique sampling methods.}
#'   \item{`get_strata_names()`}{Return stratum names from the sample table.}
#'   \item{`get_frame_column()`}{Extract a column from the sampling frame.}
#'   \item{`diagnose_coherence()`}{Run protocol coherence checks plus strata consistency checks.}
#'   \item{`post_sync_state()`}{Synchronize sampling and sampling-frame state after nested updates.}
#'   \item{`get_quarto_params()`}{Return survey-specific parameters for Quarto rendering.}
#' }
#'
#' @section Active bindings:
#' The class provides read-only active bindings for survey-type flags,
#' sampling-method flags, population summaries, sample-size summaries,
#' sample-size tables, fieldwork planning values, and formatted strata names.
#' These bindings are primarily used to populate Quarto parameters.
#'
#' @examples
#' \dontrun{
#' protocol <- SurveyProtocol$new(
#'   assessment_title = "Example Survey",
#'   country_name = "Example Country",
#'   month_year = "January 2027",
#'   framework_type = "ana"
#' )
#'
#' protocol$set_sampling_frame(frame_df)
#' protocol$get_strata_names()
#' protocol$get_sampling_methods()
#' protocol$diagnose_coherence()
#' }
#'
#' @importFrom R6 R6Class
#' @export
SurveyProtocol <- R6::R6Class(
  "SurveyProtocol",
  inherit = Protocol,
  public = list(
    #' @field sample_object A \code{\link{Sample}} object that stores the
    #'   strata/sample table and sample-size workflows.
    sample_object = NULL,

    #' @field sample_table Data frame mirror of \code{sample_object$sample_table}.
    sample_table = NULL,

    #' @field strata_names Character vector of strata names synced from
    #'   \code{sample_object}.
    strata_names = character(0),

    #' @field sampling_methods Character vector of unique sampling methods synced
    #'   from \code{sample_object}.
    sampling_methods = character(0),

    #' @field sampling_frame_strata_population Data frame of strata-level
    #'   population totals aggregated from \code{sampling_frame$log_df}.
    sampling_frame_strata_population = NULL,

    #' @field drawn_sample_from_frame Data-table-style data frame of sampled rows
    #'   synced from the nested \code{Sample} object.
    drawn_sample_from_frame = NULL,

    #' @field sampling_frame A \code{\link{SamplingFrame}} object holding and
    #'   validating the sampling frame data.  Initialised to an empty
    #'   \code{SamplingFrame} on construction; populated via
    #'   \code{set_sampling_frame()} or by passing a data frame on
    #'   initialisation.
    sampling_frame = NULL,

    #' @field sampling_frame_strata_names Character vector of unique strata names
    #'   currently available in the held \code{SamplingFrame}, when present.
    sampling_frame_strata_names = character(0),

    #' @field drawn_sample Data frame with selected PSUs (filtered from drawn_sample_full)
    drawn_sample = NULL,

    #' @field drawn_sample_full Full sampling frame with sampled_psu and allocated_sample columns
    drawn_sample_full = NULL,

    #' @description
    #' Creates a new SurveyProtocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  One of
    #'   \code{"none"} or \code{"ana"}.  Defaults to \code{"none"}.
    #' @param sampling_frame Optional data frame to initialise the
    #'   \code{\link{SamplingFrame}} with.  When \code{NULL} (default), an empty
    #'   \code{SamplingFrame} is created.
    #' @param reference_doc_filename Optional document template filename/path.
    #' @param reference_ppt_filename Optional PowerPoint template filename/path.
    #' @return A new SurveyProtocol object
    initialize = function(
      assessment_title = NULL,
      country_name = NULL,
      month_year = NULL,
      framework_type = "none",
      sampling_frame = NULL,
      reference_doc_filename = NULL,
      reference_ppt_filename = NULL
    ) {
      super$initialize(
        assessment_title = assessment_title,
        country_name = country_name,
        month_year = month_year,
        framework_type = framework_type,
        reference_doc_filename = reference_doc_filename,
        reference_ppt_filename = reference_ppt_filename
      )
      self$sample_object <- Sample$new()
      self$sampling_frame <- SamplingFrame$new(log_df = sampling_frame)
      private$..sync_state()
      invisible(self)
    },

    #' @description Set the sampling frame
    #'
    #' Validates the frame using existing \pkg{phr} validators and
    #' \code{validate_sampling_frame()}, then stores it.  If the frame does
    #' not have an \code{inclusion} column, one is added with all \code{TRUE}
    #' values.
    #'
    #' @param frame Data frame. Must contain at minimum a \code{psu} column
    #'   (primary sampling unit identifier).  A \code{population_size} column
    #'   is required for proportional, pps_cluster, and rlc sampling methods.
    #'   A \code{stratum} column enables stratified sampling.
    #' @return Invisibly returns \code{self} for method chaining.
    set_sampling_frame = function(frame) {
      phr_try(
        {
          # 1. Confirm it is a data frame and not empty
          phr_validate_dataframe(
            frame,
            origin = "SurveyProtocol$set_sampling_frame",
            soft = FALSE
          )
          phr_assert(
            nrow(frame) > 0,
            message = phr_txt("Sampling frame is empty."),
            origin = "SurveyProtocol$set_sampling_frame",
            hint = phr_txt("Provide a data frame with at least one PSU row.")
          )

          # 2. Run validate_sampling_frame — stops on hard issues
          val_result <- validate_sampling_frame(frame)
          if (!val_result$valid) {
            hard_issues <- val_result$issues[setdiff(
              names(val_result$issues),
              "missing_inclusion"
            )]
            if (length(hard_issues) > 0) {
              phr_error(
                message = phr_txt(
                  "Sampling frame validation failed: {paste(names(hard_issues), unlist(hard_issues), sep=': ', collapse='; ')}"
                ),
                origin = "SurveyProtocol$set_sampling_frame",
                hint = phr_txt(
                  "Ensure the frame has 'stratum', 'psu', and 'population_size' columns, and that any 'inclusion' column contains only TRUE/FALSE values."
                )
              )
            }
          }

          # 3. Add inclusion column (all TRUE) if absent
          if (!"inclusion" %in% names(frame)) {
            frame$inclusion <- TRUE
            phr_message(
              phr_txt(
                "'inclusion' column not found — defaulting all PSUs to TRUE."
              ),
              origin = "SurveyProtocol$set_sampling_frame"
            )
          }

          self$set_nested(
            field = "sampling_frame",
            member = "log_df",
            value = tibble::as_tibble(frame)
          )
          private$..sync_state()
          private$..touch()
          self$diagnose_coherence()
          phr_message(
            phr_txt("Sampling frame set with {nrow(frame)} PSUs."),
            origin = "SurveyProtocol$set_sampling_frame"
          )
        },
        on_error = "abort",
        origin = "SurveyProtocol$set_sampling_frame"
      )
      invisible(self)
    },

    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.
    #'
    #' @return \code{TRUE} if valid, \code{FALSE} otherwise.
    validate_strata_table = function() {
      if (
        is.null(self$sample_object) || !inherits(self$sample_object, "Sample")
      ) {
        return(FALSE)
      }
      isTRUE(self$sample_object$validate_strata_table())
    },

    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      if (
        is.null(self$sample_object) || !inherits(self$sample_object, "Sample")
      ) {
        return(NULL)
      }
      self$sample_object$get_sample_table()
    },

    # ── Sampling helpers ────────────────────────────────────────────────────

    #' @description Return the unique sampling methods used across all strata.
    #'
    #' Reads the \code{sampling_method} column of \code{self$get_sample_table()}.
    #'
    #' @return Character vector of unique, non-NA sampling method values.
    #'   Empty character vector when no sample table is set.
    get_sampling_methods = function() {
      if (
        is.null(self$sample_object) || !inherits(self$sample_object, "Sample")
      ) {
        return(character(0))
      }
      self$sample_object$get_sampling_methods()
    },

    #' @description Return the stratum names from the sample table.
    #'
    #' Uses \code{Population_Name} when available, falling back to
    #' \code{stratum_id}.
    #'
    #' @return Character vector of stratum names.  Empty character vector
    #'   when no sample table is set.
    get_strata_names = function() {
      if (
        is.null(self$sample_object) || !inherits(self$sample_object, "Sample")
      ) {
        return(character(0))
      }
      self$sample_object$get_strata_names()
    },

    #' @description Extract a column vector from the sampling frame, optionally
    #'   filtered to a specific stratum and/or to included PSUs only.
    #'
    #' @param col_name Character. Column to extract.
    #' @param strata Optional character vector of stratum names to filter to.
    #'   \code{NULL} (default) returns all strata.
    #' @param included_only Logical. When \code{TRUE} (default) only return
    #'   rows where the \code{inclusion} column is \code{TRUE}.
    #' @return Vector of values from the requested column.  \code{NULL} when
    #'   the sampling frame is not set or the column does not exist.
    get_frame_column = function(col_name, strata = NULL, included_only = TRUE) {
      sf <- if (!is.null(self$sampling_frame)) {
        self$sampling_frame$log_df
      } else {
        NULL
      }
      if (is.null(sf) || !is.data.frame(sf) || !col_name %in% names(sf)) {
        return(NULL)
      }
      df <- as.data.frame(sf, stringsAsFactors = FALSE)
      if (isTRUE(included_only) && "inclusion" %in% names(df)) {
        df <- df[!is.na(df$inclusion) & df$inclusion, , drop = FALSE]
      }
      if (!is.null(strata) && "stratum" %in% names(df)) {
        df <- df[df$stratum %in% as.character(strata), , drop = FALSE]
      }
      df[[col_name]]
    },

    #' @description Override \code{diagnose_coherence} to additionally check
    #'   strata consistency between the sampling frame and the sample table.
    #'
    #' Calls the base \code{Protocol$diagnose_coherence()} and then appends
    #' strata consistency checks to \code{self$issues_coherence}.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    diagnose_coherence = function() {
      super$diagnose_coherence()

      # Check strata consistency between frame and sample table
      st <- self$get_sample_table()
      if (
        !is.null(st) &&
          !is.null(self$sampling_frame) &&
          nrow(self$sampling_frame$log_df) > 0
      ) {
        table_strata <- st$stratum_id
        frame_strata <- unique(self$sampling_frame$log_df$stratum)

        if (!setequal(table_strata, frame_strata)) {
          missing_in_frame <- setdiff(table_strata, frame_strata)
          missing_in_table <- setdiff(frame_strata, table_strata)

          if (length(missing_in_frame) > 0) {
            self$issues_coherence$strata_missing_in_frame <- paste(
              "Strata in sample table but not in frame:",
              paste(missing_in_frame, collapse = ", ")
            )
          }
          if (length(missing_in_table) > 0) {
            self$issues_coherence$strata_missing_in_table <- paste(
              "Strata in frame but not in sample table:",
              paste(missing_in_table, collapse = ", ")
            )
          }
        }
      }

      invisible(self)
    },

    #' @description Post-sync hook for sampling-related state.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    post_sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      super$post_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      if (isTRUE(private$..post_sync_guard)) {
        return(invisible(NULL))
      }
      private$..post_sync_guard <- TRUE
      on.exit(
        {
          private$..post_sync_guard <- FALSE
        },
        add = TRUE
      )
      private$..sync_sampling_state()
      private$..sync_sample_frame_state()
      invisible(NULL)
    },
    #' @description Get Quarto parameters for rendering.
    #' @return A named list of parameters to pass to Quarto rendering.
    get_quarto_params = function() {
      params <- super$get_quarto_params()

      c(
        params,
        list(
          rate_survey = self$.rate_survey,
          individual_survey = self$.individual_survey,
          general_survey = self$.general_survey,
          ind_indicator = self$.ind_indicator,
          rate_indicator = self$.rate_indicator,
          site_selection_srs = self$.site_selection_srs,
          site_selection_systematic = self$.site_selection_systematic,
          site_selection_exhaustive = self$.site_selection_exhaustive,
          site_selection_cluster = self$.site_selection_cluster,
          site_selection_purposive = self$.site_selection_purposive,
          hh_selection_srs = self$.hh_selection_srs,
          hh_selection_systematic = self$.hh_selection_systematic,
          hh_selection_rlc = self$.hh_selection_rlc,
          multiple_methods = self$.multiple_methods,
          multiple_strata = self$.multiple_strata,
          fpc = self$.fpc,
          total_population_size = self$.total_population_size,
          total_population_size_included = self$.total_population_size_included,
          total_population_size_excluded = self$.total_population_size_excluded,
          total_population_per_strata_included = self$.total_population_per_strata_included,
          num_geographic_units = self$.num_geographic_units,
          num_strata_units = self$.num_strata_units,
          num_other_units = self$.num_other_units,
          stratified_strata_names_srs_srs = self$.stratified_strata_names_srs_srs,
          stratified_strata_names_srs_systematic = self$.stratified_strata_names_srs_systematic,
          stratified_strata_names_srs_rlc = self$.stratified_strata_names_srs_rlc,
          stratified_strata_names_systematic_srs = self$.stratified_strata_names_systematic_srs,
          stratified_strata_names_systematic_systematic = self$.stratified_strata_names_systematic_systematic,
          stratified_strata_names_systematic_rlc = self$.stratified_strata_names_systematic_rlc,
          stratified_strata_names_proportional_srs = self$.stratified_strata_names_proportional_srs,
          stratified_strata_names_proportional_systematic = self$.stratified_strata_names_proportional_systematic,
          stratified_strata_names_proportional_rlc = self$.stratified_strata_names_proportional_rlc,
          stratified_strata_names_cluster_srs = self$.stratified_strata_names_cluster_srs,
          stratified_strata_names_cluster_systematic = self$.stratified_strata_names_cluster_systematic,
          stratified_strata_names_cluster_rlc = self$.stratified_strata_names_cluster_rlc,
          stratified_strata_names_purposive_srs = self$.stratified_strata_names_purposive_srs,
          stratified_strata_names_purposive_systematic = self$.stratified_strata_names_purposive_systematic,
          stratified_strata_names_purposive_rlc = self$.stratified_strata_names_purposive_rlc,
          stratified_strata_names_site_srs = self$.stratified_strata_names_site_srs,
          stratified_strata_names_site_systematic = self$.stratified_strata_names_site_systematic,
          stratified_strata_names_site_proportional = self$.stratified_strata_names_site_proportional,
          stratified_strata_names_site_exhaustive = self$.stratified_strata_names_site_exhaustive,
          stratified_strata_names_site_cluster = self$.stratified_strata_names_site_cluster,
          stratified_strata_names_site_purposive = self$.stratified_strata_names_site_purposive,
          stratified_strata_names_hh_srs = self$.stratified_strata_names_hh_srs,
          stratified_strata_names_hh_systematic = self$.stratified_strata_names_hh_systematic,
          stratified_strata_names_hh_rlc = self$.stratified_strata_names_hh_rlc,
          sample_size_general_households = self$.sample_size_general_households,
          sample_size_ind_persons = self$.sample_size_ind_persons,
          sample_size_ind_hh = self$.sample_size_ind_hh,
          sample_size_rate_persons = self$.sample_size_rate_persons,
          sample_size_rate_persontime = self$.sample_size_rate_persontime,
          sample_size_rate_hh = self$.sample_size_rate_hh,
          sample_size_hh_final = self$.sample_size_hh_final,
          sample_size_general_table_df = self$.sample_size_general_table_df,
          sample_size_ind_table_df = self$.sample_size_ind_table_df,
          sample_size_rate_table_df = self$.sample_size_rate_table_df,
          n_sites = self$.n_sites,
          cluster_size = self$.cluster_size,
          num_enumerators_per_team = self$.num_enumerators_per_team,
          num_days_data_collection = self$.num_days_data_collection,
          strata_names = self$.strata_names
        )
      )
    }
  ),

  active = list(
    .rate_survey = function(value) {
      st <- private$..sample_table_from_nested()
      if (!missing(value)) {
        return(invisible(FALSE))
      }

      if (
        !is.null(st) &&
          "rate_indicator" %in% names(st) &&
          any(!is.na(st$rate_indicator))
      ) {
        return(TRUE)
      }
      return(FALSE)
    },
    .individual_survey = function(value) {
      st <- private$..sample_table_from_nested()
      if (!missing(value)) {
        return(invisible(FALSE))
      }

      if (
        !is.null(st) &&
          "ind_indicator" %in% names(st) &&
          any(!is.na(st$ind_indicator))
      ) {
        return(TRUE)
      }
      return(FALSE)
    },
    .general_survey = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }

      return(
        !self$.rate_survey &&
          !self$.individual_survey
      )
    },
    .ind_indicator = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"ind_indicator" %in% names(st)) {
        return(NULL)
      }
      vals <- as.character(st$ind_indicator)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      paste(unique(vals), collapse = " ")
    },
    .rate_indicator = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"rate_indicator" %in% names(st)) {
        return(NULL)
      }
      vals <- as.character(st$rate_indicator)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      paste(unique(vals), collapse = " ")
    },
    .site_selection_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_site("simple_random")
    },

    .site_selection_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_site("systematic")
    },

    .site_selection_exhaustive = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_site("proportional")
    },

    .site_selection_cluster = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_site("cluster")
    },

    .site_selection_purposive = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_site("purposive")
    },

    .hh_selection_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_household("simple_random")
    },

    .hh_selection_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_household("systematic")
    },

    .hh_selection_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..sample_has_any_method_household("rlc")
    },

    .multiple_methods = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      methods_site <- private$..sample_methods_site_used()
      if (length(methods_site) > 1L) {
        return(TRUE)
      }
      methods_hh <- private$..sample_methods_household_used()
      if (length(methods_hh) > 1L) {
        return(TRUE)
      }
      FALSE
    },

    .multiple_strata = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      is.data.frame(st) && nrow(st) > 1L
    },

    .fpc = function(value) {},

    .total_population_size = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      sf_pop <- self$sampling_frame_strata_population
      if (
        !is.null(sf_pop) &&
          is.data.frame(sf_pop) &&
          "total_population" %in% names(sf_pop)
      ) {
        return(sum(as.numeric(sf_pop$total_population), na.rm = TRUE))
      }
      sf_log <- tryCatch(
        self$access_nested(field = "sampling_frame", member = "log_df"),
        error = function(e) NULL
      )
      if (
        !is.null(sf_log) &&
          is.data.frame(sf_log) &&
          "population_size" %in% names(sf_log)
      ) {
        return(sum(as.numeric(sf_log$population_size), na.rm = TRUE))
      }
      NULL
    },

    .total_population_size_included = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      sf_log <- tryCatch(
        self$access_nested(field = "sampling_frame", member = "log_df"),
        error = function(e) NULL
      )
      if (
        !is.null(sf_log) &&
          is.data.frame(sf_log) &&
          "population_size" %in% names(sf_log) &&
          "inclusion" %in% names(sf_log)
      ) {
        included_rows <- !is.na(sf_log$inclusion) & sf_log$inclusion
        return(sum(
          as.numeric(sf_log$population_size[included_rows]),
          na.rm = TRUE
        ))
      }
      NULL
    },

    .total_population_size_excluded = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      sf_log <- tryCatch(
        self$access_nested(field = "sampling_frame", member = "log_df"),
        error = function(e) NULL
      )
      if (
        !is.null(sf_log) &&
          is.data.frame(sf_log) &&
          "population_size" %in% names(sf_log) &&
          "inclusion" %in% names(sf_log)
      ) {
        excluded_rows <- !is.na(sf_log$inclusion) & !sf_log$inclusion
        return(sum(
          as.numeric(sf_log$population_size[excluded_rows]),
          na.rm = TRUE
        ))
      }
      NULL
    },

    .total_population_per_strata_included = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      sf_pop <- self$sampling_frame_strata_population
      if (
        is.null(sf_pop) ||
          !is.data.frame(sf_pop) ||
          !all(c("stratum", "total_population") %in% names(sf_pop))
      ) {
        return(NULL)
      }
      parts <- paste0(sf_pop$stratum, " (", sf_pop$total_population, ")")
      paste(parts, collapse = ", ")
    },

    .num_geographic_units = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- tryCatch(self$get_sample_table(), error = function(e) NULL)
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        return(NULL)
      }
      nrow(st)
    },
    .num_strata_units = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- tryCatch(self$get_sample_table(), error = function(e) NULL)
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        return(NULL)
      }
      nrow(st)
    },
    .num_other_units = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- tryCatch(self$get_sample_table(), error = function(e) NULL)
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        return(NULL)
      }
      nrow(st)
    },

    # ── Stratified strata names active bindings ─────────────────────────────
    # Helper: return TRUE if any strata row has site_method AND hh_method
    # The sample table's sampling_method encodes the combined site+hh method.
    # IPHRA mapping:
    #   simple_random         -> site=srs,  hh=srs
    #   systematic            -> site=systematic, hh=systematic
    #   simple_random_rlc     -> site=srs,  hh=rlc
    #   systematic_rlc        -> site=systematic, hh=rlc
    #   proportional          -> site=proportional, hh=srs
    #   proportional_rlc      -> site=proportional, hh=rlc
    #   pps_cluster           -> site=cluster, hh=srs
    #   pps_rlc               -> site=cluster, hh=rlc
    #   purposive             -> site=purposive, hh=srs (default)

    .stratified_strata_names_srs_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "simple_random",
        method_hh = "simple_random"
      )
    },
    .stratified_strata_names_srs_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "simple_random",
        method_hh = "systematic"
      )
    },
    .stratified_strata_names_srs_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "simple_random",
        method_hh = "rlc"
      )
    },
    .stratified_strata_names_systematic_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "systematic",
        method_hh = "simple_random"
      )
    },
    .stratified_strata_names_systematic_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "systematic",
        method_hh = "systematic"
      )
    },
    .stratified_strata_names_systematic_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "systematic",
        method_hh = "rlc"
      )
    },
    .stratified_strata_names_proportional_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "proportional",
        method_hh = "simple_random"
      )
    },
    .stratified_strata_names_proportional_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "proportional",
        method_hh = "systematic"
      )
    },
    .stratified_strata_names_proportional_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "proportional",
        method_hh = "rlc"
      )
    },
    .stratified_strata_names_cluster_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "cluster",
        method_hh = "simple_random"
      )
    },
    .stratified_strata_names_cluster_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "cluster",
        method_hh = "systematic"
      )
    },
    .stratified_strata_names_cluster_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "cluster",
        method_hh = "rlc"
      )
    },
    .stratified_strata_names_purposive_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "purposive",
        method_hh = "simple_random"
      )
    },
    .stratified_strata_names_purposive_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "purposive",
        method_hh = "systematic"
      )
    },
    .stratified_strata_names_purposive_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(
        method_site = "purposive",
        method_hh = "rlc"
      )
    },
    .stratified_strata_names_site_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_site = "simple_random")
    },
    .stratified_strata_names_site_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_site = "systematic")
    },
    .stratified_strata_names_site_exhaustive = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_site = "proportional")
    },
    .stratified_strata_names_site_cluster = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_site = "cluster")
    },
    .stratified_strata_names_site_purposive = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_site = "purposive")
    },
    .stratified_strata_names_hh_srs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_hh = "simple_random")
    },
    .stratified_strata_names_hh_systematic = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_hh = "systematic")
    },
    .stratified_strata_names_hh_rlc = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..strata_names_for_method(method_hh = "rlc")
    },
    .sample_size_general_households = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"General_HH_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$General_HH_Sample_Size), na.rm = TRUE)
    },
    .sample_size_ind_persons = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Ind_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Ind_Sample_Size), na.rm = TRUE)
    },
    .sample_size_ind_hh = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Ind_HH_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Ind_HH_Sample_Size), na.rm = TRUE)
    },
    .sample_size_rate_persons = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Rate_Ind_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Rate_Ind_Sample_Size), na.rm = TRUE)
    },
    .sample_size_rate_persontime = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Rate_PT_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Rate_PT_Sample_Size), na.rm = TRUE)
    },
    .sample_size_rate_hh = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Rate_HH_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Rate_HH_Sample_Size), na.rm = TRUE)
    },
    .sample_size_hh_final = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"Final_HH_Sample_Size" %in% names(st)) {
        return(NULL)
      }
      sum(as.numeric(st$Final_HH_Sample_Size), na.rm = TRUE)
    },
    .sample_size_general_table_df = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      table <- table_sample_size_general(st)
      return(table)
    },
    .sample_size_ind_table_df = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      table <- table_sample_size_individual(st)
      return(table)
    },
    .sample_size_rate_table_df = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      table <- table_sample_size_rate(st)
      return(table)
    },
    .n_sites = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"n_sites" %in% names(st)) {
        return(NULL)
      }
      vals <- as.numeric(st$n_sites)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      sum(vals)
    },
    .cluster_size = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"cluster_size" %in% names(st)) {
        return(NULL)
      }
      vals <- as.numeric(st$cluster_size)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      max(vals)
    },
    .num_enumerators_per_team = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"enumerators_per_team" %in% names(st)) {
        return(NULL)
      }
      vals <- as.numeric(st$enumerators_per_team)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      max(vals)
    },
    .num_days_data_collection = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"num_days" %in% names(st)) {
        return(NULL)
      }
      vals <- as.numeric(st$num_days)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      max(vals)
    },
    .strata_names = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st)) {
        return(NULL)
      }
      col <- if ("stratum_name" %in% names(st)) {
        "stratum_name"
      } else if ("stratum_id" %in% names(st)) {
        "stratum_id"
      } else {
        NULL
      }
      if (is.null(col)) {
        return(NULL)
      }
      vals <- as.character(st[[col]])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) {
        return(NULL)
      }
      paste(vals, collapse = ", ")
    }
  ),

  private = list(
    #' @description Extract the sample table from the nested Sample object.
    #'
    #' Safely accesses the sample table via \code{access_nested()}, returning
    #' \code{NULL} on error.
    #'
    #' @return Data frame containing the sample table, or \code{NULL} if
    #'   unavailable or on error.
    #' @keywords internal
    #' @noRd
    ..sample_table_from_nested = function() {
      tryCatch(
        self$access_nested(
          field = "sample_object",
          member = "get_sample_table"
        ),
        error = function(e) NULL
      )
    },

    #' @description Get unique sampling methods used across all strata.
    #'
    #' Extracts the \code{sampling_method_site} column from the sample table,
    #' trims whitespace, converts to lowercase, and returns unique non-empty
    #' values.
    #'
    #' @return Character vector of unique sampling methods in lowercase.
    #'   Empty character vector if no sample table exists or no methods found.
    #' @keywords internal
    #' @noRd
    ..sample_methods_site_used = function() {
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"sampling_method_site" %in% names(st)) {
        return(character(0))
      }
      methods <- trimws(tolower(as.character(st$sampling_method_site)))
      methods <- methods[!is.na(methods) & nzchar(methods)]
      unique(methods)
    },

    #' @description Get unique sampling methods used across all strata.
    #'
    #' Extracts the \code{sampling_method_hh} column from the sample table,
    #' trims whitespace, converts to lowercase, and returns unique non-empty
    #' values.
    #'
    #' @return Character vector of unique sampling methods in lowercase.
    #'   Empty character vector if no sample table exists or no methods found.
    #' @keywords internal
    #' @noRd
    ..sample_methods_household_used = function() {
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || !"sampling_method_hh" %in% names(st)) {
        return(character(0))
      }
      methods <- trimws(tolower(as.character(st$sampling_method_hh)))
      methods <- methods[!is.na(methods) & nzchar(methods)]
      unique(methods)
    },

    #' @description Check if any of the specified sampling methods are used.
    #'
    #' Compares the provided methods against those currently in use across
    #' all strata.
    #'
    #' @param methods Character vector of sampling method names to check.
    #' @return \code{TRUE} if any provided method matches a method in use,
    #'   \code{FALSE} otherwise.
    #' @keywords internal
    #' @noRd
    ..sample_has_any_method_site = function(methods) {
      methods_used <- private$..sample_methods_site_used()
      length(intersect(methods_used, tolower(as.character(methods)))) > 0L
    },

    #' @description Check if any of the specified sampling methods are used.
    #'
    #' Compares the provided methods against those currently in use across
    #' all strata.
    #'
    #' @param methods Character vector of sampling method names to check.
    #' @return \code{TRUE} if any provided method matches a method in use,
    #'   \code{FALSE} otherwise.
    #' @keywords internal
    #' @noRd
    ..sample_has_any_method_household = function(methods) {
      methods_used <- private$..sample_methods_household_used()
      length(intersect(methods_used, tolower(as.character(methods)))) > 0L
    },

    #' @description Return strata names for a given sampling method.
    #'
    #' Checks if any strata rows in the sample table match the specified
    #' site-level and/or household-level sampling methods.
    #'
    #' @param method_site Character. Site-level sampling method to filter on.
    #'   When \code{NULL} (default), no site-level filtering is applied.
    #' @param method_hh Character. Household-level sampling method to filter on.
    #'   When \code{NULL} (default), no household-level filtering is applied.
    #' @return Character vector of strata names matching the specified method(s).
    #'   Returns an empty character vector if no matches found or sample table
    #'   unavailable.
    #'
    #' @details
    #' If both \code{method_site} and \code{method_hh} are provided, returns
    #' strata names where both conditions are met. If only one is provided,
    #' filters by that criterion alone.
    #'
    #' @keywords internal
    #' @noRd
    ..strata_names_for_method = function(method_site = NULL, method_hh = NULL) {
      st <- private$..sample_table_from_nested()
      if (!is.data.frame(st) || nrow(st) == 0L) {
        return(character(0))
      }

      mask <- rep(TRUE, nrow(st))

      if (!is.null(method_site) && "sampling_method_site" %in% names(st)) {
        site_methods <- trimws(tolower(as.character(st$sampling_method_site)))
        mask <- mask &
          !is.na(site_methods) &
          site_methods == tolower(method_site)
      }

      if (!is.null(method_hh) && "sampling_method_hh" %in% names(st)) {
        hh_methods <- trimws(tolower(as.character(st$sampling_method_hh)))
        mask <- mask & !is.na(hh_methods) & hh_methods == tolower(method_hh)
      }

      if (!any(mask)) {
        return(character(0))
      }

      col <- if ("stratum_name" %in% names(st)) {
        "stratum_name"
      } else if ("stratum_id" %in% names(st)) {
        "stratum_id"
      } else {
        NULL
      }

      if (is.null(col)) {
        return(character(0))
      }

      strata <- as.character(st[[col]][mask])
      strata <- strata[!is.na(strata) & nzchar(strata)]
      strata
    },

    #' @description Check if any household indicators are present.
    #'
    #' Verifies that a household tool role exists and contains at least one
    #' of the specified indicator codes.
    #'
    #' @param indicator_codes Character vector of indicator codes to check.
    #' @return \code{TRUE} if the household tool role exists and any indicator
    #'   codes match, \code{FALSE} otherwise.
    #' @keywords internal
    #' @noRd
    ..household_has_any_indicator = function(indicator_codes) {
      if (!private$..has_tool_role("household")) {
        return(FALSE)
      }
      hh_codes <- tryCatch(
        self$access_nested(
          field = "tools",
          role = "household",
          member = "get_indicator_codes"
        ),
        error = function(e) character(0)
      )
      hh_codes <- trimws(as.character(hh_codes))
      hh_codes <- hh_codes[!is.na(hh_codes) & nzchar(hh_codes)]
      length(intersect(hh_codes, as.character(indicator_codes))) > 0L
    },

    #' @description Guard to prevent recursive calls during state synchronization.
    #' @keywords internal
    #' @noRd
    ..post_sync_guard = FALSE,

    #' @description Synchronize sampling-related state fields.
    #'
    #' Extracts sampling metadata from the nested \code{Sample} object and
    #' updates public fields including \code{sample_table}, \code{strata_names},
    #' \code{sampling_methods}, \code{drawn_sample}, and related metadata.
    #'
    #' Populates the \code{metadata} list with:
    #' \itemize{
    #'   \item \code{sampling_strata_names}: Character vector of strata names
    #'   \item \code{sampling_method_flags}: Named logical list indicating
    #'     presence of known sampling methods
    #'   \item \code{target_strata}: Named list mapping stratum IDs to names
    #' }
    #'
    #' @return Invisibly returns \code{NULL}.
    #' @keywords internal
    #' @noRd
    ..sync_sampling_state = function() {
      st <- tryCatch(
        self$access_nested(
          field = "sample_object",
          member = "get_sample_table"
        ),
        error = function(e) NULL
      )
      strata_names <- tryCatch(
        self$access_nested(
          field = "sample_object",
          member = "get_strata_names"
        ),
        error = function(e) character(0)
      )
      methods_used <- tryCatch(
        self$access_nested(
          field = "sample_object",
          member = "get_sampling_methods"
        ),
        error = function(e) character(0)
      )
      drawn_sample <- tryCatch(
        self$access_nested(field = "sample_object", member = "drawn_sample"),
        error = function(e) NULL
      )
      drawn_sample_full <- tryCatch(
        self$access_nested(
          field = "sample_object",
          member = "drawn_sample_full"
        ),
        error = function(e) NULL
      )

      self$sample_table <- if (is.data.frame(st)) st else NULL
      self$strata_names <- unique(as.character(strata_names %||% character(0)))

      if (is.null(methods_used)) {
        methods_used <- character(0)
      }
      methods_used <- unique(trimws(tolower(as.character(methods_used))))
      methods_used <- methods_used[!is.na(methods_used) & nzchar(methods_used)]
      self$sampling_methods <- methods_used

      known_methods <- c(
        "simple_random",
        "proportional",
        "pps_cluster",
        "pps_rlc",
        "systematic",
        "simple_random_rlc",
        "systematic_rlc",
        "proportional_rlc",
        "purposive"
      )
      self$metadata$sampling_strata_names <- as.character(
        self$strata_names %||% character(0)
      )
      self$metadata$sampling_method_flags <- setNames(
        as.list(known_methods %in% methods_used),
        known_methods
      )

      if (!is.null(st) && nrow(st) > 0) {
        strata_ids <- as.character(st$stratum_id)
        strata_names <- as.character(st$stratum_name)
        self$metadata$target_strata <- setNames(
          as.list(strata_names),
          strata_ids
        )
      } else {
        self$metadata$target_strata <- list()
      }
      self$drawn_sample <- drawn_sample
      self$drawn_sample_from_frame <- if (is.data.frame(drawn_sample)) {
        data.table::as.data.table(drawn_sample)
      } else {
        NULL
      }
      self$drawn_sample_full <- drawn_sample_full
      invisible(NULL)
    },

    #' @description Synchronize sampling frame state fields.
    #'
    #' Extracts strata information from the \code{SamplingFrame} object and
    #' updates \code{sampling_frame_strata_names} and
    #' \code{sampling_frame_strata_population}. Aggregates population sizes
    #' by stratum when available.
    #'
    #' @return Invisibly returns \code{NULL}.
    #' @keywords internal
    #' @noRd
    ..sync_sample_frame_state = function() {
      sf <- tryCatch(
        self$access_nested(field = "sampling_frame", member = "log_df"),
        error = function(e) NULL
      )
      if (
        is.null(sf) ||
          !is.data.frame(sf) ||
          nrow(sf) == 0L ||
          !"stratum" %in% names(sf)
      ) {
        self$sampling_frame_strata_names <- character(0)
        self$sampling_frame_strata_population <- NULL
        return(invisible(NULL))
      }

      vals <- as.character(sf$stratum)
      self$sampling_frame_strata_names <- unique(vals[
        !is.na(vals) & nzchar(vals)
      ])

      sf2 <- sf[
        !is.na(sf$stratum) & nzchar(as.character(sf$stratum)),
        ,
        drop = FALSE
      ]
      if (nrow(sf2) == 0L) {
        self$sampling_frame_strata_population <- NULL
        return(invisible(NULL))
      }

      if ("population_size" %in% names(sf2)) {
        agg <- stats::aggregate(
          sf2$population_size,
          by = list(stratum = sf2$stratum),
          FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
        )
        names(agg)[2] <- "total_population"
      } else {
        agg <- stats::aggregate(
          rep(1, nrow(sf2)),
          by = list(stratum = sf2$stratum),
          FUN = sum
        )
        names(agg)[2] <- "total_population"
      }
      self$sampling_frame_strata_population <- as.data.frame(
        agg,
        stringsAsFactors = FALSE
      )
      invisible(NULL)
    }
  )
)
