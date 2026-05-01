#' SurveyProtocol R6 Class
#'
#' @description
#' Subclass of \code{\link{Protocol}} for managing survey-based protocol
#' workflows.  Extends the base \code{Protocol} class with strata definition,
#' sample size calculations, sampling frame management, and sample drawing.
#'
#' In addition to all capabilities inherited from \code{Protocol}, this class
#' provides:
#' 1. Strata Definition and Sample Size Calculations (\code{add_stratum()})
#' 2. Sampling Frame Validation (\code{set_sampling_frame()})
#' 3. Sample Drawing (\code{draw_sample()})
#'
#' @importFrom R6 R6Class
#' @export
SurveyProtocol <- R6::R6Class(
  "SurveyProtocol",
  inherit = Protocol,
  public = list(
    #' @field sample_table Master data frame with one row per stratum and all
    #'   relevant population, sample-size, and logistics parameters
    sample_table = NULL,

    #' @field sampling_frame Data frame with sampling units and strata
    sampling_frame = NULL,

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
    #' @return A new SurveyProtocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none") {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = framework_type
      )
      invisible(self)
    },

    #' @description Add a stratum row to the master sample table
    #'
    #' Each call appends one row to \code{sample_table}.  Every column in the
    #' master table is included; columns that are not supplied default to
    #' \code{NA}.  Parameters that have clear equivalents in the master table
    #' schema are mapped automatically (e.g. \code{population_size} maps to
    #' \code{Total_Population}).
    #'
    #' @param stratum_id Character. Unique identifier for the stratum (used as
    #'   the row key and for cross-referencing the sampling frame).
    #' @param stratum_name Character. Human-readable name (stored as
    #'   \code{Population_Name}).
    #' @param population_size Numeric. Total population for this stratum
    #'   (stored as \code{Total_Population}).
    #' @param total_households Numeric. Total number of households (stored as
    #'   \code{Total_Households}).  Defaults to \code{NA}.
    #' @param sampling_method Character. Primary sampling method for the
    #'   stratum (stored as \code{Sampling_Method}).  Also accepted via the
    #'   legacy alias \code{allocation_method}.  Defaults to \code{"srs"}.
    #' @param allocation_method Deprecated alias for \code{sampling_method}.
    #' @param pop_indicator Character. Indicator label for population-level
    #'   sample size calculation (default \code{"General"}).
    #' @param pop_expected_prevalence Numeric. Expected prevalence used for
    #'   population-level sample size (\%).
    #' @param pop_precision Numeric. Desired precision for population-level
    #'   estimate (\%).
    #' @param pop_nonresponse Numeric. Expected non-response rate for
    #'   population-level calculation (\%).
    #' @param pop_design_effect Numeric. Design effect for population-level
    #'   calculation.  Also accepted via the legacy alias \code{design_effect}.
    #' @param pop_fpc Logical. Apply finite population correction at population
    #'   level?  Defaults to \code{FALSE}.
    #' @param General_HH_Sample_Size Numeric. Calculated (or placeholder) general
    #'   household-level sample size (from population-level calculation).
    #' @param ind_indicator Character. Indicator label for individual-level
    #'   sample size calculation.
    #' @param ind_expected_prevalence Numeric. Expected prevalence for
    #'   individual-level calculation (\%).
    #' @param ind_precision Numeric. Desired precision for individual-level
    #'   estimate (\%).
    #' @param ind_nonresponse Numeric. Expected non-response rate for
    #'   individual-level calculation (\%).
    #' @param ind_design_effect Numeric. Design effect for individual-level
    #'   calculation.
    #' @param ind_avg_hh_size Numeric. Average household size (individual
    #'   level).
    #' @param ind_subpop_prop Numeric. Sub-population proportion (\%) for
    #'   individual-level calculation.
    #' @param ind_fpc Logical. Apply finite population correction at individual
    #'   level?  Defaults to \code{FALSE}.
    #' @param Ind_Sample_Size Numeric. Calculated individual-level sample size
    #'   (number of individuals).
    #' @param Ind_HH_Sample_Size Numeric. Calculated individual-level sample
    #'   size expressed in households.
    #' @param mort_indicator Character. Indicator label for mortality-level
    #'   sample size calculation.
    #' @param mort_expected_death_rate Numeric. Expected crude death rate used
    #'   for mortality sample size calculation.
    #' @param mort_precision Numeric. Desired precision for mortality estimate.
    #' @param mort_nonresponse Numeric. Expected non-response rate for
    #'   mortality calculation (\%).
    #' @param mort_design_effect Numeric. Design effect for mortality
    #'   calculation.
    #' @param mort_recall_days Integer. Recall period in days for mortality
    #'   estimate.
    #' @param mort_avg_hh_size Numeric. Average household size (mortality
    #'   level).
    #' @param mort_fpc Logical. Apply finite population correction at mortality
    #'   level?  Defaults to \code{FALSE}.
    #' @param Mort_Ind_Sample_Size Numeric. Calculated mortality-level
    #'   individual sample size.
    #' @param Mort_PT_Sample_Size Numeric. Calculated mortality-level
    #'   person-time sample size.
    #' @param Mort_HH_Sample_Size Numeric. Calculated mortality-level
    #'   household sample size.
    #' @param teams Numeric. Number of field teams.
    #' @param avg_interview_time Numeric. Average interview time in minutes.
    #' @param clusters_per_day Numeric. Number of clusters visited per day per
    #'   team.
    #' @param enumerators_per_team Numeric. Number of enumerators per team.
    #' @param avg_rest_time Numeric. Average rest/break time in minutes per
    #'   day.
    #' @param avg_travel_time Numeric. Average travel time to cluster in
    #'   minutes.
    #' @param start_time Character. Planned work start time (e.g.
    #'   \code{"08:00"}).
    #' @param end_time Character. Planned work end time (e.g.
    #'   \code{"17:00"}).
    #' @param design_effect Deprecated alias for \code{pop_design_effect}.
    #' @param precision Deprecated alias for \code{pop_precision}.
    #' @param confidence_level Deprecated / ignored in the new schema.
    #' @param n_psu Integer.  Number of PSUs to select.  Used by
    #'   \code{draw_sample()} when \code{Sampling_Method} is \code{"srs"}.
    #' @param n_clusters Integer.  Number of clusters.  Used by
    #'   \code{draw_sample()} when \code{Sampling_Method} is \code{"pps_cluster"}.
    #' @param cluster_size Integer.  Households per cluster.  Used by
    #'   \code{draw_sample()} when \code{Sampling_Method} is \code{"pps_cluster"}
    #'   or \code{"rlc"} (defaults to \code{3} for \code{"rlc"} if not set).
    #' @param n_sites Integer.  Number of sites to select.  Used by
    #'   \code{draw_sample()} when \code{Sampling_Method} is \code{"systematic"}.
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size        = NA_real_,
      total_households       = NA_real_,
      sampling_method        = "srs",
      allocation_method      = NULL,   # legacy alias for sampling_method
      n_psu                  = NA_real_,
      n_clusters             = NA_real_,
      cluster_size           = NA_real_,
      n_sites                = NA_real_,
      pop_indicator          = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision          = NA_real_,
      pop_nonresponse        = NA_real_,
      pop_design_effect      = NA_real_,
      pop_fpc                = FALSE,
      General_HH_Sample_Size = NA_real_,
      ind_indicator          = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision          = NA_real_,
      ind_nonresponse        = NA_real_,
      ind_design_effect      = NA_real_,
      ind_avg_hh_size        = NA_real_,
      ind_subpop_prop        = NA_real_,
      ind_fpc                = FALSE,
      Ind_Sample_Size        = NA_real_,
      Ind_HH_Sample_Size     = NA_real_,
      mort_indicator         = NA_character_,
      mort_expected_death_rate = NA_real_,
      mort_precision         = NA_real_,
      mort_nonresponse       = NA_real_,
      mort_design_effect     = NA_real_,
      mort_recall_days       = NA_real_,
      mort_avg_hh_size       = NA_real_,
      mort_fpc               = FALSE,
      Mort_Ind_Sample_Size   = NA_real_,
      Mort_PT_Sample_Size    = NA_real_,
      Mort_HH_Sample_Size    = NA_real_,
      teams                  = NA_real_,
      avg_interview_time     = NA_real_,
      clusters_per_day       = NA_real_,
      enumerators_per_team   = NA_real_,
      avg_rest_time          = NA_real_,
      avg_travel_time        = NA_real_,
      start_time             = NA_character_,
      end_time               = NA_character_,
      # Legacy / deprecated params kept for backward compatibility
      design_effect          = NULL,
      precision              = NULL,
      confidence_level       = NULL
    ) {

      # Resolve legacy aliases
      if (!is.null(allocation_method) && sampling_method == "srs") {
        sampling_method <- allocation_method
      }
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      new_row <- data.frame(
        stratum_id               = stratum_id,
        Population_Name          = stratum_name,
        Total_Households         = as.numeric(total_households),
        Total_Population         = as.numeric(population_size),
        Sampling_Method          = sampling_method,
        n_psu                    = as.numeric(n_psu),
        n_clusters               = as.numeric(n_clusters),
        cluster_size             = as.numeric(cluster_size),
        n_sites                  = as.numeric(n_sites),
        pop_indicator            = pop_indicator,
        pop_expected_prevalence  = as.numeric(pop_expected_prevalence),
        pop_precision            = as.numeric(pop_precision),
        pop_nonresponse          = as.numeric(pop_nonresponse),
        pop_design_effect        = as.numeric(pop_design_effect),
        pop_fpc                  = as.logical(pop_fpc),
        ind_indicator            = as.character(ind_indicator),
        ind_expected_prevalence  = as.numeric(ind_expected_prevalence),
        ind_precision            = as.numeric(ind_precision),
        ind_nonresponse          = as.numeric(ind_nonresponse),
        ind_design_effect        = as.numeric(ind_design_effect),
        ind_avg_hh_size          = as.numeric(ind_avg_hh_size),
        ind_subpop_prop          = as.numeric(ind_subpop_prop),
        ind_fpc                  = as.logical(ind_fpc),
        mort_indicator           = as.character(mort_indicator),
        mort_expected_death_rate = as.numeric(mort_expected_death_rate),
        mort_precision           = as.numeric(mort_precision),
        mort_nonresponse         = as.numeric(mort_nonresponse),
        mort_design_effect       = as.numeric(mort_design_effect),
        mort_recall_days         = as.numeric(mort_recall_days),
        mort_avg_hh_size         = as.numeric(mort_avg_hh_size),
        mort_fpc                 = as.logical(mort_fpc),
        teams                    = as.numeric(teams),
        avg_interview_time       = as.numeric(avg_interview_time),
        clusters_per_day         = as.numeric(clusters_per_day),
        enumerators_per_team     = as.numeric(enumerators_per_team),
        avg_rest_time            = as.numeric(avg_rest_time),
        avg_travel_time          = as.numeric(avg_travel_time),
        start_time               = as.character(start_time),
        end_time                 = as.character(end_time),
        # Result columns — rightmost, in prescribed order
        General_HH_Sample_Size   = as.numeric(General_HH_Sample_Size),
        Ind_Sample_Size          = as.numeric(Ind_Sample_Size),
        Ind_HH_Sample_Size       = as.numeric(Ind_HH_Sample_Size),
        Mort_Ind_Sample_Size     = as.numeric(Mort_Ind_Sample_Size),
        Mort_PT_Sample_Size      = as.numeric(Mort_PT_Sample_Size),
        Mort_HH_Sample_Size      = as.numeric(Mort_HH_Sample_Size),
        Final_HH_Sample_Size     = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phr_warning(
            message = phr_txt("Stratum ID '{stratum_id}' already exists and will be overwritten."),
            origin  = "SurveyProtocol$add_stratum",
            hint    = phr_txt("The existing row for '{stratum_id}' has been replaced with the new values.")
          )
          self$sample_table <- self$sample_table[
            self$sample_table$stratum_id != stratum_id, , drop = FALSE
          ]
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }

      self$metadata$modified_date <- Sys.time()
      private$add_target_stratum()
      private$check_issues()
      phr_message(phr_txt("Stratum '{stratum_id}' added."), origin = "SurveyProtocol$add_stratum")
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
      phr_try({

        # 1. Confirm it is a data frame and not empty
        phr_validate_dataframe(frame, origin = "SurveyProtocol$set_sampling_frame", soft = FALSE)
        phr_assert(
          nrow(frame) > 0,
          message = phr_txt("Sampling frame is empty."),
          origin  = "SurveyProtocol$set_sampling_frame",
          hint    = phr_txt("Provide a data frame with at least one PSU row.")
        )

        # 2. Run validate_sampling_frame — stops on hard issues
        val_result <- validate_sampling_frame(frame)
        if (!val_result$valid) {
          hard_issues <- val_result$issues[setdiff(names(val_result$issues), "missing_inclusion")]
          if (length(hard_issues) > 0) {
            phr_error(
              message = phr_txt("Sampling frame validation failed: {paste(names(hard_issues), unlist(hard_issues), sep=': ', collapse='; ')}"),
              origin  = "SurveyProtocol$set_sampling_frame",
              hint    = phr_txt("Ensure the frame has 'stratum', 'psu', and 'population_size' columns, and that any 'inclusion' column contains only TRUE/FALSE values.")
            )
          }
        }

        # 3. Add inclusion column (all TRUE) if absent
        if (!"inclusion" %in% names(frame)) {
          frame$inclusion <- TRUE
          phr_message(
            phr_txt("'inclusion' column not found — defaulting all PSUs to TRUE."),
            origin = "SurveyProtocol$set_sampling_frame"
          )
        }

        self$sampling_frame <- frame
        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Sampling frame set with {nrow(frame)} PSUs."),
          origin = "SurveyProtocol$set_sampling_frame"
        )

      }, on_error = "abort", origin = "SurveyProtocol$set_sampling_frame")
      invisible(self)
    },

    #' @description Draw sample from the sampling frame
    #'
    #' Applies PSU-level sampling methods to the eligible PSUs in the sampling
    #' frame.  Results are stored in \code{drawn_sample_full} (the full frame
    #' annotated with \code{sampled_psu} and \code{allocated_sample} columns)
    #' and \code{drawn_sample} (only the selected rows).
    #'
    #' Sampling method and all method-specific parameters (\code{n_psu},
    #' \code{n_clusters}, \code{cluster_size}, \code{n_sites}) are read
    #' from the strata table row(s).  When the frame contains a \code{stratum}
    #' column and the strata table has multiple rows, sampling is applied
    #' independently per stratum using that stratum's own method and parameters.
    #'
    #' Supported \code{Sampling_Method} values in the strata table:
    #' \itemize{
    #'   \item \code{"srs"} — simple random sampling; requires \code{n_psu}.
    #'   \item \code{"proportional"} — proportional allocation; requires
    #'     \code{population_size} in the frame.
    #'   \item \code{"pps_cluster"} — PPS cluster sampling; requires
    #'     \code{n_clusters} and \code{cluster_size}.
    #'   \item \code{"rlc"} — random location cluster; \code{cluster_size}
    #'     defaults to \code{3} if not set.
    #'   \item \code{"systematic"} — systematic sampling; requires
    #'     \code{n_sites}.
    #'   \item \code{"purposive"} — purposive / convenience sampling; returns
    #'     \code{NA} for \code{sampled_psu} and \code{allocated_sample} —
    #'     the user manually designates selected PSUs.
    #' }
    #'
    #' @param frame Data frame.  The sampling frame to draw from.  If
    #'   \code{NULL} (default), uses \code{self$sampling_frame}.
    #' @param strata_table Data frame.  The strata table supplying
    #'   \code{Sampling_Method}, \code{Final_HH_Sample_Size}, and sampling
    #'   parameters (\code{n_psu}, \code{n_clusters}, \code{cluster_size},
    #'   \code{n_sites}).  If \code{NULL} (default), uses
    #'   \code{self$sample_table}.
    #' @param seed Integer. Random seed for reproducibility (default \code{42}).
    #' @return Invisibly returns \code{self} for method chaining.
    draw_sample = function(frame = NULL, strata_table = NULL, seed = 42) {
      phr_try({

        # -- Resolve frame and strata_table --
        if (is.null(frame)) {
          phr_assert(
            !is.null(self$sampling_frame),
            message = phr_txt("Must set sampling frame before drawing sample."),
            origin  = "SurveyProtocol$draw_sample",
            hint    = phr_txt("Call set_sampling_frame() first, or pass a frame argument.")
          )
          frame <- self$sampling_frame
        }
        phr_validate_dataframe(frame, origin = "SurveyProtocol$draw_sample", soft = FALSE)

        if (is.null(strata_table)) {
          phr_assert(
            !is.null(self$sample_table),
            message = phr_txt("No strata table available. Call add_stratum() first or pass a strata_table argument."),
            origin  = "SurveyProtocol$draw_sample"
          )
          strata_table <- self$sample_table
        }
        phr_validate_dataframe(strata_table, origin = "SurveyProtocol$draw_sample", soft = FALSE)
        phr_assert(
          "Sampling_Method" %in% names(strata_table),
          message = phr_txt("strata_table must contain a 'Sampling_Method' column."),
          origin  = "SurveyProtocol$draw_sample"
        )

        # Ensure method-specific parameter columns exist (add as NA if absent)
        for (col in c("n_psu", "n_clusters", "cluster_size", "n_sites", "Final_HH_Sample_Size")) {
          if (!col %in% names(strata_table)) strata_table[[col]] <- NA_real_
        }

        # Eligible PSUs
        if ("inclusion" %in% names(frame)) {
          eligible_rows <- which(!is.na(frame$inclusion) & frame$inclusion)
        } else {
          eligible_rows <- seq_len(nrow(frame))
        }
        eligible_frame <- frame[eligible_rows, , drop = FALSE]

        # Initialise output columns on full frame
        frame$sampled_psu      <- NA_integer_
        frame$allocated_sample <- NA_real_

        is_stratified <- ("stratum" %in% names(eligible_frame)) &&
          ("stratum_id" %in% names(strata_table)) &&
          (nrow(strata_table) > 1L || !is.null(strata_table$stratum_id))

        if (is_stratified) {
          strata_ids     <- unique(strata_table$stratum_id)
          cluster_offset <- 0L

          for (st_id in strata_ids) {
            st_row <- strata_table[strata_table$stratum_id == st_id, ]
            if (nrow(st_row) == 0L) next

            st_eligible_rows <- which(eligible_frame$stratum == st_id)
            if (length(st_eligible_rows) == 0L) {
              phr_warning(
                message = phr_txt("Stratum '{st_id}' not found in sampling frame — skipping."),
                origin  = "SurveyProtocol$draw_sample"
              )
              next
            }
            st_frame <- eligible_frame[st_eligible_rows, , drop = FALSE]

            st_params <- private$params_from_strata_row(
              st_row, nrow(st_frame), nrow(eligible_frame)
            )

            st_result <- private$apply_sampling_method(
              frame        = st_frame,
              method       = st_params$method,
              sample_size  = st_params$sample_size,
              n_psu        = st_params$n_psu,
              n_clusters   = st_params$n_clusters,
              n_sites      = st_params$n_sites,
              cluster_size = st_params$cluster_size,
              seed         = seed
            )

            sel_mask <- !is.na(st_result$sampled_psu)
            if (any(sel_mask)) {
              st_result$sampled_psu[sel_mask] <-
                st_result$sampled_psu[sel_mask] + cluster_offset
              cluster_offset <- cluster_offset +
                max(st_result$sampled_psu[sel_mask], na.rm = TRUE)
            }

            full_frame_rows <- eligible_rows[st_eligible_rows]
            frame$sampled_psu[full_frame_rows]      <- st_result$sampled_psu
            frame$allocated_sample[full_frame_rows] <- st_result$allocated_sample
          }

        } else {
          # Non-stratified: use first strata table row for the whole frame
          st_row    <- strata_table[1L, ]
          st_params <- private$params_from_strata_row(
            st_row, nrow(eligible_frame), nrow(eligible_frame)
          )

          result <- private$apply_sampling_method(
            frame        = eligible_frame,
            method       = st_params$method,
            sample_size  = st_params$sample_size,
            n_psu        = st_params$n_psu,
            n_clusters   = st_params$n_clusters,
            n_sites      = st_params$n_sites,
            cluster_size = st_params$cluster_size,
            seed         = seed
          )
          frame$sampled_psu[eligible_rows]      <- result$sampled_psu
          frame$allocated_sample[eligible_rows] <- result$allocated_sample
        }

        self$drawn_sample_full <- frame
        # For purposive, drawn_sample is empty (user fills manually)
        self$drawn_sample <- frame[!is.na(frame$sampled_psu), , drop = FALSE]

        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Sample drawn: {nrow(self$drawn_sample)} PSU(s) selected."),
          origin = "SurveyProtocol$draw_sample"
        )

      }, on_error = "abort", origin = "SurveyProtocol$draw_sample")
      invisible(self)
    },

    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.  Returns a list with a \code{valid} flag and a
    #' \code{message}.
    #'
    #' @return Named list with elements \code{valid} (logical) and
    #'   \code{message} (character).
    validate_strata_table = function() {
      if (is.null(self$sample_table)) {
        return(list(valid = FALSE, message = "sample_table is NULL — no strata have been added yet."))
      }

      required_cols <- .strata_table_required_cols

      missing_cols <- setdiff(required_cols, names(self$sample_table))
      if (length(missing_cols) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("sample_table is missing required columns:",
                          paste(missing_cols, collapse = ", "))
        ))
      }

      dupes <- self$sample_table$stratum_id[duplicated(self$sample_table$stratum_id)]
      if (length(dupes) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("Duplicate stratum_id values:", paste(dupes, collapse = ", "))
        ))
      }

      list(valid = TRUE, message = "sample_table structure is valid.")
    },

    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      return(self$sample_table)
    },

    #' @description Get protocol summary including sampling information
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      base_summary <- super$get_protocol_summary()
      base_summary$num_strata       <- if (is.null(self$sample_table)) 0L else nrow(self$sample_table)
      base_summary$total_sample_size <- if (is.null(self$sample_table)) 0 else sum(self$sample_table$General_HH_Sample_Size, na.rm = TRUE)
      base_summary
    },

    #' @description Calculate sample sizes for all strata in the sample table
    #'
    #' Reads each stratum row from \code{sample_table}, determines which
    #' calculation parameters are available, calls the appropriate
    #' \code{calculate_sample_size_*} function for each applicable calculation
    #' type (general, individual, mortality), and stores the results back into
    #' the table.  Also sets \code{Final_HH_Sample_Size} to the maximum
    #' household sample size across the three calculation types.
    #'
    #' Required parameters per calculation type:
    #' \itemize{
    #'   \item \strong{General}: \code{pop_expected_prevalence}, \code{pop_precision}
    #'   \item \strong{Individual}: \code{ind_expected_prevalence}, \code{ind_precision},
    #'     \code{ind_avg_hh_size} (> 0)
    #'   \item \strong{Mortality}: \code{mort_expected_death_rate}, \code{mort_precision},
    #'     \code{mort_avg_hh_size} (> 0)
    #' }
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    calculate_sample_sizes = function() {
      phr_try({
        phr_assert(
          !is.null(self$sample_table) && nrow(self$sample_table) > 0,
          message = phr_txt("sample_table is empty. Call add_stratum() first."),
          origin  = "SurveyProtocol$calculate_sample_sizes"
        )
        self$sample_table <- calculate_sample_size_strata_table(self$sample_table)
        self$metadata$modified_date <- Sys.time()
        private$add_target_stratum()
        private$check_issues()
        phr_message(
          phr_txt("Sample sizes calculated for {nrow(self$sample_table)} stratum/strata."),
          origin = "SurveyProtocol$calculate_sample_sizes"
        )
      }, on_error = "abort", origin = "SurveyProtocol$calculate_sample_sizes")
      invisible(self)
    },

    #' @description Export protocol to a list including sampling data
    #' @return List containing all protocol data
    export_protocol = function() {
      base_export <- super$export_protocol()
      base_export$sample_table      <- self$sample_table
      base_export$sampling_frame    <- self$sampling_frame
      base_export$drawn_sample      <- self$drawn_sample
      base_export$drawn_sample_full <- self$drawn_sample_full
      base_export$summary           <- self$get_protocol_summary()
      base_export
    },

    #' @description Generate a Word document report including sampling information
    #'
    #' Extends \code{Protocol$generate_reach_tor()} by inserting a
    #' \strong{Sampling Design} sub-section within the Protocol Data section,
    #' summarising strata definitions, household sample sizes, and (when
    #' \code{draw_sample()} has been called) a table of selected PSUs.
    #'
    #' @param output_file Character. Output \code{.docx} file path.
    #'   Defaults to \code{"protocol_report.docx"}.
    #' @param reference_docx Character or \code{NULL}. Path to a custom
    #'   \code{.docx} template.  Uses the bundled REACH TOR template by default.
    #' @param open Logical. Open the file after writing.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    generate_reach_tor = function(output_file = "protocol_report.docx",
                                  reference_docx = NULL,
                                  open = FALSE) {
      phr_try({
        doc <- private$create_base_doc(reference_docx)
        doc <- private$add_metadata_section(doc)
        doc <- private$add_objectives_section(doc)
        doc <- private$add_tools_section(doc)
        doc <- private$add_sampling_section(doc)

        print(doc, target = output_file)
        phr_message(
          phr_txt("Protocol report saved to: {output_file}"),
          origin = "SurveyProtocol$generate_reach_tor"
        )
        if (isTRUE(open)) utils::browseURL(output_file)
      }, on_error = "abort", origin = "SurveyProtocol$generate_reach_tor")
      invisible(self)
    }
  ),

  private = list(
    # Rebuild metadata$target_strata from the current sample_table.
    # Called automatically after any method that creates or modifies sample_table
    # so that metadata strata are always aligned with the sample_table.
    add_target_stratum = function() {
      if (!is.null(self$sample_table) && nrow(self$sample_table) > 0) {
        strata_ids   <- as.character(self$sample_table$stratum_id)
        strata_names <- as.character(self$sample_table$Population_Name)
        self$metadata$target_strata <- setNames(as.list(strata_names), strata_ids)
      } else {
        self$metadata$target_strata <- list()
      }
      invisible(NULL)
    },

    # Add the sampling design section after the current cursor position.
    add_sampling_section = function(doc) {
      doc <- officer::body_add_par(doc, "Sampling Design", style = "heading 2", pos = "after")

      if (is.null(self$sample_table) || nrow(self$sample_table) == 0) {
        return(officer::body_add_par(
          doc, "No strata have been defined.", style = "Normal", pos = "after"
        ))
      }

      # --- Strata and sample sizes table ---
      doc <- officer::body_add_par(doc, "Strata and Sample Sizes", style = "heading 3", pos = "after")

      display_cols <- c("stratum_id", "Population_Name", "Total_Population",
                        "Sampling_Method", "General_HH_Sample_Size",
                        "Ind_HH_Sample_Size", "Mort_HH_Sample_Size", "Final_HH_Sample_Size")
      col_labels   <- c("Stratum ID", "Population Name", "Total Population",
                        "Sampling Method", "General HH Sample Size",
                        "Ind. HH Sample Size", "Mort. HH Sample Size", "Final HH Sample Size")
      avail_idx    <- which(display_cols %in% names(self$sample_table))

      if (length(avail_idx) > 0) {
        strata_df <- self$sample_table[, display_cols[avail_idx], drop = FALSE]
        names(strata_df) <- col_labels[avail_idx]
        ft <- flextable::flextable(strata_df)
        ft <- flextable::theme_zebra(ft)
        ft <- flextable::autofit(ft)
        doc <- flextable::body_add_flextable(doc, ft, pos = "after")
      }

      # --- Selected PSUs (only when draw_sample() has been called) ---
      if (!is.null(self$drawn_sample) && nrow(self$drawn_sample) > 0) {
        doc <- officer::body_add_par(doc, "Selected PSUs", style = "heading 3", pos = "after")
        doc <- officer::body_add_par(
          doc,
          paste0("Total PSUs selected: ", nrow(self$drawn_sample)),
          style = "Normal",
          pos   = "after"
        )

        psu_cols <- intersect(
          c("psu", "stratum", "sampled_psu", "allocated_sample"),
          names(self$drawn_sample)
        )
        if (length(psu_cols) > 0) {
          psu_df <- self$drawn_sample[, psu_cols, drop = FALSE]
          ft <- flextable::flextable(psu_df)
          ft <- flextable::theme_zebra(ft)
          ft <- flextable::autofit(ft)
          doc <- flextable::body_add_flextable(doc, ft, pos = "after")
        }
      }

      doc
    },

    # Check for issues and discrepancies in the survey protocol,
    # including strata consistency between frame and sample table.
    check_issues = function() {
      self$issues <- list()

      # Check if objectives have matching indicators in tools
      all_objectives <- flatten_objectives(self$objectives)
      if (length(all_objectives) > 0 && length(self$tools) > 0) {
        obj_sectors <- unique(sapply(all_objectives, function(x) x$sector))

        tool_sectors <- character(0)
        tryCatch({
          tool_sectors <- unique(sapply(self$tools, function(x) {
            if (is.list(x) && "sector" %in% names(x)) {
              return(x$sector)
            } else if (methods::is(x, "R6") && "sector" %in% names(x)) {
              return(x$sector)
            }
            return(NA_character_)
          }))
          tool_sectors <- tool_sectors[!is.na(tool_sectors)]
        }, error = function(e) {
          # Ignore extraction errors
        })

        missing_sectors <- setdiff(obj_sectors, tool_sectors)
        if (length(missing_sectors) > 0) {
          self$issues$tool_coverage <- paste(
            "Objectives require sectors not covered by tools:",
            paste(missing_sectors, collapse = ", ")
          )
        }
      }

      # Check strata consistency between frame and sample table
      if (!is.null(self$sample_table) && !is.null(self$sampling_frame)) {
        table_strata <- self$sample_table$stratum_id
        frame_strata <- unique(self$sampling_frame$stratum)

        if (!setequal(table_strata, frame_strata)) {
          missing_in_frame <- setdiff(table_strata, frame_strata)
          missing_in_table <- setdiff(frame_strata, table_strata)

          if (length(missing_in_frame) > 0) {
            self$issues$strata_missing_in_frame <- paste(
              "Strata in sample table but not in frame:",
              paste(missing_in_frame, collapse = ", ")
            )
          }
          if (length(missing_in_table) > 0) {
            self$issues$strata_missing_in_table <- paste(
              "Strata in frame but not in sample table:",
              paste(missing_in_table, collapse = ", ")
            )
          }
        }
      }

      invisible(self)
    },

    # Apply a PSU-level sampling method — dispatches to draw_sample_psu_* utilities
    apply_sampling_method = function(frame, method, sample_size, n_psu, n_clusters,
                                     n_sites, cluster_size, seed) {
      origin <- "SurveyProtocol$apply_sampling_method"
      valid_methods <- c("srs", "proportional", "pps_cluster", "rlc", "systematic", "purposive")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt("Unknown sampling method '{method}' — must be one of: {paste(valid_methods, collapse=', ')}."),
        origin  = origin
      )

      if (method == "srs") {
        phr_assert(!is.null(n_psu) && !is.na(n_psu),
                   message = phr_txt("n_psu is required for the 'srs' method — set the 'n_psu' column in the strata table."),
                   origin = origin)
        draw_sample_psu_srs(frame, n_psu, sample_size, seed)
      } else if (method == "proportional") {
        draw_sample_psu_proportional(frame, sample_size, seed)
      } else if (method == "pps_cluster") {
        phr_assert(!is.null(n_clusters) && !is.na(n_clusters),
                   message = phr_txt("n_clusters is required for the 'pps_cluster' method — set the 'n_clusters' column in the strata table."),
                   origin = origin)
        phr_assert(!is.null(cluster_size) && !is.na(cluster_size),
                   message = phr_txt("cluster_size is required for the 'pps_cluster' method — set the 'cluster_size' column in the strata table."),
                   origin = origin)
        draw_sample_psu_pps_cluster(frame, n_clusters, cluster_size, seed)
      } else if (method == "rlc") {
        cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) cluster_size else 3L
        draw_sample_psu_rlc(frame, sample_size, cs, seed)
      } else if (method == "systematic") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'systematic' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        draw_sample_psu_systematic(frame, n_sites, sample_size, seed)
      } else {  # purposive
        draw_sample_psu_purposive(frame, seed)
      }
    },

    # Extract sampling parameters from a single strata table row.
    # Returns a named list: method, sample_size, n_psu, n_clusters, cluster_size, n_sites.
    # sample_size falls back to General_HH_Sample_Size, then a proportional estimate when
    # Final_HH_Sample_Size is NA.
    params_from_strata_row = function(st_row, stratum_n_eligible, total_n_eligible) {
      method <- as.character(st_row$Sampling_Method[1])

      ss <- if ("Final_HH_Sample_Size" %in% names(st_row) &&
                !is.na(st_row$Final_HH_Sample_Size[1])) {
        as.integer(st_row$Final_HH_Sample_Size[1])
      } else {
        # Fallback: use the calculated general population-level sample size if available
        if ("General_HH_Sample_Size" %in% names(st_row) && !is.na(st_row$General_HH_Sample_Size[1])) {
          as.integer(st_row$General_HH_Sample_Size[1])
        } else {
          as.integer(round(100 * stratum_n_eligible / max(total_n_eligible, 1L)))
        }
      }

      # Convert NA to NULL so callers can use is.null() checks
      .na_as_null <- function(x) if (length(x) == 0L || is.na(x)) NULL else x

      list(
        method       = method,
        sample_size  = ss,
        n_psu        = .na_as_null(if ("n_psu"        %in% names(st_row)) st_row$n_psu[1]        else NA),
        n_clusters   = .na_as_null(if ("n_clusters"   %in% names(st_row)) st_row$n_clusters[1]   else NA),
        cluster_size = .na_as_null(if ("cluster_size" %in% names(st_row)) st_row$cluster_size[1] else NA),
        n_sites      = .na_as_null(if ("n_sites"      %in% names(st_row)) st_row$n_sites[1]      else NA)
      )
    }
  )
)
