#' Protocol Utility Functions
#'
#' @description
#' Utility functions for working with Protocol objects and protocol workflows.

# Minimum columns every master strata table must contain.
.strata_table_required_cols <- c(
  "stratum_id", "Population_Name", "Total_Population", "Sampling_Method",
  "pop_indicator", "General_HH_Sample_Size",
  "ind_indicator", "Ind_HH_Sample_Size",
  "mort_indicator", "Mort_HH_Sample_Size",
  "Ind_Sample_Size", "Mort_Ind_Sample_Size", "Mort_PT_Sample_Size",
  "Final_HH_Sample_Size"
)

#' Create a new survey protocol instance
#'
#' Creates a \code{\link{SurveyProtocol}} object, which extends
#' \code{\link{Protocol}} with strata definition, sample size calculations,
#' sampling frame management, and sample drawing capabilities.
#'
#' @param assessment_title Character. Title of the assessment
#' @param country_name Character. Country where assessment takes place
#' @param month_year Character. Month and year of data collection
#' @param framework_type Character. Type of framework to initialise.  One of
#'   \code{"none"} (default) or \code{"ana"}.
#' @return A new SurveyProtocol object
#' @export
create_survey_protocol <- function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                                    framework_type = "none") {
  SurveyProtocol$new(assessment_title = assessment_title, country_name = country_name,
                     month_year = month_year, framework_type = framework_type)
}

#' Validate protocol completeness
#'
#' @param protocol Protocol object to validate
#' @return List with validation results and issues
#' @export
validate_protocol <- function(protocol) {

  origin <- "validate_protocol"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin,
      hint    = phr_txt("Use Protocol$new() or SurveyProtocol$new() to create a Protocol object.")
    )

    issues <- protocol$get_issues()

    list(
      has_issues = length(issues) > 0,
      issues     = issues,
      summary    = protocol$get_protocol_summary()
    )
  }, on_error = "abort", origin = origin)
}

#' Validate the master strata table structure
#'
#' Checks that a data frame conforms to the expected master strata table
#' structure with all required columns.  This is a standalone helper that
#' mirrors the \code{Protocol$validate_strata_table()} method.
#'
#' @param sample_table A data frame to validate (typically
#'   \code{protocol$sample_table}).
#' @return Named list with elements \code{valid} (logical) and
#'   \code{message} (character).
#' @export
validate_strata_table <- function(sample_table) {

  origin <- "validate_strata_table"

  if (is.null(sample_table) || !is.data.frame(sample_table)) {
    return(list(valid = FALSE,
                message = phr_txt("sample_table must be a non-NULL data frame.")))
  }

  if (nrow(sample_table) == 0) {
    return(list(valid = FALSE,
                message = phr_txt("sample_table is empty (zero rows).")))
  }

  required_cols <- .strata_table_required_cols

  missing_cols <- setdiff(required_cols, names(sample_table))
  if (length(missing_cols) > 0) {
    return(list(
      valid   = FALSE,
      message = phr_txt("sample_table is missing required columns: {paste(missing_cols, collapse=', ')}")
    ))
  }

  dupes <- sample_table$stratum_id[duplicated(sample_table$stratum_id)]
  if (length(dupes) > 0) {
    return(list(
      valid   = FALSE,
      message = phr_txt("Duplicate stratum_id values: {paste(dupes, collapse=', ')}")
    ))
  }

  list(valid = TRUE, message = phr_txt("sample_table structure is valid."))
}

#' Recalculate sample sizes for all rows of a strata table
#'
#' Goes row by row through a master strata table and recalculates the
#' \code{General_HH_Sample_Size}, \code{Ind_Sample_Size},
#' \code{Ind_HH_Sample_Size}, \code{Mort_Ind_Sample_Size},
#' \code{Mort_PT_Sample_Size}, and \code{Mort_HH_Sample_Size} columns from
#' the stored parameters wherever sufficient information is available.  After
#' recalculation, \code{Final_HH_Sample_Size} is set to the maximum household
#' sample size across the three HH-level calculation types for each row.
#'
#' @param sample_table A data frame conforming to the master strata table
#'   schema (validated with \code{validate_strata_table}).
#' @return The updated \code{sample_table} with recalculated sample size
#'   columns.
#' @export
calculate_sample_size_strata_table <- function(sample_table) {

  origin <- "calculate_sample_size_strata_table"

  # Default design effect used as fallback when no value is stored.
  .default_design_effect <- 1.5

  phr_try({

    val_result <- validate_strata_table(sample_table)
    phr_assert(
      isTRUE(val_result$valid),
      message = val_result$message,
      origin  = origin,
      hint    = phr_txt("Ensure the strata table was built via add_stratum() or conforms to the required schema.")
    )

    for (i in seq_len(nrow(sample_table))) {
      row <- sample_table[i, ]

      # ---- General (population-level) sample size -------------------------
      if (!is.na(row$pop_expected_prevalence) && !is.na(row$pop_precision)) {
        design_effect <- if (!is.na(row$pop_design_effect) && row$pop_design_effect > 1) row$pop_design_effect else .default_design_effect
        design_type   <- if (!is.na(row$pop_design_effect) && row$pop_design_effect > 1) "cluster" else "simple_random"
        nonresponse   <- if (!is.na(row$pop_nonresponse)) row$pop_nonresponse else 5
        fpc           <- if (!is.na(row$pop_fpc)) as.logical(row$pop_fpc) else FALSE
        total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL

        pop_ss <- phr_try(
          calculate_sample_size_general(
            expected_proportion = row$pop_expected_prevalence,
            desired_precision   = row$pop_precision,
            non_response_rate   = nonresponse,
            design              = design_type,
            design_effect       = design_effect,
            fpc                 = fpc,
            total_population    = total_pop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("General sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(pop_ss)) {
          sample_table$General_HH_Sample_Size[i] <- pop_ss
        }
      }

      # ---- Individual-level sample size -----------------------------------
      if (!is.na(row$ind_expected_prevalence) && !is.na(row$ind_precision) &&
          !is.na(row$ind_avg_hh_size) && row$ind_avg_hh_size > 0) {
        design_effect <- if (!is.na(row$ind_design_effect) && row$ind_design_effect > 1) row$ind_design_effect else .default_design_effect
        design_type   <- if (!is.na(row$ind_design_effect) && row$ind_design_effect > 1) "cluster" else "simple_random"
        nonresponse   <- if (!is.na(row$ind_nonresponse)) row$ind_nonresponse else 5
        fpc           <- if (!is.na(row$ind_fpc)) as.logical(row$ind_fpc) else FALSE
        total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL
        subpop        <- if (!is.na(row$ind_subpop_prop)) row$ind_subpop_prop else 100

        ind_res <- phr_try(
          calculate_sample_size_individual(
            expected_proportion    = row$ind_expected_prevalence,
            desired_precision      = row$ind_precision,
            non_response_rate      = nonresponse,
            design                 = design_type,
            design_effect          = design_effect,
            fpc                    = fpc,
            total_population       = total_pop,
            average_household_size = row$ind_avg_hh_size,
            sub_population_percent = subpop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("Individual sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(ind_res) && !is.null(ind_res)) {
          sample_table$Ind_Sample_Size[i]    <- ind_res$sample_size_individuals
          sample_table$Ind_HH_Sample_Size[i] <- ind_res$sample_size_households
        }
      }

      # ---- Mortality-rate sample size -------------------------------------
      if (!is.na(row$mort_expected_death_rate) && !is.na(row$mort_precision) &&
          !is.na(row$mort_avg_hh_size) && row$mort_avg_hh_size > 0) {
        design_effect <- if (!is.na(row$mort_design_effect) && row$mort_design_effect > 1) row$mort_design_effect else .default_design_effect
        design_type   <- if (!is.na(row$mort_design_effect) && row$mort_design_effect > 1) "cluster" else "simple_random"
        nonresponse   <- if (!is.na(row$mort_nonresponse)) row$mort_nonresponse else 5
        fpc           <- if (!is.na(row$mort_fpc)) as.logical(row$mort_fpc) else FALSE
        total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL
        recall_days   <- if (!is.na(row$mort_recall_days) && row$mort_recall_days > 0) row$mort_recall_days else 90

        mort_res <- phr_try(
          calculate_sample_size_mortality(
            expected_death_rate    = row$mort_expected_death_rate,
            desired_precision      = row$mort_precision,
            non_response_rate      = nonresponse,
            design                 = design_type,
            design_effect          = design_effect,
            recall_days            = recall_days,
            average_household_size = row$mort_avg_hh_size,
            fpc                    = fpc,
            total_population       = total_pop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("Mortality sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(mort_res) && !is.null(mort_res)) {
          sample_table$Mort_Ind_Sample_Size[i] <- mort_res$sample_size_individuals
          sample_table$Mort_PT_Sample_Size[i]  <- mort_res$sample_size_person_time
          sample_table$Mort_HH_Sample_Size[i]  <- mort_res$sample_size_households
        }
      }

      # ---- Final household sample size: max across all three HH types -----
      hh_sizes <- c(
        pop_hh  = if (!is.na(sample_table$General_HH_Sample_Size[i])) sample_table$General_HH_Sample_Size[i] else NA_real_,
        ind_hh  = if (!is.na(sample_table$Ind_HH_Sample_Size[i]))     sample_table$Ind_HH_Sample_Size[i]     else NA_real_,
        mort_hh = if (!is.na(sample_table$Mort_HH_Sample_Size[i]))    sample_table$Mort_HH_Sample_Size[i]    else NA_real_
      )

      valid_hh <- hh_sizes[!is.na(hh_sizes)]
      if (length(valid_hh) > 0 && "Final_HH_Sample_Size" %in% names(sample_table)) {
        sample_table$Final_HH_Sample_Size[i] <- max(valid_hh)
      }
    }

    sample_table

  }, on_error = "abort", origin = origin)
}

#' Save protocol to RDS file
#'
#' @param protocol Protocol object to save
#' @param file Character. File path for saving
#' @export
save_protocol <- function(protocol, file) {

  origin <- "save_protocol"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin
    )

    protocol_export <- protocol$export_protocol()
    saveRDS(protocol_export, file = file)
    phr_message(phr_txt("Protocol saved to: {file}"), origin = origin)
  }, on_error = "abort", origin = origin)
}

#' Load protocol from RDS file
#'
#' @param file Character. File path to load from
#' @return List containing protocol data
#' @export
load_protocol <- function(file) {

  origin <- "load_protocol"

  phr_try({
    phr_assert(
      file.exists(file),
      message = phr_txt("File does not exist: '{file}'"),
      origin  = origin,
      hint    = phr_txt("Check the file path and ensure the file has not been moved or deleted.")
    )

    protocol_data <- readRDS(file)
    phr_message(phr_txt("Protocol loaded from: {file}"), origin = origin)
    protocol_data
  }, on_error = "abort", origin = origin)
}

#' Restore protocol object from exported data
#'
#' Restores a \code{\link{SurveyProtocol}} when the exported data contains
#' sampling fields (\code{sample_table}, \code{sampling_frame}, etc.),
#' otherwise restores a base \code{\link{Protocol}}.
#'
#' @param protocol_data List. Exported protocol data from export_protocol()
#' @return A new Protocol or SurveyProtocol object with restored data
#' @export
restore_protocol <- function(protocol_data) {

  origin <- "restore_protocol"

  phr_try({
    phr_assert(
      is.list(protocol_data) && !is.null(protocol_data$metadata),
      message = phr_txt("protocol_data must be a list produced by export_protocol()."),
      origin  = origin
    )

    has_sampling_data <- any(c("sample_table", "sampling_frame", "drawn_sample",
                               "drawn_sample_full") %in% names(protocol_data) &
                             !vapply(protocol_data[intersect(c("sample_table", "sampling_frame",
                                                               "drawn_sample", "drawn_sample_full"),
                                                             names(protocol_data))],
                                    is.null, logical(1)))

    if (has_sampling_data) {
      protocol <- SurveyProtocol$new(
        assessment_title = protocol_data$metadata$assessment_title,
        country_name     = protocol_data$metadata$country_name,
        month_year       = protocol_data$metadata$month_year
      )
      protocol$sample_table      <- protocol_data$sample_table
      protocol$sampling_frame    <- protocol_data$sampling_frame
      protocol$drawn_sample      <- protocol_data$drawn_sample
      protocol$drawn_sample_full <- protocol_data$drawn_sample_full
    } else {
      protocol <- Protocol$new(
        assessment_title = protocol_data$metadata$assessment_title,
        country_name     = protocol_data$metadata$country_name,
        month_year       = protocol_data$metadata$month_year
      )
    }

    protocol$metadata            <- protocol_data$metadata
    protocol$objectives          <- protocol_data$objectives %||% list()
    protocol$objective_schema    <- protocol_data$objective_schema %||% protocol$objective_schema
    if (!is.null(protocol_data$framework)) {
      protocol$framework <- restore_framework(protocol_data$framework)
    }
    protocol$tools               <- protocol_data$tools
    protocol$selected_indicators <- protocol_data$selected_indicators
    protocol$issues              <- protocol_data$issues

    phr_message(phr_txt("Protocol restored successfully."), origin = origin)
    protocol
  }, on_error = "abort", origin = origin)
}

#' Generate a Word document report from a Protocol or SurveyProtocol object
#'
#' Convenience wrapper around \code{Protocol$generate_reach_tor()} and
#' \code{SurveyProtocol$generate_reach_tor()}.  Dispatches to the correct method
#' based on the class of \code{protocol}.
#'
#' @param protocol A \code{\link{Protocol}} or \code{\link{SurveyProtocol}}
#'   object.
#' @param output_file Character. Output \code{.docx} file path.  Defaults to
#'   \code{"protocol_report.docx"} in the current working directory.
#' @param reference_docx Character or \code{NULL}. Path to a Word style
#'   reference document.  Uses the package-bundled template by default.
#' @param open Logical. Whether to open the file after writing.  Defaults to
#'   \code{FALSE}.
#' @return Invisibly returns the protocol object.
#' @export
generate_protocol_report <- function(protocol,
                                     output_file   = "protocol_report.docx",
                                     reference_docx = NULL,
                                     open           = FALSE) {

  origin <- "generate_protocol_report"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol or SurveyProtocol instance."),
      origin  = origin,
      hint    = phr_txt("Use Protocol$new() or create_survey_protocol() to create a valid object.")
    )

    protocol$generate_reach_tor(
      output_file    = output_file,
      reference_docx = reference_docx,
      open           = open
    )
  }, on_error = "abort", origin = origin)

  invisible(protocol)
}

#' @param protocol Protocol object to summarize
#' @export
print_protocol_summary <- function(protocol) {

  origin <- "print_protocol_summary"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin
    )

    summary <- protocol$get_protocol_summary()

    phr_message(
      phr_txt("Protocol: {summary$assessment_title} | Country: {summary$country_name} | {summary$month_year}"),
      origin = origin
    )
    phr_message(
      phr_txt("Objectives: {summary$num_objectives} | Strata: {if (is.null(summary$num_strata)) 'N/A' else summary$num_strata} | Tools: {summary$num_tools}"),
      origin = origin
    )

    if (summary$num_issues > 0) {
      issues <- protocol$get_issues()
      for (issue_name in names(issues)) {
        phr_warning(message = phr_txt("{issues[[issue_name]]}"), origin = origin)
      }
    } else {
      phr_message(phr_txt("No issues detected."), origin = origin)
    }
  }, on_error = "abort", origin = origin)
}

