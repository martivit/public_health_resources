#' Protocol Utility Functions
#'
#' @description
#' Utility functions for working with Protocol objects and protocol workflows.

# Minimum columns every master strata table must contain.
.strata_table_required_cols <- c(
  "stratum_id",
  "stratum_name",
  "total_population",
  "sampling_method_site",
  "sampling_method_hh",
  "pop_indicator",
  "General_HH_Sample_Size",
  "ind_indicator",
  "Ind_HH_Sample_Size",
  "rate_indicator",
  "Rate_HH_Sample_Size",
  "Ind_Sample_Size",
  "Rate_Ind_Sample_Size",
  "Rate_PT_Sample_Size",
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
#' @param sampling_frame Optional data frame to initialise the
#'   \code{\link{SamplingFrame}} with.  When \code{NULL} (default), an empty
#'   \code{SamplingFrame} is created.
#' @return A new SurveyProtocol object
#' @export
create_survey_protocol <- function(
  assessment_title = NULL,
  country_name = NULL,
  month_year = NULL,
  framework_type = "none",
  sampling_frame = NULL
) {
  SurveyProtocol$new(
    assessment_title = assessment_title,
    country_name = country_name,
    month_year = month_year,
    framework_type = framework_type,
    sampling_frame = sampling_frame
  )
}

#' Validate protocol completeness
#'
#' @param protocol Protocol object to validate
#' @return List with validation results and issues
#' @export
validate_protocol <- function(protocol) {
  origin <- "validate_protocol"

  phr_try(
    {
      phr_assert(
        inherits(protocol, "Protocol"),
        message = phr_txt("Object is not a Protocol instance."),
        origin = origin,
        hint = phr_txt(
          "Use Protocol$new() or SurveyProtocol$new() to create a Protocol object."
        )
      )

      issues <- protocol$get_issues()

      list(
        has_issues = length(issues) > 0,
        issues = issues
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Validate the master strata table structure
#'
#' Checks that a data frame conforms to the expected master strata table
#' structure with all required columns.  This is a standalone helper that
#' mirrors the \code{Protocol$validate_strata_table()} method.
#'
#' @param sample_table A data frame to validate (typically
#'   \code{protocol$sample_object} / \code{protocol$sample_table}).
#' @return \code{TRUE} if the table is valid, \code{FALSE} otherwise.
#' @export
validate_strata_table <- function(sample_table) {
  origin <- "validate_strata_table"

  if (is.null(sample_table) || !is.data.frame(sample_table)) {
    return(FALSE)
  }

  if (nrow(sample_table) == 0) {
    return(FALSE)
  }

  required_cols <- .strata_table_required_cols

  missing_cols <- setdiff(required_cols, names(sample_table))
  if (length(missing_cols) > 0) {
    return(FALSE)
  }

  dupes <- sample_table$stratum_id[duplicated(sample_table$stratum_id)]
  if (length(dupes) > 0) {
    return(FALSE)
  }

  TRUE
}

#' Save protocol to RDS file
#'
#' Saves the \code{Protocol} or \code{SurveyProtocol} object directly to an
#' RDS file.  Use \code{\link{load_protocol}} to restore it.
#'
#' @param protocol Protocol object to save
#' @param file Character. File path for saving
#' @export
save_protocol <- function(protocol, file) {
  origin <- "save_protocol"

  phr_try(
    {
      phr_assert(
        inherits(protocol, "Protocol"),
        message = phr_txt("Object is not a Protocol instance."),
        origin = origin
      )

      saveRDS(protocol, file = file)
      phr_message(phr_txt("Protocol saved to: {file}"), origin = origin)
    },
    on_error = "abort",
    origin = origin
  )
}

#' Load protocol from RDS file
#'
#' Loads a \code{Protocol} or \code{SurveyProtocol} object saved with
#' \code{\link{save_protocol}}.
#'
#' @param file Character. File path to load from
#' @return A \code{Protocol} or \code{SurveyProtocol} object
#' @export
load_protocol <- function(file) {
  origin <- "load_protocol"

  phr_try(
    {
      phr_assert(
        file.exists(file),
        message = phr_txt("File does not exist: '{file}'"),
        origin = origin,
        hint = phr_txt(
          "Check the file path and ensure the file has not been moved or deleted."
        )
      )

      protocol <- readRDS(file)
      phr_message(phr_txt("Protocol loaded from: {file}"), origin = origin)
      protocol
    },
    on_error = "abort",
    origin = origin
  )
}

#' Restore protocol object from exported data
#'
#' Restores a \code{\link{SurveyProtocol}} when the exported data contains
#' sampling fields (\code{sample_table}, \code{sampling_frame}, etc.),
#' otherwise restores a base \code{\link{Protocol}}.
#'
#' @param protocol_data List. Legacy exported protocol data (a list with a
#'   \code{metadata} element, as previously produced by the old
#'   \code{export_protocol()} method).
#' @return A new Protocol or SurveyProtocol object with restored data
#' @export
restore_protocol <- function(protocol_data) {
  origin <- "restore_protocol"

  phr_try(
    {
      phr_assert(
        is.list(protocol_data) && !is.null(protocol_data$metadata),
        message = phr_txt(
          "protocol_data must be a list with a 'metadata' element."
        ),
        origin = origin
      )

      has_sampling_data <- any(
        c(
          "sample_table",
          "sampling_frame",
          "drawn_sample",
          "drawn_sample_full"
        ) %in%
          names(protocol_data) &
          !vapply(
            protocol_data[intersect(
              c(
                "sample_table",
                "sampling_frame",
                "drawn_sample",
                "drawn_sample_full"
              ),
              names(protocol_data)
            )],
            is.null,
            logical(1)
          )
      )

      if (has_sampling_data) {
        protocol <- SurveyProtocol$new(
          assessment_title = protocol_data$metadata$assessment_title,
          country_name = protocol_data$metadata$country_name,
          month_year = protocol_data$metadata$month_year
        )
        if (
          !is.null(protocol_data$sample_object) &&
            inherits(protocol_data$sample_object, "Sample")
        ) {
          protocol$sample_object <- protocol_data$sample_object
        } else if (
          !is.null(protocol_data$sample_table) &&
            is.data.frame(protocol_data$sample_table)
        ) {
          if (
            is.null(protocol$sample_object) ||
              !inherits(protocol$sample_object, "Sample")
          ) {
            protocol$sample_object <- Sample$new()
          }
          protocol$sample_object$set_sample_table(protocol_data$sample_table)
        }
        # sampling_frame is exported as a raw data frame; restore into SamplingFrame object.
        if (
          !is.null(protocol_data$sampling_frame) &&
            is.data.frame(protocol_data$sampling_frame) &&
            nrow(protocol_data$sampling_frame) > 0
        ) {
          protocol$sampling_frame$log_df <- tibble::as_tibble(
            protocol_data$sampling_frame
          )
        }
        protocol$drawn_sample <- protocol_data$drawn_sample
        protocol$drawn_sample_full <- protocol_data$drawn_sample_full
      } else {
        protocol <- Protocol$new(
          assessment_title = protocol_data$metadata$assessment_title,
          country_name = protocol_data$metadata$country_name,
          month_year = protocol_data$metadata$month_year
        )
      }

      protocol$metadata <- protocol_data$metadata
      protocol$conditional_metadata <- protocol_data$conditional_metadata %||%
        list()
      if (!is.null(protocol_data$framework)) {
        protocol$framework <- restore_framework(protocol_data$framework)
      }
      protocol$tools <- protocol_data$tools
      # Support restoring both new field names and the old names from saved data
      protocol$framework_objective_catalog_master <-
        protocol_data$framework_objective_catalog_master %||%
        protocol_data$objective_catalog_master %||%
        protocol$framework_objective_catalog_master
      protocol$framework_objective_catalog_adjusted <-
        protocol_data$framework_objective_catalog_adjusted %||%
        protocol_data$objective_catalog_adjusted %||%
        protocol$framework_objective_catalog_adjusted
      protocol$framework_indicator_catalog_master <-
        protocol_data$framework_indicator_catalog_master %||%
        protocol_data$indicator_catalog_master %||%
        protocol$framework_indicator_catalog_master
      protocol$framework_indicator_catalog_adjusted <-
        protocol_data$framework_indicator_catalog_adjusted %||%
        protocol_data$indicator_catalog_adjusted %||%
        protocol$framework_indicator_catalog_adjusted
      protocol$tool_indicator_catalog_master <-
        protocol_data$tool_indicator_catalog_master %||%
        protocol$tool_indicator_catalog_master
      protocol$tool_indicator_catalog_revised <-
        protocol_data$tool_indicator_catalog_revised %||%
        protocol$tool_indicator_catalog_revised
      protocol$tool_objective_catalog_master <-
        protocol_data$tool_objective_catalog_master %||%
        protocol$tool_objective_catalog_master
      protocol$tool_objective_catalog_revised <-
        protocol_data$tool_objective_catalog_revised %||%
        protocol$tool_objective_catalog_revised
      protocol$issues <- protocol_data$issues
      if (
        "sync_state" %in% names(protocol) && is.function(protocol$sync_state)
      ) {
        protocol$sync_state()
      }

      phr_message(phr_txt("Protocol restored successfully."), origin = origin)
      protocol
    },
    on_error = "abort",
    origin = origin
  )
}

#' Generate a Word document report from a Protocol or SurveyProtocol object
#'
#' Convenience wrapper around \code{Protocol$generate_doc()} and
#' \code{SurveyProtocol$generate_doc()}.  Dispatches to the correct method
#' based on the class of \code{protocol}.
#'
#' @param protocol A \code{\link{Protocol}} or \code{\link{SurveyProtocol}}
#'   object.
#' @param output_file Character. Output \code{.docx} file path.  Defaults to
#'   \code{"protocol_report.docx"} in the current working directory.
#' @param open Logical. Whether to open the file after writing.  Defaults to
#'   \code{FALSE}.
#' @return Invisibly returns the protocol object.
#' @export
generate_protocol_report <- function(
  protocol,
  output_file = "protocol_report.docx",
  open = FALSE
) {
  origin <- "generate_protocol_report"

  phr_try(
    {
      phr_assert(
        inherits(protocol, "Protocol"),
        message = phr_txt(
          "Object is not a Protocol or SurveyProtocol instance."
        ),
        origin = origin,
        hint = phr_txt(
          "Use Protocol$new() or create_survey_protocol() to create a valid object."
        )
      )

      protocol$generate_doc(output_file = output_file, open = open)
    },
    on_error = "abort",
    origin = origin
  )

  invisible(protocol)
}

#' @param protocol Protocol object to summarize
#' @export
print_protocol_summary <- function(protocol) {
  origin <- "print_protocol_summary"

  phr_try(
    {
      phr_assert(
        inherits(protocol, "Protocol"),
        message = phr_txt("Object is not a Protocol instance."),
        origin = origin
      )

      md <- protocol$metadata

      phr_message(
        phr_txt(
          "Protocol: {md$assessment_title %||% 'N/A'} | Country: {md$country_name %||% 'N/A'} | {md$month_year %||% 'N/A'}"
        ),
        origin = origin
      )

      n_tools <- length(protocol$tools)
      n_objectives <- if (
        !is.null(protocol$framework) &&
          !is.null(protocol$framework$modified_objectives_schema) &&
          is.data.frame(protocol$framework$modified_objectives_schema) &&
          "objective_code" %in%
            names(protocol$framework$modified_objectives_schema)
      ) {
        length(unique(stats::na.omit(
          protocol$framework$modified_objectives_schema$objective_code
        )))
      } else {
        0L
      }

      phr_message(
        phr_txt("Objectives: {n_objectives} | Tools: {n_tools}"),
        origin = origin
      )

      issues <- protocol$get_issues()
      if (length(issues) > 0) {
        for (issue_name in names(issues)) {
          phr_warning(
            message = phr_txt("{issues[[issue_name]]}"),
            origin = origin
          )
        }
      } else {
        phr_message(phr_txt("No issues detected."), origin = origin)
      }
    },
    on_error = "abort",
    origin = origin
  )
}
