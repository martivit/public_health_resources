# --------------------------------------------------------------------------------
# R6 Class: HouseholdTool (Subclass of Tool)
# --------------------------------------------------------------------------------
#
# This module provides the HouseholdTool R6 class for managing household survey
# XLSForms used in Kobo, ODK, and other data collection applications.
#
# HouseholdTool extends the base Tool class with functionality specific to
# household surveys, including roster/repeat group management.
#
# XLSForm Reference: https://xlsform.org/en/
# --------------------------------------------------------------------------------

#' @title HouseholdTool R6 Class
#' @description
#' Subclass of Tool for managing household survey XLSForms.
#' Household surveys typically include:
#' - Household-level questions (head of household, shelter, etc.)
#' - Roster/loop for household members
#' - Sectoral questions (FSL, WASH, Health, etc.)
#'
#' @export
HouseholdTool <- R6::R6Class(
"HouseholdTool",
inherit = Tool,
public = list(

  #' @description
  #' Initialize a new HouseholdTool object.
  #'
  #' On initialization, the master survey and choices are loaded from the
  #' bundled \code{iphra_tool_v2.xlsx} template stored in the package
  #' \code{inst/resources} folder.  Any explicitly supplied \code{survey},
  #' \code{choices}, or \code{settings} arguments override the defaults.
  #'
  #' @param name Optional name for the tool.
  #' @param survey Optional data.frame to use as the master survey (overrides default).
  #' @param choices Optional data.frame to use as the master choices (overrides default).
  #' @param settings Optional data.frame to use as settings (overrides default).
  #' @return A new HouseholdTool object.
  initialize = function(name = NULL,
                        survey = NULL,
                        choices = NULL,
                        settings = NULL) {
    super$initialize(
      name = name %||% "Household Survey Tool",
      survey = survey,
      choices = choices,
      settings = settings
    )
    private$.tool_type <- "household"

    # Load the bundled default household tool template if no overrides given
    if (is.null(survey) && is.null(choices)) {
      default_path <- system.file(
        "resources", "iphra_tool_v2.xlsx",
        package = "iphRa"
      )
      if (nchar(default_path) > 0) {
        private$.load_default_tool(default_path)
      }
    }

    invisible(self)
  },

  #' @description
  #' Get roster/repeat groups from the survey.
  #' @return Data frame with roster/repeat group definitions.
  get_roster_groups = function() {
    survey <- self$survey
    if (!"type" %in% names(survey)) {
      return(data.frame())
    }
    survey[survey$type %in% c("begin_repeat", "end_repeat"), , drop = FALSE]
  },

  #' @description
  #' Check if the survey has a roster/repeat section.
  #' @return Logical indicating if roster exists.
  has_roster = function() {
    nrow(self$get_roster_groups()) > 0
  },

  #' @description
  #' Get questions within a specific roster/repeat group.
  #' @param roster_name Name of the roster/repeat group.
  #' @return Data frame with questions in the roster.
  get_roster_questions = function(roster_name) {
    survey <- self$survey
    if (!"type" %in% names(survey) || !"name" %in% names(survey)) {
      return(data.frame())
    }

    # Find start and end of roster
    begin_idx <- which(survey$type == "begin_repeat" & survey$name == roster_name)
    if (length(begin_idx) == 0) {
      return(data.frame())
    }

    # Find matching end_repeat
    end_idx <- which(survey$type == "end_repeat")
    end_idx <- end_idx[end_idx > begin_idx[1]]
    if (length(end_idx) == 0) {
      end_idx <- nrow(survey)
    } else {
      end_idx <- end_idx[1]
    }

    survey[(begin_idx[1] + 1):(end_idx - 1), , drop = FALSE]
  },

  #' @description
  #' Print summary of the household tool.
  print = function() {
    cat("XLSForm Household Tool\n")
    cat("------------------------\n")
    cat("Name:", private$.name, "\n")
    cat("Type:", private$.tool_type, "\n")
    cat("Created:", format(private$.created_at, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Modified:", format(private$.modified_at, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Master Survey Questions:", nrow(self$survey), "\n")
    cat("Has Roster:", self$has_roster(), "\n")
    if (!is.null(private$.modified_survey)) {
      cat("Modified Survey Questions:", nrow(private$.modified_survey), "\n")
    }
    cat("Choice Lists:", length(unique(self$choices$list_name)), "\n")
    cat("Selected Indicators:", length(private$.selected_indicators), "\n")
    invisible(self)
  }
)
)
