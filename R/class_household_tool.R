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
cloneable = TRUE,
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
  }
)
)
