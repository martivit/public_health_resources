# --------------------------------------------------------------------------------
# R6 Class: ObservationTool (Subclass of Tool)
# --------------------------------------------------------------------------------
#
# This module provides the ObservationTool R6 class for managing observation/checklist
# XLSForms used in Kobo, ODK, and other data collection applications.
#
# ObservationTool extends the base Tool class with functionality specific to
# observation surveys, including checklist item extraction and observation type
# classification.
#
# XLSForm Reference: https://xlsform.org/en/
# --------------------------------------------------------------------------------

#' @title ObservationTool R6 Class
#' @description
#' Subclass of Tool for managing observation/checklist XLSForms.
#' Observation tools are typically used for:
#' - Water point observations
#' - Latrine/sanitation facility observations
#' - Health facility observations
#' - Market observations
#' - Community observations
#'
#' @export
ObservationTool <- R6::R6Class(
"ObservationTool",
inherit = Tool,
public = list(

  #' @description
  #' Initialize a new ObservationTool object.
  #'
  #' On initialization, the master survey and choices are loaded from the
  #' bundled \code{iphra_observation_tool_dummy.xlsx} template stored in the
  #' package \code{inst/resources} folder.  Any explicitly supplied
  #' \code{survey}, \code{choices}, or \code{settings} arguments override
  #' the defaults.
  #'
  #' @param name Optional name for the tool.
  #' @param survey Optional data.frame to use as the master survey (overrides default).
  #' @param choices Optional data.frame to use as the master choices (overrides default).
  #' @param settings Optional data.frame to use as settings (overrides default).
  #' @param observation_type Type of observation (e.g., "water_point", "latrine").
  #' @return A new ObservationTool object.
  initialize = function(name = NULL,
                        survey = NULL,
                        choices = NULL,
                        settings = NULL,
                        observation_type = "general") {
    super$initialize(
      name = name %||% "Observation Tool",
      survey = survey,
      choices = choices,
      settings = settings
    )
    private$.tool_type <- "observation"
    private$.observation_type <- observation_type

    # Load the bundled default observation tool template if no overrides given
    if (is.null(survey) && is.null(choices)) {
      default_path <- system.file(
        "resources", "iphra_observation_tool_dummy.xlsx",
        package = "iphRa"
      )
      if (nchar(default_path) > 0) {
        private$.load_default_tool(default_path)
      }
    }

    invisible(self)
  },

  #' @description
  #' Get the observation type.
  #' @return Character string with observation type.
  get_observation_type = function() {
    private$.observation_type
  },

  #' @description
  #' Set the observation type.
  #' @param observation_type Character string for observation type.
  #' @return Invisibly returns self for method chaining.
  set_observation_type = function(observation_type) {
    valid_types <- c("general", "water_point", "latrine", "health_facility",
                     "nutrition_facility", "market", "community", "livelihoods",
                     "graves")
    if (!observation_type %in% valid_types) {
      warning(paste("Observation type", observation_type, "is not a recognized type.",
                    "Valid types:", paste(valid_types, collapse = ", ")))
    }
    private$.observation_type <- observation_type
    private$.touch()
    invisible(self)
  },

  #' @description
  #' Get observation checklist items.
  #' Filters for yes/no or binary questions typically used in checklists.
  #' @return Data frame with checklist-type questions.
  get_checklist_items = function() {
    survey <- self$survey
    if (!"type" %in% names(survey)) {
      return(data.frame())
    }

    # Look for select_one yes_no or similar binary choices
    is_binary <- grepl("select_one\\s+(yes_?no|yesno|binary|true_?false)", survey$type,
                       ignore.case = TRUE)
    survey[is_binary, , drop = FALSE]
  },

  #' @description
  #' Print summary of the observation tool.
  print = function() {
    cat("XLSForm Observation Tool\n")
    cat("--------------------------\n")
    cat("Name:", private$.name, "\n")
    cat("Type:", private$.tool_type, "\n")
    cat("Observation Type:", private$.observation_type, "\n")
    cat("Created:", format(private$.created_at, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Modified:", format(private$.modified_at, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Questions:", nrow(self$survey), "\n")
    cat("Checklist Items:", nrow(self$get_checklist_items()), "\n")
    cat("Choice Lists:", length(unique(self$choices$list_name)), "\n")
    cat("Selected Indicators:", length(private$.selected_indicators), "\n")
    invisible(self)
  }
),

private = list(
  .observation_type = NULL
)
)
