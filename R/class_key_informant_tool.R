# --------------------------------------------------------------------------------
# R6 Class: KeyInformantTool (Subclass of Tool)
# --------------------------------------------------------------------------------
#
# This module provides the KeyInformantTool R6 class for managing key informant
# interview (KII) XLSForms used in Kobo, ODK, and other data collection applications.
#
# KeyInformantTool extends the base Tool class with functionality specific to
# KII surveys, including KII type classification.
#
# XLSForm Reference: https://xlsform.org/en/
# --------------------------------------------------------------------------------

#' @title KeyInformantTool R6 Class
#' @description
#' Subclass of Tool for managing key informant interview (KII) XLSForms.
#' KII tools are typically used for:
#' - Community-level key informant interviews
#' - Service provider interviews
#' - Market vendor interviews
#' - Sector-specific expert interviews
#'
#' @export
KeyInformantTool <- R6::R6Class(
"KeyInformantTool",
inherit = Tool,
cloneable = TRUE,
public = list(

  #' @description
  #' Initialize a new KeyInformantTool object.
  #'
  #' On initialization, the master survey and choices are loaded from the
  #' bundled \code{iphra_kii_tool_dummy.xlsx} template stored in the package
  #' \code{inst/resources} folder.  Any explicitly supplied \code{survey},
  #' \code{choices}, or \code{settings} arguments override the defaults.
  #'
  #' @param name Optional name for the tool.
  #' @param survey Optional data.frame to use as the master survey (overrides default).
  #' @param choices Optional data.frame to use as the master choices (overrides default).
  #' @param settings Optional data.frame to use as settings (overrides default).
  #' @param kii_type Type of KII (e.g., "community", "health", "fsl", "wash").
  #' @return A new KeyInformantTool object.
  initialize = function(name = NULL,
                        survey = NULL,
                        choices = NULL,
                        settings = NULL,
                        kii_type = "general") {
    super$initialize(
      name = name %||% "Key Informant Interview Tool",
      survey = survey,
      choices = choices,
      settings = settings
    )
    private$.tool_type <- "key_informant"
    private$.kii_type <- kii_type

    # Load the bundled default KII tool template if no overrides given
    if (is.null(survey) && is.null(choices)) {
      default_path <- system.file(
        "resources", "iphra_kii_tool_dummy.xlsx",
        package = "iphRa"
      )
      if (nchar(default_path) > 0) {
        private$.load_default_tool(default_path)
      }
    }

    invisible(self)
  },

  #' @description
  #' Get the KII type.
  #' @return Character string with KII type.
  get_kii_type = function() {
    private$.kii_type
  },

  #' @description
  #' Set the KII type.
  #' @param kii_type Character string for KII type.
  #' @return Invisibly returns self for method chaining.
  set_kii_type = function(kii_type) {
    valid_types <- c("general", "community", "health", "fsl", "wash",
                     "nutrition", "market", "service_provider")
    if (!kii_type %in% valid_types) {
      warning(paste("KII type", kii_type, "is not a recognized type.",
                    "Valid types:", paste(valid_types, collapse = ", ")))
    }
    private$.kii_type <- kii_type
    private$.touch()
    invisible(self)
  }
),

private = list(
  .kii_type = NULL
)
)
