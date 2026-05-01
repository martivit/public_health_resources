#' IPHRAProtocol R6 Class
#'
#' @description
#' Subclass of \code{\link{Protocol}} for the Integrated Population Health
#' and Risk Assessment (IPHRA) methodology.  On initialisation the class
#' automatically creates an \code{\link{ANAFramework}} and exposes a curated
#' list of approved IPHRA data collection tools.
#'
#' Calling \code{add_tools(tool_name)} with one of the recognised tool names
#' listed below will instantiate the correct \code{\link{Tool}} subclass,
#' load its bundled XLSForm template from the package resources, and register
#' it in \code{self$tools} under the supplied name.
#'
#' Recognised tool names and their types:
#' \describe{
#'   \item{tool_household_iphra_v2}{\code{\link{HouseholdTool}}}
#'   \item{tool_kii_community_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_fsl_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_wash_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_markets_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_nutrition_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_kii_was_service_provider_iphra_v2}{\code{\link{KeyInformantTool}}}
#'   \item{tool_obs_community_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_crop_livestock_iphra_v1}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_health_facility_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_latrine_iphra_v2}{\code{\link{ObservationTool}}}
#'   \item{tool_obs_water_point_iphra_v2}{\code{\link{ObservationTool}}}
#' }
#'
#' @importFrom R6 R6Class
#' @export
IPHRAProtocol <- R6::R6Class(
  "IPHRAProtocol",
  inherit = Protocol,

  public = list(

    #' @description
    #' Creates a new IPHRAProtocol object.
    #'
    #' An \code{\link{ANAFramework}} is automatically created and stored in
    #' \code{self$framework}.
    #'
    #' @param assessment_title Character. Title of the assessment.
    #' @param country_name Character. Country where the assessment takes place.
    #' @param month_year Character. Month and year of data collection
    #'   (e.g. \code{"January 2024"}).
    #' @return A new IPHRAProtocol object.
    initialize = function(assessment_title = NULL,
                          country_name     = NULL,
                          month_year       = NULL) {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = "ana"
      )
      phr_message(phr_txt("IPHRAProtocol initialized."), origin = "IPHRAProtocol$initialize")
      invisible(self)
    },

    #' @description Add an IPHRA data collection tool by name.
    #'
    #' The \code{tool_name} must be one of the recognised IPHRA tool names
    #' (see class description).  The appropriate \code{\link{Tool}} subclass
    #' is instantiated, its bundled XLSForm template is loaded from the
    #' package resources folder (when available), and the object is stored
    #' in \code{self$tools[[tool_name]]}.
    #'
    #' @param tool_name Character. One of the recognised IPHRA tool names.
    #' @return Invisibly returns \code{self} for method chaining.
    add_tools = function(tool_name) {
      phr_try({
        allowable <- private$.iphra_tools

        phr_assert(
          is.character(tool_name) && length(tool_name) == 1 && nzchar(tool_name),
          message = phr_txt("tool_name must be a non-empty character string."),
          origin  = "IPHRAProtocol$add_tools"
        )
        phr_assert(
          tool_name %in% names(allowable),
          message = phr_txt(
            "'{tool_name}' is not a recognised IPHRA tool. Allowable tools: {paste(names(allowable), collapse=', ')}."
          ),
          origin = "IPHRAProtocol$add_tools"
        )

        tool_spec  <- allowable[[tool_name]]
        tool_class <- tool_spec$class
        xlsx_file  <- tool_spec$file

        # Locate the XLSForm resource file (gracefully handles missing files)
        tool_path <- system.file("resources", xlsx_file, package = "phr")
        if (!nzchar(tool_path) || !file.exists(tool_path)) {
          tool_path <- file.path("resources", xlsx_file)
        }

        tool <- if (identical(tool_class, "HouseholdTool")) {
          if (file.exists(tool_path)) {
            t <- HouseholdTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            HouseholdTool$new(name = tool_name)
          }
        } else if (identical(tool_class, "KeyInformantTool")) {
          if (file.exists(tool_path)) {
            t <- KeyInformantTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            KeyInformantTool$new(name = tool_name)
          }
        } else {
          # ObservationTool
          if (file.exists(tool_path)) {
            t <- ObservationTool$new(name = tool_name, survey = NULL, choices = NULL)
            private$.load_tool_from_path(t, tool_path)
            t
          } else {
            phr_warning(
              message = phr_txt("XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."),
              origin  = "IPHRAProtocol$add_tools"
            )
            ObservationTool$new(name = tool_name)
          }
        }

        if (is.null(self$tools)) self$tools <- list()
        self$tools[[tool_name]] <- tool
        self$metadata$modified_date <- Sys.time()
        phr_message(
          phr_txt("IPHRA tool '{tool_name}' added."),
          origin = "IPHRAProtocol$add_tools"
        )
      }, on_error = "abort", origin = "IPHRAProtocol$add_tools")
      invisible(self)
    },

    #' @description Return the names of all allowable IPHRA tools.
    #' @return Character vector of tool names.
    get_allowable_tools = function() {
      names(private$.iphra_tools)
    }
  ),

  private = list(

    # Named list: tool_name -> list(class = <class_name>, file = <xlsx_filename>)
    .iphra_tools = list(
      tool_household_iphra_v2 = list(
        class = "HouseholdTool",
        file  = "tool_household_iphra_v2.xlsx"
      ),
      tool_kii_community_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_community_iphra_v2.xlsx"
      ),
      tool_fsl_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_fsl_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_wash_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_wash_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_markets_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_markets_iphra_v2.xlsx"
      ),
      tool_kii_nutrition_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_nutrition_provider_iphra_v2.xlsx"
      ),
      tool_kii_was_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file  = "tool_kii_was_service_provider_iphra_v2.xlsx"
      ),
      tool_obs_community_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_community_iphra_v2.xlsx"
      ),
      tool_obs_crop_livestock_iphra_v1 = list(
        class = "ObservationTool",
        file  = "tool_obs_crop_livestock_iphra_v1.xlsx"
      ),
      tool_obs_health_facility_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_health_facility_iphra_v2.xlsx"
      ),
      tool_obs_latrine_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_latrine_iphra_v2.xlsx"
      ),
      tool_obs_water_point_iphra_v2 = list(
        class = "ObservationTool",
        file  = "tool_obs_water_point_iphra_v2.xlsx"
      )
    ),

    # Load survey/choices/settings from an xlsx path into an existing Tool object.
    .load_tool_from_path = function(tool, path) {
      if (!file.exists(path)) return(invisible(NULL))
      available_sheets <- tryCatch(
        readxl::excel_sheets(path),
        error = function(e) character(0)
      )
      if ("survey" %in% available_sheets) {
        sv <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "survey")),
          error = function(e) NULL
        )
        if (!is.null(sv)) tool$survey <- sv
      }
      if ("choices" %in% available_sheets) {
        ch <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "choices")),
          error = function(e) NULL
        )
        if (!is.null(ch)) tool$choices <- ch
      }
      if ("settings" %in% available_sheets) {
        st <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "settings")),
          error = function(e) NULL
        )
        if (!is.null(st)) tool$settings <- st
      }
      invisible(NULL)
    }
  )
)
