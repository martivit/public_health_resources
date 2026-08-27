#' IPHRAProtocol R6 Class
#'
#' @description
#' Protocol subclass for Integrated Public Health Rapid Assessment (IPHRA)
#' workflows. `IPHRAProtocol` extends `SurveyProtocol` with IPHRA-specific
#' tools, ANA framework initialization, household/KII/observation reporting
#' tables, data analysis plan outputs, and Quarto parameters for IPHRA Terms of
#' Reference generation.
#'
#' @details
#' `IPHRAProtocol` automatically initializes an ANA framework and uses the
#' IPHRA Terms of Reference reference document template. It provides helpers for
#' adding bundled IPHRA XLSForm tools, detecting which IPHRA tools and
#' household indicators are present, updating household mortality recall dates,
#' and preparing protocol tables for Quarto rendering.
#'
#' @section Inheritance:
#' Inherits from [`SurveyProtocol`], which extends [`Protocol`] with sampling
#' and sampling-frame functionality.
#'
#' @section Key methods:
#' \describe{
#'   \item{\code{initialize()}}{Create a new IPHRA protocol with an ANA framework.}
#'   \item{\code{add_tools()}}{Add a recognised IPHRA household, KII, or observation tool.}
#'   \item{\code{get_allowable_tools()}}{Return all recognised IPHRA tool names.}
#'   \item{\code{update_recall_date()}}{Update recall-date calculate rows in the household tool.}
#'   \item{\code{get_quarto_params()}}{Return IPHRA-specific parameters for Quarto rendering.}
#' }
#'
#' @section Active bindings:
#' The class provides read-only active bindings for tool-presence flags,
#' household indicator flags, tools/sample summary tables, pillar/sub-pillar
#' tables, DAP tables, and the modified framework SVG path used in report
#' generation.
#' \describe{
#'   \item{\code{.tool_household_iphra}}{Logical flag indicating if a household IPHRA tool is registered.}
#'   \item{\code{.tool_community_kii}}{Logical flag indicating if a community KII tool is registered.}
#'   \item{\code{.tool_fsl_provider_kii}}{Logical flag indicating if an FSL provider KII tool is registered.}
#'   \item{\code{.tool_market_kii}}{Logical flag indicating if a market KII tool is registered.}
#'   \item{\code{.tool_health_facility_kii}}{Logical flag indicating if a health facility KII tool is registered.}
#'   \item{\code{.tool_nutrition_facility_kii}}{Logical flag indicating if a nutrition facility KII tool is registered.}
#'   \item{\code{.tool_wash_provider_kii}}{Logical flag indicating if a WASH provider KII tool is registered.}
#'   \item{\code{.tool_community_observation}}{Logical flag indicating if a community observation tool is registered.}
#'   \item{\code{.tool_crops_livestock_observation}}{Logical flag indicating if a crops/livestock observation tool is registered.}
#'   \item{\code{.tool_health_facility_observation}}{Logical flag indicating if a health facility observation tool is registered.}
#'   \item{\code{.tool_latrine_observation}}{Logical flag indicating if a latrine observation tool is registered.}
#'   \item{\code{.tool_water_point_observation}}{Logical flag indicating if a water point observation tool is registered.}
#'   \item{\code{.ind_ecfies}}{Logical flag indicating if ECFIES indicator (10801) is included.}
#'   \item{\code{.ind_iycfe}}{Logical flag indicating if IYCF-E indicator (10802) is included.}
#'   \item{\code{.ind_measles_vaccination}}{Logical flag indicating if measles vaccination indicator (14304) is included.}
#'   \item{\code{.ind_muac_children}}{Logical flag indicating if MUAC children indicator (10701) is included.}
#'   \item{\code{.ind_muac_women}}{Logical flag indicating if MUAC women indicator (10702) is included.}
#'   \item{\code{.ind_vitamin_a_coverage}}{Logical flag indicating if vitamin A coverage indicator (14305) is included.}
#'   \item{\code{.ind_mortality}}{Logical flag indicating if mortality indicators (10501, 10502) are included.}
#'   \item{\code{.ind_fcs}}{Logical flag indicating if FCS indicator (11205) is included.}
#'   \item{\code{.ind_rcsi}}{Logical flag indicating if rCSI indicator (11202) is included.}
#'   \item{\code{.ind_hhs}}{Logical flag indicating if HHS indicator (11201) is included.}
#'   \item{\code{.ind_lcsi}}{Logical flag indicating if LCSI indicator (12301) is included.}
#'   \item{\code{.ind_hwise}}{Logical flag indicating if HWISE indicator (11701) is included.}
#'   \item{\code{.ind_lppd}}{Logical flag indicating if LPPD indicator (10901) is included.}
#'   \item{\code{.tools_table_df}}{Data frame summarizing registered tools with sampling methods and sample sizes.}
#'   \item{\code{.sample_table_df}}{Data frame of the nested sample table.}
#'   \item{\code{.pillars_table_df}}{Data frame of framework pillars.}
#'   \item{\code{.subpillars_table_df}}{Data frame of framework sub-pillars.}
#'   \item{\code{.household_dap_df}}{Data frame containing the household data analysis plan.}
#'   \item{\code{.community_kii_dap_df}}{Data frame containing the community KII data analysis plan.}
#'   \item{\code{.fsl_provider_kii_dap_df}}{Data frame containing the FSL provider KII data analysis plan.}
#'   \item{\code{.market_kii_dap_df}}{Data frame containing the market KII data analysis plan.}
#'   \item{\code{.health_facility_kii_dap_df}}{Data frame containing the health facility KII data analysis plan.}
#'   \item{\code{.nutrition_facility_kii_dap_df}}{Data frame containing the nutrition facility KII data analysis plan.}
#'   \item{\code{.wash_provider_kii_dap_df}}{Data frame containing the WASH provider KII data analysis plan.}
#'   \item{\code{.community_observation_dap_df}}{Data frame containing the community observation data analysis plan.}
#'   \item{\code{.crops_livestock_observation_dap_df}}{Data frame containing the crops/livestock observation data analysis plan.}
#'   \item{\code{.health_facility_observation_dap_df}}{Data frame containing the health facility observation data analysis plan.}
#'   \item{\code{.water_point_observation_dap_df}}{Data frame containing the water point observation data analysis plan.}
#'   \item{\code{.latrine_observation_dap_df}}{Data frame containing the latrine observation data analysis plan.}
#' }
#'
#' @section Supported IPHRA tools:
#' The class supports the bundled household tool, community KII, FSL provider
#' KII, WASH provider KII, market KII, nutrition provider KII, health provider
#' KII, community observation, crop/livestock observation, health facility
#' observation, latrine observation, and water point observation tools.
#'
#' @examples
#' \dontrun{
#' protocol <- IPHRAProtocol$new(
#'   assessment_title = "Example IPHRA",
#'   country_name = "Example Country",
#'   month_year = "January 2027"
#' )
#'
#' protocol$get_allowable_tools()
#' protocol$add_tools("tool_household_iphra_v2")
#' protocol$update_recall_date("2027-01-01")
#' protocol$.tools_table_df
#' }
#'
#' @importFrom R6 R6Class
#' @export
IPHRAProtocol <- R6::R6Class(
  "IPHRAProtocol",
  inherit = SurveyProtocol,

  public = list(
    #' @description
    #' Creates a new IPHRAProtocol object.
    #'
    #' An \code{\link{ANAFramework}} is automatically created and stored in
    #' \code{self$framework}.
    #'
    #' @param assessment_title Character. The title of the assessment.
    #' @param country_name Character. The name of the country.
    #' @param month_year Character. The month and year of the assessment.
    #'
    #' @return A new IPHRAProtocol object.
    initialize = function(
      assessment_title = NULL,
      country_name = NULL,
      month_year = NULL
    ) {
      super$initialize(
        assessment_title = assessment_title,
        country_name = country_name,
        month_year = month_year,
        framework_type = "ana",
        reference_doc_filename = "reach_tor_iphra_template.docx"
      )
      self$valid_tool_types <- c(
        "household",
        "key_informant",
        "observation",
        "generic"
      )
      self$metadata$assessment_title <- assessment_title
      self$metadata$country_name <- country_name
      self$metadata$month_year <- month_year
      self$metadata$framework_type <- "ana"

      phrutils::phr_message(
        phr_txt("IPHRAProtocol initialized."),
        origin = "IPHRAProtocol$initialize"
      )
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
      phrutils::phr_try(
        {
          allowable <- private$..iphra_tools

          phrutils::phr_assert(
            is.character(tool_name) &&
              length(tool_name) == 1 &&
              nzchar(tool_name),
            message = phr_txt(
              "tool_name must be a non-empty character string."
            ),
            origin = "IPHRAProtocol$add_tools"
          )
          phrutils::phr_assert(
            tool_name %in% names(allowable),
            message = phr_txt(
              "'{tool_name}' is not a recognised IPHRA tool. Allowable tools: {paste(names(allowable), collapse=', ')}."
            ),
            origin = "IPHRAProtocol$add_tools"
          )

          tool_spec <- allowable[[tool_name]]
          tool_class <- tool_spec$class
          xlsx_file <- tool_spec$file

          # Locate the XLSForm resource file (gracefully handles missing files)
          tool_path <- system.file("resources", xlsx_file, package = "phr")
          if (!nzchar(tool_path) || !file.exists(tool_path)) {
            tool_path <- file.path("resources", xlsx_file)
          }

          tool <- if (identical(tool_class, "HouseholdTool")) {
            if (file.exists(tool_path)) {
              t <- HouseholdTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phrutils::phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              HouseholdTool$new(name = tool_name)
            }
          } else if (identical(tool_class, "KeyInformantTool")) {
            if (file.exists(tool_path)) {
              t <- KeyInformantTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phrutils::phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              KeyInformantTool$new(name = tool_name)
            }
          } else {
            # ObservationTool
            if (file.exists(tool_path)) {
              t <- ObservationTool$new(
                name = tool_name,
                survey = NULL,
                choices = NULL
              )
              private$..load_tool_from_path(t, tool_path)
              t
            } else {
              phrutils::phr_warning(
                message = phr_txt(
                  "XLSForm file not found for '{tool_name}': {xlsx_file}. Creating empty tool."
                ),
                origin = "IPHRAProtocol$add_tools"
              )
              ObservationTool$new(name = tool_name)
            }
          }

          if (is.null(self$tools)) {
            self$tools <- list()
          }
          self$tools[[tool_name]] <- tool
          private$..touch()
          phrutils::phr_message(
            phr_txt("IPHRA tool '{tool_name}' added."),
            origin = "IPHRAProtocol$add_tools"
          )
        },
        on_error = "abort",
        origin = "IPHRAProtocol$add_tools"
      )
      invisible(self)
    },

    #' @description Return the names of all allowable IPHRA tools.
    #' @return Character vector of tool names.
    get_allowable_tools = function() {
      names(private$..iphra_tools)
    },

    #' @description Update the recall date in the household tool's calculate rows.
    #'
    #' Updates the \code{calculation} column of the \code{recall_event},
    #' \code{recall_date}, and \code{recall_month} rows in the
    #' \code{tool_household_iphra_v2} tool (both \code{survey} and
    #' \code{revised_survey}).  The recall date controls the mortality recall
    #' period used in the form.
    #'
    #' @param recall_date Character or Date. The recall start date.  Accepts a
    #'   \code{Date} object or a character string in \code{"YYYY-MM-DD"} format.
    #' @param tool_name Character. Name of the household tool to update.
    #'   Defaults to \code{"tool_household_iphra_v2"}.
    #' @return Invisibly returns \code{self} for method chaining.
    update_recall_date = function(
      recall_date,
      tool_name = "tool_household_iphra_v2"
    ) {
      phrutils::phr_try(
        {
          phrutils::phr_assert(
            !is.null(recall_date),
            message = phr_txt("recall_date must not be NULL."),
            origin = "IPHRAProtocol$update_recall_date"
          )

          # Normalise to Date then format
          date_obj <- tryCatch(
            as.Date(recall_date),
            error = function(e) {
              phr_error(
                message = phr_txt(
                  "recall_date could not be coerced to a Date: {conditionMessage(e)}"
                ),
                origin = "IPHRAProtocol$update_recall_date"
              )
            }
          )

          date_str <- format(date_obj, "%Y-%m-%d")
          month_first <- format(date_obj, "%Y-%m-01")
          recall_event_str <- format(date_obj, "%d %B %Y")

          phrutils::phr_assert(
            !is.null(self$tools) && tool_name %in% names(self$tools),
            message = phr_txt(
              "Tool '{tool_name}' not found. Add it first with add_tools()."
            ),
            origin = "IPHRAProtocol$update_recall_date"
          )

          tool <- self$tools[[tool_name]]

          .update_recall_in_survey <- function(sv) {
            if (
              is.null(sv) ||
                !"name" %in% names(sv) ||
                !"calculation" %in% names(sv)
            ) {
              return(sv)
            }
            idx_event <- which(sv$name == "recall_event")
            idx_date <- which(sv$name == "recall_date")
            idx_month <- which(sv$name == "recall_month")
            if (length(idx_event) > 0) {
              sv$calculation[idx_event] <- paste0(
                "if(1=1, '",
                recall_event_str,
                "','')"
              )
            }
            if (length(idx_date) > 0) {
              sv$calculation[idx_date] <- paste0(
                "if(1=1, date('",
                date_str,
                "'),'')"
              )
            }
            if (length(idx_month) > 0) {
              sv$calculation[idx_month] <- paste0(
                "if(1=1, date('",
                month_first,
                "'),'')"
              )
            }
            sv
          }

          tool$survey <- .update_recall_in_survey(tool$survey)
          tool$revised_survey <- .update_recall_in_survey(tool$revised_survey)

          private$..touch()
          phrutils::phr_message(
            phr_txt(
              "Recall date updated to '{date_str}' in tool '{tool_name}'."
            ),
            origin = "IPHRAProtocol$update_recall_date"
          )
        },
        on_error = "abort",
        origin = "IPHRAProtocol$update_recall_date"
      )
      invisible(self)
    },

    #' @description Get Quarto parameters for rendering.
    #' @return A named list of parameters to pass to Quarto rendering.
    get_quarto_params = function() {
      params <- super$get_quarto_params()

      c(
        params,
        list(
          anf_framework_path = self$.modified_framework_svg,
          tool_household = self$.tool_household_iphra,
          tool_community_kii = self$.tool_community_kii,
          tool_fsl_provider_kii = self$.tool_fsl_provider_kii,
          tool_market_kii = self$.tool_market_kii,
          tool_health_facility_kii = self$.tool_health_facility_kii,
          tool_nutrition_facility_kii = self$.tool_nutrition_facility_kii,
          tool_wash_provider_kii = self$.tool_wash_provider_kii,
          ind_ecfies = self$.ind_ecfies,
          ind_iycfe = self$.ind_iycfe,
          ind_measles_vaccination = self$.ind_measles_vaccination,
          ind_muac_children = self$.ind_muac_children,
          ind_muac_women = self$.ind_muac_women,
          ind_vitamin_a_coverage = self$.ind_vitamin_a_coverage,
          ind_mortality = self$.ind_mortality,
          ind_fcs = self$.ind_fcs,
          ind_rcsi = self$.ind_rcsi,
          ind_hhs = self$.ind_hhs,
          ind_lcsi = self$.ind_lcsi,
          ind_hwise = self$.ind_hwise,
          ind_lppd = self$.ind_lppd,
          tool_community_observation = self$.tool_community_observation,
          tool_crops_livestock_observation = self$.tool_crops_livestock_observation,
          tool_health_facility_observation = self$.tool_health_facility_observation,
          tool_latrine_observation = self$.tool_latrine_observation,
          tool_water_point_observation = self$.tool_water_point_observation,
          tools_table_df = private$..sanitize_quarto_df(self$.tools_table_df),
          household_pillars_table_df = private$..sanitize_quarto_df(self$.household_pillars_table_df),
          kii_pillars_table_df = private$..sanitize_quarto_df(self$.kii_pillars_table_df),
          observation_pillars_table_df = private$..sanitize_quarto_df(self$.observation_pillars_table_df),
          household_dap_df = private$..sanitize_quarto_df(
            self$.household_dap_df
          ),
          community_kii_dap_df = private$..sanitize_quarto_df(
            self$.community_kii_dap_df
          ),
          community_observation_dap_df = private$..sanitize_quarto_df(
            self$.community_observation_dap_df
          ),
          health_facility_kii_dap_df = private$..sanitize_quarto_df(
            self$.health_facility_kii_dap_df
          ),
          health_facility_observation_dap_df = private$..sanitize_quarto_df(
            self$.health_facility_observation_dap_df
          ),
          nutrition_facility_kii_dap_df = private$..sanitize_quarto_df(
            self$.nutrition_facility_kii_dap_df
          ),
          fsl_provider_kii_dap_df = private$..sanitize_quarto_df(
            self$.fsl_provider_kii_dap_df
          ),
          market_kii_dap_df = private$..sanitize_quarto_df(
            self$.market_kii_dap_df
          ),
          crop_livestock_observation_dap_df = private$..sanitize_quarto_df(
            self$.crop_livestock_observation_dap_df
          ),
          wash_provider_kii_dap_df = private$..sanitize_quarto_df(
            self$.wash_provider_kii_dap_df
          ),
          water_point_observation_dap_df = private$..sanitize_quarto_df(
            self$.water_point_observation_dap_df
          ),
          latrine_observation_dap_df = private$..sanitize_quarto_df(
            self$.latrine_observation_dap_df
          )
        )
      )
    }
  ),

  active = list(
    #' @field .tool_household_iphra Logical flag indicating if a household IPHRA tool is registered.
    .tool_household_iphra = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("household")
    },

    #' @field .tool_community_kii Logical flag indicating if a community KII tool is registered.
    .tool_community_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_community")
    },
    #' @field .tool_fsl_provider_kii Logical flag indicating if an FSL provider KII tool is registered.
    .tool_fsl_provider_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_fsl_service_provider")
    },
    #' @field .tool_market_kii Logical flag indicating if a market KII tool is registered.
    .tool_market_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_markets")
    },
    #' @field .tool_health_facility_kii Logical flag indicating if a health facility KII tool is registered.
    .tool_health_facility_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_health_service_provider")
    },
    #' @field .tool_nutrition_facility_kii Logical flag indicating if a nutrition facility KII tool is registered.
    .tool_nutrition_facility_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_nutrition_service_provider")
    },
    #' @field .tool_wash_provider_kii Logical flag indicating if a WASH provider KII tool is registered.
    .tool_wash_provider_kii = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("kii_wash_service_provider")
    },
    #' @field .ind_ecfies Logical flag indicating if ECFIES indicator (10801) is included.
    .ind_ecfies = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10801"))
    },
    #' @field .ind_iycfe Logical flag indicating if IYCF-E indicator (10802) is included.
    .ind_iycfe = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10802"))
    },
    #' @field .ind_measles_vaccination Logical flag indicating if measles vaccination indicator (14304) is included.
    .ind_measles_vaccination = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("14304"))
    },
    #' @field .ind_muac_children Logical flag indicating if MUAC children indicator (10701) is included.
    .ind_muac_children = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10701"))
    },
    #' @field .ind_muac_women Logical flag indicating if MUAC women indicator (10702) is included.
    .ind_muac_women = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10702"))
    },
    #' @field .ind_vitamin_a_coverage Logical flag indicating if vitamin A coverage indicator (14305) is included.
    .ind_vitamin_a_coverage = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("14305"))
    },
    #' @field .ind_mortality Logical flag indicating if mortality indicators (10501, 10502) are included.
    .ind_mortality = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10501", "10502"))
    },
    #' @field .ind_fcs Logical flag indicating if FCS indicator (11205) is included.
    .ind_fcs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("11205"))
    },
    #' @field .ind_rcsi Logical flag indicating if rCSI indicator (11202) is included.
    .ind_rcsi = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("11202"))
    },
    #' @field .ind_hhs Logical flag indicating if HHS indicator (11201) is included.
    .ind_hhs = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("11201"))
    },
    #' @field .ind_lcsi Logical flag indicating if LCSI indicator (12301) is included.
    .ind_lcsi = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("12301"))
    },
    #' @field .ind_hwise Logical flag indicating if HWISE indicator (11701) is included.
    .ind_hwise = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("11701"))
    },
    #' @field .ind_lppd Logical flag indicating if LPPD indicator (10901) is included.
    .ind_lppd = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..household_has_any_indicator(c("10901"))
    },

    #' @field .tool_community_observation Logical flag indicating if a community observation tool is registered.
    .tool_community_observation = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_community")
    },
    #' @field .tool_crops_livestock_observation Logical flag indicating if a crops/livestock observation tool is registered.
    .tool_crops_livestock_observation = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_crop_livestock")
    },
    #' @field .tool_health_facility_observation Logical flag indicating if a health facility observation tool is registered.
    .tool_health_facility_observation = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_health_facility")
    },
    #' @field .tool_latrine_observation Logical flag indicating if a latrine observation tool is registered.
    .tool_latrine_observation = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_latrine")
    },
    #' @field .tool_water_point_observation Logical flag indicating if a water point observation tool is registered.
    .tool_water_point_observation = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      private$..has_tool_role("obs_water_point")
    },

    #' @field .tools_table_df Data frame summarizing registered tools with sampling methods and sample sizes.
    .tools_table_df = function(value) {
      st <- private$..sample_table_from_nested()

      tool_names <- self$get_tool_names()

      table_df <- data.frame(
          Tool = character(0),
          `Sampling Method` = character(0),
          `Sample Size` = numeric(0),
          check.names = FALSE
        )

      if ("tool_household_iphra_v2" %in% tool_names) {
        table_df <- data.frame(
          Tool = "tool_household_iphra_v2",

          `Sampling Method` = if (
            !is.null(st) &&
              all(
                c("sampling_method_site", "sampling_method_hh") %in% names(st)
              )
          ) {
            sampling_methods <- paste(
              st$sampling_method_site,
              st$sampling_method_hh,
              sep = " - "
            )

            sampling_methods <- unique(stats::na.omit(sampling_methods))

            if (length(sampling_methods) > 0) {
              paste(sampling_methods, collapse = ", ")
            } else {
              NA_character_
            }
          } else {
            NA_character_
          },

          `Sample Size` = if (
            !is.null(st) &&
              "Final_HH_Sample_Size" %in% names(st)
          ) {
            sum(st$Final_HH_Sample_Size, na.rm = TRUE)
          } else {
            NA_real_
          },

          check.names = FALSE
        )
      }

      if ("tool_kii_community_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_kii_community_iphra_v2",
            `Sampling Method` = "Purposive / Random Walk",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              3 * sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_kii_fsl_service_provider_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_kii_fsl_service_provider_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_kii_health_service_provider_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_kii_health_service_provider_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_kii_nutrition_service_provider_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_kii_nutrition_service_provider_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_kii_wash_service_provider_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_kii_wash_service_provider_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_obs_community_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_obs_community_iphra_v2",
            `Sampling Method` = "Transect Walk",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_obs_crop_livestock_iphra_v1" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_obs_crop_livestock_iphra_v1",
            `Sampling Method` = "Transect Walk",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_obs_health_facility_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_obs_health_facility_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_obs_latrine_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_obs_latrine_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if ("tool_obs_water_point_iphra_v2" %in% tool_names) {
        table_df <- rbind(
          table_df,
          data.frame(
            Tool = "tool_obs_water_point_iphra_v2",
            `Sampling Method` = "Purposive",
            `Sample Size` = if (
              !is.null(st) &&
                "n_sites" %in% names(st)
            ) {
              sum(st$n_sites, na.rm = TRUE)
            } else {
              NA_real_
            },
            check.names = FALSE
          )
        )
      }

      if(nrow(table_df) == 0) {
        table_df <- data.frame(
          Tool = NA_character_,
          `Sampling Method` = NA_character_,
          `Sample Size` = NA_real_,
          check.names = FALSE
        )
      }

      return(table_df)
    },

    #' @field .household_pillars_table_df Active binding.
    .household_pillars_table_df = function(value) {
      if (!private$..has_tool_role("household")) {
        table <- data.frame(
          Pillar = NA_character_,
          `Sub-Pillar` = NA_character_,
          Indicator = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
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

      ob <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_objectives_schema"
        ),
        error = function(e) NULL
      )

      if (!is.null(ob)) {
        ob <- unique(ob[,
          c("objective_code", "pillar", "sub_pillar"),
          drop = FALSE
        ])
      }

      ib <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_indicator_bank"
        ),
        error = function(e) NULL
      )

      if (!is.null(ib)) {
        ib <- unique(ib[,
          c("objective_code", "indicator_code", "indicator_name"),
          drop = FALSE
        ])
      }

      if (is.null(ob) || is.null(ib)) {
        table <- data.frame(
          Pillar = NA_character_,
          `Sub-Pillar` = NA_character_,
          Indicator = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      # Filter indicator bank to indicators used in household survey
      ib_sub <- ib[ib$indicator_code %in% hh_codes, , drop = FALSE]

      # Get objective codes linked to those indicators
      objective_codes <- unique(ib_sub$objective_code)
      objective_codes <- objective_codes[
        !is.na(objective_codes) & nzchar(objective_codes)
      ]

      # Filter objectives schema to relevant objectives
      ob_sub <- ob[ob$objective_code %in% objective_codes, , drop = FALSE]

      # Join pillar / sub-pillar info onto indicator bank
      table <- merge(
        ib_sub,
        ob_sub[, c("objective_code", "pillar", "sub_pillar"), drop = FALSE],
        by = "objective_code",
        all.x = TRUE
      )

      # Keep final output columns
      table <- table[,
        c("pillar", "sub_pillar", "indicator_name"),
        drop = FALSE
      ]

      names(table) <- c("Pillar", "Sub-Pillar", "Indicator")

      table
    },
    #' @field .kii_pillars_table_df Active binding.
    .kii_pillars_table_df = function(value) {
      if (
        !private$..has_tool_role("kii_community") &&
          !private$..has_tool_role("kii_fsl_service_provider") &&
          !private$..has_tool_role("kii_health_service_provider") &&
          !private$..has_tool_role("kii_nutrition_service_provider") &&
          !private$..has_tool_role("kii_wash_service_provider") &&
          !private$..has_tool_role("kii_markets")
      ) {
        table <- data.frame(
          Tool = character(0),
          Pillar = character(0),
          `Sub-Pillar` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      ob <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_objectives_schema"
        ),
        error = function(e) NULL
      )

      ib <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_indicator_bank"
        ),
        error = function(e) NULL
      )

      if (is.null(ob) || is.null(ib)) {
        table <- data.frame(
          Tool = character(0),
          Pillar = character(0),
          `Sub-Pillar` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      table <- data.frame(
        Tool = character(0),
        Pillar = character(0),
        `Sub-Pillar` = character(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      if (private$..has_tool_role("kii_community")) {
        table <- private$..build_kii_obs_tool_table(
          role = "kii_community",
          tool_label = "Community Leader and Community Member KII",
          ib = ib,
          ob = ob
        )
      }

      if (private$..has_tool_role("kii_health_service_provider")) {
        health_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_health_service_provider",
          tool_label = "Health Facility KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          health_kii_table
        )
      }

      if (private$..has_tool_role("kii_nutrition_service_provider")) {
        nutrition_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_nutrition_service_provider",
          tool_label = "Nutrition Facility KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          nutrition_kii_table
        )
      }

      if (private$..has_tool_role("kii_markets")) {
        market_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_markets",
          tool_label = "Market Vendor KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          market_kii_table
        )
      }

      if (private$..has_tool_role("kii_wash_service_provider")) {
        wash_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_wash_service_provider",
          tool_label = "WASH Provider KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          wash_kii_table
        )
      }

      if (private$..has_tool_role("kii_wash_service_provider")) {
        wash_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_wash_service_provider",
          tool_label = "WASH Provider KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          wash_kii_table
        )
      }

      if (private$..has_tool_role("kii_fsl_service_provider")) {
        fsl_kii_table <- private$..build_kii_obs_tool_table(
          role = "kii_fsl_service_provider",
          tool_label = "FSL Assistance Provider KII",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          fsl_kii_table
        )
      }

      names(table) <- c("Tool", "Pillar", "Sub-Pillar")

      table
    },
    #' @field .observation_pillars_table_df Active binding.
    .observation_pillars_table_df = function(value) {
      if (
        !private$..has_tool_role("obs_community") &&
          !private$..has_tool_role("obs_health_facility") &&
          !private$..has_tool_role("obs_crop_livestock") &&
          !private$..has_tool_role("obs_latrine") &&
          !private$..has_tool_role("obs_water_point")
      ) {
        table <- data.frame(
          Tool = character(0),
          Pillar = character(0),
          `Sub-Pillar` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      ob <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_objectives_schema"
        ),
        error = function(e) NULL
      )

      ib <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_indicator_bank"
        ),
        error = function(e) NULL
      )

      if (is.null(ob) || is.null(ib)) {
        table <- data.frame(
          Tool = character(0),
          Pillar = character(0),
          `Sub-Pillar` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      table <- data.frame(
        Tool = character(0),
        Pillar = character(0),
        `Sub-Pillar` = character(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      if (private$..has_tool_role("obs_community")) {
        table <- private$..build_kii_obs_tool_table(
          role = "obs_community",
          tool_label = "Community Observation Tool",
          ib = ib,
          ob = ob
        )
      }

      if (
        private$..has_tool_role(
          "kii_health_sobs_health_facilityervice_provider"
        )
      ) {
        health_obs_table <- private$..build_kii_obs_tool_table(
          role = "obs_health_facility",
          tool_label = "Health Facility Observation Tool",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          health_obs_table
        )
      }

      if (private$..has_tool_role("obs_crop_livestock")) {
        crop_livestock_obs_table <- private$..build_kii_obs_tool_table(
          role = "obs_crop_livestock",
          tool_label = "Crops and Livestock Observation Tool",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          crop_livestock_obs_table
        )
      }

      if (private$..has_tool_role("obs_latrine")) {
        latrine_obs_table <- private$..build_kii_obs_tool_table(
          role = "obs_latrine",
          tool_label = "Latrine Observation Tool",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          latrine_obs_table
        )
      }

      if (private$..has_tool_role("obs_water_point")) {
        wash_obs_table <- private$..build_kii_obs_tool_table(
          role = "obs_water_point",
          tool_label = "Water Point Observation Tool",
          ib = ib,
          ob = ob
        )

        table <- rbind(
          table,
          wash_obs_table
        )
      }

      names(table) <- c("Tool", "Pillar", "Sub-Pillar")

      table
    },
    #' @field .household_dap_df Data frame containing the household data analysis plan.
    .household_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("hh_dap_table is read-only"),
          origin = "IPHRAProtocol$active$hh_dap_table"
        )
      }

      if (self$.tool_household_iphra) {
        self$get_dap_table("tool_household_iphra_v2")
      } else {
        data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    },
    #' @field .community_kii_dap_df Data frame containing the community KII data analysis plan.
    .community_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("community_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$community_kii_dap_table"
        )
      }
      if (self$.tool_community_kii) {
        self$get_dap_table("tool_kii_community_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .community_observation_dap_df Data frame containing the community observation data analysis plan.
    .community_observation_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("community_obs_dap_table is read-only"),
          origin = "IPHRAProtocol$active$community_obs_dap_table"
        )
      }
      if (self$.tool_community_observation) {
        self$get_dap_table("tool_obs_community_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .health_facility_kii_dap_df Data frame containing the health facility KII data analysis plan.
    .health_facility_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("health_facility_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$health_facility_kii_dap_table"
        )
      }
      if (self$.tool_health_facility_kii) {
        self$get_dap_table("tool_kii_health_service_provider_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .health_facility_observation_dap_df Data frame containing the health facility observation data analysis plan.
    .health_facility_observation_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt(
            "health_facility_observation_dap_table is read-only"
          ),
          origin = "IPHRAProtocol$active$health_facility_observation_dap_table"
        )
      }
      if (self$.tool_health_facility_observation) {
        self$get_dap_table("tool_obs_health_facility_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .nutrition_facility_kii_dap_df Data frame containing the nutrition facility KII data analysis plan.
    .nutrition_facility_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("nutrition_facility_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$nutrition_facility_kii_dap_table"
        )
      }
      if (self$.tool_nutrition_facility_kii) {
        self$get_dap_table("tool_kii_nutrition_service_provider_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .fsl_provider_kii_dap_df Data frame containing the FSL provider KII data analysis plan.
    .fsl_provider_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("fsl_provider_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$fsl_provider_kii_dap_table"
        )
      }
      if (self$.tool_fsl_provider_kii) {
        self$get_dap_table("tool_kii_fsl_service_provider_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .market_kii_dap_df Data frame containing the market KII data analysis plan.
    .market_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("market_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$market_kii_dap_table"
        )
      }
      if (self$.tool_market_kii) {
        self$get_dap_table("tool_kii_markets_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .crop_livstock_observation_dap_df Active binding.
    .crop_livstock_observation_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt(
            "crop_livestock_observation_dap_table is read-only"
          ),
          origin = "IPHRAProtocol$active$crop_livestock_observation_dap_table"
        )
      }
      if (self$.tool_crops_livestock_observation) {
        self$get_dap_table("tool_obs_crop_livestock_iphra_v1")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .wash_provider_kii_dap_df Data frame containing the WASH provider KII data analysis plan.
    .wash_provider_kii_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("wash_provider_kii_dap_table is read-only"),
          origin = "IPHRAProtocol$active$wash_provider_kii_dap_table"
        )
      }
      if (self$.tool_wash_provider_kii) {
        self$get_dap_table("tool_kii_wash_service_provider_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .water_point_observation_dap_df Data frame containing the water point observation data analysis plan.
    .water_point_observation_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("water_point_observation_dap_table is read-only"),
          origin = "IPHRAProtocol$active$water_point_observation_dap_table"
        )
      }
      if (self$.tool_water_point_observation) {
        self$get_dap_table("tool_obs_water_point_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    },
    #' @field .latrine_observation_dap_df Data frame containing the latrine observation data analysis plan.
    .latrine_observation_dap_df = function(value) {
      if (!missing(value)) {
        phr_abort(
          message = phr_txt("latrine_observation_dap_table is read-only"),
          origin = "IPHRAProtocol$active$latrine_observation_dap_table"
        )
      }
      if (self$.tool_latrine_observation) {
        self$get_dap_table("tool_obs_latrine_iphra_v2")
      } else {
        return(data.frame(
          "Research Question" = NA_character_,
          "Indicator / Variable" = NA_character_,
          "Disaggregation" = NA_character_,
          "Questionnaire Question" = NA_character_,
          "Questionnaire Responses" = NA_character_,
          "Data Collection Level" = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    }
  ),

  private = list(
    # Named list: tool_name -> list(class = <class_name>, file = <xlsx_filename>)
    ..iphra_tools = list(
      tool_household_iphra_v2 = list(
        class = "HouseholdTool",
        file = "tool_household_iphra_v2.xlsx"
      ),
      tool_kii_community_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_community_iphra_v2.xlsx"
      ),
      tool_kii_fsl_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_fsl_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_wash_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_wash_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_markets_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_markets_iphra_v2.xlsx"
      ),
      tool_kii_nutrition_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_nutrition_service_provider_iphra_v2.xlsx"
      ),
      tool_kii_health_service_provider_iphra_v2 = list(
        class = "KeyInformantTool",
        file = "tool_kii_health_service_provider_iphra_v2.xlsx"
      ),
      tool_obs_community_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_community_iphra_v2.xlsx"
      ),
      tool_obs_crop_livestock_iphra_v1 = list(
        class = "ObservationTool",
        file = "tool_obs_crop_livestock_iphra_v1.xlsx"
      ),
      tool_obs_health_facility_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_health_facility_iphra_v2.xlsx"
      ),
      tool_obs_latrine_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_latrine_iphra_v2.xlsx"
      ),
      tool_obs_water_point_iphra_v2 = list(
        class = "ObservationTool",
        file = "tool_obs_water_point_iphra_v2.xlsx"
      )
    ),

    ..build_kii_obs_tool_table = function(role, tool_label, ib, ob) {
      indicator_codes <- tryCatch(
        self$access_nested(
          field = "tools",
          role = role,
          member = "get_indicator_codes"
        ),
        error = function(e) character(0)
      )

      indicator_codes <- trimws(as.character(indicator_codes))
      indicator_codes <- indicator_codes[
        !is.na(indicator_codes) & nzchar(indicator_codes)
      ]

      ib_sub <- ib[ib$indicator_code %in% indicator_codes, , drop = FALSE]

      objective_codes <- unique(ib_sub$objective_code)
      objective_codes <- objective_codes[
        !is.na(objective_codes) & nzchar(objective_codes)
      ]

      ob_sub <- ob[ob$objective_code %in% objective_codes, , drop = FALSE]

      table <- merge(
        ib_sub,
        ob_sub[, c("objective_code", "pillar", "sub_pillar"), drop = FALSE],
        by = "objective_code",
        all.x = TRUE
      )

      unique(
        data.frame(
          Tool = tool_label,
          Pillar = table$pillar,
          `Sub-Pillar` = table$sub_pillar,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      )
    },

    # ── TOR generation private methods ─────────────────────────────────────

    ..default_template_filenames = function() {
      c(
        "reach_tor_iphra_template.docx",
        "reach_tor_template.docx",
        "protocol_report_template.docx"
      )
    },
    ..default_word_template_path = function() {
      template_file <- "quarto_doc_revised_template.qmd"

      template_path <- system.file(
        "resources",
        template_file,
        package = "phr"
      )

      if (!nzchar(template_path) || !file.exists(template_path)) {
        template_path <- file.path(
          "inst",
          "resources",
          template_file
        )
      }

      template_path
    }
  )
)
