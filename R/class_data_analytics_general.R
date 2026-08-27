#' IPHRA General Data Analytics Class
#'
#' The `GeneralDataAnalytics` R6 class extends `DataAnalytics` for general
#' quantitative analysis covering indicators not specific to other sectoral
#' subclasses (FSL, WASH, Health, Mortality, Nutrition, Demographics).
#'
#' @description
#' This class provides:
#' * A general-purpose analysis schema (non-sectoral indicators) via analysis_schema
#' * All visualizations/tables via outputs_schema (inherits from base)
#' * An empty quality schema (no sector-specific quality checks)
#'
#' @seealso [DataAnalytics]
#' @export
GeneralDataAnalytics <- R6::R6Class(
  classname = "GeneralDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new GeneralDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @return A new GeneralDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "GeneralDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL) {

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = variable_map,
        value_map = value_map,
        variable_label = variable_label,
        value_label = value_label
      )

      phrutils::phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as GeneralDataAnalytics object."))
      )
    },

    #' @description Load the default General analysis schema from template file
    #' @return A tibble containing the General analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_general_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_general_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "GeneralDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_general_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default General unified outputs schema from template file
    #' @return A list of General-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_general_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_general_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "GeneralDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_general_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  )
)
