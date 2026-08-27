#' IPHRA Demographics Data Analytics Class
#'
#' The `DemographicsDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Demographics-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Demographics-specific:
#' * Quality checks (age distribution, sex ratio, household size, dependency ratio) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @seealso [DataAnalytics]
#' @export
DemographicsDataAnalytics <- R6::R6Class(
  classname = "DemographicsDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new DemographicsDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean demographics data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @return A new DemographicsDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "DemographicsDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

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
        value_label = value_label,
        quality_schema = quality_schema
      )

      phrutils::phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as DemographicsDataAnalytics object."))
      )
    },

    #' @description Load the default Demographics quality schema from template file
    #' @return A list of Demographics-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_demographics_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_demographics_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "DemographicsDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_demographics_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Load the default Demographics unified outputs schema from template file
    #' @return A list of Demographics-specific outputs definitions (quality and analysis combined)
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_demographics_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_demographics_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "DemographicsDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_demographics_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Demographics analysis schema from template file
    #' @return A tibble containing the Demographics analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_demographics_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_demographics_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "DemographicsDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_demographics_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    }
  )
)
