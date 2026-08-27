#' IPHRA IYCF Data Analytics Class
#'
#' The `IYCFDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Infant and Young Child Feeding specific quality checks in a unified
#' analytics object.
#'
#' @description
#' This class provides IYCF-specific:
#' * Quality checks (breastfeeding, dietary diversity, meal frequency) via quality_schema
#' * All visualizations/tables via outputs_schema
#' * A default empty analysis schema (no sector-specific quantitative template)
#'
#' @seealso [DataAnalytics]
#' @export
IYCFDataAnalytics <- R6::R6Class(
  classname = "IYCFDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new IYCFDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean IYCF data)
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
    #' @return A new IYCFDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "IYCFDataAnalytics",
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
        phr_txt(glue::glue("{dataset_name} initialized as IYCFDataAnalytics object."))
      )
    },

    #' @description Load the default IYCF quality schema from template file
    #' @return A list of IYCF-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_iycf_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_iycf_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "IYCFDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_iycf_template.xlsx: {e$message}"))
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

    #' @description Load the default IYCF unified outputs schema from template file
    #' @return A list of IYCF-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_iycf_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_iycf_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "IYCFDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_iycf_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  )
)
