#' IPHRA Water Container Data Analytics Class
#'
#' The `WaterContainerDataAnalytics` R6 class extends `DataAnalytics` to provide
#' water container-specific quality checks in a unified analytics object.
#'
#' @description
#' This class provides water container-specific:
#' * Quality checks (water quality, storage practices, E.coli results) via quality_schema
#' * All visualizations/tables via outputs_schema
#' * A default empty analysis schema (no sector-specific quantitative template)
#'
#' @seealso [DataAnalytics], [WaterContainerData]
#' @export
WaterContainerDataAnalytics <- R6::R6Class(
  classname = "WaterContainerDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new WaterContainerDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean water container data)
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
    #' @return A new WaterContainerDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WaterContainerDataAnalytics",
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
        phr_txt(glue::glue("{dataset_name} initialized as WaterContainerDataAnalytics object."))
      )
    },

    #' @description Load the default water container quality schema
    #' @return A list of water container-specific quality checks, or empty list
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_water_container_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_water_container_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "WaterContainerDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_water_container_template.xlsx: {e$message}"))
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

    #' @description Load the default water container unified outputs schema
    #' @return A list of water container-specific output definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_water_container_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_water_container_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "WaterContainerDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_water_container_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  )
)
