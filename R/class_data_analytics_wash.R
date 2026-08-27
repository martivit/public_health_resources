#' IPHRA WASH Data Analytics Class
#'
#' The `WASHDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Water, Sanitation and Hygiene specific quality checks and quantitative
#' analysis in a single integrated object.
#'
#' @description
#' This class provides WASH-specific:
#' * Quality checks (water source, JMP ladder, sanitation, handwashing) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @field linked_containers_data Optional linked WaterContainerData dataframe
#' @field linked_containers_data_stage_name Name of linked containers data stage
#' @field linked_containers_data_hash Hash of linked containers data
#' @field linked_containers_variable_map Variable mappings for linked containers data
#' @field linked_containers_value_map Value mappings for linked containers data
#'
#' @seealso [DataAnalytics]
#' @export
WASHDataAnalytics <- R6::R6Class(
  classname = "WASHDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Fields for linked water container data
    linked_containers_data = NULL,
    linked_containers_data_stage_name = NULL,
    linked_containers_data_hash = NULL,
    linked_containers_variable_map = NULL,
    linked_containers_value_map = NULL,

    #' @description
    #' Initialize a new WASHDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean WASH data)
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
    #' @param linked_containers_data Optional linked WaterContainerData dataframe
    #' @param linked_containers_data_stage_name Name of linked containers data stage
    #' @param linked_containers_data_hash Hash of linked containers data
    #' @param linked_containers_variable_map Variable mappings for linked containers data
    #' @param linked_containers_value_map Value mappings for linked containers data
    #' @return A new WASHDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WASHDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_containers_data = NULL,
                          linked_containers_data_stage_name = NULL,
                          linked_containers_data_hash = NULL,
                          linked_containers_variable_map = NULL,
                          linked_containers_value_map = NULL) {

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

      self$linked_containers_data              <- linked_containers_data
      self$linked_containers_data_stage_name   <- linked_containers_data_stage_name
      self$linked_containers_data_hash         <- linked_containers_data_hash
      self$linked_containers_variable_map      <- linked_containers_variable_map
      self$linked_containers_value_map         <- linked_containers_value_map

      if (!is.null(linked_containers_data)) {
        phrutils::phr_message(
          phr_txt("WASHDataAnalytics initialized with linked water container data.")
        )
      } else {
        phrutils::phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as WASHDataAnalytics object."))
        )
      }
    },

    #' @description Load the default WASH quality schema from template file
    #' @return A list of WASH-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_wash_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_wash_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "WASHDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_wash_template.xlsx: {e$message}"))
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

    #' @description Load the default WASH unified outputs schema from template file
    #' @return A list of WASH-specific outputs definitions (quality and analysis combined)
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_wash_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_wash_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "WASHDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_wash_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default WASH analysis schema from template file
    #' @return A tibble containing the WASH analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_wash_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_wash_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "WASHDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_wash_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    }
  )
)
