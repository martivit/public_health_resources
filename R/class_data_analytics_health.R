#' IPHRA Health Data Analytics Class
#'
#' The `HealthDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Health-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Health-specific:
#' * Quality checks (facility access, utilization, insurance, maternal health) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @field linked_ind_roster_data Optional linked roster dataframe
#' @field linked_ind_roster_data_stage_name Name of linked roster data stage
#' @field linked_ind_roster_data_hash Hash of linked roster data
#' @field linked_ind_roster_variable_map Variable mappings for linked roster data
#' @field linked_ind_roster_value_map Value mappings for linked roster data
#' @field linked_ind_health_data Optional linked health individual dataframe
#' @field linked_ind_health_data_stage_name Name of linked health individual data stage
#' @field linked_ind_health_data_hash Hash of linked health individual data
#' @field linked_ind_health_variable_map Variable mappings for linked health individual data
#' @field linked_ind_health_value_map Value mappings for linked health individual data
#'
#' @seealso [DataAnalytics]
#' @export
HealthDataAnalytics <- R6::R6Class(
  classname = "HealthDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Fields for linked roster data
    linked_ind_roster_data = NULL,
    linked_ind_roster_data_stage_name = NULL,
    linked_ind_roster_data_hash = NULL,
    linked_ind_roster_variable_map = NULL,
    linked_ind_roster_value_map = NULL,

    # Fields for linked health individual data
    linked_ind_health_data = NULL,
    linked_ind_health_data_stage_name = NULL,
    linked_ind_health_data_hash = NULL,
    linked_ind_health_variable_map = NULL,
    linked_ind_health_value_map = NULL,

    #' @description
    #' Initialize a new HealthDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean health data)
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
    #' @param linked_ind_roster_data Optional linked roster dataframe
    #' @param linked_ind_roster_data_stage_name Name of linked roster data stage
    #' @param linked_ind_roster_data_hash Hash of linked roster data
    #' @param linked_ind_roster_variable_map Variable mappings for linked roster data
    #' @param linked_ind_roster_value_map Value mappings for linked roster data
    #' @param linked_ind_health_data Optional linked health individual dataframe
    #' @param linked_ind_health_data_stage_name Name of linked health data stage
    #' @param linked_ind_health_data_hash Hash of linked health data
    #' @param linked_ind_health_variable_map Variable mappings for linked health data
    #' @param linked_ind_health_value_map Value mappings for linked health data
    #' @return A new HealthDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "HealthDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_ind_roster_data = NULL,
                          linked_ind_roster_data_stage_name = NULL,
                          linked_ind_roster_data_hash = NULL,
                          linked_ind_roster_variable_map = NULL,
                          linked_ind_roster_value_map = NULL,
                          linked_ind_health_data = NULL,
                          linked_ind_health_data_stage_name = NULL,
                          linked_ind_health_data_hash = NULL,
                          linked_ind_health_variable_map = NULL,
                          linked_ind_health_value_map = NULL) {

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

      self$linked_ind_roster_data              <- linked_ind_roster_data
      self$linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      self$linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      self$linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      self$linked_ind_roster_value_map         <- linked_ind_roster_value_map

      self$linked_ind_health_data              <- linked_ind_health_data
      self$linked_ind_health_data_stage_name   <- linked_ind_health_data_stage_name
      self$linked_ind_health_data_hash         <- linked_ind_health_data_hash
      self$linked_ind_health_variable_map      <- linked_ind_health_variable_map
      self$linked_ind_health_value_map         <- linked_ind_health_value_map

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_health_data))  msg_parts <- c(msg_parts, "health individual")
      if (length(msg_parts) > 0) {
        phrutils::phr_message(
          phr_txt(glue::glue(
            "HealthDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data."
          ))
        )
      } else {
        phrutils::phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as HealthDataAnalytics object."))
        )
      }
    },

    #' @description Load the default Health quality schema from template file
    #' @return A list of Health-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_health_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_health_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "HealthDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_health_template.xlsx: {e$message}"))
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

    #' @description Load the default Health unified outputs schema from template file
    #' @return A list of Health-specific outputs definitions (quality and analysis combined)
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_health_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_health_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "HealthDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_health_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Health analysis schema from template file
    #' @return A tibble containing the Health analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_health_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_health_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "HealthDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_health_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    }
  )
)
