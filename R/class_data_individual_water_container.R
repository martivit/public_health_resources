#' IPHRA Water Container Data Class
#'
#' The `WaterContainerData` R6 class extends the base [Data] class to represent
#' water container assessment data. Each record corresponds to one water container
#' within a household.
#'
#' @description
#' This subclass enforces domain-specific validation and schema rules for
#' water container data, including:
#' * Required fields for `uuid` (unique container ID), `hh_uuid` (household linkage)
#' * Container characteristics, storage practices, water source
#' * Water quality testing results
#' * Risk assessment calculations
#'
#' @field household_link Optional linkage back to a HouseholdData object
#' @field optional_columns Character vector of optional columns for water container data
#'
#' @seealso [Data], [HouseholdData]
#' @export
WaterContainerData <- R6::R6Class(
  classname = "WaterContainerData",
  inherit   = Data,

  public = list(


    # Additional fields

    household_link   = NULL,  # linkage back to HouseholdData (optional)
    optional_columns = NULL,


    #' @description
    #' Initialize a new WaterContainerData object
    #'
    #' @param data A data frame containing water container assessment records
    #' @param dataset_name A name for this dataset
    #' @param metadata Optional list of metadata
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @return A new WaterContainerData object
    initialize = function(data,
                          dataset_name = "WaterContainerData",
                          metadata = NULL,
                          variable_map = NULL) {

      phrutils::phr_try({

        # Default mapping for water container data
        default_map <- list(
          # Identifiers
          uuid = "container_id",           # unique container identifier
          hh_uuid = "hh_uuid"     # household linkage

        )

        # Merge user-specified map over defaults
        variable_map <- modifyList(default_map, variable_map %||% list())

        # Call parent initializer
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          metadata     = metadata,
          uuid         = variable_map$uuid,
          variable_map = variable_map
        )

        # Required columns (only uuid is required, rest are optional)
        # Note: we intentionally do not define required columns here
        # to maintain flexibility - users can define them via schema

        # Optional columns
        self$optional_columns <- c(
          variable_map$hh_uuid,
          variable_map$container_number,
          variable_map$container_type,
          variable_map$container_journey_count,
          variable_map$container_capacity_liters
        )

        # Build and merge container schema into any existing schema
        container_schema <- self$default_schema()
        parent_schema    <- self$variable_schema %||% list()
        merged_schema    <- utils::modifyList(parent_schema, container_schema)

        self$set_variable_schema(merged_schema)

        # Load default indicator schema
        default_ind_schema <- self$default_indicator_schema()
        if (length(default_ind_schema) > 0) {
          self$set_indicator_schema(default_ind_schema)
          phrutils::phr_message(
            phr_txt("Loaded default indicator schema with {length(default_ind_schema)} indicator(s).")
          )
        }

        # Load default dependency schema
        default_dep_schema <- self$default_dependency_schema()
        if (length(default_dep_schema$dependencies) > 0) {
          self$set_dependency_schema(default_dep_schema)
          phrutils::phr_message(
            phr_txt("Loaded default dependency schema with {length(default_dep_schema$dependencies)} dependency/ies.")
          )
        }

        phrutils::phr_message(
          phr_txt("{dataset_name} initialized as WaterContainerData object.")
        )

      }, on_error = "abort", origin = "WaterContainerData$initialize")
    },


    #' @description
    #' Load the default variable schema for water container data
    #'
    #' Reads the variable_schema_data_water_container_template.xlsx file from package resources
    #' and converts it to a nested list of variable definitions for container characteristics.
    #'
    #' @return A list containing the water container variable schema
    default_schema = function() {

      file <- system.file(
        "resources",
        "variable_schema_data_water_container_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phr_error(
          origin  = "WaterContainerData$default_schema",
          message = phr_txt("variable_schema_data_water_container_template.xlsx not found in package resources."),
          hint    = phr_txt("Place the schema file under inst/resources/ before building the package.")
        )
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_error(
            origin  = "WaterContainerData$default_schema",
            message = phr_txt("Failed to read variable_schema_data_water_container_template.xlsx"),
            hint    = e$message
          )
        }
      )

      # Convert table → canonical nested schema list
      schema <- data_table_to_schema(df)

      return(schema)
    },

    #' @description
    #' Load the default indicator schema for water container data
    #'
    #' Reads the indicator_schema_data_water_container_template.xlsx file from package resources
    #' and converts it to a nested list of indicator definitions (e.g., water quality metrics).
    #'
    #' @return A list containing the water container indicator schema
    default_indicator_schema = function() {

      file <- system.file(
        "resources",
        "indicator_schema_data_water_container_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "WaterContainerData$default_indicator_schema",
          message = phr_txt("indicator_schema_data_water_container_template.xlsx not found in package resources. Continuing without default indicator schema.")
        )
        return(list())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "WaterContainerData$default_indicator_schema",
            message = phr_txt("Failed to read indicator_schema_data_water_container_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list())

      # Convert table → canonical nested indicator schema list
      indicator_schema <- indicator_table_to_schema(df)

      return(indicator_schema)
    },

    #' @description
    #' Load the default dependency schema for water container data
    #'
    #' Reads the dependency_schema_data_water_container_template.xlsx file from package resources
    #' and converts it to a nested list of variable dependency rules.
    #'
    #' @return A list containing the water container dependency schema
    default_dependency_schema = function() {

      file <- system.file(
        "resources",
        "dependency_schema_data_water_container_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "WaterContainerData$default_dependency_schema",
          message = phr_txt("dependency_schema_data_water_container_template.xlsx not found in package resources. Continuing without default dependency schema.")
        )
        return(list(dependencies = list()))
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "WaterContainerData$default_dependency_schema",
            message = phr_txt("Failed to read dependency_schema_data_water_container_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list(dependencies = list()))

      # Convert table → canonical nested dependency schema list
      dependency_schema <- dependency_table_to_schema(df)

      return(dependency_schema)
    },


    #' @description
    #' Perform container-specific post-validation checks
    #'
    #' Validates range values, categorical values, and logical consistency
    #' specific to water container data (e.g., E.coli results only when tested).
    #'
    #' @param df Optional data frame to validate (defaults to raw data)
    #' @return TRUE if validation passed without issues, FALSE if warnings were raised
    post_validate = function(df = NULL) {

      nm <- self$dataset_name

      # Track whether any issues occurred
      had_issues <- FALSE

      phrutils::phr_try({

        if (is.null(df)) df <- self$get_data("raw")

        vm <- self$variable_map
        schema <- self$default_schema()

        # Range checks
        for (v in names(schema$ranges)) {
          if (!v %in% names(vm)) next
          col <- vm[[v]]
          if (is.null(col) || !col %in% names(df)) next
          vals <- suppressWarnings(as.numeric(df[[col]]))
          rng <- schema$ranges[[v]]
          if (any(vals < rng[1], na.rm = TRUE) ||
              any(vals > rng[2], na.rm = TRUE)) {
            phrutils::phr_warning(
              nm,
              phr_txt(glue::glue("Variable '{col}' has values outside range [{rng[1]}, {rng[2]}]."))
            )
            had_issues <- TRUE
          }
        }

        # Categorical checks
        for (v in names(schema$allowed_values)) {
          col <- vm[[v]]
          if (is.null(col) || !col %in% names(df)) next
          allowed <- schema$allowed_values[[v]]
          bad <- setdiff(unique(df[[col]]), c(allowed, NA, "", NULL))
          if (length(bad) > 0) {
            phrutils::phr_warning(
              nm,
              phr_txt(glue::glue("Variable '{col}' contains invalid values: {paste(bad, collapse=', ')}."))
            )
            had_issues <- TRUE
          }
        }

        # Logical consistency: E.coli should only be present if tested
        tested_col <- vm[["container_water_quality_tested"]]
        ecoli_col <- vm[["container_water_quality_ecoli"]]

        if (!is.null(tested_col) && tested_col %in% names(df) &&
            !is.null(ecoli_col) && ecoli_col %in% names(df)) {

          not_tested <- tolower(df[[tested_col]]) == "no"
          has_result <- !is.na(df[[ecoli_col]])
          inconsistent <- not_tested & has_result

          if (any(inconsistent, na.rm = TRUE)) {
            phrutils::phr_warning(
              nm,
              phr_txt(glue::glue("Inconsistent data: not tested but has E.coli result for {sum(inconsistent)} records."))
            )
            had_issues <- TRUE
          }
        }

        phrutils::phr_message(
          phr_txt(glue::glue("Container-specific post-validation for {nm} complete."))
        )

      }, on_error = "warn", origin = "WaterContainerData$post_validate")

      # Return TRUE (good) or FALSE (warnings occurred)
      return(!had_issues)
    },


    #' @description
    #' Generate a WaterContainerDataAnalytics object for this water container dataset.
    #' Combines quality checks and quantitative analysis in one object.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param analysis_config Optional data analysis plan (tibble)
    #' @return A WaterContainerDataAnalytics object or NULL
    generate_data_analytics = function(stage = c("standardized", "clean"),
                                       analysis_config = NULL) {

      stage <- match.arg(stage)

      phrutils::phr_try({

        df <- self$get_data(stage)

        if (is.null(df)) {
          phrutils::phr_warning(
            self$dataset_name,
            phr_txt("No {stage} data available for DataAnalytics generation.")
          )
          return(NULL)
        }

        data_hash    <- self$get_hash(stage)
        variable_map <- self$variable_map
        value_map    <- self$value_map

        analytics <- WaterContainerDataAnalytics$new(
          data               = df,
          dap                = analysis_config,
          parent_data_object = self,
          dataset_name       = paste0(self$dataset_name, "_WaterContainerDataAnalytics"),
          data_stage_name    = stage,
          data_hash          = data_hash,
          variable_map       = variable_map,
          value_map          = value_map,
          variable_label     = self$variable_label,
          value_label        = self$value_label
        )

        phrutils::phr_message(
          phr_txt("Generated WaterContainerDataAnalytics object for {self$dataset_name}.")
        )

        return(analytics)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_analytics"))
    }

  )
)
