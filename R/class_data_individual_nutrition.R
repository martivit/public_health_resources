#' IPHRA Nutrition Individual Data Class
#'
#' Extends IndividualData by adding a comprehensive schema for
#' individual-level anthropometric and nutrition assessment data including:
#' MUAC measurements, weight/height, z-score calculations,
#' malnutrition classification, and infant feeding indicators.
#'
#' This class incorporates MUAC-for-age z-score calculations and
#' provides data quality checks and visualization capabilities.
#'
#' @export
NutritionIndividualData <- R6::R6Class(
  classname = "NutritionIndividualData",
  inherit   = IndividualData,

  public = list(

    #' @description
    #' Initialize a new NutritionIndividualData object
    #'
    #' @param data A data frame containing individual nutrition/anthropometry records
    #' @param dataset_name A name for this dataset
    #' @param metadata Optional list of metadata
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @return A new NutritionIndividualData object
    initialize = function(data,
                          dataset_name = "NutritionIndividualData",
                          metadata = NULL,
                          variable_map = NULL) {

      phrutils::phr_try({

        nutrition_map <- list(
          # Demographics (use generic names that map to data columns)
          uuid      = "person_id",
          hh_uuid    = "hh_uuid"
        )

        variable_map <- modifyList(nutrition_map, variable_map %||% list())

        # Call parent
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          metadata     = metadata,
          variable_map = variable_map
        )

        # Extend optional columns
        self$optional_columns <- c(self$optional_columns, unlist(nutrition_map))

        # Load and merge schema
        self$set_variable_schema(self$default_nutrition_schema())

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
          phr_txt("{dataset_name} initialized as NutritionIndividualData object.")
        )

      }, on_error = "abort", origin = "NutritionIndividualData$initialize")
    },


    #' @description
    #' Load the default variable schema for nutrition data
    #'
    #' Reads the variable_schema_data_individual_nutrition_template.xlsx file from package resources
    #' and converts it to a nested list of variable definitions for anthropometry and nutrition.
    #'
    #' @return A list containing the nutrition data variable schema
    default_nutrition_schema = function() {

      file <- system.file(
        "resources",
        "variable_schema_data_individual_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phr_error(
          origin  = "NutritionIndividualData$default_nutrition_schema",
          message = phr_txt("variable_schema_data_individual_nutrition_template.xlsx not found in package resources."),
          hint    = phr_txt("Place the schema file under inst/resources/ before building the package.")
        )
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_error(
            origin  = "NutritionIndividualData$default_nutrition_schema",
            message = phr_txt("Failed to read variable_schema_data_individual_nutrition_template.xlsx"),
            hint    = e$message
          )
        }
      )

      # Convert table → canonical nested schema list
      schema <- data_table_to_schema(df)

      return(schema)
    },

    #' @description
    #' Load the default indicator schema for nutrition data
    #'
    #' Reads the indicator_schema_data_individual_nutrition_template.xlsx file from package resources
    #' and converts it to a nested list of indicator definitions (e.g., malnutrition prevalence).
    #'
    #' @return A list containing the nutrition data indicator schema
    default_indicator_schema = function() {

      file <- system.file(
        "resources",
        "indicator_schema_data_individual_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "NutritionIndividualData$default_indicator_schema",
          message = phr_txt("indicator_schema_data_individual_nutrition_template.xlsx not found in package resources. Continuing without default indicator schema.")
        )
        return(list())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "NutritionIndividualData$default_indicator_schema",
            message = phr_txt("Failed to read indicator_schema_data_individual_nutrition_template.xlsx: {e$message}")
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
    #' Load the default dependency schema for nutrition data
    #'
    #' Reads the dependency_schema_data_individual_nutrition_template.xlsx file from package resources
    #' and converts it to a nested list of variable dependency rules.
    #'
    #' @return A list containing the nutrition data dependency schema
    default_dependency_schema = function() {

      file <- system.file(
        "resources",
        "dependency_schema_data_individual_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "NutritionIndividualData$default_dependency_schema",
          message = phr_txt("dependency_schema_data_individual_nutrition_template.xlsx not found in package resources. Continuing without default dependency schema.")
        )
        return(list(dependencies = list()))
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "NutritionIndividualData$default_dependency_schema",
            message = phr_txt("Failed to read dependency_schema_data_individual_nutrition_template.xlsx: {e$message}")
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
    #' Generate a NutritionDataAnalytics or IYCFDataAnalytics object.
    #' Unifies quality checks and quantitative analysis in one object.
    #' For anthropometric data, use type = "nutrition" as NutritionDataAnalytics
    #' covers anthropometric quality checks and analysis.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param type The type of analytics to create ("nutrition" or "iycf")
    #' @param analysis_config Optional data analysis plan (tibble)
    #' @return A DataAnalytics subclass object or NULL
    generate_data_analytics = function(stage = c("standardized", "clean"),
                                       type  = c("nutrition", "iycf"),
                                       analysis_config = NULL) {

      stage <- match.arg(stage)
      type  <- match.arg(type)

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

        analytics <- switch(
          type,
          "nutrition" = NutritionDataAnalytics$new(
            data               = df,
            dap                = analysis_config,
            parent_data_object = self,
            dataset_name       = paste0(self$dataset_name, "_NutritionDataAnalytics"),
            data_stage_name    = stage,
            data_hash          = data_hash,
            variable_map       = variable_map,
            value_map          = value_map,
            variable_label     = self$variable_label,
            value_label        = self$value_label
          ),
          "anthropometric" = NutritionDataAnalytics$new(
            data               = df,
            dap                = analysis_config,
            parent_data_object = self,
            dataset_name       = paste0(self$dataset_name, "_NutritionDataAnalytics"),
            data_stage_name    = stage,
            data_hash          = data_hash,
            variable_map       = variable_map,
            value_map          = value_map,
            variable_label     = self$variable_label,
            value_label        = self$value_label
          ),
          "iycf" = IYCFDataAnalytics$new(
            data               = df,
            dap                = analysis_config,
            parent_data_object = self,
            dataset_name       = paste0(self$dataset_name, "_IYCFDataAnalytics"),
            data_stage_name    = stage,
            data_hash          = data_hash,
            variable_map       = variable_map,
            value_map          = value_map,
            variable_label     = self$variable_label,
            value_label        = self$value_label
          ),
          phr_error(
            origin  = paste0(self$dataset_name, "$generate_data_analytics"),
            message = phr_txt("Unknown analytics type '{type}' for NutritionIndividualData. Valid types: nutrition, iycf")
          )
        )

        phrutils::phr_message(
          phr_txt("Generated {type} DataAnalytics object for {self$dataset_name}.")
        )

        return(analytics)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_analytics"))
    }


  )
)
