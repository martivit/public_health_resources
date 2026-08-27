#' IPHRA Individual Data Class
#'
#' The `IndividualData` R6 class extends the base [Data] class to represent
#' person-level survey data. Each record corresponds to one individual
#' enumerated within a household roster.
#'
#' @description
#' This subclass enforces domain-specific validation and schema rules for
#' individual data, including:
#' * Required fields for `uuid` (unique person ID), `hh_uuid` (household linkage), `sex`, and `age`
#' * Optional fields for estimated or exact date of birth, age in months/days,
#'   and household joining status
#' * Schema-based validation of column types and value domains
#' * Optional linkage back to a Household dataset
#'
#' @field household_link Optional linkage back to a HouseholdData object
#' @field optional_columns Character vector of optional columns for individual data
#'
#' @seealso [Data], [HouseholdData]
#' @export
IndividualData <- R6::R6Class(
  classname = "IndividualData",
  inherit   = Data,

  public = list(


    # Additional fields

    household_link   = NULL,  # linkage back to HouseholdData (optional)
    optional_columns = NULL,


    #' @description
    #' Creates a new IndividualData object with person-level survey data
    #'
    #' @param data Data frame containing individual-level survey data
    #' @param dataset_name Character name for the dataset (default: "IndividualData")
    #' @param metadata Optional list of metadata attributes
    #' @param variable_map Optional named list mapping variable roles to column names.
    #'   Common roles: uuid (person_id), hh_uuid, sex, age, est_dob, exact_dob, age_months, age_days, joined_hh
    #'
    #' @return A new IndividualData R6 object
    #'
    #' @details
    #' Initialization process:
    #' 1. Merges user variable_map with defaults (uuid defaults to "person_id")
    #' 2. Calls parent Data class initializer
    #' 3. Establishes required columns: uuid, hh_uuid, sex, age
    #' 4. Establishes optional columns: est_dob, exact_dob, age_months, age_days, joined_hh
    #' 5. Loads and merges individual-specific variable schema
    #' 6. Loads default indicator and dependency schemas
    #' 7. Auto-maps schema variables based on column names
    initialize = function(data,
                          dataset_name = "IndividualData",
                          metadata = NULL,
                          variable_map = NULL) {

      phrutils::phr_try({
        # Default mapping for individual-level data
        default_map <- list(
          uuid        = "person_id"        # unique individual ID
          # hh_uuid     = "hh_uuid"     # household linkage

        )

        # Merge user-specified map over defaults
        variable_map <- modifyList(default_map, variable_map %||% list())

        # Call parent (Data) initializer — uuid now refers to individual identifier
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          metadata     = metadata,
          uuid         = variable_map$uuid,
          variable_map = variable_map
        )

        # ---- Required & optional columns ----------------------------
        # Merge individual-required columns into Data$required_columns
        # (ensure we store *column names*, not roles)
        ind_required <- c(
          variable_map$uuid,     # unique person identifier
          variable_map$hh_uuid,  # household linkage (duplicates allowed)
          variable_map$sex,
          variable_map$age
        )
        ind_required <- unique(ind_required[!is.na(ind_required) & ind_required != ""])

        # Data$initialize already set self$required_columns <- uuid
        # We extend that with the individual-specific requirements
        self$required_columns <- unique(c(self$required_columns, ind_required))

        # Optional (mapped) columns
        self$optional_columns <- c(
          variable_map$est_dob,
          variable_map$exact_dob,
          variable_map$age_months,
          variable_map$age_days,
          variable_map$joined_hh
        )

        # ---- Build and merge individual schema into any existing schema ----
        ind_schema    <- self$default_schema()
        parent_schema <- self$variable_schema %||% list()
        merged_schema <- utils::modifyList(parent_schema, ind_schema)

        self$set_variable_schema(merged_schema)

        # ---- Load default indicator schema
        default_ind_schema <- self$default_indicator_schema()
        if (length(default_ind_schema) > 0) {
          self$set_indicator_schema(default_ind_schema)
          phrutils::phr_message(
            phr_txt("Loaded default indicator schema with {length(default_ind_schema)} indicator(s).")
          )
        }

        # ---- Load default dependency schema
        default_dep_schema <- self$default_dependency_schema()
        if (length(default_dep_schema$dependencies) > 0) {
          self$set_dependency_schema(default_dep_schema)
          phrutils::phr_message(
            phr_txt("Loaded default dependency schema with {length(default_dep_schema$dependencies)} dependency/ies.")
          )
        }

        phrutils::phr_message(phr_txt("{dataset_name} initialized as IndividualData object."))

      }, on_error = "abort", origin = "IndividualData$initialize")
    },


    #' Load Default Variable Schema
    #'
    #' @description
    #' Loads the default variable schema from the individual roster variable schema template Excel file
    #'
    #' @return Named list with variable schema definitions (types, allowed values, etc.)
    #'
    #' @details
    #' Reads variable_schema_data_individual_roster_template.xlsx from package resources and converts
    #' to canonical nested schema structure
    default_schema = function() {

      file <- system.file(
        "resources",
        "variable_schema_data_individual_roster_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phr_error(
          origin  = "IndividualData$default_schema",
          message = phr_txt("variable_schema_data_individual_roster_template.xlsx not found in package resources."),
          hint    = phr_txt("Place the schema file under inst/resources/ before building the package.")
        )
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_error(
            origin  = "IndividualData$default_schema",
            message = phr_txt("Failed to read variable_schema_data_individual_roster_template.xlsx"),
            hint    = e$message
          )
        }
      )

      # Convert table → canonical nested schema list
      schema <- data_table_to_schema(df)

      return(schema)
    },

    #' Load Default Indicator Schema
    #'
    #' @description
    #' Loads the default indicator schema from the individual roster indicator schema template Excel file
    #'
    #' @return Named list with indicator schema definitions, or empty list if template not found
    #'
    #' @details
    #' Reads indicator_schema_data_individual_roster_template.xlsx from package resources and converts
    #' to canonical nested indicator schema structure. Issues warning if file not found.
    default_indicator_schema = function() {

      file <- system.file(
        "resources",
        "indicator_schema_data_individual_roster_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "IndividualData$default_indicator_schema",
          message = phr_txt("indicator_schema_data_individual_roster_template.xlsx not found in package resources. Continuing without default indicator schema.")
        )
        return(list())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "IndividualData$default_indicator_schema",
            message = phr_txt("Failed to read indicator_schema_data_individual_roster_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list())

      # Convert table → canonical nested indicator schema list
      indicator_schema <- indicator_table_to_schema(df)

      return(indicator_schema)
    },

    #' Load Default Dependency Schema
    #'
    #' @description
    #' Loads the default dependency schema from the individual roster dependency schema template Excel file
    #'
    #' @return List with 'dependencies' element containing dependency definitions, or empty dependencies list if template not found
    #'
    #' @details
    #' Reads dependency_schema_data_individual_roster_template.xlsx from package resources and converts
    #' to canonical nested dependency schema structure. Issues warning if file not found.
    default_dependency_schema = function() {

      file <- system.file(
        "resources",
        "dependency_schema_data_individual_roster_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "IndividualData$default_dependency_schema",
          message = phr_txt("dependency_schema_data_individual_roster_template.xlsx not found in package resources. Continuing without default dependency schema.")
        )
        return(list(dependencies = list()))
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "IndividualData$default_dependency_schema",
            message = phr_txt("Failed to read dependency_schema_data_individual_roster_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list(dependencies = list()))

      # Convert table → canonical nested dependency schema list
      dependency_schema <- dependency_table_to_schema(df)

      return(dependency_schema)
    },

    #' Post-Validation Hook for Individual Data
    #'
    #' @description
    #' Hook method called after core validation completes. Performs individual-specific validation checks.
    #'
    #' @param df Data frame that was validated. If NULL, uses raw data from self$get_data("raw")
    #'
    #' @return Logical indicating validation success (TRUE if no issues, FALSE if warnings occurred)
    #'
    #' @details
    #' Performs individual-specific checks:
    #' * Household linkage validation (informational - duplicate hh_uuid expected for multi-member households)
    #' * Age validation (checks for negative ages)
    #' * Optional household link integrity check if household_link field is set
    post_validate = function(df = NULL) {

      nm <- self$dataset_name

      # Track whether any issues occurred
      had_issues <- FALSE

      phrutils::phr_try({

        if (is.null(df)) df <- self$get_data("raw")

        # Household linkage may duplicate — informational only
        hh_uuid_col <- self$variable_map$hh_uuid
        if (!is.null(hh_uuid_col) && hh_uuid_col %in% names(df)) {
          dup_hh <- df[[hh_uuid_col]][duplicated(df[[hh_uuid_col]])]
          if (length(dup_hh) > 0) {
            phrutils::phr_message(nm, phr_txt(
              "Duplicate household linkages detected (expected for multi-member households)."
            ))
          }
        }

        # Check that age is non-negative
        age_col <- self$variable_map$age
        if (!is.null(age_col) && age_col %in% names(df)) {
          if (any(df[[age_col]] < 0, na.rm = TRUE)) {
            phrutils::phr_warning(nm, phr_txt("Negative ages detected."))
            had_issues <- TRUE
          }
        }

        # Optional link integrity check
        if (!is.null(self$household_link)) {
          phrutils::phr_message(phr_txt("Validating linked HouseholdData (placeholder)."))
        }

        phrutils::phr_message(phr_txt(glue::glue("Post-validation for {nm} complete.")))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$post_validate"))

      # Return TRUE (good) or FALSE (warnings occurred)
      return(!had_issues)
    },


    #' @description
    #' Generate a DemographicsDataAnalytics object for individual data.
    #' Unifies quality checks and quantitative analysis in one object.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param type The type of analytics to create (currently "demographics")
    #' @param analysis_config Optional data analysis plan (tibble)
    #' @return A DataAnalytics subclass object or NULL
    generate_data_analytics = function(stage = c("standardized", "clean"),
                                       type  = c("demographics"),
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
          "demographics" = DemographicsDataAnalytics$new(
            data               = df,
            dap                = analysis_config,
            parent_data_object = self,
            dataset_name       = paste0(self$dataset_name, "_DemographicsDataAnalytics"),
            data_stage_name    = stage,
            data_hash          = data_hash,
            variable_map       = variable_map,
            value_map          = value_map,
            variable_label     = self$variable_label,
            value_label        = self$value_label
          ),
          phr_error(
            origin  = paste0(self$dataset_name, "$generate_data_analytics"),
            message = phr_txt("Unknown analytics type '{type}' for IndividualData. Valid types: demographics")
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
