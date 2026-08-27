#' QuantDataAnalysisPlanLog: Quant Data Analysis Plan Log Class
#'
#' @description
#' A specialized Log class for managing quantitative data analysis plans. 
#' Enforces the column structure defined in phr_analysis_plan_template.csv
#' and provides validation for analysis plan entries.
#'
#' @details
#' The QuantDataAnalysisPlanLog extends the Log base class with specific functionality for:
#' * Tracking planned quantitative analyses (indicators and calculations)
#' * Enforcing required analysis plan columns
#' * Validating calculation types and variable names
#' * Ensuring data analysis plans follow the standard template structure
#'
#' Required columns (matching phr_analysis_plan_template.csv):
#' * indicator_name: Name/label of the indicator
#' * calculation: Type of calculation (prop, mean, median, ratio)
#' * var_name: Primary variable name in the dataset
#' * denom_var: Denominator variable (for ratios)
#' * disaggregation: Disaggregation variable(s)
#' * multiplier: Multiplier for the calculation (e.g., 100 for percentages)
#' * indicator_unit: Unit of measurement for the indicator
#'
#' @keywords internal
QuantDataAnalysisPlanLog <- R6::R6Class(
  classname = "QuantDataAnalysisPlanLog",
  inherit = Log,

  public = list(

    #' @description
    #' Creates a new QuantDataAnalysisPlanLog with default schema and required columns
    #'
    #' @param log_df Optional data.frame of existing analysis plan entries
    #' @param log_name Character name for the log (default: "Quant Data Analysis Plan")
    #' @param required_columns Character vector; defaults to standard analysis plan columns
    #' @param schema List; defaults to standard analysis plan schema
    #'
    #' @return A new QuantDataAnalysisPlanLog R6 object
    #'
    #' @details
    #' Sets up default schema with appropriate types for each column.
    #' Calls parent Log$initialize() to complete setup.
    initialize = function(log_df = NULL,
                          log_name = "Quant Data Analysis Plan",
                          required_columns = NULL,
                          schema = NULL) {

      required_columns <- required_columns %||% c(
        "indicator_name",
        "calculation",
        "var_name",
        "denom_var",
        "disaggregation",
        "multiplier",
        "indicator_unit"
      )

      schema <- schema %||% list(
        types = list(
          indicator_name = "character",
          calculation = "character",
          var_name = "character",
          denom_var = "character",
          disaggregation = "character",
          multiplier = "numeric",
          indicator_unit = "character"
        ),
        allowed_values = list(
          calculation = c("prop", "mean", "median", "ratio", "cat", "categorical", "select_multiple_cat")
        )
      )

      super$initialize(
        log_df = log_df,
        log_name = log_name,
        required_columns = required_columns,
        schema = schema
      )
    },



    #' Validate Analysis Plan Log
    #'
    #' @description
    #' Validates analysis plan log structure and calculation types
    #'
    #' @return Invisible list of validation issues (empty if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Calls parent Log$validate() for basic structure and schema validation
    #' 2. Checks that critical fields (indicator_name, calculation, var_name)
    #'    are non-empty
    #' 3. Validates that multiplier is positive
    #' 4. Checks that calculation types are valid
    #'
    #' Sets self$validated and self$issues based on results.
    validate = function() {

      phrutils::phr_try({


        # 1. Start with super validation

        super_issues <- super$validate()
        if (is.null(super_issues)) super_issues <- list()

        df <- self$log_df


        # 2. Analysis plan specific completeness

        required_nonempty <- c("indicator_name", "calculation", "var_name")

        incomplete_cols <- vapply(
          required_nonempty,
          function(col) any(is.na(df[[col]]) | trimws(df[[col]]) == ""),
          logical(1)
        )

        if (any(incomplete_cols)) {
          bad_cols <- required_nonempty[incomplete_cols]

          super_issues$missing_or_empty <- bad_cols

          phrutils::phr_warning(
            self$log_name,
            phr_txt(glue::glue("Analysis plan contains missing/empty values in: {paste(bad_cols, collapse=', ')}."))
          )
        }


        # 3. Validate multiplier is positive (if present and not NA)

        if ("multiplier" %in% names(df)) {
          invalid_mult <- which(!is.na(df$multiplier) & df$multiplier <= 0)
          
          if (length(invalid_mult) > 0) {
            super_issues$invalid_multiplier <- invalid_mult
            
            phrutils::phr_warning(
              self$log_name,
              phr_txt(glue::glue("Analysis plan contains non-positive multipliers at rows: {paste(invalid_mult, collapse=', ')}."))
            )
          }
        }


        # 4. Merge issues and update state

        self$issues <- super_issues
        self$validated <- length(self$issues) == 0

        invisible(self$issues)

      }, on_error = "abort", origin = "QuantDataAnalysisPlanLog$validate")
    },



    #' Add Analysis Plan Entry
    #'
    #' @description
    #' Helper method to add an analysis plan entry with all required fields
    #'
    #' @param indicator_name Character; name/label of the indicator
    #' @param calculation Character; calculation type (prop, mean, median, ratio)
    #' @param var_name Character; primary variable name in the dataset
    #' @param denom_var Character; denominator variable (optional, for ratios)
    #' @param disaggregation Character; disaggregation variable(s) (optional)
    #' @param multiplier Numeric; multiplier for the calculation (default: 100)
    #' @param indicator_unit Character; unit of measurement (default: "\%")
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Validates inputs and calls append_entry() to add to log.
    add_indicator = function(indicator_name,
                             calculation,
                             var_name,
                             denom_var = NA_character_,
                             disaggregation = NA_character_,
                             multiplier = 100,
                             indicator_unit = "%") {

      entry <- list(
        indicator_name = as.character(indicator_name),
        calculation = as.character(calculation),
        var_name = as.character(var_name),
        denom_var = as.character(denom_var),
        disaggregation = as.character(disaggregation),
        multiplier = as.numeric(multiplier),
        indicator_unit = as.character(indicator_unit)
      )

      self$append_entry(entry)
    }
  )
)

