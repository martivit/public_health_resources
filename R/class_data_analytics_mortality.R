#' IPHRA Mortality Data Analytics Class
#'
#' The `MortalityDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Mortality-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Mortality-specific:
#' * Quality checks (death recording, mortality rates, age/sex distribution) via quality_schema
#' * Quantitative analysis indicators via analysis_schema (household), analysis_schema_roster,
#'   and analysis_schema_deaths
#' * All visualizations/tables stored in named sub-lists: \code{$household}, \code{$roster},
#'   \code{$deaths} within \code{visualizations} and \code{tables}
#'
#' @details
#' A household dataframe is required at initialization. Linked roster and deaths data are
#' optional and are populated when generated from a \code{HouseholdData} object via
#' \code{generate_data_analytics(type = "mortality")}. When \code{run_analysis()} or
#' \code{run_outputs()} are called, each available dataset (household, roster, deaths) is
#' processed using its dedicated schema and results are stored under the corresponding
#' named sub-list. This is driven by the \code{pre_run_analysis()} and
#' \code{pre_run_outputs()} hooks, which return one input set (data, schema, survey
#' designs, analysis plan) per available dataset; the inherited \code{run_analysis()}
#' and \code{run_outputs()} iterate over these sets.
#'
#' @field linked_ind_roster_data Optional linked roster dataframe
#' @field linked_ind_roster_data_stage_name Name of linked roster data stage
#' @field linked_ind_roster_data_hash Hash of linked roster data
#' @field linked_ind_roster_variable_map Variable mappings for linked roster data
#' @field linked_ind_roster_value_map Value mappings for linked roster data
#' @field linked_ind_roster_variable_label Variable labels for linked roster data
#' @field linked_ind_roster_value_label Value labels for linked roster data
#' @field linked_ind_deaths_data Optional linked deaths dataframe
#' @field linked_ind_deaths_data_stage_name Name of linked deaths data stage
#' @field linked_ind_deaths_data_hash Hash of linked deaths data
#' @field linked_ind_deaths_variable_map Variable mappings for linked deaths data
#' @field linked_ind_deaths_value_map Value mappings for linked deaths data
#' @field linked_ind_deaths_variable_label Variable labels for linked deaths data
#' @field linked_ind_deaths_value_label Value labels for linked deaths data
#' @field analysis_schema_roster Analysis schema for linked roster dataset
#' @field analysis_schema_deaths Analysis schema for linked deaths dataset
#' @field outputs_schema_roster Outputs schema for linked roster dataset
#' @field outputs_schema_deaths Outputs schema for linked deaths dataset
#' @field data_analysis_plan_roster Data analysis plan for linked roster dataset
#' @field data_analysis_plan_deaths Data analysis plan for linked deaths dataset
#' @field survey_design_roster srvyr survey design object for the linked roster dataset
#' @field survey_design_deaths srvyr survey design object for the linked deaths dataset
#' @field base_survey_design_roster Unfiltered base srvyr survey design object for the linked roster dataset
#' @field base_survey_design_deaths Unfiltered base srvyr survey design object for the linked deaths dataset
#'
#' @seealso [DataAnalytics]
#' @export
MortalityDataAnalytics <- R6::R6Class(
  classname = "MortalityDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Fields for linked roster data
    linked_ind_roster_data = NULL,
    linked_ind_roster_data_stage_name = NULL,
    linked_ind_roster_data_hash = NULL,
    linked_ind_roster_variable_map = NULL,
    linked_ind_roster_value_map = NULL,
    linked_ind_roster_variable_label = NULL,
    linked_ind_roster_value_label = NULL,

    # Fields for linked deaths data
    linked_ind_deaths_data = NULL,
    linked_ind_deaths_data_stage_name = NULL,
    linked_ind_deaths_data_hash = NULL,
    linked_ind_deaths_variable_map = NULL,
    linked_ind_deaths_value_map = NULL,
    linked_ind_deaths_variable_label = NULL,
    linked_ind_deaths_value_label = NULL,

    # Per-dataset schemas for roster and deaths
    analysis_schema_roster = NULL,
    analysis_schema_deaths = NULL,
    outputs_schema_roster  = NULL,
    outputs_schema_deaths  = NULL,

    # Per-dataset data analysis plans
    data_analysis_plan_roster = NULL,
    data_analysis_plan_deaths = NULL,

    # Per-dataset survey design objects
    survey_design_roster = NULL,
    survey_design_deaths = NULL,
    base_survey_design_roster = NULL,
    base_survey_design_deaths = NULL,

    #' @description
    #' Initialize a new MortalityDataAnalytics object
    #'
    #' @param data A required data frame (standardized or clean household/mortality data)
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
    #' @param linked_ind_roster_variable_label Variable labels for linked roster data
    #' @param linked_ind_roster_value_label Value labels for linked roster data
    #' @param linked_ind_deaths_data Optional linked deaths dataframe
    #' @param linked_ind_deaths_data_stage_name Name of linked deaths data stage
    #' @param linked_ind_deaths_data_hash Hash of linked deaths data
    #' @param linked_ind_deaths_variable_map Variable mappings for linked deaths data
    #' @param linked_ind_deaths_value_map Value mappings for linked deaths data
    #' @param linked_ind_deaths_variable_label Variable labels for linked deaths data
    #' @param linked_ind_deaths_value_label Value labels for linked deaths data
    #' @return A new MortalityDataAnalytics object
    initialize = function(data,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "MortalityDataAnalytics",
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
                          linked_ind_roster_variable_label = NULL,
                          linked_ind_roster_value_label = NULL,
                          linked_ind_deaths_data = NULL,
                          linked_ind_deaths_data_stage_name = NULL,
                          linked_ind_deaths_data_hash = NULL,
                          linked_ind_deaths_variable_map = NULL,
                          linked_ind_deaths_value_map = NULL,
                          linked_ind_deaths_variable_label = NULL,
                          linked_ind_deaths_value_label = NULL) {

      origin <- paste0(dataset_name, "$initialize")

      if (missing(data) || is.null(data)) {
        phr_error(
          origin  = origin,
          message = "The data parameter (household dataframe) is required to initialize MortalityDataAnalytics."
        )
      }

      # Pre-merge linked roster and deaths variable/value maps and labels into the
      # household maps BEFORE calling super$initialize. This ensures that when
      # super$initialize() calls generate_dap_from_schema(), self$variable_map
      # already contains all canonical-name mappings, including those for mortality
      # analysis indicators that may be defined in a linked dataset's variable_map.
      # Household data takes precedence: keys already present are never overwritten.
      merged_variable_map   <- variable_map   %||% list()
      merged_value_map      <- value_map      %||% list()
      merged_variable_label <- variable_label %||% list()
      merged_value_label    <- value_label    %||% list()

      for (linked_maps in list(
        list(
          variable_map   = linked_ind_roster_variable_map,
          value_map      = linked_ind_roster_value_map,
          variable_label = linked_ind_roster_variable_label,
          value_label    = linked_ind_roster_value_label
        ),
        list(
          variable_map   = linked_ind_deaths_variable_map,
          value_map      = linked_ind_deaths_value_map,
          variable_label = linked_ind_deaths_variable_label,
          value_label    = linked_ind_deaths_value_label
        )
      )) {
        if (!is.null(linked_maps$variable_map)) {
          for (key in names(linked_maps$variable_map)) {
            if (is.null(merged_variable_map[[key]])) {
              merged_variable_map[[key]] <- linked_maps$variable_map[[key]]
            }
          }
        }
        if (!is.null(linked_maps$value_map)) {
          for (key in names(linked_maps$value_map)) {
            if (is.null(merged_value_map[[key]])) {
              merged_value_map[[key]] <- linked_maps$value_map[[key]]
            }
          }
        }
        if (!is.null(linked_maps$variable_label)) {
          for (key in names(linked_maps$variable_label)) {
            if (is.null(merged_variable_label[[key]])) {
              merged_variable_label[[key]] <- linked_maps$variable_label[[key]]
            }
          }
        }
        if (!is.null(linked_maps$value_label)) {
          for (key in names(linked_maps$value_label)) {
            if (is.null(merged_value_label[[key]])) {
              merged_value_label[[key]] <- linked_maps$value_label[[key]]
            }
          }
        }
      }

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = merged_variable_map,
        value_map = merged_value_map,
        variable_label = merged_variable_label,
        value_label = merged_value_label,
        quality_schema = quality_schema
      )

      self$linked_ind_roster_data              <- linked_ind_roster_data
      self$linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      self$linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      self$linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      self$linked_ind_roster_value_map         <- linked_ind_roster_value_map
      self$linked_ind_roster_variable_label    <- linked_ind_roster_variable_label
      self$linked_ind_roster_value_label       <- linked_ind_roster_value_label

      self$linked_ind_deaths_data              <- linked_ind_deaths_data
      self$linked_ind_deaths_data_stage_name   <- linked_ind_deaths_data_stage_name
      self$linked_ind_deaths_data_hash         <- linked_ind_deaths_data_hash
      self$linked_ind_deaths_variable_map      <- linked_ind_deaths_variable_map
      self$linked_ind_deaths_value_map         <- linked_ind_deaths_value_map
      self$linked_ind_deaths_variable_label    <- linked_ind_deaths_variable_label
      self$linked_ind_deaths_value_label       <- linked_ind_deaths_value_label

      # Load per-dataset schemas for roster and deaths
      self$analysis_schema_roster <- self$default_analysis_schema_roster()
      self$analysis_schema_deaths <- self$default_analysis_schema_deaths()
      self$outputs_schema_roster  <- self$default_outputs_schema_roster()
      self$outputs_schema_deaths  <- self$default_outputs_schema_deaths()

      # Generate per-dataset analysis plans for roster and deaths
      self$data_analysis_plan_roster <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = "Quant Data Analysis Plan (Roster)"
      )
      self$data_analysis_plan_deaths <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = "Quant Data Analysis Plan (Deaths)"
      )

      if (!is.null(linked_ind_roster_data)) {
        self$generate_dap_from_schema_roster()
      }

      if (!is.null(linked_ind_deaths_data)) {
        self$generate_dap_from_schema_deaths()
      }

      # Create and store per-dataset survey designs for roster and deaths
      if (!is.null(linked_ind_roster_data)) {
        self$base_survey_design_roster <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_roster_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for roster data."
        )
        self$survey_design_roster <- private$.create_survey_design_for_dataset(
          data         = linked_ind_roster_data,
          variable_map = linked_ind_roster_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      if (!is.null(linked_ind_deaths_data)) {
        self$base_survey_design_deaths <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_deaths_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for deaths data."
        )
        self$survey_design_deaths <- private$.create_survey_design_for_dataset(
          data         = linked_ind_deaths_data,
          variable_map = linked_ind_deaths_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_deaths_data)) msg_parts <- c(msg_parts, "deaths")
      if (length(msg_parts) > 0) {
        phrutils::phr_message(
          phr_txt(glue::glue(
            "MortalityDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data."
          ))
        )
      } else {
        phrutils::phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as MortalityDataAnalytics object."))
        )
      }
    },

    #' @description Load the default Mortality quality schema from template file
    #' @return A list of Mortality-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_mortality_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "MortalityDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_mortality_template.xlsx: {e$message}"))
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

    #' @description Load the default Mortality household outputs schema from template file
    #' @return A list of Mortality-specific outputs definitions for the household dataset
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality household analysis schema from template file
    #' @return A tibble containing the Mortality household analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Mortality roster outputs schema from template file
    #' @return A list of outputs definitions for the linked roster dataset
    default_outputs_schema_roster = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_roster_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_roster_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema_roster",
        hint     = "Check that outputs_schema_data_analytics_mortality_roster_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality deaths outputs schema from template file
    #' @return A list of outputs definitions for the linked deaths dataset
    default_outputs_schema_deaths = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_deaths_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_deaths_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema_deaths",
        hint     = "Check that outputs_schema_data_analytics_mortality_deaths_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality roster analysis schema from template file
    #' @return A tibble containing the analysis schema for the linked roster dataset
    default_analysis_schema_roster = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_roster_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_roster_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema_roster",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_roster_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Mortality deaths analysis schema from template file
    #' @return A tibble containing the analysis schema for the linked deaths dataset
    default_analysis_schema_deaths = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phrutils::phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema_deaths",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Generate the data analysis plan for the linked roster dataset.
    #'
    #' Uses \code{analysis_schema_roster} and \code{linked_ind_roster_variable_map}
    #' (falling back to the merged \code{variable_map}) to translate canonical variable
    #' names to actual column names present in \code{linked_ind_roster_data}, then
    #' stores the resulting plan in \code{data_analysis_plan_roster}.
    #'
    #' @return Invisibly returns self.
    generate_dap_from_schema_roster = function() {
      origin <- paste0(self$dataset_name, "$generate_dap_from_schema_roster")
      phrutils::phr_message(origin, "Generating data_analysis_plan_roster from analysis_schema_roster...")

      phrutils::phr_try({

        if (is.null(self$linked_ind_roster_data) || nrow(self$linked_ind_roster_data) == 0) {
          phrutils::phr_warning(origin, "linked_ind_roster_data is not available; skipping roster DAP generation.")
          return(invisible(self))
        }

        if (is.null(self$analysis_schema_roster) || nrow(self$analysis_schema_roster) == 0) {
          phrutils::phr_warning(origin, "analysis_schema_roster is empty; data_analysis_plan_roster will remain empty.")
          return(invisible(self))
        }

        result <- private$.build_dap_from_schema(
          analysis_schema = self$analysis_schema_roster,
          variable_map    = self$linked_ind_roster_variable_map %||% self$variable_map %||% list(),
          available_vars  = names(self$linked_ind_roster_data),
          dataset_label   = "roster"
        )

        self$data_analysis_plan_roster$log_df <- result$dap_df

        if (nrow(result$issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            result$issues
          )
          phrutils::phr_warning(origin, paste0(nrow(result$issues), " roster indicators skipped due to missing variables."))
        } else {
          phrutils::phr_message(origin, "All roster schema indicators found and added to data_analysis_plan_roster.")
        }

      }, on_error = "warn", origin = origin,
         hint = "Ensure linked_ind_roster_data has the expected columns and analysis_schema_roster is valid.")

      invisible(self)
    },

    #' @description
    #' Generate the data analysis plan for the linked deaths dataset.
    #'
    #' Uses \code{analysis_schema_deaths} and \code{linked_ind_deaths_variable_map}
    #' (falling back to the merged \code{variable_map}) to translate canonical variable
    #' names to actual column names present in \code{linked_ind_deaths_data}, then
    #' stores the resulting plan in \code{data_analysis_plan_deaths}.
    #'
    #' @return Invisibly returns self.
    generate_dap_from_schema_deaths = function() {
      origin <- paste0(self$dataset_name, "$generate_dap_from_schema_deaths")
      phrutils::phr_message(origin, "Generating data_analysis_plan_deaths from analysis_schema_deaths...")

      phrutils::phr_try({

        if (is.null(self$linked_ind_deaths_data) || nrow(self$linked_ind_deaths_data) == 0) {
          phrutils::phr_warning(origin, "linked_ind_deaths_data is not available; skipping deaths DAP generation.")
          return(invisible(self))
        }

        if (is.null(self$analysis_schema_deaths) || nrow(self$analysis_schema_deaths) == 0) {
          phrutils::phr_warning(origin, "analysis_schema_deaths is empty; data_analysis_plan_deaths will remain empty.")
          return(invisible(self))
        }

        result <- private$.build_dap_from_schema(
          analysis_schema = self$analysis_schema_deaths,
          variable_map    = self$linked_ind_deaths_variable_map %||% self$variable_map %||% list(),
          available_vars  = names(self$linked_ind_deaths_data),
          dataset_label   = "deaths"
        )

        self$data_analysis_plan_deaths$log_df <- result$dap_df

        if (nrow(result$issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            result$issues
          )
          phrutils::phr_warning(origin, paste0(nrow(result$issues), " deaths indicators skipped due to missing variables."))
        } else {
          phrutils::phr_message(origin, "All deaths schema indicators found and added to data_analysis_plan_deaths.")
        }

      }, on_error = "warn", origin = origin,
         hint = "Ensure linked_ind_deaths_data has the expected columns and analysis_schema_deaths is valid.")

      invisible(self)
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #'
    #' Returns one input set per available mortality dataset:
    #' \describe{
    #'   \item{household}{Always included, using the core \code{data},
    #'     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #'     stored in \code{analysis_results$household}.}
    #'   \item{roster}{Included when \code{linked_ind_roster_data} is available and
    #'     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Included when \code{linked_ind_deaths_data} is available and
    #'     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return A named list of field sets.
    pre_run_analysis = function() {
      origin <- paste0(self$dataset_name, "$pre_run_analysis")

      sets <- list(
        household = list(
          data = "data",
          survey_design = "survey_design",
          data_analysis_plan = "data_analysis_plan"
        )
      )

      if (!is.null(self$linked_ind_roster_data)) {
        if (private$.dap_row_count(self$data_analysis_plan_roster) > 0L) {
          sets$roster <- list(
            data = "linked_ind_roster_data",
            survey_design = "survey_design_roster",
            data_analysis_plan = "data_analysis_plan_roster"
          )
        } else {
          phrutils::phr_message(origin, "Linked roster data present but data_analysis_plan_roster is empty. Skipping roster analysis.")
        }
      }

      if (!is.null(self$linked_ind_deaths_data)) {
        if (private$.dap_row_count(self$data_analysis_plan_deaths) > 0L) {
          sets$deaths <- list(
            data = "linked_ind_deaths_data",
            survey_design = "survey_design_deaths",
            data_analysis_plan = "data_analysis_plan_deaths"
          )
        } else {
          phrutils::phr_message(origin, "Linked deaths data present but data_analysis_plan_deaths is empty. Skipping deaths analysis.")
        }
      }

      sets
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_outputs()}.
    #'
    #' Returns one input set per available mortality dataset so that outputs are
    #' generated independently for each dataset and stored in named sub-lists
    #' (\code{visualizations$household} / \code{tables$household},
    #' \code{visualizations$roster} / \code{tables$roster},
    #' \code{visualizations$deaths} / \code{tables$deaths}). Each set passes
    #' the appropriate survey design objects for the dataset, so output entries
    #' with \code{dataset_type = "survey_design"} receive the weighted design and
    #' \code{"data"}/\code{"base"} entries receive the unweighted base design.
    #'
    #' @return A named list of field sets.
    pre_run_outputs = function() {
      origin <- paste0(self$dataset_name, "$pre_run_outputs")

      sets <- list(
        household = list(
          data = "data",
          outputs_schema = "outputs_schema",
          base_survey_design = "base_survey_design",
          survey_design = "survey_design",
          variable_map = "variable_map"
        )
      )

      if (!is.null(self$linked_ind_roster_data)) {
        if (length(self$outputs_schema_roster %||% list()) > 0) {
          sets$roster <- list(
            data = "linked_ind_roster_data",
            outputs_schema = "outputs_schema_roster",
            base_survey_design = "base_survey_design_roster",
            survey_design = "survey_design_roster",
            variable_map = "linked_ind_roster_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked roster data present but outputs_schema_roster is empty. Skipping roster outputs.")
        }
      }

      if (!is.null(self$linked_ind_deaths_data)) {
        if (length(self$outputs_schema_deaths %||% list()) > 0) {
          sets$deaths <- list(
            data = "linked_ind_deaths_data",
            outputs_schema = "outputs_schema_deaths",
            base_survey_design = "base_survey_design_deaths",
            survey_design = "survey_design_deaths",
            variable_map = "linked_ind_deaths_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked deaths data present but outputs_schema_deaths is empty. Skipping deaths outputs.")
        }
      }

      sets
    }

  ),

  private = list(

    # Return the number of rows in a QuantDataAnalysisPlanLog's log_df, or 0L.
    #
    # @param dap A QuantDataAnalysisPlanLog object (or NULL).
    # @return Integer row count, or 0L when dap or its log_df is NULL.
    .dap_row_count = function(dap) {
      if (!is.null(dap) && !is.null(dap$log_df)) nrow(dap$log_df) else 0L
    },

    # Create a proper survey design for an arbitrary data frame using variable_map.
    #
    # Applies the same logic as \code{create_survey_design()} (which operates on
    # \code{self$data} and \code{self$variable_map}) but accepts any data frame and
    # variable map. Reads \code{cluster_id_numeric} / \code{cluster_id},
    # \code{weight}, \code{stratum}, and \code{fpc} from \code{variable_map} and
    # builds an \code{srvyr} survey design accordingly. Falls back to a simple
    # random sample (\code{ids = 1}) when none of those columns are present.
    #
    # @param data A data frame for the linked dataset.
    # @param variable_map Named list mapping canonical names to actual column names.
    # @param origin Character; caller label used in log/warning messages.
    # @return An \code{srvyr} survey design object, or NULL on failure.
    .create_survey_design_for_dataset = function(data, variable_map, origin = NULL) {

      origin <- origin %||% paste0(self$dataset_name, "$.create_survey_design_for_dataset")

      if (is.null(data) || nrow(data) == 0) {
        phrutils::phr_warning(origin, "No data available to create survey design for linked dataset.")
        return(NULL)
      }

      vm        <- variable_map %||% list()
      data_cols <- names(data)

      cluster_col <- vm[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- vm[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- vm[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) weight_col <- NULL

      strata_col <- vm[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) strata_col <- NULL

      fpc_col    <- vm[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) fpc_col <- NULL

      if (is.null(cluster_col)) {
        phrutils::phr_message(origin, "No cluster column found for linked dataset; using ids = 1 (simple random sample design).")
      }

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phrutils::phr_try(
        srvyr::as_survey_design(
          .data   = data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data in the linked dataset."
      )

      if (!is.null(design)) {
        phrutils::phr_message(origin, "Survey design created successfully for linked dataset.")
      }

      design
    },

    # Build a DAP tibble from an analysis schema, resolving canonical variable names.
    #
    # Translates each \code{var_name} and \code{denom_var} in \code{analysis_schema}
    # through \code{variable_map}, then keeps only indicators whose resolved columns
    # are present in \code{available_vars}.
    #
    # @param analysis_schema A tibble with analysis schema columns.
    # @param variable_map Named list mapping canonical names to actual column names.
    # @param available_vars Character vector of column names available in the target dataset.
    # @param dataset_label Character label used in the returned issues tibble (e.g. "roster").
    # @return A named list with elements \code{dap_df} (tibble) and \code{issues} (tibble).
    .build_dap_from_schema = function(analysis_schema, variable_map, available_vars,
                                       dataset_label = "dataset") {

      vm <- variable_map %||% list()

      translate_var <- function(canonical_name) {
        if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
        if (canonical_name %in% names(vm)) {
          actual <- vm[[canonical_name]]
          if (!is.null(actual) && nzchar(actual)) return(actual)
        }
        return(canonical_name)
      }

      schema_valid <- analysis_schema |>
        dplyr::mutate(
          var_name_actual  = purrr::map_chr(.data$var_name,  translate_var),
          denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
          var_exists       = .data$var_name_actual %in% available_vars,
          denom_exists     = ifelse(
            !is.na(.data$denom_var_actual),
            .data$denom_var_actual %in% available_vars,
            TRUE
          )
        ) |>
        dplyr::mutate(include = .data$var_exists & .data$denom_exists)

      issues <- schema_valid |>
        dplyr::filter(!.data$include) |>
        dplyr::transmute(
          dataset        = dataset_label,
          indicator_name = .data$indicator_name,
          issue = paste0(
            "Missing variable(s): ",
            ifelse(!.data$var_exists,
                   paste0(.data$var_name, " (maps to: ", .data$var_name_actual, ")"),
                   ""),
            ifelse(!.data$denom_exists & !is.na(.data$denom_var),
                   paste0(", ", .data$denom_var, " (maps to: ", .data$denom_var_actual, ")"),
                   "")
          )
        )

      dap_df <- schema_valid |>
        dplyr::filter(.data$include) |>
        dplyr::transmute(
          indicator_name = .data$indicator_name,
          calculation    = .data$calculation,
          var_name       = .data$var_name_actual,
          denom_var      = .data$denom_var_actual,
          disaggregation = .data$disaggregation,
          multiplier     = .data$multiplier,
          indicator_unit = .data$indicator_unit
        )

      list(dap_df = dap_df, issues = issues)
    },

    # Run analysis for an arbitrary dataset using a given analysis schema.
    #
    # Creates a simple SRS survey design from \code{data}, builds a DAP from
    # \code{analysis_schema} (resolving canonical names via \code{variable_map}),
    # and runs \code{phr_calc_survey_from_plan}.
    #
    # @param data A data frame.
    # @param analysis_schema A tibble with columns: indicator_name, calculation,
    #   var_name, denom_var, disaggregation, multiplier, indicator_unit.
    # @param variable_map Optional named list mapping canonical names to actual column names.
    # @return A named list with elements \code{survey_design} and \code{base}, or NULLs.
    .run_analysis_for_dataset = function(data, analysis_schema, variable_map = NULL) {
      origin <- paste0(self$dataset_name, "$.run_analysis_for_dataset")

      if (is.null(data) || nrow(data) == 0) {
        phrutils::phr_warning(origin, "No data available for linked-dataset analysis.")
        return(list(survey_design = NULL, base = NULL))
      }

      if (is.null(analysis_schema) || nrow(analysis_schema) == 0) {
        phrutils::phr_warning(origin, "No analysis schema rows available for linked-dataset analysis.")
        return(list(survey_design = NULL, base = NULL))
      }

      survey_design <- phrutils::phr_try(
        srvyr::as_survey_design(.data = data, ids = 1),
        on_error = "warn",
        origin   = origin,
        hint     = "Could not create simple survey design from linked dataset."
      )

      if (is.null(survey_design)) {
        return(list(survey_design = NULL, base = NULL))
      }

      available_vars <- names(survey_design$variables)
      vm             <- variable_map %||% list()

      translate_var <- function(canonical_name) {
        if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
        if (canonical_name %in% names(vm)) {
          actual <- vm[[canonical_name]]
          if (!is.null(actual) && nzchar(actual)) return(actual)
        }
        return(canonical_name)
      }

      schema_valid <- analysis_schema |>
        dplyr::mutate(
          var_name_actual  = purrr::map_chr(.data$var_name,  translate_var),
          denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
          var_exists       = .data$var_name_actual %in% available_vars,
          denom_exists     = ifelse(
            !is.na(.data$denom_var_actual),
            .data$denom_var_actual %in% available_vars,
            TRUE
          )
        ) |>
        dplyr::filter(.data$var_exists & .data$denom_exists)

      if (nrow(schema_valid) == 0) {
        phrutils::phr_warning(origin, "No valid indicators found in analysis schema for linked dataset after variable matching.")
        return(list(survey_design = NULL, base = NULL))
      }

      dap_df <- schema_valid |>
        dplyr::transmute(
          indicator_name = .data$indicator_name,
          calculation    = .data$calculation,
          var_name       = .data$var_name_actual,
          denom_var      = .data$denom_var_actual,
          disaggregation = .data$disaggregation,
          multiplier     = .data$multiplier,
          indicator_unit = .data$indicator_unit
        )

      survey_results <- phrutils::phr_try(
        phr_calc_survey_from_plan(
          design        = survey_design,
          analysis_plan = dap_df
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Verify all variables exist in the linked dataset and that the analysis plan is valid."
      )

      list(survey_design = survey_results, base = survey_results)
    }

  )
)
