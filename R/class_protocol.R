#' Protocol R6 Class
#'
#' @description
#' Base protocol class for planning, managing, validating, and reporting an
#' assessment protocol. A `Protocol` stores assessment metadata, manages a
#' collection of data collection tools, links the protocol to a conceptual
#' framework, checks coherence between framework objectives and tool indicators,
#' and prepares parameters for Quarto-based report generation.
#'
#' @details
#' `Protocol` inherits from `Document` and therefore also supports document
#' generation workflows defined by the parent class. The protocol maintains a
#' `Framework` object, a named list of `Tool` objects, metadata fields used in
#' Terms of Reference and reporting templates, and helper methods for producing
#' data analysis planning outputs.
#'
#' The class is intended to be subclassed for specific protocol types, such as
#' IPHRA protocols, where additional active bindings, Quarto parameters, tool
#' types, or reporting tables may be defined.
#'
#' @section Public fields:
#' \describe{
#'   \item{framework}{A `Framework` or subclass instance associated with the protocol.}
#'   \item{tools}{Named list of registered tool objects.}
#'   \item{valid_tool_types}{Character vector of allowed tool types.}
#'   \item{issues}{List of validation or protocol issues.}
#'   \item{issues_coherence}{List of coherence issues between framework and tools.}
#'   \item{metadata}{Named list of protocol metadata used for reporting and documentation.}
#'   \item{secondary_data}{Optional secondary data sources keyed by objective code.}
#' }
#'
#' @section Key methods:
#' \describe{
#'   \item{`initialize()`}{Create a new protocol object.}
#'   \item{`add_tools()`}{Add a tool object to the protocol.}
#'   \item{`get_tool_names()`}{Return registered tool names.}
#'   \item{`is_tool_included()`}{Check whether a tool is registered.}
#'   \item{`validate_objective_schema()`}{Validate an objective schema.}
#'   \item{`diagnose_coherence()`}{Check alignment between framework indicators and tool indicators.}
#'   \item{`get_dap_table()`}{Build a data analysis plan table for a selected tool.}
#'   \item{`get_quarto_params()`}{Return protocol parameters for Quarto rendering.}
#' }
#'
#' @section Active bindings:
#' \describe{
#'   \item{`.release_date`}{Read-only binding returning the current system date.}
#'   \item{`.objectives_research_questions_df`}{Returns a table of pillars, sub-pillars, objectives, and research questions linked to indicators used across tools.}
#'   \item{`.secondary_data_sources_df`}{Returns the framework secondary data sources table.}
#'   \item{`.modified_framework_svg`}{Returns a temporary SVG file path for the adjusted or master framework diagram.}
#' }
#'
#' @examples
#' \dontrun{
#' protocol <- Protocol$new(
#'   assessment_title = "Example Assessment",
#'   country_name = "Example Country",
#'   month_year = "January 2027",
#'   framework_type = "ana"
#' )
#'
#' protocol$add_tools(tool_type = "household", tool_name = "household")
#' protocol$get_tool_names()
#' protocol$diagnose_coherence()
#' }
#'
#' @importFrom R6 R6Class
#' @export
Protocol <- R6::R6Class(
  "Protocol",
  inherit = Document,
  public = list(
    #' @field framework A \code{\link{Framework}} object (e.g.
    #'   \code{\link{ANAFramework}}) that holds the master and adjusted
    #'   reference schemas and SVG diagrams for this protocol.  Assign a
    #'   Framework instance to this field to associate a conceptual framework
    #'   with the protocol.
    framework = NULL,

    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,

    #' @field valid_tool_types Character vector of allowed tool types for
    #'   \code{add_tools()}.
    valid_tool_types = c(
      "household",
      "key_informant",
      "observation",
      "generic"
    ),

    #' @field issues List of validation issues and discrepancies
    issues = list(),

    #' @field issues_coherence List of coherence issues found between the
    #'   \code{modified_objectives_schema} indicator codes and the tool indicator
    #'   codes.  Populated by \code{diagnose_coherence()}.
    issues_coherence = list(),

    #' @field metadata List containing protocol metadata
    metadata = list(
      research_cycle_id = NULL,
      country = NULL,
      release_date = NULL,
      version_number = NULL,
      type_emergency = NULL,
      type_crisis = NULL,
      population = NULL,
      rationale = NULL,
      date_pilot_training = NULL,
      date_data_collection_start = NULL,
      date_data_collection_end = NULL,
      date_data_analysis = NULL,
      date_data_validation = NULL,
      date_preliminary_presentation = NULL,
      date_outputs_validation = NULL,
      date_outputs_publication = NULL,
      date_final_presentation = NULL,
      audience_type_cluster = NULL,
      expected_output_cluster = NULL,
      expected_output_donor = NULL,
      expected_output_operational_actor = NULL,
      expected_output_other = NULL,
      dissemination_strategy_cluster = NULL,
      dissemination_strategy_donor = NULL,
      dissemination_strategy_operational_actor = NULL,
      dissemination_strategy_other = NULL,
      access_cluster = NULL,
      access_donor = NULL,
      access_operational_actor = NULL,
      access_other = NULL,
      visibility_cluster = NULL,
      visibility_donor = NULL,
      visibility_operational_actor = NULL,
      visibility_other = NULL,

      created_date = NULL,
      modified_datetime = NULL,
      month_year = NULL,
      country_name = NULL,
      assessment_title = NULL,
      target_strata = list(),
      protocol_version = "1.0",
      version = 1L,
      # Text metadata fields
      mandating_body = NULL,
      project_code = NULL,
      overall_timeframe = NULL,
      pilot_date = NULL,
      data_start_date = NULL,
      data_end_date = NULL,
      analysis_date = NULL,
      data_validation_date = NULL,
      prelim_presentation_date = NULL,
      output_validation_date = NULL,
      output_published_date = NULL,
      final_presentation_date = NULL,
      date_milestone_donor = NULL,
      date_milestone_intercluster = NULL,
      date_milestone_cluster = NULL,
      date_milestone_ngo_platform = NULL,
      date_milestone_other = NULL,
      geographic_coverage = NULL,
      stratification = NULL,
      num_report = NULL,
      num_profile = NULL,
      num_prelim_presentation = NULL,
      num_final_presentation = NULL,
      num_factsheet = NULL,
      num_dashboard = NULL,
      num_webmap = NULL,
      num_map = NULL,
      num_output_other = NULL
    ),

    #' @field secondary_data Named list of secondary data sources keyed by
    #'   objective code.  Each element is a character string naming the source
    #'   or a URL.  Objective codes must match codes available in the
    #'   \code{master_objectives_schema} of the associated \code{\link{Framework}}.
    secondary_data = NULL,

    #' @description
    #' Creates a new Protocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  Must be
    #'   one of \code{"none"} (creates a generic \code{\link{Framework}} object) or
    #'   \code{"ana"} (creates an \code{\link{ANAFramework}} object).
    #' @param reference_doc_filename Optional document template filename/path
    #'   passed to \code{Document$initialize()}.
    #' @param reference_ppt_filename Optional PowerPoint template filename/path
    #'   passed to \code{Document$initialize()}.
    #' @return A new Protocol object
    initialize = function(
      assessment_title = NULL,
      country_name = NULL,
      month_year = NULL,
      framework_type = "none",
      reference_doc_filename = NULL,
      reference_ppt_filename = NULL
    ) {
      phr_try(
        {
          super$initialize(
            reference_doc_filename = private$..default_template_filenames(),
            reference_ppt_filename = private$..default_ppt_template_filenames()
          )
          valid_fw_types <- c("none", "ana")
          phr_assert(
            is.character(framework_type) &&
              length(framework_type) == 1 &&
              framework_type %in% valid_fw_types,
            message = phr_txt(
              "framework_type must be one of: {paste(valid_fw_types, collapse=', ')}."
            ),
            origin = "Protocol$initialize"
          )
          self$metadata$created_date <- Sys.time()
          self$metadata$modified_datetime <- Sys.time()
          self$metadata$assessment_title <- assessment_title
          self$metadata$country_name <- country_name
          self$metadata$month_year <- month_year
          self$tools <- list()
          self$valid_tool_types <- as.character(
            self$valid_tool_types %||% character(0)
          )
          self$issues <- list()
          self$issues_coherence <- list()

          self$framework <- if (framework_type == "ana") {
            ANAFramework$new()
          } else {
            Framework$new()
          }
          private$..sync_state()

          phr_message(
            phr_txt("Protocol initialized."),
            origin = "Protocol$initialize"
          )
        },
        on_error = "abort",
        origin = "Protocol$initialize"
      )
      invisible(self)
    },

    #' @description Add a single Tool object to the protocol by specifying its type.
    #' A new tool of the requested type is instantiated (loading its bundled
    #' default XLSForm template) and stored in the \code{tools} named list under
    #' \code{tool_name} so it is accessible via \code{protocol$tools$<name>}.
    #' Call this method once per tool you wish to add.
    #' @param tool_type Character. Type of tool to create.  One of
    #'   \code{"household"}, \code{"key_informant"}, \code{"observation"}, or
    #'   \code{"generic"}.  Defaults to \code{"household"}.
    #' @param tool_name Optional character. Name/key for the tool in the
    #'   \code{tools} list.  When \code{NULL} a key is generated automatically
    #'   from \code{tool_type} and the current count (e.g. \code{"household_1"}).
    #' @return Invisibly returns self for method chaining.
    add_tools = function(tool_type = "household", tool_name = NULL) {
      phr_try(
        {
          valid_types <- self$valid_tool_types %||% character(0)
          phr_assert(
            tool_type %in% valid_types,
            message = phr_txt(
              "tool_type must be one of: {paste(valid_types, collapse=', ')}."
            ),
            origin = "Protocol$add_tools"
          )

          # Determine the key to use in the named tools list.
          if (is.null(tool_name) || !nzchar(tool_name)) {
            existing_keys <- names(self$tools)
            n_same_type <- sum(grepl(
              paste0("^", tool_type, "(_[0-9]+)?$"),
              existing_keys
            ))
            tool_name <- if (n_same_type == 0) {
              tool_type
            } else {
              paste0(tool_type, "_", n_same_type + 1L)
            }
          }

          tool <- switch(
            tool_type,
            "household" = HouseholdTool$new(name = tool_name),
            "key_informant" = KeyInformantTool$new(name = tool_name),
            "observation" = ObservationTool$new(name = tool_name),
            Tool$new(name = tool_name)
          )

          if (is.null(self$tools)) {
            self$tools <- list()
          }

          self$tools[[tool_name]] <- tool
          private$..sync_state()
          private$..touch()
          self$diagnose_coherence()
          phr_message(
            phr_txt("Tool of type '{tool_type}' added as '{tool_name}'."),
            origin = "Protocol$add_tools"
          )
        },
        on_error = "abort",
        origin = "Protocol$add_tools"
      )
      invisible(self)
    },

    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
    },

    # ── Tool helpers ────────────────────────────────────────────────────────

    #' @description Return the names of all currently registered tools.
    #' @return Character vector of tool names (keys of \code{self$tools}).
    #'   Empty character vector when no tools are registered.
    get_tool_names = function() {
      if (is.null(self$tools) || length(self$tools) == 0) {
        return(character(0))
      }
      names(self$tools)
    },

    #' @description Check whether a specific tool is registered.
    #'
    #' @param tool_name Character. Tool name to look up.
    #' @return \code{TRUE} if the tool is present in \code{self$tools},
    #'   \code{FALSE} otherwise.
    is_tool_included = function(tool_name) {
      if (is.null(self$tools) || length(self$tools) == 0) {
        return(FALSE)
      }
      isTRUE(tool_name %in% names(self$tools))
    },

    #' @description Validate an objective schema data frame.
    #'
    #' Validates that \code{schema} is a non-NULL data frame with at least one
    #' row, contains all required columns, and passes completeness and type
    #' checks.
    #'
    #' @param schema Data frame to validate as an objective schema.
    #' @param soft Logical. When \code{TRUE} issues warnings rather than errors
    #'   for recoverable problems.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{TRUE} if valid.
    validate_objective_schema = function(schema, soft = FALSE) {
      phr_try(
        {
          origin <- "Protocol$validate_objective_schema"

          if (is.null(schema) || !is.data.frame(schema)) {
            phr_error(
              origin = origin,
              message = phr_txt("Objective schema must be a data frame."),
              hint = phr_txt(
                "Use load_objective_schema() to obtain the default schema."
              )
            )
          }

          if (nrow(schema) == 0) {
            msg <- phr_txt("Objective schema is empty (zero rows).")
            if (soft) {
              phr_warning(origin = origin, message = msg)
              return(invisible(FALSE))
            }
            phr_error(origin = origin, message = msg)
          }

          missing_cols <- setdiff(
            .objective_schema_required_cols,
            names(schema)
          )
          if (length(missing_cols) > 0) {
            phr_error(
              origin = origin,
              message = phr_txt(
                glue::glue(
                  "Objective schema is missing required column(s): {paste(missing_cols, collapse = ', ')}"
                )
              ),
              hint = phr_txt(
                glue::glue(
                  "Required columns are: {paste(.objective_schema_required_cols, collapse = ', ')}"
                )
              )
            )
          }

          if (all(is.na(schema$sector))) {
            phr_error(
              origin = origin,
              message = phr_txt(
                "All 'sector' values in the objective schema are NA."
              )
            )
          }

          if (all(is.na(schema$short_objective))) {
            msg <- phr_txt(
              "All 'short_objective' values in the objective schema are NA."
            )
            if (soft) {
              phr_warning(origin = origin, message = msg)
            } else {
              phr_error(origin = origin, message = msg)
            }
          }

          char_cols <- c("sector", "pillar", "sub_pillar", "text_objective")
          bad_types <- char_cols[sapply(char_cols, function(col) {
            col %in%
              names(schema) &&
              !is.character(schema[[col]]) &&
              !is.factor(schema[[col]])
          })]
          if (length(bad_types) > 0) {
            msg <- phr_txt(
              glue::glue(
                "The following column(s) should be character (or factor): {paste(bad_types, collapse = ', ')}"
              )
            )
            if (soft) {
              phr_warning(origin = origin, message = msg)
            } else {
              phr_error(origin = origin, message = msg)
            }
          }

          invisible(TRUE)
        },
        on_error = "abort",
        origin = "Protocol$validate_objective_schema"
      )
    },

    #' @description Diagnose coherence between the \code{modified_objectives_schema}
    #' indicator codes and the \code{indicator_code} values across all tools in
    #' \code{self$tools} (using each tool's \code{revised_survey}).
    #'
    #' Checks performed:
    #' \enumerate{
    #'   \item Objectives in the legacy \code{objectives} list whose sectors are
    #'     not represented by any registered tool.
    #'   \item Objectives in \code{modified_objectives_schema} that have no
    #'     matching \code{indicator_code} in any tool's \code{revised_survey}.
    #'   \item \code{indicator_code} values present in tools but absent from
    #'     \code{modified_objectives_schema}.
    #' }
    #'
    #' Results are stored in \code{self$issues_coherence} as a named list.
    #' An empty list means no coherence issues were found.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    diagnose_coherence = function() {
      self$issues_coherence <- list()

      if (is.null(self$framework) || !inherits(self$framework, "Framework")) {
        self$issues_coherence$no_framework <-
          "No framework is associated with this protocol."
        return(invisible(self))
      }

      schema <- self$framework$modified_objectives_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        self$issues_coherence$no_schema <-
          "modified_objectives_schema is empty or not set in the framework."
        return(invisible(self))
      }

      has_obj_col <- "objective_code" %in% names(schema)
      has_ind_col <- "indicator_code" %in% names(schema)

      if (!has_obj_col || !has_ind_col) {
        self$issues_coherence$schema_columns <- paste0(
          "modified_objectives_schema must contain 'objective_code' and 'indicator_code' columns. ",
          "Found: ",
          paste(names(schema), collapse = ", ")
        )
        return(invisible(self))
      }

      schema_ind_codes <- as.character(schema$indicator_code)
      schema_ind_codes <- schema_ind_codes[
        !is.na(schema_ind_codes) & nzchar(schema_ind_codes)
      ]

      # Unique objective rows (objective_code + label for reporting)
      obj_col <- as.character(schema$objective_code)
      name_col <- if ("text_objective" %in% names(schema)) {
        as.character(schema$text_objective)
      } else {
        obj_col
      }
      obj_df <- unique(data.frame(
        objective_code = obj_col,
        objective_name = name_col,
        stringsAsFactors = FALSE
      ))
      obj_df <- obj_df[!is.na(obj_df$objective_code), , drop = FALSE]

      # Collect all indicator_codes from all tools' revised_survey
      tool_ind_codes <- character(0)
      if (!is.null(self$tools) && length(self$tools) > 0) {
        for (tool in self$tools) {
          sv <- tryCatch(
            {
              if (
                !is.null(tool$revised_survey) && nrow(tool$revised_survey) > 0
              ) {
                tool$revised_survey
              } else {
                tool$survey
              }
            },
            error = function(e) NULL
          )
          if (
            !is.null(sv) && is.data.frame(sv) && "indicator_code" %in% names(sv)
          ) {
            codes <- as.character(sv$indicator_code)
            codes <- codes[!is.na(codes) & nzchar(codes)]
            tool_ind_codes <- c(tool_ind_codes, codes)
          }
        }
        tool_ind_codes <- unique(tool_ind_codes)
      }

      # Check 1: objectives with no indicator coverage in tools
      obj_no_coverage <- list()
      for (i in seq_len(nrow(obj_df))) {
        obj_code <- obj_df$objective_code[i]
        obj_name <- obj_df$objective_name[i]
        obj_inds <- schema_ind_codes[schema$objective_code == obj_code]
        obj_inds <- obj_inds[!is.na(obj_inds) & nzchar(obj_inds)]
        if (length(obj_inds) == 0 || !any(obj_inds %in% tool_ind_codes)) {
          obj_no_coverage[[obj_code]] <- paste0(
            "Objective '",
            obj_code,
            "' (",
            obj_name,
            ") has no indicators ",
            "in any tool's revised_survey."
          )
        }
      }
      if (length(obj_no_coverage) > 0) {
        self$issues_coherence$objectives_without_indicators <- obj_no_coverage
      }

      # Check 2: tool indicators not matched to any objective in schema
      if (length(tool_ind_codes) > 0) {
        unmatched <- tool_ind_codes[!tool_ind_codes %in% schema_ind_codes]
        if (length(unmatched) > 0) {
          self$issues_coherence$tool_indicators_without_objectives <- paste0(
            "The following indicator_code(s) in tools have no match in ",
            "modified_objectives_schema: ",
            paste(unmatched, collapse = ", ")
          )
        }
      }

      if (length(self$issues_coherence) == 0) {
        phr_message(
          phr_txt(
            "Coherence validation passed: all objectives have tool coverage and all tool indicators match the schema."
          ),
          origin = "Protocol$diagnose_coherence"
        )
      } else {
        phr_message(
          phr_txt(
            "Coherence validation found {length(self$issues_coherence)} issue(s). Check self$issues_coherence for details."
          ),
          origin = "Protocol$diagnose_coherence"
        )
      }
      invisible(self)
    },

    #' @description Build a data analysis plan (DAP) table from framework primary
    #'   objectives and tool survey data.
    #'
    #' @param tool_name Character. Name of the tool to use for survey questions
    #'   and responses. Must match a key in \code{self$tools}.
    #' @param lang Character. Language code for labels (default: "en").
    #' @return Data frame with columns: Research Question, Indicator Code,
    #'   Indicator / Variable, Disaggregation, Questionnaire Question,
    #'   Questionnaire Responses, Data Collection Level. Returns \code{NULL} when
    #'   framework is not configured, no primary objectives are set, or the
    #'   specified tool is not found.
    get_dap_table = function(tool_name, lang = "en") {
      phr_try(
        {
          # Validate tool_name parameter
          phr_assert(
            is.character(tool_name) &&
              length(tool_name) == 1 &&
              nzchar(tool_name),
            message = phr_txt("tool_name must be a non-empty character string"),
            origin = "Protocol$get_dap_table"
          )

          # Build and return the DAP table
          private$..build_dap_table(tool_name, lang = lang)
        },
        on_error = "abort",
        origin = "Protocol$get_dap_table"
      )
    },
    #' @description Get Quarto parameters for rendering.
    #' @return A named list of parameters to pass to Quarto rendering.
    get_quarto_params = function() {
      params <- super$get_quarto_params()

      c(
        params,
        list(
          assessment_title = self$metadata$assessment_title %||% "",
          country_name = self$metadata$country_name %||% "",
          month_year = self$metadata$month_year %||% "",
          release_date = self$.release_date %||% Sys.Date(),

          research_cycle_id = self$metadata$research_cycle_id %||% "",
          country = self$metadata$country %||% "",
          release_date = self$metadata$release_date %||% Sys.Date(),
          version_number = self$metadata$version_number %||% "",
          type_emergency = self$metadata$type_emergency %||% "",
          type_crisis = self$metadata$type_crisis %||% "",
          mandating_agency = self$metadata$mandating_agency %||% "",
          project_code = self$metadata$project_code %||% "",
          geographic_coverage = self$metadata$geographic_coverage %||% "",
          population = self$metadata$population %||% "",
          rationale = self$metadata$rationale %||% "",
          date_pilot_training = self$metadata$date_pilot_training %||% "",
          date_data_collection_start = self$metadata$date_data_collection_start %||%
            "",
          date_data_collection_end = self$metadata$date_data_collection_end %||%
            "",
          date_data_analysis = self$metadata$date_data_analysis %||% "",
          date_data_validation = self$metadata$date_data_validation %||% "",
          date_preliminary_presentation = self$metadata$date_preliminary_presentation %||%
            "",
          date_outputs_validation = self$metadata$date_outputs_validation %||%
            "",
          date_outputs_publication = self$metadata$date_outputs_publication %||%
            "",
          date_final_presentation = self$metadata$date_final_presentation %||%
            "",
          audience_type_cluster = self$metadata$audience_type_cluster %||% "",
          expected_output_cluster = self$metadata$expected_output_cluster %||%
            "",
          expected_output_donor = self$metadata$expected_output_donor %||% "",
          expected_output_operational_actor = self$metadata$expected_output_operational_actor %||%
            "",
          expected_output_other = self$metadata$expected_output_other %||% "",
          dissemination_strategy_cluster = self$metadata$dissemination_strategy_cluster %||%
            "",
          dissemination_strategy_donor = self$metadata$dissemination_strategy_donor %||%
            "",
          dissemination_strategy_operational_actor = self$metadata$dissemination_strategy_operational_actor %||%
            "",
          dissemination_strategy_other = self$metadata$dissemination_strategy_other %||%
            "",
          access_cluster = self$metadata$access_cluster %||% "",
          access_donor = self$metadata$access_donor %||% "",
          access_operational_actor = self$metadata$access_operational_actor %||%
            "",
          access_other = self$metadata$access_other %||% "",
          visibility_cluster = self$metadata$visibility_cluster %||% "",
          visibility_donor = self$metadata$visibility_donor %||% "",
          visibility_operational_actor = self$metadata$visibility_operational_actor %||%
            "",
          visibility_other = self$metadata$visibility_other %||% "",
          created_date = self$metadata$created_date %||% NULL,
          modified_datetime = self$metadata$modified_datetime %||% NULL,
          month_year = self$metadata$month_year %||% NULL,
          country_name = self$metadata$country_name %||% NULL,
          assessment_title = self$metadata$assessment_title %||% NULL,
          target_strata = self$metadata$target_strata %||% list(),
          protocol_version = self$metadata$protocol_version %||% "1.0",
          version = self$metadata$version %||% 1L,
          mandating_body = self$metadata$mandating_body %||% NULL,
          project_code = self$metadata$project_code %||% NULL,
          overall_timeframe = self$metadata$overall_timeframe %||% NULL,
          pilot_date = self$metadata$pilot_date %||% NULL,
          data_start_date = self$metadata$data_start_date %||% NULL,
          data_end_date = self$metadata$data_end_date %||% NULL,
          analysis_date = self$metadata$analysis_date %||% NULL,
          data_validation_date = self$metadata$data_validation_date %||% NULL,
          prelim_presentation_date = self$metadata$prelim_presentation_date %||%
            NULL,
          output_validation_date = self$metadata$output_validation_date %||%
            NULL,
          output_published_date = self$metadata$output_published_date %||% NULL,
          final_presentation_date = self$metadata$final_presentation_date %||%
            NULL,
          date_milestone_donor = self$metadata$date_milestone_donor %||% NULL,
          date_milestone_intercluster = self$metadata$date_milestone_intercluster %||%
            NULL,
          date_milestone_cluster = self$metadata$date_milestone_cluster %||%
            NULL,
          date_milestone_ngo_platform = self$metadata$date_milestone_ngo_platform %||%
            NULL,
          date_milestone_other = self$metadata$date_milestone_other %||% NULL,
          geographic_coverage = self$metadata$geographic_coverage %||% NULL,
          stratification = self$metadata$stratification %||% NULL,
          num_report = self$metadata$num_report %||% NULL,
          num_profile = self$metadata$num_profile %||% NULL,
          num_prelim_presentation = self$metadata$num_prelim_presentation %||%
            NULL,
          num_final_presentation = self$metadata$num_final_presentation %||%
            NULL,
          num_factsheet = self$metadata$num_factsheet %||% NULL,
          num_dashboard = self$metadata$num_dashboard %||% NULL,
          num_webmap = self$metadata$num_webmap %||% NULL,
          num_map = self$metadata$num_map %||% NULL,
          num_output_other = self$metadata$num_output_other %||% NULL,
          objectives_research_questions_df = self$.objectives_research_questions_df,
          secondary_data_sources_df = self$.secondary_data_sources_df,
          modified_framework_svg = self$.modified_framework_svg
        )
      )
    }
  ),
  active = list(
    #' @field .release_date Active binding returning the current system date.
    #'   This binding is read-only; attempts to assign return \code{invisible(FALSE)}.
    .release_date = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }
      Sys.Date()
    },

    .objectives_research_questions_df = function(value) {
      all_codes <- character(0)

      tool_roles <- names(self$tools)

      for (role in tool_roles) {
        codes <- tryCatch(
          self$access_nested(
            field = "tools",
            role = role,
            member = "get_indicator_codes",
            update_modified = FALSE
          ),
          error = function(e) character(0)
        )

        codes <- trimws(as.character(codes))
        codes <- codes[!is.na(codes) & nzchar(codes)]

        all_codes <- c(all_codes, codes)
      }

      all_codes <- unique(all_codes)

      ob <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_objectives_schema",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )

      ib <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_indicator_bank",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(ob) || is.null(ib) || length(all_codes) == 0) {
        table <- data.frame(
          Pillar = character(0),
          `Sub-Pillar` = character(0),
          Objective = character(0),
          `Research Question` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(table)
      }

      ib_sub <- ib[ib$indicator_code %in% all_codes, , drop = FALSE]

      objective_codes <- unique(ib_sub$objective_code)
      objective_codes <- objective_codes[
        !is.na(objective_codes) & nzchar(objective_codes)
      ]

      ob_sub <- ob[ob$objective_code %in% objective_codes, , drop = FALSE]

      table <- unique(
        data.frame(
          Pillar = ob_sub$pillar,
          `Sub-Pillar` = ob_sub$sub_pillar,
          Objective = ob_sub$text_objective,
          `Research Question` = ob_sub$objective_research_question,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      )

      table
    },

    .secondary_data_sources_df = function(value) {
      if (!missing(value)) {
        return(invisible(FALSE))
      }

      table <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "secondary_data_sources",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )

      return(table)
    },
    #' @field .modified_framework_svg Active binding returning a temporary SVG
    #'   file path created from \code{framework$adjusted_svg}; falls back to
    #'   \code{framework$master_svg} when adjusted SVG is unavailable.
    #'   Returns \code{NULL} if no SVG text is available. This binding is read-only.
    .modified_framework_svg = function(value) {
      if (!missing(value)) {
        return(invisible(NULL))
      }

      svg_text <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "adjusted_svg",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )

      if (
        is.null(svg_text) || !is.character(svg_text) || !nzchar(svg_text[[1L]])
      ) {
        svg_text <- tryCatch(
          self$access_nested(
            field = "framework",
            member = "master_svg",
            update_modified = FALSE
          ),
          error = function(e) NULL
        )
      }

      if (
        is.null(svg_text) || !is.character(svg_text) || !nzchar(svg_text[[1L]])
      ) {
        return(NULL)
      }

      tmp_svg <- tempfile(fileext = ".svg")
      writeLines(svg_text[[1L]], con = tmp_svg)

      normalizePath(tmp_svg, winslash = "/", mustWork = TRUE)
    }
  ),

  private = list(
    #' @description Check whether a tool with a specific role exists.
    #'   Uses \code{access_nested()} to query tools by role and verify that
    #'   a tool with that role exists and has a valid name.
    #' @param role Character. Role identifier to check for tool availability.
    #' @return Logical. \code{TRUE} if a tool with the specified role exists
    #'   and has a valid name, \code{FALSE} otherwise.
    #' @keywords internal
    #' @noRd
    ..has_tool_role = function(role) {
      out <- tryCatch(
        self$access_nested(
          field = "tools",
          role = role,
          member = "get_name",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )
      is.character(out) && length(out) == 1L && nzchar(out)
    },

    #' @description Collect unique indicator codes from included revised tools.
    #' @param tool_names Optional character vector of tool names to query.
    #' @return Character vector of unique indicator codes.
    #' @keywords internal
    #' @noRd
    ..get_tool_indicator_codes = function(
      tool_names = NULL
    ) {
      selected <- self$get_tool_names()
      if (!is.null(tool_names)) {
        selected <- intersect(selected, as.character(tool_names))
      }
      if (length(selected) == 0L) {
        return(character(0))
      }

      out <- character(0)
      for (tn in selected) {
        tool_codes <- tryCatch(
          self$access_nested(
            field = "tools",
            name = tn,
            member = "get_indicator_codes",
            update_modified = FALSE
          ),
          error = function(e) character(0)
        )
        out <- c(out, as.character(tool_codes %||% character(0)))
      }
      unique(out[nzchar(out)])
    },
    # Load survey/choices/settings from an xlsx path into an existing Tool object.
    ..load_tool_from_path = function(tool, path) {
      if (!file.exists(path)) {
        return(invisible(NULL))
      }
      available_sheets <- tryCatch(
        readxl::excel_sheets(path),
        error = function(e) character(0)
      )
      if ("survey" %in% available_sheets) {
        sv <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "survey")),
          error = function(e) NULL
        )
        if (!is.null(sv)) {
          tool$survey <- sv
          tool$revised_survey <- sv
        }
      }
      if ("choices" %in% available_sheets) {
        ch <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "choices")),
          error = function(e) NULL
        )
        if (!is.null(ch)) {
          tool$choices <- ch
          tool$revised_choices <- ch
        }
      }
      if ("settings" %in% available_sheets) {
        st <- tryCatch(
          as.data.frame(readxl::read_excel(path, sheet = "settings")),
          error = function(e) NULL
        )
        if (!is.null(st)) {
          tool$settings <- st
          tool$revised_settings <- st
        }
      }
      invisible(NULL)
    },

    #' @description Build a data analysis plan table from framework primary
    #'   objectives and specified tool survey data.
    #' @param tool_name Character. Tool name for survey/choices lookup.
    #' @return Data frame with DAP columns or NULL when missing dependencies.
    #' @keywords internal
    #' @noRd
    ..build_dap_table = function(tool_name, lang = "en") {
      # 1. Check framework availability
      if (is.null(self$framework) || !inherits(self$framework, "Framework")) {
        return(NULL)
      }

      # 2. Get specified tool's revised survey
      revised_survey <- tryCatch(
        self$access_nested(
          field = "tools",
          name = tool_name,
          member = "revised_survey"
        ),
        error = function(e) NULL
      )
      if (
        is.null(revised_survey) ||
          !is.data.frame(revised_survey) ||
          nrow(revised_survey) == 0L
      ) {
        phr_warning(
          phr_txt("Tool '{tool_name}' has no revised_survey data."),
          origin = "Protocol$get_dap_table"
        )
        return(NULL)
      }

      # 3. Get specified tool's revised choices
      revised_choices <- tryCatch(
        self$access_nested(
          field = "tools",
          name = tool_name,
          member = "revised_choices"
        ),
        error = function(e) NULL
      )

      if (!"indicator_code" %in% names(revised_survey)) {
        phr_warning(
          phr_txt("Tool '{tool_name}' survey has no indicator_code column."),
          origin = "Protocol$get_dap_table"
        )
        return(NULL)
      }

      # Extract unique indicator codes from survey
      survey_indicator_codes <- unique(trimws(unlist(
        strsplit(
          as.character(revised_survey$indicator_code[
            !is.na(revised_survey$indicator_code)
          ]),
          ","
        )
      )))

      survey_indicator_codes <- survey_indicator_codes[
        nzchar(survey_indicator_codes) & !grepl("00$", survey_indicator_codes)
      ]

      if (length(survey_indicator_codes) == 0L) {
        return(NULL)
      }

      # 4. Get master indicator bank

      indicator_bank <- tryCatch(
        self$access_nested(
          field = "framework",
          member = "master_indicator_bank",
          update_modified = FALSE
        ),
        error = function(e) NULL
      )

      if (
        is.null(indicator_bank) ||
          !is.data.frame(indicator_bank) ||
          nrow(indicator_bank) == 0L
      ) {
        return(NULL)
      }
      if (!"indicator_code" %in% names(indicator_bank)) {
        return(NULL)
      }

      # Extract indicator codes from both indicator_code and dep_indicator_code columns
      indicator_codes <- unique(as.character(indicator_bank$indicator_code))
      indicator_codes <- indicator_codes[
        !is.na(indicator_codes) & nzchar(indicator_codes)
      ]

      # Also include dep_indicator_code if the column exists
      if ("dep_indicator_code" %in% names(indicator_bank)) {
        dep_indicator_codes <- unique(as.character(
          indicator_bank$dep_indicator_code
        ))
        dep_indicator_codes <- dep_indicator_codes[
          !is.na(dep_indicator_codes) & nzchar(dep_indicator_codes)
        ]
        indicator_codes <- unique(c(indicator_codes, dep_indicator_codes))
      }

      if (length(indicator_codes) == 0L) {
        return(NULL)
      }

      # Drop threshold-specific columns if present
      drop_cols <- c(
        "threshold_name",
        "threshold_value",
        "citation_threshold"
      )

      indicator_bank <- indicator_bank[,
        setdiff(names(indicator_bank), drop_cols),
        drop = FALSE
      ]

      # Remove duplicated rows created by threshold variants
      indicator_bank <- unique(indicator_bank)

      # Filter indicator bank to indicators actually used in survey
      # Check both indicator_code and dep_indicator_code columns
      keep_rows <- indicator_bank$indicator_code %in% survey_indicator_codes
      if ("dep_indicator_code" %in% names(indicator_bank)) {
        keep_rows <- keep_rows |
          indicator_bank$dep_indicator_code %in% survey_indicator_codes
      }

      indicator_bank_filtered <- indicator_bank[keep_rows, , drop = FALSE]

      if (nrow(indicator_bank_filtered) == 0L) {
        return(NULL)
      }

      lang <- tolower(trimws(as.character(lang)))
      if (!lang %in% c("en", "fr", "es", "ar")) {
        phr_warning(
          phr_txt("Invalid lang '{lang}' specified; defaulting to 'en'."),
          origin = "Protocol$get_dap_table"
        )
        lang <- "en"
      }

      # 9. Build the DAP table
      private$..construct_dap_rows(
        survey_df = revised_survey,
        choices_df = revised_choices,
        indicator_bank = indicator_bank_filtered,
        lang = lang
      )
    },

    #' @description Construct DAP table rows from survey and framework data.
    #' @param survey_df Filtered survey data frame.
    #' @param choices_df Choices data frame (may be NULL).
    #' @param indicator_bank Data frame from the master indicator bank, filtered
    #'   to indicators used in the survey.
    #' @param lang Language code for label columns (e.g. \code{"en"}, \code{"fr"}).
    #' @return Data frame with DAP structure, without an indicator_code column,
    #'   or \code{NULL} when no rows can be constructed.
    #' @keywords internal
    #' @noRd
    ..construct_dap_rows = function(
      survey_df,
      choices_df,
      indicator_bank,
      lang
    ) {
      rows <- list()

      for (i in seq_len(nrow(survey_df))) {
        row <- survey_df[i, , drop = FALSE]

        # Skip calculate question types
        qtype <- if ("type" %in% names(row)) {
          tolower(trimws(as.character(row$type)))
        } else {
          ""
        }
        if (qtype == "calculate") {
          next
        }

        row_ind_codes <- as.character(row$indicator_code)
        if (is.na(row_ind_codes) || !nzchar(row_ind_codes)) {
          next
        }

        row_ind_codes <- trimws(unlist(strsplit(row_ind_codes, ",")))
        if (length(row_ind_codes) == 0L) {
          next
        }

        # Use first code that has a match in indicator_bank; fall back to first code
        matched_code <- row_ind_codes[
          row_ind_codes %in% indicator_bank$indicator_code
        ]
        ind_code <- if (length(matched_code) > 0L) {
          matched_code[1L]
        } else {
          row_ind_codes[1L]
        }

        # Skip rows whose indicator_code ends in "00"
        if (grepl("00$", ind_code)) {
          next
        }

        # Lookup the matching row in indicator_bank by indicator_code
        ib_row <- indicator_bank[
          indicator_bank$indicator_code == ind_code,
          ,
          drop = FALSE
        ]

        # Also check dep_indicator_code when no direct match found
        if (
          nrow(ib_row) == 0L &&
            "dep_indicator_code" %in% names(indicator_bank)
        ) {
          ib_row <- indicator_bank[
            !is.na(indicator_bank$dep_indicator_code) &
              indicator_bank$dep_indicator_code == ind_code,
            ,
            drop = FALSE
          ]
        }

        # Extract scalar fields from the matched indicator_bank row (first row if multiple)
        get_ib_field <- function(field) {
          if (nrow(ib_row) == 0L || !field %in% names(ib_row)) {
            return("")
          }
          val <- as.character(ib_row[[field]][1L])
          if (is.na(val)) "" else val
        }

        research_q <- get_ib_field("research_question")
        disagg_q <- get_ib_field("disaggregation")
        ind_name_q <- get_ib_field("indicator_name")
        dc_level_q <- get_ib_field("data_collection_level")

        # Skip rows with no Research Question value
        if (is.na(research_q) || !nzchar(trimws(research_q))) {
          next
        }

        # Resolve question label by language
        question_label <- if (
          lang == "en" && "label::English" %in% names(row)
        ) {
          as.character(row[["label::English"]])
        } else if (lang == "fr" && "label::French" %in% names(row)) {
          as.character(row[["label::French"]])
        } else if (lang == "es" && "label::Spanish" %in% names(row)) {
          as.character(row[["label::Spanish"]])
        } else if (lang == "ar" && "label::Arabic" %in% names(row)) {
          as.character(row[["label::Arabic"]])
        } else if ("label" %in% names(row)) {
          as.character(row$label)
        } else {
          ""
        }

        # Skip rows where the resolved question label is NA or empty
        if (is.na(question_label) || !nzchar(trimws(question_label))) {
          next
        }

        responses <- private$..extract_question_responses(
          row,
          choices_df,
          lang = lang
        )

        rows[[length(rows) + 1L]] <- data.frame(
          `Research Question` = research_q,
          `Indicator / Variable` = ind_name_q,
          Disaggregation = disagg_q,
          `Questionnaire Question` = question_label,
          `Questionnaire Responses` = responses,
          `Data Collection Level` = dc_level_q,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }

      if (length(rows) == 0L) {
        return(NULL)
      }

      do.call(rbind, rows)
    },

    #' @description Extract formatted response options for a survey question.
    #' @param question_row Single-row survey data frame
    #' @param choices_df Choices data frame (may be NULL)
    #' @param lang Language code (default: "en")
    #' @return Character string with responses (newline-separated for select types)
    #' @keywords internal
    #' @noRd
    ..extract_question_responses = function(
      question_row,
      choices_df,
      lang = "en"
    ) {
      qtype <- if ("type" %in% names(question_row)) {
        tolower(trimws(as.character(question_row$type)))
      } else {
        ""
      }

      # For select questions, extract from choices
      if (grepl("^select_one ", qtype) || grepl("^select_multiple ", qtype)) {
        list_name <- sub("^select_(one|multiple)\\s+", "", qtype)
        list_name <- gsub("\\s+.*$", "", list_name) # Remove anything after list name

        if (!is.null(choices_df) && "list_name" %in% names(choices_df)) {
          list_choices <- choices_df[
            choices_df$list_name == list_name,
            ,
            drop = FALSE
          ]
          if (nrow(list_choices) > 0L) {
            # Try label::English first, then label
            label_col <- if (
              lang == "en" && "label::English" %in% names(list_choices)
            ) {
              "label::English"
            } else if (
              lang == "fr" && "label::French" %in% names(list_choices)
            ) {
              "label::French"
            } else if (
              lang == "es" && "label::Spanish" %in% names(list_choices)
            ) {
              "label::Spanish"
            } else if (
              lang == "ar" && "label::Arabic" %in% names(list_choices)
            ) {
              "label::Arabic"
            } else if ("label" %in% names(list_choices)) {
              "label"
            } else {
              NULL
            }

            if (!is.null(label_col)) {
              labels <- as.character(list_choices[[label_col]])
              labels <- labels[!is.na(labels) & nzchar(labels)]
              if (length(labels) > 0L) {
                return(paste(labels, collapse = "\n")) # Join with newline per user instruction
              }
            }
          }
        }
        return("See choices")
      }

      # For integer/decimal, return "Enter number"
      if (qtype %in% c("integer", "decimal")) {
        return("Enter number")
      }

      # For date types
      if (grepl("date", qtype)) {
        return("Date (DD/MM/YYYY)")
      }

      # For text types
      if (qtype %in% c("text", "note")) {
        return("Text response")
      }

      # Default blank
      return("")
    }
  )
)
