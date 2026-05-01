#' Protocol R6 Class
#'
#' @description
#' Base class for managing protocol pipeline workflows.
#' Handles core components shared across all protocol types:
#' 1. Objective Selection (managed through the associated \code{\link{Framework}} object)
#' 2. Tool and Indicator Selection
#'
#' For survey protocols that require strata definition, sample size
#' calculations, sampling frame management, and sample drawing, use
#' the \code{\link{SurveyProtocol}} subclass instead.
#'
#' @importFrom R6 R6Class
#' @export
Protocol <- R6::R6Class(
  "Protocol",
  public = list(
    #' @field objectives Nested list of research objectives (legacy field).
    #'   Objectives are now primarily managed through the associated
    #'   \code{\link{Framework}} object via its \code{adjusted_schema}.
    #'   Use \code{objectives_to_df()} or \code{flatten_objectives()} to inspect.
    objectives = NULL,

    #' @field framework A \code{\link{Framework}} object (e.g.
    #'   \code{\link{ANAFramework}}) that holds the master and adjusted
    #'   reference schemas and SVG diagrams for this protocol.  Assign a
    #'   Framework instance to this field to associate a conceptual framework
    #'   with the protocol.
    framework = NULL,

    #' @field objective_schema Data frame containing the objective schema.
    #'   This field is no longer auto-populated on initialisation.  When a
    #'   \code{\link{Framework}} is associated with this protocol, access the
    #'   schema via \code{self$framework$master_schema} or
    #'   \code{self$framework$adjusted_schema}.  \code{objective_schema} may
    #'   still be set manually for custom schemas that do not require a full
    #'   Framework object.
    objective_schema = NULL,

    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,

    #' @field selected_indicators List of selected indicators
    selected_indicators = NULL,

    #' @field issues List of validation issues and discrepancies
    issues = list(),

    #' @field metadata List containing protocol metadata
    metadata = list(
      created_date = NULL,
      modified_date = NULL,
      month_year = NULL,
      country_name = NULL,
      assessment_title = NULL,
      target_strata = list(),
      protocol_version = "1.0"
    ),

    #' @description
    #' Creates a new Protocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  Must be
    #'   one of \code{"none"} (creates a generic \code{\link{Framework}} object) or
    #'   \code{"ana"} (creates an \code{\link{ANAFramework}} object).
    #' @return A new Protocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none") {
      phr_try({
        valid_fw_types <- c("none", "ana")
        phr_assert(
          is.character(framework_type) && length(framework_type) == 1 &&
            framework_type %in% valid_fw_types,
          message = phr_txt(
            "framework_type must be one of: {paste(valid_fw_types, collapse=', ')}."
          ),
          origin = "Protocol$initialize"
        )
        self$metadata$created_date <- Sys.time()
        self$metadata$modified_date <- Sys.time()
        self$metadata$assessment_title <- assessment_title
        self$metadata$country_name <- country_name
        self$metadata$month_year <- month_year
        self$objectives <- list()
        self$tools <- list()
        self$issues <- list()

        self$framework <- if (framework_type == "ana") {
          ANAFramework$new()
        } else {
          Framework$new()
        }

        phr_message(phr_txt("Protocol initialized."), origin = "Protocol$initialize")
      }, on_error = "abort", origin = "Protocol$initialize")
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
      phr_try({
        valid_types <- c("household", "key_informant", "observation", "generic")
        phr_assert(
          tool_type %in% valid_types,
          message = phr_txt("tool_type must be one of: {paste(valid_types, collapse=', ')}."),
          origin  = "Protocol$add_tools"
        )

        # Determine the key to use in the named tools list.
        if (is.null(tool_name) || !nzchar(tool_name)) {
          existing_keys <- names(self$tools)
          n_same_type   <- sum(grepl(paste0("^", tool_type, "(_[0-9]+)?$"), existing_keys))
          tool_name     <- if (n_same_type == 0) tool_type else paste0(tool_type, "_", n_same_type + 1L)
        }

        tool <- switch(
          tool_type,
          "household"     = HouseholdTool$new(name = tool_name),
          "key_informant" = KeyInformantTool$new(name = tool_name),
          "observation"   = ObservationTool$new(name = tool_name),
          Tool$new(name = tool_name)
        )

        if (is.null(self$tools)) {
          self$tools <- list()
        }

        self$tools[[tool_name]] <- tool
        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Tool of type '{tool_type}' added as '{tool_name}'."),
          origin = "Protocol$add_tools"
        )
      }, on_error = "abort", origin = "Protocol$add_tools")
      invisible(self)
    },
    
    #' @description Select indicators for data collection
    #' @param indicator_list List of indicators
    select_indicators = function(indicator_list) {
      self$selected_indicators <- indicator_list
      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
    },
    
    #' @description Get protocol summary
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      list(
        assessment_title = self$metadata$assessment_title,
        country_name = self$metadata$country_name,
        month_year = self$metadata$month_year,
        created = self$metadata$created_date,
        modified = self$metadata$modified_date,
        num_objectives = count_objectives(self$objectives),
        num_tools = length(self$tools),
        num_issues = length(self$issues)
      )
    },
    
    #' @description Export protocol to a list
    #' @return List containing all protocol data
    export_protocol = function() {
      list(
        metadata = self$metadata,
        objectives = self$objectives,
        objective_schema = self$objective_schema,
        framework = if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
          self$framework$export_framework()
        } else {
          NULL
        },
        tools = self$tools,
        selected_indicators = self$selected_indicators,
        issues = self$issues,
        summary = self$get_protocol_summary()
      )
    },

    #' @description Validate an objective schema data frame.
    #'
    #' Convenience method that delegates to the standalone
    #' \code{\link{validate_objective_schema}} function.  Useful for validating
    #' custom schemas before associating them with this protocol.
    #'
    #' @param schema Data frame to validate as an objective schema.
    #' @param soft Logical. When \code{TRUE} issues warnings rather than errors
    #'   for recoverable problems.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{TRUE} if valid.
    validate_objective_schema = function(schema, soft = FALSE) {
      validate_objective_schema(schema, soft = soft)
    },

    #' @description Generate a Word document report based on the REACH TOR template
    #'
    #' Creates a \code{.docx} file based on the bundled REACH Terms of Reference
    #' template (\code{inst/resources/reach_tor_template.docx}).  The document
    #' inherits the template's formatting, branding, section structure (Executive
    #' Summary, Rationale, Methodology, DAP, etc.) and key placeholder strings are
    #' replaced with protocol metadata.  A new \strong{Protocol Data} section is
    #' inserted directly after the Executive Summary table and contains:
    #' \itemize{
    #'   \item Research Objectives table
    #'   \item Data Collection Tools (one sub-section per tool with indicator list)
    #'   \item Sampling Design (base Protocol: placeholder; SurveyProtocol: full section)
    #' }
    #'
    #' @param output_file Character. Output file path including the \code{.docx}
    #'   extension.  Defaults to \code{"protocol_report.docx"} in the current
    #'   working directory.
    #' @param reference_docx Character or \code{NULL}.  Path to a custom
    #'   \code{.docx} template.  When \code{NULL} (default) the package-bundled
    #'   REACH TOR template is used.
    #' @param open Logical.  Whether to open the generated file after writing.
    #'   Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    generate_reach_tor = function(output_file = "protocol_report.docx",
                                  reference_docx = NULL,
                                  open = FALSE) {
      phr_try({
        doc <- private$create_base_doc(reference_docx)
        doc <- private$add_metadata_section(doc)
        doc <- private$add_objectives_section(doc)
        doc <- private$add_tools_section(doc)
        doc <- private$add_sampling_section(doc)

        print(doc, target = output_file)
        phr_message(
          phr_txt("Protocol report saved to: {output_file}"),
          origin = "Protocol$generate_reach_tor"
        )
        if (isTRUE(open)) utils::browseURL(output_file)
      }, on_error = "abort", origin = "Protocol$generate_reach_tor")
      invisible(self)
    }
  ),

  private = list(
    # Check for issues and discrepancies in the protocol
    check_issues = function() {
      self$issues <- list()
      
      # Check if objectives have matching indicators in tools
      all_objectives <- flatten_objectives(self$objectives)
      if (length(all_objectives) > 0 && length(self$tools) > 0) {
        obj_sectors <- unique(sapply(all_objectives, function(x) x$sector))
        
        # Placeholder: Check tool coverage (actual Tool class will define how to extract sectors)
        tool_sectors <- character(0)
        tryCatch({
          tool_sectors <- unique(sapply(self$tools, function(x) {
            if (is.list(x) && "sector" %in% names(x)) {
              return(x$sector)
            } else if (methods::is(x, "R6") && "sector" %in% names(x)) {
              return(x$sector)
            }
            return(NA_character_)
          }))
          tool_sectors <- tool_sectors[!is.na(tool_sectors)]
        }, error = function(e) {
          # Ignore extraction errors
        })
        
        missing_sectors <- setdiff(obj_sectors, tool_sectors)
        if (length(missing_sectors) > 0) {
          self$issues$tool_coverage <- paste(
            "Objectives require sectors not covered by tools:",
            paste(missing_sectors, collapse = ", ")
          )
        }
      }
      
      invisible(self)
    },

    # Create an officer doc using the REACH TOR template when available, or blank.
    create_base_doc = function(reference_docx = NULL) {
      # Caller-supplied path takes highest priority
      if (!is.null(reference_docx) && file.exists(reference_docx)) {
        return(officer::read_docx(reference_docx))
      }
      # Try the REACH TOR-based template first
      reach_path <- system.file("resources", "reach_tor_template.docx", package = "phr")
      if (nzchar(reach_path)) {
        return(officer::read_docx(reach_path))
      }
      # Fall back to the older protocol report template
      sys_path <- system.file("resources", "protocol_report_template.docx", package = "phr")
      if (nzchar(sys_path)) {
        return(officer::read_docx(sys_path))
      }
      # No template found — use a blank Word document with default styles
      officer::read_docx()
    },

    # Replace REACH TOR template placeholders with actual metadata, then
    # insert a "Protocol Data" section heading before the "Rationale" heading
    # so that all protocol-specific content sits directly after the Executive
    # Summary table in the finished document.
    add_metadata_section = function(doc) {
      title   <- self$metadata$assessment_title %||% "Protocol Report"
      country <- self$metadata$country_name     %||% ""
      period  <- self$metadata$month_year       %||% ""
      version <- self$metadata$protocol_version %||% "1.0"
      date_str <- format(Sys.Date(), "%d/%m/%Y")

      # Build objective summary text for the General Objective / Specific
      # Objectives / Research Questions cells in the Executive Summary table.
      all_objectives <- flatten_objectives(self$objectives)

      gen_obj_text <- if (length(all_objectives) > 0) {
        all_objectives[[1]]$text_objective %||% title
      } else {
        title
      }

      spec_obj_text <- if (length(all_objectives) > 0) {
        paste(vapply(seq_along(all_objectives), function(i) {
          obj <- all_objectives[[i]]
          paste0(i, ". ", obj$text_objective %||% "")
        }, character(1)), collapse = "; ")
      } else {
        ""
      }

      rq_text <- if (length(all_objectives) > 0) {
        paste(vapply(seq_along(all_objectives), function(i) {
          obj <- all_objectives[[i]]
          paste0(i, ". ", obj$short_objective %||% "")
        }, character(1)), collapse = "; ")
      } else {
        ""
      }

      # Placeholder → replacement pairs (REACH TOR template placeholders).
      # Single-run placeholders (reliable cross-run replacements marked below).
      replace_pairs <- list(
        c("[Research Cycle title]", title),
        c("[Country]",              country),
        c("[Release date]",         date_str),
        c("[Version number]",       paste0("v", version))
      )
      if (nzchar(country)) {
        replace_pairs <- c(replace_pairs, list(
          c("[Specify name(s) here]", country),
          c("[Describe here the geographic area that the research aims to provide findings about]",
            country)
        ))
      }
      if (nzchar(gen_obj_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[Describe here what this research aims to inform \u2013",
                   " for specific guidance on this, see Step 1.2 of the",
                   " IMPACT Research Design Guidelines]"),
            gen_obj_text)
        ))
      }
      if (nzchar(spec_obj_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[List here what the research aims to identify to facilitate",
                   " the general objective \u2013 for specific guidance on this,",
                   " see Step 1.2 of the IMPACT Research Design Guidelines]"),
            spec_obj_text)
        ))
      }
      if (nzchar(rq_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[List here the research questions that will need to be answered",
                   " to meet the objectives\u2013 for specific guidance on this,",
                   " see Step 2.1 of the IMPACT Research Design Guidelines]"),
            rq_text)
        ))
      }

      for (rp in replace_pairs) {
        tryCatch(
          doc <- officer::body_replace_all_text(doc,
                                                old_value = rp[1],
                                                new_value = rp[2],
                                                fixed     = TRUE),
          error = function(e) {
            phr_warning(
              phr_txt("Placeholder replacement failed for '{rp[1]}': {conditionMessage(e)}"),
              origin = "Protocol$add_metadata_section"
            )
          }
        )
      }

      # Navigate to the "Rationale" heading (present in the REACH TOR template)
      # and insert the "Protocol Data" section heading immediately before it.
      # This places all protocol-specific content directly after the Executive
      # Summary table.  Falls back to appending at the end of the document when
      # "Rationale" is absent (e.g. a custom template without that section).
      # NOTE: subsequent section methods (add_objectives_section, add_tools_section,
      # add_sampling_section) rely on the cursor being positioned at the last
      # inserted paragraph so they can continue appending with pos = "after".
      meta_line <- paste(
        Filter(nzchar, c(
          if (nzchar(country)) paste0("Country: ", country),
          if (nzchar(period))  paste0("Period: ",  period),
          paste0("Generated: ", format(Sys.Date(), "%d %B %Y")),
          paste0("Version: ", version)
        )),
        collapse = "  |  "
      )

      doc <- tryCatch({
        doc <- officer::cursor_begin(doc)
        doc <- officer::cursor_reach(doc, keyword = "Rationale")
        doc <- officer::body_add_par(doc, "Protocol Data", style = "heading 1", pos = "before")
        doc <- officer::body_add_par(doc, meta_line, style = "Normal", pos = "after")
        doc
      }, error = function(e) {
        # Fallback when "Rationale" section is absent (e.g. custom template).
        phr_warning(
          phr_txt("Could not find 'Rationale' heading in template; appending Protocol Data at end of document."),
          origin = "Protocol$add_metadata_section"
        )
        doc <- officer::cursor_end(doc)
        doc <- officer::body_add_par(doc, "Protocol Data", style = "heading 1")
        doc <- officer::body_add_par(doc, meta_line, style = "Normal")
        doc
      })

      doc
    },

    # Add the research objectives section after the current cursor position.
    # When a Framework is attached and has an adjusted_schema, that is used as
    # the source of objectives.  Falls back to the legacy self$objectives list.
    add_objectives_section = function(doc) {
      doc <- officer::body_add_par(doc, "Research Objectives", style = "heading 2", pos = "after")

      # Prefer framework adjusted_schema when available
      obj_df <- NULL
      if (!is.null(self$framework) && inherits(self$framework, "Framework") &&
          !is.null(self$framework$adjusted_schema) &&
          is.data.frame(self$framework$adjusted_schema) &&
          nrow(self$framework$adjusted_schema) > 0) {
        schema <- self$framework$adjusted_schema
        cols_map <- list(
          Sector       = "sector",
          Pillar       = "pillar",
          "Sub-Pillar" = "sub_pillar",
          ID           = "short_objective",
          Objective    = "text_objective"
        )
        obj_df <- as.data.frame(
          lapply(cols_map, function(col) {
            if (col %in% names(schema)) as.character(schema[[col]]) else rep("", nrow(schema))
          }),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        all_objectives <- flatten_objectives(self$objectives)
        if (length(all_objectives) > 0) {
          obj_df <- do.call(rbind, lapply(all_objectives, function(x) {
            data.frame(
              Sector        = as.character(x$sector          %||% ""),
              Pillar        = as.character(x$pillar          %||% ""),
              "Sub-Pillar"  = as.character(x$sub_pillar      %||% ""),
              ID            = as.character(x$short_objective %||% ""),
              Objective     = as.character(x$text_objective  %||% ""),
              "Data Source" = as.character(x$data_source     %||% "primary"),
              check.names   = FALSE,
              stringsAsFactors = FALSE
            )
          }))
        }
      }

      if (is.null(obj_df) || nrow(obj_df) == 0) {
        return(officer::body_add_par(
          doc, "No objectives have been defined.", style = "Normal", pos = "after"
        ))
      }

      ft <- flextable::flextable(obj_df)
      ft <- flextable::theme_zebra(ft)
      ft <- flextable::autofit(ft)
      flextable::body_add_flextable(doc, ft, pos = "after")
    },

    # Add the data collection tools section after the current cursor position.
    # Skipped (with a placeholder note) when self$tools is empty.
    add_tools_section = function(doc) {
      doc <- officer::body_add_par(doc, "Data Collection Tools", style = "heading 2", pos = "after")

      if (length(self$tools) == 0) {
        return(officer::body_add_par(
          doc, "No data collection tools have been defined.", style = "Normal", pos = "after"
        ))
      }

      for (i in seq_along(self$tools)) {
        tool <- self$tools[[i]]

        if (methods::is(tool, "R6")) {
          tool_name  <- tryCatch(tool$get_name(),      error = function(e) {
            phr_warning(phr_txt("Could not read name for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            paste("Tool", i)
          })
          tool_type  <- tryCatch(tool$get_tool_type(), error = function(e) {
            phr_warning(phr_txt("Could not read type for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            "unknown"
          })
          survey     <- tryCatch(tool$get_survey(),    error = function(e) {
            phr_warning(phr_txt("Could not read survey sheet for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            NULL
          })
          indicators <- private$extract_indicators_from_survey(survey)
        } else {
          tool_name  <- tool$tool_name  %||% paste("Tool", i)
          tool_type  <- tool$tool_type  %||% "unknown"
          indicators <- tool$indicators %||% character(0)
        }

        doc <- officer::body_add_par(
          doc,
          paste0(i, ". ", tool_name, " (", tool_type, ")"),
          style = "heading 3",
          pos   = "after"
        )

        if (length(indicators) > 0) {
          ind_df <- data.frame(Indicator = as.character(indicators), stringsAsFactors = FALSE)
          ft <- flextable::flextable(ind_df)
          ft <- flextable::autofit(ft)
          doc <- flextable::body_add_flextable(doc, ft, pos = "after")
        } else {
          doc <- officer::body_add_par(
            doc, "No indicators defined for this tool.", style = "Normal", pos = "after"
          )
        }
      }

      doc
    },

    # Add a sampling design section (placeholder for base Protocol class).
    # SurveyProtocol overrides this with a full section.
    add_sampling_section = function(doc) {
      doc <- officer::body_add_par(doc, "Sampling Design", style = "heading 2", pos = "after")
      officer::body_add_par(
        doc,
        "No sampling information available for this protocol type.",
        style = "Normal",
        pos   = "after"
      )
    },

    # Extract unique non-NA indicator names from an XLSForm survey data frame.
    # Returns an empty character vector when the survey is absent or has no
    # 'name' column.
    extract_indicators_from_survey = function(survey) {
      if (!is.null(survey) && is.data.frame(survey) &&
          nrow(survey) > 0 && "name" %in% names(survey)) {
        unique(stats::na.omit(survey$name))
      } else {
        character(0)
      }
    }
  )
)
