# Internal helper: parse an SVG string and return a named list that maps each
# objective_code (as a character string) to a character vector of SVG <g> group
# ids that carry that code as a text label.
#
# Three label formats are recognised:
#   1. Leaf sub-pillar blocks whose <text> element contains "(CODE)" in
#      parentheses, e.g. "HH Consump. (112)".
#   2. Parent blocks whose second <text> element is exactly "OC: CODE",
#      e.g. "OC: 105".
#   3. Parent blocks with a code range "OC: CODE1-CODE2",
#      e.g. "OC: 152-154" — all codes in the range are mapped.
#
# The function works by splitting the SVG markup on </g> so that each
# chunk contains at most one <g id="..."> opening tag. It never reads
# It never reads the reference files and never uses the sub_pillar column.
.build_code_svg_map <- function(svg) {
  code_map <- list() # named list: code_str -> character vector of group ids

  chunks <- strsplit(svg, "</g>", fixed = TRUE)[[1]]

  for (chunk in chunks) {
    id_match <- regmatches(
      chunk,
      regexpr('<g id="([^"]+)"', chunk, perl = TRUE)
    )
    if (length(id_match) == 0L || !nzchar(id_match)) {
      next
    }
    id_val <- sub('<g id="([^"]+)"', "\\1", id_match, perl = TRUE)

    # Pattern 1: leaf node – text contains "(CODE)"
    leaf_match <- regmatches(chunk, regexpr("\\((\\d+)\\)", chunk, perl = TRUE))
    if (length(leaf_match) > 0L && nzchar(leaf_match)) {
      code_str <- sub("\\((\\d+)\\)", "\\1", leaf_match, perl = TRUE)
      code_map[[code_str]] <- c(code_map[[code_str]], id_val)
      next
    }

    # Pattern 2: range "OC: N-M"
    range_match <- regmatches(
      chunk,
      regexpr("OC:\\s*(\\d+)-(\\d+)", chunk, perl = TRUE)
    )
    if (length(range_match) > 0L && nzchar(range_match)) {
      from_c <- as.integer(sub(
        "OC:\\s*(\\d+)-(\\d+)",
        "\\1",
        range_match,
        perl = TRUE
      ))
      to_c <- as.integer(sub(
        "OC:\\s*(\\d+)-(\\d+)",
        "\\2",
        range_match,
        perl = TRUE
      ))
      for (c in seq(from_c, to_c)) {
        code_str <- as.character(c)
        code_map[[code_str]] <- c(code_map[[code_str]], id_val)
      }
      next
    }

    # Pattern 3: single "OC: N"
    oc_match <- regmatches(chunk, regexpr("OC:\\s*(\\d+)", chunk, perl = TRUE))
    if (length(oc_match) > 0L && nzchar(oc_match)) {
      code_str <- sub("OC:\\s*(\\d+)", "\\1", oc_match, perl = TRUE)
      code_map[[code_str]] <- c(code_map[[code_str]], id_val)
    }
  }

  code_map
}


#' Framework R6 Class
#'
#' @description
#' Base class for conceptual frameworks used in protocol planning.
#' A Framework holds a master objectives schema (all objectives), a modified
#' objectives schema filtered to the currently selected objectives, a master
#' indicator bank (all indicators), a modified indicator bank filtered to
#' selected objective codes, a master SVG diagram representing the full
#' conceptual framework, and an adjusted SVG diagram derived from the master
#' and trimmed to the selected objectives.
#'
#' Subclasses are responsible for loading domain-specific master schemas and
#' master SVG diagrams.  Use \code{\link{ANAFramework}} for the ANA
#' (Area-based Needs Assessment) conceptual framework.
#'
#' @importFrom R6 R6Class
#' @export
Framework <- R6::R6Class(
  "Framework",
  public = list(
    #' @field master_objectives_schema Data frame containing the full reference
    #'   schema with all available objectives.
    master_objectives_schema = NULL,

    #' @field master_indicator_bank Data frame containing the full indicator
    #'   bank with all available indicators and their associated metadata,
    #'   including threshold columns.  Loaded separately from
    #'   \code{master_objectives_schema}.
    master_indicator_bank = NULL,

    #' @field modified_objectives_schema Data frame derived from
    #'   \code{master_objectives_schema} and filtered to the currently selected
    #'   objectives.
    modified_objectives_schema = NULL,

    #' @field modified_indicator_bank Data frame derived from
    #'   \code{master_indicator_bank} and filtered to the currently selected
    #'   objective codes.  Initialised to the same value as
    #'   \code{master_indicator_bank} on construction and updated by
    #'   \code{modify_indicator_bank()}.
    modified_indicator_bank = NULL,

    #' @field master_svg Character string with the full SVG diagram for the
    #'   conceptual framework.  Can be set with \code{set_master_svg()}.
    master_svg = NULL,

    #' @field adjusted_svg Character string with an SVG diagram derived from
    #'   \code{master_svg} and modified to reflect the selected objectives.
    adjusted_svg = NULL,

    #' @field primary_objectives Numeric vector of objective codes selected as
    #'   primary data collection objectives.  Used by
    #'   \code{modify_adjusted_svg()} to colour the corresponding sub-pillar
    #'   blocks light green in the adjusted SVG.
    primary_objectives = NULL,

    #' @field secondary_objectives Numeric vector of objective codes selected as
    #'   secondary data objectives.  Used by \code{modify_adjusted_svg()} to
    #'   colour the corresponding sub-pillar blocks light blue in the adjusted
    #'   SVG.
    secondary_objectives = NULL,

    #' @field primary_indicator_codes Character vector of primary indicator codes
    #'   selected by the user.
    primary_indicator_codes = NULL,

    #' @field secondary_indicator_codes Character vector of secondary indicator
    #'   codes selected by the user.
    secondary_indicator_codes = NULL,

    #' @field modified_primary_indicator_codes Data frame of primary indicator
    #'   codes derived from \code{modified_objectives_schema}.
    modified_primary_indicator_codes = NULL,

    #' @field modified_secondary_indicator_codes Data frame of secondary
    #'   indicator codes derived from \code{modified_objectives_schema}.
    modified_secondary_indicator_codes = NULL,

    #' @field secondary_data_sources Data frame of secondary
    #'   objectives, data sources, and purposes.
    secondary_data_sources = NULL,

    #' @field metadata List containing framework metadata including
    #'   \code{created_datetime} and \code{modified_datetime}, both initialised
    #'   to \code{Sys.time()} on construction.
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    #' @description
    #' Creates a new Framework object.
    #' @return A new Framework object.
    initialize = function() {
      phr_try(
        {
          self$master_objectives_schema <- NULL
          self$master_indicator_bank <- NULL
          self$modified_objectives_schema <- NULL
          self$modified_indicator_bank <- NULL
          self$master_svg <- NULL
          self$adjusted_svg <- NULL
          self$primary_objectives <- NULL
          self$secondary_objectives <- NULL
          self$primary_indicator_codes <- NULL
          self$secondary_indicator_codes <- NULL
          self$modified_primary_indicator_codes <- NULL
          self$modified_secondary_indicator_codes <- NULL
          self$metadata$created_datetime <- Sys.time()
          self$metadata$modified_datetime <- Sys.time()
          phr_message(
            phr_txt("Framework initialized."),
            origin = "Framework$initialize"
          )
        },
        on_error = "abort",
        origin = "Framework$initialize"
      )
      invisible(self)
    },

    #' @description Set the primary objectives for this framework.
    #'
    #' Stores a numeric vector of objective codes as
    #' \code{primary_objectives} and calls \code{touch()} to update the
    #' \code{modified_datetime}.
    #'
    #' @param objective_codes Numeric vector of primary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_primary_objectives = function(objective_codes) {
      phr_try(
        {
          self$primary_objectives <- as.numeric(unlist(objective_codes))
          private$.touch()
          phr_message(
            phr_txt(
              "Primary objectives set ({length(self$primary_objectives)} code(s))."
            ),
            origin = "Framework$set_primary_objectives"
          )
        },
        on_error = "abort",
        origin = "Framework$set_primary_objectives"
      )
      invisible(self)
    },

    #' @description Set the secondary objectives for this framework.
    #'
    #' Stores a numeric vector of objective codes as
    #' \code{secondary_objectives} and calls \code{touch()} to update the
    #' \code{modified_datetime}.
    #'
    #' @param objective_codes Numeric vector of secondary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_secondary_objectives = function(objective_codes) {
      phr_try(
        {
          self$secondary_objectives <- as.numeric(unlist(objective_codes))
          private$.touch()
          phr_message(
            phr_txt(
              "Secondary objectives set ({length(self$secondary_objectives)} code(s))."
            ),
            origin = "Framework$set_secondary_objectives"
          )
        },
        on_error = "abort",
        origin = "Framework$set_secondary_objectives"
      )
      invisible(self)
    },

    #' @description Set primary indicator codes for this framework.
    #'
    #' Stores a character vector of indicator codes as
    #' \code{primary_indicator_codes} and updates modified primary indicator
    #' caches from the current adjusted schema.
    #'
    #' @param indicator_codes Character/numeric vector (or list) of indicator
    #'   codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_primary_indicators = function(indicator_codes) {
      phr_try(
        {
          self$primary_indicator_codes <- as.character(unlist(indicator_codes))
          private$.refresh_modified_indicator_codes()
          private$.touch()
          phr_message(
            phr_txt(
              "Primary indicators set ({length(self$primary_indicator_codes)} code(s))."
            ),
            origin = "Framework$set_primary_indicators"
          )
        },
        on_error = "abort",
        origin = "Framework$set_primary_indicators"
      )
      invisible(self)
    },

    #' @description Set secondary indicator codes for this framework.
    #'
    #' Stores a character vector of indicator codes as
    #' \code{secondary_indicator_codes} and updates modified secondary indicator
    #' caches from the current adjusted schema.
    #'
    #' @param indicator_codes Character/numeric vector (or list) of indicator
    #'   codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_secondary_indicators = function(indicator_codes) {
      phr_try(
        {
          self$secondary_indicator_codes <- as.character(unlist(
            indicator_codes
          ))
          private$.refresh_modified_indicator_codes()
          private$.touch()
          phr_message(
            phr_txt(
              "Secondary indicators set ({length(self$secondary_indicator_codes)} code(s))."
            ),
            origin = "Framework$set_secondary_indicators"
          )
        },
        on_error = "abort",
        origin = "Framework$set_secondary_indicators"
      )
      invisible(self)
    },

    #' @description Add a source to the secondary data sources table.
    #' @param objective Character. The objective code that this data source relates to.
    #' @param source Character. The name of the secondary data source.
    #' @param purpose Character. The purpose of the secondary data source.
    #' @return Invisibly returns \code{self} for method chaining.
    add_secondary_data_source = function(objective, source, purpose) {
      phr_try(
        {
          self$secondary_data_sources <- rbind(
            self$secondary_data_sources,
            data.frame(
              objective = objective,
              source = source,
              purpose = purpose,
              stringsAsFactors = FALSE
            )
          )
          private$.touch()
          phr_message(
            phr_txt("Secondary data source added."),
            origin = "Framework$set_secondary_data_source"
          )
        },
        on_error = "abort",
        origin = "Framework$set_secondary_data_source"
      )
      invisible(self)
    },

    #' @description Remove a source from the secondary data sources table.
    #' @param objective Character. The objective code that this data source relates to.
    #' @param source Character. The name of the secondary data source to remove.
    #' @return Invisibly returns \code{self} for method chaining.
    #' @details
    #' Removes a secondary data source from the \code{secondary_data_sources} data frame
    #' by matching both the objective code and source name. If the specified source is not found, a warning is issued.
    remove_secondary_data_source = function(objective, source) {
      phr_try(
        {
          if (is.null(self$secondary_data_sources)) {
            phr_warning(
              message = phr_txt(
                "No secondary data sources to remove."
              ),
              origin = "Framework$remove_secondary_data_source"
            )
            return(invisible(self))
          }
          self$secondary_data_sources <- subset(
            self$secondary_data_sources,
            !(objective == objective & source == source)
          )
          private$.touch()
          phr_message(
            phr_txt("Secondary data source removed."),
            origin = "Framework$remove_secondary_data_source"
          )
        },
        on_error = "abort",
        origin = "Framework$remove_secondary_data_source"
      )
      invisible(self)
    },

    #' @description Set the master reference schema (objectives).
    #'
    #' Validates and stores a data frame as the master objectives schema.  The
    #' data frame must contain at minimum the columns required by
    #' \code{\link{validate_objective_schema}}: \code{sector},
    #' \code{pillar}, \code{sub_pillar}, \code{short_objective}, and
    #' \code{text_objective}.
    #'
    #' @param schema Data frame. The master reference schema.
    #' @return Invisibly returns \code{self} for method chaining.
    set_master_schema = function(schema) {
      phr_try(
        {
          validate_objective_schema(schema, soft = FALSE)
          self$master_objectives_schema <- as.data.frame(
            schema,
            stringsAsFactors = FALSE
          )
          phr_message(
            phr_txt(
              "Master objectives schema set ({nrow(self$master_objectives_schema)} rows)."
            ),
            origin = "Framework$set_master_schema"
          )
        },
        on_error = "abort",
        origin = "Framework$set_master_schema"
      )
      invisible(self)
    },

    #' @description Set the master SVG diagram.
    #'
    #' Stores a character string as the master SVG diagram for the conceptual
    #' framework.  The string should be valid SVG markup.
    #'
    #' @param svg_content Character. SVG content as a single string.
    #' @return Invisibly returns \code{self} for method chaining.
    set_master_svg = function(svg_content) {
      phr_try(
        {
          phr_assert(
            is.character(svg_content) && length(svg_content) >= 1,
            message = phr_txt(
              "svg_content must be a non-empty character string."
            ),
            origin = "Framework$set_master_svg"
          )
          self$master_svg <- svg_content
          phr_message(
            phr_txt("Master SVG set."),
            origin = "Framework$set_master_svg"
          )
        },
        on_error = "abort",
        origin = "Framework$set_master_svg"
      )
      invisible(self)
    },

    #' @description Colour sub-pillar blocks in the adjusted SVG according to
    #'   the \code{primary_objectives} and \code{secondary_objectives} fields.
    #'
    #' Derives \code{adjusted_svg} from \code{master_svg} by applying fill
    #' colours to each sub-pillar block whose \code{objective_code} is present
    #' in \code{primary_objectives}, \code{secondary_objectives}, or both.
    #' The mapping from objective codes to SVG group ids uses the
    #' \code{objective_code} and \code{sub_pillar} columns of
    #' \code{master_schema}.
    #'
    #' Colour rules (applied per sub-pillar, using all objective codes that
    #' belong to that sub-pillar):
    #' \itemize{
    #'   \item \strong{white} – no codes present in either vector (default).
    #'   \item \strong{\code{#90EE90}} (light green) – one or more codes are
    #'     present in \code{primary_objectives} only.
    #'   \item \strong{\code{#ADD8E6}} (light blue) – one or more codes are
    #'     present in \code{secondary_objectives} only.
    #'   \item \strong{\code{#DDA0DD}} (light purple) – one or more codes are
    #'     present in both \code{primary_objectives} and
    #'     \code{secondary_objectives}.
    #' }
    #'
    #' \code{master_svg} and \code{master_objectives_schema} is \code{NULL} the
    #' method issues a warning and returns without modifying \code{adjusted_svg}.
    #'
    #' @param primary_objective_codes Numeric vector of primary objective codes
    #'   to highlight in light green.  When \code{NULL} (the default) the
    #'   \code{primary_objectives} field is used as a fallback.
    #' @param secondary_objective_codes Numeric vector of secondary objective
    #'   codes to highlight in light blue.  When \code{NULL} (the default) the
    #'   \code{secondary_objectives} field is used as a fallback.
    #' @return Invisibly returns \code{self} for method chaining.
    modify_adjusted_svg = function(
      primary_objective_codes = NULL,
      secondary_objective_codes = NULL
    ) {
      phr_try(
        {
          if (is.null(self$master_svg)) {
            phr_warning(
              message = phr_txt(
                "master_svg is not set; skipping modify_adjusted_svg()."
              ),
              origin = "Framework$modify_adjusted_svg"
            )
            return(invisible(self))
          }

          if (
            is.null(self$master_objectives_schema) ||
              !is.data.frame(self$master_objectives_schema) ||
              nrow(self$master_objectives_schema) == 0
          ) {
            phr_warning(
              message = phr_txt(
                "master_objectives_schema is not set; skipping modify_adjusted_svg()."
              ),
              origin = "Framework$modify_adjusted_svg"
            )
            return(invisible(self))
          }

          phr_assert(
            "objective_code" %in% names(self$master_objectives_schema),
            message = phr_txt(
              "master_objectives_schema must contain an 'objective_code' column for modify_adjusted_svg()."
            ),
            origin = "Framework$modify_adjusted_svg"
          )

          primary <- as.numeric(
            if (!is.null(primary_objective_codes)) {
              primary_objective_codes
            } else {
              self$primary_objectives
            }
          )
          secondary <- as.numeric(
            if (!is.null(secondary_objective_codes)) {
              secondary_objective_codes
            } else {
              self$secondary_objectives
            }
          )

          svg <- self$master_svg

          # Primary lookup: build objective_code -> [svg_group_id, ...] by parsing
          # the numeric codes embedded in the SVG diagram text labels.  This is the
          # authoritative source and does not rely on sub_pillar column values.
          svg_code_map <- .build_code_svg_map(svg)

          # Fallback lookup: derive objective_code -> sub_pillar from master_objectives_schema.
          # Used only for codes whose SVG groups do not carry embedded code labels
          # (e.g. minimal test-fixture SVGs where group ids equal sub_pillar values).
          schema_code_map <- list()
          if ("sub_pillar" %in% names(self$master_objectives_schema)) {
            for (i in seq_len(nrow(self$master_objectives_schema))) {
              code <- self$master_objectives_schema$objective_code[[i]]
              sp <- self$master_objectives_schema$sub_pillar[[i]]
              if (!is.na(code) && !is.na(sp) && nzchar(as.character(sp))) {
                code_str <- as.character(as.integer(code))
                if (is.null(schema_code_map[[code_str]])) {
                  schema_code_map[[code_str]] <- as.character(sp)
                }
              }
            }
          }

          # Merge: SVG-parsed entries take priority over schema-derived fallbacks.
          code_map <- schema_code_map
          for (code_str in names(svg_code_map)) {
            code_map[[code_str]] <- svg_code_map[[code_str]]
          }

          # Retain only entries whose key is a valid integer code string.
          valid_keys <- names(code_map)[grepl("^\\d+$", names(code_map))]

          # Colour each SVG group whose objective_code appears in the selected sets.
          for (code_str in valid_keys) {
            code <- as.numeric(code_str)
            svg_ids <- code_map[[code_str]]

            in_primary <- length(primary) > 0L && code %in% primary
            in_secondary <- length(secondary) > 0L && code %in% secondary

            if (!in_primary && !in_secondary) {
              next
            }

            colour <- if (in_primary && in_secondary) {
              "#DDA0DD"
            } else if (in_primary) {
              "#90EE90"
            } else {
              "#ADD8E6"
            }

            for (svg_id in svg_ids) {
              pattern <- paste0(
                '(<g id="',
                svg_id,
                '">[^<]*<rect(?:[^>]*?) )fill="[^"]*"([^>]*>)'
              )
              replacement <- paste0('\\1fill="', colour, '"\\2')
              svg <- gsub(pattern, replacement, svg, perl = TRUE)
            }
          }

          self$adjusted_svg <- svg
          private$.touch()
          phr_message(
            phr_txt("Adjusted SVG updated via modify_adjusted_svg()."),
            origin = "Framework$modify_adjusted_svg"
          )
        },
        on_error = "abort",
        origin = "Framework$modify_adjusted_svg"
      )
      invisible(self)
    },

    #' @description Render the framework SVG and display it in the active
    #'   graphics device (e.g. the RStudio Plots pane).
    #'
    #' The \code{version} argument controls which SVG is rendered.  Pass
    #' \code{"adjusted"} (the default) to render \code{adjusted_svg}, falling
    #' back to \code{master_svg} when \code{adjusted_svg} is \code{NULL}.
    #' Pass \code{"master"} to render \code{master_svg} directly, regardless of
    #' whether an adjusted version exists.
    #'
    #' Requires the \pkg{rsvg} and \pkg{grid} packages.
    #' When neither is installed the SVG is written to a temporary file and
    #' the file path is returned visibly so the caller can open it manually.
    #'
    #' @param version Character. Which SVG to render: \code{"adjusted"}
    #'   (default) or \code{"master"}.
    #' @return Invisibly returns \code{self} for method chaining.  When
    #'   \pkg{rsvg}/\pkg{grid} are unavailable the path to the written SVG
    #'   file is returned visibly instead.
    render_framework_svg = function(version = "adjusted") {
      phr_try(
        {
          phr_assert(
            is.character(version) &&
              length(version) == 1 &&
              version %in% c("adjusted", "master"),
            message = phr_txt("version must be 'adjusted' or 'master'."),
            origin = "Framework$render_framework_svg"
          )
          svg_content <- if (version == "master") {
            self$master_svg
          } else {
            self$adjusted_svg %||% self$master_svg
          }
          phr_assert(
            !is.null(svg_content) && nzchar(svg_content),
            message = phr_txt(
              "No SVG content available. Call modify_adjusted_schema() or set_master_svg() first."
            ),
            origin = "Framework$render_framework_svg"
          )

          tmp_svg <- tempfile(fileext = ".svg")
          writeLines(svg_content, con = tmp_svg)

          if (
            requireNamespace("rsvg", quietly = TRUE) &&
              requireNamespace("grid", quietly = TRUE)
          ) {
            native_raster <- rsvg::rsvg_nativeraster(tmp_svg)
            grid::grid.newpage()
            grid::grid.raster(native_raster)
            phr_message(
              phr_txt("Framework SVG rendered to the active graphics device."),
              origin = "Framework$render_framework_svg"
            )
          } else {
            phr_warning(
              message = phr_txt(
                "Packages 'rsvg' and 'grid' are required to display the SVG in the plots window. SVG written to: {tmp_svg}"
              ),
              origin = "Framework$render_framework_svg"
            )
            return(tmp_svg)
          }
        },
        on_error = "abort",
        origin = "Framework$render_framework_svg"
      )
      invisible(self)
    },

    #' @description Filter the master objectives schema to the specified
    #'   objective codes and store the result in \code{modified_objectives_schema}.
    #'
    #' Filters \code{master_objectives_schema} to retain matching rows and
    #' stores the result as \code{modified_objectives_schema}.  The column used
    #' for matching is determined automatically: if \code{master_objectives_schema}
    #' contains an \code{objective_code} column, filtering is done on that column
    #' (useful for \code{ANAFramework} which uses numeric objective codes);
    #' otherwise \code{short_objective} is used.  When \code{objective_codes} is
    #' \code{NULL} or an empty vector the method first checks whether
    #' \code{primary_objectives} or \code{secondary_objectives} are set on the
    #' Framework; if either is set their combined unique codes are used as the
    #' filter.  Only if both are \code{NULL} does the method fall back to
    #' retaining all rows from \code{master_objectives_schema}.  Also updates
    #' \code{modified_primary_indicator_codes} and
    #' \code{modified_secondary_indicator_codes} from the resulting
    #' \code{modified_objectives_schema}.
    #'
    #' @param objective_codes Character or numeric vector (or list) of objective
    #'   code values to retain.  The type should match the filter column:
    #'   numeric for \code{objective_code}, character for \code{short_objective}.
    #'   Pass \code{NULL} (the default) to use \code{primary_objectives} and
    #'   \code{secondary_objectives}, or all rows as a final fallback.
    #' @return Invisibly returns \code{self} for method chaining.
    modify_adjusted_schema = function(objective_codes = NULL) {
      phr_try(
        {
          phr_assert(
            !is.null(self$master_objectives_schema) &&
              is.data.frame(self$master_objectives_schema) &&
              nrow(self$master_objectives_schema) > 0,
            message = phr_txt(
              "master_objectives_schema must be set before calling modify_adjusted_schema()."
            ),
            origin = "Framework$modify_adjusted_schema"
          )

          # Determine which column to filter on
          filter_col <- if (
            "objective_code" %in% names(self$master_objectives_schema)
          ) {
            "objective_code"
          } else {
            "short_objective"
          }

          if (is.null(objective_codes) || length(objective_codes) == 0) {
            # Use primary_objectives and secondary_objectives if either is set;
            # fall back to all rows only when both are NULL.
            combined <- c(self$primary_objectives, self$secondary_objectives)
            if (!is.null(combined) && length(combined) > 0) {
              objective_codes <- unique(combined)
            } else {
              objective_codes <- unique(self$master_objectives_schema[[
                filter_col
              ]])
            }
          } else {
            objective_codes <- unlist(objective_codes)
          }

          self$modified_objectives_schema <- self$master_objectives_schema[
            self$master_objectives_schema[[filter_col]] %in% objective_codes,
            ,
            drop = FALSE
          ]

          # Update modified indicator code caches from the modified_objectives_schema
          private$.refresh_modified_indicator_codes()
          private$.touch()

          phr_message(
            phr_txt(
              "Modified objectives schema updated: {nrow(self$modified_objectives_schema)} of {nrow(self$master_objectives_schema)} rows selected."
            ),
            origin = "Framework$modify_adjusted_schema"
          )
        },
        on_error = "abort",
        origin = "Framework$modify_adjusted_schema"
      )
      invisible(self)
    },

    #' @description Filter the master indicator bank to the specified objective
    #'   codes and store the result in \code{modified_indicator_bank}.
    #'
    #' Filters \code{master_indicator_bank} to retain only rows whose
    #' \code{objective_code} column matches one of the supplied codes, and
    #' stores the result as \code{modified_indicator_bank}.  When
    #' \code{objective_codes} is \code{NULL} or an empty vector the full
    #' \code{master_indicator_bank} is copied to \code{modified_indicator_bank}
    #' (i.e. no filtering).  The method silently returns without modifying
    #' \code{modified_indicator_bank} when \code{master_indicator_bank} is
    #' \code{NULL} or does not contain an \code{objective_code} column.
    #'
    #' @param objective_codes Numeric or character vector (or list) of objective
    #'   codes to filter by.  Pass \code{NULL} (the default) to reset
    #'   \code{modified_indicator_bank} to the full master.
    #' @return Invisibly returns \code{self} for method chaining.
    modify_indicator_bank = function(objective_codes = NULL) {
      phr_try(
        {
          if (
            is.null(self$master_indicator_bank) ||
              !is.data.frame(self$master_indicator_bank)
          ) {
            phr_warning(
              message = phr_txt(
                "master_indicator_bank is not set; skipping modify_indicator_bank()."
              ),
              origin = "Framework$modify_indicator_bank"
            )
            return(invisible(self))
          }

          if (is.null(objective_codes) || length(objective_codes) == 0) {
            self$modified_indicator_bank <- self$master_indicator_bank
            phr_message(
              phr_txt(
                "Modified indicator bank reset to full master ({nrow(self$master_indicator_bank)} rows)."
              ),
              origin = "Framework$modify_indicator_bank"
            )
            return(invisible(self))
          }

          if (!"objective_code" %in% names(self$master_indicator_bank)) {
            phr_warning(
              message = phr_txt(
                "master_indicator_bank does not contain an 'objective_code' column; skipping modify_indicator_bank()."
              ),
              origin = "Framework$modify_indicator_bank"
            )
            return(invisible(self))
          }

          codes <- unlist(objective_codes)
          self$modified_indicator_bank <- self$master_indicator_bank[
            self$master_indicator_bank[["objective_code"]] %in% codes,
            ,
            drop = FALSE
          ]
          private$.touch()
          phr_message(
            phr_txt(
              "Modified indicator bank updated: {nrow(self$modified_indicator_bank)} of {nrow(self$master_indicator_bank)} rows selected."
            ),
            origin = "Framework$modify_indicator_bank"
          )
        },
        on_error = "abort",
        origin = "Framework$modify_indicator_bank"
      )
      invisible(self)
    }
  ),

  private = list(
    # Update modified_datetime timestamp.
    .touch = function() {
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    # Rebuild modified indicator code fields from the current
    # modified_objectives_schema.
    .refresh_modified_indicator_codes = function() {
      if (
        !is.null(self$modified_objectives_schema) &&
          is.data.frame(self$modified_objectives_schema) &&
          "indicator_code" %in% names(self$modified_objectives_schema)
      ) {
        schema <- self$modified_objectives_schema
        code_col <- if ("objective_code" %in% names(schema)) {
          "objective_code"
        } else if ("short_objective" %in% names(schema)) {
          "short_objective"
        } else {
          NULL
        }
        ind_codes <- as.character(schema[["indicator_code"]])
        valid_ind <- !is.na(ind_codes) & nzchar(ind_codes)

        build_indicator_df <- function(objective_codes) {
          if (
            is.null(code_col) ||
              is.null(objective_codes) ||
              length(objective_codes) == 0L
          ) {
            return(NULL)
          }
          objective_codes <- as.character(unique(unlist(objective_codes)))
          obj_vals <- as.character(schema[[code_col]])
          keep <- valid_ind & obj_vals %in% objective_codes
          if (!any(keep)) {
            return(NULL)
          }
          out <- data.frame(
            indicator_code = ind_codes[keep],
            stringsAsFactors = FALSE
          )
          unique(out)
        }

        self$modified_primary_indicator_codes <-
          build_indicator_df(self$primary_objectives)
        self$modified_secondary_indicator_codes <-
          build_indicator_df(self$secondary_objectives)
      } else {
        self$modified_primary_indicator_codes <- NULL
        self$modified_secondary_indicator_codes <- NULL
      }
    }
  )
)
