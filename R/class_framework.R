#' Framework R6 Class
#'
#' @description
#' Base class for conceptual frameworks used in protocol planning.
#' A Framework holds a master reference schema (all objectives and indicators),
#' an adjusted schema filtered to the currently selected objectives, a
#' master SVG diagram representing the full conceptual framework, and an
#' adjusted SVG diagram derived from the master and trimmed to the selected
#' objectives.
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
    #' @field master_schema Data frame containing the full reference schema with
    #'   all available objectives and indicators.
    master_schema = NULL,

    #' @field adjusted_schema Data frame derived from \code{master_schema} and
    #'   filtered to the currently selected objectives.
    adjusted_schema = NULL,

    #' @field master_svg Character string with the full SVG diagram for the
    #'   conceptual framework.  Can be set with \code{set_master_svg()}.
    master_svg = NULL,

    #' @field adjusted_svg Character string with an SVG diagram derived from
    #'   \code{master_svg} and modified to reflect the selected objectives.
    adjusted_svg = NULL,

    #' @description
    #' Creates a new Framework object.
    #' @return A new Framework object.
    initialize = function() {
      phr_try({
        self$master_schema   <- NULL
        self$adjusted_schema <- NULL
        self$master_svg      <- NULL
        self$adjusted_svg    <- NULL
        phr_message(phr_txt("Framework initialized."), origin = "Framework$initialize")
      }, on_error = "abort", origin = "Framework$initialize")
      invisible(self)
    },

    #' @description Set the master reference schema.
    #'
    #' Validates and stores a data frame as the master schema.  The data frame
    #' must contain at minimum the columns required by
    #' \code{\link{validate_objective_schema}}: \code{sector},
    #' \code{pillar}, \code{sub_pillar}, \code{short_objective}, and
    #' \code{text_objective}.
    #'
    #' @param schema Data frame. The master reference schema.
    #' @return Invisibly returns \code{self} for method chaining.
    set_master_schema = function(schema) {
      phr_try({
        validate_objective_schema(schema, soft = FALSE)
        self$master_schema <- as.data.frame(schema, stringsAsFactors = FALSE)
        phr_message(
          phr_txt("Master schema set ({nrow(self$master_schema)} rows)."),
          origin = "Framework$set_master_schema"
        )
      }, on_error = "abort", origin = "Framework$set_master_schema")
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
      phr_try({
        phr_assert(
          is.character(svg_content) && length(svg_content) >= 1,
          message = phr_txt("svg_content must be a non-empty character string."),
          origin  = "Framework$set_master_svg"
        )
        self$master_svg <- svg_content
        phr_message(phr_txt("Master SVG set."), origin = "Framework$set_master_svg")
      }, on_error = "abort", origin = "Framework$set_master_svg")
      invisible(self)
    },

    #' @description Update the adjusted schema based on selected objectives.
    #'
    #' Filters the master schema to retain only the rows whose
    #' \code{short_objective} value matches one of the supplied identifiers.
    #' When \code{selected_objectives} is \code{NULL} or an empty vector the
    #' full master schema is used as the adjusted schema.
    #'
    #' @param selected_objectives Character vector of \code{short_objective}
    #'   values to retain.  Pass \code{NULL} to reset to the full master schema.
    #' @return Invisibly returns \code{self} for method chaining.
    update_adjusted_schema = function(selected_objectives = NULL) {
      phr_try({
        phr_assert(
          !is.null(self$master_schema) && is.data.frame(self$master_schema) &&
            nrow(self$master_schema) > 0,
          message = phr_txt("master_schema must be set before calling update_adjusted_schema()."),
          origin  = "Framework$update_adjusted_schema"
        )

        if (is.null(selected_objectives) || length(selected_objectives) == 0) {
          self$adjusted_schema <- self$master_schema
          phr_message(
            phr_txt("Adjusted schema reset to full master schema ({nrow(self$adjusted_schema)} rows)."),
            origin = "Framework$update_adjusted_schema"
          )
        } else {
          self$adjusted_schema <- self$master_schema[
            self$master_schema$short_objective %in% selected_objectives, ,
            drop = FALSE
          ]
          phr_message(
            phr_txt(
              "Adjusted schema updated: {nrow(self$adjusted_schema)} of {nrow(self$master_schema)} rows selected."
            ),
            origin = "Framework$update_adjusted_schema"
          )
        }
      }, on_error = "abort", origin = "Framework$update_adjusted_schema")
      invisible(self)
    },

    #' @description Update the adjusted SVG based on the current adjusted schema.
    #'
    #' Derives \code{adjusted_svg} from \code{master_svg} by modifying elements
    #' whose \code{id} attribute matches a \code{short_objective} value in the
    #' adjusted schema.  Elements whose id is \emph{not} present in the adjusted
    #' schema have their \code{visibility} set to \code{"hidden"} via an inline
    #' style attribute.
    #'
    #' When \code{master_svg} or \code{adjusted_schema} is \code{NULL} the
    #' method issues a warning and returns without modifying \code{adjusted_svg}.
    #'
    #' Subclasses may override this method to implement richer SVG manipulation
    #' tailored to a specific diagram structure.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    update_adjusted_svg = function() {
      phr_try({
        if (is.null(self$master_svg)) {
          phr_warning(
            message = phr_txt("master_svg is not set; skipping adjusted SVG update."),
            origin  = "Framework$update_adjusted_svg"
          )
          return(invisible(self))
        }

        if (is.null(self$adjusted_schema) || nrow(self$adjusted_schema) == 0) {
          phr_warning(
            message = phr_txt(
              "adjusted_schema is empty or not set; skipping adjusted SVG update."
            ),
            origin = "Framework$update_adjusted_svg"
          )
          return(invisible(self))
        }

        selected_ids <- unique(self$adjusted_schema$short_objective)
        selected_ids <- selected_ids[!is.na(selected_ids)]

        svg <- self$master_svg

        # Find all id="..." occurrences in the SVG and hide those not selected.
        # Pattern matches id="<value>" and captures the value.
        all_ids <- regmatches(svg, gregexpr('id="([^"]+)"', svg))[[1]]
        all_ids <- sub('^id="(.+)"$', "\\1", all_ids)

        for (obj_id in setdiff(all_ids, selected_ids)) {
          # Append visibility:hidden to an existing style attribute, or add one.
          pattern_existing <- paste0('(id="', obj_id, '"[^>]*style="[^"]*)(")' )
          if (grepl(pattern_existing, svg)) {
            svg <- gsub(
              pattern_existing,
              paste0('\\1;visibility:hidden\\2'),
              svg
            )
          } else {
            svg <- gsub(
              paste0('(id="', obj_id, '")'),
              paste0('\\1 style="visibility:hidden"'),
              svg
            )
          }
        }

        self$adjusted_svg <- svg
        phr_message(
          phr_txt("Adjusted SVG updated ({length(selected_ids)} objective(s) visible)."),
          origin = "Framework$update_adjusted_svg"
        )
      }, on_error = "abort", origin = "Framework$update_adjusted_svg")
      invisible(self)
    },

    #' @description Export framework data to a plain list.
    #'
    #' Returns a serialisable list that can be stored alongside a protocol
    #' export.  R6 references are not preserved; restore with
    #' \code{\link{restore_framework}}.
    #'
    #' @return Named list with elements \code{class}, \code{master_schema},
    #'   \code{adjusted_schema}, \code{master_svg}, and \code{adjusted_svg}.
    export_framework = function() {
      list(
        class           = class(self)[1],
        master_schema   = self$master_schema,
        adjusted_schema = self$adjusted_schema,
        master_svg      = self$master_svg,
        adjusted_svg    = self$adjusted_svg
      )
    },

    #' @description Add a single objective row to the adjusted schema.
    #'
    #' Appends a new row to \code{adjusted_schema}.  If \code{adjusted_schema}
    #' is \code{NULL}, it is initialised from the supplied row.  The row must
    #' contain at minimum the columns required by
    #' \code{\link{validate_objective_schema}}: \code{sector},
    #' \code{pillar}, \code{sub_pillar}, \code{short_objective}, and
    #' \code{text_objective}.
    #'
    #' @param row Named list or single-row data frame representing one
    #'   objective (must contain required columns).
    #' @return Invisibly returns \code{self} for method chaining.
    add_objective_row = function(row) {
      phr_try({
        required <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")
        if (is.list(row) && !is.data.frame(row)) {
          row <- as.data.frame(row, stringsAsFactors = FALSE)
        }
        phr_assert(
          is.data.frame(row) && nrow(row) >= 1L,
          message = phr_txt("row must be a named list or single-row data frame."),
          origin  = "Framework$add_objective_row"
        )
        missing_cols <- setdiff(required, names(row))
        if (length(missing_cols) > 0) {
          phr_error(
            message = phr_txt(
              "row is missing required fields: {paste(missing_cols, collapse = ', ')}"
            ),
            origin = "Framework$add_objective_row"
          )
        }
        row <- row[1L, , drop = FALSE]
        if (is.null(self$adjusted_schema)) {
          self$adjusted_schema <- row
        } else {
          # Align columns: add any new columns as NA in the existing schema and vice versa
          all_cols <- union(names(self$adjusted_schema), names(row))
          for (col in setdiff(all_cols, names(self$adjusted_schema))) {
            self$adjusted_schema[[col]] <- NA
          }
          for (col in setdiff(all_cols, names(row))) {
            row[[col]] <- NA
          }
          self$adjusted_schema <- rbind(
            self$adjusted_schema[, all_cols, drop = FALSE],
            row[, all_cols, drop = FALSE]
          )
        }
        phr_message(
          phr_txt("Objective row '{row$short_objective}' added to adjusted_schema."),
          origin = "Framework$add_objective_row"
        )
      }, on_error = "abort", origin = "Framework$add_objective_row")
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
      phr_try({
        phr_assert(
          is.character(version) && length(version) == 1 &&
            version %in% c("adjusted", "master"),
          message = phr_txt("version must be 'adjusted' or 'master'."),
          origin  = "Framework$render_framework_svg"
        )
        svg_content <- if (version == "master") {
          self$master_svg
        } else {
          self$adjusted_svg %||% self$master_svg
        }
        phr_assert(
          !is.null(svg_content) && nzchar(svg_content),
          message = phr_txt(
            "No SVG content available. Call update_adjusted_svg() or set_master_svg() first."
          ),
          origin = "Framework$render_framework_svg"
        )

        tmp_svg <- tempfile(fileext = ".svg")
        writeLines(svg_content, con = tmp_svg)

        if (requireNamespace("rsvg", quietly = TRUE) &&
            requireNamespace("grid", quietly = TRUE)) {
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
      }, on_error = "abort", origin = "Framework$render_framework_svg")
      invisible(self)
    },

    #' @description Create the adjusted schema by filtering the master schema
    #'   to a specified set of objective codes.
    #'
    #' Filters \code{master_schema} to retain only rows whose
    #' \code{short_objective} value is present in \code{objective_codes} and
    #' stores the result as \code{adjusted_schema}.  When \code{objective_codes}
    #' is \code{NULL} or an empty vector all objective codes found in
    #' \code{master_schema} are used, making \code{adjusted_schema} identical to
    #' \code{master_schema}.
    #'
    #' @param objective_codes Character vector or list of \code{short_objective}
    #'   values to retain.  Pass \code{NULL} (the default) to include all
    #'   objectives from the master schema.
    #' @return Invisibly returns \code{self} for method chaining.
    create_adjusted_schema = function(objective_codes = NULL) {
      phr_try({
        phr_assert(
          !is.null(self$master_schema) && is.data.frame(self$master_schema) &&
            nrow(self$master_schema) > 0,
          message = phr_txt("master_schema must be set before calling create_adjusted_schema()."),
          origin  = "Framework$create_adjusted_schema"
        )

        if (is.null(objective_codes) || length(objective_codes) == 0) {
          objective_codes <- unique(self$master_schema$short_objective)
        } else {
          objective_codes <- as.character(unlist(objective_codes))
        }

        self$adjusted_schema <- self$master_schema[
          self$master_schema$short_objective %in% objective_codes, ,
          drop = FALSE
        ]
        phr_message(
          phr_txt(
            "Adjusted schema created: {nrow(self$adjusted_schema)} of {nrow(self$master_schema)} rows selected."
          ),
          origin = "Framework$create_adjusted_schema"
        )
      }, on_error = "abort", origin = "Framework$create_adjusted_schema")
      invisible(self)
    },

    #' @description Remove objective row(s) from the adjusted schema by
    #'   \code{short_objective} value.
    #'
    #' Removes all rows in \code{adjusted_schema} whose \code{short_objective}
    #' matches the supplied value.  Rows with \code{NA} in
    #' \code{short_objective} are never removed by this method (they must be
    #' removed by directly modifying \code{adjusted_schema}).
    #'
    #' @param short_objective Character. The \code{short_objective} value to
    #'   remove.
    #' @return Invisibly returns \code{self} for method chaining.
    remove_objective_row = function(short_objective) {
      phr_try({
        phr_assert(
          !is.null(self$adjusted_schema) && is.data.frame(self$adjusted_schema) &&
            nrow(self$adjusted_schema) > 0,
          message = phr_txt("adjusted_schema is empty or not set; nothing to remove."),
          origin  = "Framework$remove_objective_row"
        )
        before <- nrow(self$adjusted_schema)
        self$adjusted_schema <- self$adjusted_schema[
          is.na(self$adjusted_schema$short_objective) |
            self$adjusted_schema$short_objective != short_objective,
          , drop = FALSE
        ]
        removed <- before - nrow(self$adjusted_schema)
        if (removed == 0L) {
          phr_warning(
            message = phr_txt(
              "No rows with short_objective '{short_objective}' found in adjusted_schema."
            ),
            origin = "Framework$remove_objective_row"
          )
        } else {
          phr_message(
            phr_txt("{removed} row(s) removed for short_objective '{short_objective}'."),
            origin = "Framework$remove_objective_row"
          )
        }
      }, on_error = "abort", origin = "Framework$remove_objective_row")
      invisible(self)
    }
  )
)
