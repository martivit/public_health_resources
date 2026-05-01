#' ANAFramework R6 Class
#'
#' @description
#' Subclass of \code{\link{Framework}} for the ANA (Area-based Needs
#' Assessment) conceptual framework.  On initialisation the class automatically
#' loads:
#' \itemize{
#'   \item the bundled \code{reference.xlsx} as \code{master_schema};
#'   \item the bundled \code{ana_framework.svg} as \code{master_svg}.
#' }
#'
#' Call \code{create_adjusted_schema()} or \code{update_adjusted_schema()} with
#' a vector of \code{short_objective} values and then \code{update_adjusted_svg()}
#' to produce a highlighted SVG that shows which conceptual framework blocks are
#' covered by the selection.
#'
#' @importFrom R6 R6Class
#' @export
ANAFramework <- R6::R6Class(
  "ANAFramework",
  inherit = Framework,
  public = list(
    #' @description
    #' Creates a new ANAFramework object.
    #'
    #' The bundled \code{reference.xlsx} is loaded as \code{master_schema} and
    #' \code{ana_framework.svg} is loaded as \code{master_svg}.  Warnings are
    #' issued if either file cannot be found or read.
    #'
    #' @return A new ANAFramework object.
    initialize = function() {
      super$initialize()

      # ---- Load master schema from reference.xlsx ----
      phr_try({
        schema <- load_objective_schema()
        if (!is.null(schema) && is.data.frame(schema) && nrow(schema) > 0) {
          self$master_schema <- schema
          phr_message(
            phr_txt(
              "ANAFramework: master schema loaded ({nrow(schema)} rows)."
            ),
            origin = "ANAFramework$initialize"
          )
        } else {
          phr_warning(
            message = phr_txt(
              "ANAFramework: reference.xlsx could not be loaded; master_schema is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure reference.xlsx is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      # ---- Load master SVG from ana_framework.svg ----
      phr_try({
        svg_file <- system.file("resources", "ana_framework.svg", package = "phr")
        if (!nzchar(svg_file) || !file.exists(svg_file)) {
          svg_file <- file.path("resources", "ana_framework.svg")
        }
        if (file.exists(svg_file)) {
          svg_content <- paste(readLines(svg_file, warn = FALSE), collapse = "\n")
          self$master_svg <- svg_content
          phr_message(
            phr_txt("ANAFramework: master SVG loaded from ana_framework.svg."),
            origin = "ANAFramework$initialize"
          )
        } else {
          phr_warning(
            message = phr_txt(
              "ANAFramework: ana_framework.svg could not be found; master_svg is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure ana_framework.svg is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      invisible(self)
    },

    #' @description Filter the master schema to objectives whose \code{pillar}
    #'   matches one or more of the supplied pillar names.
    #'
    #' This corresponds to the pillar-dimension filter used in the Shiny module
    #' (\code{reference_objectives[reference_objectives$pillar %in%
    #' selected_pillars, ]}).
    #'
    #' @param pillars Character vector of pillar names to retain.
    #' @return Data frame subset of \code{master_schema}.
    filter_schema_by_pillar = function(pillars) {
      phr_try({
        phr_assert(
          !is.null(self$master_schema) && is.data.frame(self$master_schema),
          message = phr_txt("master_schema must be set before calling filter_schema_by_pillar()."),
          origin  = "ANAFramework$filter_schema_by_pillar"
        )
        phr_assert(
          is.character(pillars) && length(pillars) > 0,
          message = phr_txt("pillars must be a non-empty character vector."),
          origin  = "ANAFramework$filter_schema_by_pillar"
        )
        self$master_schema[self$master_schema$pillar %in% pillars, , drop = FALSE]
      }, on_error = "abort", origin = "ANAFramework$filter_schema_by_pillar")
    },

    #' @description Update the adjusted SVG by highlighting blocks for the
    #'   currently selected objectives.
    #'
    #' For each unique \code{sub_pillar} value in \code{adjusted_schema} the
    #' corresponding \code{<g id="...">} group's primary \code{<rect>} element
    #' (the one with \code{stroke="black"}) has its \code{fill} attribute
    #' changed to \code{highlight_colour}.  All other block rects are reset to
    #' \code{default_colour}.
    #'
    #' This mirrors the JavaScript highlight logic from the ANA Shiny module
    #' (\code{r.setAttribute('fill','lightgreen')}) but uses pure R string
    #' manipulation so it works outside of Shiny.
    #'
    #' @param highlight_colour Character. Fill colour for selected blocks.
    #'   Defaults to \code{"lightgreen"}.
    #' @param default_colour Character. Fill colour for unselected blocks.
    #'   Defaults to \code{"white"}.
    #' @return Invisibly returns \code{self} for method chaining.
    update_adjusted_svg = function(highlight_colour = "lightgreen",
                                   default_colour   = "white") {
      phr_try({
        if (is.null(self$master_svg)) {
          phr_warning(
            message = phr_txt("master_svg is not set; skipping adjusted SVG update."),
            origin  = "ANAFramework$update_adjusted_svg"
          )
          return(invisible(self))
        }

        if (is.null(self$adjusted_schema) || nrow(self$adjusted_schema) == 0) {
          phr_warning(
            message = phr_txt(
              "adjusted_schema is empty or not set; skipping adjusted SVG update."
            ),
            origin = "ANAFramework$update_adjusted_svg"
          )
          return(invisible(self))
        }

        # sub_pillar values for selected objectives (these are the SVG group IDs)
        selected_blocks <- unique(self$adjusted_schema$sub_pillar)
        selected_blocks <- selected_blocks[!is.na(selected_blocks) & nzchar(selected_blocks)]

        # All block IDs present in master_schema
        all_blocks <- unique(self$master_schema$sub_pillar)
        all_blocks  <- all_blocks[!is.na(all_blocks) & nzchar(all_blocks)]

        svg <- self$master_svg

        # For every known block, set fill to highlight or default on the first
        # <rect stroke="black"> inside <g id="BLOCK_ID">
        svg <- private$set_block_fills(svg, all_blocks, selected_blocks,
                                       highlight_colour, default_colour)

        self$adjusted_svg <- svg
        phr_message(
          phr_txt(
            "ANAFramework adjusted SVG updated: {length(selected_blocks)} block(s) highlighted."
          ),
          origin = "ANAFramework$update_adjusted_svg"
        )
      }, on_error = "abort", origin = "ANAFramework$update_adjusted_svg")
      invisible(self)
    }
  ),

  private = list(
    # Replace fill="..." on the primary <rect stroke="black"> inside each
    # named <g id="BLOCK_ID"> group.
    #
    # @param svg Character. SVG markup to modify.
    # @param all_blocks Character vector of all known sub_pillar block IDs.
    # @param selected_blocks Character vector of the currently selected block IDs.
    # @param highlight_colour Character. Fill colour for selected blocks.
    # @param default_colour Character. Fill colour for unselected blocks.
    # @return Modified SVG character string.
    set_block_fills = function(svg, all_blocks, selected_blocks,
                               highlight_colour, default_colour) {
      for (bid in all_blocks) {
        colour <- if (bid %in% selected_blocks) highlight_colour else default_colour
        # Match:  <g id="BID">  (optional whitespace/newline)
        #         <rect  (any attrs)  fill="ANYTHING"  stroke="black"
        # and replace the fill value.
        pattern <- paste0(
          '(<g id="', bid, '">[^<]*<rect(?:[^>]*?) )fill="[^"]*"',
          '([^>]*?stroke="black")'
        )
        replacement <- paste0('\\1fill="', colour, '"\\2')
        svg <- gsub(pattern, replacement, svg, perl = TRUE)
      }
      svg
    }
  )
)

