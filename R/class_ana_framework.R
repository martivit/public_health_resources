#' ANAFramework R6 Class
#'
#' @description
#' Subclass of \code{\link{Framework}} for the ANA (Area-based Needs
#' Assessment) conceptual framework.  On initialisation the class automatically
#' loads:
#' \itemize{
#'   \item the bundled \code{reference_objectives.xlsx} as
#'     \code{master_objectives_schema};
#'   \item the bundled \code{reference_indicator_bank.xlsx} as
#'     \code{master_indicator_bank} (and \code{modified_indicator_bank});
#'   \item the bundled \code{ana_framework.svg} as \code{master_svg}.
#' }
#'
#' Call \code{modify_adjusted_schema()} with a vector of objective codes to
#' filter \code{master_objectives_schema} into \code{modified_objectives_schema},
#' and \code{modify_indicator_bank()} to filter the indicator bank.
#' Call \code{modify_adjusted_svg()} to produce a highlighted SVG.
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
    #' The bundled \code{reference_objectives.xlsx} is loaded as
    #' \code{master_objectives_schema} and \code{reference_indicator_bank.xlsx}
    #' is loaded as both \code{master_indicator_bank} and
    #' \code{modified_indicator_bank}.  All are stored as separate fields.
    #' \code{ana_framework.svg} is loaded as \code{master_svg}.  Warnings are
    #' issued if any file cannot be found or read.
    #'
    #' @return A new ANAFramework object.
    initialize = function() {
      super$initialize()

      # ---- Load master objectives schema from reference_objectives.xlsx ----
      phrutils::phr_try({
        objectives <- load_objective_schema()
        if (!is.null(objectives) && is.data.frame(objectives) && nrow(objectives) > 0) {
          self$master_objectives_schema <- objectives
          phrutils::phr_message(
            phr_txt(
              "ANAFramework: master objectives schema loaded ({nrow(objectives)} rows)."
            ),
            origin = "ANAFramework$initialize"
          )
        } else {
          phrutils::phr_warning(
            message = phr_txt(
              "ANAFramework: reference_objectives.xlsx could not be loaded; master_objectives_schema is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure reference_objectives.xlsx is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      # ---- Load indicator bank from reference_indicator_bank.xlsx ----
      phrutils::phr_try({
        indicators <- load_indicator_bank()
        if (!is.null(indicators) && is.data.frame(indicators) && nrow(indicators) > 0) {
          self$master_indicator_bank   <- indicators
          self$modified_indicator_bank <- indicators
          phrutils::phr_message(
            phr_txt(
              "ANAFramework: indicator bank loaded ({nrow(indicators)} rows)."
            ),
            origin = "ANAFramework$initialize"
          )
        } else {
          phrutils::phr_warning(
            message = phr_txt(
              "ANAFramework: reference_indicator_bank.xlsx could not be loaded; master_indicator_bank is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure reference_indicator_bank.xlsx is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      # ---- Load master SVG from ana_framework.svg ----
      phrutils::phr_try({
        svg_file <- system.file("resources", "ana_framework.svg", package = "phr")
        if (!nzchar(svg_file) || !file.exists(svg_file)) {
          svg_file <- file.path("resources", "ana_framework.svg")
        }
        if (file.exists(svg_file)) {
          svg_content <- paste(readLines(svg_file, warn = FALSE), collapse = "\n")
          self$master_svg  <- svg_content
          self$adjusted_svg <- svg_content
          phrutils::phr_message(
            phr_txt("ANAFramework: master SVG loaded from ana_framework.svg."),
            origin = "ANAFramework$initialize"
          )
        } else {
          phrutils::phr_warning(
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

