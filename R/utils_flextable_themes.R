#' Apply Standard phr Flextable Theme
#'
#' Applies a consistent, standardised theme to a \code{flextable} object using
#' the Roboto Condensed font and REACH brand colour palette for column headers.
#' This function centralises all common styling so that every table produced by
#' phr looks consistent.
#'
#' @param ft A \code{flextable} object to theme.
#' @param color_palette Character string naming the colour palette to use.
#'   The first colour in the resolved palette is used as the header cell
#'   background. Defaults to \code{"reach1"} (REACH red, \code{"#EE5859"}).
#'   Any palette name accepted by \code{\link{get_color_palette}} is valid,
#'   e.g. \code{"reach2"}, \code{"reach3"}, \code{"group"}.
#' @param header_color Character. Text colour for header cells.
#'   Default: \code{"white"}.
#' @param font_name Character. Font family to use. Default:
#'   \code{"Roboto Condensed"}.
#' @param font_size Numeric. Base font size in points. Default: \code{10}.
#' @param inner_border_color Character. Colour for inner borders.
#'   Default: \code{"#D3D3D3"}.
#' @param inner_border_width Numeric. Width of inner borders in points.
#'   Default: \code{0.5}.
#' @param outer_border_color Character. Colour for outer borders.
#'   Default: \code{"#808080"}.
#' @param outer_border_width Numeric. Width of outer borders in points.
#'   Default: \code{1}.
#'
#' @return The themed \code{flextable} object.
#' @export
#'
#' @examples
#' \dontrun{
#'   ft <- flextable::flextable(data.frame(a = 1:3, b = letters[1:3]))
#'   ft <- apply_phr_flextable_theme(ft)
#'   ft <- apply_phr_flextable_theme(ft, color_palette = "reach2")
#'   ft <- apply_phr_flextable_theme(ft, color_palette = "group")
#' }
apply_phr_flextable_theme <- function(ft,
                                        color_palette      = "reach1",
                                        header_color       = "white",
                                        font_name          = "Roboto Condensed",
                                        font_size          = 10,
                                        inner_border_color = "#D3D3D3",
                                        inner_border_width = 0.5,
                                        outer_border_color = "#808080",
                                        outer_border_width = 1) {

  origin <- "apply_phr_flextable_theme"

  phrutils::phr_try({

    # Resolve the palette name to a vector of colours, then use the first
    # colour as the header background (same pattern as plot_* functions)
    palette_colors <- phrutils::get_color_palette(type = color_palette)
    header_bg <- palette_colors[[1]]

    # Base booktabs theme
    ft <- flextable::theme_booktabs(ft)

    # Font
    ft <- flextable::font(ft, fontname = font_name, part = "all")
    ft <- flextable::fontsize(ft, size = font_size, part = "all")

    # Header styling
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::bg(ft, part = "header", bg = header_bg)
    ft <- flextable::color(ft, part = "header", color = header_color)
    ft <- flextable::align(ft, align = "left", part = "header")

    # Borders
    ft <- flextable::border_inner(
      ft,
      border = officer::fp_border(color = inner_border_color,
                                  width = inner_border_width),
      part = "body"
    )
    ft <- flextable::border_inner_h(
      ft,
      border = officer::fp_border(color = inner_border_color,
                                  width = inner_border_width),
      part = "header"
    )
    ft <- flextable::border_inner_v(
      ft,
      border = officer::fp_border(color = inner_border_color,
                                  width = inner_border_width),
      part = "header"
    )
    ft <- flextable::border_outer(
      ft,
      border = officer::fp_border(color = outer_border_color,
                                  width = outer_border_width),
      part = "all"
    )

    # Auto-fit column widths
    ft <- flextable::autofit(ft)

    return(ft)

  }, on_error = "warn", origin = origin)
}
