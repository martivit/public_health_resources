#' Framework Utility Functions
#'
#' @description
#' Utility functions for creating and restoring Framework objects.
#'
#' @name framework_utils
NULL


#' Create a new base Framework object
#'
#' Convenience constructor for \code{\link{Framework}}.
#'
#' @return A new \code{Framework} object.
#' @export
create_framework <- function() {
  Framework$new()
}


#' Restore a Framework object from exported data
#'
#' Reconstructs a \code{\link{Framework}} or \code{\link{ANAFramework}} object
#' from a plain list produced by \code{Protocol$export_protocol()$framework}.
#'
#' @param framework_data Named list containing framework data.
#'   Must contain a \code{class} element.
#' @return A \code{Framework} or \code{ANAFramework} object with fields
#'   restored from \code{framework_data}.
#' @export
restore_framework <- function(framework_data) {

  origin <- "restore_framework"

  phr_try({
    phr_assert(
      is.list(framework_data),
      message = phr_txt("framework_data must be a named list containing framework export data."),
      origin  = origin
    )

    cls <- framework_data$class %||% "Framework"

    fw <- if (identical(cls, "ANAFramework")) {
      ANAFramework$new()
    } else {
      Framework$new()
    }

    # Restore fields; prefer the exported values over the defaults loaded on init.
    if (!is.null(framework_data$master_objectives_schema))  fw$master_objectives_schema  <- framework_data$master_objectives_schema
    if (!is.null(framework_data$master_indicator_bank))     fw$master_indicator_bank     <- framework_data$master_indicator_bank
    if (!is.null(framework_data$modified_objectives_schema)) fw$modified_objectives_schema <- framework_data$modified_objectives_schema
    if (!is.null(framework_data$modified_indicator_bank))   fw$modified_indicator_bank   <- framework_data$modified_indicator_bank
    if (!is.null(framework_data$master_svg))                fw$master_svg                <- framework_data$master_svg
    if (!is.null(framework_data$adjusted_svg))              fw$adjusted_svg              <- framework_data$adjusted_svg
    if (!is.null(framework_data$primary_objectives))        fw$primary_objectives        <- framework_data$primary_objectives
    if (!is.null(framework_data$secondary_objectives))      fw$secondary_objectives      <- framework_data$secondary_objectives

    phr_message(phr_txt("Framework restored (class: {cls})."), origin = origin)
    fw
  }, on_error = "abort", origin = origin)
}
