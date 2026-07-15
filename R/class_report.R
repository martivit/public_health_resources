#' Report R6 Class
#'
#' @description
#' Subclass of \code{\link{Document}} for holding a protocol object, multiple
#' data analytics objects, and an integrated analysis object.
#'
#' @importFrom R6 R6Class
Report <- R6::R6Class(
  "Report",
  inherit = Document,
  public = list(
    #' @field protocol A \code{\link{Protocol}} object.
    protocol = NULL,

    #' @field data_analytics Named list of \code{\link{DataAnalytics}} objects.
    data_analytics = list(),

    #' @field integrated_analysis An \code{\link{IntegratedAnalysis}} object.
    integrated_analysis = NULL,

    #' @description
    #' Creates a new Report object.
    #' @param protocol Optional \code{\link{Protocol}} object.
    #' @param data_analytics Optional named list of DataAnalytics objects.
    #' @param integrated_analysis Optional \code{\link{IntegratedAnalysis}}
    #'   object.
    #' @return A new Report object.
    initialize = function(protocol = NULL,
                          data_analytics = list(),
                          integrated_analysis = NULL) {
      super$initialize()
      self$protocol <- protocol
      self$data_analytics <- data_analytics
      self$integrated_analysis <- integrated_analysis
      invisible(self)
    }
  )
)
