#' IntegratedAnalysis R6 Class
#'
#' @description
#' Subclass of \code{\link{Orchestrator}} for storing multiple
#' \code{\link{DataAnalytics}} objects and building unified analysis tables.
#'
#' @importFrom R6 R6Class
IntegratedAnalysis <- R6::R6Class(
  "IntegratedAnalysis",
  inherit = Orchestrator,
  public = list(
    #' @field data_analytics Named list of \code{\link{DataAnalytics}} objects.
    data_analytics = list(),

    #' @description
    #' Creates a new IntegratedAnalysis object.
    #' @param data_analytics Optional named list of DataAnalytics objects.
    #' @return A new IntegratedAnalysis object.
    initialize = function(data_analytics = list()) {
      super$initialize()
      self$data_analytics <- data_analytics
      invisible(self)
    },

    #' @description Add a DataAnalytics object.
    #' @param name Character key used to store the object.
    #' @param analytics A \code{\link{DataAnalytics}} object.
    #' @return Invisibly returns \code{self}.
    add_data_analytics = function(name, analytics) {
      phr_assert(
        is.character(name) && length(name) == 1L && nzchar(name),
        message = phr_txt("name must be a non-empty character string."),
        origin = "IntegratedAnalysis$add_data_analytics"
      )
      phr_assert(
        !is.null(analytics) && inherits(analytics, "DataAnalytics"),
        message = phr_txt("analytics must inherit from DataAnalytics."),
        origin = "IntegratedAnalysis$add_data_analytics"
      )
      self$data_analytics[[name]] <- analytics
      private$.touch()
      invisible(self)
    },

    #' @description Return stored analytics names.
    #' @return Character vector.
    get_data_analytics_names = function() {
      names(self$data_analytics) %||% character(0)
    },

    #' @description Build a unified table from a shared result field.
    #'
    #' @param field Character scalar naming a field present in each stored
    #'   analytics object that should be row-bound.
    #' @return Combined data frame with an added \code{analysis_name} column.
    build_unified_table = function(field) {
      phr_assert(
        is.character(field) && length(field) == 1L && nzchar(field),
        message = phr_txt("field must be a non-empty character string."),
        origin = "IntegratedAnalysis$build_unified_table"
      )
      if (length(self$data_analytics) == 0L) return(data.frame())

      parts <- list()
      for (nm in names(self$data_analytics)) {
        obj <- self$data_analytics[[nm]]
        if (is.null(obj) || is.null(obj[[field]]) || !is.data.frame(obj[[field]])) next
        df <- as.data.frame(obj[[field]], stringsAsFactors = FALSE)
        df$analysis_name <- nm
        parts[[length(parts) + 1L]] <- df
      }
      if (length(parts) == 0L) return(data.frame())
      out <- do.call(rbind, parts)
      rownames(out) <- NULL
      private$.touch()
      out
    }
  )
)
