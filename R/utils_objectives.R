#' Objectives Management Functions
#'
#' @description
#' Functions for creating and managing research objectives in the protocol pipeline.
#' Objectives are stored on a Protocol as a nested list keyed by
#' \code{sector → pillar → sub_pillar → data_source}, where \code{data_source}
#' captures whether the objective is "primary" or "secondary" (or any other
#' value from the objective schema).

# Internal helper: normalise a data_source value (NULL / NA → "primary").
.normalize_data_source <- function(data_source) {
  if (is.null(data_source) || (length(data_source) == 1L && is.na(data_source)) || !nzchar(as.character(data_source))) {
    "primary"
  } else {
    as.character(data_source)
  }
}

# Internal helper: determine whether a list is a flat list of objectives.
# A flat list is one where every top-level element is a named list that
# contains at least a $short_objective field.  Returns FALSE for empty lists
# so that empty inputs are passed through directly without unnecessary nesting.
.is_flat_objectives <- function(objectives) {
  if (length(objectives) == 0L) return(FALSE)
  all(vapply(objectives, function(x) is.list(x) && !is.null(x$short_objective), logical(1L)))
}

#' Create multiple objectives from a data frame
#'
#' @param objectives_df Data frame with columns: sector, pillar, sub_pillar,
#'   short_objective, text_objective, and optionally data_source.
#' @return List of objective objects (flat list).
#' @export
create_objectives_from_df <- function(objectives_df) {

  origin <- "create_objectives_from_df"

  phr_try({

    phr_validate_dataframe(objectives_df, origin = origin, soft = FALSE)

    required_cols <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")
    phr_validate_columns(objectives_df, required_cols, origin = origin, soft = FALSE)

    objectives_list <- list()

    for (i in seq_len(nrow(objectives_df))) {
      row <- objectives_df[i, ]

      raw_ds      <- if ("data_source" %in% names(row)) row$data_source else NA_character_
      data_source <- .normalize_data_source(raw_ds)

      obj <- list(
        sector          = as.character(row$sector),
        pillar          = as.character(row$pillar),
        sub_pillar      = as.character(row$sub_pillar),
        short_objective = as.character(row$short_objective),
        text_objective  = as.character(row$text_objective),
        data_source     = data_source,
        created_date    = Sys.time()
      )

      objectives_list[[length(objectives_list) + 1]] <- obj
    }

    objectives_list

  }, on_error = "abort", origin = origin)
}

# ---------------------------------------------------------------------------
# Nested-structure helpers
# ---------------------------------------------------------------------------

#' Flatten a nested objectives list to a flat list of objectives
#'
#' Converts the nested \code{sector → pillar → sub_pillar → data_source → [objectives]}
#' structure stored on a Protocol to a simple flat list of objective lists,
#' suitable for iteration, validation, and conversion to a data frame.
#' A flat list of objectives is returned unchanged.
#'
#' @param objectives List.  The nested or flat objectives structure (e.g.
#'   \code{protocol$objectives}).
#' @return Flat list of objective named lists.
#' @export
flatten_objectives <- function(objectives) {
  if (is.null(objectives) || length(objectives) == 0) return(list())

  # If every top-level element is an objective (has $short_objective), already flat
  if (.is_flat_objectives(objectives)) return(objectives)

  flat <- list()
  for (sector in names(objectives)) {
    for (pillar in names(objectives[[sector]])) {
      for (sub_pillar in names(objectives[[sector]][[pillar]])) {
        for (ds in names(objectives[[sector]][[pillar]][[sub_pillar]])) {
          objs <- objectives[[sector]][[pillar]][[sub_pillar]][[ds]]
          flat <- c(flat, objs)
        }
      }
    }
  }
  flat
}

#' Nest a flat list of objectives into the standard hierarchical structure
#'
#' Converts a flat list of objective lists into the nested
#' \code{sector → pillar → sub_pillar → data_source → [objectives]} structure.
#'
#' @param objectives_flat List.  Flat list of objective named lists as produced
#'   by \code{create_objectives_from_df()}.
#' @return Nested list suitable for assignment to \code{protocol$objectives}.
#' @export
nest_objectives <- function(objectives_flat) {
  origin <- "nest_objectives"
  phr_try({
    phr_assert(is.list(objectives_flat),
               message = phr_txt("objectives_flat must be a list."), origin = origin)
    if (length(objectives_flat) == 0) return(list())

    nested <- list()
    for (obj in objectives_flat) {
      required <- c("sector", "pillar", "sub_pillar", "data_source", "short_objective")
      missing  <- setdiff(required, names(obj))
      if (length(missing) > 0) {
        phr_warning(
          message = phr_txt("Skipping objective missing fields: {paste(missing, collapse=', ')}."),
          origin  = origin
        )
        next
      }
      s  <- obj$sector
      p  <- obj$pillar
      sp <- obj$sub_pillar
      ds <- obj$data_source

      if (is.null(nested[[s]]))           nested[[s]]           <- list()
      if (is.null(nested[[s]][[p]]))      nested[[s]][[p]]      <- list()
      if (is.null(nested[[s]][[p]][[sp]])) nested[[s]][[p]][[sp]] <- list()
      if (is.null(nested[[s]][[p]][[sp]][[ds]])) nested[[s]][[p]][[sp]][[ds]] <- list()

      nested[[s]][[p]][[sp]][[ds]] <- c(nested[[s]][[p]][[sp]][[ds]], list(obj))
    }
    nested
  }, on_error = "abort", origin = origin)
}

#' Count objectives in a (possibly nested) objectives structure
#'
#' @param objectives List.  Flat or nested objectives structure.
#' @return Integer count of objectives.
#' @export
count_objectives <- function(objectives) {
  length(flatten_objectives(objectives))
}

# ---------------------------------------------------------------------------
# Validation, querying, and display
# ---------------------------------------------------------------------------

#' Validate objectives
#'
#' Accepts either a flat list of objectives or the nested structure stored on a
#' Protocol.
#'
#' @param objectives List of objective objects (flat or nested).
#' @return List with validation results.
#' @export
validate_objectives <- function(objectives) {

  origin <- "validate_objectives"

  phr_try({

    phr_assert(
      is.list(objectives) && length(objectives) > 0,
      message = phr_txt("Objectives must be a non-empty list."),
      origin  = origin
    )

    flat <- flatten_objectives(objectives)

    phr_assert(
      length(flat) > 0,
      message = phr_txt("No objectives found after flattening nested structure."),
      origin  = origin
    )

    issues <- list()

    required_fields <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")

    # Check for duplicate short_objective labels
    short_objs <- sapply(flat, function(x) x$short_objective)
    if (any(duplicated(short_objs))) {
      issues$duplicate_short_objectives <- short_objs[duplicated(short_objs)]
      phr_warning(
        message = phr_txt("Duplicate short_objective labels found: {paste(issues$duplicate_short_objectives, collapse=', ')}"),
        origin  = origin,
        hint    = phr_txt("Each objective should have a unique short_objective label.")
      )
    }

    # Check for missing required fields
    for (i in seq_along(flat)) {
      obj     <- flat[[i]]
      missing <- setdiff(required_fields, names(obj))
      if (length(missing) > 0) {
        issues$missing_fields <- c(issues$missing_fields,
                                    paste0("objective[", i, "]: ", paste(missing, collapse = ", ")))
      }
    }

    # Check for vague objectives (very short text_objective)
    for (i in seq_along(flat)) {
      obj <- flat[[i]]
      if (!is.null(obj$text_objective) && nchar(obj$text_objective) < 20) {
        issues$vague_objectives <- c(issues$vague_objectives, obj$short_objective)
        phr_warning(
          message = phr_txt("Objective '{obj$short_objective}' has a very short text_objective (< 20 characters)."),
          origin  = origin,
          hint    = phr_txt("Consider expanding the text_objective for clarity.")
        )
      }
    }

    if (length(issues) == 0) {
      list(valid = TRUE, message = phr_txt("All objectives are valid."))
    } else {
      list(valid = FALSE, issues = issues)
    }

  }, on_error = "abort", origin = origin)
}

#' Get objectives by sector
#'
#' Works on both flat lists and the nested structure stored on a Protocol.
#'
#' @param objectives List of objectives (flat or nested).
#' @param sector Character. Sector to filter by.
#' @return Flat list of objectives for the specified sector.
#' @export
get_objectives_by_sector <- function(objectives, sector) {
  flat <- flatten_objectives(objectives)
  Filter(function(x) identical(x$sector, sector), flat)
}

#' Get objectives by data source
#'
#' Works on both flat lists and the nested structure stored on a Protocol.
#'
#' @param objectives List of objectives (flat or nested).
#' @param data_source Character. Data source value to filter by (e.g. "primary"
#'   or "secondary").
#' @return Flat list of matching objectives.
#' @export
get_objectives_by_data_source <- function(objectives, data_source) {
  flat <- flatten_objectives(objectives)
  Filter(function(x) identical(x$data_source, data_source), flat)
}

#' Convert objectives to data frame
#'
#' Accepts either a flat list of objectives or the nested structure stored on a
#' Protocol.
#'
#' @param objectives List of objectives (flat or nested).
#' @return Data frame with one row per objective.
#' @export
objectives_to_df <- function(objectives) {

  origin <- "objectives_to_df"

  phr_try({

    phr_assert(
      is.list(objectives),
      message = phr_txt("objectives must be a list."),
      origin  = origin
    )

    if (length(objectives) == 0) {
      return(data.frame(
        sector = character(0), pillar = character(0), sub_pillar = character(0),
        short_objective = character(0), text_objective = character(0),
        data_source = character(0),
        stringsAsFactors = FALSE
      ))
    }

    flat <- flatten_objectives(objectives)

    data.frame(
      sector          = sapply(flat, function(x) x$sector %||% NA_character_),
      pillar          = sapply(flat, function(x) x$pillar %||% NA_character_),
      sub_pillar      = sapply(flat, function(x) x$sub_pillar %||% NA_character_),
      short_objective = sapply(flat, function(x) x$short_objective %||% NA_character_),
      text_objective  = sapply(flat, function(x) x$text_objective %||% NA_character_),
      data_source     = sapply(flat, function(x) x$data_source %||% NA_character_),
      stringsAsFactors = FALSE
    )

  }, on_error = "abort", origin = origin)
}

#' Print objectives summary
#'
#' Accepts either a flat list of objectives or the nested structure stored on a
#' Protocol.
#'
#' @param objectives List of objectives (flat or nested).
#' @export
print_objectives_summary <- function(objectives) {

  origin <- "print_objectives_summary"

  flat <- flatten_objectives(objectives)

  if (length(flat) == 0) {
    phr_message(phr_txt("No objectives defined."), origin = origin)
    return(invisible(NULL))
  }

  sectors      <- unique(sapply(flat, function(x) x$sector))
  data_sources <- unique(sapply(flat, function(x) x$data_source %||% "unknown"))

  phr_message(
    phr_txt("Objectives Summary — {length(flat)} total objective(s). Sectors: {paste(sectors, collapse=', ')}. Data sources: {paste(data_sources, collapse=', ')}."),
    origin = origin
  )

  for (sector in sectors) {
    sector_objs <- get_objectives_by_sector(flat, sector)
    phr_message(phr_txt("{sector}: {length(sector_objs)} objective(s)"), origin = origin)
  }
}
