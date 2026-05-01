#' Sampling Frame Validation Functions
#'
#' @description
#' Functions for validating sampling frames before sample drawing.

#' Validate sampling frame structure
#'
#' Validates a sampling frame for use in sample drawing operations.  Uses
#' existing \pkg{phr} validator helpers where possible.
#'
#' Required columns (in the prescribed order for a compliant frame):
#' \code{stratum}, \code{psu}, \code{population_size}, \code{inclusion}.
#' The columns \code{sampled_psu} and \code{allocated_sample} are added
#' automatically by \code{draw_sample()} and are not required on input.
#'
#' Optional but validated if present:
#' \itemize{
#'   \item \code{inclusion} — logical column marking PSUs for inclusion;
#'     created with all \code{TRUE} values by \code{set_sampling_frame()} if
#'     absent.
#'   \item \code{population_size} — positive numeric sizes.
#' }
#'
#' @param frame Data frame. The sampling frame to validate.
#' @param required_cols Character vector. Additional required column names
#'   beyond the standard set (default \code{character(0)}).
#' @return List with \code{valid} (logical), \code{message} (character on
#'   failure), \code{issues} (named list on failure), or \code{summary}
#'   (named list on success).
#' @export
validate_sampling_frame <- function(frame, required_cols = character(0)) {

  issues <- list()

  # ---- Basic data-frame check (reuse phr_validate_dataframe) ----------
  if (!phr_validate_dataframe(frame, soft = TRUE)) {
    return(list(valid = FALSE,
                message = "Frame must be a non-NULL data frame with atomic columns."))
  }

  # ---- Non-empty check ------------------------------------------------
  if (nrow(frame) == 0) {
    return(list(valid = FALSE, message = "Frame is empty."))
  }

  # ---- Required columns: stratum, psu, population_size ----------------
  standard_required <- c("stratum", "psu", "population_size")
  all_required <- unique(c(standard_required, required_cols))
  missing_cols <- setdiff(all_required, names(frame))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }

  # ---- PSU column checks ----------------------------------------------
  if ("psu" %in% names(frame)) {
    if (any(duplicated(frame$psu))) {
      issues$duplicate_psu <- frame$psu[duplicated(frame$psu)]
    }
    if (any(is.na(frame$psu))) {
      issues$missing_psu_values <- sum(is.na(frame$psu))
    }
  }

  # ---- inclusion column -----------------------------------------------
  if ("inclusion" %in% names(frame)) {
    if (!is.logical(frame$inclusion)) {
      issues$invalid_inclusion <- "The 'inclusion' column must be logical (TRUE/FALSE)."
    }
  } else {
    issues$missing_inclusion <- paste(
      "Frame does not have an 'inclusion' column.",
      "Consider adding it to mark PSUs for inclusion (TRUE/FALSE)."
    )
  }

  # ---- population_size checks -----------------------------------------
  if ("population_size" %in% names(frame)) {
    if (any(frame$population_size <= 0, na.rm = TRUE)) {
      issues$invalid_population_size <- sum(frame$population_size <= 0, na.rm = TRUE)
    }
  }

  # ---- Return result --------------------------------------------------
  # issues only about missing inclusion do not make the frame invalid
  hard_issues <- issues[setdiff(names(issues), "missing_inclusion")]

  if (length(hard_issues) == 0) {
    n_included <- if ("inclusion" %in% names(frame)) sum(frame$inclusion, na.rm = TRUE) else nrow(frame)

    return(list(
      valid = TRUE,
      message = "Sampling frame is valid.",
      issues  = if (length(issues) > 0) issues else NULL,
      summary = list(
        num_units      = nrow(frame),
        num_included   = n_included,
        num_strata     = if ("stratum" %in% names(frame)) length(unique(frame$stratum)) else 1L,
        total_population = if ("population_size" %in% names(frame)) {
          sum(frame$population_size, na.rm = TRUE)
        } else {
          NA_real_
        }
      )
    ))
  } else {
    return(list(valid = FALSE, issues = issues))
  }
}

#' Check frame coverage for sample table
#'
#' @param frame Data frame. The sampling frame
#' @param sample_table Data frame. The sample table with strata
#' @return List with coverage check results
#' @export
check_frame_coverage <- function(frame, sample_table) {

  origin <- "check_frame_coverage"

  phr_try({
    phr_assert(
      is.data.frame(frame) && is.data.frame(sample_table),
      message = phr_txt("Both frame and sample_table must be data frames."),
      origin  = origin
    )

    # Get strata from each
    frame_strata <- unique(frame$stratum)
    table_strata <- sample_table$stratum_id
  
  # Check coverage
  missing_in_frame <- setdiff(table_strata, frame_strata)
  missing_in_table <- setdiff(frame_strata, table_strata)
  
  # Check if frame has enough units for each stratum
  insufficient_strata <- list()
  for (i in seq_len(nrow(sample_table))) {
    stratum_id <- sample_table$stratum_id[i]
    needed <- sample_table$sample_size[i]
    available <- sum(frame$stratum == stratum_id)
    
    if (available < needed) {
      insufficient_strata[[stratum_id]] <- list(
        needed = needed,
        available = available
      )
    }
  }
  
  issues <- list()
  if (length(missing_in_frame) > 0) {
    issues$missing_in_frame <- missing_in_frame
  }
  if (length(missing_in_table) > 0) {
    issues$missing_in_table <- missing_in_table
  }
  if (length(insufficient_strata) > 0) {
    issues$insufficient_units <- insufficient_strata
  }
  
    if (length(issues) == 0) {
      list(valid = TRUE, message = phr_txt("Frame provides adequate coverage for sample table."))
    } else {
      list(valid = FALSE, issues = issues)
    }
  }, on_error = "abort", origin = origin)
}

#' Create a synthetic sampling frame for testing
#'
#' Creates a synthetic sampling frame with the standard column structure:
#' \code{stratum}, \code{psu}, \code{population_size}, \code{inclusion}.
#'
#' @param strata_config List. Configuration for each stratum with name, size, pop_range
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame. A synthetic sampling frame
#' @export
create_synthetic_frame <- function(strata_config, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  frames_list <- list()
  
  for (config in strata_config) {
    stratum_name <- config$name
    num_units <- config$size
    pop_range <- config$pop_range
    
    # Generate random population sizes
    populations <- round(runif(num_units, 
                              min = pop_range[1], 
                              max = pop_range[2]))
    
    # Create frame for this stratum (standard column order: stratum, psu, population_size, inclusion)
    stratum_frame <- data.frame(
      stratum         = stratum_name,
      psu             = paste0(stratum_name, "_", seq_len(num_units)),
      population_size = populations,
      inclusion       = TRUE,
      stringsAsFactors = FALSE
    )
    
    frames_list[[length(frames_list) + 1]] <- stratum_frame
  }
  
  # Combine all strata
  frame <- do.call(rbind, frames_list)
  rownames(frame) <- NULL
  
  return(frame)
}

#' Get sampling frame summary
#'
#' @param frame Data frame. The sampling frame
#' @return List with summary statistics
#' @export
summarize_sampling_frame <- function(frame) {

  origin <- "summarize_sampling_frame"

  phr_try({
    phr_validate_dataframe(frame, origin = origin, soft = FALSE)

    summary_out <- list(
      total_units  = nrow(frame),
      num_strata   = if ("stratum" %in% names(frame)) length(unique(frame$stratum)) else 1L,
      strata_names = if ("stratum" %in% names(frame)) unique(frame$stratum) else NA_character_
    )

    if ("population_size" %in% names(frame)) {
      summary_out$total_population          <- sum(frame$population_size, na.rm = TRUE)
      summary_out$mean_population_per_unit  <- mean(frame$population_size, na.rm = TRUE)
      summary_out$median_population_per_unit <- median(frame$population_size, na.rm = TRUE)
    }

    if ("stratum" %in% names(frame)) {
      stratum_summaries <- list()
      for (stratum in unique(frame$stratum)) {
        stratum_data <- frame[frame$stratum == stratum, ]
        stratum_summaries[[stratum]] <- list(
          num_units        = nrow(stratum_data),
          total_population = if ("population_size" %in% names(stratum_data)) {
            sum(stratum_data$population_size, na.rm = TRUE)
          } else {
            NA
          }
        )
      }
      summary_out$by_stratum <- stratum_summaries
    }

    summary_out
  }, on_error = "abort", origin = origin)
}

#' Print sampling frame summary
#'
#' @param frame Data frame. The sampling frame
#' @export
print_frame_summary <- function(frame) {

  origin <- "print_frame_summary"

  phr_try({
    summary <- summarize_sampling_frame(frame)

    phr_message(
      phr_txt("Sampling Frame: {summary$total_units} units across {summary$num_strata} {ifelse(summary$num_strata == 1, 'stratum', 'strata')}."),
      origin = origin
    )

    if (!is.null(summary$total_population)) {
      phr_message(
        phr_txt("Population: total={summary$total_population}, mean/unit={round(summary$mean_population_per_unit,1)}, median/unit={round(summary$median_population_per_unit,1)}"),
        origin = origin
      )
    }

    if (!is.null(summary$by_stratum)) {
      for (stratum in names(summary$by_stratum)) {
        st_s <- summary$by_stratum[[stratum]]
        pop_txt <- if (!is.na(st_s$total_population)) phr_txt(", population={st_s$total_population}") else ""
        phr_message(phr_txt("{stratum}: {st_s$num_units} units{pop_txt}"), origin = origin)
      }
    }
  }, on_error = "abort", origin = origin)
}
