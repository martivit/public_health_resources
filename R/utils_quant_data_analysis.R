#' Safe numeric coercion
#'
#' Returns the numeric value of a length-1 non-NULL object,
#' or \code{NA_real_} otherwise.
#'
#' @param x Object to coerce.
#' @return Numeric scalar or \code{NA_real_}.
#' @noRd
safe_num <- function(x) if (length(x) == 1 && !is.null(x)) as.numeric(x) else NA_real_

#' Safe character coercion
#'
#' Returns the character value of a length-1 non-NULL object,
#' or \code{NA_character_} otherwise.
#'
#' @param x Object to coerce.
#' @return Character scalar or \code{NA_character_}.
#' @noRd
safe_chr <- function(x) if (length(x) == 1 && !is.null(x)) as.character(x) else NA_character_

#' Safe logical coercion
#'
#' Returns the logical value of a length-1 non-NULL object,
#' or \code{NA} otherwise.
#'
#' @param x Object to coerce.
#' @return Logical scalar or \code{NA}.
#' @noRd
safe_lgl <- function(x) if (length(x) == 1 && !is.null(x)) as.logical(x) else NA

#' Execute a data analysis plan against a survey design
#'
#' Loops through each row of \code{analysis_plan}, dispatches the appropriate
#' survey calculation function (\code{prop}, \code{mean}, \code{median},
#' \code{ratio}, or \code{categorical}), handles optional disaggregation, and
#' binds all results into a single tibble.
#'
#' Results columns are returned in the following order:
#' \code{plan_row}, \code{variable}, \code{indicator_name},
#' \code{indicator_unit}, \code{group_name}, \code{disaggregation_value},
#' \code{calculation}, \code{point.estimate}, \code{lower_ci},
#' \code{upper_ci}, \code{ci_method}, \code{n_unweighted},
#' \code{n_weighted}, \code{denom_unweighted}, \code{denom_weighted},
#' \code{n_eff}, \code{deff}, \code{note}.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param analysis_plan A data frame with columns \code{indicator_name},
#'   \code{calculation}, \code{var_name}, \code{denom_var},
#'   \code{disaggregation}, \code{multiplier}, and \code{indicator_unit}.
#' @param high_design_complexity Logical; if \code{TRUE}, triggers stricter
#'   CI selection (e.g. logit-transformed CIs for proportions).
#'
#' @return A tibble of analysis results, one row per indicator (or one row per
#'   disaggregation group when disaggregation is specified).
#' @noRd
phr_calc_survey_from_plan <- function(design,
                                        analysis_plan,
                                        high_design_complexity = FALSE) {
  origin <- "phr_calc_survey_from_plan"
  phrutils::phr_message(origin, "Starting execution of data analysis plan...")

  # --- Validation
  phrutils::phr_try({
    if (is.null(design)) phr_error(origin, "Survey design object is NULL.")
    if (!inherits(analysis_plan, "data.frame")) {
      phr_error(origin, "analysis_plan must be a data.frame or tibble.")
    }

    required_cols <- c(
      "indicator_name", "calculation", "var_name",
      "denom_var", "disaggregation", "multiplier", "indicator_unit"
    )
    missing_cols <- setdiff(required_cols, names(analysis_plan))
    if (length(missing_cols) > 0) {
      phr_error(origin, paste("Analysis plan missing columns:", paste(missing_cols, collapse = ", ")))
    }
  },
  on_error = "abort",
  origin = origin,
  hint = "Check that your data_analysis_plan follows the expected template.")

  # --- Initialize results holder
  results <- list()

  # --- Loop through analysis plan
  for (i in seq_len(nrow(analysis_plan))) {
    row <- analysis_plan[i, ]
    indicator <- row$indicator_name
    calc_type <- tolower(trimws(row$calculation))
    var_name  <- row$var_name
    denom_var <- row$denom_var
    disagg    <- if (!is.null(row$disaggregation) && !is.na(row$disaggregation)) row$disaggregation else NULL
    mult      <- ifelse(is.null(row$multiplier) || is.na(row$multiplier), 1, row$multiplier)
    unit      <- ifelse(is.null(row$indicator_unit) || is.na(row$indicator_unit), "", row$indicator_unit)

    phrutils::phr_message(origin, paste0("Running [", i, "/", nrow(analysis_plan), "]: ", indicator, " (", calc_type, ")"))

    # --- Internal helper for one calculation
    run_single_calc <- function(design_subset, group_value = NA_character_) {
      result_i <- phrutils::phr_try({
        if (calc_type %in% c("prop", "proportion")) {
          phr_calc_survey_prop_single(
            design = design_subset,
            var_name = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )

        } else if (calc_type %in% c("mean", "average")) {
          phr_calc_survey_mean_single(
            design = design_subset,
            var_name = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )

        } else if (calc_type %in% c("median")) {
          phr_calc_survey_median_single(
            design = design_subset,
            var_name = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )

        } else if (calc_type %in% c("ratio", "rate")) {
          phr_calc_survey_ratio_single(
            design = design_subset,
            numerator_var = var_name,
            denominator_var = denom_var,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )

        }
        else if (calc_type %in% c("categorical", "category", "cat")) {
          phr_calc_survey_categorical_single(
            design = design_subset,
            var_name = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )
        } else if (calc_type %in% c("select_multiple_cat")) {
          phr_calc_multiple_choice_cat(
            design = design_subset,
            var_name = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            multiplier = mult,
            group_name_label = group_value,
            high_design_complexity = high_design_complexity
          )
        } else {
          phrutils::phr_warning(origin, paste("Unknown calculation type for indicator:", indicator))
          tibble::tibble(
            variable = var_name,
            indicator_name = indicator,
            indicator_unit = unit,
            point.estimate = NA_real_,
            lower_ci = NA_real_,
            upper_ci = NA_real_,
            ci_method = NA_character_,
            n_unweighted = NA_real_,
            n_weighted = NA_real_,
            denom_unweighted = NA_real_,
            denom_weighted = NA_real_,
            n_eff = NA_real_,
            deff = NA_real_,
            group_name = group_value,
            note = "unknown calculation type"
          )
        }
      },
      on_error = "warn",
      origin = paste0(origin, ":", indicator),
      hint = paste("Check variable", var_name, "and denominator", denom_var, "if applicable."))

      # Add traceability columns and reorder to canonical column order:
      # plan_row, variable, indicator_name, indicator_unit, group_name,
      # disaggregation_value, calculation, point.estimate, lower_ci,
      # upper_ci, ci_method, n_unweighted, n_weighted, denom_unweighted,
      # denom_weighted, n_eff, deff, note
      result_i$plan_row             <- i
      result_i$disaggregation_value <- group_value
      result_i$calculation          <- calc_type

      col_order <- c(
        "plan_row", "variable", "indicator_name", "indicator_unit",
        "group_name", "disaggregation_value", "calculation",
        "point.estimate", "lower_ci", "upper_ci", "ci_method",
        "n_unweighted", "n_weighted", "denom_unweighted", "denom_weighted",
        "n_eff", "deff", "note"
      )
      present <- col_order[col_order %in% names(result_i)]
      extra   <- setdiff(names(result_i), col_order)
      result_i[c(present, extra)]
    }

    # --- Handle disaggregated analysis
    if (!is.null(disagg) && disagg %in% names(design$variables)) {
      group_levels <- unique(na.omit(design$variables[[disagg]]))
      phrutils::phr_message(origin, paste("Disaggregating by:", disagg, "(", length(group_levels), "groups )"))

      group_results <- purrr::map_dfr(group_levels, function(g) {
        subset_design <- tryCatch(
          subset(design, design$variables[[disagg]] == g),
          error = function(e) {
            phrutils::phr_warning(origin, paste("Subset failed for", disagg, "=", g))
            NULL
          }
        )
        if (!is.null(subset_design)) {
          run_single_calc(subset_design, group_value = as.character(g))
        } else {
          tibble::tibble()
        }
      })

      results[[i]] <- group_results

    } else {
      results[[i]] <- run_single_calc(design, group_value = "Overall")
    }
  }

  # --- Bind and return
  out <- tryCatch(
    dplyr::bind_rows(results),
    error = function(e) {
      phrutils::phr_warning(origin, paste("Binding failed:", e$message))
      dplyr::tibble()
    }
  )

  phrutils::phr_message(origin, paste("Completed", nrow(analysis_plan), "indicators successfully."))
  return(out)
}

#' Select a confidence interval method for a survey estimate
#'
#' Determines the most appropriate confidence interval (CI) method based on
#' sample size, effective sample size, design effect, whether the estimate is
#' for a rare proportion, and whether the variable is numeric or a ratio.
#'
#' @param n_unweighted Unweighted sample size (number of respondents with
#'   non-missing values).
#' @param n_eff Effective sample size (\eqn{\bar{w}^2 / \sum w_i^2}).
#' @param p_estimate Quick (unweighted) mean or proportion estimate used to
#'   detect rare/extreme proportions.
#' @param deff Design effect (DEFF); triggers complex-design CI when > 5.
#' @param lonely_psu Logical; if \code{TRUE}, a strata with a single PSU was
#'   detected and \code{survey.lonely.psu} is set to \code{"adjust"}.
#' @param high_design_complexity Logical; if \code{TRUE}, forces complex-design
#'   CI selection regardless of DEFF.
#' @param is_numeric Logical; \code{TRUE} for continuous numeric variables
#'   (mean/median). When \code{TRUE} the Wald CI for means is used.
#' @param is_ratio Logical; \code{TRUE} for ratio/rate indicators. Activates
#'   the Taylor-linearised (default) or replicate-weight CI path.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{method}}{Character; one of \code{"design-wald"},
#'       \code{"design-logit"}, \code{"mean-wald"}, \code{"design-taylor"},
#'       \code{"design-replicate"}, or \code{"design-meanbased"}.}
#'     \item{\code{note}}{Character; human-readable summary of the decision.}
#'     \item{\code{flags}}{Named list of logical diagnostic flags.}
#'   }
#' @noRd
phr_pick_ci_method <- function(n_unweighted = NULL,
                                 n_eff = NULL,
                                 p_estimate = NULL,
                                 deff = NULL,
                                 lonely_psu = FALSE,
                                 high_design_complexity = FALSE,
                                 is_numeric = FALSE,
                                 is_ratio = FALSE) {

  flags <- c(
    flag_small_n    = FALSE,
    flag_low_neff   = FALSE,
    flag_rare_p     = FALSE,
    flag_high_deff  = FALSE,
    flag_lonely_psu = FALSE,
    flag_var_failed = FALSE
  )
  notes <- c()

  # --- Handle missing inputs gracefully
  if (is.null(n_unweighted)) n_unweighted <- NA_real_
  if (is.null(n_eff)) n_eff <- NA_real_
  if (is.null(p_estimate)) p_estimate <- NA_real_
  if (is.null(deff)) deff <- NA_real_

  # --- Lonely PSU handling
  if (isTRUE(lonely_psu)) {
    options(survey.lonely.psu = "adjust")
    flags["flag_lonely_psu"] <- TRUE
    notes <- c(notes, "lonely PSU adjusted (variance mode = 'adjust')")
  }

  # --- Trigger checks
  if (is.finite(n_unweighted) && n_unweighted < 30) {
    flags["flag_small_n"] <- TRUE
    notes <- c(notes, "small sample size (n < 30)")
  }

  if (is.finite(n_eff) && n_eff < 10) {
    flags["flag_low_neff"] <- TRUE
    notes <- c(notes, "low effective sample size (n_eff < 10)")
  }

  if (!is_numeric && is.finite(p_estimate) && (p_estimate < 0.05 || p_estimate > 0.95)) {
    # Rare proportions only apply for binary variables
    flags["flag_rare_p"] <- TRUE
    notes <- c(notes, "rare/extreme proportion (p < 0.05 or > 0.95)")
  }

  if (isTRUE(high_design_complexity) || (is.finite(deff) && deff > 5)) {
    flags["flag_high_deff"] <- TRUE
    notes <- c(notes, "high design effect or complex design (DEFF > 5)")
  }


  # Decision logic for general (non-ratio) cases

  if (!is_ratio) {
    if (is_numeric) {
      method <- "mean-wald"
      notes <- c(notes, "numeric variable: Wald CI for mean selected")
    } else if (isTRUE(flags["flag_rare_p"])) {
      # Extreme proportions: Wilson Score interval handles near-0/1 proportions
      # better than logit or Wald across all sample sizes
      method <- "wilson"
      notes <- c(notes, "Wilson Score CI selected (extreme proportion near 0 or 1)")
    } else if (any(flags[c("flag_small_n", "flag_low_neff", "flag_high_deff")])) {
      method <- "design-logit"
      notes <- c(notes, "logit-transformed CI selected")
    } else {
      method <- "design-wald"
      notes <- c(notes, "Wald CI (default) selected")
    }
  } else {

    # Decision logic for RATIO cases


    # Default: Taylor-linearized (survey::svyratio)
    method <- "design-taylor"
    notes <- c(notes, "ratio: default Taylor-linearized variance")

    # Escalate if design is complex or small sample
    if (any(flags[c("flag_small_n", "flag_low_neff", "flag_high_deff")])) {
      method <- "design-replicate"
      notes <- c(notes, "ratio: replicate-based variance suggested (small n / high DEFF)")
    }

    # If variance known to fail or sample too small → fallback to simple mean-based ratio
    if (is.finite(n_unweighted) && n_unweighted < 10) {
      method <- "design-meanbased"
      notes <- c(notes, "ratio: fallback to mean-based (unweighted) ratio due to very small sample")
    }
  }


  # Output

  note_text <- paste(unique(notes), collapse = "; ")

  return(list(
    method = method,
    note   = note_text,
    flags  = as.list(flags)
  ))
}

#' Calculate a survey-weighted proportion for a single binary variable
#'
#' Validates that the variable is binary (0/1/NA), selects the appropriate CI
#' method via \code{\link{phr_pick_ci_method}}, and estimates the weighted
#' proportion using \code{survey::svyciprop()}.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param var_name Character; name of the binary (0/1) variable to analyse.
#' @param indicator_name Character; human-readable name for the indicator.
#' @param indicator_unit Character; unit label (default \code{"\%"}).
#' @param multiplier Numeric; scale factor applied to the point estimate and
#'   CI bounds (default \code{100} to express as a percentage).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_pick_ci_method}}.
#'
#' @return A one-row tibble with columns \code{variable}, \code{indicator_name},
#'   \code{indicator_unit}, \code{point.estimate}, \code{lower_ci},
#'   \code{upper_ci}, \code{ci_method}, \code{n_unweighted}, \code{n_weighted},
#'   \code{denom_unweighted}, \code{denom_weighted}, \code{n_eff}, \code{deff},
#'   \code{group_name}, and \code{note}.
#' @noRd
phr_calc_survey_prop_single <- function(design,
                                          var_name,
                                          indicator_name = "Proportion",
                                          indicator_unit = "%",
                                          multiplier = 100,
                                          group_name_label = "Overall",
                                          high_design_complexity = FALSE) {
  origin <- "phr_calc_survey_prop_single"
  phrutils::phr_message(origin, paste("Starting proportion calculation for:", indicator_name))


  # 1. Input Validation

  if (is.null(design) || !inherits(design, c("survey.design", "srvyr_svy", "tbl_svy"))) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid or missing survey design"
    ))
  }

  if (!var_name %in% names(design$variables)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "variable not found in dataset"
    ))
  }

  var_sym <- rlang::sym(var_name)
  data <- design$variables


  # 2. Binary Validation for Proportions

  unique_vals <- unique(stats::na.omit(data[[var_name]]))
  valid_binary <- all(unique_vals %in% c(0, 1))
  if (!valid_binary) {
    phrutils::phr_warning(origin, paste0("Variable '", var_name,
                                 "' is not binary (contains values other than 0, 1, or NA)."))
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = sum(!is.na(data[[var_name]])),
      n_weighted = NA_real_,
      denom_unweighted = sum(!is.na(data[[var_name]])),
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid input: variable not binary (expected 0/1/NA)"
    ))
  }


  # 3. Prepare design and denominators

  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)

  dsn_sum <- design_srvyr |>
    srvyr::summarise(
      n_event_weighted   = srvyr::survey_total(!!var_sym, vartype = NULL, na.rm = TRUE),
      n_event_unweighted = sum(!!var_sym, na.rm = TRUE),
      denom_weighted     = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      denom_unweighted   = sum(!is.na(!!var_sym))
    )

  n_event_weighted   <- as.numeric(dsn_sum$n_event_weighted)
  n_event_unweighted <- as.numeric(dsn_sum$n_event_unweighted)
  denom_weighted     <- as.numeric(dsn_sum$denom_weighted)
  denom_unweighted   <- as.numeric(dsn_sum$denom_unweighted)

  # Early return when there are no non-NA observations (e.g. all records filtered
  # out by an age-range restriction such as mfaz columns outside 6-59 months).
  # survey::svyciprop would otherwise throw "Argument mu must be a nonempty
  # numeric vector".
  if (is.na(denom_unweighted) || denom_unweighted == 0) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = 0,
      n_weighted = NA_real_,
      denom_unweighted = 0,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "no valid (non-NA) observations in this group"
    ))
  }

  # Effective sample size
  w <- tryCatch(stats::weights(design), error = function(e) rep(1, denom_unweighted))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_

  # Quick mean estimate (for CI policy decision)
  quick_est <- mean(data[[var_name]], na.rm = TRUE)


  # 4. Diagnostics and CI Policy

  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)

  policy <- phr_pick_ci_method(
    n_unweighted = denom_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA_real_,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity
  )


  # 5. Estimate weighted proportion
  # For "wilson", use method="mean" to obtain the weighted point estimate;
  # CI bounds are computed analytically in step 7.

  est <- phrutils::phr_try({
    fmla <- as.formula(paste0("~I(", var_name, " == 1)"))
    suppressWarnings({
      if (policy$method == "design-logit") {
        survey::svyciprop(fmla, design, method = "logit", level = 0.95, na.rm = TRUE)
      } else {
        survey::svyciprop(fmla, design, method = "mean", level = 0.95, na.rm = TRUE)
      }
    })
  },
  on_error = "warn",
  origin = origin,
  hint = paste("Error computing survey proportion for", var_name))


  # 6. Handle failed estimation

  if (is.null(est)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = policy$method,
      n_unweighted = n_event_unweighted,
      n_weighted = n_event_weighted,
      denom_unweighted = denom_unweighted,
      denom_weighted = denom_weighted,
      n_eff = n_eff,
      deff = NA_real_,
      group_name = group_name_label,
      note = paste("estimation failed;", policy$note)
    ))
  }


  # 6b. Compute design effect via svymean (svyciprop does not support deff parameter)

  prop_deff <- tryCatch({
    deff_est <- survey::svymean(
      as.formula(paste0("~", var_name)),
      design,
      na.rm = TRUE,
      deff = "replace"
    )
    as.numeric(attr(deff_est, "deff"))
  }, error = function(e) NA_real_)


  # 7. Extract and prepare results

  p_est <- as.numeric(coef(est))

  if (policy$method == "wilson") {
    # Wilson Score Interval using effective sample size to account for design
    z   <- stats::qnorm(0.975)
    n_w <- if (is.finite(n_eff) && n_eff > 0) n_eff else denom_unweighted
    if (is.finite(n_w) && n_w > 0) {
      denom_w  <- 1 + z^2 / n_w
      center_w <- (p_est + z^2 / (2 * n_w)) / denom_w
      half_w   <- z * sqrt(p_est * (1 - p_est) / n_w + z^2 / (4 * n_w^2)) / denom_w
      lower_ci <- center_w - half_w
      upper_ci <- center_w + half_w
    } else {
      lower_ci <- NA_real_
      upper_ci <- NA_real_
    }
    deff <- prop_deff
  } else {
    ci <- tryCatch(as.numeric(confint(est)[1, ]), error = function(e) rep(NA_real_, 2))
    lower_ci <- ifelse(length(ci) >= 1, ci[1], NA_real_)
    upper_ci <- ifelse(length(ci) >= 2, ci[2], NA_real_)
    deff <- prop_deff
  }

  # Floor lower_ci at 0
  if (!is.na(lower_ci) && lower_ci < 0) lower_ci <- 0


  # 8. Construct standardized tibble output safely


  out <- tibble::tibble(
    variable           = safe_chr(var_name),
    indicator_name     = safe_chr(indicator_name),
    indicator_unit     = safe_chr(indicator_unit),
    point.estimate     = round(safe_num(p_est) * multiplier, 2),
    lower_ci           = round(safe_num(lower_ci) * multiplier, 2),
    upper_ci           = round(safe_num(upper_ci) * multiplier, 2),
    ci_method          = safe_chr(policy$method),
    n_unweighted       = safe_num(n_event_unweighted),
    n_weighted         = safe_num(n_event_weighted),
    denom_unweighted   = safe_num(denom_unweighted),
    denom_weighted     = safe_num(denom_weighted),
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    group_name         = safe_chr(group_name_label),
    note               = safe_chr(policy$note)
  )

  phrutils::phr_message(origin, paste("Completed proportion estimate for:", indicator_name))
  return(out)
}

#' Calculate a survey-weighted mean for a single numeric variable
#'
#' Validates that the variable is numeric, selects the CI method via
#' \code{\link{phr_pick_ci_method}} (always \code{"mean-wald"} for numeric
#' variables), and estimates the weighted mean using
#' \code{survey::svymean()}.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param var_name Character; name of the numeric variable to analyse.
#' @param indicator_name Character; human-readable name for the indicator.
#' @param indicator_unit Character; unit label (default \code{""}).
#' @param multiplier Numeric; scale factor applied to the point estimate and
#'   CI bounds (default \code{1}).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_pick_ci_method}}.
#'
#' @return A one-row tibble with columns \code{variable}, \code{indicator_name},
#'   \code{indicator_unit}, \code{point.estimate}, \code{lower_ci},
#'   \code{upper_ci}, \code{ci_method}, \code{n_unweighted}, \code{n_weighted},
#'   \code{denom_unweighted}, \code{denom_weighted}, \code{n_eff}, \code{deff},
#'   \code{group_name}, and \code{note}.
#' @noRd
phr_calc_survey_mean_single <- function(design,
                                          var_name,
                                          indicator_name = "Mean",
                                          indicator_unit = "",
                                          multiplier = 1,
                                          group_name_label = "Overall",
                                          high_design_complexity = FALSE) {
  origin <- "phr_calc_survey_mean_single"
  phrutils::phr_message(origin, paste("Starting mean calculation for:", indicator_name))


  # 1. Input Validation

  if (is.null(design) || !inherits(design, c("survey.design", "srvyr_svy", "tbl_svy"))) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid or missing survey design"
    ))
  }

  if (!var_name %in% names(design$variables)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "variable not found in dataset"
    ))
  }

  var_sym <- rlang::sym(var_name)
  data <- design$variables


  # 2. Numeric Validation for Means

  if (!is.numeric(data[[var_name]])) {
    phrutils::phr_warning(origin, paste0("Variable '", var_name, "' is not numeric."))
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = sum(!is.na(data[[var_name]])),
      n_weighted = NA_real_,
      denom_unweighted = sum(!is.na(data[[var_name]])),
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid input: variable not numeric"
    ))
  }


  # 3. Prepare design and denominators

  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)

  dsn_sum <- design_srvyr |>
    srvyr::summarise(
      n_weighted       = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      n_unweighted     = sum(!is.na(!!var_sym)),
      denom_weighted   = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      denom_unweighted = sum(!is.na(!!var_sym))
    )

  n_weighted       <- as.numeric(dsn_sum$n_weighted)
  n_unweighted     <- as.numeric(dsn_sum$n_unweighted)
  denom_weighted   <- as.numeric(dsn_sum$denom_weighted)
  denom_unweighted <- as.numeric(dsn_sum$denom_unweighted)

  # Effective sample size
  w <- tryCatch(stats::weights(design), error = function(e) rep(1, denom_unweighted))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_

  # Quick mean estimate (for CI policy decision)
  quick_est <- mean(data[[var_name]], na.rm = TRUE)


  # 4. Diagnostics and CI Policy

  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)

  policy <- phr_pick_ci_method(
    n_unweighted = denom_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA_real_,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity,
    is_numeric = TRUE
  )


  # 5. Estimate weighted mean

  est <- phrutils::phr_try({
    fmla <- as.formula(paste0("~", var_name))
    suppressWarnings({
      survey::svymean(fmla, design, na.rm = TRUE, deff = "replace")
    })
  },
  on_error = "warn",
  origin = origin,
  hint = paste("Error computing survey mean for", var_name))


  # 6. Handle failed estimation

  if (is.null(est)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = policy$method,
      n_unweighted = n_unweighted,
      n_weighted = n_weighted,
      denom_unweighted = denom_unweighted,
      denom_weighted = denom_weighted,
      n_eff = n_eff,
      deff = NA_real_,
      group_name = group_name_label,
      note = paste("estimation failed;", policy$note)
    ))
  }


  # 7. Extract and prepare results

  mean_est <- as.numeric(coef(est))
  se <- as.numeric(sqrt(diag(vcov(est))))
  ci_range <- 1.96 * se  # Wald 95% CI
  lower_ci <- mean_est - ci_range
  upper_ci <- mean_est + ci_range
  deff <- tryCatch(as.numeric(attr(est, "deff")), error = function(e) NA_real_)


  # 8. Construct standardized tibble output safely


  out <- tibble::tibble(
    variable           = safe_chr(var_name),
    indicator_name     = safe_chr(indicator_name),
    indicator_unit     = safe_chr(indicator_unit),
    point.estimate     = round(safe_num(mean_est) * multiplier, 2),
    lower_ci           = round(safe_num(lower_ci) * multiplier, 2),
    upper_ci           = round(safe_num(upper_ci) * multiplier, 2),
    ci_method          = safe_chr(policy$method),
    n_unweighted       = safe_num(n_unweighted),
    n_weighted         = safe_num(n_weighted),
    denom_unweighted   = safe_num(denom_unweighted),
    denom_weighted     = safe_num(denom_weighted),
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    group_name         = safe_chr(group_name_label),
    note               = safe_chr(policy$note)
  )

  phrutils::phr_message(origin, paste("Completed mean estimate for:", indicator_name))
  return(out)
}

#' Calculate a survey-weighted median for a single numeric variable
#'
#' Validates that the variable is numeric, selects the CI method via
#' \code{\link{phr_pick_ci_method}}, and estimates the weighted median using
#' \code{survey::svyquantile()} at quantile 0.5.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param var_name Character; name of the numeric variable to analyse.
#' @param indicator_name Character; human-readable name for the indicator.
#' @param indicator_unit Character; unit label (default \code{""}).
#' @param multiplier Numeric; scale factor applied to the point estimate and
#'   CI bounds (default \code{1}).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_pick_ci_method}}.
#'
#' @return A one-row tibble with columns \code{variable}, \code{indicator_name},
#'   \code{indicator_unit}, \code{point.estimate}, \code{lower_ci},
#'   \code{upper_ci}, \code{ci_method}, \code{n_unweighted}, \code{n_weighted},
#'   \code{denom_unweighted}, \code{denom_weighted}, \code{n_eff}, \code{deff},
#'   \code{group_name}, and \code{note}.
#' @noRd
phr_calc_survey_median_single <- function(design,
                                            var_name,
                                            indicator_name = "Median",
                                            indicator_unit = "",
                                            multiplier = 1,
                                            group_name_label = "Overall",
                                            high_design_complexity = FALSE) {
  origin <- "phr_calc_survey_median_single"
  phrutils::phr_message(origin, paste("Starting median calculation for:", indicator_name))



  # 1. Input Validation

  if (is.null(design) || !inherits(design, c("survey.design", "srvyr_svy", "tbl_svy"))) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid or missing survey design"
    ))
  }

  if (!var_name %in% names(design$variables)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "variable not found in dataset"
    ))
  }

  var_sym <- rlang::sym(var_name)
  data <- design$variables


  # 2. Numeric Validation

  if (!is.numeric(data[[var_name]])) {
    phrutils::phr_warning(origin, paste0("Variable '", var_name, "' is not numeric."))
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = sum(!is.na(data[[var_name]])),
      n_weighted = NA_real_,
      denom_unweighted = sum(!is.na(data[[var_name]])),
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid input: variable not numeric"
    ))
  }


  # 3. Prepare design and denominators

  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)

  dsn_sum <- design_srvyr |>
    srvyr::summarise(
      n_weighted       = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      n_unweighted     = sum(!is.na(!!var_sym)),
      denom_weighted   = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      denom_unweighted = sum(!is.na(!!var_sym))
    )

  n_weighted       <- as.numeric(dsn_sum$n_weighted)
  n_unweighted     <- as.numeric(dsn_sum$n_unweighted)
  denom_weighted   <- as.numeric(dsn_sum$denom_weighted)
  denom_unweighted <- as.numeric(dsn_sum$denom_unweighted)

  # Effective sample size
  w <- tryCatch(stats::weights(design), error = function(e) rep(1, denom_unweighted))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_

  # Quick numeric summary for CI policy
  quick_est <- median(data[[var_name]], na.rm = TRUE)


  # 4. Diagnostics and CI Policy

  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)

  policy <- phr_pick_ci_method(
    n_unweighted = denom_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA_real_,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity,
    is_numeric = TRUE
  )


  # 5. Estimate weighted median (survey quantile)

  est <- phrutils::phr_try({
    fmla <- as.formula(paste0("~", var_name))
    suppressWarnings({
      survey::svyquantile(fmla, design, quantiles = 0.5, ci = TRUE, na.rm = TRUE)[[1]]
    })
  },
  on_error = "warn",
  origin = origin,
  hint = paste("Error computing survey median for", var_name))


  # 6. Handle failed estimation

  if (is.null(est)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = policy$method,
      n_unweighted = n_unweighted,
      n_weighted = n_weighted,
      denom_unweighted = denom_unweighted,
      denom_weighted = denom_weighted,
      n_eff = n_eff,
      deff = NA_real_,
      group_name = group_name_label,
      note = paste("estimation failed;", policy$note)
    ))
  }


  # 7. Extract and prepare results

  # Extract median safely
  med_est <- tryCatch(as.numeric(est[1]), error = function(e) NA_real_)

  # CIs
  ci_raw <- tryCatch(attr(est, "ci"), error = function(e) NULL)

  if (is.null(ci_raw) || nrow(ci_raw) == 0) {
    lower_ci <- NA_real_
    upper_ci <- NA_real_
  } else {
    ci_vals <- suppressWarnings(as.numeric(ci_raw[1, ]))
    lower_ci <- ifelse(is.finite(ci_vals[1]), ci_vals[1], NA_real_)
    upper_ci <- ifelse(is.finite(ci_vals[2]), ci_vals[2], NA_real_)
  }

  # Extract deff safely
  deff <- tryCatch(as.numeric(attr(est, "deff")), error = function(e) NA_real_)

  # 8. Construct standardized tibble output safely

  out <- tibble::tibble(
    variable           = safe_chr(var_name),
    indicator_name     = safe_chr(indicator_name),
    indicator_unit     = safe_chr(indicator_unit),
    point.estimate     = round(safe_num(est[1][1]) * multiplier, 2),
    lower_ci           = round(safe_num(est[1][2]) * multiplier, 2),
    upper_ci           = round(safe_num(est[1][3]) * multiplier, 2),
    ci_method          = safe_chr(policy$method),
    n_unweighted       = safe_num(n_unweighted),
    n_weighted         = safe_num(n_weighted),
    denom_unweighted   = safe_num(denom_unweighted),
    denom_weighted     = safe_num(denom_weighted),
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    group_name         = safe_chr(group_name_label),
    note               = safe_chr(policy$note)
  )

  phrutils::phr_message(origin, paste("Completed median estimate for:", indicator_name))
  return(out)
}

#' Calculate a survey-weighted ratio or rate
#'
#' Validates that both the numerator and denominator are numeric, selects the
#' CI method (Taylor-linearised by default, replicates for complex designs, and
#' a simple mean-based fallback for very small samples), and calls
#' \code{survey::svyratio()}.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param numerator_var Character; name of the numeric numerator variable.
#' @param denominator_var Character; name of the numeric denominator variable.
#' @param indicator_name Character; human-readable name for the indicator.
#' @param indicator_unit Character; unit label (default \code{""}).
#' @param multiplier Numeric; scale factor applied to the point estimate and
#'   CI bounds (default \code{1}).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_pick_ci_method}}.
#'
#' @return A one-row tibble with columns \code{variable}, \code{indicator_name},
#'   \code{indicator_unit}, \code{point.estimate}, \code{lower_ci},
#'   \code{upper_ci}, \code{ci_method}, \code{n_unweighted}, \code{n_weighted},
#'   \code{denom_unweighted}, \code{denom_weighted}, \code{n_eff}, \code{deff},
#'   \code{group_name}, and \code{note}.
#' @noRd
phr_calc_survey_ratio_single <- function(design,
                                           numerator_var,
                                           denominator_var,
                                           indicator_name = "Ratio",
                                           indicator_unit = "",
                                           multiplier = 1,
                                           group_name_label = "Overall",
                                           high_design_complexity = FALSE) {
  origin <- "phr_calc_survey_ratio_single"
  phrutils::phr_message(origin, paste("Starting ratio calculation for:", indicator_name))


  # 1. Validation

  if (is.null(design) || !inherits(design, c("survey.design", "srvyr_svy", "tbl_svy"))) {
    return(tibble::tibble(
      variable = paste0(numerator_var, "/", denominator_var),
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid or missing survey design"
    ))
  }

  data <- design$variables

  if (!numerator_var %in% names(data) || !denominator_var %in% names(data)) {
    missing_vars <- setdiff(c(numerator_var, denominator_var), names(data))
    return(tibble::tibble(
      variable = paste0(numerator_var, "/", denominator_var),
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = paste("missing variable(s):", paste(missing_vars, collapse = ", "))
    ))
  }

  num_sym <- rlang::sym(numerator_var)
  den_sym <- rlang::sym(denominator_var)


  # 2. Numeric validation

  if (!is.numeric(data[[numerator_var]]) || !is.numeric(data[[denominator_var]])) {
    phrutils::phr_warning(origin, paste("Numerator and denominator must be numeric:", numerator_var, denominator_var))
    return(tibble::tibble(
      variable = paste0(numerator_var, "/", denominator_var),
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      group_name = group_name_label,
      note = "invalid input: numerator or denominator not numeric"
    ))
  }


  # 3. Weighted and unweighted counts

  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)

  dsn_sum <- design_srvyr |>
    srvyr::summarise(
      n_weighted       = srvyr::survey_total(!!num_sym, vartype = NULL, na.rm = TRUE),
      n_unweighted     = sum(!!num_sym, na.rm = TRUE),
      denom_weighted   = srvyr::survey_total(!!den_sym, vartype = NULL, na.rm = TRUE),
      denom_unweighted = sum(!!den_sym, na.rm = TRUE)
    )

  n_weighted       <- as.numeric(dsn_sum$n_weighted)
  n_unweighted     <- as.numeric(dsn_sum$n_unweighted)
  denom_weighted   <- as.numeric(dsn_sum$denom_weighted)
  denom_unweighted <- as.numeric(dsn_sum$denom_unweighted)

  w <- tryCatch(stats::weights(design), error = function(e) rep(1, nrow(data)))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_

  # Quick mean ratio
  quick_est <- mean(data[[numerator_var]] / data[[denominator_var]], na.rm = TRUE)


  # 4. Lonely PSU and policy selection

  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)

  policy <- phr_pick_ci_method(
    n_unweighted = denom_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA_real_,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity,
    is_numeric = TRUE,
    is_ratio = TRUE
  )

  phrutils::phr_message(origin, paste("Policy decision:", policy$method))


  # 5. Estimation logic by method

  ratio_est <- lower_ci <- upper_ci <- deff <- NA_real_
  note <- policy$note

  if (policy$method == "design-taylor") {
    phrutils::phr_message(origin, "Using Taylor-linearized variance (svyratio).")

    est <- phrutils::phr_try({
      survey::svyratio(
        numerator = as.formula(paste0("~", numerator_var)),
        denominator = as.formula(paste0("~", denominator_var)),
        design = design
      )
    }, on_error = "warn", origin = origin, hint = "Taylor variance ratio failed")

    if (!is.null(est)) {
      vc <- tryCatch(vcov(est), error = function(e) NA)
      if (anyNA(vc) || any(vc <= 0)) {
        phrutils::phr_warning(origin, "Variance estimation failed for svyratio (Taylor).")
        policy$method <- "design-meanbased"
        note <- paste(note, "Taylor variance failed; fallback to mean-based ratio.")
      } else {
        ratio_est <- as.numeric(coef(est))
        se <- sqrt(vc)
        ci_range <- 1.96 * se
        lower_ci <- ratio_est - ci_range
        upper_ci <- ratio_est + ci_range
      }
    }
  }

  if (policy$method == "design-replicate") {
    phrutils::phr_message(origin, "Attempting replicate-weight variance method.")
    rep_design <- tryCatch(survey::as.svrepdesign(design, type = "bootstrap"), error = function(e) NULL)
    if (!is.null(rep_design)) {
      est <- phrutils::phr_try({
        survey::svyratio(
          numerator = as.formula(paste0("~", numerator_var)),
          denominator = as.formula(paste0("~", denominator_var)),
          design = rep_design
        )
      }, on_error = "warn", origin = origin, hint = "Replicate variance ratio failed")
      if (!is.null(est)) {
        ratio_est <- as.numeric(coef(est))
        se <- sqrt(vcov(est))
        ci_range <- 1.96 * se
        lower_ci <- ratio_est - ci_range
        upper_ci <- ratio_est + ci_range
      }
    } else {
      phrutils::phr_warning(origin, "Replicate design could not be created; using mean-based fallback.")
      policy$method <- "design-meanbased"
      note <- paste(note, "Replicate design unavailable; fallback to mean-based ratio.")
    }
  }

  if (policy$method == "design-meanbased") {
    phrutils::phr_message(origin, "Using fallback: mean-based ratio.")
    ratio_est <- mean(data[[numerator_var]], na.rm = TRUE) / mean(data[[denominator_var]], na.rm = TRUE)
    lower_ci <- NA_real_
    upper_ci <- NA_real_
  }

  # 5b. Compute design effect separately (svyratio does not support deff)

  deff <- tryCatch({
    deff_est <- survey::svymean(
      as.formula(paste0("~", numerator_var)),
      design,
      na.rm = TRUE,
      deff = "replace"
    )
    as.numeric(attr(deff_est, "deff"))
  }, error = function(e) NA_real_)

  # Floor lower_ci at 0
  if (!is.na(lower_ci) && lower_ci < 0) lower_ci <- 0

  # 6. Output safely

  out <- tibble::tibble(
    variable           = safe_chr(paste0(numerator_var, "/", denominator_var)),
    indicator_name     = safe_chr(indicator_name),
    indicator_unit     = safe_chr(indicator_unit),
    point.estimate     = round(safe_num(ratio_est) * multiplier, 2),
    lower_ci           = round(safe_num(lower_ci) * multiplier, 2),
    upper_ci           = round(safe_num(upper_ci) * multiplier, 2),
    ci_method          = safe_chr(policy$method),
    n_unweighted       = safe_num(n_unweighted),
    n_weighted         = safe_num(n_weighted),
    denom_unweighted   = safe_num(denom_unweighted),
    denom_weighted     = safe_num(denom_weighted),
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    group_name         = safe_chr(group_name_label),
    note               = safe_chr(note)
  )

  phrutils::phr_message(origin, paste("Completed ratio estimate for:", indicator_name))
  return(out)
}

#' Calculate survey-weighted proportions for all levels of a categorical variable
#'
#' Converts the categorical variable to a factor, then calls
#' \code{\link{phr_calc_survey_prop_single}} for each level using a temporary
#' binary indicator.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param var_name Character; name of the categorical variable to analyse.
#' @param indicator_name Character; human-readable name for the indicator.
#'   Each level result is labelled as \code{"\{indicator_name\} - \{level\}"}.
#' @param indicator_unit Character; unit label (default \code{"\%"}).
#' @param multiplier Numeric; scale factor (default \code{100}).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_calc_survey_prop_single}}.
#'
#' @return A tibble with one row per category level, with the same columns as
#'   \code{\link{phr_calc_survey_prop_single}}.
#' @noRd
phr_calc_survey_categorical_single <- function(
    design,
    var_name,
    indicator_name,
    indicator_unit = "%",
    multiplier = 100,
    group_name_label = "Overall",
    high_design_complexity = FALSE
) {

  data <- design$variables[[var_name]]

  # Ensure factor
  if (!is.factor(data)) data <- factor(data)

  categories <- levels(data)

  results <- purrr::map_dfr(categories, function(cat) {

    # Temporary binary variable
    design_tmp <- design
    design_tmp$variables$.tmp_cat <- ifelse(data == cat, 1, 0)

    out <- phr_calc_survey_prop_single(
      design = design_tmp,
      var_name = ".tmp_cat",
      indicator_name = paste0(indicator_name, " - ", cat),
      indicator_unit = indicator_unit,
      multiplier = multiplier,
      group_name_label = group_name_label,
      high_design_complexity = high_design_complexity
    )

    # 🔧 Fix: overwrite variable name to original var_name
    out$variable <- var_name

    out
  })

  return(results)
}

#' Calculate survey-weighted proportions for each response in a select-multiple variable
#'
#' For select-multiple (multi-select) columns where a single cell may contain
#' several space-separated responses (e.g. \code{"fed_other_milk lack_of_time"}),
#' this function:
#' \enumerate{
#'   \item Uses \code{grepl} to identify all unique valid responses across all
#'         non-missing values in the column.
#'   \item Creates a temporary binary indicator per response
#'         (1 = response present, 0 = absent) for all non-missing records.
#'   \item Calls \code{\link{phr_calc_survey_prop_single}} for each response to
#'         obtain the survey-weighted proportion of records that include that
#'         response.
#' }
#'
#' The denominator for each response is the number of records with a non-missing
#' value in \code{var_name} (i.e. records that answered the question), not the
#' total number of records.
#'
#' @param design A \code{srvyr} or \code{survey} design object.
#' @param var_name Character; name of the select-multiple variable in
#'   \code{design$variables}.  Values should be character strings of
#'   space-separated response tokens, or \code{NA} for missing.
#' @param indicator_name Character; human-readable label for the indicator.
#'   Each response result is labelled as
#'   \code{"\{indicator_name\} - \{response\}"}.
#' @param indicator_unit Character; unit label (default \code{"\%"}).
#' @param multiplier Numeric; scale factor applied to the point estimate and
#'   CI bounds (default \code{100} to express as a percentage).
#' @param group_name_label Character; label identifying the disaggregation
#'   group (e.g. \code{"Overall"} or a specific stratum value).
#' @param high_design_complexity Logical; passed to
#'   \code{\link{phr_calc_survey_prop_single}}.
#'
#' @return A tibble with one row per unique response token, with the same
#'   columns as \code{\link{phr_calc_survey_prop_single}}.  The
#'   \code{variable} column always contains \code{var_name} and
#'   \code{indicator_name} is formatted as
#'   \code{"\{indicator_name\} - \{response\}"}.
#'   Returns a single-row NA tibble (with an informative \code{note}) when the
#'   design or variable is invalid, or when no valid responses are found.
#' @noRd
phr_calc_multiple_choice_cat <- function(
    design,
    var_name,
    indicator_name = "Select Multiple",
    indicator_unit = "%",
    multiplier = 100,
    group_name_label = "Overall",
    high_design_complexity = FALSE
) {
  origin <- "phr_calc_multiple_choice_cat"
  phrutils::phr_message(origin, paste("Starting select-multiple calculation for:", indicator_name))

  # Helper to build a standard NA result row
  na_row <- function(note_text) {
    tibble::tibble(
      variable         = var_name,
      indicator_name   = indicator_name,
      indicator_unit   = indicator_unit,
      point.estimate   = NA_real_,
      lower_ci         = NA_real_,
      upper_ci         = NA_real_,
      ci_method        = NA_character_,
      n_unweighted     = NA_real_,
      n_weighted       = NA_real_,
      denom_unweighted = NA_real_,
      denom_weighted   = NA_real_,
      n_eff            = NA_real_,
      deff             = NA_real_,
      group_name       = group_name_label,
      note             = note_text
    )
  }


  # 1. Input Validation

  if (is.null(design) || !inherits(design, c("survey.design", "srvyr_svy", "tbl_svy"))) {
    return(na_row("invalid or missing survey design"))
  }

  if (!var_name %in% names(design$variables)) {
    return(na_row("variable not found in dataset"))
  }

  raw_values <- design$variables[[var_name]]

  if (!is.character(raw_values) && !is.factor(raw_values)) {
    phrutils::phr_warning(origin, paste0("Variable '", var_name,
                               "' is not character or factor; coercing to character."))
  }
  raw_values <- as.character(raw_values)


  # 2. Identify unique responses using grepl across all non-missing values

  non_missing <- raw_values[!is.na(raw_values) & nzchar(trimws(raw_values))]

  if (length(non_missing) == 0) {
    return(na_row("no non-missing values found in variable"))
  }

  # Split each record's responses on whitespace to get individual tokens
  all_tokens <- unlist(strsplit(non_missing, "\\s+"))
  unique_responses <- sort(unique(all_tokens[nzchar(all_tokens)]))

  if (length(unique_responses) == 0) {
    return(na_row("no valid response tokens found in variable"))
  }

  phrutils::phr_message(origin, paste0("Found ", length(unique_responses),
                              " unique response(s): ",
                              paste(unique_responses, collapse = ", ")))


  # 3. Subset design to respondents who answered (non-missing)
  #    so the denominator is "those who answered", not total records.

  design_answered <- tryCatch(
    subset(design, !is.na(design$variables[[var_name]]) &
             nzchar(trimws(as.character(design$variables[[var_name]])))),
    error = function(e) {
      phrutils::phr_warning(origin, paste("Could not subset to non-missing respondents:", e$message))
      design
    }
  )


  # 4. For each unique response, create a binary indicator and call
  #    phr_calc_survey_prop_single

  results <- purrr::map_dfr(unique_responses, function(resp) {

    # Escape any regex special characters in the token before building pattern
    resp_esc <- gsub("([.\\[\\]()^$*+?{}|\\\\])", "\\\\\\1", resp, perl = TRUE)

    # Use grepl to flag records where this response token appears as a whole word
    design_tmp <- design_answered
    design_tmp$variables$.tmp_mc <- as.integer(
      grepl(pattern = paste0("(^|\\s)", resp_esc, "(\\s|$)"),
            x       = design_tmp$variables[[var_name]],
            perl    = TRUE)
    )

    out <- phr_calc_survey_prop_single(
      design                = design_tmp,
      var_name              = ".tmp_mc",
      indicator_name        = paste0(indicator_name, " - ", resp),
      indicator_unit        = indicator_unit,
      multiplier            = multiplier,
      group_name_label      = group_name_label,
      high_design_complexity = high_design_complexity
    )

    # Restore original variable name
    out$variable <- var_name

    out
  })

  phrutils::phr_message(origin, paste("Completed select-multiple calculation for:", indicator_name))
  return(results)
}
