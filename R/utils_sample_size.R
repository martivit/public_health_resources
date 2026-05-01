#' Sample Size Calculation Functions
#'
#' @description
#' Four functions for calculating sample sizes and field work plans for
#' humanitarian assessments: general indicators, individual-level indicators,
#' mortality surveys, and field data collection planning.

#' Calculate sample size for general indicators
#'
#' Estimates the required household sample size for a general proportion-based
#' indicator using either simple random sampling or cluster sampling.  When
#' cluster sampling is used the base sample size is inflated by the design
#' effect.  An optional finite population correction (FPC) can reduce the
#' estimate for small populations.
#'
#' @param expected_proportion Numeric. Expected proportion/prevalence in
#'   percentages (0–100).
#' @param desired_precision Numeric. Desired half-width of the confidence
#'   interval in percentage points (0–100, exclusive).
#' @param non_response_rate Numeric. Anticipated non-response rate in
#'   percentages (0–100, default = 5).
#' @param design Character. Sampling design: \code{"simple_random"} or
#'   \code{"cluster"} (default = \code{"simple_random"}).
#' @param design_effect Numeric. Design effect for cluster sampling; must be
#'   \eqn{\ge 1} (default = 1.5, ignored when \code{design = "simple_random"}).
#' @param fpc Logical. Whether to apply the finite population correction
#'   (default = \code{FALSE}).
#' @param total_population Numeric. Total population size; required (and must
#'   be positive) when \code{fpc = TRUE}.
#' @param number_clusters Integer or \code{NULL}. Number of clusters; when
#'   \code{NULL} or \eqn{\le 0} the SMART-recommended \eqn{t = 2.045} is
#'   used for cluster designs, otherwise the \eqn{t}-distribution critical
#'   value with \code{number_clusters - 1} degrees of freedom is applied
#'   (default = \code{NULL}).
#' @param confidence_level Numeric. Desired confidence level as a proportion
#'   (default = 0.95).
#' @return A single positive integer giving the estimated sample size after
#'   non-response adjustment.
#' @export
calculate_sample_size_general <- function(expected_proportion,
                                         desired_precision,
                                         non_response_rate = 5,
                                         design = "simple_random",
                                         design_effect = 1.5,
                                         fpc = FALSE,
                                         total_population = NULL,
                                         number_clusters = NULL,
                                         confidence_level = 0.95) {

  origin <- "calculate_sample_size_general"

  # --- Type validation ---
  phr_validate_numeric(expected_proportion, origin = origin, soft = FALSE)
  phr_validate_numeric(desired_precision,   origin = origin, soft = FALSE)
  phr_validate_numeric(non_response_rate,   origin = origin, soft = FALSE)
  phr_validate_numeric(confidence_level,    origin = origin, soft = FALSE)
  phr_validate_logical(fpc,                 origin = origin, soft = FALSE)
  phr_validate_character(design,            origin = origin, soft = FALSE)

  # --- Range / logical checks ---
  phr_assert(
    expected_proportion >= 0 && expected_proportion <= 100,
    message = "expected_proportion must be between 0 and 100.",
    origin = origin
  )
  phr_assert(
    desired_precision > 0 && desired_precision < 100,
    message = "desired_precision must be between 0 and 100 (exclusive).",
    origin = origin
  )
  phr_assert(
    non_response_rate >= 0 && non_response_rate < 100,
    message = "non_response_rate must be between 0 and 100 (exclusive).",
    origin = origin
  )
  phr_assert(
    design %in% c("simple_random", "cluster"),
    message = "design must be 'simple_random' or 'cluster'.",
    origin = origin
  )
  if (design == "cluster") {
    phr_validate_numeric(design_effect, origin = origin, soft = FALSE)
    phr_assert(
      design_effect >= 1,
      message = "design_effect must be >= 1 for cluster designs.",
      origin = origin
    )
  }
  if (fpc) {
    phr_assert(
      !is.null(total_population) && is.numeric(total_population) && total_population > 0,
      message = "total_population must be provided and positive when fpc = TRUE.",
      origin = origin
    )
  }

  # Determine t-statistic for cluster design
  if (design == "cluster") {
    print(t)
    if (is.null(number_clusters) || number_clusters <= 0) {
      # Per SMART survey guidance with higher number of clusters 25+
      const_t <- 2.045
    } else {
      const_t <- stats::qt((1 + confidence_level) / 2, df = number_clusters - 1)
    }
  }

  # Convert percentages to proportions
  p <- expected_proportion / 100
  d <- desired_precision / 100

  # Calculate z-score (used for simple random design)
  z <- stats::qnorm((1 + confidence_level) / 2)

  # Calculate base sample size
  if (design == "simple_random") {

    n0 <- (z^2 * p * (1 - p)) / d^2

    if (fpc) {
      n <- n0 / (1 + (n0 - 1) / total_population)
    } else {
      n <- n0
    }

  } else if (design == "cluster") {

    n0 <- ((const_t^2 * p * (1 - p)) / d^2)*design_effect

    if (fpc) {
      n <- n0 / (1 + ((n0 - 1) / total_population))
    } else {
      n <- n0
    }

  } else {
    phr_error("Invalid design type. Must be 'simple_random' or 'cluster'.")
  }

  # Adjust for non-response
  n_adjusted <- n / (1 - non_response_rate / 100)

  return(ceiling(n_adjusted))
}

#' Calculate sample size for individual-level indicators
#'
#' Estimates the required sample size when the indicator of interest is
#' measured at the individual level (e.g. anaemia prevalence in children
#' under five).  The function first computes the number of individuals
#' needed (via \code{\link{calculate_sample_size_general}}), adjusts for a
#' sub-population percentage, and then converts to the number of households
#' required.
#'
#' @param expected_proportion Numeric. Expected proportion/prevalence in
#'   percentages (0–100).
#' @param desired_precision Numeric. Desired half-width of the confidence
#'   interval in percentage points (0–100, exclusive).
#' @param non_response_rate Numeric. Anticipated non-response rate in
#'   percentages (0–100, default = 5).
#' @param design Character. Sampling design: \code{"simple_random"} or
#'   \code{"cluster"} (default = \code{"simple_random"}).
#' @param design_effect Numeric. Design effect for cluster sampling; must be
#'   \eqn{\ge 1} (default = 1.5, ignored when \code{design = "simple_random"}).
#' @param fpc Logical. Whether to apply the finite population correction
#'   (default = \code{FALSE}).
#' @param total_population Numeric. Total population size; required (and must
#'   be positive) when \code{fpc = TRUE}.
#' @param num_clusters Integer or \code{NULL}. Number of clusters; passed to
#'   \code{\link{calculate_sample_size_general}} as \code{number_clusters}
#'   (default = \code{NULL}).
#' @param average_household_size Numeric. Average number of individuals per
#'   household (must be positive).
#' @param sub_population_percent Numeric. The percentage of the household
#'   population belonging to the target sub-population (0–100, default = 100).
#'   Values below 100 inflate the individual sample size proportionally.
#' @param confidence_level Numeric. Desired confidence level as a proportion
#'   (default = 0.95).
#' @return A named list with four elements:
#'   \describe{
#'     \item{sample_size_individuals}{Integer. Required number of individuals.}
#'     \item{sample_size_households}{Integer. Required number of households.}
#'     \item{average_household_size}{Numeric. The value supplied to the argument
#'       \code{average_household_size}.}
#'     \item{sub_population_percent}{Numeric. The value supplied to the argument
#'       \code{sub_population_percent}.}
#'   }
#' @export
calculate_sample_size_individual <- function(expected_proportion,
                                            desired_precision,
                                            non_response_rate = 5,
                                            design = "simple_random",
                                            design_effect = 1.5,
                                            fpc = FALSE,
                                            total_population = NULL,
                                            num_clusters = NULL,
                                            average_household_size,
                                            sub_population_percent = 100,
                                            confidence_level = 0.95) {

  origin <- "calculate_sample_size_individual"

  # --- Type validation ---
  phr_validate_numeric(sub_population_percent, origin = origin, soft = FALSE)

  # --- Range checks ---
  if (missing(average_household_size)) {
    phr_error("average_household_size must be provided and positive.", origin = origin)
  }
  phr_validate_numeric(average_household_size, origin = origin, soft = FALSE)
  phr_assert(
    average_household_size > 0,
    message = "average_household_size must be positive.",
    origin = origin
  )
  phr_assert(
    sub_population_percent > 0 && sub_population_percent <= 100,
    message = "sub_population_percent must be between 0 (exclusive) and 100.",
    origin = origin
  )

  # Calculate base sample size using general function
  n_individuals <- calculate_sample_size_general(
    expected_proportion = expected_proportion,
    desired_precision = desired_precision,
    non_response_rate = non_response_rate,
    design = design,
    design_effect = design_effect,
    number_clusters = num_clusters,
    fpc = fpc,
    total_population = total_population,
    confidence_level = confidence_level
  )

  # Adjust for sub-population percentage
  n_individuals <- ceiling(n_individuals / (sub_population_percent / 100))

  # Calculate households needed
  n_households <- ceiling(n_individuals / average_household_size)

  return(list(
    sample_size_individuals  = n_individuals,
    sample_size_households   = n_households
  ))
}

#' Calculate sample size for mortality surveys
#'
#' Estimates the required sample size for a retrospective household mortality
#' survey using a Poisson-rate approach.  The function returns both individual
#' and household sample sizes as well as the total person-time (in person-days)
#' and the expected number of deaths that would be observed in the sample.
#'
#' @param expected_death_rate Numeric. Expected crude death rate (CDR) in
#'   deaths per 10,000 people per day (must be positive).
#' @param desired_precision Numeric. Desired precision as a half-width in
#'   deaths per 10,000 people per day (must be positive).
#' @param non_response_rate Numeric. Anticipated non-response rate in
#'   percentages (0–100, default = 5).
#' @param design Character. Sampling design: \code{"simple_random"} or
#'   \code{"cluster"} (default = \code{"cluster"}).
#' @param design_effect Numeric. Design effect for cluster sampling; must be
#'   \eqn{\ge 1} (default = 1.5, ignored when \code{design = "simple_random"}).
#' @param number_clusters Integer or \code{NULL}. Number of clusters; when
#'   \code{NULL} or \eqn{\le 0} the SMART-recommended \eqn{t = 2.045} is used,
#'   otherwise the \eqn{t}-distribution with \code{number_clusters - 1} degrees
#'   of freedom is applied (default = \code{NULL}).
#' @param recall_days Numeric. Length of the recall period in days
#'   (default = 90).
#' @param average_household_size Numeric. Average number of individuals per
#'   household (must be positive).
#' @param fpc Logical. Whether to apply the finite population correction
#'   (default = \code{FALSE}).
#' @param total_population Numeric. Total population size; required (and must
#'   be positive) when \code{fpc = TRUE}.
#' @param confidence_level Numeric. Desired confidence level as a proportion
#'   (default = 0.95).
#' @return A named list with six elements:
#'   \describe{
#'     \item{sample_size_households}{Integer. Households required after
#'       non-response adjustment.}
#'     \item{sample_size_individuals}{Integer. Individuals required (no
#'       non-response adjustment).}
#'     \item{sample_size_person_time}{Numeric. Total person-days of observation
#'       (\code{sample_size_individuals} \eqn{\times} \code{recall_days}).}
#'     \item{expected_deaths}{Integer. Expected number of deaths in the sample
#'       over the recall period.}
#'     \item{recall_days}{Numeric. The recall period used (echo of input).}
#'     \item{design_effect}{Numeric. The effective design effect applied:
#'       1 for simple random sampling, or the supplied value for cluster
#'       sampling.}
#'   }
#' @export
calculate_sample_size_mortality <- function(expected_death_rate,
                                           desired_precision,
                                           non_response_rate = 5,
                                           design = "cluster",
                                           design_effect = 1.5,
                                           number_clusters = NULL,
                                           recall_days = 90,
                                           average_household_size,
                                           fpc = FALSE,
                                           total_population = NULL,
                                           confidence_level = 0.95) {

  origin <- "calculate_sample_size_mortality"

  # --- Type validation ---
  phr_validate_numeric(expected_death_rate,  origin = origin, soft = FALSE)
  phr_validate_numeric(desired_precision,    origin = origin, soft = FALSE)
  phr_validate_numeric(non_response_rate,    origin = origin, soft = FALSE)
  phr_validate_numeric(recall_days,          origin = origin, soft = FALSE)
  phr_validate_numeric(confidence_level,     origin = origin, soft = FALSE)
  phr_validate_logical(fpc,                  origin = origin, soft = FALSE)
  phr_validate_character(design,             origin = origin, soft = FALSE)

  # --- Range / logical checks ---
  phr_assert(
    expected_death_rate > 0,
    message = "expected_death_rate must be positive.",
    origin = origin
  )
  phr_assert(
    desired_precision > 0,
    message = "desired_precision must be positive.",
    origin = origin
  )
  if (missing(average_household_size)) {
    phr_error("average_household_size must be provided and positive.", origin = origin)
  }
  phr_validate_numeric(average_household_size, origin = origin, soft = FALSE)
  phr_assert(
    average_household_size > 0,
    message = "average_household_size must be positive.",
    origin = origin
  )
  phr_assert(
    recall_days > 0,
    message = "recall_days must be positive.",
    origin = origin
  )
  phr_assert(
    design %in% c("simple_random", "cluster"),
    message = "design must be 'simple_random' or 'cluster'.",
    origin = origin
  )
  if (design == "cluster") {
    phr_validate_numeric(design_effect, origin = origin, soft = FALSE)
    phr_assert(
      design_effect >= 1,
      message = "design_effect must be >= 1 for cluster designs.",
      origin = origin
    )
  }
  if (fpc) {
    phr_assert(
      !is.null(total_population) && is.numeric(total_population) && total_population > 0,
      message = "total_population must be provided and positive when fpc = TRUE.",
      origin = origin
    )
  }

  # Determine t-statistic for cluster design (computed but available for
  # potential future use; z is used in the base formula for both designs)
  if (design == "cluster") {
    if (is.null(number_clusters) || number_clusters <= 0) {
      # Per SMART survey guidance with higher number of clusters 25+
      const_t <- 2.045  # nolint
    } else {
      const_t <- stats::qt((1 + confidence_level) / 2, df = number_clusters - 1)  # nolint
    }
  }

  # Calculate z-score
  z <- stats::qnorm((1 + confidence_level) / 2)
  r <- expected_death_rate / 10000
  d <- desired_precision / 10000



  if (design == "simple_random") {

    numerator   <- z^2 * r # * (1 - r)
    denominator <- d^2 * recall_days

    n_individuals <- numerator / denominator

    if (fpc) {
      n_adj_individuals <- (n_individuals * total_population) /
        (n_individuals + (total_population - 1))
    } else {
      n_adj_individuals <- n_individuals
    }

    effective_design_effect <- 1

  } else if (design == "cluster") {

    numerator   <- const_t^2 * r # * (1 - r)
    denominator <- d^2 * recall_days

    n_individuals <- (numerator / denominator) * design_effect

    if (fpc) {
      n_adj_individuals <- (n_individuals * total_population) /
        (n_individuals + (total_population - 1))
    } else {
      n_adj_individuals <- n_individuals
    }

    effective_design_effect <- design_effect

  } else {
    phr_error("Invalid design type. Must be 'simple_random' or 'cluster'.")
  }

  # Round individuals up before deriving secondary quantities
  n_adj_individuals <- ceiling(n_adj_individuals)

  # Total person-days of observation
  n_pt <- n_adj_individuals * recall_days

  # Expected deaths in the sample over the recall period
  expected_deaths <- ceiling(r * n_adj_individuals * recall_days)

  # Households required (non-response adjustment applied here)
  n_households <- ceiling(
    (n_adj_individuals / average_household_size) / (1 - non_response_rate / 100)
  )

  return(list(
    sample_size_households  = n_households,
    sample_size_individuals = n_adj_individuals,
    sample_size_person_time = n_pt
  ))
}

#' Estimate field work plan for data collection
#'
#' Estimates the number of interviews an enumerator can complete per day and
#' the total number of data-collection days required to meet the target sample
#' size, given available teams and working-time constraints.  For cluster
#' designs the function also derives the number of PSUs (clusters) needed and
#' the implied cluster size.
#'
#' @param sample_design Character. Sampling design: \code{"simple_random"} or
#'   \code{"cluster"}.
#' @param number_of_teams Integer. Number of simultaneous data-collection teams
#'   (must be positive).
#' @param enumerators_per_team Integer. Number of enumerators in each team
#'   (must be positive).
#' @param number_of_psu_per_team_per_day Numeric or \code{NULL}. Number of PSUs
#'   (clusters) each team can complete in a day; required and must be positive
#'   when \code{sample_design = "cluster"} (default = \code{NULL}).
#' @param start_time Character or \code{Date}. Start date of data collection;
#'   character strings are coerced via \code{as.Date()}.
#' @param end_time Character or \code{Date}. End date of data collection;
#'   character strings are coerced via \code{as.Date()}.  Must be after
#'   \code{start_time}.
#' @param average_interview_time Numeric. Average time to complete one
#'   interview, in minutes (must be positive).
#' @param average_travel_time Numeric. Average travel time between clusters
#'   (or households) per day, in minutes (must be non-negative).
#' @param average_rest_time Numeric. Average break/rest time per day, in
#'   minutes (must be non-negative).
#' @param total_sample_size Integer or \code{NULL}. Total sample size needed
#'   (used to compute \code{num_days} and, for cluster designs,
#'   \code{num_psu_needed}).
#' @return A named list with four elements:
#'   \describe{
#'     \item{num_interview_per_enum_per_day}{Integer. Estimated interviews per
#'       enumerator per day.}
#'     \item{num_days}{Numeric. Estimated number of data-collection days needed.}
#'     \item{num_psu_needed}{\code{NA} for simple random sampling; integer
#'       number of PSUs required for cluster designs.}
#'     \item{psu_size}{\code{NA} for simple random sampling; numeric cluster
#'       size for cluster designs.}
#'   }
#' @export
estimate_field_plan <- function(sample_design,
                                number_of_teams,
                                enumerators_per_team,
                                number_of_psu_per_team_per_day = NULL,
                                start_time,
                                end_time,
                                average_interview_time,
                                average_travel_time,
                                average_rest_time,
                                total_sample_size = NULL) {

  origin <- "estimate_field_plan"

  # --- Type validation ---
  phr_validate_character(sample_design, origin = origin, soft = FALSE)
  phr_validate_numeric(number_of_teams,       origin = origin, soft = FALSE)
  phr_validate_numeric(enumerators_per_team,  origin = origin, soft = FALSE)
  phr_validate_numeric(average_interview_time, origin = origin, soft = FALSE)
  phr_validate_numeric(average_travel_time,   origin = origin, soft = FALSE)
  phr_validate_numeric(average_rest_time,     origin = origin, soft = FALSE)

  # --- Range / logic checks ---
  phr_assert(
    number_of_teams > 0,
    message = "number_of_teams must be positive.",
    origin = origin
  )
  phr_assert(
    enumerators_per_team > 0,
    message = "enumerators_per_team must be positive.",
    origin = origin
  )
  phr_assert(
    average_interview_time > 0,
    message = "average_interview_time must be positive.",
    origin = origin
  )
  phr_assert(
    average_travel_time >= 0,
    message = "average_travel_time must be non-negative.",
    origin = origin
  )
  phr_assert(
    average_rest_time >= 0,
    message = "average_rest_time must be non-negative.",
    origin = origin
  )
  phr_assert(
    sample_design %in% c("simple_random", "cluster"),
    message = "sample_design must be 'simple_random' or 'cluster'.",
    origin = origin
  )

  if (sample_design == "cluster") {
    phr_assert(
      !is.null(number_of_psu_per_team_per_day) &&
        is.numeric(number_of_psu_per_team_per_day) &&
        number_of_psu_per_team_per_day > 0,
      message = "number_of_psu_per_team_per_day must be positive for cluster designs.",
      origin = origin
    )
  }

  # Convert times to Date if needed
  if (is.character(start_time)) start_time <- as.Date(start_time)
  if (is.character(end_time))   end_time   <- as.Date(end_time)

  # Calculate available working time
  total_working_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  total_working_hours   <- total_working_minutes / 60

  phr_assert(
    total_working_minutes > 0,
    message = "end_time must be after start_time.",
    origin = origin
  )

  effective_working_time <- total_working_minutes - average_rest_time - average_travel_time
  interviews_per_enumerator_per_day <- floor(effective_working_time / average_interview_time)

  if (sample_design == "simple_random") {

    number_days_needed <- (total_sample_size * average_interview_time) /
      (effective_working_time * number_of_teams * enumerators_per_team)

    return(list(
      num_interview_per_enum_per_day = interviews_per_enumerator_per_day,
      num_days    = number_days_needed,
      num_psu_needed = NA,
      psu_size       = NA
    ))

  } else {

    psu_size <- (interviews_per_enumerator_per_day * enumerators_per_team) /
      number_of_psu_per_team_per_day
    number_psu_needed  <- ceiling(total_sample_size / psu_size)
    number_days_needed <- ceiling(
      number_psu_needed / (number_of_psu_per_team_per_day * number_of_teams)
    )

    return(list(
      num_interview_per_enum_per_day = interviews_per_enumerator_per_day,
      num_days       = number_days_needed,
      num_psu_needed = number_psu_needed,
      psu_size       = psu_size
    ))
  }
}
