#' Statistical Quality Test Functions
#'
#' This file contains statistical test functions used by the DataAnalytics class
#' for plausibility testing. Each function returns a test statistic or metric
#' that can be compared against thresholds to determine data quality.
#'
#' @name quality_tests
NULL

#' Correlation Test
#'
#' Calculate correlation coefficient between two numeric variables
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 variable names
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#' @return List with statistic (correlation coefficient) and p_value
#' @export
quality_test_correlation <- function(
  survey_design,
  variables,
  method = "pearson"
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (length(variables) != 2) {
        phr_error(
          origin = "quality_test_correlation",
          message = "Correlation test requires exactly 2 variables"
        )
      }

      var1 <- variables[1]
      var2 <- variables[2]

      if (!var1 %in% names(data) || !var2 %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_correlation",
          message = "One or both variables not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var1]])
      y <- as.numeric(data[[var2]])

      # Remove pairs with missing values
      complete_cases <- complete.cases(x, y)
      if (sum(complete_cases) < 3) {
        phrutils::phr_warning(
          origin = "quality_test_correlation",
          message = "Insufficient complete cases for correlation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Use cor.test to get both correlation and p-value
      test_result <- cor.test(
        x[complete_cases],
        y[complete_cases],
        method = method
      )

      return(list(
        statistic = as.numeric(test_result$estimate),
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_correlation"
  )
}

#' T-Test for Mean Comparison
#'
#' Perform a one-sample t-test or two-sample t-test
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of 1 or 2 variable names
#' @param mu Expected mean for one-sample test (default: 0)
#' @param paired Logical, whether to perform paired t-test for two variables
#' @return List with statistic (t-value) and p-value
#' @export
quality_test_ttest <- function(
  survey_design,
  variables,
  mu = 0,
  paired = FALSE
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (length(variables) == 0 || length(variables) > 2) {
        phr_error(
          origin = "quality_test_ttest",
          message = "T-test requires 1 or 2 variables"
        )
      }

      var1 <- variables[1]

      if (!var1 %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_ttest",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var1]])
      x <- x[!is.na(x)]

      if (length(x) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_ttest",
          message = "Insufficient data for t-test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (length(variables) == 1) {
        # One-sample t-test
        test_result <- t.test(x, mu = mu)
      } else {
        # Two-sample t-test
        var2 <- variables[2]
        if (!var2 %in% names(data)) {
          phrutils::phr_warning(
            origin = "quality_test_ttest",
            message = "Second variable not found in data"
          )
          return(list(statistic = NA_real_, p_value = NA_real_))
        }

        y <- as.numeric(data[[var2]])
        y <- y[!is.na(y)]

        if (length(y) < 2) {
          phrutils::phr_warning(
            origin = "quality_test_ttest",
            message = "Insufficient data in second variable"
          )
          return(list(statistic = NA_real_, p_value = NA_real_))
        }

        test_result <- t.test(x, y, paired = paired)
      }

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_ttest"
  )
}

#' Chi-Squared Test for Independence
#'
#' Perform chi-squared test of independence between two categorical variables
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 variable names
#' @return List with statistic (chi-squared value) and p-value
#' @export
quality_test_chisq <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (length(variables) != 2) {
        phr_error(
          origin = "quality_test_chisq",
          message = "Chi-squared test requires exactly 2 variables"
        )
      }

      var1 <- variables[1]
      var2 <- variables[2]

      if (!var1 %in% names(data) || !var2 %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_chisq",
          message = "One or both variables not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- data[[var1]]
      y <- data[[var2]]

      # Remove missing values
      complete_cases <- complete.cases(x, y)
      x <- x[complete_cases]
      y <- y[complete_cases]

      if (length(x) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_chisq",
          message = "Insufficient data for chi-squared test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Create contingency table
      cont_table <- table(x, y)

      # Check if table has sufficient cells
      if (any(dim(cont_table) < 2)) {
        phrutils::phr_warning(
          origin = "quality_test_chisq",
          message = "Contingency table too small for chi-squared test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      test_result <- chisq.test(cont_table)

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_chisq"
  )
}

#' Flag Percentage Test
#'
#' Calculate the percentage of records with a specific flag value
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector with 1 variable name
#' @param flag_value The value to count (default: TRUE or 1)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_flag_percentage <- function(
  survey_design,
  variables,
  flag_value = TRUE
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (length(variables) != 1) {
        phr_error(
          origin = "quality_test_flag_percentage",
          message = "Flag percentage test requires exactly 1 variable"
        )
      }

      var <- variables[1]

      if (!var %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_flag_percentage",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- data[[var]]
      x <- x[!is.na(x)]

      if (length(x) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_flag_percentage",
          message = "No non-missing values found"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Count matching values
      flag_count <- sum(x == flag_value)
      percentage <- (flag_count / length(x)) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_flag_percentage"
  )
}

#' Missing Data Percentage Test
#'
#' Calculate the percentage of missing values across specified variables
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of variable names
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_missing_percentage <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (length(variables) == 0) {
        phr_error(
          origin = "quality_test_missing_percentage",
          message = "Missing percentage test requires at least 1 variable"
        )
      }

      # Check which variables exist
      existing_vars <- intersect(variables, names(data))

      if (length(existing_vars) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_missing_percentage",
          message = "No specified variables found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Calculate missing percentage across all specified variables
      total_cells <- 0
      missing_cells <- 0

      for (var in existing_vars) {
        x <- data[[var]]
        total_cells <- total_cells + length(x)
        missing_cells <- missing_cells + sum(is.na(x))
      }

      if (total_cells == 0) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      percentage <- (missing_cells / total_cells) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_missing_percentage"
  )
}

#' Outlier Detection Test (Z-Score Method)
#'
#' Calculate the percentage of outliers based on z-score threshold
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector with 1 variable name
#' @param z_threshold Z-score threshold (default: 3)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_outlier_percentage <- function(
  survey_design,
  variables,
  z_threshold = 3
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has correct length
      if (is.null(variables) || length(variables) != 1) {
        phr_error(
          origin = "quality_test_outlier_percentage",
          message = "Outlier test requires exactly 1 variable"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_outlier_percentage",
          message = "Variable name must be a character string"
        )
      }

      # Validate z_threshold is numeric and positive
      if (!is.numeric(z_threshold) || length(z_threshold) != 1) {
        phr_error(
          origin = "quality_test_outlier_percentage",
          message = "z_threshold must be a single numeric value"
        )
      }

      if (is.na(z_threshold) || z_threshold <= 0) {
        phr_error(
          origin = "quality_test_outlier_percentage",
          message = "z_threshold must be a positive number"
        )
      }

      var <- variables[1]

      if (!var %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_outlier_percentage",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate that the variable is numeric
      is_valid <- phrutils::phr_validate_numeric(
        data[[var]],
        origin = "quality_test_outlier_percentage",
        hint = phr_txt(
          "Outlier detection requires numeric data for z-score calculations."
        ),
        soft = TRUE
      )

      # If validation failed, return NA
      if (isFALSE(is_valid)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var]])
      x <- x[!is.na(x)]

      if (length(x) < 3) {
        phrutils::phr_warning(
          origin = "quality_test_outlier_percentage",
          message = "Insufficient data for outlier detection (need at least 3 non-missing values)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Check for zero variance (all values identical)
      if (sd(x) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_outlier_percentage",
          message = "Variable has zero variance (all values are identical)"
        )
        return(list(statistic = 0, p_value = NA_real_))
      }

      # Calculate z-scores
      z_scores <- (x - mean(x)) / sd(x)

      # Additional safety check for Inf/NaN in z-scores
      if (any(is.infinite(z_scores)) || any(is.nan(z_scores))) {
        phrutils::phr_warning(
          origin = "quality_test_outlier_percentage",
          message = "Unable to calculate valid z-scores"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      outlier_count <- sum(abs(z_scores) > z_threshold)
      percentage <- (outlier_count / length(x)) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_outlier_percentage"
  )
}

#' Coefficient of Variation Test
#'
#' Calculate the coefficient of variation (CV) for a numeric variable
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector with 1 variable name
#' @return List with statistic (CV percentage) and p_value (NA)
#' @export
quality_test_coefficient_variation <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has correct length
      if (is.null(variables) || length(variables) != 1) {
        phr_error(
          origin = "quality_test_coefficient_variation",
          message = "CV test requires exactly 1 variable"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_coefficient_variation",
          message = "Variable name must be a character string"
        )
      }

      var <- variables[1]

      if (!var %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_coefficient_variation",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate that the variable is numeric
      is_valid <- phrutils::phr_validate_numeric(
        data[[var]],
        origin = "quality_test_coefficient_variation",
        hint = phr_txt("CV calculation requires numeric data."),
        soft = TRUE
      )

      # If validation failed, return NA
      if (isFALSE(is_valid)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var]])
      x <- x[!is.na(x)]

      if (length(x) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_coefficient_variation",
          message = "Insufficient data for CV calculation (need at least 2 non-missing values)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      mean_x <- mean(x)
      sd_x <- sd(x)

      # Check for zero variance (all values identical)
      if (sd_x == 0) {
        phrutils::phr_warning(
          origin = "quality_test_coefficient_variation",
          message = "Variable has zero variance (all values are identical), CV is 0"
        )
        return(list(statistic = 0, p_value = NA_real_))
      }

      # Check for zero mean
      if (mean_x == 0) {
        phrutils::phr_warning(
          origin = "quality_test_coefficient_variation",
          message = "Mean is zero, CV is undefined"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      cv <- (sd_x / abs(mean_x)) * 100

      # Additional safety check for Inf/NaN in CV
      if (is.infinite(cv) || is.nan(cv)) {
        phrutils::phr_warning(
          origin = "quality_test_coefficient_variation",
          message = "Unable to calculate valid CV"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      return(list(
        statistic = cv,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_coefficient_variation"
  )
}

#' Range Violation Test
#'
#' Calculate the percentage of values outside the specified range
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector with 1 variable name
#' @param min_value Minimum allowed value (default: -Inf)
#' @param max_value Maximum allowed value (default: Inf)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_range_violation <- function(
  survey_design,
  variables,
  min_value = -Inf,
  max_value = Inf
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has correct length
      if (is.null(variables) || length(variables) != 1) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "Range test requires exactly 1 variable"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "Variable name must be a character string"
        )
      }

      # Validate min_value is numeric
      if (!is.numeric(min_value) || length(min_value) != 1) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "min_value must be a single numeric value"
        )
      }

      # Validate max_value is numeric
      if (!is.numeric(max_value) || length(max_value) != 1) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "max_value must be a single numeric value"
        )
      }

      # Check that min_value and max_value are not NA
      if (is.na(min_value)) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "min_value cannot be NA"
        )
      }

      if (is.na(max_value)) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "max_value cannot be NA"
        )
      }

      # Validate that min_value <= max_value
      if (min_value > max_value) {
        phr_error(
          origin = "quality_test_range_violation",
          message = "min_value must be less than or equal to max_value"
        )
      }

      var <- variables[1]

      if (!var %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_range_violation",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate that the variable is numeric
      is_valid <- phrutils::phr_validate_numeric(
        data[[var]],
        origin = "quality_test_range_violation",
        hint = phr_txt("Range violation test requires numeric data."),
        soft = TRUE
      )

      # If validation failed, return NA
      if (isFALSE(is_valid)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var]])
      x <- x[!is.na(x)]

      if (length(x) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_range_violation",
          message = "No non-missing values found"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Check for infinite values in the data
      if (any(is.infinite(x))) {
        phrutils::phr_warning(
          origin = "quality_test_range_violation",
          message = "Variable contains infinite values which will be excluded from range check"
        )
        x <- x[is.finite(x)]

        if (length(x) == 0) {
          phrutils::phr_warning(
            origin = "quality_test_range_violation",
            message = "No finite values found after removing infinite values"
          )
          return(list(statistic = NA_real_, p_value = NA_real_))
        }
      }

      out_of_range <- sum(x < min_value | x > max_value)
      percentage <- (out_of_range / length(x)) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_range_violation"
  )
}

#' Standard Deviation Test
#'
#' Calculate the standard deviation for a numeric variable
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector with 1 variable name
#' @return List with statistic (standard deviation) and p_value (NA)
#' @export
quality_test_sd <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has correct length
      if (is.null(variables) || length(variables) != 1) {
        phr_error(
          origin = "quality_test_sd",
          message = "Standard deviation test requires exactly 1 variable"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_sd",
          message = "Variable name must be a character string"
        )
      }

      var <- variables[1]

      if (!var %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_sd",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate that the variable is numeric
      is_valid <- phrutils::phr_validate_numeric(
        data[[var]],
        origin = "quality_test_sd",
        hint = phr_txt("Standard deviation calculation requires numeric data."),
        soft = TRUE
      )

      # If validation failed, return NA
      if (isFALSE(is_valid)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- as.numeric(data[[var]])
      x <- x[!is.na(x)]

      if (length(x) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_sd",
          message = "Insufficient data for standard deviation calculation (need at least 2 non-missing values)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      sd_x <- sd(x)

      # Additional safety check for Inf/NaN in standard deviation
      if (is.infinite(sd_x) || is.nan(sd_x)) {
        phrutils::phr_warning(
          origin = "quality_test_sd",
          message = "Unable to calculate valid standard deviation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      return(list(
        statistic = sd_x,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_sd"
  )
}

#' Standard Deviation Across Columns Percentage Test
#'
#' Calculate the percentage of rows where the standard deviation across
#' specified columns falls below a threshold value
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of variable names (minimum 2)
#' @param threshold Threshold value for SD comparison (default: 0.8)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_sd_across_percentage <- function(
  survey_design,
  variables,
  threshold = 0.8
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has at least 2 variables
      if (is.null(variables) || length(variables) < 2) {
        phr_error(
          origin = "quality_test_sd_across_percentage",
          message = "SD across percentage test requires at least 2 variables"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_sd_across_percentage",
          message = "Variable names must be a character vector"
        )
      }

      # Validate threshold is numeric and positive
      if (!is.numeric(threshold) || length(threshold) != 1) {
        phr_error(
          origin = "quality_test_sd_across_percentage",
          message = "threshold must be a single numeric value"
        )
      }

      if (is.na(threshold) || threshold < 0) {
        phr_error(
          origin = "quality_test_sd_across_percentage",
          message = "threshold must be a non-negative number"
        )
      }

      # Check which variables exist in the data
      existing_vars <- intersect(variables, names(data))
      missing_vars <- setdiff(variables, names(data))

      if (length(missing_vars) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_sd_across_percentage",
          message = paste0(
            "Variables not found in data: ",
            paste(missing_vars, collapse = ", ")
          )
        )
      }

      if (length(existing_vars) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_sd_across_percentage",
          message = "At least 2 variables must exist in data for SD calculation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate that all existing variables are numeric
      for (var in existing_vars) {
        is_valid <- phrutils::phr_validate_numeric(
          data[[var]],
          origin = "quality_test_sd_across_percentage",
          hint = phr_txt(paste0(
            "Variable '",
            var,
            "' must be numeric for SD calculation."
          )),
          soft = TRUE
        )

        if (isFALSE(is_valid)) {
          phrutils::phr_warning(
            origin = "quality_test_sd_across_percentage",
            message = paste0(
              "Variable '",
              var,
              "' is not numeric and will be excluded"
            )
          )
          existing_vars <- setdiff(existing_vars, var)
        }
      }

      # Check again after removing non-numeric variables
      if (length(existing_vars) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_sd_across_percentage",
          message = "At least 2 numeric variables required after validation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Extract the subset of data with selected variables
      subset_data <- data[, existing_vars, drop = FALSE]

      # Convert all columns to numeric
      subset_data <- as.data.frame(lapply(subset_data, as.numeric))

      # Calculate row-wise standard deviation
      # Using apply with MARGIN = 1 for row-wise calculation
      row_sd <- apply(subset_data, 1, function(row) {
        # Remove NA values for each row
        row_clean <- row[!is.na(row)]

        # Need at least 2 non-missing values to calculate SD
        if (length(row_clean) < 2) {
          return(NA_real_)
        }

        return(sd(row_clean))
      })

      # Count rows with valid SD calculations
      valid_rows <- !is.na(row_sd)

      if (sum(valid_rows) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_sd_across_percentage",
          message = "No rows with sufficient non-missing values for SD calculation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Create flag: 1 if SD < threshold, 0 otherwise
      flag_sd_rcsicoping <- ifelse(valid_rows & row_sd < threshold, 1, 0)

      # Calculate percentage of flagged rows (only among valid rows)
      flagged_count <- sum(flag_sd_rcsicoping[valid_rows] == 1)
      percentage <- (flagged_count / sum(valid_rows)) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_sd_across_percentage"
  )
}

#' Any Flag Percentage Test
#'
#' Calculate the percentage of rows where at least one of the specified
#' variables has the flag value
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of variable names (minimum 1)
#' @param flag_value The value to check for (default: 1)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_any_flag_percentage <- function(
  survey_design,
  variables,
  flag_value = 1
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate variables is not NULL and has at least 1 variable
      if (is.null(variables) || length(variables) < 1) {
        phr_error(
          origin = "quality_test_any_flag_percentage",
          message = "Any flag percentage test requires at least 1 variable"
        )
      }

      # Validate variables is character
      if (!is.character(variables)) {
        phr_error(
          origin = "quality_test_any_flag_percentage",
          message = "Variable names must be a character vector"
        )
      }

      # Validate flag_value is provided and not NULL
      if (is.null(flag_value) || length(flag_value) != 1) {
        phr_error(
          origin = "quality_test_any_flag_percentage",
          message = "flag_value must be a single value"
        )
      }

      # Check which variables exist in the data
      existing_vars <- intersect(variables, names(data))
      missing_vars <- setdiff(variables, names(data))

      if (length(missing_vars) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_any_flag_percentage",
          message = paste0(
            "Variables not found in data: ",
            paste(missing_vars, collapse = ", ")
          )
        )
      }

      if (length(existing_vars) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_any_flag_percentage",
          message = "No specified variables found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Extract the subset of data with selected variables
      subset_data <- data[, existing_vars, drop = FALSE]

      # Check if we have any rows
      if (nrow(subset_data) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_any_flag_percentage",
          message = "Data has no rows"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Calculate row-wise: does any column have the flag value?
      # Using apply with MARGIN = 1 for row-wise calculation
      any_flag <- apply(subset_data, 1, function(row) {
        # Remove NA values for each row
        row_clean <- row[!is.na(row)]

        # If all values are NA, return NA
        if (length(row_clean) == 0) {
          return(NA)
        }

        # Check if any value matches flag_value
        any(row_clean == flag_value)
      })

      # Convert logical to numeric (TRUE -> 1, FALSE -> 0)
      flag_column <- as.integer(any_flag)

      # Count rows with valid flag calculations (non-NA)
      valid_rows <- !is.na(flag_column)

      if (sum(valid_rows) == 0) {
        phrutils::phr_warning(
          origin = "quality_test_any_flag_percentage",
          message = "No rows with non-missing values for flag calculation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Calculate percentage of flagged rows (only among valid rows)
      flagged_count <- sum(flag_column[valid_rows] == 1)
      percentage <- (flagged_count / sum(valid_rows)) * 100

      return(list(
        statistic = percentage,
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_any_flag_percentage"
  )
}

#' Sex Ratio Test (Male:Female)
#'
#' Perform a chi-squared goodness-of-fit test comparing the observed male/female
#' counts to an expected male:female ratio (default 1:1).
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character scalar. Name of the sex column in `data`
#' @param male_val Value in `variables` that indicates "male"
#' @param female_val Value in `variables` that indicates "female"
#' @param expected_ratio_val Single positive numeric giving expected male:female ratio.
#'   Default 1 (i.e., 1:1).
#' @return List with statistic (observed male:female ratio) and p-value
#' @export
quality_test_sexratio <- function(
  survey_design,
  variables,
  male_val,
  female_val,
  expected_ratio_val = 1
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 1L) {
        phr_error(
          origin = "quality_test_sexratio",
          message = "`variables` must be a single character column name"
        )
      }

      if (!variables %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio",
          message = "Sex column not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (!is.numeric(expected_ratio_val) || length(expected_ratio_val) != 1L) {
        phr_error(
          origin = "quality_test_sexratio",
          message = "`expected_ratio_val` must be a single numeric value (male:female)"
        )
      }

      if (!is.finite(expected_ratio_val) || expected_ratio_val <= 0) {
        phr_error(
          origin = "quality_test_sexratio",
          message = "`expected_ratio_val` must be a finite positive number"
        )
      }

      if (identical(male_val, female_val)) {
        phr_error(
          origin = "quality_test_sexratio",
          message = "`male_val` and `female_val` must be different"
        )
      }

      s <- data[[variables]]
      s <- s[!is.na(s)]

      if (length(s) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio",
          message = "Insufficient data for sex ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Exclude values other than male_val/female_val, and warn if any are found
      is_mf <- (s == male_val) | (s == female_val)
      n_excluded <- sum(!is_mf)

      if (n_excluded > 0) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio",
          message = paste0(
            "Excluding ",
            n_excluded,
            " record(s) with sex values not equal to `male_val` or `female_val`"
          )
        )
      }

      s <- s[is_mf]

      if (length(s) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio",
          message = "Insufficient male/female data for sex ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      obs_male <- sum(s == male_val, na.rm = TRUE)
      obs_female <- sum(s == female_val, na.rm = TRUE)

      if (obs_male == 0L || obs_female == 0L) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio",
          message = "Only one sex category present; cannot test sex ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      obs <- c(male = obs_male, female = obs_female)

      # expected_ratio_val is male:female
      p_male <- expected_ratio_val / (expected_ratio_val + 1)
      p_female <- 1 / (expected_ratio_val + 1)
      p <- c(p_male, p_female)

      obs_ratio <- obs_male / obs_female

      test_result <- chisq.test(x = obs, p = p)

      return(list(
        statistic = obs_ratio,
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_sexratio"
  )
}

#' Age Group Ratio Test (Two Indicator Columns)
#'
#' Perform a chi-squared goodness-of-fit test comparing the observed counts of "yes"
#' in two different age-group indicator columns to an expected ratio (default 1:1).
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of length 2. Names of the two age-group indicator columns in `data`
#' @param yes_val Value indicating "yes" in the indicator columns. Default 1.
#' @param no_val Value indicating "no" in the indicator columns. Default 0.
#' @param expected_ratio_val Single positive numeric giving expected ratio of
#'   variables[1]:variables[2]. Default 1 (i.e., 1:1).
#' @return List with statistic (observed col1:col2 ratio) and p-value
#' @export
quality_test_ageratio <- function(
  survey_design,
  variables,
  yes_val = 1,
  no_val = 0,
  expected_ratio_val = 0.85
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_ageratio",
          message = "`variables` must be a character vector of length 2 (two column names)"
        )
      }

      age_group_col1 <- variables[[1]]
      age_group_col2 <- variables[[2]]

      if (
        !age_group_col1 %in% names(data) || !age_group_col2 %in% names(data)
      ) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio",
          message = "One or both age group columns not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (identical(age_group_col1, age_group_col2)) {
        phr_error(
          origin = "quality_test_ageratio",
          message = "The two column names in `variables` must be different"
        )
      }

      if (identical(yes_val, no_val)) {
        phr_error(
          origin = "quality_test_ageratio",
          message = "`yes_val` and `no_val` must be different"
        )
      }

      if (!is.numeric(expected_ratio_val) || length(expected_ratio_val) != 1L) {
        phr_error(
          origin = "quality_test_ageratio",
          message = "`expected_ratio_val` must be a single numeric value (col1:col2)"
        )
      }

      if (!is.finite(expected_ratio_val) || expected_ratio_val <= 0) {
        phr_error(
          origin = "quality_test_ageratio",
          message = "`expected_ratio_val` must be a finite positive number"
        )
      }

      x <- data[[age_group_col1]]
      y <- data[[age_group_col2]]

      # Remove missing values
      complete_cases <- complete.cases(x, y)
      x <- x[complete_cases]
      y <- y[complete_cases]

      if (length(x) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio",
          message = "Insufficient data for age ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Exclude values not equal to yes_val/no_val in either column, and warn
      valid_x <- (x == yes_val) | (x == no_val)
      valid_y <- (y == yes_val) | (y == no_val)
      valid <- valid_x & valid_y

      n_excluded <- sum(!valid)
      if (n_excluded > 0) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio",
          message = paste0(
            "Excluding ",
            n_excluded,
            " record(s) with values not equal to `yes_val` or `no_val` in one or both age group columns"
          )
        )
      }

      x <- x[valid]
      y <- y[valid]

      if (length(x) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio",
          message = "Insufficient valid data for age ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Observed "yes" counts in each indicator column
      yes1 <- sum(x == yes_val, na.rm = TRUE)
      yes2 <- sum(y == yes_val, na.rm = TRUE)

      if (yes1 == 0L || yes2 == 0L) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio",
          message = "One of the age groups has zero 'yes' counts; cannot test age ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      obs <- c(col1_yes = yes1, col2_yes = yes2)

      # expected_ratio_val is col1:col2
      p1 <- expected_ratio_val / (expected_ratio_val + 1)
      p2 <- 1 / (expected_ratio_val + 1)
      p <- c(p1, p2)

      obs_ratio <- yes1 / yes2

      test_result <- chisq.test(x = obs, p = p)

      return(list(
        statistic = obs_ratio,
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_ageratio"
  )
}

#' Digit Preference Score (internal helper)
#'
#' Computes the digit preference score (DPS) using the WHO MONICA formula.
#' The DPS is derived from a chi-squared test for uniformity over the
#' distribution of the final recorded digit.
#'
#' @param x Numeric vector of measurements (NAs should be removed beforehand).
#' @param digits Number of decimal places to which \code{x} is formatted before
#'   extracting the final digit. Default is \code{1}.
#' @param values Integer vector of possible final-digit values. Default is
#'   \code{0:9}.
#'
#' @return A single numeric DPS value, rounded to 2 decimal places.
#'
#' @references
#' Kuulasmaa K, Hense HW, Tolonen H (WHO MONICA Project).
#' Quality Assessment of Data on Blood Pressure in the WHO MONICA Project.
#' WHO MONICA Project e-publications No. 9, WHO, Geneva, 1998.
#' \url{https://www.thl.fi/publications/monica/bp/bpqa.htm}
#'
#' @noRd
digit_preference_score <- function(x, digits = 1, values = 0:9) {
  x_fmt <- formatC(x, digits = digits, format = "f")
  final_digit <- substr(x_fmt, nchar(x_fmt), nchar(x_fmt))
  tab <- table(factor(final_digit, levels = as.character(values)))
  chi_sq <- stats::chisq.test(tab)
  dps <- round(
    100 * sqrt(chi_sq$statistic / (sum(chi_sq$observed) * chi_sq$parameter)),
    2
  )
  as.numeric(dps)
}

#' Digit Preference Score (MUAC)
#'
#' Compute digit preference score for a numeric variable.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character scalar. Name of the column in `data`
#' @return List with statistic (digit preference score) and p_value (NA_real_)
#' @export
quality_test_digit_preference <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 1L) {
        phr_error(
          origin = "quality_test_digit_preference",
          message = "`variables` must be a single character column name"
        )
      }

      if (!variables %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_digit_preference",
          message = "Variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- data[[variables]]

      # Remove missing values
      x <- x[!is.na(x)]

      if (length(x) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_digit_preference",
          message = "Insufficient data for digit preference calculation"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (!is.numeric(x)) {
        phrutils::phr_warning(
          origin = "quality_test_digit_preference",
          message = "Variable is not numeric; cannot compute digit preference score"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      dp <- digit_preference_score(x)

      return(list(
        statistic = as.numeric(dp),
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_digit_preference"
  )
}

#' ANOVA Test
#'
#' Perform a one-way analysis of variance (ANOVA) to compare group means
#' across two or more groups.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 column names. The first
#'   element is the numeric outcome column and the second is the grouping
#'   column (must have at least 2 distinct levels).
#' @return List with statistic (F-value) and p_value
#' @export
quality_test_anova <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_anova",
          message = "`variables` must be a character vector of exactly 2 column names (outcome, group)"
        )
      }

      outcome_col <- variables[1]
      group_col <- variables[2]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_anova",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      is_valid <- phrutils::phr_validate_numeric(
        data[[outcome_col]],
        origin = "quality_test_anova",
        hint = phr_txt("ANOVA requires a numeric outcome variable."),
        soft = TRUE
      )

      if (isFALSE(is_valid)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      outcome <- as.numeric(data[[outcome_col]])
      group <- data[[group_col]]

      complete_cases <- complete.cases(outcome, group)
      outcome <- outcome[complete_cases]
      group <- group[complete_cases]

      if (length(outcome) < 3) {
        phrutils::phr_warning(
          origin = "quality_test_anova",
          message = "Insufficient data for ANOVA (need at least 3 non-missing observations)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      group <- as.factor(group)

      if (nlevels(group) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_anova",
          message = "Group column must have at least 2 distinct levels for ANOVA"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      model <- aov(
        outcome ~ group,
        data = data.frame(outcome = outcome, group = group)
      )
      anova_table <- summary(model)[[1]]

      return(list(
        statistic = as.numeric(anova_table[["F value"]][1]),
        p_value = as.numeric(anova_table[["Pr(>F)"]][1])
      ))
    },
    on_error = "warn",
    origin = "quality_test_anova"
  )
}

#' Chi-Squared Test with Binary Contingency Table
#'
#' Perform a chi-squared test of independence between two categorical columns in
#' the dataset. The contingency table is built internally from the raw data.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 variable names. The first
#'   element is the first categorical column and the second element is the
#'   second categorical column. A contingency table is formed from their
#'   cross-tabulation.
#' @return List with statistic (chi-squared value) and p_value
#' @export
quality_test_chisq_binary <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_chisq_binary",
          message = "`variables` must be a character vector of exactly 2 column names"
        )
      }

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_chisq_binary",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      cat1_col <- variables[1]
      cat2_col <- variables[2]

      x <- data[[cat1_col]]
      y <- data[[cat2_col]]

      complete_cases <- complete.cases(x, y)
      x <- x[complete_cases]
      y <- y[complete_cases]

      if (length(x) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_chisq_binary",
          message = "Insufficient data for chi-squared binary test (need at least 5 complete observations)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      contingency_table <- table(x, y)

      if (any(dim(contingency_table) < 2)) {
        phrutils::phr_warning(
          origin = "quality_test_chisq_binary",
          message = "Contingency table too small for chi-squared test (each variable must have at least 2 distinct levels)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      test_result <- chisq.test(contingency_table)

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = test_result$p.value
      ))
    },
    on_error = "warn",
    origin = "quality_test_chisq_binary"
  )
}

#' Binomial Ratio Test (Row-wise)
#'
#' Perform a row-wise exact binomial proportion test comparing the observed
#' success rate against an expected ratio, appending a p-value column to the
#' data frame.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 column names. The first
#'   element is the column containing the number of successes (non-negative
#'   integer) and the second is the column containing the total number of
#'   trials (positive integer \eqn{\geq} success count).
#' @param expected_ratio Numeric scalar. Expected proportion under the null
#'   hypothesis (default: 0.5; must be strictly between 0 and 1)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @param pval_colname Character scalar. Name for the new p-value column
#'   (default: \code{"p_value"})
#' @return Data frame with an additional column containing the row-wise
#'   binomial test p-values (rounded to 5 decimal places); rows with missing
#'   or invalid values receive \code{NA}
#' @export
quality_test_binomial_ratio_rowwise <- function(
  survey_design,
  variables,
  expected_ratio = 0.5,
  alternative = "two.sided",
  pval_colname = "p_value"
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_binomial_ratio_rowwise",
          message = "`variables` must be a character vector of exactly 2 column names (success, total)"
        )
      }

      if (
        !is.numeric(expected_ratio) ||
          length(expected_ratio) != 1L ||
          is.na(expected_ratio) ||
          expected_ratio <= 0 ||
          expected_ratio >= 1
      ) {
        phr_error(
          origin = "quality_test_binomial_ratio_rowwise",
          message = "`expected_ratio` must be a single numeric value strictly between 0 and 1"
        )
      }

      if (!alternative %in% c("two.sided", "greater", "less")) {
        phr_error(
          origin = "quality_test_binomial_ratio_rowwise",
          message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
        )
      }

      if (!is.character(pval_colname) || length(pval_colname) != 1L) {
        phr_error(
          origin = "quality_test_binomial_ratio_rowwise",
          message = "`pval_colname` must be a single character string"
        )
      }

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_binomial_ratio_rowwise",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(data)
      }

      success_col <- variables[1]
      total_col <- variables[2]
      success_vals <- data[[success_col]]
      total_vals <- data[[total_col]]

      p_values <- vapply(
        seq_len(nrow(data)),
        function(i) {
          s <- success_vals[i]
          n <- total_vals[i]

          if (
            is.na(s) ||
              is.na(n) ||
              s < 0 ||
              n <= 0 ||
              n < s ||
              n != floor(n) ||
              s != floor(s)
          ) {
            return(NA_real_)
          }

          tryCatch(
            round(
              binom.test(
                x = as.integer(s),
                n = as.integer(n),
                p = expected_ratio,
                alternative = alternative
              )$p.value,
              digits = 5
            ),
            error = function(e) NA_real_
          )
        },
        numeric(1)
      )

      data[[pval_colname]] <- p_values
      return(data)
    },
    on_error = "warn",
    origin = "quality_test_binomial_ratio_rowwise"
  )
}

#' One-Sample T-Test from Actual Data (Row-wise Across Columns)
#'
#' Perform a row-wise one-sample t-test on the actual data values. For each
#' row, the values across the columns specified in \code{variables} are
#' extracted and used to compute a one-sample t-test against
#' \code{expected_mean}. A p-value column is appended to the data frame.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of column names whose values are used
#'   row-wise for the t-test. At least 2 columns must be provided so that a
#'   standard deviation can be estimated.
#' @param expected_mean Numeric scalar. Hypothesised population mean under the
#'   null hypothesis (default: 0)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @param pval_colname Character scalar. Name for the new p-value column
#'   (default: \code{"p_value"})
#' @return Data frame with an additional column containing the row-wise
#'   t-test p-values (rounded to 3 decimal places); rows with fewer than 2
#'   non-missing values receive \code{NA}
#' @export
quality_test_ttest_summary_rowwise <- function(
  survey_design,
  variables,
  expected_mean = 0,
  alternative = "two.sided",
  pval_colname = "p_value"
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) < 2L) {
        phr_error(
          origin = "quality_test_ttest_summary_rowwise",
          message = "`variables` must be a character vector of at least 2 column names"
        )
      }

      if (
        !is.numeric(expected_mean) ||
          length(expected_mean) != 1L ||
          is.na(expected_mean)
      ) {
        phr_error(
          origin = "quality_test_ttest_summary_rowwise",
          message = "`expected_mean` must be a single non-missing numeric value"
        )
      }

      if (!alternative %in% c("two.sided", "greater", "less")) {
        phr_error(
          origin = "quality_test_ttest_summary_rowwise",
          message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
        )
      }

      if (!is.character(pval_colname) || length(pval_colname) != 1L) {
        phr_error(
          origin = "quality_test_ttest_summary_rowwise",
          message = "`pval_colname` must be a single character string"
        )
      }

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_ttest_summary_rowwise",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(data)
      }

      existing_vars <- intersect(variables, names(data))

      subset_mat <- as.matrix(as.data.frame(lapply(
        data[, existing_vars, drop = FALSE],
        as.numeric
      )))

      p_values <- vapply(
        seq_len(nrow(subset_mat)),
        function(i) {
          row_vals <- subset_mat[i, ]
          row_vals <- row_vals[!is.na(row_vals)]

          if (length(row_vals) < 2) {
            return(NA_real_)
          }

          tryCatch(
            round(
              t.test(
                row_vals,
                mu = expected_mean,
                alternative = alternative
              )$p.value,
              digits = 3
            ),
            error = function(e) NA_real_
          )
        },
        numeric(1)
      )

      data[[pval_colname]] <- p_values
      return(data)
    },
    on_error = "warn",
    origin = "quality_test_ttest_summary_rowwise"
  )
}

#' Poisson Rate Test
#'
#' Perform an exact Poisson test comparing an observed event count
#' against an expected rate, appending a p-value column to the data frame.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character vector of exactly 2 column names. The first
#'   element is the column containing the number of events (non-negative
#'   integer vector) and the second is the column containing the exposure units
#'   (positive numeric vector, e.g., person-time or number of households).
#' @param expected_rate Numeric scalar. Expected event rate per unit exposure
#'   under the null hypothesis (default: 0.02; must be positive)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @return Data frame with an additional column containing the row-wise
#'   Poisson test p-values (rounded to 5 decimal places); rows with missing
#'   or invalid values receive \code{NA}
#' @export
quality_test_poisson_ratio <- function(
  survey_design,
  variables,
  expected_rate = 0.02,
  alternative = "two.sided"
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_poisson_ratio",
          message = "`variables` must be a character vector of exactly 2 column names (events, exposure)"
        )
      }

      if (
        !is.numeric(expected_rate) ||
          length(expected_rate) != 1L ||
          is.na(expected_rate) ||
          expected_rate <= 0
      ) {
        phr_error(
          origin = "quality_test_poisson_ratio",
          message = "`expected_rate` must be a single positive numeric value"
        )
      }

      if (!alternative %in% c("two.sided", "greater", "less")) {
        phr_error(
          origin = "quality_test_poisson_ratio",
          message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
        )
      }

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_poisson_ratio",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(data)
      }

      event_col <- variables[1]
      exposure_col <- variables[2]

      event_vals <- suppressWarnings(as.numeric(data[[event_col]]))
      exposure_vals <- suppressWarnings(as.numeric(data[[exposure_col]]))

      # Aggregate
      total_events <- sum(event_vals, na.rm = TRUE)
      total_exposure <- sum(exposure_vals, na.rm = TRUE)

      # Validate aggregated values
      if (
        is.na(total_events) ||
          is.na(total_exposure) ||
          total_exposure <= 0 ||
          total_events < 0
      ) {
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Perform single Poisson exact test
      pt <- tryCatch(
        poisson.test(
          x = total_events,
          T = total_exposure,
          r = expected_rate,
          alternative = alternative
        ),
        error = function(e) NULL
      )

      if (is.null(pt)) {
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Extract statistic + p-value
      test_statistic <- as.numeric(pt$statistic)
      p_value <- as.numeric(pt$p.value)
      # test_statistic <- unname(pt$statistic) # usually rate ratio or test statistic
      # p_value <- round(pt$p.value, 5)

      return(list(
        test_statistic = test_statistic,
        p_value = p_value
      ))
    },
    on_error = "warn",
    origin = "quality_test_poisson_ratio"
  )
}

#' Non-missing Count Test
#'
#' Return the number of non-missing (non-NA) values in a specified column.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param variables Character scalar. Name of the column to count non-missing values for
#' @return List with statistic (non-missing count) and p_value (NA_real_)
#' @export
quality_test_count <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      if (!is.character(variables) || length(variables) != 1L) {
        phr_error(
          origin = "quality_test_count",
          message = "`variables` must be a single character column name"
        )
      }

      if (!variables %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_count",
          message = "Column not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      x <- data[[variables]]
      n_non_missing <- sum(!is.na(x))

      return(list(
        statistic = as.numeric(n_non_missing),
        p_value = NA_real_
      ))
    },
    on_error = "warn",
    origin = "quality_test_count"
  )
}

#' Index of Dispersion Test for Spatial Event Distribution
#'
#' Computes the Index of Dispersion (variance-to-mean ratio) for event counts
#' across survey clusters or sites and performs the classical chi-square
#' dispersion test. This test evaluates whether the spatial distribution of
#' events is consistent with a Poisson process (equidispersion), or whether
#' there is evidence of over-dispersion (clustering) or under-dispersion
#' (uniformity).
#'
#' The function accepts record-level survey data containing an event-count
#' column and a cluster-identifier column. Event counts are aggregated to the
#' cluster level, and the dispersion test is applied to the resulting
#' cluster-level totals. The function returns:
#' \itemize{
#'   \item \code{test_statistic}: the chi-square statistic
#'   \item \code{p_value}: the p-value for the dispersion test
#' }
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 2. The first element must be
#'   the column containing record-level event counts (non-negative numeric),
#'   and the second must be the column containing cluster or site identifiers.
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{test_statistic} The chi-square test statistic for the
#'       index of dispersion test.
#'     \item \code{p_value} The p-value associated with the chi-square test.
#'   }
#'
#' @details
#' Event counts are first aggregated by cluster. Let \eqn{X_i} denote the total
#' number of events in cluster \eqn{i}. The Index of Dispersion (ID) is defined as:
#'
#' \deqn{ID = \frac{\mathrm{Var}(X_i)}{\mathrm{Mean}(X_i)}}
#'
#' Under the null hypothesis that cluster-level counts follow a Poisson
#' distribution, the statistic:
#'
#' \deqn{X^2 = (n - 1) \times ID}
#'
#' approximately follows a chi-square distribution with \eqn{n - 1} degrees of
#' freedom, where \eqn{n} is the number of clusters.
#'
#' A small p-value indicates significant over-dispersion (clustering) or
#' under-dispersion (uniformity), suggesting potential spatial bias or data
#' quality issues.
#'
#' Records with missing event counts or missing cluster identifiers are
#' excluded. Negative event counts trigger a warning and result in \code{NA}
#' outputs. If fewer than two clusters contain valid data, the function returns
#' \code{NA} for both outputs.
#'
#' @export
quality_test_index_dispersion <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phrutils::phr_get_data_from_design(survey_design)

      # Expect exactly 2 variables: event_count, cluster_id
      if (!is.character(variables) || length(variables) != 2L) {
        phrutils::phr_error(
          origin = "quality_test_index_dispersion",
          message = "`variables` must be a character vector of length 2 (event_count, cluster_id)"
        )
      }

      event_col <- variables[1]
      cluster_col <- variables[2]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_index_dispersion",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Extract
      events <- suppressWarnings(as.numeric(data[[event_col]]))
      clusters <- data[[cluster_col]]

      cc <- complete.cases(events, clusters)
      events <- events[cc]
      clusters <- clusters[cc]

      if (length(events) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_index_dispersion",
          message = "Insufficient data for dispersion test"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      if (any(events < 0, na.rm = TRUE)) {
        phrutils::phr_warning(
          origin = "quality_test_index_dispersion",
          message = "Negative event counts found; returning NA"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Aggregate to cluster-level counts
      events_by_cluster <- tapply(events, clusters, sum)

      # Validate cluster-level counts
      if (length(events_by_cluster) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_index_dispersion",
          message = "Need at least 2 clusters with non-missing counts"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      mean_count <- mean(events_by_cluster)
      var_count <- var(events_by_cluster)

      if (mean_count <= 0) {
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Index of dispersion
      ID <- var_count / mean_count

      # Chi-square statistic
      n <- length(events_by_cluster)
      test_statistic <- (n - 1) * ID

      # p-value
      p_value <- pchisq(test_statistic, df = n - 1, lower.tail = FALSE)

      return(list(
        test_statistic = as.numeric(test_statistic),
        p_value = as.numeric(round(p_value, 5))
      ))
    },
    on_error = "warn",
    origin = "quality_test_index_dispersion"
  )
}

#' Chi-Square Test for Temporal Distribution of Events Across Months
#'
#' This function evaluates whether the temporal distribution of events (e.g.,
#' deaths) across months is consistent with an expected uniform pattern. It
#' accepts a survey design object and two variables: one containing the number
#' of events recorded for each survey record, and one containing comma-separated
#' dates of those events. The function performs several validation and
#' transformation steps before running a chi-square goodness-of-fit test on the
#' monthly distribution of events.
#'
#' Specifically, the function:
#' \itemize{
#'   \item Checks that the number of events in each record matches the number of
#'         comma-separated dates provided.
#'   \item Expands comma-separated dates into long format (one date per row).
#'   \item Converts each date to a month-year string (YYYY-MM).
#'   \item Aggregates counts of events per month-year.
#'   \item Performs a chi-square goodness-of-fit test comparing observed monthly
#'         counts to an expected uniform distribution.
#' }
#'
#' The function returns only the chi-square test statistic and p-value, suitable
#' for use in mortality plausibility scoring frameworks.
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 2. The first element must be
#'   the column containing the number of events per record (non-negative
#'   integer). The second element must be the column containing comma-separated
#'   dates of events (character).
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{test_statistic} The chi-square test statistic for the
#'           goodness-of-fit test.
#'     \item \code{p_value} The p-value associated with the chi-square test.
#'   }
#'
#' @details
#' The chi-square goodness-of-fit test evaluates whether the observed counts of
#' events per month-year differ significantly from an expected uniform
#' distribution. A small p-value indicates temporal irregularities such as
#' heaping, displacement, or recall bias.
#'
#' Records where the number of events does not match the number of dates are
#' flagged, and the function returns \code{NA} values for both outputs.
#'
#' @export
quality_test_events_months_distribution <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Validate inputs
      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_events_months_distribution",
          message = "`variables` must be a character vector of exactly 2 column names (events, dates)"
        )
      }

      event_col <- variables[1]
      dates_col <- variables[2]

      if (!event_col %in% names(data) || !dates_col %in% names(data)) {
        phrutils::phr_warning(
          origin = "quality_test_events_months_distribution",
          message = paste0(
            "Columns not found in data: ",
            paste(variables, collapse = ", ")
          )
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      events <- data[[event_col]]
      dates <- data[[dates_col]]

      # Basic validation
      if (all(is.na(events)) || all(is.na(dates))) {
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # ---- Step 1: Check consistency: number of deaths vs number of dates
      check_ok <- vapply(
        seq_len(nrow(data)),
        function(i) {
          ev <- events[i]
          dt <- dates[i]

          if (is.na(ev) && is.na(dt)) {
            return(TRUE)
          }
          if (is.na(ev) && !is.na(dt)) {
            return(FALSE)
          }
          if (!is.na(ev) && is.na(dt)) {
            return(ev == 0)
          }

          # Split comma-separated dates
          dt_split <- strsplit(dt, ",")[[1]]
          dt_split <- trimws(dt_split)

          return(length(dt_split) == ev)
        },
        logical(1)
      )

      if (!all(check_ok)) {
        phrutils::phr_warning(
          origin = "quality_test_events_months_distribution",
          message = "Mismatch between number of deaths and number of dates in some records"
        )
        # Continue anyway, but return NA
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # ---- Step 2: Expand dates into long format
      long_dates <- do.call(
        rbind,
        lapply(seq_len(nrow(data)), function(i) {
          ev <- events[i]
          dt <- dates[i]

          if (is.na(ev) || ev == 0 || is.na(dt)) {
            return(NULL)
          }

          dt_split <- strsplit(dt, ",")[[1]]
          dt_split <- trimws(dt_split)

          data.frame(date = dt_split, stringsAsFactors = FALSE)
        })
      )

      if (is.null(long_dates) || nrow(long_dates) == 0) {
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # ---- Step 3: Convert to month-year
      long_dates$date <- suppressWarnings(as.Date(long_dates$date))

      if (any(is.na(long_dates$date))) {
        phrutils::phr_warning(
          origin = "quality_test_events_months_distribution",
          message = "Some dates could not be parsed; returning NA"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      long_dates$month_year <- format(long_dates$date, "%Y-%m")

      # ---- Step 4: Count deaths per month-year
      counts <- table(long_dates$month_year)

      if (length(counts) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_events_months_distribution",
          message = "Insufficient distinct months for chi-squared test"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # ---- Step 5: Chi-square goodness-of-fit test
      expected <- rep(sum(counts) / length(counts), length(counts))

      test_result <- suppressWarnings(
        chisq.test(x = as.numeric(counts), p = expected / sum(expected))
      )

      return(list(
        test_statistic = as.numeric(test_result$statistic),
        p_value = round(test_result$p.value, 5)
      ))
    },
    on_error = "warn",
    origin = "quality_test_events_months_distribution"
  )
}

#' ANOVA Test for Event Rates Adjusted by Exposure
#'
#' This function performs a one-way analysis of variance (ANOVA) to evaluate
#' whether event *rates* differ significantly across groups (e.g., enumerators,
#' clusters, sites). It accepts three variables: the number of events observed,
#' an exposure measure (such as person-time or number of household members),
#' and a grouping variable. The function computes an event rate for each record
#' as \code{events / exposure}, then fits an ANOVA model of the form:
#'
#' \deqn{\text{rate} \sim \text{group}}
#'
#' The function returns only the ANOVA F-statistic and associated p-value,
#' suitable for use in survey quality assessment and plausibility scoring.
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 3. The first element must be
#'   the column containing the number of events (non-negative numeric). The
#'   second element must be the column containing the exposure measure (positive
#'   numeric). The third element must be the grouping variable (categorical)
#'   with at least two distinct levels.
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{statistic} The F-statistic from the ANOVA model.
#'     \item \code{p_value} The p-value associated with the F-test.
#'   }
#'
#' @details
#' The ANOVA evaluates whether the mean event rate differs across groups. A
#' small p-value indicates that event rates vary significantly between groups,
#' which may suggest enumerator bias, spatial bias, or other systematic
#' differences in data collection.
#'
#' Records with missing or invalid values (e.g., non-positive exposure) are
#' excluded from the analysis. If insufficient valid data remain, the function
#' returns \code{NA} for both outputs.
#'
#' @export
quality_test_anova_by_exposure <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Expect exactly 3 columns: events, exposure, group
      if (!is.character(variables) || length(variables) != 3L) {
        phr_error(
          origin = "quality_test_anova_by_exposure",
          message = "`variables` must be a character vector of exactly 3 column names (events, exposure, group)"
        )
      }

      event_col <- variables[1]
      exposure_col <- variables[2]
      group_col <- variables[3]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_anova_by_exposure",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      events <- data[[event_col]]
      exposure <- data[[exposure_col]]
      group <- data[[group_col]]

      # Validate numeric inputs
      is_valid_events <- phrutils::phr_validate_numeric(
        events,
        origin = "quality_test_anova_by_exposure",
        hint = phr_txt("Events column must be numeric."),
        soft = TRUE
      )

      is_valid_exposure <- phrutils::phr_validate_numeric(
        exposure,
        origin = "quality_test_anova_by_exposure",
        hint = phr_txt("Exposure column must be numeric."),
        soft = TRUE
      )

      if (isFALSE(is_valid_events) || isFALSE(is_valid_exposure)) {
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Remove missing values
      complete_cases <- complete.cases(events, exposure, group)
      events <- events[complete_cases]
      exposure <- exposure[complete_cases]
      group <- group[complete_cases]

      if (length(events) < 3) {
        phrutils::phr_warning(
          origin = "quality_test_anova_by_exposure",
          message = "Insufficient data for ANOVA (need at least 3 non-missing observations)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Filter out records with invalid (non-positive) exposure
      initial_count <- length(events)
      valid_exposure <- exposure > 0
      events <- events[valid_exposure]
      exposure <- exposure[valid_exposure]
      group <- group[valid_exposure]
      removed_count <- initial_count - length(events)

      if (removed_count > 0) {
        phrutils::phr_warning(
          origin = "quality_test_anova_by_exposure",
          message = paste0(
            removed_count,
            " records with non-positive exposure were removed from the calculation"
          )
        )
      }

      if (length(events) < 3) {
        phrutils::phr_warning(
          origin = "quality_test_anova_by_exposure",
          message = "Insufficient data for ANOVA after filtering (need at least 3 valid observations)"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Compute rate = events / exposure

      rate <- events / exposure
      group <- as.factor(group)

      if (nlevels(group) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_anova_by_exposure",
          message = "Group column must have at least 2 distinct levels for ANOVA"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Run ANOVA on rate ~ group
      model <- aov(
        rate ~ group,
        data = data.frame(rate = rate, group = group)
      )
      anova_table <- summary(model)[[1]]

      return(list(
        statistic = as.numeric(anova_table[["F value"]][1]),
        p_value = as.numeric(anova_table[["Pr(>F)"]][1])
      ))
    },
    on_error = "warn",
    origin = "quality_test_anova"
  )
}

#' Variance Partitioning of Event Rates Across Two Grouping Levels
#'
#' This function estimates how much of the variation in event *rates* is
#' attributable to two hierarchical grouping structures, typically enumerators
#' (group 1) and clusters (group 2). It accepts four variables: the number of
#' events observed, an exposure measure (e.g., person-time or number of
#' household members), a group 1 identifier, and a group 2 identifier.
#'
#' The function computes an event rate for each record as:
#' \deqn{\text{rate} = \frac{\text{events}}{\text{exposure}}}
#'
#' and fits a mixed-effects model of the form:
#' \deqn{\text{rate} \sim 1 + (1|\text{group1}) + (1|\text{group2})}
#'
#' From this model, the function extracts the variance components associated
#' with group 1, group 2, and the residual error. It then calculates the
#' percentage of total variance attributable to group 1, which serves as a
#' measure of how strongly event reporting varies by enumerator or team after
#' accounting for cluster-level differences.
#'
#' A likelihood ratio test is performed to assess whether the group 1 random
#' effect significantly improves model fit compared to a model containing only
#' the group 2 random effect. The resulting p-value indicates whether variation
#' attributable to group 1 is statistically meaningful.
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 4. The elements must be, in
#'   order: (1) the events column, (2) the exposure column, (3) the group 1
#'   identifier (e.g., enumerator or team), and (4) the group 2 identifier
#'   (e.g., cluster or site).
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{test_statistic} The percentage of total variance in event
#'           rates attributable to group 1.
#'     \item \code{p_value} The p-value from the likelihood ratio test comparing
#'           the full model to a reduced model without the group 1 random
#'           effect.
#'   }
#'
#' @details
#' This test is designed for survey-based event data where events may be rare
#' and exposure varies across records. Using event rates rather than raw counts
#' helps distinguish true underlying differences between clusters from
#' enumerator-driven variation in reporting.
#'
#' A high percentage of variance attributed to group 1 suggests potential
#' enumerator bias or inconsistent reporting practices. A low percentage
#' indicates that most variation is driven by genuine differences between
#' clusters.
#'
#' If the mixed-effects model fails to converge or insufficient valid data are
#' available, the function returns \code{NA} for both outputs.
#'
#' @export
quality_test_event_group_variance <- function(survey_design, variables) {
  phrutils::phr_try(
    {
      data <- phrutils::phr_get_data_from_design(survey_design)

      # Expect exactly 4 columns: events, exposure, group1, group2
      if (!is.character(variables) || length(variables) != 4L) {
        phrutils::phr_error(
          origin = "quality_test_event_group_variance",
          message = "`variables` must be a character vector of exactly 4 column names (events, exposure, group1, group2)"
        )
      }

      event_col <- variables[1]
      exposure_col <- variables[2]
      group1_col <- variables[3] # enumerator/team
      group2_col <- variables[4] # cluster/site

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_event_group_variance",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      events <- data[[event_col]]
      exposure <- data[[exposure_col]]
      group1 <- as.factor(data[[group1_col]])
      group2 <- as.factor(data[[group2_col]])

      # Filter out records with invalid (non-positive) exposure
      initial_count <- length(events)
      valid_exposure <- exposure > 0
      events <- events[valid_exposure]
      exposure <- exposure[valid_exposure]
      group1 <- group1[valid_exposure]
      group2 <- group2[valid_exposure]
      removed_count <- initial_count - length(events)

      if (removed_count > 0) {
        phrutils::phr_warning(
          origin = "quality_test_event_group_variance",
          message = paste0(
            removed_count,
            " records with non-positive exposure were removed from the calculation"
          )
        )
      }

      # Remove missing
      cc <- complete.cases(events, exposure, group1, group2)
      events <- events[cc]
      exposure <- exposure[cc]
      group1 <- group1[cc]
      group2 <- group2[cc]

      if (length(events) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_event_group_variance",
          message = "Insufficient data for variance partitioning"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Compute rate = events / exposure
      rate <- events / exposure

      # Fit full mixed model: rate ~ 1 + (1|group1) + (1|group2)
      full_model <- tryCatch(
        # suppressMessages(
        lme4::lmer(rate ~ 1 + (1 | group1) + (1 | group2)),
        # )
        error = function(e) NULL
      )

      if (is.null(full_model)) {
        phrutils::phr_warning(
          origin = "quality_test_variance_partitioning",
          message = "Mixed model failed to converge"
        )
        return(list(test_statistic = NA_real_, p_value = NA_real_))
      }

      # Extract variance components
      vc <- lme4::VarCorr(full_model)
      var_group1 <- as.numeric(vc$group1)
      var_group2 <- as.numeric(vc$group2)
      var_resid <- attr(vc, "sc")^2

      total_var <- var_group1 + var_group2 + var_resid

      # Percent variance attributed to group1 (enumerators)
      pct_group1 <- 100 * var_group1 / total_var

      # Likelihood ratio test for group1 random effect
      reduced_model <- tryCatch(
        # suppressMessages(
        lme4::lmer(rate ~ 1 + (1 | group2)),
        error = function(e) NULL
        # )
      )

      if (is.null(reduced_model)) {
        return(list(test_statistic = pct_group1, p_value = NA_real_))
      }

      lrt <- suppressWarnings(
        anova(reduced_model, full_model)
      )

      p_value <- round(lrt$`Pr(>Chisq)`[2], 5)

      return(list(
        test_statistic = pct_group1,
        p_value = p_value
      ))
    },
    on_error = "warn",
    origin = "quality_test_event_group_variance"
  )
}

#' Sex Ratio Consistency Test Against Expected Ratio (Count-Based)
#'
#' This function evaluates whether the observed sex ratio (male vs female)
#' differs significantly from an expected ratio supplied by the user.
#' It is designed for household-level survey data where each record contains
#' numeric counts of males and females.
#'
#' A 2×1 contingency table is constructed from total male and female counts.
#' A chi-squared goodness-of-fit test is performed comparing observed counts
#' to expected proportions:
#'
#' \deqn{\text{Observed sex counts} \;\sim\; \text{Expected male:female ratio}}
#'
#' @param survey_design A srvyr survey design object.
#' @param variables A character vector of length 2: male count column, female count column.
#' @param expected_ratio A numeric vector of length 2 giving expected proportions
#'        (e.g., c(male = 0.51, female = 0.49)).
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{statistic} Chi-squared test statistic.
#'     \item \code{p_value} P-value from the goodness-of-fit test.
#'   }
#'
#' @export
quality_test_sexratio_count <- function(
  survey_design,
  variables,
  expected_ratio = c(0.5, 0.5)
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Expect exactly 2 variables: num_male, num_female
      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_sexratio_count_expected",
          message = "`variables` must be a character vector of length 2 (num_male, num_female)"
        )
      }

      male_col <- variables[1]
      female_col <- variables[2]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_expected",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate expected ratio
      if (
        !is.numeric(expected_ratio) ||
          length(expected_ratio) != 2 ||
          any(expected_ratio < 0) ||
          sum(expected_ratio) <= 0
      ) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_expected",
          message = "`expected_ratio` must be numeric length 2 with non-negative values"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Normalize expected ratio to proportions
      expected_ratio <- expected_ratio / sum(expected_ratio)

      male_counts <- data[[male_col]]
      female_counts <- data[[female_col]]

      cc <- complete.cases(male_counts, female_counts)
      male_counts <- male_counts[cc]
      female_counts <- female_counts[cc]

      if (length(male_counts) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_expected",
          message = "Insufficient data for sex ratio expected test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Must be non-negative numeric counts
      if (
        any(male_counts < 0, na.rm = TRUE) ||
          any(female_counts < 0, na.rm = TRUE)
      ) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_expected",
          message = "Negative counts found; cannot test sex ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Aggregate totals
      total_male <- sum(male_counts)
      total_female <- sum(female_counts)

      if (total_male == 0 || total_female == 0) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_expected",
          message = "One sex category has zero total counts; cannot test sex ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      observed <- c(male = total_male, female = total_female)

      # Expected counts under expected ratio
      expected <- sum(observed) * expected_ratio

      # Chi-square goodness-of-fit test
      test_result <- suppressWarnings(chisq.test(
        x = observed,
        p = expected_ratio
      ))

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = as.numeric(test_result$p.value)
      ))
    },
    on_error = "warn",
    origin = "quality_test_sexratio_count_expected"
  )
}

#' Sex Ratio Consistency Test Across Groups (Count-Based)
#'
#' This function evaluates whether the reported sex ratio (male vs female)
#' differs significantly across groups (e.g., enumerators, teams, clusters).
#' It is designed for household-level survey data where each record may contain
#' multiple individuals. Instead of a single sex variable, the function accepts
#' two numeric count columns: number of males and number of females in each
#' household.
#'
#' A 2×K contingency table is constructed, where rows represent sex
#' (male/female) and columns represent groups. A chi-squared test of
#' independence is then performed:
#'
#' \deqn{\text{sex counts} \;\perp\!\!\!\perp\; \text{group}}
#'
#' The resulting p-value indicates whether sex ratios vary across groups more
#' than expected by chance. The test statistic is the chi-squared value from
#' the independence test.
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 3. The first element must be
#'   the male count column, the second the female count column, and the third
#'   the grouping column.
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{statistic} The chi-squared test statistic from the
#'           sex-count-by-group contingency table.
#'     \item \code{p_value} The p-value from the chi-squared test of
#'           independence.
#'   }
#'
#' @details
#' This test detects inconsistent reporting of sex composition across groups.
#' A low p-value indicates that at least one group reports a sex ratio that
#' differs significantly from the others, which may suggest enumerator bias,
#' data entry issues, or inconsistent interpretation of household composition.
#'
#' Records with missing values in any of the required columns are excluded.
#' Negative counts trigger a warning and result in \code{NA} outputs. If
#' insufficient valid data remain or if only one sex or one group has non-zero
#' counts, the function returns \code{NA} for both outputs.
#'
#' @export
quality_test_sexratio_count_group <- function(
  survey_design,
  variables
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Expect exactly 3 variables: num_male, num_female, group
      if (!is.character(variables) || length(variables) != 3L) {
        phr_error(
          origin = "quality_test_sexratio_count_group",
          message = "`variables` must be a character vector of length 3 (num_male, num_female, group column)"
        )
      }

      male_col <- variables[1]
      female_col <- variables[2]
      group_col <- variables[3]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_group",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      male_counts <- data[[male_col]]
      female_counts <- data[[female_col]]
      groups <- data[[group_col]]

      cc <- complete.cases(male_counts, female_counts, groups)
      male_counts <- male_counts[cc]
      female_counts <- female_counts[cc]
      groups <- groups[cc]

      if (length(male_counts) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_group",
          message = "Insufficient data for sex ratio count group test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Must be non-negative numeric counts
      if (
        any(male_counts < 0, na.rm = TRUE) ||
          any(female_counts < 0, na.rm = TRUE)
      ) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_group",
          message = "Negative counts found; cannot test sex ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      grp <- as.factor(groups)

      # Aggregate counts by group
      male_by_group <- tapply(male_counts, grp, sum)
      female_by_group <- tapply(female_counts, grp, sum)

      # Need at least 2 groups and both sexes present
      if (length(male_by_group) < 2 || length(female_by_group) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_group",
          message = "Insufficient variation across groups for sex ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (all(male_by_group == 0) || all(female_by_group == 0)) {
        phrutils::phr_warning(
          origin = "quality_test_sexratio_count_group",
          message = "One sex category has zero counts across all groups; cannot test sex ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Build 2×K contingency table
      tab <- rbind(
        male = male_by_group,
        female = female_by_group
      )

      # Chi-square test of independence
      test_result <- suppressWarnings(chisq.test(tab))

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = as.numeric(test_result$p.value)
      ))
    },
    on_error = "warn",
    origin = "quality_test_sexratio_count_group"
  )
}

#' Age Group Ratio Test Against Expected Ratio (Count-Based)
#'
#' This function evaluates whether the observed ratio of individuals in two
#' age groups differs significantly from an expected ratio supplied by the user.
#' It is designed for household-level survey data where each record contains
#' numeric counts of individuals in two age groups.
#'
#' A chi-squared goodness-of-fit test is performed comparing observed total
#' counts to expected proportions:
#'
#' \deqn{\text{Observed age-group counts} \;\sim\; \text{Expected ratio}}
#'
#' @param survey_design A srvyr survey design object.
#' @param variables A character vector of length 2: age group 1 count column,
#'        age group 2 count column.
#' @param expected_ratio A numeric vector of length 2 giving expected proportions
#'        (e.g., c(0.60, 0.40)).
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{statistic} Chi-squared test statistic.
#'     \item \code{p_value} P-value from the goodness-of-fit test.
#'   }
#'
#' @export
quality_test_ageratio_count <- function(
  survey_design,
  variables,
  expected_ratio
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Expect exactly 2 variables: agegroup1_count, agegroup2_count
      if (!is.character(variables) || length(variables) != 2L) {
        phr_error(
          origin = "quality_test_ageratio_count",
          message = "`variables` must be a character vector of length 2 (agegroup1_count, agegroup2_count)"
        )
      }

      age1_col <- variables[1]
      age2_col <- variables[2]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Validate expected ratio
      if (
        !is.numeric(expected_ratio) ||
          length(expected_ratio) != 2 ||
          any(expected_ratio < 0) ||
          sum(expected_ratio) <= 0
      ) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count",
          message = "`expected_ratio` must be numeric length 2 with non-negative values"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Normalize expected ratio to proportions
      expected_ratio <- expected_ratio / sum(expected_ratio)

      age1_counts <- data[[age1_col]]
      age2_counts <- data[[age2_col]]

      cc <- complete.cases(age1_counts, age2_counts)
      age1_counts <- age1_counts[cc]
      age2_counts <- age2_counts[cc]

      if (length(age1_counts) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count",
          message = "Insufficient data for age ratio expected test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Must be non-negative numeric counts
      if (
        any(age1_counts < 0, na.rm = TRUE) ||
          any(age2_counts < 0, na.rm = TRUE)
      ) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count",
          message = "Negative counts found; cannot test age ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Aggregate totals
      total_age1 <- sum(age1_counts)
      total_age2 <- sum(age2_counts)

      if (total_age1 == 0 || total_age2 == 0) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count",
          message = "One age group has zero total counts; cannot test age ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      observed <- c(age_group1 = total_age1, age_group2 = total_age2)

      # Expected counts under expected ratio
      expected <- sum(observed) * expected_ratio

      # Chi-square goodness-of-fit test
      test_result <- suppressWarnings(chisq.test(
        x = observed,
        p = expected_ratio
      ))

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = as.numeric(test_result$p.value)
      ))
    },
    on_error = "warn",
    origin = "quality_test_ageratio_count"
  )
}

#' Age Group Ratio Consistency Test Across Groups (Count-Based)
#'
#' This function evaluates whether the ratio of individuals in two age groups
#' differs significantly across groups (e.g., enumerators, teams, clusters).
#' It is designed for household-level survey data where each record may contain
#' multiple individuals. Instead of binary indicators, the function accepts two
#' numeric count columns: the number of individuals in age group 1 and the
#' number of individuals in age group 2.
#'
#' A 2×K contingency table is constructed, where rows represent age groups
#' (age group 1 vs. age group 2) and columns represent groups. A chi-squared
#' test of independence is then performed:
#'
#' \deqn{\text{age-group counts} \;\perp\!\!\!\perp\; \text{group}}
#'
#' The resulting p-value indicates whether age-group ratios vary across groups
#' more than expected by chance. The test statistic is the chi-squared value
#' from the independence test.
#'
#' @param survey_design A srvyr survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param variables A character vector of length 3. The first element must be
#'   the age group 1 count column, the second the age group 2 count column, and
#'   the third the grouping column.
#'
#' @return A list with two numeric elements:
#'   \itemize{
#'     \item \code{statistic} The chi-squared test statistic from the
#'           age-group-count-by-group contingency table.
#'     \item \code{p_value} The p-value from the chi-squared test of
#'           independence.
#'   }
#'
#' @details
#' This test detects inconsistent reporting of household age composition across
#' groups. A low p-value indicates that at least one group reports age-group
#' membership differently from the others, which may suggest enumerator bias,
#' data entry issues, or inconsistent interpretation of age-group definitions.
#'
#' Records with missing values in any of the required columns are excluded.
#' Negative counts trigger a warning and result in \code{NA} outputs. If
#' insufficient valid data remain or if only one age group or one group has
#' non-zero counts, the function returns \code{NA} for both outputs.
#'
#' @export
quality_test_ageratio_count_group <- function(
  survey_design,
  variables
) {
  phrutils::phr_try(
    {
      data <- phr_get_data_from_design(survey_design)

      # Expect exactly 3 variables: agegroup1_count, agegroup2_count, group
      if (!is.character(variables) || length(variables) != 3L) {
        phr_error(
          origin = "quality_test_ageratio_count_group",
          message = "`variables` must be a character vector of length 3 (agegroup1_count, agegroup2_count, group column)"
        )
      }

      age1_col <- variables[1]
      age2_col <- variables[2]
      group_col <- variables[3]

      missing_cols <- setdiff(variables, names(data))
      if (length(missing_cols) > 0) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count_group",
          message = paste0(
            "Columns not found in data: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      age1_counts <- data[[age1_col]]
      age2_counts <- data[[age2_col]]
      groups <- data[[group_col]]

      cc <- complete.cases(age1_counts, age2_counts, groups)
      age1_counts <- age1_counts[cc]
      age2_counts <- age2_counts[cc]
      groups <- groups[cc]

      if (length(age1_counts) < 5) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count_group",
          message = "Insufficient data for age ratio count group test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Must be non-negative numeric counts
      if (
        any(age1_counts < 0, na.rm = TRUE) || any(age2_counts < 0, na.rm = TRUE)
      ) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count_group",
          message = "Negative counts found; cannot test age ratio"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      grp <- as.factor(groups)

      # Aggregate counts by group
      age1_by_group <- tapply(age1_counts, grp, sum)
      age2_by_group <- tapply(age2_counts, grp, sum)

      # Need at least 2 groups and both age groups present
      if (length(age1_by_group) < 2 || length(age2_by_group) < 2) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count_group",
          message = "Insufficient variation across groups for age ratio test"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      if (all(age1_by_group == 0) || all(age2_by_group == 0)) {
        phrutils::phr_warning(
          origin = "quality_test_ageratio_count_group",
          message = "One age group has zero counts across all groups; cannot test age ratio consistency"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      # Build 2×K contingency table
      tab <- rbind(
        age_group1 = age1_by_group,
        age_group2 = age2_by_group
      )

      # Chi-square test of independence
      test_result <- suppressWarnings(chisq.test(tab))

      return(list(
        statistic = as.numeric(test_result$statistic),
        p_value = as.numeric(test_result$p.value)
      ))
    },
    on_error = "warn",
    origin = "quality_test_ageratio_count_group"
  )
}
