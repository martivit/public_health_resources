# ---------------------------
# UTILS QUALITY TESTS TEST SUITE
# ---------------------------

library(testthat)
library(tibble)
library(srvyr)


# 1. CORRELATION TEST ####

test_that("quality_test_correlation calculates correlation correctly", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(2, 4, 6, 8, 10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_correlation(sdesign, c("var1", "var2"))

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_equal(result$statistic, 1.0, tolerance = 0.01) # Perfect correlation
})

test_that("quality_test_correlation handles missing values", {
  df <- tibble::tibble(
    var1 = c(1, 2, NA, 4, 5),
    var2 = c(2, 4, 6, NA, 10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_correlation(sdesign, c("var1", "var2"))

  expect_true(is.list(result))
  # Should still calculate correlation for complete cases
})

test_that("quality_test_correlation errors with wrong number of variables", {
  df <- tibble::tibble(var1 = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_correlation(sdesign, c("var1")),
    regexp = "exactly 2 variables"
  )
})

test_that("quality_test_correlation handles missing variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_correlation(sdesign, c("var1", "nonexistent")),
    regexp = "variables not found" # May or may not warn
  )
})

test_that("quality_test_correlation supports different methods", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(2, 4, 6, 8, 10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  pearson <- quality_test_correlation(
    sdesign,
    c("var1", "var2"),
    method = "pearson"
  )
  spearman <- quality_test_correlation(
    sdesign,
    c("var1", "var2"),
    method = "spearman"
  )

  expect_true(!is.na(pearson$statistic))
  expect_true(!is.na(spearman$statistic))
})

test_that("quality_test_correlation handles insufficient data", {
  df <- tibble::tibble(
    var1 = c(1, NA, NA),
    var2 = c(NA, 2, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_correlation(sdesign, c("var1", "var2")),
    regexp = "Insufficient complete cases" # May warn about insufficient cases
  )
})

test_that("quality_test functions handle edge cases gracefully", {
  # Empty data frame
  df_empty <- tibble::tibble(var1 = numeric(0), var2 = numeric(0))
  sdesign_empty <- srvyr::as_survey_design(df_empty, ids = 1)

  expect_warning(
    result <- quality_test_correlation(sdesign_empty, c("var1", "var2")),
    regexp = "Insufficient complete cases"
  )

  # All NA data
  df_na <- tibble::tibble(var1 = rep(NA_real_, 5), var2 = rep(NA_real_, 5))
  sdesign_na <- srvyr::as_survey_design(df_na, ids = 1)

  expect_warning(
    result <- quality_test_correlation(sdesign_na, c("var1", "var2")),
    regexp = "Insufficient complete cases"
  )
})

# 2. T-TEST####

test_that("quality_test_ttest performs one-sample t-test", {
  df <- tibble::tibble(
    var1 = c(10, 12, 14, 16, 18)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest(sdesign, "var1", mu = 10)

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("quality_test_ttest performs two-sample t-test", {
  df <- tibble::tibble(
    var1 = c(10, 12, 14, 16, 18),
    var2 = c(11, 13, 15, 17, 19)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest(sdesign, c("var1", "var2"))

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(!is.na(result$p_value))
})

test_that("quality_test_ttest handles paired t-test", {
  df <- tibble::tibble(
    before = c(10, 10, 8, 25, 18),
    after = c(11, 13, 15, 17, 19)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest(sdesign, c("before", "after"), paired = TRUE)

  expect_true(is.list(result))
})

test_that("quality_test_ttest errors with invalid number of variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6, var3 = 3:7)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ttest(sdesign, c("var1", "var2", "var3")),
    regexp = "1 or 2 variables"
  )
})

test_that("quality_test_ttest handles missing variable", {
  df <- tibble::tibble(var1 = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ttest(sdesign, "nonexistent"),
    regexp = "Variable not found" # May or may not warn
  )
})

test_that("quality_test_ttest handles insufficient data", {
  df <- tibble::tibble(var1 = c(1, NA))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ttest(sdesign, "var1"),
    regexp = "Insufficient data" # May warn about insufficient data
  )
})

test_that("quality_test_ttest removes NA values", {
  df <- tibble::tibble(
    var1 = c(10, NA, 14, 16, 18)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest(sdesign, "var1", mu = 10)

  expect_true(is.list(result))
  # Should calculate with non-NA values
})


# 3. CHI-SQUARED TEST ####

test_that("quality_test_chisq performs chi-squared test", {
  df <- tibble::tibble(
    gender = rep(c("M", "F"), each = 25),
    outcome = sample(c("yes", "no"), 50, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_chisq(sdesign, c("gender", "outcome"))

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("quality_test_chisq errors with wrong number of variables", {
  df <- tibble::tibble(var1 = c("a", "b", "c"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_chisq(sdesign, "var1"),
    regexp = "exactly 2 variables"
  )
})

test_that("quality_test_chisq handles missing variables", {
  df <- tibble::tibble(var1 = c("a", "b"), var2 = c("x", "y"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq(sdesign, c("var1", "nonexistent")),
    regexp = "both variables not found" # May or may not warn
  )
})

test_that("quality_test_chisq handles insufficient data", {
  df <- tibble::tibble(
    var1 = c("a", "b"),
    var2 = c("x", "y")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq(sdesign, c("var1", "var2")),
    regexp = "Insufficient data" # May warn about insufficient data
  )
})

test_that("quality_test_chisq removes missing values", {
  df <- tibble::tibble(
    gender = c("M", "F", NA, "M", "F", "M", "F", "M", "F", "M"),
    outcome = c("yes", "no", "yes", NA, "no", "yes", "no", "yes", "no", "yes")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_chisq(sdesign, c("gender", "outcome"))

  expect_true(is.list(result))
  # Should calculate with complete cases only
})

test_that("quality_test_chisq handles small contingency tables", {
  df <- tibble::tibble(
    var1 = rep("a", 10), # Only one level
    var2 = sample(c("x", "y"), 10, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq(sdesign, c("var1", "var2")),
    regexp = "Contingency table too small" # May warn about table size
  )
})

# 4. FLAG PERCENTAGE TEST ####

# Test Suite for quality_test_flag_percentage

test_that("quality_test_flag_percentage calculates percentage correctly with TRUE flags", {
  df <- tibble::tibble(
    flag = c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_flag_percentage(sdesign, "flag", flag_value = TRUE)

  expect_true(is.list(result))
  expect_equal(result$statistic, 60) # 3 out of 5 = 60%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage works with numeric flags", {
  df <- tibble::tibble(
    status = c(1, 0, 1, 1, 0, 0, 1)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_flag_percentage(sdesign, "status", flag_value = 1)

  expect_true(is.list(result))
  expect_equal(result$statistic, 4 / 7 * 100) # 4 out of 7 ≈ 57.14%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage handles all flags TRUE/FALSE", {
  # All TRUE
  df_all_true <- tibble::tibble(
    flag = c(TRUE, TRUE, TRUE, TRUE)
  )
  sdesign_all_true <- srvyr::as_survey_design(df_all_true, ids = 1)

  result_all_true <- quality_test_flag_percentage(
    sdesign_all_true,
    "flag",
    flag_value = TRUE
  )
  expect_equal(result_all_true$statistic, 100)

  # All FALSE
  df_all_false <- tibble::tibble(
    flag = c(FALSE, FALSE, FALSE, FALSE)
  )
  sdesign_all_false <- srvyr::as_survey_design(df_all_false, ids = 1)

  result_all_false <- quality_test_flag_percentage(
    sdesign_all_false,
    "flag",
    flag_value = TRUE
  )
  expect_equal(result_all_false$statistic, 0)
})

test_that("quality_test_flag_percentage handles NA values correctly", {
  df <- tibble::tibble(
    flag = c(TRUE, NA, FALSE, TRUE, NA, FALSE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_flag_percentage(sdesign, "flag", flag_value = TRUE)

  expect_true(is.list(result))
  # Should ignore NAs: 2 TRUE out of 4 non-NA = 50%
  expect_equal(result$statistic, 50)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage handles edge cases and errors", {
  # Edge case: All NA values (>= 2 PSUs to avoid svydesign error)
  df_all_na <- tibble::tibble(
    flag = c(NA, NA, NA),
    psu = 1:3
  )
  sdesign_all_na <- srvyr::as_survey_design(df_all_na, ids = psu)

  expect_warning(
    result_all_na <- quality_test_flag_percentage(
      sdesign_all_na,
      "flag",
      flag_value = TRUE
    ),
    "No non-missing values found"
  )
  expect_true(is.na(result_all_na$statistic))
  expect_true(is.na(result_all_na$p_value))

  # Edge case: Effectively a single usable value (one TRUE + one NA), with 2 PSUs
  df_single <- tibble::tibble(
    flag = c(TRUE, NA),
    psu = 1:2
  )
  sdesign_single <- srvyr::as_survey_design(df_single, ids = psu)

  result_single <- quality_test_flag_percentage(
    sdesign_single,
    "flag",
    flag_value = TRUE
  )
  expect_equal(result_single$statistic, 100)

  # Error case: Variable not found (>= 2 PSUs)
  df <- tibble::tibble(
    flag = c(TRUE, FALSE),
    psu = 1:2
  )
  sdesign <- srvyr::as_survey_design(df, ids = psu)

  expect_warning(
    result_missing <- quality_test_flag_percentage(
      sdesign,
      "nonexistent",
      flag_value = TRUE
    ),
    "Variable not found in data"
  )
  expect_true(is.na(result_missing$statistic))

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_flag_percentage(
      sdesign,
      c("flag", "other"),
      flag_value = TRUE
    ),
    "Flag percentage test requires exactly 1 variable"
  )

  expect_warning(
    quality_test_flag_percentage(sdesign, character(0), flag_value = TRUE),
    "Flag percentage test requires exactly 1 variable"
  )
})

test_that("quality_test_flag_percentage works with custom flag values", {
  df <- tibble::tibble(
    status = c("yes", "no", "yes", "yes", "no")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_flag_percentage(sdesign, "status", flag_value = "yes")

  expect_true(is.list(result))
  expect_equal(result$statistic, 60) # 3 out of 5 = 60%
  expect_true(is.na(result$p_value))
})

# 5. FLAG PERCENT MISSING ####

# Test Suite for quality_test_missing_percentage

test_that("quality_test_missing_percentage calculates percentage correctly for single variable", {
  df <- tibble::tibble(
    var1 = c(1, 2, NA, 4, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_missing_percentage(sdesign, "var1")

  expect_true(is.list(result))
  expect_equal(result$statistic, 40) # 2 out of 5 = 40%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage works with multiple variables", {
  df <- tibble::tibble(
    var1 = c(1, NA, 3, 4),
    var2 = c(NA, 2, 3, NA),
    var3 = c(1, 2, 3, 4)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_missing_percentage(sdesign, c("var1", "var2", "var3"))

  expect_true(is.list(result))
  # var1: 1 NA, var2: 2 NAs, var3: 0 NAs = 3 NAs out of 12 cells = 25%
  expect_equal(result$statistic, 25)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles all missing or no missing cases", {
  # All missing
  df_all_missing <- tibble::tibble(
    var1 = c(NA, NA, NA),
    var2 = c(NA, NA, NA)
  )
  sdesign_all_missing <- srvyr::as_survey_design(df_all_missing, ids = 1)

  result_all_missing <- quality_test_missing_percentage(
    sdesign_all_missing,
    c("var1", "var2")
  )
  expect_equal(result_all_missing$statistic, 100)

  # No missing
  df_no_missing <- tibble::tibble(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )
  sdesign_no_missing <- srvyr::as_survey_design(df_no_missing, ids = 1)

  result_no_missing <- quality_test_missing_percentage(
    sdesign_no_missing,
    c("var1", "var2")
  )
  expect_equal(result_no_missing$statistic, 0)
})

test_that("quality_test_missing_percentage handles partial variable matches", {
  df <- tibble::tibble(
    var1 = c(1, NA, 3),
    var2 = c(4, 5, 6)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Some variables exist, some don't
  result <- quality_test_missing_percentage(
    sdesign,
    c("var1", "var2", "nonexistent")
  )

  expect_true(is.list(result))
  # Should only count var1 and var2: 1 NA out of 6 cells ≈ 16.67%
  expect_equal(result$statistic, 1 / 6 * 100, tolerance = 0.01)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles different data types", {
  df <- tibble::tibble(
    numeric_var = c(1, NA, 3),
    character_var = c("a", NA, "c"),
    logical_var = c(TRUE, FALSE, NA),
    date_var = as.Date(c("2023-01-01", NA, "2023-01-03")),
    psu = 1:3
  )

  sdesign <- srvyr::as_survey_design(df, ids = psu)

  result <- quality_test_missing_percentage(
    sdesign,
    c("numeric_var", "character_var", "logical_var", "date_var")
  )

  expect_true(is.list(result))
  # 4 NAs out of 12 cells = 33.3%
  expect_equal(result$statistic, 100 / 3)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles edge cases and errors", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3),
    psu = 1:3
  )
  sdesign <- srvyr::as_survey_design(df, ids = psu)

  # Edge case: No variables specified
  expect_warning(
    quality_test_missing_percentage(sdesign, character(0)),
    "Missing percentage test requires at least 1 variable"
  )

  # Edge case: None of the specified variables exist
  expect_warning(
    result_no_vars <- quality_test_missing_percentage(
      sdesign,
      c("nonexistent1", "nonexistent2")
    ),
    "No specified variables found in data"
  )
  expect_true(is.na(result_no_vars$statistic))
  expect_true(is.na(result_no_vars$p_value))

  # Edge case: Single value, no missing
  # Use 2 PSUs while keeping the intent "0% missing"
  df_single <- tibble::tibble(
    var1 = c(1, 1),
    psu = 1:2
  )
  sdesign_single <- srvyr::as_survey_design(df_single, ids = psu)

  result_single <- quality_test_missing_percentage(sdesign_single, "var1")
  expect_equal(result_single$statistic, 0)

  # Edge case: Single value, is missing
  # Use 2 PSUs while keeping the intent "100% missing"
  df_single_na <- tibble::tibble(
    var1 = c(NA, NA),
    psu = 1:2
  )
  sdesign_single_na <- srvyr::as_survey_design(df_single_na, ids = psu)

  result_single_na <- quality_test_missing_percentage(sdesign_single_na, "var1")
  expect_equal(result_single_na$statistic, 100)
})

test_that("quality_test_missing_percentage handles empty data frame edge case", {
  # Empty data frame (0 rows)
  df_empty <- tibble::tibble(
    var1 = numeric(0),
    var2 = character(0)
  )
  sdesign_empty <- srvyr::as_survey_design(df_empty, ids = 1)

  result <- quality_test_missing_percentage(sdesign_empty, c("var1", "var2"))

  # 0 total cells should return NA
  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage calculates correctly with varying missingness patterns", {
  df <- tibble::tibble(
    complete = c(1, 2, 3, 4, 5), # 0 NAs
    half_missing = c(1, NA, 3, NA, 5), # 2 NAs
    mostly_missing = c(NA, NA, NA, NA, 5) # 4 NAs
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_missing_percentage(
    sdesign,
    c("complete", "half_missing", "mostly_missing")
  )

  expect_true(is.list(result))
  # 0 + 2 + 4 = 6 NAs out of 15 cells = 40%
  expect_equal(result$statistic, 40)
  expect_true(is.na(result$p_value))
})

# 6. OUTLIER DETECTION TESTS with Z-SCORES ####

# Test Suite for quality_test_outlier_percentage

test_that("quality_test_outlier_percentage calculates percentage correctly with outliers", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100) # 100 is an outlier
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_true(result$statistic > 0) # Should detect at least one outlier
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage works with no outliers", {
  df <- tibble::tibble(
    values = c(10, 11, 12, 13, 14, 15) # No extreme outliers
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = 3)

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # No outliers expected
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage works with different z_threshold values", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 50)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Stricter threshold (z = 2)
  result_strict <- quality_test_outlier_percentage(
    sdesign,
    "values",
    z_threshold = 2
  )

  # Looser threshold (z = 4)
  result_loose <- quality_test_outlier_percentage(
    sdesign,
    "values",
    z_threshold = 4
  )

  # Stricter threshold should find more outliers
  expect_true(result_strict$statistic >= result_loose$statistic)
})

test_that("quality_test_outlier_percentage handles NA values correctly", {
  df <- tibble::tibble(
    values = c(1, 2, NA, 3, 4, NA, 5, 100)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = 3)

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic)) # Should calculate despite NAs
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles multiple outliers", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100, 200) # Two outliers
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = .7)

  expect_true(is.list(result))
  expect_true(result$statistic > 0)
  # With 2 outliers out of 7 values, should be ~28.57%
  expect_true(result$statistic > 28)
})

test_that("quality_test_outlier_percentage handles zero variance data", {
  # All identical values
  df <- tibble::tibble(
    values = c(5, 5, 5, 5, 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressWarnings(
    result <- quality_test_outlier_percentage(
      sdesign,
      "values",
      z_threshold = 3
    )
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # No outliers when all values identical
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Should work with numeric
  result_numeric <- quality_test_outlier_percentage(
    sdesign,
    "numeric_var",
    z_threshold = 3
  )
  expect_true(is.list(result_numeric))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_outlier_percentage(
      sdesign,
      "character_var",
      z_threshold = 3
    ),
    "numeric data"
  )

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_outlier_percentage(
      sdesign,
      "logical_var",
      z_threshold = 3
    ),
    "numeric data"
  )
})

test_that("quality_test_outlier_percentage handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Error case: Not a data frame
  expect_warning(
    quality_test_outlier_percentage(list(values = c(1, 2, 3)), "values"),
    "survey design"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_outlier_percentage(sdesign, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_outlier_percentage(sdesign, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_outlier_percentage(sdesign, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_outlier_percentage(sdesign, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_outlier_percentage(sdesign, "nonexistent"),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: Insufficient data (< 3 values)
  df_small <- tibble::tibble(
    values = c(1, 2)
  )
  sdesign_small <- srvyr::as_survey_design(df_small, ids = 1)

  expect_warning(
    result_small <- quality_test_outlier_percentage(sdesign_small, "values"),
    "Insufficient data"
  )
  expect_true(is.na(result_small$statistic))

  # Edge case: All NA values
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA)
  )
  sdesign_all_na <- srvyr::as_survey_design(df_all_na, ids = 1)

  expect_warning(
    result_all_na <- quality_test_outlier_percentage(sdesign_all_na, "values"),
    "requires numeric data"
  )
  expect_true(is.na(result_all_na$statistic))
})

test_that("quality_test_outlier_percentage validates z_threshold parameter", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Error case: Non-numeric z_threshold
  expect_warning(
    quality_test_outlier_percentage(sdesign, "values", z_threshold = "3"),
    "single numeric value"
  )

  # Error case: Negative z_threshold
  expect_warning(
    quality_test_outlier_percentage(sdesign, "values", z_threshold = -2),
    "positive number"
  )

  # Error case: Zero z_threshold
  expect_warning(
    quality_test_outlier_percentage(sdesign, "values", z_threshold = 0),
    "positive number"
  )

  # Error case: NA z_threshold
  expect_warning(
    quality_test_outlier_percentage(sdesign, "values", z_threshold = NA),
    "single numeric value"
  )

  # Error case: Multiple z_threshold values
  expect_warning(
    quality_test_outlier_percentage(sdesign, "values", z_threshold = c(2, 3)),
    "single numeric value"
  )
})

test_that("quality_test_outlier_percentage calculates exact percentages correctly", {
  # Create data with known outliers
  df <- tibble::tibble(
    values = c(rep(10, 9), 1000) # 1 outlier out of 10 = 10%
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_equal(result$statistic, 10, tolerance = 0.1)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles negative outliers", {
  df <- tibble::tibble(
    values = c(100, 101, 102, 103, 104, 1) # 1 is a negative outlier
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(sdesign, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_true(result$statistic > 0) # Should detect the negative outlier
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles extreme outliers on both ends", {
  df <- tibble::tibble(
    values = c(-100, 10, 11, 12, 13, 14, 15, 200) # Outliers on both ends
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_outlier_percentage(
    sdesign,
    "values",
    z_threshold = 1.45
  )

  expect_true(is.list(result))
  # Should detect both outliers: 2 out of 8 = 25%
  expect_true(result$statistic >= 20)
  expect_true(is.na(result$p_value))
})

# 7. COEFFICIENT VARIATION TEST ####

# Test Suite for quality_test_coefficient_variation

test_that("quality_test_coefficient_variation calculates CV correctly with normal data", {
  df <- tibble::tibble(
    values = c(10, 12, 14, 16, 18, 20)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0) # Should have some variation
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation calculates exact CV correctly", {
  # Create data with known mean and sd
  # values: 10, 20, 30 -> mean = 20, sd = 10, CV = (10/20)*100 = 50%
  df <- tibble::tibble(
    values = c(10, 20, 30)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_equal(result$statistic, 50, tolerance = 0.1)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles low variation data", {
  # Values very close together should have low CV
  df <- tibble::tibble(
    values = c(100, 100.1, 100.2, 99.9, 99.8)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(result$statistic < 1) # Very low CV
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles high variation data", {
  # Values spread far apart should have high CV
  df <- tibble::tibble(
    values = c(1, 50, 100, 150, 200)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(result$statistic > 50) # High CV
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles NA values correctly", {
  df <- tibble::tibble(
    values = c(10, NA, 20, NA, 30, 40)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic)) # Should calculate despite NAs
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles zero variance data", {
  # All identical values
  df <- tibble::tibble(
    values = c(5, 5, 5, 5, 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_coefficient_variation(sdesign, "values"),
    "zero variance"
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # CV = 0 when no variation
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles zero mean data", {
  # Values that average to zero
  df <- tibble::tibble(
    values = c(-10, -5, 0, 5, 10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_coefficient_variation(sdesign, "values"),
    "Mean is zero"
  )

  expect_true(is.list(result))
  expect_true(is.na(result$statistic)) # CV undefined when mean = 0
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles negative values correctly", {
  # CV should use absolute value of mean
  df <- tibble::tibble(
    values = c(-20, -18, -16, -14, -12, -10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Should work with numeric
  result_numeric <- quality_test_coefficient_variation(sdesign, "numeric_var")
  expect_true(is.list(result_numeric))
  expect_true(!is.na(result_numeric$statistic))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_coefficient_variation(sdesign, "character_var"),
    "numeric data"
  )
  expect_true(is.na(result_char$statistic))

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_coefficient_variation(
      sdesign,
      "logical_var"
    ),
    "numeric data"
  )
  expect_true(is.na(result_logical$statistic))
})

test_that("quality_test_coefficient_variation handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5),
    psu = 1:5
  )
  sdesign <- srvyr::as_survey_design(df, ids = psu)

  # Error case: Not a data frame
  expect_warning(
    quality_test_coefficient_variation(list(values = c(1, 2, 3)), "values"),
    "survey design"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_coefficient_variation(sdesign, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_coefficient_variation(sdesign, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_coefficient_variation(sdesign, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_coefficient_variation(sdesign, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_coefficient_variation(
      sdesign,
      "nonexistent"
    ),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: Insufficient data (< 2 values)
  # Use 2 PSUs while still having only 1 observation
  df_small <- tibble::tibble(
    values = c(1, NA),
    psu = 1:2
  )
  sdesign_small <- srvyr::as_survey_design(df_small, ids = psu)

  expect_warning(
    result_small <- quality_test_coefficient_variation(sdesign_small, "values"),
    "Insufficient data"
  )
  expect_true(is.na(result_small$statistic))

  # Edge case: All NA values (>= 2 PSUs)
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA),
    psu = 1:4
  )
  sdesign_all_na <- srvyr::as_survey_design(df_all_na, ids = psu)

  expect_warning(
    result_all_na <- quality_test_coefficient_variation(
      sdesign_all_na,
      "values"
    ),
    "strictly numeric value"
  )
  expect_true(is.na(result_all_na$statistic))
})

test_that("quality_test_coefficient_variation handles minimum data case", {
  # Exactly 2 values (minimum for CV calculation)
  df <- tibble::tibble(
    values = c(10, 20)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  # For values 10, 20: mean = 15, sd = 7.07, CV ≈ 47.14%
  expect_equal(
    result$statistic,
    (sd(c(10, 20)) / mean(c(10, 20))) * 100,
    tolerance = 0.01
  )
})

test_that("quality_test_coefficient_variation handles empty data frame edge case", {
  # Empty data frame (0 rows)
  df_empty <- tibble::tibble(
    values = numeric(0)
  )
  sdesign_empty <- srvyr::as_survey_design(df_empty, ids = 1)

  expect_warning(
    result <- quality_test_coefficient_variation(sdesign_empty, "values"),
    "Insufficient data"
  )

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation compares relative variation correctly", {
  # Two datasets with same SD but different means
  # Higher mean should have lower CV
  df1 <- tibble::tibble(
    low_mean = c(8, 10, 12) # mean = 10, sd ≈ 2
  )
  sdesign1 <- srvyr::as_survey_design(df1, ids = 1)

  df2 <- tibble::tibble(
    high_mean = c(98, 100, 102) # mean = 100, sd ≈ 2
  )
  sdesign2 <- srvyr::as_survey_design(df2, ids = 1)

  result1 <- quality_test_coefficient_variation(sdesign1, "low_mean")
  result2 <- quality_test_coefficient_variation(sdesign2, "high_mean")

  # CV should be higher for lower mean (same SD, smaller mean = higher CV)
  expect_true(result1$statistic > result2$statistic)
})

test_that("quality_test_coefficient_variation handles mixed positive and negative values", {
  # Mixed values with non-zero mean
  df <- tibble::tibble(
    values = c(-5, -2, 1, 4, 7, 10) # mean = 2.5
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles very small positive values", {
  # Very small positive values near zero
  df <- tibble::tibble(
    values = c(0.001, 0.002, 0.003, 0.004, 0.005)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_coefficient_variation(sdesign, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(is.finite(result$statistic))
})

# 8. RANGE VIOLATION TESTS ####

# Test Suite for quality_test_range_violation

test_that("quality_test_range_violation calculates percentage correctly with violations", {
  df <- tibble::tibble(
    values = c(1, 5, 10, 15, 20, 100) # 100 is outside range [0, 50]
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 50
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 1 / 6 * 100, tolerance = 0.01) # 1 out of 6 ≈ 16.67%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with no violations", {
  df <- tibble::tibble(
    values = c(10, 20, 30, 40, 50)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 100
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # All values within range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with all violations", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 10,
    max_value = 20
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 100) # All values outside range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles lower bound violations", {
  df <- tibble::tibble(
    values = c(-5, -2, 0, 5, 10, 15)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 100
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 6 * 100, tolerance = 0.01) # 2 values below 0
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles upper bound violations", {
  df <- tibble::tibble(
    values = c(5, 10, 15, 20, 105, 110)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 100
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 6 * 100, tolerance = 0.01) # 2 values above 100
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles both bound violations", {
  df <- tibble::tibble(
    values = c(-10, 5, 10, 15, 20, 110)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 100
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 6 * 100, tolerance = 0.01) # 1 below, 1 above
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles NA values correctly", {
  df <- tibble::tibble(
    values = c(5, NA, 10, NA, 15, 105)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 100
  )

  expect_true(is.list(result))
  # Should ignore NAs: 1 violation out of 4 non-NA = 25%
  expect_equal(result$statistic, 25)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with default infinite bounds", {
  df <- tibble::tibble(
    values = c(-1000, -100, 0, 100, 1000)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Default min_value = -Inf, max_value = Inf
  result <- quality_test_range_violation(sdesign, "values")

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # All finite values are within infinite range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with only min_value specified", {
  df <- tibble::tibble(
    values = c(-10, 0, 5, 10, 15)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(sdesign, "values", min_value = 0)

  expect_true(is.list(result))
  expect_equal(result$statistic, 1 / 5 * 100) # Only -10 violates
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with only max_value specified", {
  df <- tibble::tibble(
    values = c(5, 10, 15, 20, 25)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(sdesign, "values", max_value = 15)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 5 * 100) # 20 and 25 violate
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles infinite values in data", {
  df <- tibble::tibble(
    values = c(5, 10, Inf, 15, 20)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_range_violation(
      sdesign,
      "values",
      min_value = 0,
      max_value = 100
    ),
    "infinite values"
  )

  expect_true(is.list(result))
  # Should exclude Inf: 0 violations out of 4 finite values
  expect_equal(result$statistic, 0)
})

test_that("quality_test_range_violation handles negative infinite values in data", {
  df <- tibble::tibble(
    values = c(-Inf, 5, 10, 15, 20)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_range_violation(
      sdesign,
      "values",
      min_value = 0,
      max_value = 100
    ),
    "infinite values"
  )

  expect_true(is.list(result))
  # Should exclude -Inf: 0 violations out of 4 finite values
  expect_equal(result$statistic, 0)
})

test_that("quality_test_range_violation validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Should work with numeric
  result_numeric <- quality_test_range_violation(
    sdesign,
    "numeric_var",
    min_value = 0,
    max_value = 10
  )
  expect_true(is.list(result_numeric))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_range_violation(
      sdesign,
      "character_var",
      min_value = 0,
      max_value = 10
    ),
    "numeric data"
  )
  expect_true(is.na(result_char$statistic))

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_range_violation(
      sdesign,
      "logical_var",
      min_value = 0,
      max_value = 10
    ),
    "numeric data"
  )
  expect_true(is.na(result_logical$statistic))
})

test_that("quality_test_range_violation handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5),
    psu = 1:5
  )
  sdesign <- srvyr::as_survey_design(df, ids = psu)

  # Error case: Not a data frame
  expect_warning(
    quality_test_range_violation(list(values = c(1, 2, 3)), "values"),
    "survey design"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_range_violation(sdesign, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_range_violation(sdesign, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_range_violation(sdesign, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_range_violation(sdesign, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_range_violation(sdesign, "nonexistent"),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: All NA values (>= 2 PSUs)
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA),
    psu = 1:4
  )
  sdesign_all_na <- srvyr::as_survey_design(df_all_na, ids = psu)

  expect_warning(
    result_all_na <- quality_test_range_violation(sdesign_all_na, "values"),
    "requires numeric data"
  )
  expect_true(is.na(result_all_na$statistic))

  # Edge case: Single value within range
  # Use 2 PSUs while keeping the intent "all (finite) values are within range"
  df_single <- tibble::tibble(
    values = c(5, 5),
    psu = 1:2
  )
  sdesign_single <- srvyr::as_survey_design(df_single, ids = psu)

  result_single <- quality_test_range_violation(
    sdesign_single,
    "values",
    min_value = 0,
    max_value = 10
  )
  expect_equal(result_single$statistic, 0)

  # Edge case: Single value outside range
  result_single_out <- quality_test_range_violation(
    sdesign_single,
    "values",
    min_value = 10,
    max_value = 20
  )
  expect_equal(result_single_out$statistic, 100)
})

test_that("quality_test_range_violation validates range parameters", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Error case: Non-numeric min_value
  expect_warning(
    quality_test_range_violation(sdesign, "values", min_value = "0"),
    "single numeric value"
  )

  # Error case: Non-numeric max_value
  expect_warning(
    quality_test_range_violation(sdesign, "values", max_value = "10"),
    "single numeric value"
  )

  # Error case: NA min_value
  expect_warning(
    quality_test_range_violation(sdesign, "values", min_value = NA),
    "single numeric value"
  )

  # Error case: NA max_value
  expect_warning(
    quality_test_range_violation(sdesign, "values", max_value = NA),
    "single numeric value"
  )

  # Error case: min_value > max_value
  expect_warning(
    quality_test_range_violation(
      sdesign,
      "values",
      min_value = 10,
      max_value = 5
    ),
    "less than or equal to"
  )

  # Error case: Multiple min_value values
  expect_warning(
    quality_test_range_violation(sdesign, "values", min_value = c(0, 5)),
    "single numeric value"
  )

  # Error case: Multiple max_value values
  expect_warning(
    quality_test_range_violation(sdesign, "values", max_value = c(10, 20)),
    "single numeric value"
  )
})

test_that("quality_test_range_violation handles boundary values correctly", {
  df <- tibble::tibble(
    values = c(0, 5, 10, 15, 20)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Values exactly at boundaries should NOT violate
  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 20
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 0) # Boundary values are included in valid range
})

test_that("quality_test_range_violation handles negative ranges", {
  df <- tibble::tibble(
    values = c(-20, -15, -10, -5, 0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = -15,
    max_value = -5
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 5 * 100) # -20 and 0 violate
})

test_that("quality_test_range_violation handles empty data frame edge case", {
  df_empty <- tibble::tibble(
    values = numeric(0)
  )
  sdesign_empty <- srvyr::as_survey_design(df_empty, ids = 1)

  expect_warning(
    result <- quality_test_range_violation(
      sdesign_empty,
      "values",
      min_value = 0,
      max_value = 10
    ),
    "No non-missing values"
  )

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles data with only infinite values", {
  df <- tibble::tibble(
    values = c(Inf, -Inf, Inf)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressWarnings(expect_warning(
    result <- quality_test_range_violation(
      sdesign,
      "values",
      min_value = 0,
      max_value = 100
    ),
    "No finite values found"
  ))

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation calculates exact percentages", {
  # Create data with known violations
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100) # 1 violation out of 10 = 10%
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 0,
    max_value = 50
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 10)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with very tight ranges", {
  df <- tibble::tibble(
    values = c(9.9, 10.0, 10.1, 10.2, 10.3)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_range_violation(
    sdesign,
    "values",
    min_value = 10.0,
    max_value = 10.2
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 2 / 5 * 100) # 9.9 and 10.3 violate
})

# 7. INTEGRATION TESTS ####

test_that("quality tests work together in a pipeline", {
  df <- tibble::tibble(
    age = c(25, 30, 35, 40, 45, 50, 55, 60),
    income = c(30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000),
    gender = rep(c("M", "F"), 4),
    employed = sample(c("yes", "no"), 8, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Correlation test
  cor_result <- quality_test_correlation(sdesign, c("age", "income"))
  expect_true(!is.na(cor_result$statistic))

  # T-test
  t_result <- quality_test_ttest(sdesign, "age", mu = 40)
  expect_true(!is.na(t_result$statistic))

  # Chi-squared test
  chisq_result <- quality_test_chisq(sdesign, c("gender", "employed"))
  expect_true(is.list(chisq_result))
})

test_that("quality tests return consistent structure", {
  df <- tibble::tibble(
    var1 = 1:10,
    var2 = 2:11,
    cat1 = rep(c("a", "b"), 5),
    cat2 = rep(c("x", "y"), 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressWarnings({
    cor_result <- quality_test_correlation(sdesign, c("var1", "var2"))
    t_result <- quality_test_ttest(sdesign, "var1")
    chisq_result <- quality_test_chisq(sdesign, c("cat1", "cat2"))
  })

  # All should return lists with statistic and p_value
  expect_true(all(c("statistic", "p_value") %in% names(cor_result)))
  expect_true(all(c("statistic", "p_value") %in% names(t_result)))
  expect_true(all(c("statistic", "p_value") %in% names(chisq_result)))
})


# 6. NUMERIC CONVERSION TESTS ####

test_that("quality tests handle character numeric input", {
  df <- tibble::tibble(
    var1 = c("1", "2", "3", "4", "5"),
    var2 = c("2", "4", "6", "8", "10")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_correlation(sdesign, c("var1", "var2"))

  expect_true(is.list(result))
  # Should convert to numeric and calculate correlation
})

test_that("quality tests handle factor input", {
  df <- tibble::tibble(
    var1 = factor(c("a", "b", "a", "b", "a")),
    var2 = factor(c("x", "y", "x", "y", "x"))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressWarnings(result <- quality_test_chisq(sdesign, c("var1", "var2")))

  expect_true(is.list(result))
})


# 8. ANOVA TEST ####

test_that("quality_test_anova returns F-statistic and p-value for a clear group difference", {
  df <- tibble::tibble(
    score = c(10, 11, 12, 20, 21, 22, 30, 31, 32),
    group = c("A", "A", "A", "B", "B", "B", "C", "C", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_anova(sdesign, c("score", "group"))

  expect_true(is.list(result))
  expect_true(all(c("statistic", "p_value") %in% names(result)))
  expect_true(!is.na(result$statistic))
  expect_true(!is.na(result$p_value))
  expect_true(result$statistic > 1)
  expect_true(result$p_value < 0.05)
})

test_that("quality_test_anova handles missing values", {
  df <- tibble::tibble(
    score = c(10, NA, 12, 20, 21, NA, 30, 31, 32),
    group = c("A", "A", "A", "B", "B", "B", "C", "C", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_anova(sdesign, c("score", "group"))

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
})

test_that("quality_test_anova warns when a column is not found", {
  df <- tibble::tibble(score = 1:5, group = c("A", "A", "B", "B", "B"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova(sdesign, c("nonexistent", "group")),
    regexp = "not found"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_anova warns when group column not found", {
  df <- tibble::tibble(score = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova(sdesign, c("score", "nonexistent")),
    regexp = "not found"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_anova warns with fewer than 2 group levels", {
  df <- tibble::tibble(
    score = c(10, 11, 12, 13, 14),
    group = rep("A", 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova(sdesign, c("score", "group")),
    regexp = "at least 2 distinct levels"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_anova warns with non-character variables", {
  df <- tibble::tibble(score = 1:5, group = c("A", "A", "B", "B", "B"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_anova(sdesign, 1:2),
    regexp = "exactly 2 column names"
  )
})

test_that("quality_test_anova warns when variables has wrong length", {
  df <- tibble::tibble(score = 1:5, group = c("A", "A", "B", "B", "B"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_anova(sdesign, "score"),
    regexp = "exactly 2 column names"
  )
})


# 9. CHI-SQUARED BINARY TEST ####

test_that("quality_test_chisq_binary returns statistic and p-value from raw data", {
  df <- tibble::tibble(
    treatment = rep(c("A", "B"), each = 50),
    outcome = c(
      rep(c("yes", "no"), times = c(35, 15)),
      rep(c("yes", "no"), times = c(20, 30))
    )
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_chisq_binary(sdesign, c("treatment", "outcome"))

  expect_true(is.list(result))
  expect_true(all(c("statistic", "p_value") %in% names(result)))
  expect_true(!is.na(result$statistic))
  expect_true(!is.na(result$p_value))
})

test_that("quality_test_chisq_binary warns when a column is missing", {
  df <- tibble::tibble(cat1 = c("A", "B", "A", "B", "A"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq_binary(sdesign, c("cat1", "nonexistent")),
    regexp = "not found"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_chisq_binary warns with insufficient data", {
  df <- tibble::tibble(
    cat1 = c("A", "B", "A"),
    cat2 = c("X", NA, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq_binary(sdesign, c("cat1", "cat2")),
    regexp = "Insufficient"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_chisq_binary warns when variable has only one level", {
  df <- tibble::tibble(
    cat1 = rep("A", 10),
    cat2 = rep(c("X", "Y"), 5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_chisq_binary(sdesign, c("cat1", "cat2")),
    regexp = "too small"
  )
  expect_equal(result$statistic, NA_real_)
})

test_that("quality_test_chisq_binary warns with non-character variables", {
  df <- tibble::tibble(c1 = c("A", "B"), c2 = c("X", "Y"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_chisq_binary(sdesign, 1:2),
    regexp = "exactly 2 column names"
  )
})


# 10. BINOMIAL RATIO ROWWISE TEST ####

test_that("quality_test_binomial_ratio_rowwise adds p-value column", {
  df <- tibble::tibble(
    successes = c(5L, 8L, 3L),
    trials = c(10L, 10L, 10L)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_binomial_ratio_rowwise(
    sdesign,
    c("successes", "trials")
  )

  expect_true(is.data.frame(result))
  expect_true("p_value" %in% names(result))
  expect_equal(nrow(result), 3L)
  expect_true(all(!is.na(result$p_value)))
})

test_that("quality_test_binomial_ratio_rowwise uses custom pval_colname", {
  df <- tibble::tibble(s = c(5L, 8L), n = c(10L, 10L))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_binomial_ratio_rowwise(
    sdesign,
    c("s", "n"),
    pval_colname = "binom_p"
  )

  expect_true("binom_p" %in% names(result))
})

test_that("quality_test_binomial_ratio_rowwise returns NA for invalid rows", {
  df <- tibble::tibble(
    s = c(5L, NA, -1L, 3L),
    n = c(10L, 10L, 10L, 0L)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_binomial_ratio_rowwise(sdesign, c("s", "n"))

  expect_true(is.data.frame(result))
  expect_false(is.na(result$p_value[1]))
  expect_true(is.na(result$p_value[2]))
  expect_true(is.na(result$p_value[3]))
  expect_true(is.na(result$p_value[4]))
})

test_that("quality_test_binomial_ratio_rowwise supports alternative hypotheses", {
  df <- tibble::tibble(s = c(8L, 2L), n = c(10L, 10L))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  r_greater <- quality_test_binomial_ratio_rowwise(
    sdesign,
    c("s", "n"),
    alternative = "greater"
  )
  r_less <- quality_test_binomial_ratio_rowwise(
    sdesign,
    c("s", "n"),
    alternative = "less"
  )

  expect_true(all(!is.na(r_greater$p_value)))
  expect_true(all(!is.na(r_less$p_value)))
})

test_that("quality_test_binomial_ratio_rowwise warns when column is missing", {
  df <- tibble::tibble(s = c(5L, 8L))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_binomial_ratio_rowwise(sdesign, c("s", "nonexistent")),
    regexp = "not found"
  )
})

test_that("quality_test_binomial_ratio_rowwise warns with invalid expected_ratio", {
  # Use >= 2 PSUs to avoid svydesign error
  df <- tibble::tibble(
    s = c(5L, 5L),
    n = c(10L, 10L),
    psu = 1:2
  )
  sdesign <- srvyr::as_survey_design(df, ids = psu)

  expect_warning(
    quality_test_binomial_ratio_rowwise(
      sdesign,
      c("s", "n"),
      expected_ratio = 1.5
    ),
    regexp = "strictly between 0 and 1"
  )
})


# 11. T-TEST SUMMARY ROWWISE TEST ####

test_that("quality_test_ttest_summary_rowwise adds p-value column using actual data", {
  df <- tibble::tibble(
    w1 = c(5.0, 10.0, 0.5),
    w2 = c(6.0, 11.0, 1.5),
    w3 = c(5.5, 10.5, 1.0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest_summary_rowwise(
    sdesign,
    c("w1", "w2", "w3"),
    expected_mean = 0
  )

  expect_true(is.data.frame(result))
  expect_true("p_value" %in% names(result))
  expect_equal(nrow(result), 3L)
  expect_true(all(!is.na(result$p_value)))
})

test_that("quality_test_ttest_summary_rowwise uses custom pval_colname", {
  df <- tibble::tibble(a = c(1.0, 2.0), b = c(3.0, 4.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest_summary_rowwise(
    sdesign,
    c("a", "b"),
    pval_colname = "t_p"
  )

  expect_true("t_p" %in% names(result))
})

test_that("quality_test_ttest_summary_rowwise returns NA when all row values are missing", {
  df <- tibble::tibble(
    a = c(5.0, NA, 1.0),
    b = c(6.0, NA, 2.0),
    c = c(5.5, NA, 1.5)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ttest_summary_rowwise(sdesign, c("a", "b", "c"))

  expect_false(is.na(result$p_value[1]))
  expect_true(is.na(result$p_value[2]))
  expect_false(is.na(result$p_value[3]))
})

test_that("quality_test_ttest_summary_rowwise supports alternative hypotheses", {
  df <- tibble::tibble(
    a = c(50.0, -50.0),
    b = c(52.0, -48.0),
    c3 = c(51.0, -49.0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  r_greater <- quality_test_ttest_summary_rowwise(
    sdesign,
    c("a", "b", "c3"),
    alternative = "greater"
  )
  r_less <- quality_test_ttest_summary_rowwise(
    sdesign,
    c("a", "b", "c3"),
    alternative = "less"
  )

  expect_true(all(!is.na(r_greater$p_value)))
  expect_true(all(!is.na(r_less$p_value)))
})

test_that("quality_test_ttest_summary_rowwise warns when column is missing", {
  df <- tibble::tibble(a = c(1.0, 2.0), b = c(3.0, 4.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ttest_summary_rowwise(sdesign, c("a", "b", "nonexistent")),
    regexp = "not found"
  )
})

test_that("quality_test_ttest_summary_rowwise warns with invalid alternative", {
  df <- tibble::tibble(a = c(1.0, 2.0), b = c(3.0, 4.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ttest_summary_rowwise(
      sdesign,
      c("a", "b"),
      alternative = "both"
    ),
    regexp = "two.sided.*greater.*less"
  )
})

test_that("quality_test_ttest_summary_rowwise warns with fewer than 2 variables", {
  df <- tibble::tibble(a = c(1.0, 2.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ttest_summary_rowwise(sdesign, "a"),
    regexp = "at least 2 column names"
  )
})

# 13. DIGIT PREFERENCE SCORE TEST ####

test_that("digit_preference_score returns a single numeric value", {
  set.seed(1)
  x <- round(rnorm(100, mean = 12.5, sd = 1.5), 1)
  result <- phr:::digit_preference_score(x)
  expect_true(is.numeric(result))
  expect_length(result, 1L)
  expect_false(is.na(result))
  expect_true(result >= 0)
})

test_that("digit_preference_score detects strong digit preference (all 0s)", {
  # All values end in 0 -> perfect preference for digit 0 -> high DPS
  x <- seq(100, 200, by = 10)
  result <- phr:::digit_preference_score(x)
  expect_true(
    result > 20,
    info = "Strong preference for 0 should yield DPS > 20 (Problematic)"
  )
})

test_that("digit_preference_score is near 0 for perfectly uniform digits", {
  # Each digit 0-9 appears exactly once -> chi-squared statistic is 0
  x <- 0:9
  suppressWarnings(result <- phr:::digit_preference_score(x, digits = 0))
  expect_equal(result, 0)
})

test_that("quality_test_digit_preference returns list with statistic and p_value", {
  set.seed(7)
  df <- tibble::tibble(muac = round(rnorm(50, 12.5, 1.5), 1))
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  result <- quality_test_digit_preference(sdesign, "muac")
  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_digit_preference warns for insufficient data", {
  df <- tibble::tibble(muac = c(12.5, 11.0, 13.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    result <- quality_test_digit_preference(sdesign, "muac"),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_digit_preference warns for missing variable", {
  df <- tibble::tibble(muac = c(12.5, 11.0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    result <- quality_test_digit_preference(sdesign, "nonexistent"),
    regexp = "Variable not found"
  )
  expect_true(is.na(result$statistic))
})


# 14. POISSON RATIO TEST ####

test_that("quality_test_poisson_ratio performs test correctly", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6, 4),
    exposure = c(100, 150, 80, 120, 90)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_poisson_ratio(sdesign, c("events", "exposure"))

  expect_true(is.list(result))
  expect_true("test_statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$test_statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_poisson_ratio errors with wrong number of variables", {
  df <- tibble::tibble(events = c(5, 8, 3))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_poisson_ratio(sdesign, c("events")),
    regexp = "exactly 2 column names"
  )
})

test_that("quality_test_poisson_ratio handles missing columns", {
  df <- tibble::tibble(events = c(5, 8, 3))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_poisson_ratio(sdesign, c("events", "nonexistent")),
    regexp = "not found"
  )
})

test_that("quality_test_poisson_ratio supports alternative hypotheses", {
  df <- tibble::tibble(
    events = c(5, 8, 3),
    exposure = c(100, 150, 80)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  r_two <- quality_test_poisson_ratio(
    sdesign,
    c("events", "exposure"),
    alternative = "two.sided"
  )
  r_greater <- quality_test_poisson_ratio(
    sdesign,
    c("events", "exposure"),
    alternative = "greater"
  )
  r_less <- quality_test_poisson_ratio(
    sdesign,
    c("events", "exposure"),
    alternative = "less"
  )

  expect_true(!is.na(r_two$p_value))
  expect_true(!is.na(r_greater$p_value))
  expect_true(!is.na(r_less$p_value))
})

test_that("quality_test_poisson_ratio handles custom expected_rate", {
  df <- tibble::tibble(
    events = c(5, 8, 3),
    exposure = c(100, 150, 80)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_poisson_ratio(
    sdesign,
    c("events", "exposure"),
    expected_rate = 0.05
  )

  expect_true(is.numeric(result$test_statistic))
  expect_true(is.numeric(result$p_value))
})


# 15. COUNT TEST ####

test_that("quality_test_count returns non-missing count", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, NA, 5, NA, 7)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_count(sdesign, "var1")

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_equal(result$statistic, 5) # 5 non-missing values
})

test_that("quality_test_count handles all NA values", {
  df <- tibble::tibble(
    var1 = c(NA, NA, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_count(sdesign, "var1")

  expect_equal(result$statistic, 0)
})

test_that("quality_test_count errors with multiple variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_count(sdesign, c("var1", "var2")),
    regexp = "single character column name"
  )
})

test_that("quality_test_count handles missing column", {
  df <- tibble::tibble(var1 = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_count(sdesign, "nonexistent"),
    regexp = "not found"
  )
})


# 16. INDEX DISPERSION TEST ####

test_that("quality_test_index_dispersion performs test correctly", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6, 4, 7, 2, 9, 5, 6)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_index_dispersion(sdesign, "events")

  expect_true(is.list(result))
  expect_true("test_statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$test_statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_index_dispersion errors with multiple variables", {
  df <- tibble::tibble(events = c(5, 8, 3))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_index_dispersion(sdesign, c("events", "another")),
    regexp = "exactly 1 column name"
  )
})

test_that("quality_test_index_dispersion handles missing column", {
  df <- tibble::tibble(events = c(5, 8, 3))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_index_dispersion(sdesign, "nonexistent"),
    regexp = "not found"
  )
  expect_true(is.na(result$test_statistic))
})

test_that("quality_test_index_dispersion handles negative counts", {
  df <- tibble::tibble(
    events = c(5, -2, 3)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_index_dispersion(sdesign, "events"),
    regexp = "Negative event counts"
  )
  expect_true(is.na(result$test_statistic))
})

test_that("quality_test_index_dispersion handles all NA values", {
  df <- tibble::tibble(
    events = c(NA, NA, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_index_dispersion(sdesign, "events")

  expect_true(is.na(result$test_statistic))
})


# 17. EVENTS MONTHS DISTRIBUTION TEST ####

test_that("quality_test_events_months_distribution performs test correctly", {
  df <- tibble::tibble(
    num_events = c(3, 2, 4, 3, 2),
    event_dates = c(
      "2020-01-15, 2020-02-20, 2020-03-10",
      "2020-01-05, 2020-02-15",
      "2020-01-25, 2020-02-10, 2020-03-05, 2020-04-20",
      "2020-02-15, 2020-03-20, 2020-04-10",
      "2020-01-10, 2020-03-15"
    )
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_events_months_distribution(
    sdesign,
    c("num_events", "event_dates")
  )

  expect_true(is.list(result))
  expect_true("test_statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$test_statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_events_months_distribution errors with wrong number of variables", {
  df <- tibble::tibble(num_events = c(3, 2))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_events_months_distribution(sdesign, c("num_events")),
    regexp = "exactly 2 column names"
  )
})

test_that("quality_test_events_months_distribution handles missing columns", {
  df <- tibble::tibble(num_events = c(3, 2))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_events_months_distribution(
      sdesign,
      c("num_events", "nonexistent")
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$test_statistic))
})

test_that("quality_test_events_months_distribution detects mismatch between events and dates", {
  df <- tibble::tibble(
    num_events = c(3, 2), # Says 3 events, but only 2 dates provided
    event_dates = c("2020-01-15, 2020-02-20", "2020-01-05, 2020-02-15")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_events_months_distribution(
      sdesign,
      c("num_events", "event_dates")
    ),
    regexp = "Mismatch"
  )
  expect_true(is.na(result$test_statistic))
})

# 18. ANOVA BY EXPOSURE TEST ####

test_that("quality_test_anova_by_exposure performs test correctly", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6, 4, 7, 2, 9),
    exposure = c(100, 150, 80, 120, 90, 130, 70, 140),
    group = c("A", "B", "A", "B", "A", "B", "A", "B")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_anova_by_exposure(
    sdesign,
    c("events", "exposure", "group")
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_anova_by_exposure errors with wrong number of variables", {
  df <- tibble::tibble(events = c(5, 8), exposure = c(100, 150))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_anova_by_exposure(sdesign, c("events", "exposure")),
    regexp = "exactly 3 column names"
  )
})

test_that("quality_test_anova_by_exposure handles missing columns", {
  df <- tibble::tibble(events = c(5, 8), exposure = c(100, 150))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova_by_exposure(
      sdesign,
      c("events", "exposure", "nonexistent")
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_anova_by_exposure handles insufficient data", {
  df <- tibble::tibble(
    events = c(5, 8),
    exposure = c(100, 150),
    group = c("A", "B")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova_by_exposure(
      sdesign,
      c("events", "exposure", "group")
    ),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_anova_by_exposure handles non-positive exposure", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6),
    exposure = c(100, 0, 80, -50),
    group = c("A", "B", "A", "B")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova_by_exposure(
      sdesign,
      c("events", "exposure", "group")
    ),
    regexp = "2 records with non-positive exposure were removed"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_anova_by_exposure handles single group level", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6),
    exposure = c(100, 150, 80, 120),
    group = c("A", "A", "A", "A")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_anova_by_exposure(
      sdesign,
      c("events", "exposure", "group")
    ),
    regexp = "at least 2 distinct levels"
  )
  expect_true(is.na(result$statistic))
})


# 19. EVENT GROUP VARIANCE TEST ####

test_that("quality_test_event_group_variance performs test correctly", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6, 4, 7, 2, 9, 5, 6),
    exposure = c(100, 150, 80, 120, 90, 130, 70, 140, 110, 125),
    enum = c("E1", "E2", "E1", "E2", "E1", "E2", "E1", "E2", "E1", "E2"),
    cluster = c("C1", "C1", "C2", "C2", "C3", "C3", "C4", "C4", "C5", "C5")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_event_group_variance(
    sdesign,
    c("events", "exposure", "enum", "cluster")
  )

  expect_true(is.list(result))
  expect_true("test_statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  # test_statistic should be a percentage (0-100)
  expect_true(is.numeric(result$test_statistic))
})

test_that("quality_test_event_group_variance errors with wrong number of variables", {
  df <- tibble::tibble(events = c(5, 8), exposure = c(100, 150))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_event_group_variance(sdesign, c("events", "exposure", "enum")),
    regexp = "exactly 4 column names"
  )
})

test_that("quality_test_event_group_variance handles missing columns", {
  df <- tibble::tibble(events = c(5, 8), exposure = c(100, 150))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_event_group_variance(
      sdesign,
      c("events", "exposure", "enum", "nonexistent")
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$test_statistic))
})

test_that("quality_test_event_group_variance handles insufficient data", {
  df <- tibble::tibble(
    events = c(5, 8, 3),
    exposure = c(100, 150, 80),
    enum = c("E1", "E2", "E1"),
    cluster = c("C1", "C2", "C3")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_event_group_variance(
      sdesign,
      c("events", "exposure", "enum", "cluster")
    ),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$test_statistic))
})

test_that("quality_test_event_group_variance handles non-positive exposure", {
  df <- tibble::tibble(
    events = c(5, 8, 3, 6, 4, 7),
    exposure = c(100, 0, 80, -50, 90, 130),
    enum = c("E1", "E2", "E1", "E2", "E1", "E2"),
    cluster = c("C1", "C1", "C2", "C2", "C3", "C3")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_event_group_variance(
      sdesign,
      c("events", "exposure", "enum", "cluster")
    ),
    regexp = "Exposure must be positive"
  )
  expect_true(is.na(result$test_statistic))
})


# 20. SEX RATIO COUNT GROUP TEST ####

test_that("quality_test_sexratio_count_group performs test correctly", {
  df <- tibble::tibble(
    num_male = c(5, 8, 3, 6, 4, 7, 2, 9),
    num_female = c(6, 7, 4, 5, 5, 6, 3, 8),
    group = c("A", "B", "C", "A", "B", "C", "A", "B")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sexratio_count_group(
    sdesign,
    c("num_male", "num_female", "group")
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_sexratio_count_group errors with wrong number of variables", {
  df <- tibble::tibble(num_male = c(5, 8), num_female = c(6, 7))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_sexratio_count_group(sdesign, c("num_male", "num_female")),
    regexp = "length 3"
  )
})

test_that("quality_test_sexratio_count_group handles missing columns", {
  df <- tibble::tibble(num_male = c(5, 8), num_female = c(6, 7))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio_count_group(
      sdesign,
      c("num_male", "num_female", "nonexistent")
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sexratio_count_group handles insufficient data", {
  df <- tibble::tibble(
    num_male = c(5, 8, 3),
    num_female = c(6, 7, 4),
    group = c("A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio_count_group(
      sdesign,
      c("num_male", "num_female", "group")
    ),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sexratio_count_group handles negative counts", {
  df <- tibble::tibble(
    num_male = c(5, -2, 3, 6, 4, 7),
    num_female = c(6, 7, 4, 5, 5, 6),
    group = c("A", "B", "C", "A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio_count_group(
      sdesign,
      c("num_male", "num_female", "group")
    ),
    regexp = "Negative counts"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sexratio_count_group handles all zeros for one sex", {
  df <- tibble::tibble(
    num_male = c(0, 0, 0, 0, 0, 0),
    num_female = c(6, 7, 4, 5, 5, 6),
    group = c("A", "B", "C", "A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio_count_group(
      sdesign,
      c("num_male", "num_female", "group")
    ),
    regexp = "zero counts"
  )
  expect_true(is.na(result$statistic))
})


# 21. AGE RATIO COUNT GROUP TEST ####

test_that("quality_test_ageratio_count_group performs test correctly", {
  df <- tibble::tibble(
    age_group1 = c(5, 8, 3, 6, 4, 7, 2, 9),
    age_group2 = c(6, 7, 4, 5, 5, 6, 3, 8),
    group = c("A", "B", "C", "A", "B", "C", "A", "B")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ageratio_count_group(
    sdesign,
    c("age_group1", "age_group2", "group")
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_ageratio_count_group errors with wrong number of variables", {
  df <- tibble::tibble(age_group1 = c(5, 8), age_group2 = c(6, 7))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ageratio_count_group(sdesign, c("age_group1", "age_group2")),
    regexp = "length 3"
  )
})

test_that("quality_test_ageratio_count_group handles missing columns", {
  df <- tibble::tibble(age_group1 = c(5, 8), age_group2 = c(6, 7))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio_count_group(
      sdesign,
      c("age_group1", "age_group2", "nonexistent")
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_ageratio_count_group handles insufficient data", {
  df <- tibble::tibble(
    age_group1 = c(5, 8, 3),
    age_group2 = c(6, 7, 4),
    group = c("A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio_count_group(
      sdesign,
      c("age_group1", "age_group2", "group")
    ),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_ageratio_count_group handles negative counts", {
  df <- tibble::tibble(
    age_group1 = c(5, -2, 3, 6, 4, 7),
    age_group2 = c(6, 7, 4, 5, 5, 6),
    group = c("A", "B", "C", "A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio_count_group(
      sdesign,
      c("age_group1", "age_group2", "group")
    ),
    regexp = "Negative counts"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_ageratio_count_group handles all zeros for one age group", {
  df <- tibble::tibble(
    age_group1 = c(0, 0, 0, 0, 0, 0),
    age_group2 = c(6, 7, 4, 5, 5, 6),
    group = c("A", "B", "C", "A", "B", "C")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio_count_group(
      sdesign,
      c("age_group1", "age_group2", "group")
    ),
    regexp = "zero counts"
  )
  expect_true(is.na(result$statistic))
})


# 22. SD TEST ####

test_that("quality_test_sd calculates standard deviation correctly", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sd(sdesign, "var1")

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.na(result$p_value))
  expect_equal(result$statistic, sd(df$var1), tolerance = 0.01)
})

test_that("quality_test_sd errors with multiple variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_sd(sdesign, c("var1", "var2")),
    regexp = "exactly 1 variable"
  )
})

test_that("quality_test_sd handles missing column", {
  df <- tibble::tibble(var1 = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sd(sdesign, "nonexistent"),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sd handles NA values", {
  df <- tibble::tibble(var1 = c(1, 2, NA, 4, 5))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sd(sdesign, "var1")

  expect_true(!is.na(result$statistic))
  expect_equal(result$statistic, sd(c(1, 2, 4, 5)), tolerance = 0.01)
})


# 23. SD ACROSS PERCENTAGE TEST ####

test_that("quality_test_sd_across_percentage calculates percentage correctly", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(1.1, 2.1, 3.1, 4.1, 5.1),
    var3 = c(1.2, 2.2, 3.2, 4.2, 5.2)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sd_across_percentage(
    sdesign,
    c("var1", "var2", "var3"),
    threshold = 0.5
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.na(result$p_value))
  expect_true(result$statistic >= 0 && result$statistic <= 100)
})

test_that("quality_test_sd_across_percentage errors with single variable", {
  df <- tibble::tibble(var1 = 1:5)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_sd_across_percentage(sdesign, "var1"),
    regexp = "at least 2 variables"
  )
})

test_that("quality_test_sd_across_percentage handles missing columns", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sd_across_percentage(
      sdesign,
      c("var1", "nonexistent")
    ),
    regexp = "not found"
  )
})

test_that("quality_test_sd_across_percentage uses custom threshold", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(1.1, 2.1, 3.1, 4.1, 5.1),
    var3 = c(1.2, 2.2, 3.2, 4.2, 5.2)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sd_across_percentage(
    sdesign,
    c("var1", "var2", "var3"),
    threshold = 2.0
  )

  expect_true(is.numeric(result$statistic))
})


# 24. ANY FLAG PERCENTAGE TEST ####

test_that("quality_test_any_flag_percentage calculates percentage correctly", {
  df <- tibble::tibble(
    flag1 = c(0, 1, 0, 1, 0),
    flag2 = c(0, 0, 1, 1, 0),
    flag3 = c(0, 0, 0, 0, 0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_any_flag_percentage(
    sdesign,
    c("flag1", "flag2", "flag3")
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.na(result$p_value))
  expect_equal(result$statistic, 60) # 3 out of 5 rows have at least one flag
})

test_that("quality_test_any_flag_percentage handles custom flag value", {
  df <- tibble::tibble(
    flag1 = c(2, 0, 2, 0, 0),
    flag2 = c(0, 2, 0, 2, 0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_any_flag_percentage(
    sdesign,
    c("flag1", "flag2"),
    flag_value = 2
  )

  expect_true(is.numeric(result$statistic))
  expect_equal(result$statistic, 80) # 4 out of 5 rows have flag value 2
})

test_that("quality_test_any_flag_percentage handles missing columns", {
  df <- tibble::tibble(flag1 = c(0, 1, 0))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_any_flag_percentage(
      sdesign,
      c("flag1", "nonexistent")
    ),
    regexp = "not found"
  )
})

test_that("quality_test_any_flag_percentage handles all NA values", {
  df <- tibble::tibble(
    flag1 = c(NA, NA, NA),
    flag2 = c(NA, NA, NA)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_any_flag_percentage(sdesign, c("flag1", "flag2")),
    regexp = "No rows with non-missing values"
  )
  expect_true(is.na(result$statistic))
})


# 25. SEX RATIO TEST ####

test_that("quality_test_sexratio performs test correctly", {
  df <- tibble::tibble(
    sex = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sexratio(
    sdesign,
    "sex",
    male_val = "M",
    female_val = "F"
  )

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
  expect_equal(result$statistic, 1.0, tolerance = 0.01) # 1:1 ratio
})

test_that("quality_test_sexratio errors with multiple variables", {
  df <- tibble::tibble(sex = c("M", "F", "M"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_sexratio(
      sdesign,
      c("sex", "other"),
      male_val = "M",
      female_val = "F"
    ),
    regexp = "single character column"
  )
})

test_that("quality_test_sexratio handles missing column", {
  df <- tibble::tibble(sex = c("M", "F", "M"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio(
      sdesign,
      "nonexistent",
      male_val = "M",
      female_val = "F"
    ),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sexratio handles insufficient data", {
  df <- tibble::tibble(sex = c("M", "F"))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_sexratio(
      sdesign,
      "sex",
      male_val = "M",
      female_val = "F"
    ),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_sexratio handles custom expected ratio", {
  df <- tibble::tibble(
    sex = c("M", "M", "F", "M", "F", "M", "F", "M", "F", "M")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_sexratio(
    sdesign,
    "sex",
    male_val = "M",
    female_val = "F",
    expected_ratio_val = 1.5
  )

  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})


# 26. AGE RATIO TEST ####

test_that("quality_test_ageratio performs test correctly", {
  df <- tibble::tibble(
    age_group1 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    age_group2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  result <- quality_test_ageratio(sdesign, c("age_group1", "age_group2"))

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("quality_test_ageratio errors with wrong number of variables", {
  df <- tibble::tibble(age_group1 = c(1, 0, 1))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    quality_test_ageratio(sdesign, "age_group1"),
    regexp = "length 2"
  )
})

test_that("quality_test_ageratio handles missing columns", {
  df <- tibble::tibble(age_group1 = c(1, 0, 1))
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio(sdesign, c("age_group1", "nonexistent")),
    regexp = "not found"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_ageratio handles insufficient data", {
  df <- tibble::tibble(
    age_group1 = c(1, 0),
    age_group2 = c(0, 1)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    result <- quality_test_ageratio(sdesign, c("age_group1", "age_group2")),
    regexp = "Insufficient data"
  )
  expect_true(is.na(result$statistic))
})

test_that("quality_test_ageratio handles custom expected ratio", {
  df <- tibble::tibble(
    age_group1 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    age_group2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressWarnings(
    result <- quality_test_ageratio(
      sdesign,
      c("age_group1", "age_group2"),
      expected_ratio_val = 1.2
    )
  )

  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})
