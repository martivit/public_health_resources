# ---------------------------
# QUANT DATA ANALYSIS PLAN LOG CLASS TEST SUITE
# ---------------------------

library(testthat)
library(tibble)

# ============================================================
# 1. INITIALIZATION TESTS
# ============================================================

test_that("QuantDataAnalysisPlanLog initializes with required columns", {

  log <- QuantDataAnalysisPlanLog$new()

  expect_true(is.data.frame(log$get("log_df")))
  expect_setequal(
    names(log$get("log_df")),
    c(
      "indicator_name", "calculation", "var_name", "denom_var",
      "disaggregation", "multiplier", "indicator_unit"
    )
  )
})

test_that("QuantDataAnalysisPlanLog fills missing required columns when provided log_df is incomplete", {

  df <- tibble(indicator_name = "test", calculation = "prop")

  log <- QuantDataAnalysisPlanLog$new(log_df = df)

  # Should not error
  expect_s3_class(log, "QuantDataAnalysisPlanLog")

  # Required columns should all exist
  expect_true(all(log$required_columns %in% names(log$get("log_df"))))

  # Columns not supplied should be filled with NA
  missing_cols <- setdiff(log$required_columns, names(df))

  for (col in missing_cols) {
    expect_true(all(is.na(log$get("log_df")[[col]])))
  }
})

test_that("QuantDataAnalysisPlanLog attaches schema correctly", {

  log <- QuantDataAnalysisPlanLog$new()

  expect_true("types" %in% names(log$get("schema")))
  expect_equal(log$get("schema")$types$indicator_name, "character")
  expect_equal(log$get("schema")$types$calculation, "character")
  expect_equal(log$get("schema")$types$multiplier, "numeric")
  expect_equal(log$get("schema")$allowed_values$calculation,
               c("prop", "mean", "median", "ratio", "cat", "categorical", "select_multiple_cat"))
})

test_that("QuantDataAnalysisPlanLog initializes from CSV template structure", {
  
  # Simulate the structure from phr_analysis_plan_template.csv
  df <- tibble(
    indicator_name = character(),
    calculation = character(),
    var_name = character(),
    denom_var = character(),
    disaggregation = character(),
    multiplier = numeric(),
    indicator_unit = character()
  )
  
  log <- QuantDataAnalysisPlanLog$new(log_df = df)
  
  expect_s3_class(log, "QuantDataAnalysisPlanLog")
  expect_equal(nrow(log$get("log_df")), 0)
  expect_equal(ncol(log$get("log_df")), 7)
})


# ============================================================
# 2. VALIDATION TESTS
# ============================================================

test_that("QuantDataAnalysisPlanLog validate passes on correct data", {

  df <- tibble(
    indicator_name = "Test Indicator",
    calculation = "prop",
    var_name = "test_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  log <- QuantDataAnalysisPlanLog$new(df)

  expect_silent(log$validate())
  expect_true(log$validated)
})

test_that("QuantDataAnalysisPlanLog validate warns on empty required fields", {

  df <- tibble(
    indicator_name = "",
    calculation = "prop",
    var_name = "test_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  log <- QuantDataAnalysisPlanLog$new(df)

  expect_warning(log$validate(), regexp = "missing/empty")
  expect_false(log$validated)
  expect_true("missing_or_empty" %in% names(log$issues))
})

test_that("QuantDataAnalysisPlanLog validate rejects invalid calculation types", {

  df <- tibble(
    indicator_name = "Test",
    calculation = "invalid_calc",
    var_name = "test_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  log <- QuantDataAnalysisPlanLog$new(df)

  issues <- log$validate()
  expect_true("disallowed_values" %in% names(issues))
  expect_false(log$validated)
})

test_that("QuantDataAnalysisPlanLog validate warns on non-positive multipliers", {

  df <- tibble(
    indicator_name = "Test",
    calculation = "prop",
    var_name = "test_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = -5,
    indicator_unit = "%"
  )

  log <- QuantDataAnalysisPlanLog$new(df)

  expect_warning(log$validate(), regexp = "non-positive multipliers")
  expect_false(log$validated)
  expect_true("invalid_multiplier" %in% names(log$issues))
})

test_that("QuantDataAnalysisPlanLog validate handles multiple indicators", {

  df <- tibble(
    indicator_name = c("Indicator 1", "Indicator 2", "Indicator 3"),
    calculation = c("prop", "mean", "ratio"),
    var_name = c("var1", "var2", "var3"),
    denom_var = c(NA_character_, NA_character_, "denom_var"),
    disaggregation = c("admin1", NA_character_, "admin2"),
    multiplier = c(100, 1, 10000),
    indicator_unit = c("%", "mean", "per 10,000")
  )

  log <- QuantDataAnalysisPlanLog$new(df)

  expect_silent(log$validate())
  expect_true(log$validated)
})


# ============================================================
# 3. ADD INDICATOR TESTS
# ============================================================

test_that("QuantDataAnalysisPlanLog add_indicator adds entry correctly", {

  log <- QuantDataAnalysisPlanLog$new()

  log$add_indicator(
    indicator_name = "Test Indicator",
    calculation = "prop",
    var_name = "test_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  expect_equal(nrow(log$get("log_df")), 1)
  expect_equal(log$get("log_df")$indicator_name[1], "Test Indicator")
  expect_equal(log$get("log_df")$calculation[1], "prop")
  expect_equal(log$get("log_df")$multiplier[1], 100)
})

test_that("QuantDataAnalysisPlanLog add_indicator handles multiple entries", {

  log <- QuantDataAnalysisPlanLog$new()

  log$add_indicator(
    indicator_name = "Indicator 1",
    calculation = "prop",
    var_name = "var1"
  )

  log$add_indicator(
    indicator_name = "Indicator 2",
    calculation = "mean",
    var_name = "var2"
  )

  expect_equal(nrow(log$get("log_df")), 2)
  expect_equal(log$get("log_df")$indicator_name, c("Indicator 1", "Indicator 2"))
})

test_that("QuantDataAnalysisPlanLog add_indicator uses defaults correctly", {

  log <- QuantDataAnalysisPlanLog$new()

  log$add_indicator(
    indicator_name = "Test",
    calculation = "prop",
    var_name = "var1"
  )

  expect_equal(log$get("log_df")$multiplier[1], 100)
  expect_equal(log$get("log_df")$indicator_unit[1], "%")
  expect_true(is.na(log$get("log_df")$denom_var[1]))
  expect_true(is.na(log$get("log_df")$disaggregation[1]))
})


# ============================================================
# 4. INTEGRATION WITH LOG BASE CLASS TESTS
# ============================================================

test_that("QuantDataAnalysisPlanLog inherits from Log", {
  log <- QuantDataAnalysisPlanLog$new()
  expect_true(inherits(log, "Log"))
})

test_that("QuantDataAnalysisPlanLog can export to CSV", {
  df <- tibble(
    indicator_name = "Test",
    calculation = "prop",
    var_name = "var1",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )
  
  log <- QuantDataAnalysisPlanLog$new(df)
  
  temp_file <- tempfile(fileext = ".csv")
  
  expect_message(log$export(temp_file, format = "csv"))
  expect_true(file.exists(temp_file))
  
  # Clean up
  unlink(temp_file)
})

test_that("QuantDataAnalysisPlanLog clear() removes all entries", {
  df <- tibble(
    indicator_name = c("Test 1", "Test 2"),
    calculation = c("prop", "mean"),
    var_name = c("var1", "var2"),
    denom_var = c(NA_character_, NA_character_),
    disaggregation = c(NA_character_, NA_character_),
    multiplier = c(100, 1),
    indicator_unit = c("%", "mean")
  )
  
  log <- QuantDataAnalysisPlanLog$new(df)
  expect_equal(nrow(log$get("log_df")), 2)
  
  log$clear()
  expect_equal(nrow(log$get("log_df")), 0)
  expect_equal(ncol(log$get("log_df")), 7)  # Columns preserved
})

test_that("QuantDataAnalysisPlanLog summary() returns correct info", {
  log <- QuantDataAnalysisPlanLog$new()
  
  summary <- log$summary()
  
  expect_equal(summary$log_name, "Quant Data Analysis Plan")
  expect_equal(summary$n_entries, 0)
  expect_equal(summary$n_columns, 7)
  expect_true(summary$schema_attached)
})
