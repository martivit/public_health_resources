
# CLEANING LOG TEST SUITE


library(testthat)
library(tibble)


# Helpers: Minimal mock Data object for post_validate tests


MockData <- R6::R6Class(
  classname = "MockData",
  public = list(
    data = NULL,
    uuid = "uuid",
    variable_map = NULL,

    initialize = function(df) {
      self$data <- df
      self$variable_map <- list()
    },

    get_data = function(stage = "clean") {
      self$data
    }
  )
)


# 1. INITIALIZATION TESTS


test_that("CleaningLog initializes with required columns", {

  log <- CleaningLog$new()

  expect_true(is.data.frame(log$log_df))
  expect_setequal(
    names(log$log_df),
    c(
      "uuid","enum_id","device_id","question.name","issue",
      "feedback","changed","old.value","new.value"
    )
  )
})

test_that("CleaningLog fills missing required columns when provided log_df is incomplete", {

  df <- tibble(uuid = "1", issue = "x")   # missing many required columns

  log <- CleaningLog$new(log_df = df)

  # Should not error anymore
  expect_s3_class(log, "CleaningLog")

  # Required columns should all exist
  expect_true(all(log$required_columns %in% names(log$log_df)))

  # Columns not supplied should be filled with NA
  missing_cols <- setdiff(log$required_columns, names(df))

  for (col in missing_cols) {
    expect_true(all(is.na(log$log_df[[col]])))
  }
})

test_that("CleaningLog attaches schema correctly", {

  log <- CleaningLog$new()

  expect_true("types" %in% names(log$schema))
  expect_equal(log$schema$types$changed, "character")
  expect_equal(log$schema$allowed_values$changed, c("yes","no"))
})


# 2. INTERNAL VALIDATE() TESTS


test_that("CleaningLog validate passes on correct data", {

  df <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "fix",
    feedback = "ok",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df)

  expect_silent(log$validate())
})

test_that("CleaningLog validate warns on empty required fields and sets validated = FALSE", {

  df <- tibble(
    uuid = "",
    enum_id = "e",
    device_id = "d",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df)

  expect_warning(
    {
      result <- log$validate()
    },
    regexp = "missing/empty"
  )

  expect_false(log$validated)
})

test_that("CleaningLog validate checks allowed values for changed", {

  df <- tibble::tibble(
    uuid = "u1",
    enum_id = "e",
    device_id = "d",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "maybe",   # invalid
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df)

  # Run once to get issues (suppress the expected warning output)
  issues <- suppressWarnings(log$validate())
  expect_false(log$validated)
  expect_true("disallowed_values" %in% names(issues))

  # Assert the warning exists, but don't print it
  expect_warning(
    log$validate(),
    regexp = "Log validation completed with issues"
  )
})

test_that("CleaningLog validate coerces safely coercible types", {

  df <- tibble(
    uuid = 1,            # numeric → coercible to character
    enum_id = "e",
    device_id = "d",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df)
  expect_no_error(log$validate())
  expect_true(is.character(log$log_df$uuid))
})


# 3. ADD_CHANGE() TEST


test_that("add_change() inserts properly formatted row", {

  log <- CleaningLog$new()

  log$add_change(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "bad value",
    feedback = "fixed",
    changed = "YES",
    old.value = "10",
    new.value = "20"
  )

  expect_equal(nrow(log$log_df), 1)
  expect_equal(log$log_df$changed, "yes")
})


# 4. POST VALIDATE TESTS


test_that("post_validate errors if dataset is NULL", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df_log)
  d <- MockData$new(df = NULL)

  expect_error(
    log$post_validate(d),
    regexp = "Dataset is NULL"
  )
})

test_that("post_validate errors if UUID column missing from dataset", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  log <- CleaningLog$new(df_log)

  d <- MockData$new(tibble(x = 1))   # missing uuid column

  expect_error(
    log$post_validate(d),
    regexp = "missing UUID column"
  )
})

test_that("post_validate errors if uuid not present in dataset", {

  df_log <- tibble(
    uuid = "missing",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  df_data <- tibble(uuid = "u1", age = "10")
  log <- CleaningLog$new(df_log)
  d <- MockData$new(df_data)

  expect_warning(
    log$post_validate(d),
    regexp = "Unknown UUID"
  )
})

test_that("post_validate errors if question.name not in dataset", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "nonexistent",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",
    new.value = "11"
  )

  df_data <- tibble(uuid = "u1", age = "10")
  log <- CleaningLog$new(df_log)
  d <- MockData$new(df_data)

  expect_warning(
    log$post_validate(d),
    regexp = "Unknown question.name"
  )
})

test_that("post_validate errors on old.value mismatch", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "10",   # should match dataset
    new.value = "12"
  )

  df_data <- tibble(uuid = "u1", age = "99")   # mismatch
  log <- CleaningLog$new(df_log)
  d <- MockData$new(df_data)

  expect_warning(
    log$post_validate(d),
    regexp = "old.value mismatch"
  )
})

test_that("post_validate passes on fully matching data", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    question.name = "age",
    issue = "x",
    feedback = "y",
    changed = "yes",
    old.value = "30",
    new.value = "31"
  )

  df_data <- tibble(uuid = "u1", age = "30")
  log <- CleaningLog$new(df_log)
  d <- MockData$new(df_data)

  expect_silent(log$post_validate(d))
})


# 5. EDGE CASES


test_that("CleaningLog validate allows multiple rows", {

  df <- tibble(
    uuid = c("u1","u2"),
    enum_id = c("e1","e2"),
    device_id = c("d1","d2"),
    question.name = c("age","age"),
    issue = c("fix","fix"),
    feedback = c("ok","ok"),
    changed = c("yes","no"),
    old.value = c("10","20"),
    new.value = c("11","20")
  )

  log <- CleaningLog$new(df)
  expect_silent(log$validate())
})

test_that("CleaningLog ignores enum_id validations if no mapping exists", {

  df_log <- tibble(
    uuid = "u1",
    enum_id = "e_missing",
    device_id = "d",
    question.name = "age",
    issue = "fix",
    feedback = "ok",
    changed = "yes",
    old.value = "10",
    new.value = "20"
  )

  df_data <- tibble(uuid = "u1", age = "10")

  log <- CleaningLog$new(df_log)
  d <- MockData$new(df_data)
  d$variable_map <- list()   # no enum_id mapping

  expect_silent(log$post_validate(d))
})

