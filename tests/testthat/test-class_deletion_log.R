# ============================================================
# DELETION LOG TEST SUITE
# ============================================================

library(testthat)
library(tibble)
library(phrutils)

# ------------------------------------------------------------
# Reuse the MockData class from CleaningLog tests
# ------------------------------------------------------------
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


# ============================================================
# 1. INITIALIZATION TESTS
# ============================================================

test_that("DeletionLog initializes with required columns", {
  log <- DeletionLog$new()

  expect_true(is.data.frame(log$get("log_df")))
  expect_setequal(
    names(log$get("log_df")),
    c("uuid", "enum_id", "device_id", "issue", "feedback")
  )
})

test_that("DeletionLog fills missing required columns when provided log_df is incomplete", {
  df <- tibble(uuid = "1", issue = "bad") # missing enum_id, device_id, feedback, etc.

  log <- DeletionLog$new(log_df = df)

  # Should no longer error
  expect_s3_class(log, "DeletionLog")

  # It should contain all required columns
  expect_true(all(log$required_columns %in% names(log$get("log_df"))))

  # All missing columns should be filled with NA
  missing_cols <- setdiff(log$required_columns, names(df))

  for (col in missing_cols) {
    expect_true(all(is.na(log$get("log_df")[[col]])))
  }
})

test_that("DeletionLog initializes with correct schema types", {
  log <- DeletionLog$new()

  expect_equal(log$get("schema")$types$uuid, "character")
  expect_equal(log$get("schema")$types$issue, "character")
  expect_equal(log$get("schema")$types$feedback, "character")
})


# ============================================================
# 2. add_deletion() TESTS
# ============================================================

test_that("add_deletion() appends a properly formatted row", {
  log <- DeletionLog$new()

  log$add_deletion(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    issue = "duplicate",
    feedback = "confirmed"
  )

  expect_equal(nrow(log$get("log_df")), 1)
  expect_equal(log$get("log_df")$uuid, "u1")
  expect_equal(log$get("log_df")$issue, "duplicate")
})


test_that("add_deletion() allows NA for optional fields", {
  log <- DeletionLog$new()

  log$add_deletion(uuid = "u1", issue = "bad record")

  expect_equal(nrow(log$get("log_df")), 1)
  expect_equal(log$get("log_df")$uuid, "u1")
  expect_equal(log$get("log_df")$issue, "bad record")
  expect_true(is.na(log$get("log_df")$enum_id))
})


# ============================================================
# 3. INTERNAL validate() TESTS
# ============================================================

test_that("DeletionLog validate passes when data is correct", {
  df <- tibble(
    uuid = "u1",
    enum_id = "e1",
    device_id = "d1",
    issue = "bad entry",
    feedback = "resolved"
  )

  log <- DeletionLog$new(df)

  expect_silent(log$validate())
})

test_that("DeletionLog validate errors when required non-empty fields are empty", {
  df <- tibble(
    uuid = "",
    enum_id = "e1",
    device_id = "d1",
    issue = "",
    feedback = "f"
  )

  log <- DeletionLog$new(df)

  expect_warning(
    log$validate(),
    regexp = "missing or empty"
  )
})

test_that("DeletionLog validate coerces safely coercible types", {
  df <- tibble(
    uuid = 1001, # numeric → character
    enum_id = "e1",
    device_id = "d1",
    issue = "bad",
    feedback = "ok"
  )

  log <- DeletionLog$new(df)

  expect_no_error(log$validate())
  expect_true(is.character(log$get("log_df")$uuid))
})


# ============================================================
# 4. POST VALIDATE TESTS
# ============================================================

test_that("post_validate errors if dataset missing UUID column", {
  df_log <- tibble(
    uuid = "u1",
    enum_id = "e",
    device_id = "d",
    issue = "bad",
    feedback = "ok"
  )

  log <- DeletionLog$new(df_log)
  d <- MockData$new(df = tibble(x = 1)) # no uuid column

  expect_error(
    log$post_validate(d),
    regexp = "UUID column 'uuid' not found"
  )
})

test_that("post_validate warns if log UUIDs not in dataset", {
  df_log <- tibble(
    uuid = c("missing1", "missing2"),
    enum_id = c("e", "e"),
    device_id = c("d", "d"),
    issue = c("bad", "worse"),
    feedback = c("x", "y")
  )

  df_data <- tibble(uuid = "u1", x = 1)

  log <- DeletionLog$new(df_log)
  d <- MockData$new(df_data)

  expect_warning(
    log$post_validate(d),
    regexp = "do not exist in dataset"
  )
})

test_that("post_validate passes when all UUIDs match dataset", {
  df_log <- tibble(
    uuid = c("u1"),
    enum_id = "e",
    device_id = "d",
    issue = "bad",
    feedback = "ok"
  )

  df_data <- tibble(uuid = "u1")

  log <- DeletionLog$new(df_log)
  d <- MockData$new(df_data)

  expect_silent(log$post_validate(d))
})


# ============================================================
# 5. EDGE CASES
# ============================================================

test_that("DeletionLog validate allows multiple rows", {
  df <- tibble(
    uuid = c("u1", "u2", "u3"),
    enum_id = c("e1", "e2", "e3"),
    device_id = c("d1", "d2", "d3"),
    issue = c("bad", "worse", "ok"),
    feedback = c("x", "y", "z")
  )

  log <- DeletionLog$new(df)

  expect_silent(log$validate())
})

test_that("DeletionLog initialize allows empty log_df and creates proper structure", {
  log <- DeletionLog$new(log_df = NULL)

  expect_equal(nrow(log$get("log_df")), 0)
  expect_setequal(
    names(log$get("log_df")),
    c("uuid", "enum_id", "device_id", "issue", "feedback")
  )
})


test_that("DeletionLog post_validate allows extra unmapped columns silently", {
  df_log <- tibble(
    uuid = "u1",
    enum_id = "E1",
    device_id = "D1",
    issue = "bad case",
    feedback = "fix"
  )

  df_data <- tibble(
    uuid = "u1",
    extra1 = 1,
    extra2 = "abc"
  )

  log <- DeletionLog$new(df_log)
  d <- MockData$new(df_data)

  expect_silent(log$post_validate(d))
})
