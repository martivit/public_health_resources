# ---------------------------
# LOG CLASS TEST SUITE
# ---------------------------

library(testthat)
library(tibble)

# ============================================================
# 1. INITIALIZATION TESTS
# ============================================================

test_that("Log initializes with empty df when log_df is NULL", {
  log <- Log$new(
    log_df = NULL,
    log_name = "TestLog",
    required_columns = c("uuid", "issue")
  )

  expect_true(is.data.frame(log$get("log_df")))
  expect_equal(names(log$get("log_df")), c("uuid", "issue"))
  expect_equal(nrow(log$get("log_df")), 0)
})

test_that("Log rejects non-dataframe input on initialize", {
  expect_error(
    Log$new(log_df = "not a df", required_columns = "uuid"),
    regexp = "data frame"
  )
})

test_that("Log fills missing required columns instead of erroring", {
  df <- tibble(a = 1) # missing required 'uuid'

  log <- Log$new(
    log_df = df,
    required_columns = "uuid"
  )

  # Should construct a Log object without error
  expect_s3_class(log, "Log")

  # Should now include all required columns
  expect_true("uuid" %in% names(log$get("log_df")))

  # Newly added required column should be filled with NA
  expect_true(all(is.na(log$get("log_df")$uuid)))
})

test_that("Log stores metadata correctly", {
  suppressMessages(
    log <- Log$new(
      log_df = NULL,
      log_name = "MetaTest",
      required_columns = c("uuid")
    )
  )
  expect_true(c("created_datetime") %in% names(log$get("metadata")))
  expect_true(c("modified_datetime") %in% names(log$get("metadata")))
})

# ============================================================
# 2. SCHEMA & VALIDATION TESTS
# ============================================================

test_that("Log attaches schema using set_schema()", {
  suppressMessages(log <- Log$new(NULL, "SchemaLog", required_columns = "uuid"))
  log$set_schema(list(types = list(uuid = "character")))
  expect_true(is.list(log$get("schema")))
})

test_that("validate() passes when schema matches types", {
  df <- tibble(uuid = c("A", "B"))
  suppressMessages(log <- Log$new(df, required_columns = "uuid"))

  suppressMessages(log$set_schema(list(types = list(uuid = "character"))))

  expect_silent(log$validate())
})

test_that("validate() coerces safely coercible types", {
  df <- tibble(uuid = 1:3) # numeric → character coercible
  suppressMessages(log <- Log$new(df, required_columns = "uuid"))

  suppressMessages(log$set_schema(list(types = list(uuid = "character"))))

  log$validate()

  expect_true(is.character(log$get("log_df")$uuid))
})

test_that("validate() flags type mismatches", {
  df <- tibble(uuid = c("A", "B"))
  suppressMessages(log <- Log$new(df, required_columns = "uuid"))

  suppressMessages(log$set_schema(list(types = list(uuid = "numeric"))))

  issues <- log$validate()
  expect_true("type_mismatch" %in% names(issues))
})

test_that("validate() flags disallowed values", {
  df <- tibble(uuid = c("A", "BAD"))
  suppressMessages(log <- Log$new(df, required_columns = "uuid"))

  suppressMessages(log$set_schema(list(
    allowed_values = list(uuid = c("A", "B"))
  )))

  issues <- log$validate()
  expect_true("disallowed_values" %in% names(issues))
})

test_that("validate() supports schema_override", {
  df <- tibble(uuid = 1:3)
  suppressMessages(log <- Log$new(df, required_columns = "uuid"))

  override <- list(types = list(uuid = "character"))

  expect_no_error(log$validate(schema_override = override))
  expect_true(is.character(log$get("log_df")$uuid))
})

# ============================================================
# 3. APPEND ENTRY TESTS
# ============================================================

test_that("append_entry() adds a row to the log", {
  suppressMessages(log <- Log$new(NULL, required_columns = c("uuid", "issue")))

  suppressMessages(log$append_entry(list(uuid = "1", issue = "test")))

  expect_equal(nrow(log$get("log_df")), 1)
})

test_that("append_entry() errors when required fields missing", {
  suppressMessages(log <- Log$new(NULL, required_columns = c("uuid", "issue")))

  expect_error(
    suppressMessages(log$append_entry(list(uuid = "1"))),
    regexp = "Missing required fields"
  )
})

test_that("append_entry() updates metadata timestamp", {
  suppressMessages(log <- Log$new(NULL, required_columns = c("uuid", "issue")))

  old_time <- log$get("metadata")$modified_datetime
  Sys.sleep(0.01)

  suppressMessages(log$append_entry(list(uuid = "1", issue = "x")))

  expect_true(log$get("metadata")$modified_datetime > old_time)
})

# ============================================================
# 4. CLEAR TESTS
# ============================================================

test_that("clear() empties the log", {
  suppressMessages(
    log <- Log$new(
      log_df = tibble(uuid = "1", issue = "a"),
      required_columns = c("uuid", "issue")
    )
  )

  suppressMessages(log$clear())
  expect_equal(nrow(log$get("log_df")), 0)
})

# ============================================================
# 5. EXPORT TESTS
# ============================================================

test_that("export() writes CSV", {
  tmp <- tempfile(fileext = ".csv")
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))

  suppressMessages(log$append_entry(list(uuid = "x")))

  expect_no_error(log$export(tmp, "csv"))
  expect_true(file.exists(tmp))
})

test_that("export() writes RDS", {
  tmp <- tempfile(fileext = ".rds")
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))

  suppressMessages(log$append_entry(list(uuid = "x")))

  expect_no_error(log$export(tmp, "rds"))
  expect_true(file.exists(tmp))
})

test_that("export() errors for invalid format", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_error(log$export("file.invalid", "badformat"))
})

# ============================================================
# 6. HASH TESTS
# ============================================================

test_that("get_hash() returns a hash string", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  suppressMessages(log$append_entry(list(uuid = "X")))

  h <- log$get_hash()
  expect_true(is.character(h))
})

# ============================================================
# 7. SUMMARY TESTS
# ============================================================

test_that("summary() returns expected structure", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))

  suppressMessages(s <- log$summary())

  expect_true(is.list(s))
  expect_true("log_name" %in% names(s))
  expect_true("n_entries" %in% names(s))
  expect_true("validated" %in% names(s))
})

# ============================================================
# 8. EDGE CASE TESTS
# ============================================================

test_that("validate() handles empty logs without error", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_silent(log$validate())
})

test_that("validate() allows schema with no types section", {
  log <- Log$new(NULL, required_columns = "uuid")
  log$set_schema(list())
  expect_silent(log$validate())
})

test_that("Log accepts extra non-schema columns", {
  df <- tibble(uuid = "1", extra = "x")
  log <- Log$new(df, required_columns = "uuid")

  expect_silent(log$validate())
  expect_true("extra" %in% names(log$get("log_df")))
})

# ============================================================
# 9. GET AND SET METHOD TESTS
# ============================================================

test_that("get() retrieves log_df correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_true(is.data.frame(log$get("log_df")))
})

test_that("get() retrieves log_name correctly", {
  suppressMessages(log <- Log$new(NULL, log_name = "MyLog", required_columns = "uuid"))
  expect_equal(log$get("log_name"), "MyLog")
})

test_that("get() retrieves required_columns correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = c("uuid", "issue")))
  expect_equal(log$get("required_columns"), c("uuid", "issue"))
})

test_that("get() retrieves schema correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  log$set_schema(list(types = list(uuid = "character")))
  expect_true(is.list(log$get("schema")))
})

test_that("get() retrieves validated correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_false(log$get("validated"))
})

test_that("get() retrieves metadata correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  metadata <- log$get("metadata")
  expect_true(is.list(metadata))
  expect_true("created_datetime" %in% names(metadata))
})

test_that("get() errors on invalid field", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_error(log$get("invalid_field"), regexp = "not accessible")
})

test_that("set() updates log_name correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  suppressMessages(log$set("log_name", "NewName"))
  expect_equal(log$get("log_name"), "NewName")
})

test_that("set() updates validated correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  suppressMessages(log$set("validated", TRUE))
  expect_true(log$get("validated"))
})

test_that("set() updates metadata correctly", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  new_metadata <- list(custom_field = "value")
  suppressMessages(log$set("metadata", new_metadata))
  expect_equal(log$get("metadata")$custom_field, "value")
})

test_that("set() validates log_df when setting", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_error(
    suppressMessages(log$set("log_df", "not a dataframe")),
    regexp = "data frame"
  )
})

test_that("set() errors on invalid field", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  expect_error(
    suppressMessages(log$set("invalid_field", "value")),
    regexp = "not settable"
  )
})

test_that("set() updates modified timestamp", {
  suppressMessages(log <- Log$new(NULL, required_columns = "uuid"))
  old_time <- log$get("metadata")$modified_datetime
  Sys.sleep(0.01)
  suppressMessages(log$set("log_name", "UpdatedName"))
  expect_true(log$get("metadata")$modified_datetime > old_time)
})
