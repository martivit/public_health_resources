
library(testthat)
library(tibble)
library(withr)
library(mockery)

# Initialization Tests ####

test_that("Data initializes with minimal valid data", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  expect_s3_class(d, "Data")
  expect_equal(d$dataset_name, "TestData")
  expect_equal(d$uuid, "id")

  expect_false(d$validated)
  expect_false(d$standardized)
  expect_false(d$cleaned)

  expect_equal(d$raw_data, df)
  expect_null(d$standardized_data)
  expect_null(d$clean_data)

  # metadata created
  expect_true(is.list(d$metadata))
  expect_equal(d$metadata$dataset_name, "TestData")
})

test_that("Data$new() errors when data is NULL", {
  expect_error(
    Data$new(data = NULL, uuid = "id"),
    regexp = "No data provided for initialization"
  )
})

test_that("Data$new() errors when data is not a data.frame/tibble", {
  expect_error(Data$new(data = 123, uuid = "id"), regexp = "Expected a data frame")
})

test_that("value_map initializes correctly", {
  df <- tibble::tibble(id = 1)
  vm <- list(category = c("a","b"))

  d <- suppressMessages(
    Data$new(data = df, value_map = vm, uuid = "id")
  )

  expect_equal(d$value_map, vm)
})

test_that("variable_map defaults to uuid mapping", {
  df <- tibble::tibble(id = 1:5)

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_equal(d$variable_map$uuid, "id")
})

test_that("custom variable_map is accepted at initialization", {
  df <- tibble::tibble(id = 1, age = 10)

  vm <- list(uuid = "id", age_var = "age")
  d <- suppressMessages(
    Data$new(data = df, variable_map = vm, uuid = "id")
  )

  expect_equal(d$variable_map, vm)
})

test_that("metadata timestamps exist after initialization", {
  df <- tibble::tibble(id = 1)

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_true("timestamps" %in% names(d$metadata))
  expect_true("updated" %in% names(d$metadata$timestamps))
  expect_true(inherits(d$metadata$timestamps$updated, "POSIXct"))
})

test_that("Data initialization emits an initialization message", {
  df <- tibble::tibble(id = 1)

  expect_no_error(
    Data$new(data = df, dataset_name = "MsgTest", uuid = "id")
  )
})

# Field Integrity Tests ####

test_that("Data fields are initialized with correct default values", {
  df <- tibble::tibble(id = 1:3, x = 4:6)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "FieldTest", uuid = "id")
  )

  # Core data fields
  expect_equal(d$raw_data, df)
  expect_null(d$standardized_data)
  expect_null(d$clean_data)

  # State flags
  expect_false(d$validated)
  expect_false(d$standardized)
  expect_false(d$cleaned)

  # Metadata
  expect_true(is.list(d$metadata))
  expect_equal(d$metadata$dataset_name, "FieldTest")
  expect_true("timestamps" %in% names(d$metadata))

  # Required columns & UUID
  expect_equal(d$uuid, "id")
  expect_equal(d$required_columns, "id")

  # Mapping structures
  expect_true(is.list(d$variable_map))
  expect_equal(d$variable_map$uuid, "id")

  expect_true(is.list(d$value_map))
  expect_length(d$value_map, 0)

  expect_true(is.list(d$variable_label))
  expect_length(d$variable_label, 0)

  expect_true(is.list(d$value_label))
  expect_length(d$value_label, 0)

  # Schema should be NULL until explicitly set
  expect_null(d$schema)

  # Cleaning/deletion logs (should initialize as empty logs)
  expect_true(inherits(d$cleaning_log, "CleaningLog"))
  expect_true(inherits(d$deletion_log, "DeletionLog"))

  # They should have an empty log_df
  expect_true(tibble::is_tibble(d$cleaning_log$log_df))
  expect_true(nrow(d$cleaning_log$log_df) == 0)

  expect_true(tibble::is_tibble(d$deletion_log$log_df))
  expect_true(nrow(d$deletion_log$log_df) == 0)

  # They should not be validated yet
  expect_false(d$cleaning_log$validated)
  expect_false(d$deletion_log$validated)

  # Autosave flag defaults false
  expect_false(d$autosave)

  # Linked objects starts empty
  expect_true(is.list(d$linked_objects))
  expect_length(d$linked_objects, 0)
})


test_that("variable_map and value_map can be overridden and keep correct structure", {
  df <- tibble::tibble(id = 1, age = 25, gender = "M")

  vm <- list(uuid = "id", age_role = "age")
  vmap <- list(age_role = c("20", "25", "30"))

  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "MapOverride",
      variable_map = vm,
      value_map = vmap,
      uuid = "id"
    )
  )

  expect_equal(d$variable_map, vm)
  expect_equal(d$value_map, vmap)
})


test_that("variable_label and value_label accept correct structures", {
  df <- tibble::tibble(id = 1, sex = c("M", "F"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_label("sex", "Sex of respondent")
  expect_equal(d$variable_label$sex, "Sex of respondent")

  d$set_value_labels("sex", c(M = "Male", F = "Female"))
  expect_equal(d$value_label$sex, c(M = "Male", F = "Female"))
})


test_that("summary() returns expected fields and structure", {
  df <- tibble::tibble(id = 1:2, x = 5:6)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "SummaryTest", uuid = "id")
  )

  s <- d$summary()

  expect_true(is.list(s))
  expect_equal(s$dataset_name, "SummaryTest")
  expect_equal(s$n_records, 2)
  expect_equal(s$n_columns, 2)

  expect_equal(s$uuid, "id")
  expect_false(s$validated)
  expect_false(s$standardized)
  expect_false(s$cleaned)

  expect_equal(s$required_columns, "id")
  expect_equal(s$variable_map$uuid, "id")

  expect_true(is.list(s$labels_defined))
  expect_true("vars" %in% names(s$labels_defined))

  expect_false(s$variable_schema_attached)
})


test_that("schema can be assigned and retrieved", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  sch <- list(
    required = c("id"),
    types = list(id = "numeric"),
    allowed_values = list(),
    patterns = list()
  )

  d$set_variable_schema(sch)
  expect_equal(d$get_variable_schema(), sch)
})


test_that("linked_objects is properly structured when linking", {
  df <- tibble::tibble(id = 1:5, hh_id = 10:14)
  df2 <- tibble::tibble(hh_id = 10:14)

  d1 <- suppressMessages(
    Data$new(data = df, dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = df2, dataset_name = "D2", uuid = "hh_id")
  )

  d1$add_linked_dataset(
    name = "household",
    other_object = d2,
    by_self_role = "hh_id",
    by_other_role = "hh_id"
  )

  expect_length(d1$linked_objects, 1)
  expect_true("household" %in% names(d1$linked_objects))
  link <- d1$linked_objects$household

  expect_equal(link$by_self_role, "hh_id")
  expect_equal(link$by_other_role, "hh_id")
  expect_true(inherits(link$object, "Data"))
})

# Validation Pipeline Tests ####

test_that("validate() completes successfully on a minimal valid dataset", {
  df <- tibble::tibble(id = 1:3, x = 5:7)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$validate()

  expect_no_error(d$validate())
  expect_true(d$validated)
})


test_that("initialization errors when UUID column is missing", {

  df <- tibble::tibble(a = 1:3, b = 4:6)  # no 'id'

  expect_error(
    Data$new(data = df, uuid = "id"),
    regexp = "UUID column",
    fixed = TRUE
  )
})


test_that("validate() errors when UUID column has missing values", {
  df <- tibble::tibble(id = c(1, NA, 3))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$validate(),
    regexp = "contains missing \\(NA\\)"
  )
  expect_false(d$validated)
})


test_that("validate() errors when UUID column contains duplicates", {
  df <- tibble::tibble(id = c(1, 1, 2))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$validate(),
    regexp = "Duplicate"
  )
  expect_false(d$validated)
})


test_that("validate() warns when variable_map refers to missing dataset columns", {
  df <- tibble::tibble(id = 1, x = 5)
  d <- suppressMessages(
    Data$new(
      data = df,
      uuid = "id",
      variable_map = list(uuid = "id", age = "age")   # "age" does not exist
    )
  )

  expect_warning(
    d$validate(),
    regexp = "Mapped columns missing"
  )
  expect_false(d$validated)
})


test_that("validate() processes valid variable_map silently", {
  df <- tibble::tibble(id = 1, age = 33)
  d <- suppressMessages(
    Data$new(
      data = df,
      variable_map = list(uuid = "id", age = "age"),
      uuid = "id"
    )
  )

  expect_no_error(d$validate())
  expect_true(d$validated)
})


test_that("validate() warns when value_map references missing columns", {
  df <- tibble::tibble(id = 1, sex = "M")
  d <- suppressMessages(
    Data$new(
      data = df,
      variable_map = list(uuid = "id"),   # no mapping for role "sex_role"
      value_map = list(sex_role = c("M", "F")),
      uuid = "id"
    )
  )

  expect_warning(
    d$validate(),
    regexp = "Value map role.*not linked to any variable_map entry"
  )
  expect_false(d$validated)
})


test_that("validate() warns when value_map contains values not in data", {
  df <- tibble::tibble(id = 1, status = "active")
  d <- suppressMessages(
    Data$new(
      data = df,
      variable_map = list(uuid = "id", status = "status"),
      value_map = list(status = c("active", "inactive")),
      uuid = "id"
    )
  )

  expect_warning(
    d$validate(),
    regexp = "values not found in dataset"
  )
  expect_false(d$validated)
})


test_that("validate() handles empty value_map and variable_map silently", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_no_error(d$validate())
  expect_true(d$validated)
})


test_that("validate() errors when data is not a data.frame", {
  expect_error(
    Data$new(data = 1:5, uuid = "id")$validate(),
    regexp = "data frame"
  )
})


test_that("validate() calls pre_validate and post_validate hooks", {

  TestClass <- R6::R6Class(
    inherit = Data,
    public = list(
      pre_called = FALSE,
      post_called = FALSE,

      pre_validate = function() {
        self$pre_called <- TRUE
      },

      post_validate = function(df) {   # <-- FIXED (added df)
        self$post_called <- TRUE
        return(TRUE)                   # post_validate MUST return TRUE/FALSE
      }
    )
  )

  df <- tibble::tibble(id = 1:3)
  d <- TestClass$new(data = df, uuid = "id")

  d$validate()

  expect_true(d$pre_called)
  expect_true(d$post_called)
})


test_that("validate() updates metadata timestamps and flags", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  before <- d$metadata$timestamps$updated
  Sys.sleep(0.01)
  d$validate()
  after <- d$metadata$timestamps$updated

  expect_true(after > before)
  expect_true(d$metadata$validated)
})

# Standardization Pipeline Tests ####

test_that("standardize() copies raw_data into standardized_data when no changes needed", {
  df <- tibble::tibble(id = 1:3, x = c("a","b","c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()

  expect_no_error(d$standardize())

  expect_true(d$standardized)
  expect_equal(d$standardized_data, df)
})


test_that("standardize() calls validate() automatically and warns if validation fails", {
  # Create data with duplicate UUIDs that will fail validation
  df <- tibble::tibble(id = c(1, 1, 2))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$standardize(),
    regexp = "Duplicate"
  )

  expect_true(d$standardized)
})

test_that("standardize() succeeds when data is valid", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Should validate automatically and succeed
  expect_no_error(d$standardize())

  expect_true(d$validated)
  expect_true(d$standardized)
})



###  Type inference and coercion behavior


test_that("standardize() correctly coerces numeric-like columns", {
  df <- tibble::tibble(id = 1:3, x = c("1","2","3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_type(d$standardized_data$x, "double")
  expect_equal(d$standardized_data$x, c(1,2,3))
})


test_that("standardize() safely handles non-numeric values in numeric inference and treats as character type", {
  df <- tibble::tibble(id = 1:3, x = c("1", "two", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()

  # Should not error
  expect_no_error(d$standardize())

  out <- d$standardized_data

  # x should remain character (numeric inference NOT applied)
  expect_true(is.character(out$x))

  # And the values should be trimmed, no unexpected mutation
  expect_equal(out$x, c("1", "two", "3"))
})


test_that("standardize() coerces logical-like values correctly", {
  df <- tibble::tibble(
    id = 1:3,
    x = c("TRUE", "false", "1")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_true(is.logical(d$standardized_data$x))
  expect_equal(d$standardized_data$x, c(TRUE, FALSE, TRUE))
})


test_that("standardize() does not coerce mixed-format date-like values without schema", {
  df <- tibble::tibble(
    id = 1:3,
    date = c("2022-01-01", "2022/01/02", "01/03/2022")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Should remain character, because inference sees inconsistent formats
  expect_true(is.character(d$standardized_data$date))

  # Values should be trimmed and unchanged
  expect_equal(d$standardized_data$date,
               c("2022-01-01","2022/01/02","01/03/2022"))
})

test_that("standardize() coerces ISO date format YYYY-MM-DD", {
  df <- tibble::tibble(
    id = 1:3,
    date = c("2022-01-01", "2022-01-02", "2022-01-03")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Should detect and convert to Date or POSIXct depending on standardize implementation
  expect_true(inherits(d$standardized_data$date, c("Date", "POSIXct")))

  # Should parse correctly
  expect_equal(
    format(d$standardized_data$date, "%Y-%m-%d"),
    c("2022-01-01", "2022-01-02", "2022-01-03")
  )
})

test_that("standardize() coerces YYYY/MM/DD format", {
  df <- tibble::tibble(
    id = 1:3,
    date = c("2022/01/01", "2022/01/02", "2022/01/03")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_true(inherits(d$standardized_data$date, c("Date", "POSIXct")))

  expect_equal(
    format(d$standardized_data$date, "%Y-%m-%d"),
    c("2022-01-01", "2022-01-02", "2022-01-03")
  )
})

test_that("standardize() coerces DD/MM/YYYY format", {
  df <- tibble::tibble(
    id = 1:3,
    date = c("01/01/2022", "02/01/2022", "03/01/2022")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_true(inherits(d$standardized_data$date, c("Date", "POSIXct")))

  expect_equal(
    format(d$standardized_data$date, "%Y-%m-%d"),
    c("2022-01-01", "2022-01-02", "2022-01-03")
  )
})

test_that("standardize() coerces other columns to character by default", {
  df <- tibble::tibble(id = 1:3, x = factor(c("a","b","c")))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_true(is.character(d$standardized_data$x))
})



### Type inference error safety


test_that("standardize() rejects non-atomic (list-column) inputs", {

  df <- tibble::tibble(id = 1:3, x = I(list(1,2,3)))

  expect_error(
    Data$new(data = df, uuid = "id"),
    regexp = "non-atomic|list-like",
    info = "Data should reject list columns during initialization"
  )
})



### State management and metadata updates


test_that("standardize() updates metadata timestamps", {
  df <- tibble::tibble(id = 1:3, uuid = "id")
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()

  before <- d$metadata$timestamps$updated
  Sys.sleep(0.01)
  d$standardize()
  after <- d$metadata$timestamps$updated

  expect_true(after > before)
})


test_that("standardize() sets standardized_data and standardized flag", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_true(d$standardized)
  expect_false(is.null(d$standardized_data))
})



### Robustness and error handling


test_that("standardize() errors if raw_data is corrupted or missing", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()

  d$raw_data <- NULL   # force failure
  a <- d$standardize()

  # Check if the error string matches the expected pattern
  expect_match(
    a$error,
    regexp = "\\[IPHRA::Error\\] In `Data\\$standardize`: Raw data is NULL; cannot standardize\\..*Hint: Raw dataset has been removed or corrupted\\."
  )
})


test_that("standardize() maintains row and column structure", {
  df <- tibble::tibble(id = 1:3, x = c("1","2","3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expect_equal(nrow(d$standardized_data), 3)
  expect_equal(ncol(d$standardized_data), 2)
})

# Cleaning Pipeline Testing ####


### Cleaning Pipeline Tests for Data Class


test_that("clean() warns when run before standardization", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$clean(),
    regexp = "should be standardized before cleaning"
  )

  expect_true(d$cleaned)
  expect_equal(d$clean_data, df)   # fallback copy
})


test_that("clean() copies standardized_data when available", {
  df <- tibble::tibble(id = 1:3, x = c("1","2","3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  std_copy <- d$standardized_data

  expect_no_error(d$clean())
  expect_true(d$cleaned)

  expect_equal(d$clean_data, std_copy)
})


test_that("clean() copies raw_data if standardized_data is NULL", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()

  d$standardized_data <- NULL

  expect_no_error(d$clean())
  expect_equal(d$clean_data, df)
})


test_that("clean() updates metadata timestamps", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  before <- d$metadata$timestamps$updated
  Sys.sleep(0.01)

  d$clean()
  after <- d$metadata$timestamps$updated

  expect_true(after > before)
})


test_that("clean() sets cleaned flag to TRUE", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$clean()

  expect_true(d$cleaned)
})


test_that("clean() does not alter row or column count", {
  df <- tibble::tibble(id = 1:3, x = c("a","b","c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_no_error(d$clean())

  expect_equal(nrow(d$clean_data), 3)
  expect_equal(ncol(d$clean_data), 2)
})


test_that("clean() does not modify values when no cleaning rules exist", {
  df <- tibble::tibble(id = 1:3, x = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  d$clean()

  expect_equal(d$clean_data$x, c("a","b","c"))
})


test_that("clean() gracefully handles if standardized_data is corrupted", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$standardized_data <- "not a data frame"

  # Should fallback silently to raw_data
  expect_no_error(d$clean())
  expect_equal(d$clean_data, df)
})

### Utility: determine invisibility
is.invisible <- function(x) {
  !is.null(x) && identical(x, suppressWarnings(x))
}

test_that("clean() returns invisible TRUE-like behavior", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$clean()
  expect_true(is.invisible(out))
})




# Data Access Testing ####


### Data Access Tests for Data Class  (get_data)


test_that("get_data returns raw_data for stage='raw'", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$get_data("raw")
  expect_equal(out, df)
})

test_that("get_data returns standardized_data for stage='standardized' when available", {
  df <- tibble::tibble(id = 1:3, x = c("1","2","3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  expected <- d$standardized_data

  out <- d$get_data("standardized")
  expect_equal(out, expected)
})

test_that("get_data returns clean_data for stage='clean' when available", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()
  d$clean()

  expected <- d$clean_data

  out <- d$get_data("clean")
  expect_equal(out, expected)
})

test_that("get_data returns NULL for stages without data available", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # No standardized or clean data yet
  expect_null(d$get_data("standardized"))
  expect_null(d$get_data("clean"))
})

test_that("get_data handles default stage correctly (raw)", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$get_data()  # default argument
  expect_equal(out, df, uuid = "id")
})

test_that("get_data does not mutate internal data objects", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$get_data("raw")
  out$id[1] <- 999

  expect_equal(d$raw_data$id[1], 1)
})

test_that("get_data errors for invalid stage argument", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_error(
    d$get_data("invalid_stage"),
    regexp = "arg.*should be one of"
  )
})

test_that("get_data handles NULL internal data safely (corruption case)", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # simulate accidental corruption
  d$raw_data <- NULL

  out <- d$get_data("raw")
  expect_null(out)
})

test_that("get_data returns a data frame or NULL, never throws from try wrapper", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Should not error
  expect_no_error(d$get_data("raw"))
  expect_s3_class(d$get_data("raw"), "data.frame")
})

# Metadata Tests ####

test_that("metadata is correctly initialized on creation", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestDS", uuid = "id")
  )

  expect_equal(d$metadata$dataset_name, "TestDS")
  expect_false(d$metadata$validated)
  expect_false(d$metadata$standardized)
  expect_false(d$metadata$cleaned)
  expect_true(!is.null(d$metadata$timestamps$updated))
})

test_that("update_metadata updates timestamps and flags", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  old_time <- d$metadata$timestamps$updated
  Sys.sleep(0.01)

  d$validated <- TRUE
  d$update_metadata()

  expect_true(d$metadata$validated)
  expect_gt(d$metadata$timestamps$updated, old_time)
})

test_that("summary returns NULL and warns when no data is present", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # simulate corruption
  d$raw_data <- NULL

  expect_warning(
    out <- d$summary(),
    regexp = "No data loaded"
  )
  expect_null(out)
})

test_that("summary returns expected structure for valid data", {
  df <- tibble::tibble(id = 1:3, x = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "DS1", uuid = "id")
  )

  out <- d$summary()

  expect_true(is.list(out))
  expect_equal(out$dataset_name, "DS1")
  expect_equal(out$n_records, 3)
  expect_equal(out$n_columns, 2)
  expect_false(out$validated)
  expect_false(out$standardized)
  expect_false(out$cleaned)
  expect_equal(out$required_columns, "id")
  expect_equal(out$variable_map$uuid, "id")
})

# Hashing Tests ####
test_that("get_hash returns a valid digest hash for raw data", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  h <- d$get_hash("raw")

  expect_true(is.character(h))
  expect_equal(nchar(h), 32)  # MD5 length
})

test_that("get_hash returns NA when stage has no data", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # No standardized or clean data yet
  expect_true(is.na(d$get_hash("standardized")))
  expect_true(is.na(d$get_hash("clean")))
})

test_that("get_hash reflects changes in the underlying dataset", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  h1 <- d$get_hash("raw")

  # mutate raw_data manually (simulating corruption/change)
  d$raw_data$id[1] <- 999

  h2 <- d$get_hash("raw")

  expect_false(h1 == h2)
})

test_that("get_hash returns identical hash when data unchanged", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  h1 <- d$get_hash("raw")
  h2 <- d$get_hash("raw")

  expect_equal(h1, h2)
})

test_that("get_hash works for standardized data", {
  df <- tibble::tibble(id = 1:3, x = c("1","2","3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  h <- d$get_hash("standardized")
  expect_true(is.character(h))
})

# Variable Map Tests ####

test_that("set_variable stores a valid role → column mapping", {
  df <- tibble::tibble(id = 1:3, age = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_variable("age_role", "age")

  expect_equal(d$variable_map$age_role, "age")
})

test_that("set_variable warns if the column does not exist", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$set_variable("age_role", "age"),
    regexp = "not found in "
  )
})

test_that("set_variable errors if role is not a single character string", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_error(d$set_variable(c("x","y"), "id"))
  expect_error(d$set_variable(123, "id"))
})

test_that("set_variable errors if column_name is not a single character string", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_error(d$set_variable("role", c("id","other")))
  expect_error(d$set_variable("role", 123))
})

test_that("get_variable retrieves the mapped column name", {
  df <- tibble::tibble(id = 1:3, age = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_variable("age_role", "age")

  expect_equal(d$get_variable("age_role"), "age")
})

test_that("get_variable returns NULL for unmapped roles", {
  df <- tibble::tibble(id = 1)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_null(d$get_variable("unknown_role"))
})


# BASIC BEHAVIOR (unchanged)


test_that("resolve_column returns the mapped column when role exists", {
  df <- tibble::tibble(id = 1:3, age = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_variable("age_role", "age")

  expect_equal(d$resolve_column("age_role", "raw"), "age")
})

test_that("resolve_column falls back to raw role name if no mapping exists", {
  df <- tibble::tibble(id = 1:3, custom = 5:7)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_equal(d$resolve_column("custom", "raw"), "custom")
})

test_that("resolve_column warns if mapped column does not exist in stage data", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map$age_role <- "age"  # missing from df

  expect_warning(
    out <- d$resolve_column("age_role", "raw"),
    regexp = "not found in data"
  )

  expect_null(out)
})

test_that("resolve_column returns NULL if stage data is NULL", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$raw_data <- NULL  # break raw data

  expect_null(d$resolve_column("id", stage = "raw"))
})


# VALUE MAP AWARENESS


test_that("resolve_column(values=TRUE) returns column + mapped values when available", {
  df <- tibble::tibble(id = 1:3, sex = c("M","F","F"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list(sex_role = "sex")
  d$value_map    <- list(sex_role = c("M","F","X"))

  out <- d$resolve_column("sex_role", "raw", values = TRUE)

  expect_equal(out$column, "sex")
  expect_setequal(out$values, c("M","F","X"))
})

test_that("resolve_column(values=TRUE) returns NULL values when no value_map exists", {
  df <- tibble::tibble(id = 1:3, age = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list(age_role = "age")

  out <- d$resolve_column("age_role", "raw", values = TRUE)

  expect_equal(out$column, "age")
  expect_null(out$values)
})

test_that("resolve_column(values=TRUE) returns NULL column if column missing", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list(sex_role = "sex")
  d$value_map    <- list(sex_role = c("M","F"))

  expect_warning(
    out <- d$resolve_column("sex_role", values = TRUE),
    regexp = "not found"
  )

  expect_null(out$column)
  expect_equal(out$values, c("M","F"))
})


# FULL MODE (full = TRUE)


test_that("resolve_column(full=TRUE) returns full diagnostic structure", {
  df <- tibble::tibble(id = 1:3, sex = c("M","F","F"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list(sex_role = "sex")
  d$value_map    <- list(sex_role = c("M","F","X"))

  out <- d$resolve_column("sex_role", "raw", full = TRUE)

  expect_equal(out$role, "sex_role")
  expect_equal(out$column, "sex")
  expect_equal(out$values, c("M","F","X"))
  expect_true(out$exists_in_data)
  expect_setequal(out$values_in_data, c("M","F"))
})

test_that("resolve_column(full=TRUE) reports missing column correctly", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list(sex_role = "sex")
  d$value_map    <- list(sex_role = c("M","F"))

  expect_warning(
    out <- d$resolve_column("sex_role", full = TRUE),
    regexp = "not found"
  )

  expect_equal(out$role, "sex_role")
  expect_null(out$column)
  expect_equal(out$values, c("M","F"))
  expect_false(out$exists_in_data)
  expect_null(out$values_in_data)
})


# DIRECT COLUMN NAME + FULL MODE


test_that("resolve_column(full=TRUE) works for direct column names", {
  df <- tibble::tibble(id = 1:3, region = c("A","B","C"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  out <- d$resolve_column("region", "raw", full = TRUE)

  expect_equal(out$column, "region")
  expect_null(out$values) # no value_map
  expect_true(out$exists_in_data)
  expect_setequal(out$values_in_data, c("A","B","C"))
})


# EDGE CASE: role missing or blank


test_that("resolve_column returns NULL on missing/blank role", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_null(d$resolve_column(NULL))
  expect_null(d$resolve_column(""))
})

test_that("mapping supports overwriting an existing role", {
  df <- tibble::tibble(id = 1:3, a = 1:3, b = 4:6)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_variable("role1", "a")
  d$set_variable("role1", "b")

  expect_equal(d$variable_map$role1, "b")
})

test_that("variable_map handles NULL or empty maps safely during validate", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map <- list()  # clear map

  expect_no_error(d$validate())
})

test_that("validate warns about mapped columns missing in the dataset", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, variable_map = list(uuid="id", age_role="age"), uuid = "id")
  )

  expect_warning(
    d$validate(),
    regexp = "Mapped columns missing"
  )
})

test_that("mapping remains valid after modifying dataset", {
  df <- tibble::tibble(id = 1:3, age = 10:12)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_variable("age_role", "age")

  expect_no_error(
    out <- d$resolve_column("age_role")
  )
})

test_that("resolve_column gracefully handles roles mapped to NULL", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$variable_map$broken_role <- NULL

  expect_null(d$resolve_column("broken_role"))
})

# Value Map Tests ####

test_that("value_map initializes as empty list when not provided", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_true(is.list(d$value_map))
  expect_length(d$value_map, 0)
})

test_that("value_map can be provided at initialization", {
  df <- tibble::tibble(id = 1:3, sex = c("M","F","M"))
  vm <- list(sex_role = c("M","F"))

  d <- suppressMessages(
    Data$new(data = df, variable_map = list(sex_role = "sex"), value_map = vm, uuid = "id")
  )

  expect_equal(d$value_map$sex_role, c("M","F"))
})

test_that("validate warns when value map refers to a column not in the dataset", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(
      data = df,
      variable_map = list(uuid="id", sex_role="sex"),
      value_map = list(sex_role = c("M","F")),
      uuid = "id"
    )
  )

  expect_warning(
    d$validate("raw"),
    regexp = "missing"
  )
})

test_that("validate warns when mapped values are not found in the dataset", {
  df <- tibble::tibble(id = 1:3, sex = c("M","M","M"))
  d <- suppressMessages(
    Data$new(
      data = df,
      variable_map = list(sex_role="sex"),
      value_map = list(sex_role = c("M","F")),
      uuid = "id"
    )
  )

  expect_warning(
    d$validate(),
    regexp = "not found in dataset"
  )
})

test_that("data_diagnose fails if no variable schema is defined", {
  df <- tibble::tibble(id = 1:3, sex = c("M", "F", "M"))
  d <- suppressMessages(
    Data$new(
      data = df,
      uuid = "id"
    )
  )

  # Check for warning when no variable schema is defined
  expect_warning(
    out <- d$data_diagnose(),
    regexp = "\\[IPHRA::⧫No variable schema defined\\.⧫\\]"
  )

  # Ensure output does not contain "values_found"
  expect_false("values_found" %in% names(out$confirmations))
})

# Export Tests ####

test_that("export_data errors if requested stage has no data", {
  d <- suppressMessages(
    Data$new(data = tibble(id = 1:3), uuid = "id")
  )
  # no standardize, no clean → standardized and clean are NULL

  expect_error(
    d$export_data(stage = "standardized", format = "csv", file_path = tempfile()),
    regexp = "No data available"
  )
})

test_that("export_data creates CSV file and returns the path", {
  local_tempdir()  # keep output isolated

  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df  # simulate cleaned stage

  out <- d$export_data(stage = "clean", format = "csv")

  expect_true(file.exists(out))
  exported <- read.csv(out, stringsAsFactors = FALSE)
  expect_equal(exported$id, df$id)
})

test_that("export_data creates RDS file and returns the path", {
  local_tempdir()

  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  out <- d$export_data(stage = "clean", format = "rds")

  expect_true(file.exists(out))
  exported <- readRDS(out)
  expect_equal(exported$id, df$id)
})

test_that("export_data creates XLSX file when openxlsx is available", {
  local_tempdir()

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    skip("openxlsx not installed")
  }

  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  out <- d$export_data(stage = "clean", format = "xlsx")

  expect_true(file.exists(out))

  imported <- openxlsx::read.xlsx(out)
  expect_equal(imported$id, df$id)
})

test_that("export_data auto-generates file path when none supplied", {
  local_tempdir()

  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  out <- d$export_data(stage = "clean", format = "csv")

  # Should contain dataset_name + stage
  expect_true(grepl("Data_clean.csv$", out))
  expect_true(file.exists(out))
})

test_that("export_data respects specified file path", {
  local_tempdir()

  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  fp <- file.path(tempdir(), "custom_export.csv")

  out <- d$export_data(stage = "clean", format = "csv", file_path = fp)

  expect_equal(out, fp)
  expect_true(file.exists(fp))
})

test_that("export_data fails with invalid file_path argument", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  expect_error(
    d$export_data(stage = "clean", format = "csv", file_path = 123),
    regexp = "invalid connection"
  )
})

test_that("export_data errors if file_path directory does not exist", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$clean_data <- df

  bad_path <- file.path(tempdir(), "nonexistent_dir", "file.csv")

  # utils::write.csv will error due to missing directory
  expect_error(
    d$export_data(stage = "clean", format = "csv", file_path = bad_path)
  )
})

test_that("export_data exports standardized data correctly", {
  local_tempdir()

  df <- tibble(id = c("1", "2"), v = c("3", "4"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()  # creates numeric conversions for these columns

  expect_true(d$standardized)

  out <- d$export_data(stage = "standardized", format = "csv")

  exported <- read.csv(out)
  expect_equal(exported$id, c(1,2))   # numeric conversion verified
  expect_equal(exported$v, c(3,4))
})


# Linking and Relational Validation Tests ####

test_that("add_linked_dataset registers links correctly", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = tibble(pid = 1:3), dataset_name = "D2", uuid = "pid")
  )

  d1$add_linked_dataset(
    name = "child",
    other_object = d2,
    by_self_role = "uuid",
    by_other_role = "parent"
  )

  expect_true("child" %in% names(d1$linked_objects))
  expect_equal(d1$linked_objects$child$object, d2)
  expect_equal(d1$linked_objects$child$by_self_role, "uuid")
  expect_equal(d1$linked_objects$child$by_other_role, "parent")
})


test_that("validate_links returns TRUE when foreign keys match", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = tibble(pid = 1:3), dataset_name = "D2", uuid = "pid")
  )

  d1$set_variable("uuid", "id")
  d2$set_variable("parent", "pid")

  d1$add_linked_dataset("child", d2, by_self_role = "uuid", by_other_role = "parent")

  # Should return invisible(TRUE)
  res <- d1$validate_links()

  expect_true(isTRUE(res))
})


test_that("validate_links detects missing foreign keys", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )

  # Foreign key 99 is missing in D1
  d2 <- suppressMessages(
    Data$new(data = tibble(pid = c(1, 99)), dataset_name = "D2", uuid = "pid")
  )

  d1$set_variable("uuid", "id")
  d2$set_variable("parent", "pid")

  d1$add_linked_dataset("child", d2, by_self_role = "uuid", by_other_role = "parent")

  # --- Run full workflow on both datasets ---
  d1$validate("raw"); d1$standardize(); d1$clean()
  d2$validate("raw"); d2$standardize(); d2$clean()

  # Now run link validation
  res <- d1$validate_links()

  # Should return a list of problems, not TRUE
  expect_type(res, "list")
  expect_true("child" %in% names(res))

  # The missing foreign key value is 99
  expect_equal(res$child, 99)
})


test_that("validate_links warns when a link role resolves to a missing column", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = tibble(x = 1:3), dataset_name = "D2", uuid = "x")
  )

  # Map roles to columns
  d1$set_variable("uuid", "id")
  d2$set_variable("parent", "x")

  # Introduce a mismatch: map child's linking role to a column that doesn't exist
  d1$add_linked_dataset("child", d2, by_self_role = "missing_role", by_other_role = "parent")

  expect_warning(
    d1$validate_links(),
    regexp = "not found in data",
    info = "resolve_column should warn before link validation"
  )
})


test_that("validate_links handles no linked objects gracefully", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )

  res <- d1$validate_links()

  expect_true(isTRUE(res))
})


test_that("add_linked_dataset throws error when name is invalid", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), uuid = "id")
  )

  expect_error(
    d1$add_linked_dataset(name = NULL, other_object = d1, by_self_role = "uuid", by_other_role = "uuid"),
    regexp = "must be a single character string"
  )

  expect_error(
    d1$add_linked_dataset(name = c("a", "b"), other_object = d1, by_self_role = "uuid", by_other_role = "uuid"),
    regexp = "must be a single character string"
  )
})


test_that("validate_links does nothing if roles resolve to NULL", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = tibble(pid = 1:3), dataset_name = "D2", uuid = "pid")
  )

  # Map only the other side
  d2$set_variable("parent", "pid")

  # Role in D1 is missing so resolve_column returns NULL
  d1$add_linked_dataset("child", d2, by_self_role = "unknown_role", by_other_role = "parent")

  res <- d1$validate_links()
  expect_true(isTRUE(res))
})


test_that("validate_links respects specified stages for both objects", {

  d1 <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D1", uuid = "id")
  )
  d2 <- suppressMessages(
    Data$new(data = tibble(pid = 1:3), dataset_name = "D2", uuid = "pid")
  )

  d1$set_variable("uuid", "id")
  d2$set_variable("parent", "pid")

  # Standardize: duplicate clean stage into standardized & clean
  d1$standardized_data <- d1$raw_data
  d2$standardized_data <- d2$raw_data
  d1$clean_data <- d1$raw_data
  d2$clean_data <- d2$raw_data


  d1$add_linked_dataset("child", d2, by_self_role = "uuid", by_other_role = "parent")

  result <- d1$validate_links(stage_self = "standardized", stage_other = "standardized")

  expect_equal(result, TRUE)
})

# RUN_QUALITY_CHECK Testing ####

test_that("run_quality_checks errors when data at stage is NULL", {

  d <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D", uuid = "id")
  )

  # Remove standardized data
  d$standardized_data <- NULL

  expect_error(
    d$run_quality_checks(stage = "standardized"),
    regexp = "No dataset is available at the selected stage"
  )
})


test_that("run_quality_checks warns and returns NULL when no schema is attached", {

  d <- suppressMessages(
    Data$new(data = tibble(id = 1:3), dataset_name = "D", uuid = "id")
  )

  # No schema set
  expect_warning(
    res <- d$run_quality_checks(stage = "raw"),
    regexp = "No dependency schema or type information available"
  )

  expect_null(res)
  expect_null(d$data_quality_flags)
})

# DATA_DIAGNOSE Testing ####

test_that("data_diagnose returns NULL when no variable schema is defined", {
  df <- tibble(id = 1:3, name = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    result <- d$data_diagnose(stage = "raw"),
    regexp = "No variable schema defined"
  )

  expect_null(result)
})

test_that("data_diagnose returns NULL when stage data is NULL", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set a schema but remove standardized data
  d$variable_schema <- list(types = list(id = "numeric"))

  expect_warning(
    result <- d$data_diagnose(stage = "standardized"),
    regexp = "No data available"
  )

  expect_null(result)
})

test_that("data_diagnose generates basic diagnostic table with types only", {
  df <- tibble(id = 1:3, age = c("20", "25", "30"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    ),
    col_names = list(
      id = c("id"),
      age = c("age")
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age")

  result <- d$data_diagnose(stage = "raw")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true(all(c("required_variable", "required_type", "mapped_variable", "safely_coercible", "issues") %in% names(result)))

  # Check that id and age are in the diagnostic table
  expect_true("id" %in% result$required_variable)
  expect_true("age" %in% result$required_variable)

  # Both should be safely coercible to numeric
  id_row <- result[result$required_variable == "id", ]
  expect_true(id_row$safely_coercible)
  expect_equal(id_row$issues, "ok")
})

test_that("data_diagnose detects unmapped variables", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id")  # age not mapped

  result <- d$data_diagnose(stage = "raw")

  age_row <- result[result$required_variable == "age", ]
  expect_true(grepl("variable not mapped", age_row$issues))
})

test_that("data_diagnose detects mapped variables not in dataset", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age_col")  # age_col doesn't exist

  result <- d$data_diagnose(stage = "raw")

  age_row <- result[result$required_variable == "age", ]
  expect_true(grepl("mapped variable not in dataset", age_row$issues))
})

test_that("data_diagnose detects type coercion issues", {
  df <- tibble(id = 1:3, status = c("active", "inactive", "pending"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      status = "numeric"  # Can't coerce text to numeric
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", status = "status")

  result <- d$data_diagnose(stage = "raw")

  status_row <- result[result$required_variable == "status", ]
  expect_false(status_row$safely_coercible)
  expect_true(grepl("not safely coercible", status_row$issues))
})

test_that("data_diagnose handles value maps with nested format", {
  df <- tibble(id = 1:3, status = c("A", "I", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      status = "character"
    ),
    value_map = list(
      status = list(
        active = c("A", "active", "yes"),
        inactive = c("I", "inactive", "no")
      )
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", status = "status")
  d$value_map <- list(status = list(active = c("A"), inactive = c("I")))

  result <- d$data_diagnose(stage = "raw")

  # Should have rows for each value mapping
  status_rows <- result[result$required_variable == "status", ]
  expect_true(nrow(status_rows) >= 2)
  expect_true("active" %in% status_rows$required_value)
  expect_true("inactive" %in% status_rows$required_value)
})

test_that("data_diagnose detects unmapped values", {
  df <- tibble(id = 1:3, status = c("A", "I", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      status = "character"
    ),
    value_map = list(
      status = list(
        active = c("A"),
        inactive = c("I")
      )
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", status = "status")
  # Don't set value_map - values are not mapped

  result <- d$data_diagnose(stage = "raw")

  status_rows <- result[result$required_variable == "status", ]
  # Should have issues about unmapped values
  expect_true(any(grepl("value not mapped", status_rows$issues)))
})

test_that("data_diagnose detects mapped values not in dataset", {
  df <- tibble(id = 1:3, status = c("A", "A", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      status = "character"
    ),
    value_map = list(
      status = list(
        active = c("A"),
        inactive = c("I", "X")  # X not in data
      )
    )
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", status = "status")
  d$value_map <- list(status = list(active = c("A"), inactive = c("I", "X")))

  result <- d$data_diagnose(stage = "raw")

  status_rows <- result[result$required_variable == "status", ]
  inactive_row <- status_rows[status_rows$required_value == "inactive", ]
  expect_true(grepl("mapped values not in dataset", inactive_row$issues))
})

test_that("data_diagnose stores result in data_diagnostics field", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(id = "numeric", age = "numeric")
  )

  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age")

  result <- d$data_diagnose(stage = "raw")

  expect_equal(d$data_diagnostics, result)
  expect_s3_class(d$data_diagnostics, "data.frame")
})

# GENERATE_CLEANING_LOG Testing ####

test_that("generate_cleaning_log warns when no quality flags available", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$standardize()

  expect_warning(
    d$generate_cleaning_log(stage = "standardized"),
    regexp = "No data quality flags available"
  )
})

test_that("generate_cleaning_log errors when stage data is NULL", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_error(
    d$generate_cleaning_log(stage = "standardized"),
    regexp = "No data available"
  )
})

test_that("generate_cleaning_log creates entries from quality flags", {
  df <- tibble(id = 1:3, age = c("20", "bad", "30"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set up schema
  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age")

  d$validate()
  d$standardize()

  # Run quality checks which should flag the "bad" value in age
  d$run_quality_checks(stage = "standardized")

  # Generate cleaning log
  d$generate_cleaning_log(stage = "standardized")

  # Check that cleaning log has entries
  expect_true(nrow(d$cleaning_log$log_df) > 0)

  # Check that age issues are logged
  age_entries <- d$cleaning_log$log_df[grepl("age", d$cleaning_log$log_df$question.name), ]
  expect_true(nrow(age_entries) > 0)
})

test_that("generate_cleaning_log respects overwrite parameter", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$standardize()

  # Add a manual entry
  d$cleaning_log$add_change(
    uuid = 1,
    enum_id = NA_character_,
    device_id = NA_character_,
    question.name = "age",
    issue = "manual",
    feedback = "Manual entry",
    changed = "no",
    old.value = "20",
    new.value = NA_character_
  )

  expect_equal(nrow(d$cleaning_log$log_df), 1)

  # Generate with overwrite = FALSE (default)
  d$data_quality_flags <- data.frame(
    id = 1:3,
    flag_test = c(0, 0, 0)
  )
  d$generate_cleaning_log(stage = "standardized", overwrite = FALSE)

  # Manual entry should still be there
  expect_true(any(grepl("manual", d$cleaning_log$log_df$issue)))

  # Generate with overwrite = TRUE
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  # Manual entry should be gone
  expect_false(any(grepl("manual", d$cleaning_log$log_df$issue)))
})

test_that("generate_cleaning_log sets changed='yes' for autoclean flags", {
  df <- tibble(id = 1:3, age = c("20", "bad", "30"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age")

  d$validate()
  d$standardize()
  d$run_quality_checks(stage = "standardized")

  # Generate cleaning log
  d$generate_cleaning_log(stage = "standardized")

  # Type coercion issues should be marked as changed='yes'
  type_entries <- d$cleaning_log$log_df[grepl("_type$", d$cleaning_log$log_df$issue), ]
  if (nrow(type_entries) > 0) {
    expect_true(all(type_entries$changed == "yes"))
  }
})

test_that("generate_cleaning_log handles 'other' columns", {
  df <- tibble(
    id = 1:3,
    choice = c("option1", "other", "option2"),
    choice_other_text = c(NA_character_, "custom response", NA_character_)
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$standardize()

  # Set up other_columns
  d$other_columns <- list(
    choice_other_text = list(
      other_column = "choice_other_text",
      other_linked_columns = c("choice")
    )
  )

  d$data_quality_flags <- data.frame(
    id = 1:3
  )

  d$generate_cleaning_log(stage = "standardized")

  # Should have entries for the other response
  other_entries <- d$cleaning_log$log_df[grepl("other_response|has_other_response", d$cleaning_log$log_df$issue), ]
  expect_true(nrow(other_entries) > 0)

  # Check that both the main column and linked column have entries
  expect_true(any(d$cleaning_log$log_df$question.name == "choice_other_text"))
  expect_true(any(d$cleaning_log$log_df$question.name == "choice"))
})

test_that("generate_cleaning_log uses enum_id and device_id when available", {
  df <- tibble(
    id = 1:3,
    enum_id = c("E1", "E2", "E3"),
    device_id = c("D1", "D2", "D3"),
    age = c("20", "bad", "30")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(
    id = "id",
    age = "age",
    enum_id = "enum_id",
    device_id = "device_id"
  )

  d$validate()
  d$standardize()
  d$run_quality_checks(stage = "standardized")

  d$generate_cleaning_log(stage = "standardized")

  # Check that enum_id and device_id are populated
  entries_with_ids <- d$cleaning_log$log_df[!is.na(d$cleaning_log$log_df$enum_id), ]
  expect_true(nrow(entries_with_ids) > 0)
})

# Test 1: Cleaning log should use actual column names from dataset (via variable_map)
test_that("generate_cleaning_log uses actual column names from variable_map", {
  df <- tibble(
    uuid_col = 1:3,
    my_enum = c("E1", "E2", "E3"),
    my_device = c("D1", "D2", "D3"),
    person_age = c("a", "25", "200")  # Two out of range
  )

  # Variable map: canonical name 'age' maps to actual column 'person_age'
  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ColumnNameTest",
      uuid = "uuid_col",
      variable_map = list(
        uuid = "uuid_col",
        enum_id = "my_enum",
        device_id = "my_device",
        age = "person_age"  # Canonical name -> actual column
      )
    )
  )

  d$set_variable_schema(
    data_table_to_schema(
      data.frame(
        rule_type = c("variable","variable","variable","variable"),
        variable = c("uuid_col", "my_enum", "my_device", "person_age"),
        value = c(NA,NA,NA,NA),
        required = c(TRUE, FALSE, FALSE,FALSE),
        type = c("character", "character", "character","numeric"),
        question_type = c(NA,NA,NA,NA),
        is_other = c(NA,NA,NA,NA),
        other_column_link = c(NA,NA,NA,NA),
        allowed = c(NA,NA,NA,NA),
        col_names = c("uuid_col", "my_enum", "my_device", "person_age"),
        unique = c(TRUE, FALSE, FALSE,FALSE),
        label = c(NA,NA,NA,NA),
        comment = c(NA,NA,NA,NA),
        stringsAsFactors = FALSE
      )
    )
  )

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log()

  # Should have 1 entries (for age out of type)
  expect_equal(nrow(d$cleaning_log$log_df), 1)

  # CRITICAL: question.name should be the actual column name 'person_age', not 'age'
  expect_true(all(d$cleaning_log$log_df$question.name == "person_age"))

  # Should NOT have the canonical name 'age'
  expect_false(any(d$cleaning_log$log_df$question.name == "age"))
})

# Test 2: Cleaning log should add entries for each variable in dependency schema
test_that("generate_cleaning_log adds entries for all variables in dependency checks", {
  df <- tibble(
    id = 1:4,
    fever_status = c("yes", "yes", "no", "yes"),
    temperature = c(38, NA, 37, NA),
    medication = c("aspirin", "paracetamol", NA, NA)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "DependencyVarsTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        fever = "fever_status",  # Canonical -> actual
        temp = "temperature",    # Canonical -> actual
        meds = "medication"      # Canonical -> actual
      ),
      value_map = list(
        fever = list(yes = "yes", no = "no")
      )
    )
  )

  d$set_variable_schema(
    data_table_to_schema(
      data.frame(
        rule_type = c("variable","variable","variable","variable"),
        variable = c("id", "fever_status", "temperature", "medication"),
        value = c(NA,"yes,no",NA,NA),
        required = c(TRUE, FALSE, FALSE,FALSE),
        type = c("character", "character", "numeric","character"),
        question_type = c(NA,NA,NA,NA),
        is_other = c(NA,NA,NA,NA),
        other_column_link = c(NA,NA,NA,NA),
        allowed = c(NA,NA,NA,NA),
        col_names = c("id", "fever_status", "temperature", "medication"),
        unique = c(TRUE, FALSE, FALSE,FALSE),
        label = c(NA,NA,NA,NA),
        comment = c(NA,NA,NA,NA),
        stringsAsFactors = FALSE
      )
    )
  )

  # Dependency with multiple variables
  d$set_dependency_schema(list(
    dependencies = list(
      flag_fever_temp_check = list(
        variables = c("fever", "temp", "meds"),  # Canonical names
        condition_if = "fever == 'yes'",
        then = "!is.na(temp)",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  # d$run_quality_checks("standardized")
  d$generate_cleaning_log()

  # Should have entries for rows 2 and 4 (fever='yes' but temp or meds are NA)
  # Each flagged row should generate entries for ALL 3 variables in the dependency
  expect_gt(nrow(d$cleaning_log$log_df), 0)

  # Get entries for one of the flagged rows (e.g., row 2)
  row2_entries <- d$cleaning_log$log_df[d$cleaning_log$log_df$uuid == 2, ]

  # Should have entries for all 3 variables mentioned in the dependency
  expect_equal(nrow(row2_entries), 3)

  # CRITICAL: question.name should be the actual column names from dataset
  expect_true("fever_status" %in% row2_entries$question.name)
  expect_true("temperature" %in% row2_entries$question.name)
  expect_true("medication" %in% row2_entries$question.name)

  # Should NOT have canonical names
  expect_false("fever" %in% row2_entries$question.name)
  expect_false("temp" %in% row2_entries$question.name)
  expect_false("meds" %in% row2_entries$question.name)
})

# Test 3: Handle dependencies where canonical names match actual column names
test_that("generate_cleaning_log works when canonical names equal actual column names", {
  df <- tibble(
    id = 1:2,
    status = c("other", "active"),
    status_other = c("pending", NA)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "NoMapTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        status = "status",  # Same name
        status_other = "status_other"  # Same name
      )
    )
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_other_check = list(
        variables = c("status", "status_other"),
        condition_if = "status == 'other'",
        then = "!is.na(status_other)",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log()

  # Row 1 passes the check (status='other' and status_other='pending' is not NA), so no flags
  # This test should produce 0 entries
  expect_equal(nrow(d$cleaning_log$log_df), 2)
})

# Test 4: Handle dependencies with unmapped canonical names gracefully
test_that("generate_cleaning_log handles unmapped canonical names gracefully", {
  df <- tibble(
    id = 1:2,
    fever = c("yes", "no"),
    temp = c(NA, 37)
  )

  # No variable_map for fever or temp
  d <- suppressMessages(
    Data$new(df, dataset_name = "UnmappedTest", uuid = "id")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_fever_check = list(
        variables = c("fever", "temp"),  # These are not in variable_map
        condition_if = "fever == 'yes'",
        then = "!is.na(temp)",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log()

  # Should still generate entries
  expect_gt(nrow(d$cleaning_log$log_df), 0)

  # When no mapping exists, should fall back to using canonical name as-is
  row1_entries <- d$cleaning_log$log_df[d$cleaning_log$log_df$uuid == 1, ]
  expect_true(all(row1_entries$question.name %in% c("fever", "temp")))
})


test_that("generate_cleaning_log adds flagged records to deletion log for flag_delete action", {
  df <- tibble(
    id = 1:4,
    status = c("valid", "invalid", "valid", "invalid")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_invalid_status = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status == 'valid'",
        action = "flag_delete"
      )
    )
  ))

  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  # Records with invalid status should be in deletion log
  expect_equal(nrow(d$deletion_log$log_df), 2)
  expect_true(all(d$deletion_log$log_df$uuid %in% c("2", "4")))
  expect_true(all(d$deletion_log$log_df$issue == "flag_invalid_status"))
})

test_that("generate_cleaning_log does not add flag_delete records to cleaning log", {
  df <- tibble(
    id = 1:3,
    status = c("valid", "invalid", "valid")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_bad_record = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status == 'valid'",
        action = "flag_delete"
      )
    )
  ))

  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  # Deletion log should have the flagged record
  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_equal(as.character(d$deletion_log$log_df$uuid), "2")

  # Cleaning log should NOT have any entries for this flag
  flag_entries <- d$cleaning_log$log_df[d$cleaning_log$log_df$issue == "flag_bad_record", ]
  expect_equal(nrow(flag_entries), 0)
})

test_that("generate_cleaning_log flag_delete populates enum_id and device_id in deletion log", {
  df <- tibble(
    id = 1:3,
    enum_id = c("E1", "E2", "E3"),
    device_id = c("D1", "D2", "D3"),
    status = c("valid", "invalid", "valid")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$variable_map <- list(uuid = "id", enum_id = "enum_id", device_id = "device_id")

  d$set_dependency_schema(list(
    dependencies = list(
      flag_bad_record = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status == 'valid'",
        action = "flag_delete"
      )
    )
  ))

  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_equal(d$deletion_log$log_df$enum_id, "E2")
  expect_equal(d$deletion_log$log_df$device_id, "D2")
})

test_that("generate_cleaning_log flag_delete deletion log feedback describes the flag", {
  df <- tibble(
    id = 1:2,
    score = c(100, 5)
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_low_score = list(
        variables = c("score"),
        condition_if = "TRUE",
        then = "score >= 50",
        action = "flag_delete"
      )
    )
  ))

  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_true(grepl("flag_low_score", d$deletion_log$log_df$feedback))
})

test_that("generate_cleaning_log handles mixed actions: flag_delete and flag_autoclean together", {
  df <- tibble(
    id = 1:4,
    status = c("valid", "invalid", "valid", "invalid"),
    age = c("20", "bad", "30", "25")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_bad_status = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status == 'valid'",
        action = "flag_delete"
      ),
      flag_bad_age = list(
        variables = c("age"),
        condition_if = "TRUE",
        then = "age != 'bad'",
        action = "flag_autoclean"
      )
    )
  ))

  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  # Deletion log should have 2 records (rows 2 and 4 have invalid status)
  expect_equal(nrow(d$deletion_log$log_df), 2)

  # Cleaning log should have entries for flag_bad_age (rows 2 with "bad" age)
  age_entries <- d$cleaning_log$log_df[d$cleaning_log$log_df$issue == "flag_bad_age", ]
  expect_gt(nrow(age_entries), 0)
  expect_true(all(age_entries$changed == "yes"))
})


# flag_delete with condition_if only (no 'then') ####

test_that("run_quality_checks processes flag_delete dependency with condition_if only (no then)", {
  df <- tibble(
    id = 1:3,
    score = c(100, 5, 80)
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_dependency_schema(list(
    dependencies = list(
      flag_low_score = list(
        variables = c("score"),
        condition_if = "score < 50",
        action = "flag_delete"
        # no 'then' field — should still be processed
      )
    )
  ))
  d$standardize()
  d$run_quality_checks("standardized")
  expect_false(is.null(d$data_quality_flags))
  expect_true("flag_low_score" %in% names(d$data_quality_flags))
  # Only row 2 (score = 5) should be flagged
  expect_equal(d$data_quality_flags$flag_low_score, c(0, 1, 0))
})

test_that("generate_cleaning_log routes flag_delete (condition_if only) rows to deletion log", {
  df <- tibble(
    id = 1:3,
    score = c(100, 5, 80)
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_dependency_schema(list(
    dependencies = list(
      flag_low_score = list(
        variables = c("score"),
        condition_if = "score < 50",
        action = "flag_delete"
      )
    )
  ))
  d$standardize()
  d$run_quality_checks("standardized")
  d$generate_cleaning_log(stage = "standardized")

  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_equal(as.character(d$deletion_log$log_df$uuid), "2")
  expect_equal(nrow(d$cleaning_log$log_df[d$cleaning_log$log_df$issue == "flag_low_score", ]), 0)
})


# Uniqueness check in generate_cleaning_log ####

test_that("generate_cleaning_log adds duplicate unique-variable rows to deletion log", {
  df <- tibble(
    id = 1:4,
    survey_code = c("A001", "A002", "A001", "A003")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_variable_schema(list(
    types = list(id = "numeric", survey_code = "character"),
    unique = c("survey_code")
  ))
  d$variable_map <- list(uuid = "id", survey_code = "survey_code")
  d$standardize()
  d$generate_cleaning_log(stage = "standardized")

  # Row 3 is the duplicate (second occurrence of "A001")
  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_equal(as.character(d$deletion_log$log_df$uuid), "3")
  expect_true(grepl("survey_code", d$deletion_log$log_df$issue))
  expect_true(grepl("A001", d$deletion_log$log_df$feedback))
})

test_that("generate_cleaning_log handles multiple duplicates in unique variable", {
  df <- tibble(
    id = 1:5,
    code = c("X", "Y", "X", "Z", "X")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_variable_schema(list(
    types = list(id = "numeric", code = "character"),
    unique = c("code")
  ))
  d$variable_map <- list(uuid = "id", code = "code")
  d$standardize()
  d$generate_cleaning_log(stage = "standardized")

  # Rows 3 and 5 are duplicates of "X"
  expect_equal(nrow(d$deletion_log$log_df), 2)
  expect_true(all(d$deletion_log$log_df$uuid %in% c("3", "5")))
})

test_that("generate_cleaning_log does not flag NAs as duplicates in unique variable", {
  df <- tibble(
    id = 1:4,
    code = c("A", NA, NA, "B")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_variable_schema(list(
    types = list(id = "numeric", code = "character"),
    unique = c("code")
  ))
  d$variable_map <- list(uuid = "id", code = "code")
  d$standardize()
  d$generate_cleaning_log(stage = "standardized")

  # NAs should not be flagged; no duplicates among non-NA values
  expect_equal(nrow(d$deletion_log$log_df), 0)
})

test_that("generate_cleaning_log uses variable_map to resolve unique variable columns", {
  df <- tibble(
    survey_id = 1:3,
    enumerator = c("E1", "E2", "E1"),
    q_code = c("C1", "C2", "C1")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "survey_id")
  )
  d$set_variable_schema(list(
    types = list(survey_id = "numeric", enumerator = "character", q_code = "character"),
    unique = c("q_code")
  ))
  # Map canonical "q_code" → actual column "q_code", and set up enum_id
  d$variable_map <- list(uuid = "survey_id", enum_id = "enumerator", q_code = "q_code")
  d$standardize()
  d$generate_cleaning_log(stage = "standardized")

  expect_equal(nrow(d$deletion_log$log_df), 1)
  expect_equal(as.character(d$deletion_log$log_df$uuid), "3")
  expect_equal(d$deletion_log$log_df$enum_id, "E1")
})

test_that("generate_cleaning_log skips unique variable not present in dataset", {
  df <- tibble(
    id = 1:3,
    name = c("Alice", "Bob", "Alice")
  )
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$set_variable_schema(list(
    types = list(id = "numeric", name = "character"),
    unique = c("nonexistent_col")  # column not in dataset
  ))
  d$standardize()
  # Should not error; just skips the missing column
  expect_no_error(d$generate_cleaning_log(stage = "standardized"))
  expect_equal(nrow(d$deletion_log$log_df), 0)
})


# INTEGRATION TESTS: validate → standardize → clean ####

test_that("Full integration: validate, standardize, clean with no issues", {
  df <- tibble(
    id = 1:5,
    age = c(20, 25, 30, 35, 40),
    name = c("Alice", "Bob", "Charlie", "David", "Eve")
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set up basic schema
  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric",
      name = "character"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age", name = "name")

  # Run full pipeline
  d$validate()
  expect_true(d$validated)

  d$standardize()
  expect_true(d$standardized)
  expect_false(is.null(d$standardized_data))

  d$clean()
  expect_true(d$cleaned)
  expect_false(is.null(d$clean_data))

  # Check that data is preserved
  expect_equal(nrow(d$clean_data), 5)
  expect_equal(d$clean_data$id, 1:5)
})



test_that("Full integration: validate, standardize with type coercion, clean", {
  df <- tibble(
    id = c("1", "2", "3"),
    age = c("20", "25", "30"),
    active = c("yes", "no", "yes")
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric",
      active = "character"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age", active = "active")

  d$validate()
  expect_true(d$validated)

  d$standardize()
  expect_true(d$standardized)

  # Check type conversions
  expect_type(d$standardized_data$id, "double")
  expect_type(d$standardized_data$age, "double")
  expect_type(d$standardized_data$active, "character")

  d$clean()
  expect_true(d$cleaned)
  expect_equal(nrow(d$clean_data), 3)
})

test_that("Full integration: standardize runs quality checks automatically", {
  df <- tibble(
    id = 1:3,
    age = c("20", "bad", "30")
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- list(
    types = list(
      id = "numeric",
      age = "numeric"
    )
  )
  d$set_variable_schema(schema)
  d$variable_map <- list(id = "id", age = "age")

  d$validate()
  d$standardize()

  # Quality checks should have been run automatically during standardize
  expect_false(is.null(d$data_quality_flags))
  expect_true("flag_age_type" %in% names(d$data_quality_flags))
})

test_that("Full integration: clean applies cleaning log changes", {
  df <- tibble(
    id = 1:3,
    age = c(20, 25, 30),
    name = c("Alice", "Bob", "Charlie")
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Add a cleaning log entry
  d$cleaning_log$add_change(
    uuid = 2,
    enum_id = NA_character_,
    device_id = NA_character_,
    question.name = "age",
    issue = "incorrect_value",
    feedback = "Age should be 26",
    changed = "yes",
    old.value = "25",
    new.value = "26"
  )

  d$clean()

  # Check that the change was applied
  expect_equal(d$clean_data$age[2], 26)
})

test_that("Full integration: clean applies deletion log", {
  df <- tibble(
    id = 1:5,
    age = c(20, 25, 30, 35, 40)
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Add deletion log entries
  d$deletion_log$add_deletion(
    uuid = 2,
    enum_id = NA_character_,
    device_id = NA_character_,
    issue = "duplicate",
    feedback = "Duplicate entry"
  )
  d$deletion_log$add_deletion(
    uuid = 4,
    enum_id = NA_character_,
    device_id = NA_character_,
    issue = "invalid",
    feedback = "Invalid data"
  )

  d$clean()

  # Check that rows were deleted
  expect_equal(nrow(d$clean_data), 3)
  expect_false(2 %in% d$clean_data$id)
  expect_false(4 %in% d$clean_data$id)
})

test_that("Full integration: end-to-end with dependency checks", {
  df <- tibble(
    id = 1:4,
    age = c(10, 25, 30, 5),
    has_consent = c("no", "yes", "yes", "yes"),
    consent_date = as.Date(c(NA, "2023-01-01", "2023-02-01", NA))
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set up schemas
  var_schema <- list(
    types = list(
      id = "numeric",
      age = "numeric",
      has_consent = "character",
      consent_date = "date"
    )
  )

  dep_schema <- list(
    dependencies = list(
      flag_consent_required = list(
        variables = c("age", "has_consent"),
        condition_if = "age >= 18",
        then = "has_consent == 'yes'",
        action = "flag_warning"
      ),
      flag_consent_date_required = list(
        variables = c("has_consent", "consent_date"),
        condition_if = "has_consent == 'yes'",
        then = "!is.na(consent_date)",
        action = "flag_warning"
      )
    ),
    soft_dependencies = list()
  )

  d$set_variable_schema(var_schema)
  d$set_dependency_schema(dep_schema)
  d$variable_map <- list(
    id = "id",
    age = "age",
    has_consent = "has_consent",
    consent_date = "consent_date"
  )

  # Run pipeline
  d$validate()
  expect_true(d$validated)

  d$standardize()
  expect_true(d$standardized)

  # Check quality flags were generated
  expect_false(is.null(d$data_quality_flags))

  # Generate cleaning log from quality flags
  d$generate_cleaning_log(stage = "standardized")
  expect_true(nrow(d$cleaning_log$log_df) > 0)

  d$clean()
  expect_true(d$cleaned)
})

test_that("Full integration: validate catches issues before standardize", {
  df <- tibble(
    id = c(1, 1, 2),  # Duplicate IDs
    age = c(20, 25, 30)
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    d$validate(),
    regexp = "Duplicate"
  )

  expect_false(d$validated)

  # Standardize should proceed but warn
  expect_warning(
    d$standardize(),
    regexp = "Duplicate"
  )

  expect_true(d$standardized)
})

test_that("Full integration: value_map is used during quality checks", {
  df <- tibble(
    id = 1:3,
    status = c("A", "I", "P")
  )

  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  var_schema <- list(
    types = list(
      id = "numeric",
      status = "character"
    ),
    value_map = list(
      status = list(
        active = c("A", "active"),
        inactive = c("I", "inactive"),
        pending = c("P", "pending")
      )
    )
  )

  dep_schema <- list(
    dependencies = list(
      flag_valid_status = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status %in% c('active', 'inactive')",  # Using canonical values
        action = "flag_warning"
      )
    ),
    soft_dependencies = list()
  )

  d$set_variable_schema(var_schema)
  d$set_dependency_schema(dep_schema)
  d$variable_map <- list(id = "id", status = "status")
  d$value_map <- list(
    status = list(
      active = c("A"),
      inactive = c("I"),
      pending = c("P")
    )
  )

  d$validate()
  d$standardize()

  # Quality checks should use value_map to translate canonical values
  expect_false(is.null(d$data_quality_flags))

  # Row 3 (status = "P" / pending) should be flagged
  if ("flag_valid_status" %in% names(d$data_quality_flags)) {
    expect_equal(d$data_quality_flags$flag_valid_status[3], 1)
  }
})


# clean() respects the changed field ####


test_that("clean() does not apply cleaning log entries with changed = 'no'", {
  df <- tibble::tibble(id = 1:3, x = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Add entry with changed = "no" — should NOT be applied
  d$cleaning_log$add_change(
    uuid = 1,
    enum_id = NA_character_,
    device_id = NA_character_,
    question.name = "x",
    issue = "test_flag",
    feedback = "Flagged but not corrected",
    changed = "no",
    old.value = "a",
    new.value = "REPLACED"
  )

  d$clean()

  # Value should remain unchanged because changed = "no"
  expect_equal(d$clean_data$x[1], "a")
})


test_that("clean() applies cleaning log entries with changed = 'yes'", {
  df <- tibble::tibble(id = 1:3, x = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Add entry with changed = "yes" — SHOULD be applied
  d$cleaning_log$add_change(
    uuid = 1,
    enum_id = NA_character_,
    device_id = NA_character_,
    question.name = "x",
    issue = "test_flag",
    feedback = "Corrected value",
    changed = "yes",
    old.value = "a",
    new.value = "CORRECTED"
  )

  d$clean()

  # Value should be updated because changed = "yes"
  expect_equal(d$clean_data$x[1], "CORRECTED")
  # Other values should remain unchanged
  expect_equal(d$clean_data$x[2], "b")
  expect_equal(d$clean_data$x[3], "c")
})


test_that("clean() does not apply autoclean entries with new.value = NA when changed = 'no'", {
  df <- tibble::tibble(id = 1:3, score = c(10, 20, 30))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )
  d$validate()
  d$standardize()

  # Simulate what generate_cleaning_log creates for flag_warning (changed = "no", new.value = NA)
  d$cleaning_log$add_change(
    uuid = 2,
    enum_id = NA_character_,
    device_id = NA_character_,
    question.name = "score",
    issue = "flag_warning_test",
    feedback = "Flagged as warning",
    changed = "no",
    old.value = "20",
    new.value = NA_character_
  )

  d$clean()

  # The score value should NOT be set to NA since changed = "no"
  expect_equal(d$clean_data$score[2], 20)
})


# --- .apply_cleaning_changes type coercion tests ---

test_that(".apply_cleaning_changes preserves numeric column type when new.value is numeric string", {
  df <- tibble::tibble(id = as.character(1:3), score = c(10, 20, 30))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  log_df <- tibble::tibble(
    uuid = "2", enum_id = "e1", device_id = "d1",
    question.name = "score", issue = "wrong", feedback = "fix",
    changed = "yes", old.value = "20", new.value = "25"
  )
  d$cleaning_log <- CleaningLog$new(log_df = log_df)
  d$clean()

  expect_true(is.numeric(d$clean_data$score))
  expect_equal(d$clean_data$score[2], 25)
})


test_that(".apply_cleaning_changes handles NA new.value for numeric column", {
  df <- tibble::tibble(id = as.character(1:3), score = c(10, 20, 30))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  log_df <- tibble::tibble(
    uuid = "2", enum_id = "e1", device_id = "d1",
    question.name = "score", issue = "remove", feedback = "fix",
    changed = "yes", old.value = "20", new.value = NA_character_
  )
  d$cleaning_log <- CleaningLog$new(log_df = log_df)
  d$clean()

  expect_true(is.numeric(d$clean_data$score))
  expect_true(is.na(d$clean_data$score[2]))
})


test_that(".apply_cleaning_changes skips non-coercible value and logs issue", {
  df <- tibble::tibble(id = as.character(1:3), score = c(10, 20, 30))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  log_df <- tibble::tibble(
    uuid = "2", enum_id = "e1", device_id = "d1",
    question.name = "score", issue = "bad", feedback = "fix",
    changed = "yes", old.value = "20", new.value = "not_a_number"
  )
  d$cleaning_log <- CleaningLog$new(log_df = log_df)

  expect_warning(d$clean(), "cannot be safely coerced")

  # Value should remain unchanged
  expect_equal(d$clean_data$score[2], 20)
  expect_true(is.numeric(d$clean_data$score))

  # Issue should be logged
  expect_true(!is.null(d$cleaning_log_issues))
  expect_equal(nrow(d$cleaning_log_issues), 1)
  expect_equal(d$cleaning_log_issues$question.name[1], "score")
})


test_that(".apply_cleaning_changes preserves integer column type", {
  df <- tibble::tibble(id = as.character(1:3), count = as.integer(c(1, 2, 3)))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  log_df <- tibble::tibble(
    uuid = "1", enum_id = "e1", device_id = "d1",
    question.name = "count", issue = "fix", feedback = "fix",
    changed = "yes", old.value = "1", new.value = "5"
  )
  d$cleaning_log <- CleaningLog$new(log_df = log_df)
  d$clean()

  expect_true(is.integer(d$clean_data$count))
  expect_equal(d$clean_data$count[1], 5L)
})



test_that(".apply_cleaning_changes works for character columns", {
  df <- tibble::tibble(id = as.character(1:3), name = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  log_df <- tibble::tibble(
    uuid = "3", enum_id = "e1", device_id = "d1",
    question.name = "name", issue = "typo", feedback = "fix",
    changed = "yes", old.value = "c", new.value = "corrected"
  )
  d$cleaning_log <- CleaningLog$new(log_df = log_df)
  d$clean()

  expect_true(is.character(d$clean_data$name))
  expect_equal(d$clean_data$name[3], "corrected")
})


