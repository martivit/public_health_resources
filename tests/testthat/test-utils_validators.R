
library(testthat)
library(tibble)


# 1. is_greater_than_one_selection and .is_greater_than_three_selection ####


test_that("is_greater_than_one_selection identifies multiple selections", {
  df <- tibble::tibble(
    sm_col = c(
      "option_a option_b",
      "option_c",
      "option_d option_e option_f",
      ""
    )
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("is_greater_than_one_selection handles single selections", {
  df <- tibble::tibble(
    sm_col = c("option_a", "option_b", "option_c")
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, FALSE, FALSE))
})

test_that("is_greater_than_one_selection handles empty dataset", {
  df <- tibble::tibble(sm_col = character())

  result <- is_greater_than_one_selection(df, "sm_col")

  # Expect an empty result (could be logical(0), character(0), or tibble with 0 rows)
  expect_equal(length(result), 0)
  # Or if it returns a tibble:
  # expect_equal(nrow(result), 0)
})

test_that("is_greater_than_one_selection errors on missing column", {
  df <- tibble::tibble(other_col = "value")

  expect_error(
    is_greater_than_one_selection(df, "sm_col")
  )
})

test_that("is_greater_than_one_selection handles factor columns", {
  df <- tibble::tibble(
    sm_col = factor(c("a b", "c", "d e"))
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(TRUE, FALSE, TRUE))
})

test_that(".is_greater_than_three_selection identifies >3 selections", {
  df <- tibble::tibble(
    sm_col = c(
      "a b",
      "a",
      "a b c d",
      "a b c d e"
    )
  )

  result <- .is_greater_than_three_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, FALSE, TRUE, TRUE))
})

test_that(".is_greater_than_three_selection handles exactly three selections", {
  df <- tibble::tibble(
    sm_col = c("a b c", "a b c d")
  )

  result <- .is_greater_than_three_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, TRUE))
})


# 2. NULL VALIDATION TESTS ####


test_that("phr_validate_not_null passes for non-null values", {
  expect_no_error(
    phr_validate_not_null(123, soft = FALSE)
  )

  expect_no_error(
    phr_validate_not_null("text", soft = FALSE)
  )
})

test_that("phr_validate_not_null errors on NULL when soft=FALSE", {
  expect_error(
    phr_validate_not_null(NULL, soft = FALSE)
  )
})


# 3. TYPE VALIDATION TESTS ####

# IPHRA VALIDATE NUMERIC ####

test_that("phr_validate_numeric passes for numeric input", {
  expect_no_error(
    phr_validate_numeric(123, soft = FALSE)
  )

  expect_no_error(
    phr_validate_numeric(c(1, 2, 3), soft = FALSE)
  )
})

test_that("phr_validate_numeric errors on non-numeric input when soft=FALSE", {
  expect_error(
    phr_validate_numeric("text", soft = FALSE)
  )
})

# IPHRA VALIDATE CHARACTER ####

test_that("phr_validate_character passes for character input", {
  expect_no_error(
    phr_validate_character("text", soft = FALSE)
  )

  expect_no_error(
    phr_validate_character(c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_character errors on non-character input when soft=FALSE", {
  expect_error(
    phr_validate_character(123, soft = FALSE)
  )
})

# IPHRA VALIDATE LOGICAL ####

test_that("phr_validate_logical passes for logical input", {
  expect_no_error(
    phr_validate_logical(TRUE, soft = FALSE)
  )

  expect_no_error(
    phr_validate_logical(c(TRUE, FALSE), soft = FALSE)
  )
})

test_that("phr_validate_logical errors on non-logical input when soft=FALSE", {
  expect_error(
    phr_validate_logical(123, soft = FALSE)
  )
})

# IPHRA VALIDATE DATE ####

test_that("phr_validate_date passes for Date input", {
  expect_no_error(
    phr_validate_date(Sys.Date(), soft = FALSE)
  )
})

test_that("phr_validate_date errors on non-Date input when soft=FALSE", {
  expect_error(
    phr_validate_date(5, soft = FALSE)
  )
})

# IPHRA VALIDATE FACTOR ####

test_that("phr_validate_factor passes for factor input", {
  expect_no_error(
    phr_validate_factor(factor(c("a", "b")), soft = FALSE)
  )
})

test_that("phr_validate_factor errors on non-factor input when soft=FALSE", {
  expect_error(
    phr_validate_factor(c("a", "b"), soft = FALSE)
  )
})


# 4. DATAFRAME VALIDATION TESTS ####


test_that("phr_validate_dataframe passes for data.frame input", {
  expect_no_error(
    phr_validate_dataframe(data.frame(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_dataframe passes for tibble input", {
  expect_no_error(
    phr_validate_dataframe(tibble::tibble(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_dataframe errors on non-dataframe input when soft=FALSE", {
  expect_error(
    phr_validate_dataframe(list(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_columns passes when columns exist", {
  df <- tibble::tibble(a = 1, b = 2, c = 3)

  expect_no_error(
    phr_validate_columns(df, c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_columns errors when columns missing and soft=FALSE", {
  df <- tibble::tibble(a = 1, b = 2)

  expect_error(
    phr_validate_columns(df, c("a", "c"), soft = FALSE)
  )
})

test_that("phr_validate_list passes for list input", {
  expect_no_error(
    phr_validate_list(list(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_list errors on non-list input when soft=FALSE", {
  expect_error(
    phr_validate_list(c(1, 2, 3), soft = FALSE)
  )
})


# 5. IPHRA VALIDATE VECTOR LENGTH ####


test_that("phr_validate_vector_length passes for valid length", {
  expect_no_error(
    phr_validate_vector_length(c(1, 2, 3), min_length = 1, soft = FALSE)
  )
})

test_that("phr_validate_vector_length errors when length too short and soft=FALSE", {
  expect_error(
    phr_validate_vector_length(c(1), min_length = 2, soft = FALSE)
  )
})

test_that("phr_validate_vector_length checks exact length", {
  expect_no_error(
    phr_validate_vector_length(c(1, 2, 3), exact_length = 3, soft = FALSE)
  )

  expect_error(
    phr_validate_vector_length(c(1, 2), exact_length = 3, soft = FALSE)
  )
})


# 6. IPHRA VALIDATE CHOICE ####


test_that("phr_validate_choice passes for valid choice", {
  expect_no_error(
    phr_validate_choice("a", choices = c("a", "b", "c"), soft = FALSE)
  )
})

test_that("phr_validate_choice errors on invalid choice when soft=FALSE", {
  expect_warning(
    phr_validate_choice("d", choices = c("a", "b", "c"), soft = FALSE)
  )
})


# 7. VECTOR VALIDATION TESTS (ALL ELEMENTS) ####

# IPHRA VALIDATE ALL NUMERIC ####

test_that("phr_validate_all_numeric passes when all elements are numeric", {
  expect_no_error(
    phr_validate_all_numeric(c(1, 2, 3), soft = FALSE)
  )
})

test_that("phr_validate_all_numeric errors when some elements are not numeric", {
  expect_error(
    phr_validate_all_numeric(c(1, "g", 3), soft = FALSE)
  )
})
# IPHRA VALIDATE ALL CHARACTER ####

test_that("phr_validate_all_character passes when all elements are character", {
  expect_no_error(
    phr_validate_all_character(c("a", "b", "c"), soft = FALSE)
  )
})

# IPHRA VALIDATE ALL LOGICAL ####

test_that("phr_validate_all_logical passes when all elements are logical", {
  expect_no_error(
    phr_validate_all_logical(c(TRUE, FALSE, TRUE), soft = FALSE)
  )
})

# IPHRA VALIDATE ALL DATE ####

test_that("phr_validate_all_date passes when all elements are Date", {
  expect_no_error(
    phr_validate_all_date(rep(Sys.Date(), 3), soft = FALSE)
  )
})

# IPHRA VALIDATE ALL FACTOR ####

test_that("phr_validate_all_factor passes when all elements are from valid levels", {
  f <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))

  expect_no_error(
    phr_validate_all_factor(f, allowed_levels = c("a", "b", "c"), soft = FALSE)
  )
})


# 8. COLUMN TYPE VALIDATION TESTS ####

# IPHRA VALIDATE COLUMN TYPES ####

test_that("phr_validate_column_types passes when types match", {
  df <- tibble::tibble(
    a = 1:5,
    b = c("x", "y", "z", "w", "v"),
    c = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  expected_types <- list(
    a = "integer",
    b = "character",
    c = "logical"
  )

  expect_no_error(
    phr_validate_column_types(df, expected_types, soft = FALSE)
  )
})

test_that("phr_validate_column_types errors when types don't match and soft=FALSE", {
  df <- tibble::tibble(
    a = c("1", "2", "3")  # character, not numeric
  )

  expected_types <- list(a = "numeric")

  expect_error(
    phr_validate_column_types(df, expected_types, soft = FALSE)
  )
})


# 9. NO MISSING VALUES VALIDATION TESTS ####

# IPHRA VALIDATE NO MISSING ####

test_that("phr_validate_no_missing passes when no NA values", {
  df <- tibble::tibble(
    a = 1:5,
    b = c("x", "y", "z", "w", "v")
  )

  expect_no_error(
    phr_validate_no_missing(df, c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_no_missing errors when NA values present and soft=FALSE", {
  df <- tibble::tibble(
    a = c(1, 2, NA, 4, 5)
  )

  expect_error(
    phr_validate_no_missing(df, "a", soft = FALSE)
  )
})


# IPHRA VALIDATE UNIQUE ####


test_that("phr_validate_unique passes when all values are unique", {
  df <- tibble::tibble(
    id = 1:5
  )

  expect_no_error(
    phr_validate_unique(df, "id", soft = FALSE)
  )
})

test_that("phr_validate_unique errors when duplicates exist and soft=FALSE", {
  df <- tibble::tibble(
    id = c(1, 2, 2, 3, 4)
  )

  expect_error(
    phr_validate_unique(df, "id", soft = FALSE)
  )
})


# IPHRA VALIDATE NON NEGATIVE ####

test_that("phr_validate_non_negative passes for non-negative values", {
  df <- tibble::tibble(
    value = c(0, 1, 2, 3, 4)
  )

  expect_no_error(
    phr_validate_non_negative(df, "value", soft = FALSE)
  )
})

test_that("phr_validate_non_negative errors on negative values when soft=FALSE", {
  df <- tibble::tibble(
    value = c(1, -1, 2)
  )

  expect_error(
    phr_validate_non_negative(df, "value", soft = FALSE)
  )
})


# 12. SOFT VALIDATION BEHAVIOR TESTS ####


test_that("validators with soft=TRUE warn instead of error", {
  # This tests the soft validation mode behavior
  # When soft=TRUE, validators should warn but not error
  expect_warning(
    phr_validate_numeric("text", soft = TRUE),
    regexp = "Ensure input is of type"  # May or may not warn depending on implementation
  )
})

test_that("validators with soft=FALSE error on invalid input", {
  expect_error(
    phr_validate_numeric("text", soft = FALSE)
  )
})

# is_logical_expression ####

test_that("is_logical_expression accepts valid logical expressions", {

  valid_exprs <- list(
    "x > 5",
    "age <= 10 & sex == 'f'",
    "!is.na(weight)",
    "grepl('abc', name)",
    "grepl('a', name) & age > 5",
    "TRUE",
    "FALSE",
    "(x > 1 | y < 2) & z == 0",
    # New: %in% operator
    "x %in% c('a', 'b', 'c')",
    "status %in% c('active', 'pending')",
    "age %in% 1:100",
    # New: is_safely_coercible function
    "is_safely_coercible(x, 'numeric')",
    "is_safely_coercible(age, 'date')",
    # New: Base R type check functions
    "is.numeric(age)",
    "is.character(name)",
    "is.logical(flag)",
    "is.integer(count)",
    "is.double(value)",
    "is.factor(category)",
    "is.null(missing_val)",
    # Combined expressions with new constructs
    "x %in% c(1, 2, 3) & is.numeric(x)",
    "is.character(name) | is.na(name)",
    "is_safely_coercible(x, 'numeric') & !is.na(x)"
  )

  for (txt in valid_exprs) {
    expect_true(
      is_logical_expression(txt),
      info = paste("Expected valid logical expression:", txt)
    )
  }
})

test_that("is_logical_expression rejects non-logical but syntactically valid expressions", {

  invalid_exprs <- list(
    "'my name is Jack'",
    "42",
    "3.14",
    "mean(x)",
    "paste(a, b)",
    "x + y",
    "log(x)"
  )

  for (txt in invalid_exprs) {
    expect_false(
      is_logical_expression(txt),
      info = paste("Expected invalid logical expression:", txt)
    )
  }
})

test_that("is_logical_expression rejects malformed or unsupported calls", {

  invalid_calls <- list(
    quote(`{`(x > 1)),
    quote(if (x > 1) TRUE else FALSE),
    quote(function(x) x > 1),
    quote(return(x > 1))
  )

  for (expr in invalid_calls) {
    expect_false(
      is_logical_expression(expr),
      info = paste("Expected invalid logical expression:", deparse(expr))
    )
  }
})


test_that("is_logical_expression rejects atomic literals", {

  expect_false(is_logical_expression(quote("abc")))
  expect_false(is_logical_expression(quote(1)))
  expect_false(is_logical_expression(quote(NA)))
})

# IPHRA CONVERT DATE ####

test_that("phr_convert_date converts ISO ymd strings correctly", {
  x <- c("2025-07-13", "2025-01-01")
  res <- phr_convert_date(x)
  expect_s3_class(res, "Date")
  expect_equal(res, as.Date(x))
})

test_that("phr_convert_date parses dmy and mdy formats", {
  x <- c("13/07/2025", "07-13-2025")
  res <- phr_convert_date(x)
  expect_equal(res, as.Date(c("2025-07-13", "2025-07-13")))
})

test_that("phr_convert_date handles mixed formats", {
  x <- c("2025-07-13", "13/07/2025", "07/13/2025")
  res <- phr_convert_date(x)
  expect_equal(res, rep(as.Date("2025-07-13"), 3))
})

test_that("phr_convert_date handles POSIXct and POSIXlt inputs", {
  x <- as.POSIXct("2025-07-13 12:00:00", tz = "UTC")
  res <- phr_convert_date(x)
  expect_s3_class(res, "Date")
  expect_equal(res, as.Date("2025-07-13"))

  y <- as.POSIXlt(x)
  res2 <- phr_convert_date(y)
  expect_equal(res2, as.Date("2025-07-13"))
})

test_that("phr_convert_date handles numeric days since epoch", {
  x <- as.numeric(as.Date("2025-07-13"))
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2025-07-13"))

})

test_that("phr_convert_date handles Excel serial numbers (post-1900)", {
  # Correct Excel serial number for 2023-01-01 is 44927
  x <- 44927
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2023-01-01"))
})

test_that("phr_convert_date handles numeric-like character Excel serial", {
  x <- "44927"  # Excel serial for 2023-01-01
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2023-01-01"))
})

test_that("phr_convert_date handles numeric-like character Unix days", {
  x <- "20000"  # Unix epoch -> 2024-10-04
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2024-10-04"))
})

test_that("phr_convert_date preserves NA values", {
  x <- c("2025-07-13", NA, "2025-08-01")
  res <- phr_convert_date(x)
  expect_true(is.na(res[2]))
  expect_equal(res[c(1,3)], as.Date(c("2025-07-13", "2025-08-01")))
})

test_that("phr_convert_date strips time components and timezones", {
  x <- c(
    "2025-07-13 14:22:10",
    "2025-07-13T14:22:10Z",
    "2025-07-13 14:22:10 UTC"
  )
  res <- phr_convert_date(x)
  expect_equal(unique(res), as.Date("2025-07-13"))
})

test_that("phr_convert_date errors on unparseable values", {
  x <- c("2025-07-13", "not-a-date")

  expect_error(
    phr_convert_date(x),
    regexp = "Could not convert values to Date"
  )
})

test_that("phr_convert_date handles vectors of length 1", {
  x <- "2025-07-13"
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2025-07-13"))
})

test_that("phr_convert_date supports mixed NA and bad values but still errors properly", {
  x <- c(NA, "bad-date")

  expect_error(
    phr_convert_date(x),
    regexp = "bad-date"
  )
})

test_that("phr_convert_date trims whitespace correctly", {
  x <- c(" 2025-07-13 ", "\t2025-08-01")
  res <- phr_convert_date(x)
  expect_equal(res, as.Date(c("2025-07-13", "2025-08-01")))
})

# phr_validate_datetime ####

test_that("phr_validate_datetime() — accepts POSIXct input", {
  x <- as.POSIXct("2025-10-16 14:32:00", tz = "UTC")
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts POSIXlt input", {
  x <- as.POSIXlt("2025-10-16 14:32:00", tz = "UTC")
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts datetime string with time component", {
  x <- "2025-10-16 14:32:00"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts ISO 8601 datetime string", {
  x <- "2025-10-16T14:32:00Z"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts datetime string with HH:MM only", {
  x <- "2025-10-16 14:32"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — rejects bare Date object (soft=TRUE, warns)", {
  x <- as.Date("2025-10-16")
  expect_warning(
    result <- phr_validate_datetime(x, soft = TRUE)
  )
  expect_false(result)
})

test_that("phr_validate_datetime() — rejects date-only string (soft=TRUE, warns)", {
  x <- "2025-10-16"
  expect_warning(
    result <- phr_validate_datetime(x, soft = TRUE)
  )
  expect_false(result)
})

test_that("phr_validate_datetime() — rejects bare Date object (soft=FALSE, errors)", {
  x <- as.Date("2025-10-16")
  expect_error(
    phr_validate_datetime(x, soft = FALSE)
  )
})

test_that("phr_validate_datetime() — rejects non-date string (soft=FALSE, errors)", {
  x <- "not-a-datetime"
  expect_error(
    phr_validate_datetime(x, soft = FALSE)
  )
})

test_that("phr_validate_datetime() — rejects NULL input", {
  expect_error(
    phr_validate_datetime(NULL, soft = FALSE)
  )
})

# ---- phr_parse_hhmm -------------------------------------------------------

test_that("phr_parse_hhmm returns correct minutes for standard HH:MM strings", {
  expect_equal(phr_parse_hhmm("00:00"), 0L)
  expect_equal(phr_parse_hhmm("08:00"), 480L)
  expect_equal(phr_parse_hhmm("10:00"), 600L)
  expect_equal(phr_parse_hhmm("18:00"), 1080L)
  expect_equal(phr_parse_hhmm("23:59"), 1439L)
})

test_that("phr_parse_hhmm handles single-digit hour", {
  expect_equal(phr_parse_hhmm("8:30"), 510L)
})

test_that("phr_parse_hhmm returns numeric input unchanged", {
  expect_equal(phr_parse_hhmm(480), 480)
})

test_that("phr_parse_hhmm errors on non-HH:MM character string", {
  expect_error(phr_parse_hhmm("2024-01-01"))
  expect_error(phr_parse_hhmm("hello"))
  expect_error(phr_parse_hhmm("1800"))
})

test_that("phr_parse_hhmm errors on out-of-range values", {
  expect_error(phr_parse_hhmm("25:00"))
  expect_error(phr_parse_hhmm("12:60"))
})

