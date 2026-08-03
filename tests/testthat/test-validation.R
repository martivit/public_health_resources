
# ============================================================================
# Test: Fix for select_multiple value validation
# ============================================================================

test_that("validate() correctly validates select_multiple mapped values", {
  # Create data with select_multiple column (space-separated values)
  df <- tibble::tibble(
    id = 1:5,
    livelihood = c(
      "farming fishing",
      "trading",
      "farming trading other",
      "fishing",
      "farming"
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema with select_multiple question type and nested value_map
  schema <- list(
    types = list(
      livelihood = "character"
    ),
    col_names = list(
      livelihood = c("livelihood", "income_source")
    ),
    question_types = list(
      livelihood = "select_multiple"
    ),
    value_map = list(
      livelihood = list(
        agriculture = c("farming", "agriculture", "crop"),
        fishing = c("fishing", "fishery"),
        business = c("trading", "business", "commerce"),
        other = c("other", "autre")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Verify that value map was built correctly
  expect_true("livelihood" %in% names(d$value_map))
  expect_true("agriculture" %in% names(d$value_map$livelihood))
  expect_true("farming" %in% d$value_map$livelihood$agriculture)
  expect_true("fishing" %in% d$value_map$livelihood$fishing)
  expect_true("trading" %in% d$value_map$livelihood$business)
  expect_true("other" %in% d$value_map$livelihood$other)

  # Run validate - should NOT produce warnings about missing values
  # since the mapped values ARE present (as tokens in space-separated strings)
  d$validate()

  # The validate() method should set validated to TRUE
  # because all mapped values are found as tokens in the data
  expect_true(d$validated)
})

test_that("data_diagnose() correctly diagnoses select_multiple mapped values", {
  # Create data with select_multiple column
  df <- tibble::tibble(
    id = 1:5,
    skills = c(
      "reading writing",
      "writing reading math",
      "math reading",
      "reading",
      "writing math"
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(
      skills = "character"
    ),
    col_names = list(
      skills = c("skills", "abilities")
    ),
    question_types = list(
      skills = "select_multiple"
    ),
    value_map = list(
      skills = list(
        literacy = c("reading", "writing"),
        numeracy = c("math", "mathematics")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Run data_diagnose
  diag <- d$data_diagnose()

  # Should not report any issues for mapped values that exist as tokens
  # Filter to literacy/reading and literacy/writing rows
  literacy_reading <- diag[diag$required_variable == "skills" &
                             diag$required_value == "literacy" &
                             grepl("reading", diag$mapped_value), ]
  literacy_writing <- diag[diag$required_variable == "skills" &
                             diag$required_value == "literacy" &
                             grepl("writing", diag$mapped_value), ]
  numeracy_math <- diag[diag$required_variable == "skills" &
                          diag$required_value == "numeracy" &
                          grepl("math", diag$mapped_value), ]

  # These should all be "ok" or not have "mapped values not in dataset" issue
  if (nrow(literacy_reading) > 0) {
    expect_false(grepl("mapped values not in dataset", literacy_reading$issues[1]))
  }
  if (nrow(literacy_writing) > 0) {
    expect_false(grepl("mapped values not in dataset", literacy_writing$issues[1]))
  }
  if (nrow(numeracy_math) > 0) {
    expect_false(grepl("mapped values not in dataset", numeracy_math$issues[1]))
  }

  # Should still report if a value is not in the data at all
  # "mathematics" is not in the data, so numeracy should have only "math"
  expect_false("mathematics" %in% d$value_map$skills$numeracy)
})

test_that("validate() correctly handles select_multiple with old allowed_values format", {
  # Test backward compatibility with old allowed_values structure
  df <- tibble::tibble(
    id = 1:3,
    transport = c("car bus", "bike", "car bike")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(
      transport = "character"
    ),
    col_names = list(
      transport = c("transport", "vehicle")
    ),
    question_types = list(
      transport = "select_multiple"
    ),
    allowed_values = list(
      transport = c("car", "bus", "bike", "walk", "taxi")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Run validate - should work correctly
  d$validate()

  # Should be validated successfully
  expect_true(d$validated)
})

test_that("validate() still catches truly missing values in select_multiple", {
  # Test that we still detect when a mapped value is NOT in the data
  df <- tibble::tibble(
    id = 1:3,
    food = c("rice beans", "rice", "beans")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Manually set up a value_map with a value that doesn't exist
  d$variable_map$food <- "food"
  d$value_map$food <- list(
    cereals = c("rice", "wheat"),  # "wheat" is NOT in the data
    legumes = c("beans", "lentils")  # "lentils" is NOT in the data
  )

  # Set minimal schema for the helper method to work
  d$variable_schema <- list(
    types = list(food = "character"),
    question_types = list(food = "select_multiple")
  )

  # Run validate - should produce warnings about missing values
  d$validate()

  # Should NOT be validated (has warnings)
  expect_false(d$validated)
})

test_that(".extract_select_multiple_tokens helper works correctly", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Test normal space-separated values
  tokens <- d$.extract_select_multiple_tokens(c("a b", "c d", "a"))
  expect_equal(sort(tokens), sort(c("a", "b", "c", "d")))

  # Test with extra spaces
  tokens <- d$.extract_select_multiple_tokens(c("a  b", " c ", "d"))
  expect_equal(sort(tokens), sort(c("a", "b", "c", "d")))

  # Test with NA and empty strings
  tokens <- d$.extract_select_multiple_tokens(c("a b", NA, "", "c"))
  expect_equal(sort(tokens), sort(c("a", "b", "c")))

  # Test uniqueness
  tokens <- d$.extract_select_multiple_tokens(c("a b", "b c", "a"))
  expect_equal(sort(tokens), sort(c("a", "b", "c")))
})

test_that(".is_select_multiple helper works correctly", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Without schema
  expect_false(d$.is_select_multiple("any_var"))

  # With schema but no question_types
  d$variable_schema <- list(
    types = list(var1 = "character")
  )
  expect_false(d$.is_select_multiple("var1"))

  # With schema and select_multiple question type
  d$variable_schema <- list(
    types = list(
      var1 = "character",
      var2 = "character"
    ),
    question_types = list(
      var1 = "select_multiple",
      var2 = "select_one"
    )
  )
  expect_true(d$.is_select_multiple("var1"))
  expect_false(d$.is_select_multiple("var2"))
  expect_false(d$.is_select_multiple("var3"))
})

# Tests for automatic stage selection in validate() method ####

test_that("validate() uses raw data when neither standardized nor cleaned", {
  df <- tibble::tibble(id = 1:3, x = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # No standardization or cleaning yet
  expect_false(d$standardized)
  expect_false(d$cleaned)

  # Should validate against raw data
  d$validate()

  # Validation should succeed
  expect_true(d$validated)
})

test_that("validate() uses standardized data when self$standardized is TRUE", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Standardize the data
  d$validate()
  d$standardize()

  expect_true(d$standardized)
  expect_false(d$cleaned)

  # Modify raw data to be invalid (should not affect validation)
  d$raw_data <- tibble::tibble(id = c(1, 1, 1), x = c("bad", "bad", "bad"))

  # Re-validate without specifying stage - should use standardized data
  d$validated <- FALSE  # Reset validation flag
  d$validate()

  # Should succeed because it validates against standardized data, not raw
  expect_true(d$validated)
})

test_that("validate() uses clean data when self$cleaned is TRUE", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Go through full pipeline
  d$validate()
  d$standardize()
  d$clean()

  expect_true(d$standardized)
  expect_true(d$cleaned)

  # Modify raw and standardized data to be invalid
  d$raw_data <- tibble::tibble(id = c(1, 1, 1), x = c("bad", "bad", "bad"))
  d$standardized_data <- tibble::tibble(id = c(2, 2, 2), x = c("bad", "bad", "bad"))

  # Re-validate without specifying stage - should use clean data
  d$validated <- FALSE  # Reset validation flag
  d$validate()

  # Should succeed because it validates against clean data
  expect_true(d$validated)
})

test_that("validate() respects explicit stage parameter over automatic selection", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Standardize and clean
  d$validate()
  d$standardize()
  d$clean()

  expect_true(d$standardized)
  expect_true(d$cleaned)

  # Explicitly validate raw stage - should use raw data even though cleaned is TRUE
  d$validated <- FALSE
  d$validate(stage = "raw")

  expect_true(d$validated)

  # Explicitly validate standardized stage
  d$validated <- FALSE
  d$validate(stage = "standardized")

  expect_true(d$validated)
})

test_that("validate() automatic selection works with invalid standardized data", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Standardize
  d$validate()
  d$standardize()

  # Corrupt standardized data with duplicate IDs
  d$standardized_data <- tibble::tibble(id = c(1, 1, 2), x = c("1", "2", "3"))

  # Re-validate - should fail because it uses standardized data
  d$validated <- FALSE
  expect_warning(
    d$validate(),
    regexp = "Duplicate"
  )

  expect_false(d$validated)
})

test_that("validate() automatic selection message indicates correct stage", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Test with raw data (default)
  expect_message(
    d$validate(),
    regexp = "Starting validation"
  )

  # Test with standardized data
  d$standardize()
  d$validated <- FALSE
  expect_message(
    d$validate(),
    regexp = "Starting validation"
  )

  # Test with clean data
  d$clean()
  d$validated <- FALSE
  expect_message(
    d$validate(),
    regexp = "Starting validation"
  )
})

test_that("validate() works correctly when cleaned flag is TRUE but clean_data is NULL", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set cleaned flag but don't actually have clean data (edge case)
  d$cleaned <- TRUE
  d$clean_data <- NULL

  # Validation should handle this gracefully
  # get_data("clean") will return NULL, which should be caught
  expect_error(
    d$validate(),
    regexp = "expected a valid object"
  )
})

test_that("validate() works correctly when standardized flag is TRUE but standardized_data is NULL", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set standardized flag but don't actually have standardized data (edge case)
  d$standardized <- TRUE
  d$standardized_data <- NULL

  # Validation should handle this gracefully
  # get_data("standardized") will return NULL, which should be caught
  expect_error(
    d$validate(),
    regexp = "expected a valid object"
  )
})

test_that("validate() preserves backward compatibility with explicit stage calls", {
  df <- tibble::tibble(id = 1:3, x = c("1", "2", "3"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # All explicit stage calls should still work as before
  d$validate(stage = "raw")
  expect_true(d$validated)

  d$standardize()
  d$validated <- FALSE
  d$validate(stage = "standardized")
  expect_true(d$validated)

  d$clean()
  d$validated <- FALSE
  d$validate(stage = "clean")
  expect_true(d$validated)
})


