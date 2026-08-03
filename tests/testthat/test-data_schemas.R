# Tests for Data class indicator schema functionality

library(testthat)
library(tibble)
library(withr)

# Data class indicator schema field ####

test_that("Data class has indicator_schema field", {

  df <- tibble::tibble(id = 1:3, value = c(10, 20, 30))

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "Test", uuid = "id")
  )

  expect_true("indicator_schema" %in% names(d))
  expect_null(d$indicator_schema)
})





# Indicator processing in standardize ####

test_that("Data class processes indicator schema during standardize", {

  # Create a mock add_ function for testing
  add_test_indicator <- function(.dataset, multiplier = "2") {
    mult <- as.numeric(multiplier)
    .dataset$result <- .dataset$value * mult
    return(.dataset)
  }

  # Make function available
  assign("add_test_indicator", add_test_indicator, envir = .GlobalEnv)
  withr::defer(rm(add_test_indicator, envir = .GlobalEnv))

  # Create test data
  df <- tibble::tibble(
    id = 1:3,
    value = c(10, 20, 30)
  )

  # Create indicator schema
  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test_indicator",
      variables = c("test_value"),  # Canonical name
      arguments = list(multiplier = "3")
    )
  )

  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestInd",
      uuid = "id",
      variable_map = list(uuid = "id", test_value = "value")  # Map canonical to actual
    )
  )
  d$set_indicator_schema(indicator_schema)

  expect_no_error(d$standardize())
  expect_true(d$standardized)
  expect_true("result" %in% names(d$standardized_data))
  expect_equal(d$standardized_data$result, c(30, 60, 90))
})

test_that("Data class resolves variable_map in indicator arguments", {

  # Create a mock add_ function
  add_test_var_map <- function(.dataset, input_col = "value") {
    .dataset$output <- .dataset[[input_col]] * 2
    return(.dataset)
  }

  assign("add_test_var_map", add_test_var_map, envir = .GlobalEnv)
  withr::defer(rm(add_test_var_map, envir = .GlobalEnv))

  df <- tibble::tibble(
    id = 1:3,
    my_value = c(5, 10, 15)
  )

  indicator_schema <- list(
    var_map_ind = list(
      indicator_name = "var_map_ind",
      function_name = "add_test_var_map",
      variables = c("data_col"),  # Canonical name
      arguments = list(input_col = "@variable_map$data_col")
    )
  )

  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestVarMap",
      uuid = "id",
      variable_map = list(uuid = "id", data_col = "my_value")
    )
  )
  d$set_indicator_schema(indicator_schema)

  expect_no_error(d$standardize())
  expect_true("output" %in% names(d$standardized_data))
  expect_equal(d$standardized_data$output, c(10, 20, 30))
})

test_that("Data class handles missing indicator function gracefully", {

  df <- tibble::tibble(id = 1:3, value = c(1, 2, 3))

  indicator_schema <- list(
    bad_ind = list(
      indicator_name = "bad_ind",
      function_name = "add_nonexistent_function",
      variables = c("value"),
      arguments = list()
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestMissing", uuid = "id")
  )
  d$set_indicator_schema(indicator_schema)

  # Should warn but not fail
  expect_warning(d$standardize(), regexp = "not found")
  expect_true(d$standardized)
})

test_that("Data class skips indicator with missing function name", {

  df <- tibble::tibble(id = 1:3, value = c(1, 2, 3))

  indicator_schema <- list(
    no_func_ind = list(
      indicator_name = "no_func_ind",
      function_name = "",
      variables = c("value"),
      arguments = list()
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestNoFunc", uuid = "id")
  )
  d$set_indicator_schema(indicator_schema)

  # Should warn and skip
  expect_warning(d$standardize(), regexp = "no function_name")
  expect_true(d$standardized)
})

test_that("Data class skips indicator when required variables are not mapped", {

  # Create a mock add_ function
  add_test_vars <- function(.dataset, var1 = "a", var2 = "b") {
    .dataset$result <- .dataset[[var1]] + .dataset[[var2]]
    return(.dataset)
  }

  assign("add_test_vars", add_test_vars, envir = .GlobalEnv)
  withr::defer(rm(add_test_vars, envir = .GlobalEnv))

  # Data only has 'id' and 'value', not 'a' or 'b'
  df <- tibble::tibble(id = 1:3, value = c(1, 2, 3))

  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test_vars",
      variables = c("canonical_a", "canonical_b"),  # Canonical names that aren't mapped
      arguments = list(var1 = "a", var2 = "b")
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestMissingVars", uuid = "id")
  )
  d$set_indicator_schema(indicator_schema)

  # Should warn and skip indicator due to missing variable map entries
  expect_warning(d$standardize(), regexp = "not mapped in variable_map")
  expect_true(d$standardized)

  # Result column should NOT be created
  expect_false("result" %in% names(d$standardized_data))
})

test_that("Data class uses variable_map to resolve canonical names in indicator variables", {

  # Create a mock add_ function that uses canonical variable references
  add_wash_indicator <- function(.dataset, plans_col = "wash_hwise_plans", drink_col = "wash_hwise_drink") {
    # Simple indicator that combines two columns
    .dataset$wash_indicator <- paste(.dataset[[plans_col]], .dataset[[drink_col]], sep = "_")
    return(.dataset)
  }

  assign("add_wash_indicator", add_wash_indicator, envir = .GlobalEnv)
  withr::defer(rm(add_wash_indicator, envir = .GlobalEnv))

  # Data has actual column names (with typo/variation)
  df <- tibble::tibble(
    id = 1:3,
    wash_wise_plans = c("never", "rarely", "sometimes"),  # Note: "wise" not "hwise"
    wash_wise_drink = c("never", "sometimes", "always")
  )

  # Indicator schema uses canonical variable names
  indicator_schema <- list(
    wash_ind = list(
      indicator_name = "wash_ind",
      function_name = "add_wash_indicator",
      variables = c("wash_hwise_plans", "wash_hwise_drink"),  # Canonical names
      arguments = list(
        plans_col = "@variable_map$wash_hwise_plans",
        drink_col = "@variable_map$wash_hwise_drink"
      )
    )
  )

  # variable_map provides mapping from canonical to actual column names
  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestWASH",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        wash_hwise_plans = "wash_wise_plans",  # Map canonical to actual
        wash_hwise_drink = "wash_wise_drink"
      )
    )
  )
  d$set_indicator_schema(indicator_schema)

  # Should succeed - indicator should find mapped columns
  expect_no_error(d$standardize())
  expect_true(d$standardized)

  # Indicator should have been computed
  expect_true("wash_indicator" %in% names(d$standardized_data))
  expect_equal(d$standardized_data$wash_indicator, c("never_never", "rarely_sometimes", "sometimes_always"))
})

test_that("Data class skips indicator when mapped columns don't exist in dataset", {

  # Create a mock add_ function
  add_test_indicator <- function(.dataset, col1 = "col1") {
    .dataset$result <- .dataset[[col1]] * 2
    return(.dataset)
  }

  assign("add_test_indicator", add_test_indicator, envir = .GlobalEnv)
  withr::defer(rm(add_test_indicator, envir = .GlobalEnv))

  # Data has different columns than what's mapped
  df <- tibble::tibble(id = 1:3, value = c(10, 20, 30))

  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test_indicator",
      variables = c("canonical_var"),  # Canonical name
      arguments = list(col1 = "@variable_map$canonical_var")
    )
  )

  # variable_map maps to a column that doesn't exist in the dataset
  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestMissingCol",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        canonical_var = "nonexistent_column"  # This column doesn't exist in df
      )
    )
  )
  d$set_indicator_schema(indicator_schema)

  # Should warn and skip indicator - mapped column not in dataset
  expect_warning(d$standardize(), regexp = "columns not present in dataset")
  expect_true(d$standardized)

  # Result column should NOT be created
  expect_false("result" %in% names(d$standardized_data))
})

test_that("Multiple indicators are executed in order with proper variable mapping", {

  # Create two mock add_ functions
  add_step1 <- function(.dataset, input = "value") {
    .dataset$step1_result <- .dataset[[input]] * 2
    return(.dataset)
  }

  add_step2 <- function(.dataset, input = "step1_result") {
    .dataset$step2_result <- .dataset[[input]] + 10
    return(.dataset)
  }

  assign("add_step1", add_step1, envir = .GlobalEnv)
  assign("add_step2", add_step2, envir = .GlobalEnv)
  withr::defer({
    rm(add_step1, add_step2, envir = .GlobalEnv)
  })

  # Create variable schema so that step1_result gets mapped automatically
  variable_schema <- list(
    types = list(
      uuid = "character",
      input_value = "numeric",
      step1_output = "numeric"
    ),
    col_names = list(
      uuid = c("id"),
      input_value = c("value"),
      step1_output = c("step1_result")  # Map canonical step1_output to actual step1_result
    )
  )

  indicator_schema <- list(
    indicator1 = list(
      indicator_name = "indicator1",
      function_name = "add_step1",
      variables = c("input_value"),  # Canonical name
      arguments = list(input = "value")
    ),
    indicator2 = list(
      indicator_name = "indicator2",
      function_name = "add_step2",
      variables = c("step1_output"),  # Canonical name - will be mapped after indicator1 runs
      arguments = list(input = "step1_result")
    )
  )

  test_data <- tibble::tibble(
    id = as.character(1:3),
    value = c(5, 10, 15)
  )

  d <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestMulti",
      uuid = "id",
      variable_map = list(uuid = "id", input_value = "value")
    )
  )
  d$set_variable_schema(variable_schema)
  d$set_indicator_schema(indicator_schema)

  expect_no_error(d$standardize())

  # Check both indicators were executed
  expect_true("step1_result" %in% names(d$standardized_data))
  expect_true("step2_result" %in% names(d$standardized_data))

  # Verify calculations (step1: value * 2, step2: step1 + 10)
  expect_equal(d$standardized_data$step1_result, c(10, 20, 30))
  expect_equal(d$standardized_data$step2_result, c(20, 30, 40))

  # Verify that step1_output was mapped after first indicator
  expect_equal(d$variable_map$step1_output, "step1_result")
})

test_that("indicator_schema_to_table converts basic indicator schema", {

  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test",
      variables = c("var1", "var2"),
      arguments = list(param1 = "value1", param2 = "value2"),
      label = "Test Indicator",
      comment = "A test indicator"
    )
  )

  tab <- indicator_schema_to_table(indicator_schema)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 1)
  expect_equal(tab$indicator_name, "test_ind")
  expect_equal(tab$function_name, "add_test")
  expect_equal(tab$variables, "var1,var2")
  expect_equal(tab$arguments, "param1=value1,param2=value2")
  expect_equal(tab$label, "Test Indicator")
  expect_equal(tab$comment, "A test indicator")
})

test_that("indicator_schema_to_table handles empty schema", {

  indicator_schema <- list()

  tab <- indicator_schema_to_table(indicator_schema)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 0)
  expect_true("indicator_name" %in% names(tab))
  expect_true("function_name" %in% names(tab))
})

test_that("indicator_schema_to_table handles multiple indicators", {

  indicator_schema <- list(
    ind1 = list(
      indicator_name = "ind1",
      function_name = "add_ind1",
      variables = c("x"),
      arguments = list(a = "1")
    ),
    ind2 = list(
      indicator_name = "ind2",
      function_name = "add_ind2",
      variables = c("y", "z"),
      arguments = list()
    )
  )

  tab <- indicator_schema_to_table(indicator_schema)

  expect_equal(nrow(tab), 2)
  expect_setequal(tab$indicator_name, c("ind1", "ind2"))
})


# INDICATOR_TABLE_TO_SCHEMA Testing ####

test_that("indicator_table_to_schema parses basic indicator table", {

  df <- tibble::tibble(
    indicator_name = "test_ind",
    function_name = "add_test",
    variables = "var1,var2",
    arguments = "param1=value1,param2=value2",
    label = "Test Indicator",
    comment = "A test comment"
  )

  schema <- indicator_table_to_schema(df)

  expect_true(is.list(schema))
  expect_equal(length(schema), 1)
  expect_true("test_ind" %in% names(schema))

  ind <- schema$test_ind
  expect_equal(ind$indicator_name, "test_ind")
  expect_equal(ind$function_name, "add_test")
  expect_equal(ind$variables, c("var1", "var2"))
  expect_equal(ind$arguments$param1, "value1")
  expect_equal(ind$arguments$param2, "value2")
  expect_equal(ind$label, "Test Indicator")
  expect_equal(ind$comment, "A test comment")
})

test_that("indicator_table_to_schema handles empty arguments", {

  df <- tibble::tibble(
    indicator_name = "test_ind",
    function_name = "add_test",
    variables = "var1",
    arguments = NA,
    label = NA,
    comment = NA
  )

  schema <- indicator_table_to_schema(df)

  expect_equal(length(schema$test_ind$arguments), 0)
  expect_null(schema$test_ind$label)
  expect_null(schema$test_ind$comment)
})

test_that("indicator_table_to_schema handles multiple indicators", {

  df <- tibble::tibble(
    indicator_name = c("ind1", "ind2"),
    function_name = c("add_ind1", "add_ind2"),
    variables = c("x", "y,z"),
    arguments = c("a=1", "b=2,c=3"),
    label = c(NA, "Indicator 2"),
    comment = c(NA, NA)
  )

  schema <- indicator_table_to_schema(df)

  expect_equal(length(schema), 2)
  expect_true("ind1" %in% names(schema))
  expect_true("ind2" %in% names(schema))
  expect_equal(schema$ind2$variables, c("y", "z"))
})


# Round-trip Testing ####

test_that("indicator schema round-trips correctly", {

  original_schema <- list(
    fcs_ind = list(
      indicator_name = "fcs_ind",
      function_name = "add_fcs",
      variables = c("fsl_fcs_cereal", "fsl_fcs_legumes"),
      arguments = list(
        cutoffs = "normal",
        fsl_fcs_cereal = "@variable_map$fsl_fcs_cereal"
      ),
      label = "FCS Indicator",
      comment = "Food Consumption Score"
    ),
    hhs_ind = list(
      indicator_name = "hhs_ind",
      function_name = "add_hhs",
      variables = c("fsl_hhs_nofoodhh"),
      arguments = list(),
      label = "HHS Indicator"
    )
  )

  # Convert to table
  table1 <- indicator_schema_to_table(original_schema)

  # Verify table structure
  expect_equal(nrow(table1), 2)

  # Convert back to schema
  schema2 <- indicator_table_to_schema(table1)

  # Verify schema is preserved
  expect_equal(length(schema2), 2)
  expect_true("fcs_ind" %in% names(schema2))
  expect_true("hhs_ind" %in% names(schema2))

  # Check details
  expect_equal(schema2$fcs_ind$function_name, "add_fcs")
  expect_equal(schema2$fcs_ind$variables, c("fsl_fcs_cereal", "fsl_fcs_legumes"))
  expect_equal(schema2$fcs_ind$arguments$cutoffs, "normal")
  expect_equal(schema2$fcs_ind$arguments$fsl_fcs_cereal, "@variable_map$fsl_fcs_cereal")
  expect_equal(schema2$fcs_ind$label, "FCS Indicator")
})


# INDICATOR_VALIDATE_TABLE_TO_SCHEMA Testing ####

test_that("indicator_validate_table_to_schema validates required columns", {

  df <- tibble::tibble(
    indicator_name = "test",
    function_name = "add_test"
    # Missing required columns
  )

  expect_error(
    indicator_validate_table_to_schema(df),
    regexp = "missing required columns"
  )
})

test_that("indicator_validate_table_to_schema accepts valid table", {

  df <- tibble::tibble(
    indicator_name = "test",
    function_name = "add_test",
    variables = "x",
    arguments = "a=1",
    label = NA,
    comment = NA
  )

  expect_true(indicator_validate_table_to_schema(df))
})

test_that("indicator_validate_table_to_schema warns on duplicates", {

  df <- tibble::tibble(
    indicator_name = c("test", "test"),
    function_name = c("add_test1", "add_test2"),
    variables = c("x", "y"),
    arguments = c(NA, NA),
    label = c(NA, NA),
    comment = c(NA, NA)
  )

  expect_warning(
    indicator_validate_table_to_schema(df),
    regexp = "Duplicate indicator names"
  )
})


# Vector Argument Parsing Tests ####

test_that("indicator_table_to_schema handles vector arguments with c()", {

  df <- tibble::tibble(
    indicator_name = "test_ind",
    function_name = "add_test",
    variables = "var1",
    arguments = "improved=c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$protected_well,@value_map$wash_water_source$tap)",
    label = NA,
    comment = NA
  )

  schema <- indicator_table_to_schema(df)

  expect_true(is.list(schema))
  expect_equal(length(schema), 1)
  expect_true("test_ind" %in% names(schema))

  ind <- schema$test_ind
  expect_equal(ind$indicator_name, "test_ind")
  expect_equal(ind$function_name, "add_test")

  # The argument should be stored as a single string with c(...)
  expect_equal(ind$arguments$improved, "c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$protected_well,@value_map$wash_water_source$tap)")
})

test_that("indicator_table_to_schema handles multiple arguments with vectors", {

  df <- tibble::tibble(
    indicator_name = "water_cat",
    function_name = "add_water_cat",
    variables = "water_source",
    arguments = "col=@variable_map$wash_water_source,improved=c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$tap),unimproved=c(@value_map$wash_water_source$unprotected_well),surface=c(@value_map$wash_water_source$surface_water)",
    label = NA,
    comment = NA
  )

  schema <- indicator_table_to_schema(df)

  ind <- schema$water_cat
  expect_equal(ind$arguments$col, "@variable_map$wash_water_source")
  expect_equal(ind$arguments$improved, "c(@value_map$wash_water_source$piped_dwelling,@value_map$wash_water_source$tap)")
  expect_equal(ind$arguments$unimproved, "c(@value_map$wash_water_source$unprotected_well)")
  expect_equal(ind$arguments$surface, "c(@value_map$wash_water_source$surface_water)")
})

# Test for is_other field type conversion from Excel files

test_that("data_table_to_schema handles logical TRUE/FALSE for is_other", {

  # Simulate what happens when reading from Excel - boolean columns become logical
  schema_df <- data.frame(
    rule_type = rep("variable", 3),
    variable = c("id", "water_source", "water_source_other"),
    value = rep(NA, 3),
    required = c(TRUE, FALSE, FALSE),
    type = c("character", "character", "character"),
    allowed = c(NA, "well,river,tap", NA),
    col_names = rep(NA, 3),
    unique = c(FALSE, FALSE, FALSE),  # logical FALSE
    label = rep(NA, 3),
    comment = rep(NA, 3),
    question_type = c(NA, "select_one", "text"),
    is_other = c(FALSE, FALSE, TRUE),  # logical TRUE/FALSE, not string
    other_column_link = c(NA, NA, "water_source"),
    stringsAsFactors = FALSE
  )

  # This should NOT throw an error and should correctly parse is_other
  schema <- data_table_to_schema(schema_df)

  # Check is_other
  expect_true("is_other" %in% names(schema))
  expect_true(schema$is_other$water_source_other)
  expect_false(schema$is_other$water_source)
  expect_false(schema$is_other$id)

  # Check other_column_link
  expect_true("other_column_link" %in% names(schema))
  expect_equal(schema$other_column_link$water_source_other, "water_source")
})


test_that("data_table_to_schema handles string TRUE/FALSE for is_other", {

  # Test with string "TRUE"/"FALSE" (for backward compatibility)
  schema_df <- data.frame(
    rule_type = rep("variable", 3),
    variable = c("id", "water_source", "water_source_other"),
    value = rep(NA, 3),
    required = c(TRUE, FALSE, FALSE),
    type = c("character", "character", "character"),
    allowed = c(NA, "well,river,tap", NA),
    col_names = rep(NA, 3),
    unique = c(NA, NA, NA),
    label = rep(NA, 3),
    comment = rep(NA, 3),
    question_type = c(NA, "select_one", "text"),
    is_other = c("FALSE", "FALSE", "TRUE"),  # string TRUE/FALSE
    other_column_link = c(NA, NA, "water_source"),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  # Check is_other
  expect_true("is_other" %in% names(schema))
  expect_true(schema$is_other$water_source_other)
  expect_false(schema$is_other$water_source)
  expect_false(schema$is_other$id)
})


test_that("data_table_to_schema handles mixed types for boolean fields", {

  # Test with numeric 1/0 (sometimes Excel converts boolean to numeric)
  schema_df <- data.frame(
    rule_type = rep("variable", 3),
    variable = c("id", "date_of_visit", "water_source_other"),
    value = rep(NA, 3),
    required = c(TRUE, FALSE, FALSE),
    type = c("character", "date", "character"),
    allowed = rep(NA, 3),
    col_names = rep(NA, 3),
    unique = c(1, 0, 0),  # numeric 1/0
    label = rep(NA, 3),
    comment = rep(NA, 3),
    question_type = c(NA, "date", "text"),
    is_other = c(0, 0, 1),  # numeric 1/0
    other_column_link = rep(NA, 3),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  # Check unique constraint
  expect_true("id" %in% schema$unique)
  expect_false("date_of_visit" %in% schema$unique)

  # Check is_other
  expect_true(schema$is_other$water_source_other)
  expect_false(schema$is_other$id)
})

# Testing .safe_bool ####

test_that(".safe_bool helper function works correctly", {
  # Note: Using ::: to test internal function is acceptable for helper functions
  # that are critical to data integrity and type conversion

  # Test logical values
  expect_true(phr:::.safe_bool(TRUE))
  expect_false(phr:::.safe_bool(FALSE))

  # Test string values
  expect_true(phr:::.safe_bool("TRUE"))
  expect_true(phr:::.safe_bool("true"))
  expect_true(phr:::.safe_bool("True"))
  expect_true(phr:::.safe_bool("T"))
  expect_true(phr:::.safe_bool("1"))
  expect_false(phr:::.safe_bool("FALSE"))
  expect_false(phr:::.safe_bool("false"))
  expect_false(phr:::.safe_bool(""))

  # Test numeric values
  expect_true(phr:::.safe_bool(1))
  expect_false(phr:::.safe_bool(0))

  # Test NA values
  expect_false(phr:::.safe_bool(NA))
  expect_false(phr:::.safe_bool(NA_character_))
  expect_false(phr:::.safe_bool("NA"))
})




# TEST VARIABLE SCHEMA ####

# Tests for new schema fields: question_types, is_other, other_column_link


# Part 1: Schema Conversion Tests


test_that("data_schema_to_table includes new fields", {

  schema <- list(
    required = "id",
    types = list(
      id = "character",
      water_source = "character",
      water_source_other = "character",
      livelihood = "character"
    ),
    question_types = list(
      water_source = "select_one",
      water_source_other = "text",
      livelihood = "select_multiple"
    ),
    is_other = list(
      water_source_other = TRUE
    ),
    other_column_link = list(
      water_source_other = c("water_source")
    )
  )

  tab <- data_schema_to_table(schema)

  # Check that new columns exist
  expect_true("question_type" %in% names(tab))
  expect_true("is_other" %in% names(tab))
  expect_true("other_column_link" %in% names(tab))

  # Check water_source_other row
  other_row <- tab[tab$variable == "water_source_other", ]
  expect_equal(other_row$question_type, "text")
  expect_equal(other_row$is_other, "TRUE")
  expect_equal(other_row$other_column_link, "water_source")

  # Check livelihood row
  liv_row <- tab[tab$variable == "livelihood", ]
  expect_equal(liv_row$question_type, "select_multiple")
})


test_that("data_table_to_schema parses new fields correctly", {

  # Create a schema table with new fields
  schema_df <- data.frame(
    rule_type = rep("variable", 4),
    variable = c("id", "water_source", "water_source_other", "livelihood"),
    value = c("A", "borehole", "well", "farming"),
    required = c(TRUE, FALSE, FALSE, FALSE),
    type = c("character", "character", "character", "character"),
    allowed = c(NA, "well,river,tap", NA, "farming,fishing,trading"),
    col_names = rep(NA, 4),
    unique = rep(NA, 4),
    label = rep(NA, 4),
    comment = rep(NA, 4),
    question_type = c(NA, "select_one", "text", "select_multiple"),
    is_other = c(NA, NA, "TRUE", NA),
    other_column_link = c(NA, NA, "water_source", NA),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  # Check question_types
  expect_true("question_types" %in% names(schema))
  expect_equal(schema$question_types$water_source, "select_one")
  expect_equal(schema$question_types$water_source_other, "text")
  expect_equal(schema$question_types$livelihood, "select_multiple")

  # Check is_other
  expect_true("is_other" %in% names(schema))
  expect_true(schema$is_other$water_source_other)

  # Check other_column_link
  expect_true("other_column_link" %in% names(schema))
  expect_equal(schema$other_column_link$water_source_other, "water_source")
})


test_that("schema roundtrip preserves new fields", {

  original_schema <- list(
    required = "id",
    types = list(id = "character", skills = "character", skills_other = "character"),
    question_types = list(
      skills = "select_multiple",
      skills_other = "text"
    ),
    is_other = list(skills_other = TRUE),
    other_column_link = list(skills_other = c("skills"))
  )

  # Convert to table and back
  tab <- data_schema_to_table(original_schema)
  reconstructed_schema <- data_table_to_schema(tab)

  # Verify fields are preserved
  expect_equal(reconstructed_schema$question_types$skills, "select_multiple")
  expect_equal(reconstructed_schema$question_types$skills_other, "text")
  expect_true(reconstructed_schema$is_other$skills_other)
  expect_equal(reconstructed_schema$other_column_link$skills_other, "skills")
})



# Part 2: Select Multiple Expansion Tests


test_that("expand_select_multiple creates correct dummy columns", {

  # Test data with space-separated select_multiple values
  test_col <- c(
    "option1 option2",
    "option2 option3",
    "option1",
    NA,
    "",
    "option3 option1 option2"
  )

  result <- expand_select_multiple(test_col, "test_var")

  # Should have 3 columns (option1, option2, option3)
  expect_equal(ncol(result), 3)
  expect_true(all(c("test_var.option1", "test_var.option2", "test_var.option3") %in% names(result)))

  # Check row 1: should have option1=1, option2=1, option3=0
  expect_equal(result[1, "test_var.option1"], 1)
  expect_equal(result[1, "test_var.option2"], 1)
  expect_equal(result[1, "test_var.option3"], 0)

  # Check row 3: should have option1=1, option2=0, option3=0
  expect_equal(result[3, "test_var.option1"], 1)
  expect_equal(result[3, "test_var.option2"], 0)
  expect_equal(result[3, "test_var.option3"], 0)

  # Check row 4 (NA): should have all 0s
  expect_equal(sum(result[4, ]), 0)
})


test_that("expand_select_multiple handles different orderings", {

  test_col <- c(
    "b a c",
    "c b a",
    "a c b"
  )

  result <- expand_select_multiple(test_col, "var")

  # All rows should have all three options
  expect_equal(rowSums(result), c(3, 3, 3))
})


test_that("expand_select_multiple handles empty data", {

  test_col <- c(NA, "", NA)
  result <- expand_select_multiple(test_col, "var")

  # Should return empty data frame (0 rows, 0 cols) when no valid values exist
  # This is the intended behavior - no dummy columns are created
  expect_equal(ncol(result), 0)
  expect_equal(nrow(result), 0)
})


test_that("process_select_multiple_columns integrates into dataset", {

  # Create test dataset
  test_data <- data.frame(
    id = 1:3,
    livelihood = c("farming fishing", "trading", "farming trading fishing"),
    age = c(25, 30, 35),
    stringsAsFactors = FALSE
  )

  # Create schema with select_multiple
  schema <- list(
    question_types = list(
      livelihood = "select_multiple"
    )
  )

  result <- process_select_multiple_columns(test_data, schema)

  # Should have original columns plus dummy columns
  expect_gt(ncol(result$data), ncol(test_data))

  # Check expanded columns were tracked
  expect_true(length(result$expanded_columns) > 0)
  expect_true(all(grepl("^livelihood.", result$expanded_columns)))

  # Original columns should still exist
  expect_true("id" %in% names(result$data))
  expect_true("age" %in% names(result$data))
})


test_that("process_select_multiple_columns handles missing schema", {

  test_data <- data.frame(id = 1:3, val = c("a b", "c", "a"))

  # No schema
  result <- process_select_multiple_columns(test_data, NULL)
  expect_equal(result$data, test_data)
  expect_equal(length(result$expanded_columns), 0)

  # Schema without question_types
  result <- process_select_multiple_columns(test_data, list())
  expect_equal(result$data, test_data)
  expect_equal(length(result$expanded_columns), 0)
})



# Part 3: Data Class Integration Tests


test_that("Data$standardize expands select_multiple columns", {

  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:4),
    skills = c("reading writing", "writing math", "reading", NA)
  )

  schema <- list(
    required = "uuid",
    types = list(uuid = "character", skills = "character"),
    question_types = list(skills = "select_multiple")
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Check standardized data has dummy columns
  std_data <- d$standardized_data
  expect_true("skills.math" %in% names(std_data))
  expect_true("skills.reading" %in% names(std_data))
  expect_true("skills.writing" %in% names(std_data))

  # Verify dummy values are correct
  expect_equal(std_data$skills.reading[1], 1)
  expect_equal(std_data$skills.writing[1], 1)
  expect_equal(std_data$skills.math[1], 0)
})


test_that("Data$standardize identifies schema-defined other columns", {

  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:3),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "spring", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      water_source = "character",
      water_source_other = "character"
    ),
    is_other = list(water_source_other = TRUE),
    other_column_link = list(water_source_other = c("water_source"))
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Check that water_source_other was added to other_columns
  expect_true("water_source_other" %in% names(d$other_columns))

  # Check the structure of the other_columns entry
  expect_true(is.list(d$other_columns$water_source_other))
  expect_equal(d$other_columns$water_source_other$other_column, "water_source_other")
  expect_true("water_source" %in% d$other_columns$water_source_other$other_linked_columns)
})



# Part 4: Cleaning Log Generation Tests


test_that("generate_cleaning_log creates entries for other columns", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2", "id_3"),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "mountain spring", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      water_source = "character",
      water_source_other = "character"
    ),
    is_other = list(water_source_other = TRUE),
    other_column_link = list(water_source_other = c("water_source"))
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Generate cleaning log (without quality flags, only other columns)
  d$data_quality_flags <- data.frame(uuid = test_df$uuid)  # Empty flags
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_df <- d$cleaning_log$log_df

  # Should have entries for row 2 (where water_source_other has value)
  # Two entries: one for water_source_other, one for water_source
  expect_gte(nrow(log_df), 2)

  # Check that entries exist for the row with "other" text
  id2_entries <- log_df[log_df$uuid == "id_2", ]
  expect_gte(nrow(id2_entries), 2)

  # One entry should be for water_source_other column
  expect_true(any(id2_entries$question.name == "water_source_other"))
  expect_true(any(id2_entries$issue == "other_response"))

  # One entry should be for water_source column
  expect_true(any(id2_entries$question.name == "water_source"))
  expect_true(any(id2_entries$issue == "has_other_response"))
})


test_that("Data$standardize detects both inferred and schema-identified other columns", {


  # Construct test data

  set.seed(123)

  df <- tibble::tibble(
    uuid = 1:30,

    # Inference-based "other":
    # Column name matches "_other" pattern
    free_text_other = c(
      rep(NA_character_, 20),
      paste0("response_", 1:10)
    ),

    # Schema-declared "other"
    declared_other = sample(c("A", "B", NA), 30, replace = TRUE),

    # Normal column (should NOT be detected)
    age = sample(18:60, 30, replace = TRUE)
  )


  # Minimal schema

  schema <- list(
    types = list(
      uuid = "numeric",
      age  = "numeric"
    ),
    is_other = list(
      declared_other = TRUE
    )
  )


  # Run Data lifecycle

  d <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestData",
      uuid = "uuid"
    )
  )

  d$set_variable_schema(schema)
  d$standardize()


  # Assertions

  expect_true("free_text_other" %in% names(d$other_columns))
  expect_true("declared_other" %in% names(d$other_columns))

  # Ensure no false positives
  expect_false("age" %in% names(d$other_columns))

  # Ensure uniqueness (no duplicate names)
  expect_equal(
    length(names(d$other_columns)),
    length(unique(names(d$other_columns)))
  )
})



test_that("generate_cleaning_log handles detected other columns", {

  # Create data with a column that will be detected as "other"
  # (column name matches "_other" pattern and has text values)
  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:20),
    main_var = rep("value", 20),
    detected_other = c(
      rep("", 15),  # Many blanks (75%)
      paste0("unique_response_", 1:5)  # Unique responses
    )
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )

  # No schema, so detected_other should be auto-detected
  d$standardize()

  # Check if detected
  expect_true("detected_other" %in% names(d$other_columns))

  # Generate cleaning log
  d$data_quality_flags <- data.frame(uuid = test_df$uuid)
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_df <- d$cleaning_log$log_df

  # Should have entries for rows 16-20 (with unique responses)
  expect_gte(nrow(log_df), 5)
})



# Part 5: Edge Cases and Error Handling


test_that("schema with only some new fields works correctly", {

  # Schema with question_types but not is_other
  schema1 <- list(
    required = "id",
    types = list(id = "character", var1 = "character"),
    question_types = list(var1 = "select_multiple")
  )

  tab1 <- data_schema_to_table(schema1)
  expect_true("question_type" %in% names(tab1))
  expect_true("is_other" %in% names(tab1))

  # Schema with is_other but not question_types
  schema2 <- list(
    required = "id",
    types = list(id = "character", var1_other = "character"),
    is_other = list(var1_other = TRUE)
  )

  tab2 <- data_schema_to_table(schema2)
  expect_true("question_type" %in% names(tab2))
  expect_true("is_other" %in% names(tab2))
})


test_that("multiple other columns can link to same main column", {

  schema_df <- data.frame(
    rule_type = rep("variable", 4),
    variable = c("id", "main_q", "other1", "other2"),
    value = c("id", "main_q", "other1", "other2"),
    required = c(TRUE, FALSE, FALSE, FALSE),
    type = rep("character", 4),
    allowed = rep(NA, 4),
    col_names = rep(NA, 4),

    unique = rep(NA, 4),
    mutex_group = rep(NA, 4),

    label = rep(NA, 4),
    comment = rep(NA, 4),
    question_type = rep(NA, 4),
    is_other = c(NA, NA, "TRUE", "TRUE"),
    other_column_link = c(NA, NA, "main_q", "main_q"),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  expect_true(schema$is_other$other1)
  expect_true(schema$is_other$other2)
  expect_equal(schema$other_column_link$other1, "main_q")
  expect_equal(schema$other_column_link$other2, "main_q")
})


test_that("select_multiple with no data returns empty expansion", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2"),
    skills = c(NA, NA)
  )

  schema <- list(
    required = "uuid",
    types = list(uuid = "character", skills = "character"),
    question_types = list(skills = "select_multiple")
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)

  # Should not error
  expect_no_error(d$standardize())

  # Should not have added dummy columns (since all NA)
  std_data <- d$standardized_data
  dummy_cols <- grep("^skills_", names(std_data), value = TRUE)
  expect_equal(length(dummy_cols), 0)
})



# Part 6: New List Structure Tests for other_columns


test_that("other_columns uses list structure with other_column and other_linked_columns", {

  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:3),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "spring water", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      water_source = "character",
      water_source_other = "character"
    ),
    is_other = list(water_source_other = TRUE),
    other_column_link = list(water_source_other = c("water_source"))
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Check that other_columns is a list
  expect_true(is.list(d$other_columns))

  # Check that water_source_other entry exists
  expect_true("water_source_other" %in% names(d$other_columns))

  # Check structure of the entry
  entry <- d$other_columns$water_source_other
  expect_true(is.list(entry))
  expect_true("other_column" %in% names(entry))
  expect_true("other_linked_columns" %in% names(entry))

  # Check values
  expect_equal(entry$other_column, "water_source_other")
  expect_equal(entry$other_linked_columns, "water_source")
})


test_that("select_multiple with other creates proper list structure", {

  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:3),
    skills = c("reading writing other", "math", "reading"),
    skills_other_text = c("coding", "", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      skills = "character",
      skills_other_text = "character"
    ),
    question_types = list(skills = "select_multiple")
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Check that skills_other_text entry exists
  expect_true("skills_other_text" %in% names(d$other_columns))

  # Check structure
  entry <- d$other_columns$skills_other_text
  expect_equal(entry$other_column, "skills_other_text")

  # Should have skills and skills.other as linked columns
  expect_true("skills" %in% entry$other_linked_columns)
  expect_true("skills.other" %in% entry$other_linked_columns)
})


test_that("inferred other columns have proper list structure", {

  # Create data with a column that will be detected as "other"
  set.seed(456)
  test_df <- tibble::tibble(
    uuid = paste0("id_", 1:25),
    main_var = rep("value", 25),
    detected_other = c(
      rep("", 20),  # 80% blanks
      paste0("unique_", 1:5)  # Many unique responses
    )
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$standardize()

  # Should be detected
  expect_true("detected_other" %in% names(d$other_columns))

  # Check structure
  entry <- d$other_columns$detected_other
  expect_true(is.list(entry))
  expect_equal(entry$other_column, "detected_other")

  # Since naming doesn't match pattern, linked_columns should be empty
  expect_true(length(entry$other_linked_columns) == 0)
})


test_that("generate_cleaning_log uses new list structure correctly", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2", "id_3"),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "lake", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      water_source = "character",
      water_source_other = "character"
    ),
    is_other = list(water_source_other = TRUE),
    other_column_link = list(water_source_other = c("water_source"))
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Verify other_columns structure
  expect_true("water_source_other" %in% names(d$other_columns))
  expect_equal(d$other_columns$water_source_other$other_column, "water_source_other")
  expect_equal(d$other_columns$water_source_other$other_linked_columns, "water_source")

  # Generate cleaning log
  d$data_quality_flags <- data.frame(uuid = test_df$uuid)
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_df <- d$cleaning_log$log_df

  # Should have 2 entries for id_2 (one for water_source_other, one for water_source)
  id2_entries <- log_df[log_df$uuid == "id_2", ]
  expect_equal(nrow(id2_entries), 2)

  # Check question names
  expect_true("water_source_other" %in% id2_entries$question.name)
  expect_true("water_source" %in% id2_entries$question.name)

  # Check issues
  other_entry <- id2_entries[id2_entries$question.name == "water_source_other", ]
  expect_equal(other_entry$issue, "other_response")

  linked_entry <- id2_entries[id2_entries$question.name == "water_source", ]
  expect_equal(linked_entry$issue, "has_other_response")
})

# TEST DEPENDENCY SCHEMA ####




# TEST INDICATOR SCHEMA ####

# import_indicator_schema ####

test_that("Data$import_indicator_schema imports indicator table", {

  df <- tibble::tibble(id = 1:3, value = c(10, 20, 30))

  indicator_table <- tibble::tibble(
    indicator_name = "test_ind",
    function_name = "add_test",
    variables = "value",
    arguments = "multiplier=2",
    label = "Test Indicator",
    comment = NA
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "Test", uuid = "id")
  )

  expect_no_error(d$import_indicator_schema(indicator_table))

  expect_false(is.null(d$indicator_schema))
  expect_equal(length(d$indicator_schema), 1)
  expect_true("test_ind" %in% names(d$indicator_schema))
})


# export_indicator_schema ####

test_that("Data$export_indicator_schema exports indicator schema", {

  df <- tibble::tibble(id = 1:3, value = c(10, 20, 30))

  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test",
      variables = c("value"),
      arguments = list(multiplier = "2")
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "Test", uuid = "id")
  )
  d$indicator_schema <- indicator_schema

  exported <- d$export_indicator_schema()

  expect_s3_class(exported, "data.frame")
  expect_equal(nrow(exported), 1)
  expect_equal(exported$indicator_name, "test_ind")
})

test_that("Data$export_indicator_schema warns when no schema", {

  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "Test", uuid = "id")
  )

  expect_warning(
    d$export_indicator_schema(),
    regexp = "No indicator schema"
  )
})


# set_indicator_schema ####

test_that("Data$set_indicator_schema sets indicator schema", {

  df <- tibble::tibble(id = 1:3, value = c(10, 20, 30))

  indicator_schema <- list(
    test_ind = list(
      indicator_name = "test_ind",
      function_name = "add_test",
      variables = c("value"),
      arguments = list()
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "Test", uuid = "id")
  )

  expect_no_error(d$set_indicator_schema(indicator_schema))

  expect_false(is.null(d$indicator_schema))
  expect_equal(length(d$indicator_schema), 1)
})

# Test for variable_map resolution in indicator functions

test_that("indicator with unresolved @variable_map$ reference passes NULL", {
  # Create a simple dataset
  df <- tibble::tibble(
    uuid = c("1", "2", "3"),
    age = c(10, 20, 30)
  )

  # Create a Data object with a simple variable schema
  test_data <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestData", uuid = "uuid"
    )
  )

  # Set variable map (only map 'age', not 'age_months')
  test_data$variable_map <- list(
    uuid = "uuid",
    age_years = "age"
  )

  # Set indicator schema that references a non-existent variable map role
  test_data$indicator_schema <- list(
    test_indicator = list(
      indicator_name = "test_indicator",
      function_name = "add_standardized_age",
      variables = c("age_years"),
      arguments = list(
        age_years_col = "@variable_map$age_years",
        age_months_col = "@variable_map$age_months"  # This role doesn't exist
      )
    )
  )

  # Run standardize - should warn about age_months not found but pass NULL
  expect_warning(
    test_data$standardize(),
    "Variable map role 'age_months' not found"
  )

  # Check that standardized data was created successfully
  expect_true(test_data$standardized)
  expect_true("calc_age_years" %in% names(test_data$standardized_data))
})

test_that("indicator with all resolved @variable_map$ references works", {
  # Create a simple dataset
  df <- tibble::tibble(
    uuid = c("1", "2", "3"),
    age = c(10, 20, 30),
    months = c(120, 240, 360)
  )

  # Create a Data object
  test_data <- suppressMessages(
    Data$new(
      data = df,
      dataset_name = "TestData", uuid = "uuid"
    )
  )

  # Set variable map with both roles
  test_data$variable_map <- list(
    uuid = "uuid",
    age_years = "age",
    age_months = "months"
  )

  # Set indicator schema that references existing variable map roles
  test_data$indicator_schema <- list(
    test_indicator = list(
      indicator_name = "test_indicator",
      function_name = "add_standardized_age",
      variables = c("age_years", "age_months"),
      arguments = list(
        age_years_col = "@variable_map$age_years",
        age_months_col = "@variable_map$age_months"
      )
    )
  )

  # Run standardize - should work without warnings
  test_data$standardize()

  # Check that standardized data was created successfully
  expect_true(test_data$standardized)
  expect_true("calc_age_years" %in% names(test_data$standardized_data))
  expect_true("calc_age_months" %in% names(test_data$standardized_data))
})

# DATA_SCHEMA_TO_TABLE Testing ####

# VALUE_MAP FUNCTIONALITY TESTS ####

test_that("data_schema_to_table handles value_map with multiple canonical values", {

  schema <- list(
    types = list(priority = "character"),
    value_map = list(
      priority = list(
        water = c("drinking_water", "eau_potable"),
        food = c("food", "nourriture"),
        shelter = c("shelter", "abri")
      )
    )
  )

  tab <- data_schema_to_table(schema)

  expect_s3_class(tab, "data.frame")
  # Should have 3 rows - one per canonical value
  expect_equal(nrow(tab), 3)

  # Check that value column is present
  expect_true("value" %in% names(tab))

  # Check canonical values
  expect_setequal(tab$value, c("water", "food", "shelter"))

  # Check that all rows have the same variable
  expect_true(all(tab$variable == "priority"))

  # Check allowed values for each canonical value
  expect_equal(
    tab$allowed[tab$value == "water"],
    "drinking_water,eau_potable"
  )
  expect_equal(
    tab$allowed[tab$value == "food"],
    "food,nourriture"
  )
})

test_that("data_schema_to_table handles mixed value_map and plain variables", {

  schema <- list(
    types = list(
      uuid = "character",
      status = "character"
    ),
    value_map = list(
      status = list(
        yes = c("yes", "y", "1"),
        no = c("no", "n", "0")
      )
    )
  )

  tab <- data_schema_to_table(schema)

  # uuid should have 1 row with value = NA
  # status should have 2 rows (one per canonical value)
  expect_equal(nrow(tab), 3)

  uuid_row <- tab[tab$variable == "uuid", ]
  expect_true(is.na(uuid_row$value))

  status_rows <- tab[tab$variable == "status", ]
  expect_equal(nrow(status_rows), 2)
  expect_setequal(status_rows$value, c("yes", "no"))
})

test_that("data_schema_to_table does not export deprecated columns", {

  # Create a schema with some fields that were previously exported but should no longer be
  schema <- list(
    types = list(
      uuid = "character",
      age = "numeric"
    ),
    required = c("uuid"),
    allowed_values = list(),
    value_map = list(),
    col_names = list(),
    unique = character(0),
    # These fields may still exist in old schemas but should not be exported to table
    patterns = list(uuid = "^[A-Z0-9]+$"),
    ranges = list(age = c(0, 120)),
    precision_limits = list(age = 0),
    mutually_exclusive = list(group1 = c("var1", "var2")),
    date_validity = list(date_field = list(not_future = TRUE))
  )

  tab <- data_schema_to_table(schema)

  expect_s3_class(tab, "data.frame")

  # Verify that deprecated columns are NOT in the exported table
  expect_false("pattern" %in% names(tab),
               info = "pattern column should not be exported")
  expect_false("range" %in% names(tab),
               info = "range column should not be exported")
  expect_false("precision_limits" %in% names(tab),
               info = "precision_limits column should not be exported")
  expect_false("mutex_group" %in% names(tab),
               info = "mutex_group column should not be exported")
  expect_false("not_future" %in% names(tab),
               info = "not_future column should not be exported")

  # Verify that valid columns are still present
  expect_true("rule_type" %in% names(tab))
  expect_true("variable" %in% names(tab))
  expect_true("value" %in% names(tab))
  expect_true("required" %in% names(tab))
  expect_true("type" %in% names(tab))
  expect_true("allowed" %in% names(tab))
  expect_true("col_names" %in% names(tab))
  expect_true("unique" %in% names(tab))
  expect_true("label" %in% names(tab))
  expect_true("comment" %in% names(tab))
})

test_that("data_table_to_schema builds value_map from value column", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 3),
    variable         = rep("status", 3),
    value            = c("yes", "no", "unknown"),
    required         = rep(FALSE, 3),
    type             = rep("character", 3),
    question_type    = rep(NA, 3),
    is_other         = rep(NA, 3),
    other_column_link= rep(NA, 3),
    allowed          = c("yes,y,1,oui", "no,n,0,non", "unknown,inconnu"),
    col_names        = rep(NA, 3),
    unique           = rep(NA, 3),
    label            = rep(NA, 3),
    comment          = rep(NA, 3)
  )

  schema <- data_table_to_schema(df)

  # Check that value_map is built correctly
  expect_true("value_map" %in% names(schema))
  expect_true("status" %in% names(schema$value_map))

  # Check the nested structure
  status_map <- schema$value_map$status
  expect_setequal(names(status_map), c("yes", "no", "unknown"))

  # Check each canonical value's allowed dataset values
  expect_setequal(status_map$yes, c("yes", "y", "1", "oui"))
  expect_setequal(status_map$no, c("no", "n", "0", "non"))
  expect_setequal(status_map$unknown, c("unknown", "inconnu"))
})

test_that("data_table_to_schema handles variables without value column (backward compatibility)", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("uuid", "age"),
    value            = c(NA, NA),
    required         = c(TRUE, FALSE),
    type             = c("character", "numeric"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c(NA, NA),
    col_names        = c(NA, NA),
    unique           = c(NA, NA),
    label            = c(NA, NA),
    comment          = c(NA, NA)
  )

  schema <- data_table_to_schema(df)

  # These variables should not have value_map entries
  expect_false("uuid" %in% names(schema$value_map))
  expect_false("age" %in% names(schema$value_map))

  # Old fields should still work
  expect_equal(schema$types$uuid, "character")
  expect_equal(schema$types$age, "numeric")
})

test_that("data_validate_table_to_schema accepts duplicate variables with different values", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("status", "status"),
    value            = c("yes", "no"),
    required         = c(FALSE, FALSE),
    type             = c("character", "character"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c("yes,y", "no,n"),
    col_names        = c(NA, NA),
    unique           = c(NA, NA),
    label            = c(NA, NA),
    comment          = c(NA, NA)
  )

  # Should not warn about duplicates since they have different values
  expect_no_warning(data_validate_table_to_schema(df))
})

test_that("data_validate_table_to_schema warns on duplicate (variable, value) pairs", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("status", "status"),
    value            = c("yes", "yes"),  # Duplicate pair
    required         = c(FALSE, FALSE),
    type             = c("character", "character"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c("yes,y", "yes,y"),
    col_names        = c(NA, NA),
    unique           = c(NA, NA),
    label            = c(NA, NA),
    comment          = c(NA, NA)
  )

  # Should warn about duplicate (variable, value) pairs
  expect_warning(
    data_validate_table_to_schema(df),
    regexp = "Duplicate.*variable, value"
  )
})

test_that("data_schema_to_table and data_table_to_schema round-trip with value_map", {

  original_schema <- list(
    types = list(
      uuid = "character",
      priority = "character"
    ),
    value_map = list(
      priority = list(
        water = c("drinking_water", "eau_potable"),
        food = c("food", "nourriture")
      )
    )
  )

  # Convert to table
  tab <- data_schema_to_table(original_schema)

  # Convert back to schema
  schema2 <- data_table_to_schema(tab)

  # Check value_map is preserved
  expect_equal(
    schema2$value_map$priority,
    original_schema$value_map$priority
  )

  # Check types are preserved
  expect_equal(schema2$types, original_schema$types)
})

# ORIGINAL DATA_SCHEMA_TO_TABLE TESTS ####

test_that("data_schema_to_table converts a normal schema correctly", {

  # Note: patterns have been moved to dependency_schema
  schema <- list(
    required = c("id", "age"),
    types = list(id = "character", age = "numeric", sex = "character"),
    allowed_values = list(sex = c("male", "female")),
    patterns = list(id = "^[A-Z0-9]+$")  # This is ignored (moved to dependency_schema)
  )

  tab <- data_schema_to_table(schema)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 3)  # id, age, sex

  # Check that value column exists
  expect_true("value" %in% names(tab))

  # pattern column no longer exists
  expect_false("pattern" %in% names(tab))

  expect_equal(
    sort(tab$variable),
    c("age", "id", "sex")
  )

  expect_true(tab$required[tab$variable == "id"])
  expect_true(tab$required[tab$variable == "age"])
  expect_false(tab$required[tab$variable == "sex"])

  expect_equal(
    tab$type[tab$variable == "age"],
    "numeric"
  )

  expect_equal(
    tab$allowed[tab$variable == "sex"],
    "male,female"
  )
})

test_that("data_schema_to_table handles schema with missing optional fields", {

  schema <- list(
    required = "uuid",
    types = list(uuid = "character")
    # allowed_values and patterns omitted
  )

  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 1)
  expect_equal(tab$variable, "uuid")
  expect_equal(tab$type, "character")
  expect_true(tab$required)
  expect_true(is.na(tab$allowed))
  # pattern column no longer exists in output
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table works with an empty schema list", {

  schema <- list()

  tab <- data_schema_to_table(schema)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 0)
})

test_that("data_schema_to_table errors for NULL schema", {
  expect_error(
    data_schema_to_table(NULL),
    regexp = "Schema must be a list"
  )
})

test_that("data_schema_to_table errors for non-list schema", {
  expect_error(
    data_schema_to_table("not_a_list"),
    regexp = "Schema must be a list"
  )
})

test_that("data_schema_to_table handles mixed completeness per variable", {

  schema <- list(
    required = c("var1"),
    types = list(var1 = "numeric", var2 = "character"),
    allowed_values = list(var3 = c("A", "B")),
    patterns = list(var4 = "^\\d+$")  # Ignored - moved to dependency_schema
  )

  tab <- data_schema_to_table(schema)

  # Only 3 rows now (var1, var2, var3) - patterns no longer collected
  expect_equal(nrow(tab), 3)

  # var1
  expect_true(tab$required[tab$variable == "var1"])
  expect_equal(tab$type[tab$variable == "var1"], "numeric")

  # var2
  expect_false(tab$required[tab$variable == "var2"])
  expect_equal(tab$type[tab$variable == "var2"], "character")

  # var3
  expect_equal(tab$allowed[tab$variable == "var3"], "A,B")

  # pattern column no longer exists
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table handles allowed values without types (patterns moved to dependency_schema)", {

  # Note: patterns have been moved to dependency_schema and are no longer
  # collected as variables in the variable schema table
  schema <- list(
    allowed_values = list(color = c("red","green","blue")),
    patterns = list(code = "^[A-Z]{3}\\d{2}$")  # This is ignored (backward compatibility)
  )

  tab <- data_schema_to_table(schema)

  # Only color should be in the table now (patterns no longer collected)
  expect_equal(nrow(tab), 1)

  expect_equal(tab$allowed[tab$variable == "color"], "red,green,blue")

  # pattern column no longer exists in output
  expect_false("pattern" %in% names(tab))

  # Missing fields should be NA
  expect_true(is.na(tab$type[tab$variable == "color"]))
  expect_false(tab$required[tab$variable == "color"])
})

test_that("data_schema_to_table handles required-only entries", {

  schema <- list(
    required = c("a","b","c")
  )

  tab <- data_schema_to_table(schema)

  expect_equal(sort(tab$variable), c("a","b","c"))
  expect_true(all(tab$required))
  expect_true(all(is.na(tab$type)))
  expect_true(all(is.na(tab$allowed)))
  # pattern column no longer exists
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table deduplicates variable names across schema fields", {

  schema <- list(
    required = c("id","id"),
    types = list(id = "character"),
    allowed_values = list(id = c("A","B"))
  )

  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 1)
  expect_equal(tab$variable, "id")
  expect_true(tab$required)
})

test_that("data_schema_to_table handles large schemas efficiently", {

  vars <- paste0("var", 1:500)

  schema <- list(
    required = vars[1:50],
    types = as.list(setNames(rep("numeric", 500), vars)),
    allowed_values = as.list(setNames(rep(list(c("A", "B")), 500), vars))
  )

  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 500)
  expect_true(all(vars %in% tab$variable))
  expect_equal(sum(tab$required), 50)
})

test_that("data_schema_to_table supports unusual variable names", {

  schema <- list(
    required = c("var 1", "var-2"),
    types = list("var 1" = "character", "var-2" = "numeric", "Ωmega" = "character"),
    patterns = list("Ωmega" = "^Ω[0-9]+$")  # Ignored - moved to dependency_schema
  )

  tab <- data_schema_to_table(schema)

  # Only 3 variables (patterns no longer collected)
  expect_equal(sort(tab$variable), sort(c("var 1", "var-2", "Ωmega")))
  expect_equal(tab$type[tab$variable == "var 1"], "character")
  expect_equal(tab$type[tab$variable == "var-2"], "numeric")
  expect_equal(tab$type[tab$variable == "Ωmega"], "character")
  # pattern column no longer exists
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table errors on invalid or non-character type definitions", {

  schema <- list(
    types = list(
      a = 123,              # numeric
      b = TRUE,             # logical
      c = as.list(1:3)      # list
    )
  )

  expect_error(
    data_schema_to_table(schema),
    regexp = "Invalid type entry",   # key part of your phr_error message
    fixed  = FALSE
  )
})


test_that("data_schema_to_table errors when allowed_values contain non-atomic values", {

  schema <- list(
    allowed_values = list(
      a = c(1, 2, 3),              # OK
      b = list("x", "y"),          # INVALID
      c = factor(c("low", "high")) # OK
    )
  )

  expect_error(
    data_schema_to_table(schema),
    regexp = "Allowed values.*must be atomic"
  )
})

test_that("data_schema_to_table handles patterns for backward compatibility (ignored)", {
  # Patterns are now in dependency_schema, but we support them in variable schema
  # for backward compatibility - they are just ignored (not collected as variables)

  schema <- list(
    patterns = list(
      a = "(unclosed",
      b = "[0-9]+)",
      c = "\\K"  # PCRE extension not valid in base R
    )
  )

  # Patterns are ignored - no variables collected from them
  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 0)
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table handles disjoint variable definitions", {

  schema <- list(
    types = list(a = "numeric"),
    allowed_values = list(b = c("yes","no")),
    patterns = list(c = "^abc$")  # Ignored - moved to dependency_schema
  )

  tab <- data_schema_to_table(schema)

  # Only a and b (patterns no longer collected)
  expect_equal(sort(tab$variable), c("a","b"))

  expect_equal(tab$type[tab$variable == "a"], "numeric")
  expect_equal(tab$allowed[tab$variable == "b"], "yes,no")
  # pattern column no longer exists
  expect_false("pattern" %in% names(tab))
})

test_that("data_schema_to_table handles empty schema components gracefully", {

  schema <- list(
    required = character(0),
    types = list(),
    allowed_values = list(),
    patterns = list()
  )

  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 0)
})

test_that("data_schema_to_table deduplicates variable names across schema components", {

  schema <- list(
    required = c("id", "id"),
    types = list(id = "character"),
    allowed_values = list(id = c("X","Y")),
    patterns = list(id = "^.+$")
  )

  tab <- data_schema_to_table(schema)

  expect_equal(nrow(tab), 1)
  expect_equal(tab$variable, "id")
})

# DATA_VALIDATE_SCHEMA_TO_TABLE ####

test_that("data_validate_schema_to_table passes for a minimal valid schema", {

  schema <- list(
    required = c("id", "name"),
    types = list(id = "character"),
    allowed_values = list(name = c("A", "B")),
    patterns = list(id = "^[A-Z]+$")
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table errors when schema_list is NULL", {
  expect_error(
    data_validate_schema_to_table(NULL),
    regexp = "Schema must be a list object"
  )
})

test_that("data_validate_schema_to_table errors when schema_list is not a list", {
  expect_error(
    data_validate_schema_to_table("notalist"),
    regexp = "Schema must be a list object"
  )
})

test_that("data_validate_schema_to_table errors when required is not character", {

  schema <- list(
    required = 123
  )

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "`required` must be a character vector"
  )
})

test_that("data_validate_schema_to_table passes with empty required", {
  schema <- list(required = character(0))
  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table accepts all valid type strings", {

  schema <- list(
    types = list(
      a = "numeric",
      b = "character",
      c = "logical",
      d = "factor",
      e = "date",
      f = "Date",
      g = "POSIXct"
    )
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table errors when types is not a list", {

  schema <- list(types = "character")

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "`types` must be a list"
  )
})

test_that("data_validate_schema_to_table errors when types is not a named list", {

  schema <- list(
    types = list("numeric")   # unnamed list element → invalid
  )

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "`types` must be a named list"
  )
})

test_that("data_validate_schema_to_table errors on invalid type entries", {

  schema <- list(
    types = list(
      a = 123,
      b = TRUE,
      c = c("numeric","character"),
      d = list("numeric")
    )
  )

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "Invalid type entry"
  )
})

test_that("data_validate_schema_to_table accepts character allowed values", {

  schema <- list(
    allowed_values = list(x = c("a", "b", "c"))
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table accepts numeric allowed values via safe coercion", {

  schema <- list(
    allowed_values = list(x = c(1, 2, 3))
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table accepts factor allowed values", {

  schema <- list(
    allowed_values = list(
      x = factor(c("low", "high"))
    )
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table errors on nested or list allowed values", {

  schema <- list(
    allowed_values = list(
      x = list("a", "b")  # invalid
    )
  )

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "Allowed values.*must be atomic"
  )
})

test_that("data_validate_schema_to_table accepts valid pattern definitions", {
  schema <- list(patterns = list(id = "^[A-Z]+$"))
  expect_silent(data_validate_schema_to_table(schema))
})

test_that("repair_maps errors on invalid input structure", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Missing required columns
  bad_df <- data.frame(
    wrong_col = c("a", "b"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    d$repair_maps(variable_map_df = bad_df),
    regexp = "missing required columns:"
  )
})

test_that("data_validate_schema_to_table errors when patterns is not a list", {

  schema <- list(patterns = "^[A-Z]+$")

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "`patterns` must be a list"
  )
})

test_that("data_validate_schema_to_table errors when patterns is not a named list", {

  schema <- list(
    patterns = list("^[A-Z]+$")   # unnamed → invalid
  )

  expect_error(
    data_validate_schema_to_table(schema),
    regexp = "`patterns` must be a named list"
  )
})

test_that("data_validate_schema_to_table passes when some schema parts are missing", {

  schema <- list(
    required = "id",
    types = list(id = "character")
    # no allowed_values, no patterns
  )

  expect_silent(data_validate_schema_to_table(schema))
})

test_that("data_validate_schema_to_table passes with completely empty schema", {

  schema <- list()

  expect_silent(data_validate_schema_to_table(schema))
})

# DATA_TABLE_TO_SCHEMA Testing ####

test_that("data_table_to_schema converts a normal schema table correctly", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 3),
    variable         = c("id", "age", "sex"),
    value        = c("id", "age", "sex"),
    required         = c(TRUE, FALSE, TRUE),
    type             = c("character", "numeric", NA),
    question_type    = c(NA, NA, NA),
    is_other         = c(NA, NA, NA),
    other_column_link= c(NA, NA, NA),
    allowed          = c(NA, NA, "M,F"),
    col_names        = c(NA, NA, NA),
    unique           = NA,
    label            = NA,
    comment          = NA
  )

  sch <- data_table_to_schema(df)

  expect_equal(sch$required, c("id", "sex"))
  expect_equal(sch$types, list(id = "character", age = "numeric"))
  expect_equal(sch$value_map$sex, list(sex = c("M", "F")))
})

# EDGE CASES

test_that("data_table_to_schema handles empty tables", {

  df <- tibble::tibble(
    rule_type        = character(0),
    variable         = character(0),
    value        = character(0),
    required         = logical(0),
    type             = character(0),
    question_type    = character(0),
    is_other         = character(0),
    other_column_link= character(0),
    allowed          = character(0),
    col_names        = character(0),

    unique           = character(0),

    label            = character(0),
    comment          = character(0)
  )

  sch <- data_table_to_schema(df)

  expect_equal(sch$required, character(0))
  expect_identical(sch$types, list())
  expect_identical(sch$value_map, list())
})


test_that("data_table_to_schema handles NA values in optional columns", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("id", "age"),
    value        = c("id", "age"),
    required         = c(TRUE, FALSE),
    type             = c(NA, ""),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c("", NA),
    col_names        = c(NA, NA),

    unique           = NA,

    label            = NA,
    comment          = NA
  )

  sch <- data_table_to_schema(df)

  # only id is required
  expect_equal(sch$required, "id")

  # no types defined
  expect_identical(sch$types, list())

  # no allowed values
  expect_identical(sch$value_map, list())

})

test_that("data_table_to_schema trims allowed-values and splits correctly", {

  df <- tibble::tibble(
    rule_type        = "variable",
    variable         = "sex",
    value        = "sex",
    required         = FALSE,
    type             = NA,
    question_type    = NA,
    is_other         = NA,
    other_column_link= NA,
    allowed          = " M , F ,  G ",
    col_names        = NA,

    unique           = NA,

    label            = NA,
    comment          = NA
  )

  sch <- data_table_to_schema(df)

  expect_equal(sch$value_map$sex, list(sex = c("M", "F", "G")))
})


test_that("data_table_to_schema preserves the order of required variables", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 3),
    variable         = c("a", "b", "c"),
    value        = c("a", "b", "c"),
    required         = c(TRUE, TRUE, FALSE),
    type             = c(NA, NA, NA),
    question_type    = c(NA, NA, NA),
    is_other         = c(NA, NA, NA),
    other_column_link= c(NA, NA, NA),
    allowed          = c(NA, NA, NA),
    col_names        = c(NA, NA, NA),

    unique           = NA,

    label            = NA,
    comment          = NA
  )

  sch <- data_table_to_schema(df)

  expect_equal(sch$required, c("a", "b"))
})



# MULTI-ROW & DUPLICATE HANDLING


test_that("data_table_to_schema handles duplicated variables (last row wins)", {

  df <- tibble::tibble(
    rule_type        = c("variable", "variable"),
    variable         = c("id", "id"),
    value        = c("id", "id"),
    required         = c(TRUE, FALSE),            # TRUE should still win
    type             = c("character", "character"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c("A,B", "A,B,C"),         # last row wins ("A,B,C")
    col_names        = c(NA, NA),

    unique           = NA,

    label            = NA,
    comment          = NA
  )

  sch <- data_table_to_schema(df)

  # 'type' collapses to a single entry
  expect_equal(sch$types, list(id = "character"))

  # allowed_values→ last non-NA allowed row ("A,B,C")
  expect_equal(sch$value_map$id, list(id=c("A", "B", "C")) )

  # required: TRUE or FALSE taken from df$required logic ("id" is required)
  expect_equal(sch$required, "id")
})




# ERROR HANDLING


test_that("data_table_to_schema errors when input is not a dataframe", {

  expect_error(
    data_table_to_schema("not a data frame"),
    regexp = "data_table_to_schema"
  )
})


test_that("data_table_to_schema errors when required columns are missing", {

  df <- tibble::tibble(
    variable = c("id"),
    # required column missing entirely
    type     = c("character"),
    allowed  = c(NA),
    pattern  = c(NA)
  )

  expect_error(
    data_table_to_schema(df),
    regexp = "required"
  )
})


test_that("data_table_to_schema errors when variable column is missing", {

  df <- tibble::tibble(
    required = TRUE,
    type     = "character",
    allowed  = NA,
    pattern  = NA
  )

  expect_error(
    data_table_to_schema(df),
    regexp = "variable"
  )
})



# ROUND-TRIP CONSISTENCY


test_that("data_table_to_schema round-trips with data_schema_to_table", {

  # Create a fully-expanded schema TABLE
  tbl <- tibble::tibble(
    rule_type        = c("variable", "variable", "variable"),
    variable         = c("id", "age", "sex"),
    value        = c("id", "age", "sex"),
    required         = c(TRUE, FALSE, TRUE),
    type             = c("character", "numeric", NA),
    question_type    = c(NA, NA, NA),
    is_other         = c(NA, NA, NA),
    other_column_link= c(NA, NA, NA),
    allowed          = c(NA, NA, "M,F"),
    col_names        = c(NA, NA, NA),

    unique           = c("TRUE", NA, NA),

    label            = c("Record ID", NA, "Sex of respondent"),
    comment          = c("Unique identifier", NA, NA)
  )

  # Convert TABLE → SCHEMA → TABLE
  schema1 <- data_table_to_schema(tbl)
  tbl2    <- data_schema_to_table(schema1)

  # Now convert TABLE2 back into SCHEMA
  schema2 <- data_table_to_schema(tbl2)

  # --- CORE schema fields ---
  expect_equal(schema2$required, schema1$required)
  expect_equal(schema2$types, schema1$types)
  expect_equal(schema2$value_map, schema1$value_map)

  # --- Extended schema fields ---
  expect_equal(schema2$unique, schema1$unique)

  # If you add dependencies, test those also
})

test_that("data_table_to_schema parses extended fields correctly", {
  df <- tibble::tibble(
    rule_type         = "variable",
    variable          = "id",
    value         = "id",
    required          = TRUE,
    type              = "character",
    question_type     = "text",
    is_other          = "TRUE",
    other_column_link = "main_id",
    allowed           = NA_character_,
    col_names         = NA_character_,

    unique            = "TRUE",

    label             = "Identifier",
    comment           = "ID comment"
  )
  schema <- data_table_to_schema(df)
  expect_equal(schema$types, list(id = "character"))
  expect_equal(schema$unique, "id")
  expect_equal(schema$question_types, list(id = "text"))
  expect_equal(schema$is_other, list(id = TRUE))
  expect_equal(schema$other_column_link, list(id = "main_id"))
})

# Real schema template round-trip tests
#
# These tests use the provided Excel templates to ensure that real-world
# schemas can be validated and round-trip between table and schema formats
# without losing variables. They are skipped automatically if the readxl
# package is not available.

# test_that("real household schema templates validate and round-trip", {
#   testthat::skip_if_not_installed("readxl")
#   files <- c(
#     "resources/household_schema_template.xlsx",
#     "resources/household_fsl_schema_template.xlsx",
#     "resources/household_wash_schema_template.xlsx"
#   )
#   for (f in files) {
#     df <- readxl::read_excel(f)
#     # Validate the schema table
#     expect_true(data_validate_table_to_schema(df))
#     # Convert table → schema → table
#     sch1 <- data_table_to_schema(df)
#     tbl2 <- data_schema_to_table(sch1)
#     # Ensure that variables in the round-tripped table match original
#     expect_setequal(tbl2$variable, unique(df$variable))
#   }
# })


# DATA_VALIDATE_TABLE_TO_SCHEMA ####

test_that("data_validate_table_to_schema passes for valid table", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("id", "age"),
    value            = c(NA, NA),
    required         = c(TRUE, FALSE),
    type             = c("character", "numeric"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c(NA, NA),
    col_names        = c(NA, NA),
    pattern          = c(NA, NA),

    unique           = NA_character_,

    label            = NA_character_,
    comment          = NA_character_
  )

  expect_no_error(data_validate_table_to_schema(df))
})


test_that("data_validate_table_to_schema errors when schema columns missing", {
  df <- tibble::tibble(
    variable = "id",
    type = "character"
  )
  expect_error(data_validate_table_to_schema(df), regexp = "Missing required columns")
})

test_that("data_validate_table_to_schema warns on duplicate (variable, value) pairs", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("id", "id"),     # duplicated
    value            = c(NA, NA),          # both NA - same (variable, value) pair
    required         = c(TRUE, FALSE),
    type             = c("character", "character"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c(NA, NA),
    col_names        = c(NA, NA),

    unique           = NA_character_,

    label            = NA_character_,
    comment          = NA_character_
  )

  expect_warning(
    data_validate_table_to_schema(df),
    regexp = "Duplicate.*variable, value"
  )
})


test_that("data_validate_table_to_schema does not warn when unknown variables exist", {

  df <- tibble::tibble(
    rule_type        = rep("variable", 2),
    variable         = c("a", "z"),     # z not in dataset, but this function shouldn't care
    value            = c(NA, NA),
    required         = c(TRUE, FALSE),
    type             = c("numeric", "character"),
    question_type    = c(NA, NA),
    is_other         = c(NA, NA),
    other_column_link= c(NA, NA),
    allowed          = c(NA, NA),
    col_names        = c(NA, NA),

    unique           = NA_character_,

    label            = NA_character_,
    comment          = NA_character_
  )

  expect_no_warning(data_validate_table_to_schema(df))
})


test_that("data_validate_table_to_schema errors on empty table", {
  df <- tibble::tibble()

  expect_error(
    data_validate_table_to_schema(df),
    regexp = "Missing required columns"
  )
})
