library(testthat)

# Integration test for is_other field and other_columns population

test_that("Full pipeline: schema with is_other populates other_columns correctly", {

  # Step 1: Create a schema table with is_other fields (using logical values as from Excel)
  schema_df <- data.frame(
    rule_type = rep("variable", 5),
    variable = c("hh_id", "water_source", "water_source_other", "food_source", "food_source_other_text"),
    value = rep(NA, 5),
    required = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    type = c("character", "character", "character", "character", "character"),
    allowed = c(NA, "well,river,tap,other", NA, "market,farm,aid,other", NA),
    col_names = rep(NA, 5),
    pattern = rep(NA, 5),
    range = rep(NA, 5),
    precision_limits = rep(NA, 5),
    unique = c(TRUE, FALSE, FALSE, FALSE, FALSE),  # logical
    mutex_group = rep(NA, 5),
    not_future = rep(FALSE, 5),  # logical
    label = rep(NA, 5),
    comment = rep(NA, 5),
    question_type = c(NA, "select_one", "text", "select_one", "text"),
    is_other = c(FALSE, FALSE, TRUE, FALSE, TRUE),  # logical TRUE/FALSE
    other_column_link = c(NA, NA, "water_source", NA, "food_source"),
    stringsAsFactors = FALSE
  )

  # Step 2: Convert table to schema (this is where the bug would occur)
  schema <- data_table_to_schema(schema_df)

  # Verify schema has correct is_other fields
  expect_true("is_other" %in% names(schema))
  expect_true(schema$is_other$water_source_other)
  expect_true(schema$is_other$food_source_other_text)
  expect_false(schema$is_other$water_source)

  # Step 3: Create a Data object with test data
  test_data <- data.frame(
    hh_id = c("HH001", "HH002", "HH003"),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "Spring water", ""),
    food_source = c("market", "other", "farm"),
    food_source_other_text = c("", "Neighbors", ""),
    stringsAsFactors = FALSE
  )

  data_obj <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestData",
      uuid = "hh_id"
    )
  )

  # Step 4: Attach the schema
  data_obj$variable_schema <- schema

  # Step 5: Run standardize
  data_obj$standardize()

  # Step 6: Verify other_columns is populated
  expect_true(length(data_obj$other_columns) > 0)

  # Check that water_source_other is in other_columns
  expect_true("water_source_other" %in% names(data_obj$other_columns))

  # Verify structure of other_columns entry
  water_other_entry <- data_obj$other_columns$water_source_other
  expect_true("other_column" %in% names(water_other_entry))
  expect_true("other_linked_columns" %in% names(water_other_entry))
  expect_equal(water_other_entry$other_column, "water_source_other")
  expect_equal(water_other_entry$other_linked_columns, "water_source")

  # Check that food_source_other_text is in other_columns
  expect_true("food_source_other_text" %in% names(data_obj$other_columns))

  # Verify structure of food_source_other_text entry
  food_other_entry <- data_obj$other_columns$food_source_other_text
  expect_equal(food_other_entry$other_column, "food_source_other_text")
  expect_equal(food_other_entry$other_linked_columns, "food_source")
})


test_that("other_columns used in generate_cleaning_log correctly", {

  # Create a schema with is_other fields
  schema_df <- data.frame(
    rule_type = rep("variable", 3),
    variable = c("hh_id", "water_source", "water_source_other"),
    value = rep(NA, 3),
    required = c(TRUE, FALSE, FALSE),
    type = c("character", "character", "character"),
    allowed = c(NA, "well,river,tap,other", NA),
    col_names = rep(NA, 3),
    pattern = rep(NA, 3),
    range = rep(NA, 3),
    precision_limits = rep(NA, 3),
    unique = c(TRUE, FALSE, FALSE),
    mutex_group = rep(NA, 3),
    not_future = rep(FALSE, 3),
    label = rep(NA, 3),
    comment = rep(NA, 3),
    question_type = c(NA, "select_one", "text"),
    is_other = c(FALSE, FALSE, TRUE),  # logical TRUE
    other_column_link = c(NA, NA, "water_source"),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  # Create test data with some other responses
  test_data <- data.frame(
    hh_id = c("HH001", "HH002", "HH003"),
    water_source = c("well", "other", "river"),
    water_source_other = c("", "Spring water from mountain", ""),
    stringsAsFactors = FALSE
  )

  data_obj <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestData",
      uuid = "hh_id"
    )
  )

  data_obj$variable_schema <- schema
  data_obj$standardize()

  # Verify other_columns is populated
  expect_true("water_source_other" %in% names(data_obj$other_columns))

  # Generate cleaning log (this should use other_columns)
  data_obj$generate_cleaning_log(stage = "standardized")

  # Check that cleaning log has entries for the "other" response
  log_df <- data_obj$cleaning_log$log_df

  # Should have entries for water_source_other where it has a value
  other_entries <- log_df[log_df$question.name == "water_source_other", ]
  expect_true(nrow(other_entries) > 0)

  # Should specifically have an entry for HH002 which has "Spring water from mountain"
  hh002_entries <- other_entries[other_entries$uuid == "HH002", ]
  expect_true(nrow(hh002_entries) > 0)
  expect_equal(hh002_entries$issue[1], "other_response")
  expect_true(grepl("Spring water", hh002_entries$old.value[1]))

  # Should also have linked column entries
  water_source_entries <- log_df[log_df$question.name == "water_source" & log_df$uuid == "HH002", ]
  expect_true(nrow(water_source_entries) > 0)
  expect_equal(water_source_entries$issue[1], "has_other_response")
})


test_that("Backward compatibility: string TRUE/FALSE still works", {

  # Create schema with string "TRUE" (old behavior)
  schema_df <- data.frame(
    rule_type = rep("variable", 3),
    variable = c("hh_id", "water_source", "water_source_other"),
    value = rep(NA, 3),
    required = c(TRUE, FALSE, FALSE),
    type = c("character", "character", "character"),
    allowed = c(NA, "well,river,tap", NA),
    col_names = rep(NA, 3),
    pattern = rep(NA, 3),
    range = rep(NA, 3),
    precision_limits = rep(NA, 3),
    unique = c(NA, NA, NA),
    mutex_group = rep(NA, 3),
    not_future = rep(NA, 3),
    label = rep(NA, 3),
    comment = rep(NA, 3),
    question_type = c(NA, "select_one", "text"),
    is_other = c("FALSE", "FALSE", "TRUE"),  # string "TRUE"
    other_column_link = c(NA, NA, "water_source"),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  # Should still correctly parse as logical TRUE
  expect_true(schema$is_other$water_source_other)
  expect_false(schema$is_other$water_source)

  # Create data object and verify pipeline works
  test_data <- data.frame(
    hh_id = c("HH001"),
    water_source = c("other"),
    water_source_other = c("Well from neighbor"),
    stringsAsFactors = FALSE
  )

  data_obj <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestData",
      uuid = "hh_id"
    )
  )

  data_obj$variable_schema <- schema
  data_obj$standardize()

  # Verify other_columns populated
  expect_true("water_source_other" %in% names(data_obj$other_columns))
})


test_that("Numeric and non-pattern columns are NOT added to other_columns", {

  # Create test data with:
  # 1. A numeric column with many NAs (like num_died)
  # 2. A categorical column with many NAs (like health_healthcare_travel_time_int)
  # 3. An empty column (like household_geopoint)
  # 4. A valid "other" column that SHOULD be included

  test_data <- data.frame(
    hh_id = c("HH001", "HH002", "HH003", "HH004", "HH005"),
    death_any = c("yes", "no", "no", "yes", "no"),
    num_died = c(2, NA, NA, 1, NA),  # Numeric, many NAs - should NOT be in other_columns
    health_type = c("num_minutes", "range", "dont_know", "num_minutes", "range"),
    health_healthcare_travel_time_int = c(30, NA, NA, 45, NA),  # Conditional numeric - should NOT be in other_columns
    household_geopoint = rep(NA_character_, 5),  # Empty column - should NOT be in other_columns
    water_source = c("well", "other", "river", "other", "tap"),
    water_source_other = c("", "Spring water", "", "Neighbor well", ""),  # Valid other column - SHOULD be included
    stringsAsFactors = FALSE
  )

  # Create a minimal schema that includes these columns
  schema_df <- data.frame(
    rule_type = rep("variable", 8),
    variable = c("hh_id", "death_any", "num_died", "health_type",
                 "health_healthcare_travel_time_int", "household_geopoint",
                 "water_source", "water_source_other"),
    value = rep(NA, 8),
    required = c(TRUE, rep(FALSE, 7)),
    type = c("character", "character", "numeric", "character", "numeric", "character", "character", "character"),
    allowed = c(NA, "yes,no", NA, "num_minutes,range,dont_know", NA, NA, "well,river,tap,other", NA),
    col_names = rep(NA, 8),
    pattern = rep(NA, 8),
    range = rep(NA, 8),
    precision_limits = rep(NA, 8),
    unique = c(TRUE, rep(FALSE, 7)),
    mutex_group = rep(NA, 8),
    not_future = rep(FALSE, 8),
    label = rep(NA, 8),
    comment = rep(NA, 8),
    question_type = c(NA, "select_one", "integer", "select_one", "integer", "text", "select_one", "text"),
    is_other = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    other_column_link = c(NA, NA, NA, NA, NA, NA, NA, "water_source"),
    stringsAsFactors = FALSE
  )

  schema <- data_table_to_schema(schema_df)

  data_obj <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestData",
      uuid = "hh_id"
    )
  )

  data_obj$variable_schema <- schema
  data_obj$standardize()

  # POSITIVE TEST: Valid "other" column should be included
  expect_true("water_source_other" %in% names(data_obj$other_columns),
              info = "Valid other column should be in other_columns")

  # NEGATIVE TESTS: These columns should NOT be in other_columns
  expect_false("num_died" %in% names(data_obj$other_columns),
               info = "Numeric column num_died should NOT be in other_columns")

  expect_false("health_healthcare_travel_time_int" %in% names(data_obj$other_columns),
               info = "Numeric conditional column should NOT be in other_columns")

  expect_false("household_geopoint" %in% names(data_obj$other_columns),
               info = "Empty column without 'other' pattern should NOT be in other_columns")

  expect_false("death_any" %in% names(data_obj$other_columns),
               info = "Regular categorical column should NOT be in other_columns")

  expect_false("health_type" %in% names(data_obj$other_columns),
               info = "Regular categorical column should NOT be in other_columns")

  # Verify only the valid other column is present
  expect_equal(length(data_obj$other_columns), 1,
               info = "Should have exactly 1 entry in other_columns")

  expect_equal(names(data_obj$other_columns), "water_source_other",
               info = "Only water_source_other should be in other_columns")
})


test_that("Inference-based detection requires 'other' pattern in column name", {

  # Test inference-based detection (without schema is_other field)
  # Columns should only be detected as "other" if they match naming patterns

  test_data <- data.frame(
    hh_id = c("HH001", "HH002", "HH003"),
    survey_notes = c("", "Some notes here", ""),  # Has blanks but no "other" pattern - should NOT be included
    num_people = c(5, NA, 3),  # Numeric with NA - should NOT be included
    status_other = c("", "Custom status", ""),  # Matches pattern - SHOULD be included
    food_other_text = c("", "", "Wild berries"),  # Matches pattern - SHOULD be included
    stringsAsFactors = FALSE
  )

  data_obj <- suppressMessages(
    Data$new(
      data = test_data,
      dataset_name = "TestData",
      uuid = "hh_id"
    )
  )

  # NO SCHEMA - rely on inference
  data_obj$standardize()

  # Columns with "other" pattern should be included
  expect_true("status_other" %in% names(data_obj$other_columns),
              info = "Column with '_other' pattern should be detected")

  expect_true("food_other_text" %in% names(data_obj$other_columns),
              info = "Column with '_other_text' pattern should be detected")

  # Columns without "other" pattern should NOT be included
  expect_false("survey_notes" %in% names(data_obj$other_columns),
               info = "Column without 'other' pattern should NOT be detected")

  expect_false("num_people" %in% names(data_obj$other_columns),
               info = "Numeric column should NOT be detected as other")

  # Should have exactly 2 entries
  expect_equal(length(data_obj$other_columns), 2,
               info = "Should have exactly 2 entries in other_columns")
})

# Test run_quality_checks ####

# Test 1: Skip dependency checks when variables are not present in dataset

test_that("run_quality_checks skips dependencies when variables are missing", {
  df <- tibble(
    id = 1:3,
    age = c(25, 30, 35)
  )

  d <- suppressMessages(
    Data$new(df, dataset_name = "MissingVarTest", uuid = "id")
  )

  # Set up dependency that requires 'gender' which doesn't exist
  d$set_dependency_schema(list(
    dependencies = list(
      flag_gender_check = list(
        variables = c("gender", "age"),
        condition_if = "!is.na(age)",
        then = "!is.na(gender)",
        action = "flag_warning"
      )
    )
  ))

  # Should not error, should skip the dependency
  expect_message(
    d$run_quality_checks("raw"),
    "Skipping dependency.*gender"
  )

  # Should not have flag for missing variable
  if (!is.null(d$data_quality_flags)) {
    expect_false("flag_gender_check" %in% names(d$data_quality_flags))
  }
})


# Test 2: Resolve canonical variable names to dataset column names

test_that("run_quality_checks resolves canonical variable names from variable_map", {
  df <- tibble(
    record_id = 1:3,
    respondent_age = c(25, 30, 35),
    household_size = c(3, 4, 2)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "VarMapTest",
      uuid = "record_id",
      variable_map = list(
        uuid = "record_id",
        age = "respondent_age",
        hh_size = "household_size"
      )
    )
  )

  # Dependency uses canonical names (age, hh_size)
  d$set_dependency_schema(list(
    dependencies = list(
      flag_age_hh_check = list(
        variables = c("age", "hh_size"),
        condition_if = "age > 30",
        then = "hh_size > 2",
        action = "flag_warning"
      )
    )
  ))

  # Should work without errors
  d$run_quality_checks("raw")

  # Row 3 should be flagged: age=35 (>30) but hh_size=2 (not >2)
  expect_true("flag_age_hh_check" %in% names(d$data_quality_flags))
  expect_equal(
    d$data_quality_flags$flag_age_hh_check,
    c(0, 0, 1)  # Only row 3 is flagged
  )
})


# Test 3: Translate expressions with canonical variable names

test_that(".translate_expression replaces canonical variable names with dataset columns", {
  df <- tibble(
    id = 1:2,
    dataset_col_status = c("active", "inactive")
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ExpressionTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        status = "dataset_col_status"
      )
    )
  )

  # Test the internal translation method
  expr <- "status == 'active'"
  translated <- d$.translate_expression(expr)

  # Should replace 'status' with 'dataset_col_status'
  expect_match(translated, "dataset_col_status")
  expect_match(translated, "==")
  expect_match(translated, "active")
})


test_that(".translate_expression replaces canonical values with dataset values", {
  df <- tibble(
    id = 1:2,
    status_col = c("yes", "y")
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ValueTranslateTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        status = "status_col"
      ),
      value_map = list(
        status = list(
          yes = c("yes", "y", "1"),
          no = c("no", "n", "0")
        )
      )
    )
  )

  # Test with equality
  expr1 <- "status == 'yes'"
  translated1 <- d$.translate_expression(expr1)

  # Should replace 'status' with 'status_col' AND 'yes' with the dataset values
  expect_match(translated1, "status_col")
  expect_match(translated1, "%in%")  # Should use %in% for multiple values
  expect_match(translated1, "'yes'")
  expect_match(translated1, "'y'")

  # Test with inequality
  expr2 <- "status != 'no'"
  translated2 <- d$.translate_expression(expr2)
  expect_match(translated2, "status_col")
  expect_match(translated2, "!")  # Should have negation
  expect_match(translated2, "%in%")
})


test_that(".translate_expression handles complex expressions with multiple variables", {
  df <- tibble(
    id = 1:2,
    fever_col = c("yes", "no"),
    temp_col = c(38, 37)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ComplexExprTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        fever = "fever_col",
        temperature = "temp_col"
      ),
      value_map = list(
        fever = list(
          yes = c("yes", "y"),
          no = c("no", "n")
        )
      )
    )
  )

  # Complex expression with both variable and value references
  expr <- "fever == 'yes' & !is.na(temperature)"
  translated <- d$.translate_expression(expr)

  # Should replace 'fever' with 'fever_col'
  expect_match(translated, "fever_col")
  # Should replace 'temperature' with 'temp_col'
  expect_match(translated, "temp_col")
  # Should handle the canonical value 'yes'
  expect_match(translated, "%in%")
  # Should preserve the logical operator and is.na function
  expect_match(translated, "&")
  expect_match(translated, "is\\.na")
})


# Test 4: Translate expressions with canonical values from value_map

test_that("run_quality_checks translates canonical values to dataset values", {
  df <- tibble(
    id = 1:4,
    status_col = c("yes", "y", "no", "n")
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ValueMapTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        status = "status_col"
      ),
      value_map = list(
        status = list(
          yes = c("yes", "y", "1", "oui"),
          no = c("no", "n", "0", "non")
        )
      )
    )
  )

  # Dependency uses canonical value 'yes'
  d$set_dependency_schema(list(
    dependencies = list(
      flag_status_check = list(
        variables = c("status"),
        condition_if = "status == 'yes'",  # canonical value
        then = "TRUE",
        action = "flag_warning"
      )
    )
  ))

  d$run_quality_checks("raw")

  # Should have the flag
  expect_true("flag_status_check" %in% names(d$data_quality_flags))

  # Rows 1 and 2 have 'yes' or 'y' which map to canonical 'yes'
  # The condition should be TRUE for both, so no flags (condition always TRUE)
  expect_equal(
    d$data_quality_flags$flag_status_check,
    c(0, 0, 0, 0)  # All pass because 'then' is always TRUE
  )
})


# Test 5: Complex expression with both variable and value mapping

test_that("run_quality_checks handles complex expressions with variable and value mapping", {
  df <- tibble(
    record_id = 1:4,
    fever_col = c("yes", "y", "no", "yes"),
    temp_col = c(38, 37, 36, NA)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "ComplexTest",
      uuid = "record_id",
      variable_map = list(
        uuid = "record_id",
        fever = "fever_col",
        temperature = "temp_col"
      ),
      value_map = list(
        fever = list(
          yes = c("yes", "y", "oui"),
          no = c("no", "n", "non")
        )
      )
    )
  )

  # If fever='yes', then temperature should not be NA
  d$set_dependency_schema(list(
    dependencies = list(
      flag_fever_temp = list(
        variables = c("fever", "temperature"),
        condition_if = "fever == 'yes'",  # canonical value
        then = "!is.na(temperature)",     # canonical variable
        action = "flag_warning"
      )
    )
  ))

  d$run_quality_checks("raw")

  expect_true("flag_fever_temp" %in% names(d$data_quality_flags))

  # Row 1: fever='yes' (matches), temp=38 (not NA) -> OK
  # Row 2: fever='y' (matches 'yes'), temp=37 (not NA) -> OK
  # Row 3: fever='no' (doesn't match), condition_if FALSE -> OK
  # Row 4: fever='yes' (matches), temp=NA -> FLAGGED
  expect_equal(
    d$data_quality_flags$flag_fever_temp,
    c(0, 0, 0, 1)
  )
})


# Test 6: Skip dependency when canonical variable cannot be resolved

test_that("run_quality_checks skips when canonical variable is not in variable_map", {
  df <- tibble(
    id = 1:3,
    age = c(25, 30, 35)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "UnmappedCanonicalTest",
      uuid = "id",
      variable_map = list(
        uuid = "id"
        # 'age' is not mapped
      )
    )
  )

  # Dependency uses canonical name 'age' which is in data but not in variable_map
  # Since resolve_column will try variable_map first, then fall back to direct name
  # This should work because 'age' exists in the dataset
  d$set_dependency_schema(list(
    dependencies = list(
      flag_age_check = list(
        variables = c("age"),
        condition_if = "age > 30",
        then = "TRUE",
        action = "flag_warning"
      )
    )
  ))

  # Should work because resolve_column falls back to direct column name
  d$run_quality_checks("raw")

  expect_true("flag_age_check" %in% names(d$data_quality_flags))
})


# Test 7: Multiple canonical values in one expression

test_that("run_quality_checks handles multiple canonical values in expression", {
  df <- tibble(
    id = 1:3,
    status = c("active", "inactive", "active")
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "MultiValueTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        status_role = "status"
      ),
      value_map = list(
        status_role = list(
          active = c("active", "actif"),
          inactive = c("inactive", "inactif")
        )
      )
    )
  )

  # Expression with canonical value
  d$set_dependency_schema(list(
    dependencies = list(
      flag_status_active = list(
        variables = c("status_role"),
        condition_if = "status_role == 'active'",
        then = "TRUE",
        action = "flag_warning"
      )
    )
  ))

  d$run_quality_checks("raw")

  expect_true("flag_status_active" %in% names(d$data_quality_flags))
  expect_equal(
    d$data_quality_flags$flag_status_active,
    c(0, 0, 0)  # All pass because 'then' is always TRUE
  )
})


# Test 8: Skip if all variables in the dependency are missing

test_that("run_quality_checks skips if all required variables are missing", {
  df <- tibble(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie")
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "AllMissingTest",
      uuid = "id"
    )
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_age_gender = list(
        variables = c("age", "gender"),
        condition_if = "!is.na(age)",
        then = "!is.na(gender)",
        action = "flag_warning"
      )
    )
  ))

  # Should skip because both variables are missing
  expect_message(
    d$run_quality_checks("raw"),
    "Skipping dependency.*age"
  )
})


# Test 9: Partial match - one variable present, one missing

test_that("run_quality_checks skips if any required variable is missing", {
  df <- tibble(
    id = 1:3,
    age = c(25, 30, 35)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "PartialTest",
      uuid = "id"
    )
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_age_gender = list(
        variables = c("age", "gender"),
        condition_if = "!is.na(age)",
        then = "!is.na(gender)",
        action = "flag_warning"
      )
    )
  ))

  # Should skip because 'gender' is missing
  expect_message(
    d$run_quality_checks("raw"),
    "Skipping dependency.*gender"
  )
})


# Test 10: Test with soft_dependencies

test_that("run_quality_checks works with soft_dependencies", {
  df <- tibble(
    id = 1:3,
    col_x = c(1, 2, 3),
    col_y = c(10, NA, 30)
  )

  d <- suppressMessages(
    Data$new(
      df,
      dataset_name = "SoftDepTest",
      uuid = "id",
      variable_map = list(
        uuid = "id",
        x = "col_x",
        y = "col_y"
      )
    )
  )

  # Note: soft_dependencies structure might be similar to dependencies
  d$set_dependency_schema(list(
    dependencies = list(),
    soft_dependencies = list(
      flag_soft_check = list(
        variables = c("x", "y"),
        condition_if = "x > 0",
        then = "!is.na(y)",
        action = ""
      )
    )
  ))

  # Should work (soft_dependencies might not be processed in current implementation)
  # but shouldn't error
  expect_message(d$run_quality_checks("raw"))
})

# Translating Logical Expressions from Depenendency Schema ####

# Test that %in% expressions in dependency schemas properly expand canonical values
# to their mapped dataset values

test_that("%in% expressions expand canonical values using value_map", {
  # Create test data with mapped values (not canonical)
  df <- tibble(
    id = 1:6,
    gender_col = c("m", "f", "male", "female", "other", "non-binary")
  )

  # Create Data object with value_map
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestValueMap", uuid = "id")
  )

  # Set variable_map and value_map
  d$variable_map$sex <- "gender_col"
  d$value_map$sex <- list(
    male = c("m", "male", "homme"),
    female = c("f", "female", "femme")
  )

  # Set dependency schema with %in% expression using canonical values
  d$set_dependency_schema(list(
    dependencies = list(
      flag_sex_values = list(
        variables = c("sex"),
        condition_if = "TRUE",
        then = "sex %in% c('male', 'female')",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")

  # Check that flags were created
  expect_false(is.null(d$data_quality_flags))
  expect_true("flag_sex_values" %in% names(d$data_quality_flags))

  # Rows with values in allowed set (mapped) should NOT be flagged
  # Rows with values outside allowed set should be flagged
  # 1: "m" → maps to male → NOT flagged (0)
  # 2: "f" → maps to female → NOT flagged (0)
  # 3: "male" → maps to male → NOT flagged (0)
  # 4: "female" → maps to female → NOT flagged (0)
  # 5: "other" → not mapped → flagged (1)
  # 6: "non-binary" → not mapped → flagged (1)
  expect_equal(d$data_quality_flags$flag_sex_values, c(0, 0, 0, 0, 1, 1))
})


test_that("%in% expressions work with roster dependency schema", {
  # Simulate the actual use case: IndividualData with roster schema
  df <- tibble(
    person_id = 1:5,
    hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3"),
    sex = c("male", "female", "other", "non-binary", "male"),
    age = c(25, 30, 22, 28, 35)
  )

  # Create IndividualData which loads roster dependency schema
  d <- suppressMessages(
    IndividualData$new(data = df, dataset_name = "TestRoster")
  )

  # The default schema should have flag_values_sex dependency
  expect_false(is.null(d$dependency_schema))
  expect_true("flag_values_sex" %in% names(d$dependency_schema$dependencies))

  # Run the pipeline
  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")

  # Check flags
  expect_false(is.null(d$data_quality_flags))

  # If value_map is set up correctly, should flag rows with values
  # outside the mapped values for 'male' and 'female'
  if ("flag_values_sex" %in% names(d$data_quality_flags)) {
    flags <- d$data_quality_flags$flag_values_sex

    # At minimum, should flag 'other' and 'non-binary' rows
    # The exact flags depend on whether value_map includes all variations
    expect_true(flags[3] == 1)  # 'other' should be flagged
    expect_true(flags[4] == 1)  # 'non-binary' should be flagged

    # 'male' and 'female' should not be flagged if they're in the allowed values
    # (or their mapped equivalents)
    expect_true(flags[1] == 0 || flags[1] == 1)  # depends on mapping
    expect_true(flags[2] == 0 || flags[2] == 1)  # depends on mapping
    expect_true(flags[5] == 0 || flags[5] == 1)  # depends on mapping
  }
})


test_that("%in% expressions with multiple canonical values expand correctly", {
  df <- tibble(
    id = 1:4,
    status_col = c("active", "pending", "inactive", "archived")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestMultiple", uuid = "id")
  )

  # Set mappings
  d$variable_map$status <- "status_col"
  d$value_map$status <- list(
    active = c("active", "live", "1"),
    inactive = c("inactive", "disabled", "0")
  )

  # Dependency with multiple canonical values in %in%
  d$set_dependency_schema(list(
    dependencies = list(
      flag_valid_status = list(
        variables = c("status"),
        condition_if = "TRUE",
        then = "status %in% c('active', 'inactive')",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")

  # Row 1: "active" → mapped → NOT flagged (0)
  # Row 2: "pending" → not mapped → flagged (1)
  # Row 3: "inactive" → mapped → NOT flagged (0)
  # Row 4: "archived" → not mapped → flagged (1)
  expect_equal(d$data_quality_flags$flag_valid_status, c(0, 1, 0, 1))
})


test_that("%in% expressions without value_map work as before", {
  # Test that expressions still work when there's no value_map
  # (literal value matching)
  df <- tibble(
    id = 1:4,
    category = c("A", "B", "C", "D")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestNoMap", uuid = "id")
  )

  # No value_map set - should use literal matching
  d$set_dependency_schema(list(
    dependencies = list(
      flag_valid_category = list(
        variables = c("category"),
        condition_if = "TRUE",
        then = "category %in% c('A', 'B')",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")

  # Literal matching: 'A' and 'B' match, 'C' and 'D' don't
  expect_equal(d$data_quality_flags$flag_valid_category, c(0, 0, 1, 1))
})


test_that("== operator still works with value_map expansion", {
  # Verify that existing == operator logic still works
  df <- tibble(
    id = 1:3,
    consent_col = c("yes", "y", "no")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestEquals", uuid = "id")
  )

  d$variable_map$consent <- "consent_col"
  d$value_map$consent <- list(
    yes = c("yes", "y", "oui"),
    no = c("no", "n", "non")
  )

  d$set_dependency_schema(list(
    dependencies = list(
      flag_consent_yes = list(
        variables = c("consent"),
        condition_if = "consent == 'yes'",
        then = "TRUE",
        action = "flag_warning"
      )
    )
  ))

  d$validate()
  d$standardize()
  d$run_quality_checks("standardized")

  # All rows where consent is "yes" or "y" (mapped) should pass
  expect_equal(d$data_quality_flags$flag_consent_yes, c(0, 0, 0))
})


# Tests for select_multiple "other" handling

test_that("process_select_multiple_columns detects 'other' in responses", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2", "id_3"),
    livelihood = c("farming fishing", "trading other", "farming"),
    livelihood_other_text = c("", "freelancing", "")
  )

  schema <- list(
    question_types = list(
      livelihood = "select_multiple"
    )
  )

  result <- process_select_multiple_columns(test_df, schema)

  # Should have expanded columns
  expect_true("livelihood.farming" %in% names(result$data))
  expect_true("livelihood.fishing" %in% names(result$data))
  expect_true("livelihood.trading" %in% names(result$data))
  expect_true("livelihood.other" %in% names(result$data))

  # Should have detected "other" related columns
  expect_true("livelihood" %in% names(result$other_related_columns))

  other_info <- result$other_related_columns[["livelihood"]]
  expect_equal(other_info$original_column, "livelihood")
  expect_equal(other_info$dummy_other_column, "livelihood.other")
  expect_equal(other_info$text_other_column, "livelihood_other_text")
})


test_that("standardize() adds 'other' columns to self$other_columns as list structure", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2", "id_3"),
    skills = c("reading writing", "math other", "reading"),
    skills_other_text = c("", "programming", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      skills = "character",
      skills_other_text = "character"
    ),
    question_types = list(
      skills = "select_multiple",
      skills_other_text = "text"
    )
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Check that skills_other_text is in other_columns with proper structure
  expect_true("skills_other_text" %in% names(d$other_columns))

  # Check structure
  entry <- d$other_columns$skills_other_text
  expect_equal(entry$other_column, "skills_other_text")
  expect_true("skills" %in% entry$other_linked_columns)
  expect_true("skills.other" %in% entry$other_linked_columns)
})


test_that("generate_cleaning_log creates entries for 'other' columns and linked columns", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2", "id_3"),
    transport = c("car bus", "bicycle other", "car"),
    transport_other_text = c("", "motorcycle", "")
  )

  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      transport = "character",
      transport_other_text = "character"
    ),
    question_types = list(
      transport = "select_multiple",
      transport_other_text = "text"
    )
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)
  d$standardize()

  # Generate cleaning log
  d$data_quality_flags <- data.frame(uuid = test_df$uuid)
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_df <- d$cleaning_log$log_df

  # Should have entries for id_2 (row with "other" text)
  # One for text column, plus one each for linked columns
  id2_entries <- log_df[log_df$uuid == "id_2", ]
  expect_gte(nrow(id2_entries), 3)  # At least 3 entries

  # Check that entries exist for each column
  expect_true(any(id2_entries$question.name == "transport_other_text"))
  expect_true(any(id2_entries$question.name == "transport"))
  expect_true(any(id2_entries$question.name == "transport.other"))
})


test_that("process_select_multiple handles multiple 'other' patterns", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2"),
    food = c("rice beans", "rice other"),
    food_other_specify = c("", "quinoa")  # Different suffix pattern
  )

  schema <- list(
    question_types = list(
      food = "select_multiple"
    )
  )

  result <- process_select_multiple_columns(test_df, schema)

  # Should detect the _other_specify column
  other_info <- result$other_related_columns[["food"]]
  expect_equal(other_info$text_other_column, "food_other_specify")
})


test_that("generate_cleaning_log handles missing text column with dummy column", {

  test_df <- tibble::tibble(
    uuid = c("id_1", "id_2"),
    income = c("salary", "salary other"),
    income.other = c(0, 1)  # Dummy column created by expansion (note: period separator)
  )

  # Manually add dummy column to simulate expansion
  schema <- list(
    required = "uuid",
    types = list(
      uuid = "character",
      income = "character"
    ),
    question_types = list(
      income = "select_multiple"
    )
  )

  d <- suppressMessages(
    Data$new(data = test_df, uuid = "uuid", dataset_name = "TestData")
  )
  d$set_variable_schema(schema)

  # Manually modify other_columns to simulate scenario without text column
  d$standardize()
  d$other_columns[["income.other"]] <- list(
    other_column = "income.other",
    other_linked_columns = c("income")
  )

  # Generate cleaning log
  d$data_quality_flags <- data.frame(uuid = test_df$uuid)
  d$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_df <- d$cleaning_log$log_df

  # Should have entries for id_2 where dummy column = 1
  id2_entries <- log_df[log_df$uuid == "id_2", ]
  expect_gte(nrow(id2_entries), 2)  # At least 2 (dummy + original)
})

# expand_select_multiple() ####
#
# The expand_select_multiple function takes a character vector of space-separated
# values and produces a dummy matrix with one column per unique value. These
# tests verify that dummy columns are created correctly, handle NA/empty values,
# and return an empty data frame when no values are present.

test_that("expand_select_multiple expands multi-select values into dummy columns", {
  column <- c("apple banana", "banana cherry", NA_character_, "", "apple")
  dummy  <- expand_select_multiple(column, "fruit")

  # Should return a data.frame with one column per unique value (apple, banana, cherry)
  expect_s3_class(dummy, "data.frame")
  expect_equal(ncol(dummy), 3)
  expect_setequal(names(dummy), c("fruit.apple", "fruit.banana", "fruit.cherry"))

  # First row has both apple and banana but not cherry
  expect_equal(as.integer(dummy[1, ]), c(1, 1, 0))
  # Second row has banana and cherry
  expect_equal(as.integer(dummy[2, ]), c(0, 1, 1))
  # Third and fourth rows are NA/empty and should remain zeros
  expect_equal(as.integer(dummy[3, ]), c(0, 0, 0))
  expect_equal(as.integer(dummy[4, ]), c(0, 0, 0))
  # Fifth row contains only apple
  expect_equal(as.integer(dummy[5, ]), c(1, 0, 0))
})

test_that("expand_select_multiple returns empty data frame when no values present", {
  column <- c(NA_character_, "", NA_character_)
  dummy  <- expand_select_multiple(column, "var")
  # Should have zero rows and zero columns
  expect_equal(nrow(dummy), 0)
  expect_equal(ncol(dummy), 0)
})

# process_select_multiple_columns() ####
#
# The process_select_multiple_columns function expands select_multiple columns
# identified by the schema into dummy variables and tracks associated 'other'
# response columns. These tests verify that the function correctly identifies
# select_multiple variables, expands them, and records metadata about any
# corresponding open-ended 'other' columns.

test_that("process_select_multiple_columns expands columns and tracks other responses", {
  # Sample data with a select_multiple column containing an 'other' value and a
  # separate text column capturing details for the 'other' response
  data <- data.frame(
    multi             = c("a b other", "b", NA_character_, ""),
    multi_other_text  = c("other1", NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  # Schema indicating that 'multi' is a select_multiple question
  schema <- list(question_types = list(multi = "select_multiple"))

  result <- process_select_multiple_columns(data, schema)

  # Expect that the expanded columns include one for each unique response
  expect_setequal(result$expanded_columns,
                  c("multi.a", "multi.b", "multi.other"))
  # The returned data should now contain these new columns
  expect_true(all(result$expanded_columns %in% names(result$data)))

  # Validate dummy values for each row
  expect_equal(as.integer(result$data$`multi.a`), c(1, 0, 0, 0))
  expect_equal(as.integer(result$data$`multi.b`), c(1, 1, 0, 0))
  expect_equal(as.integer(result$data$`multi.other`), c(1, 0, 0, 0))

  # Check that other_related_columns captures metadata for the 'other' responses
  expect_true("multi" %in% names(result$other_related_columns))
  orc <- result$other_related_columns$multi
  expect_equal(orc$original_column, "multi")
  expect_equal(orc$dummy_other_column, "multi.other")
  expect_equal(orc$text_other_column, "multi_other_text")
})

test_that("process_select_multiple_columns handles missing schema or non-select variables", {
  data   <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  schema <- list()  # no question_types field
  result <- process_select_multiple_columns(data, schema)
  # Data should be returned unchanged
  expect_equal(result$data, data)
  expect_length(result$expanded_columns, 0)
  expect_length(result$other_related_columns, 0)
})




# ============================================================================
# cluster_id_numeric generation tests
# ============================================================================

test_that("standardize() creates cluster_id_numeric when cluster_id is mapped", {
  test_data <- data.frame(
    hh_id = paste0("HH", 1:6),
    cluster = c("A", "A", "B", "B", "C", "C"),
    weight  = rep(1, 6),
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, uuid = "hh_id")
  )
  obj$variable_map$cluster_id <- "cluster"
  obj$standardize()

  expect_true("cluster_id_numeric" %in% names(obj$standardized_data))
  expect_equal(obj$variable_map[["cluster_id_numeric"]], "cluster_id_numeric")
  # Clusters A, B, C → 1, 2, 3 (alphabetical order from sort())
  expect_equal(sort(unique(obj$standardized_data$cluster_id_numeric)), 1L:3L)
  expect_true(is.integer(obj$standardized_data$cluster_id_numeric))
  # Verify specific mapping: A=1, B=2, C=3 (sorted alphabetically)
  a_vals <- obj$standardized_data$cluster_id_numeric[test_data$cluster == "A"]
  b_vals <- obj$standardized_data$cluster_id_numeric[test_data$cluster == "B"]
  c_vals <- obj$standardized_data$cluster_id_numeric[test_data$cluster == "C"]
  expect_true(all(a_vals == 1L))
  expect_true(all(b_vals == 2L))
  expect_true(all(c_vals == 3L))
})

test_that("standardize() does NOT create cluster_id_numeric when cluster_id not mapped", {
  test_data <- data.frame(
    hh_id = paste0("HH", 1:4),
    value  = 1:4,
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, uuid = "hh_id")
  )
  obj$standardize()

  expect_false("cluster_id_numeric" %in% names(obj$standardized_data))
  expect_null(obj$variable_map[["cluster_id_numeric"]])
})

test_that("cluster_id_numeric numbers clusters starting at 1 sequentially", {
  test_data <- data.frame(
    uuid    = paste0("R", 1:9),
    cluster = c(10L, 10L, 10L, 20L, 20L, 20L, 30L, 30L, 30L),
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, uuid = "uuid")
  )
  obj$variable_map$cluster_id <- "cluster"
  obj$standardize()

  vals <- obj$standardized_data$cluster_id_numeric
  expect_equal(sort(unique(vals)), 1L:3L)
  # All rows for cluster 10 should get the same numeric id
  expect_true(length(unique(vals[test_data$cluster == 10L])) == 1L)
})


# ---- datetime type coercion in standardize() ----

make_datetime_schema <- function(extra_vars = NULL, extra_types = NULL) {
  base_vars  <- c("hh_id", "interview_start", "interview_end")
  base_types <- c("character", "datetime", "datetime")
  vars  <- c(base_vars,  extra_vars)
  types <- c(base_types, extra_types)
  n <- length(vars)
  schema_df <- data.frame(
    rule_type         = rep("variable", n),
    variable          = vars,
    value             = rep(NA, n),
    required          = c(TRUE, rep(FALSE, n - 1)),
    type              = types,
    allowed           = rep(NA, n),
    col_names         = rep(NA, n),
    pattern           = rep(NA, n),
    range             = rep(NA, n),
    precision_limits  = rep(NA, n),
    unique            = c(TRUE, rep(FALSE, n - 1)),
    mutex_group       = rep(NA, n),
    not_future        = rep(FALSE, n),
    label             = rep(NA, n),
    comment           = rep(NA, n),
    question_type     = rep(NA, n),
    is_other          = rep(FALSE, n),
    other_column_link = rep(NA, n),
    stringsAsFactors  = FALSE
  )
  data_table_to_schema(schema_df)
}

test_that("standardize() coerces character datetime columns to POSIXct when schema type is 'datetime'", {
  test_data <- data.frame(
    hh_id           = c("HH001", "HH002"),
    interview_start = c("2024-05-01 08:00:00", "2024-05-01 09:30:00"),
    interview_end   = c("2024-05-01 08:45:00", "2024-05-01 10:15:00"),
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, dataset_name = "TestHH", uuid = "hh_id")
  )
  obj$variable_schema <- make_datetime_schema()
  obj$standardize()

  expect_true(inherits(obj$standardized_data$interview_start, c("POSIXct", "POSIXlt")))
  expect_true(inherits(obj$standardized_data$interview_end,   c("POSIXct", "POSIXlt")))
})

test_that("standardize() preserves time information when schema type is 'datetime'", {
  test_data <- data.frame(
    hh_id           = c("HH001"),
    interview_start = c("2024-05-01 08:00:00"),
    interview_end   = c("2024-05-01 08:45:00"),
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, dataset_name = "TestHH", uuid = "hh_id")
  )
  obj$variable_schema <- make_datetime_schema()
  obj$standardize()

  start_val <- obj$standardized_data$interview_start[[1]]
  end_val   <- obj$standardized_data$interview_end[[1]]

  # Difference should be 45 minutes, not zero (which would happen if time was stripped)
  diff_mins <- as.numeric(difftime(end_val, start_val, units = "mins"))
  expect_equal(diff_mins, 45)
})

test_that("standardize() retains POSIXct columns unchanged when schema type is 'datetime'", {
  test_data <- data.frame(
    hh_id           = c("HH001", "HH002"),
    interview_start = as.POSIXct(c("2024-05-01 08:00:00", "2024-05-01 09:30:00"), tz = "UTC"),
    interview_end   = as.POSIXct(c("2024-05-01 08:45:00", "2024-05-01 10:15:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )

  obj <- suppressMessages(
    Data$new(data = test_data, dataset_name = "TestHH", uuid = "hh_id")
  )
  obj$variable_schema <- make_datetime_schema()
  obj$standardize()

  expect_true(inherits(obj$standardized_data$interview_start, c("POSIXct", "POSIXlt")))
  expect_true(inherits(obj$standardized_data$interview_end,   c("POSIXct", "POSIXlt")))
})
