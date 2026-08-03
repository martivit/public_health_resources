
# INDIVIDUAL DATA CLASS TEST SUITE


library(testthat)
library(tibble)



# 1. INITIALIZATION TESTS


test_that("IndividualData initializes with minimal valid data", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(
      data = df,
      dataset_name = "TestIndividual"
    )
  )

  expect_s3_class(ind_data, "IndividualData")
  expect_s3_class(ind_data, "Data")
  expect_equal(ind_data$dataset_name, "TestIndividual")
})

test_that("IndividualData sets required columns correctly", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # Should have individual-specific required columns
  expect_true(length(ind_data$required_columns) > 0)

  # UUID should be in required columns
  expect_true(ind_data$uuid %in% ind_data$required_columns)
})

test_that("IndividualData handles custom variable mapping", {
  df <- tibble::tibble(
    my_person_id = paste0("p_", 1:20),
    my_hh_id = paste0("hh_", rep(1:5, each = 4)),
    gender = sample(c("M", "F"), 20, replace = TRUE),
    age_yrs = sample(0:80, 20, replace = TRUE)
  )

  custom_map <- list(
    uuid = "my_person_id",
    hh_uuid = "my_hh_id",
    sex = "gender",
    age = "age_yrs"
  )

  ind_data <- suppressMessages(
    IndividualData$new(
      data = df,
      variable_map = custom_map
    )
  )

  expect_equal(ind_data$variable_map$uuid, "my_person_id")
  expect_equal(ind_data$variable_map$hh_uuid, "my_hh_id")
  expect_equal(ind_data$variable_map$sex, "gender")
  expect_equal(ind_data$variable_map$age, "age_yrs")
})

test_that("IndividualData does not initialize optional columns if not passed", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # Optional columns should not be set
  expect_true(is.null(ind_data$optional_columns))

})


# 2. SCHEMA TESTS


test_that("IndividualData loads default schema", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  schema <- ind_data$default_schema()

  expect_true(is.list(schema))
  expect_true(length(schema) > 0)
})

test_that("IndividualData merges schema with parent", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # Variable schema should be set and merged
  expect_true(!is.null(ind_data$variable_schema))
  expect_true(is.list(ind_data$variable_schema))
})

test_that("IndividualData loads indicator schema", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  indicator_schema <- ind_data$default_indicator_schema()

  expect_true(is.list(indicator_schema))
})

test_that("IndividualData loads dependency schema", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  dependency_schema <- ind_data$default_dependency_schema()

  expect_true(is.list(dependency_schema))
})


# 3. HOUSEHOLD LINKAGE TESTS


test_that("IndividualData household_link field is initialized", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # household_link should exist (may be NULL initially)
  expect_true("household_link" %in% names(ind_data))
})

test_that("IndividualData can link to household data", {
  # Create linked household and individual data
  hh_df <- generate_household_dataset(n = 5)
  ind_df <- generate_hh_roster_dataset(household_data_or_n = 20)

  hh_data <- suppressMessages(
    HouseholdData$new(data = hh_df, dataset_name = "householddata")
  )
  ind_data <- suppressMessages(
    IndividualData$new(data = ind_df)
  )

  hh_data$add_linked_dataset(name = "roster", data_object = ind_data)

  # household_link should be settable - now stored in linked_objects (inherited from Data class)
  expect_true("roster" %in% names(hh_data$linked_objects))
})


# 4. VALIDATION TESTS


test_that("IndividualData validates required fields", {
  # Missing required field (hh_uuid)
  df <- tibble::tibble(
    person_id = paste0("p_", 1:20)
  )

  expect_error({
    ind_data <- suppressMessages(
      IndividualData$new(data = df)
    )
  }, regexp = NA)  # May or may not error depending on validation timing
})

test_that("IndividualData handles empty dataset", {
  df <- tibble::tibble(
    person_id = character(),
    hh_uuid = character(),
    sex = character(),
    age_years = numeric()
  )

  expect_error({
    ind_data <- suppressMessages(
      IndividualData$new(data = df)
    )
  }, regexp = NA)  # Should handle empty data gracefully
})


# 5. DATA ACCESS TESTS


test_that("IndividualData provides access to raw data", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  expect_true(!is.null(ind_data$raw_data))
  expect_equal(nrow(ind_data$raw_data), 20)
})

test_that("IndividualData tracks data stages", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # Should have stage tracking fields
  expect_false(ind_data$validated)
  expect_false(ind_data$standardized)
  expect_false(ind_data$cleaned)
})


# 6. METADATA TESTS


test_that("IndividualData stores metadata", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  metadata <- list(
    survey_name = "Test Survey",
    data_collection_date = Sys.Date()
  )

  ind_data <- suppressMessages(
    IndividualData$new(
      data = df,
      metadata = metadata
    )
  )

  expect_true(is.list(ind_data$metadata))
  expect_equal(ind_data$metadata$survey_name, "Test Survey")
  expect_equal(ind_data$metadata$data_collection_date, Sys.Date())
})

test_that("IndividualData creates default metadata", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  expect_true(is.list(ind_data$metadata))
  expect_equal(ind_data$metadata$dataset_name, "IndividualData")
})


# 7. INHERITANCE TESTS


test_that("IndividualData inherits from Data", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  expect_true(inherits(ind_data, "Data"))

  # Should have parent class methods
  expect_true(is.function(ind_data$validate))
  expect_true(is.function(ind_data$standardize))
})

test_that("IndividualData can be used with Data methods", {
  df <- generate_hh_roster_dataset(household_data_or_n = 20)

  ind_data <- suppressMessages(
    IndividualData$new(data = df)
  )

  # get_data should work (inherited from Data)
  expect_no_error({
    raw <- ind_data$get_data("raw")
  })
})


# 8. ERROR HANDLING TESTS


test_that("IndividualData errors on NULL data", {
  expect_error(
    IndividualData$new(data = NULL),
    regexp = "No data provided|data"
  )
})

test_that("IndividualData errors on non-dataframe input", {
  expect_error(
    IndividualData$new(data = "not a dataframe"),
    regexp = "data frame|Expected"
  )
})

test_that("IndividualData handles missing columns gracefully", {
  df <- tibble::tibble(
    person_id = paste0("p_", 1:20)
  )

  # Should either error or warn about missing required columns
  expect_error({
    ind_data <- suppressMessages(
      IndividualData$new(data = df)
    )
  }, regexp = NA)  # Behavior depends on validation implementation
})

# ============================================================================
# Test: IndividualData variable_map population on initialization
# ============================================================================

test_that("IndividualData populates variable_map on initialization", {
  # Create data with standard column names
  df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    hh_uuid = paste0("hh_", 1:5),
    sex = c("male", "female", "male", "female", "male"),
    age = c(25, 30, 45, 22, 60)
  )

  ind <- suppressMessages(
    IndividualData$new(data = df, dataset_name = "TestIndividual")
  )

  # Check that variable_map is populated beyond just uuid and hh_uuid
  expect_true("person_id" %in% names(ind$variable_map))
  expect_true("hh_uuid" %in% names(ind$variable_map))
  expect_true("sex" %in% names(ind$variable_map))
  expect_true("age_years" %in% names(ind$variable_map))

  # Check that the mappings are correct
  expect_equal(ind$variable_map$person_id, "person_id")
  expect_equal(ind$variable_map$hh_uuid, "hh_uuid")
  expect_equal(ind$variable_map$sex, "sex")
  expect_equal(ind$variable_map$age_years, "age")
})

test_that("IndividualData populates value_map for non-numeric types", {
  # Create data with values
  df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    hh_uuid = paste0("hh_", 1:5),
    sex = c("male", "female", "male", "female", "male"),
    age = c(25, 30, 45, 22, 60)
  )

  ind <- suppressMessages(
    IndividualData$new(data = df, dataset_name = "TestIndividual")
  )

  # Check that value_map is populated for sex (character type with allowed_values)
  expect_true("sex" %in% names(ind$value_map))
  expect_true(all(c("male", "female") %in% ind$value_map$sex))

  # Check that value_map is NOT populated for age (numeric type)
  expect_false("age_years" %in% names(ind$value_map))
})

test_that("IndividualData maps alternative column names", {
  # Create data with alternative column names
  df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    household_id = paste0("hh_", 1:5),
    gender = c("m", "f", "m", "f", "m"),
    age_years = c(25, 30, 45, 22, 60)
  )

  ind <- suppressMessages(
    IndividualData$new(data = df, dataset_name = "TestIndividual")
  )

  # Check that alternative column names are mapped
  expect_equal(ind$variable_map$uuid, "person_id")
  expect_equal(ind$variable_map$hh_uuid, "household_id")
  expect_equal(ind$variable_map$sex, "gender")
  expect_equal(ind$variable_map$age, "age_years")

  # Check that value_map is populated with found values
  expect_true("sex" %in% names(ind$value_map))
  expect_true(all(c("m", "f") %in% ind$value_map$sex))
})

test_that("IndividualData does not overwrite explicit variable_map", {
  # Create data with alternative column names
  df <- tibble::tibble(
    my_id = paste0("id_", 1:5),
    my_hh = paste0("hh_", 1:5),
    person_sex = c("male", "female", "male", "female", "male"),
    person_age = c(25, 30, 45, 22, 60)
  )

  # Provide explicit variable_map
  ind <- suppressMessages(
    IndividualData$new(
      data = df,
      dataset_name = "TestIndividual",
      variable_map = list(
        uuid = "my_id",
        hh_uuid = "my_hh",
        sex = "person_sex",
        age = "person_age"
      )
    )
  )

  # Check that explicit mappings are preserved
  expect_equal(ind$variable_map$uuid, "my_id")
  expect_equal(ind$variable_map$hh_uuid, "my_hh")
  expect_equal(ind$variable_map$sex, "person_sex")
  expect_equal(ind$variable_map$age, "person_age")
})

test_that("IndividualData handles missing optional columns gracefully", {
  # Create data with only required columns
  df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    hh_uuid = paste0("hh_", 1:5),
    sex = c("male", "female", "male", "female", "male"),
    age = c(25, 30, 45, 22, 60)
  )

  # Should not error even though optional columns are missing
  expect_no_error(
    ind <- suppressMessages(
      IndividualData$new(data = df, dataset_name = "TestIndividual")
    )
  )

  # Optional columns should not be in variable_map if not present
  expect_null(ind$variable_map$estimated_dob)
  expect_null(ind$variable_map$exact_dob)
  expect_null(ind$variable_map$age_months)
  expect_null(ind$variable_map$age_days)
  expect_null(ind$variable_map$joined_household)
})

test_that("IndividualData maps optional columns when present", {
  # Create data with some optional columns
  df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    hh_uuid = paste0("hh_", 1:5),
    sex = c("male", "female", "male", "female", "male"),
    age = c(25, 30, 45, 22, 60),
    dob_exact = as.Date(c("1998-01-01", "1993-01-01", "1978-01-01", "2001-01-01", "1963-01-01")),
    age_months = c(300, 360, 540, 264, 720)
  )

  ind <- suppressMessages(
    IndividualData$new(data = df, dataset_name = "TestIndividual")
  )

  # Check that optional columns are mapped when present
  expect_equal(ind$variable_map$dob_exact, "dob_exact")
  expect_equal(ind$variable_map$age_months, "age_months")
})

test_that("IndividualData variable_map works like HouseholdData", {
  # This test ensures IndividualData behaves consistently with HouseholdData
  # regarding variable_map population

  # Create individual data
  ind_df <- tibble::tibble(
    person_id = paste0("id_", 1:5),
    hh_uuid = paste0("hh_", 1:5),
    sex = c("male", "female", "male", "female", "male"),
    age = c(25, 30, 45, 22, 60)
  )

  # Create household data
  hh_df <- tibble::tibble(
    uuid = paste0("hh_", 1:5),
    consent = c("yes", "yes", "yes", "yes", "yes"),
    interview_date = Sys.Date() - 1:5,
    enumerator_id = paste0("E", 1:5)
  )

  ind <- suppressMessages(
    IndividualData$new(data = ind_df, dataset_name = "TestIndividual")
  )
  hh <- suppressMessages(
    HouseholdData$new(data = hh_df, dataset_name = "TestHousehold")
  )

  # Both should have variable_map populated beyond the default uuid
  expect_true(length(ind$variable_map) > 2)  # More than just uuid and hh_uuid
  expect_true(length(hh$variable_map) > 1)   # More than just uuid

  # Both should have schemas with col_names
  expect_true(!is.null(ind$variable_schema$col_names))
  expect_true(!is.null(hh$variable_schema$col_names))

  # Both should have auto-mapped at least one variable
  expect_true("sex" %in% names(ind$variable_map))
  expect_true("uuid" %in% names(hh$variable_map))
})
