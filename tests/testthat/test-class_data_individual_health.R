library(testthat)
library(tibble)


# HealthIndividualData Tests

# Initialization Tests ####

test_that("HealthIndividualData initializes with minimal valid data", {

  df <- tibble::tibble(
    person_id = "1",
    hh_uuid = "hh_1",
    sex = "male",
    age_years = 25
  )

  health <- suppressWarnings(suppressMessages(
    HealthIndividualData$new(data = df,
                                       )
  ))

  expect_s3_class(health, "HealthIndividualData")
  expect_s3_class(health, "IndividualData")
  expect_s3_class(health, "Data")
  expect_equal(health$dataset_name, "HealthIndividualData")
})

test_that("HealthIndividualData supports multiple schemas and validations", {

  # Generate a dataset for testing
  df <- generate_health_ind_dataset(roster_data_or_n = 5)
  health <- suppressWarnings(suppressMessages(
    HealthIndividualData$new(data = df)
  ))

  # Export schemas
  var_schema <- suppressWarnings(suppressMessages(health$export_variable_schema()))
  dep_schema <- suppressWarnings(suppressMessages(health$export_dependency_schema()))
  ind_schema <- suppressWarnings(suppressMessages(health$export_indicator_schema()))

  # Define specific tests for each schema

  # Check the class of the schema (use expect_s3_class for tibbles)
  expect_s3_class(var_schema, "tbl_df") # Tibble-specific expectation
  expect_gt(nrow(var_schema), 0)        # Variable schema should have rows
  expect_named(var_schema)              # Variable schema should have column names

  expect_s3_class(dep_schema, "tbl_df") # Tibble-specific expectation
  expect_gt(nrow(dep_schema), 0)        # Dependency schema should have rows
  expect_true(all(c("rule_type", "dep_name", "variables", "condition_if", "then", "action") %in% names(dep_schema)))

  expect_s3_class(ind_schema, "tbl_df") # Tibble-specific expectation
  expect_gt(nrow(ind_schema), 0)        # Indicator schema should have rows
  expect_true(any(grepl("indicator", names(ind_schema)))) # Indicator schema should reference "indicators"
})


# Care Seeking Analysis Tests ####





# Integration Tests ####

test_that("HealthIndividualData completes full pipeline", {

  df <- generate_health_ind_dataset(roster_data_or_n = 50)

  health <- suppressWarnings(suppressMessages(
    HealthIndividualData$new(data = df)
  ))

  # Validation
  expect_no_error(suppressWarnings(suppressMessages(health$validate())))

  # Standardization
  expect_no_error(suppressWarnings(suppressMessages(health$standardize())))
  expect_true(health$standardized)

  # Cleaning
  suppressWarnings(suppressMessages(health$generate_cleaning_log()))
  expect_no_error(suppressWarnings(suppressMessages(health$clean())))
  expect_true(health$cleaned)

})

test_that("HealthIndividualData can link to HouseholdData", {

  hh_df <- generate_household_dataset(n = 10)
  ind_df <- generate_health_ind_dataset(roster_data_or_n = 30, hh_uuids = hh_df$uuid)

  hh <- suppressWarnings(suppressMessages(
    HouseholdData$new(data = hh_df)
  ))
  health_ind <- suppressWarnings(suppressMessages(
    HealthIndividualData$new(data = ind_df)
  ))

  # Link individuals to households
  suppressWarnings(suppressMessages(
    health_ind$add_linked_dataset("household", hh, by_self_role = "hh_uuid", by_other_role = "uuid")
  ))

  expect_true("household" %in% names(health_ind$linked_objects))
})

