library(testthat)
library(tibble)


# ==============================================================================
# WaterContainerData Tests
# ==============================================================================

# Initialization Tests ####

test_that("WaterContainerData initializes with minimal valid data", {

  df <- tibble::tibble(
    container_id = "1",
    hh_uuid = "hh_1"
  )

  container <- suppressMessages(
    WaterContainerData$new(data = df)
  )

  expect_s3_class(container, "WaterContainerData")
  expect_s3_class(container, "Data")
  expect_equal(container$dataset_name, "WaterContainerData")
})

test_that("WaterContainerData supports multiple schemas and validations", {

  # Generate a dataset for testing
  df <- generate_water_count_loop_dataset(household_data_or_n = 5)
  container <- suppressMessages(
    WaterContainerData$new(data = df)
  )

  # Export schemas
  type_schema <- container$export_variable_schema()
  range_schema <- container$export_dependency_schema()
  values_schema <- container$export_indicator_schema()

  # Define specific tests for each schema

  # Check the Type Schema
  expect_s3_class(type_schema, "tbl_df")             # Ensure schema is a tibble
  expect_gt(nrow(type_schema), 0)                   # Type schema should have rows
  expect_true("question_type" %in% names(type_schema)) # Type schema should have 'container_type' column

  # Check the Range Schema
  expect_s3_class(range_schema, "tbl_df")           # Ensure schema is a tibble
  expect_gt(nrow(range_schema), 0)                 # Range schema should have rows
  expect_true("condition_if" %in% names(range_schema)) # Range schema should have 'container_capacity_liters' column

  # Check the Allowed Values Schema
  expect_s3_class(values_schema, "tbl_df")          # Ensure schema is a tibble
  expect_gt(nrow(values_schema), 0)                # Allowed values schema should have rows
  expect_true("arguments" %in% names(values_schema)) # Allowed values schema should have 'container_cleanliness' column
})


