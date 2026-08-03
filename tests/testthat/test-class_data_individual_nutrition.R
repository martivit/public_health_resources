library(testthat)
library(tibble)


# ==============================================================================
# NutritionIndividualData Tests
# ==============================================================================

# Initialization Tests ####

test_that("NutritionIndividualData initializes with minimal valid data", {

  df <- tibble::tibble(
    person_id = "1",
    hh_uuid = "hh_1"
  )

  nutr <- suppressMessages(
    NutritionIndividualData$new(data = df)
  )

  expect_s3_class(nutr, "NutritionIndividualData")
  expect_s3_class(nutr, "IndividualData")
  expect_s3_class(nutr, "Data")
  expect_equal(nutr$dataset_name, "NutritionIndividualData")
})

test_that("NutritionIndividualData initializes with nutrition-specific columns recognized from variable schema", {

  df <- generate_child_nutrition_dataset(roster_data_or_n = 10)

  nutr <- suppressMessages(
    NutritionIndividualData$new(data = df)
  )

  expect_true("nut_muac_mm" %in% names(nutr$variable_map))
  expect_true("ecfies_s01" %in% names(nutr$variable_map))
  expect_true("nut_bf_yesterday" %in% names(nutr$variable_map))
})



# Integration Tests ####

test_that("NutritionIndividualData completes full pipeline", {

  df <- generate_child_nutrition_dataset(roster_data_or_n = 50)

  nutr <- suppressMessages(
    NutritionIndividualData$new(data = df)
  )

  # Validation
  expect_no_error(nutr$validate())
  expect_true(nutr$validated)

  # Standardization
  expect_no_error(nutr$standardize())
  expect_true(nutr$standardized)

  # Cleaning
  expect_no_error(nutr$generate_cleaning_log(stage = "standardized"))
  expect_no_error(nutr$clean())
  expect_true(nutr$cleaned)

  # Summary

})

test_that("NutritionIndividualData can link to HouseholdData", {

  hh_df <- generate_household_dataset(n = 10)
  nutr_df <- generate_child_nutrition_dataset(roster_data_or_n = 30, hh_uuids = hh_df$uuid)

  hh <- suppressMessages(
    HouseholdData$new(data = hh_df)
  )
  nutr <- suppressMessages(
    NutritionIndividualData$new(data = nutr_df)
  )

  # Link nutrition data to households
  nutr$add_linked_dataset("household", hh, by_self_role = "hh_uuid", by_other_role = "uuid")

  expect_true("household" %in% names(nutr$linked_objects))
})

test_that("NutritionIndividualData handles realistic SMART survey data", {

  # Create data mimicking a typical SMART survey
  n <- 100
  set.seed(42)

  df <- tibble::tibble(
    person_id = paste0("child_", sprintf("%03d", 1:n)),
    hh_uuid = paste0("hh_", sprintf("%03d", sample(1:50, n, replace = TRUE))),
    sex = sample(c("male", "female"), n, replace = TRUE),
    nutr_age_months = sample(6:59, n, replace = TRUE),
    nutr_muac_mm = round(rnorm(n, mean = 135, sd = 15), 0),
    nutr_oedema = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.02, 0.98))
  )

  nutr <- suppressMessages(
    NutritionIndividualData$new(data = df)
  )

  expect_no_error({
    nutr$validate()
    nutr$standardize()
  })

  # Check that malnutrition was classified
  std_data <- nutr$standardized_data


})
