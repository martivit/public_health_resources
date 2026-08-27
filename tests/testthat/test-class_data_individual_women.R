library(testthat)
library(tibble)


# ==============================================================================
# WomenIndividualData Tests
# ==============================================================================

# Initialization Tests ####

test_that("WomenIndividualData initializes with minimal valid data", {

  df <- tibble::tibble(
    person_id = "1",
    hh_uuid = "hh_1"
  )

  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(data = df)
  ))

  expect_s3_class(women, "WomenIndividualData")
  expect_s3_class(women, "IndividualData")
  expect_s3_class(women, "Data")
  expect_equal(women$dataset_name, "WomenIndividualData")
})

test_that("WomenIndividualData initializes with custom dataset name", {

  df <- tibble::tibble(
    person_id = "1",
    hh_uuid = "hh_1"
  )

  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(
      data = df,
      dataset_name = "CustomWomenData"
    )
  ))

  expect_equal(women$dataset_name, "CustomWomenData")
})

test_that("WomenIndividualData loads default schemas on initialization", {

  df <- tibble::tibble(
    person_id = "1",
    hh_uuid = "hh_1"
  )

  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(data = df)
  ))

  # Should have variable schema
  expect_true(length(women$variable_schema) > 0)

  # Check that indicator schema was loaded if available
  # (it may be empty if the template has no indicators)
  expect_true(is.list(women$indicator_schema))

  # Check that dependency schema was loaded
  expect_true(is.list(women$dependency_schema))
})


# Integration Tests ####

test_that("WomenIndividualData completes validation", {

  df <- tibble::tibble(
    person_id = paste0("woman_", 1:10),
    hh_uuid = paste0("hh_", 1:10),
    sex = rep("female", 10),
    age = rep(25, 10)
  )

  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(data = df)
  ))

  # Validation
  expect_no_error(suppressWarnings(suppressMessages(women$validate())))
  expect_true(women$validated)
})

test_that("WomenIndividualData can link to HouseholdData", {

  hh_df <- generate_household_dataset(n = 10)
  women_df <- tibble::tibble(
    person_id = paste0("woman_", 1:30),
    hh_uuid = rep(hh_df$uuid, length.out = 30),
    sex = rep("female", 30),
    age = sample(15:49, 30, replace = TRUE)
  )

  hh <- suppressWarnings(suppressMessages(
    HouseholdData$new(data = hh_df)
  ))
  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(data = women_df)
  ))

  # Link women data to households
  suppressWarnings(suppressMessages(
    women$add_linked_dataset("household", hh, by_self_role = "hh_uuid", by_other_role = "uuid")
  ))

  expect_true("household" %in% names(women$linked_objects))
})

test_that("WomenIndividualData handles women of reproductive age", {

  # Create data for women of reproductive age (15-49 years)
  n <- 50
  set.seed(42)

  df <- tibble::tibble(
    person_id = paste0("woman_", sprintf("%03d", 1:n)),
    hh_uuid = paste0("hh_", sprintf("%03d", sample(1:25, n, replace = TRUE))),
    sex = rep("female", n),
    age = sample(15:49, n, replace = TRUE)
  )

  women <- suppressWarnings(suppressMessages(
    WomenIndividualData$new(data = df)
  ))

  expect_no_error({
    suppressWarnings(suppressMessages(women$validate("raw")))
  })

  # Check that data was loaded correctly
  expect_equal(nrow(women$raw_data), n)
  expect_true(all(women$raw_data$sex == "female"))
  expect_true(all(women$raw_data$age >= 15 & women$raw_data$age <= 49))
})

