library(testthat)
library(tibble)
library(mockery)
library(phrutils)

# Household Data Schema Loading ####

test_that("household schema template files exist in inst/resources", {
  # Check for new separated templates
  var_path <- system.file(
    "resources",
    "variable_schema_data_household_template.xlsx",
    package = "phr"
  )
  dep_path <- system.file(
    "resources",
    "dependency_schema_data_household_template.xlsx",
    package = "phr"
  )

  expect_true(
    file.exists(var_path),
    info = "variable_schema_data_household_template.xlsx should exist"
  )
  expect_true(
    file.exists(dep_path),
    info = "dependency_schema_data_household_template.xlsx should exist"
  )

  # Old combined template may still exist for backwards compatibility
  old_path <- system.file(
    "resources",
    "household_schema_template.xlsx",
    package = "phr"
  )
  # Not requiring old file to exist, but if it does, that's OK
})

test_that("default_schema loads schema template and converts correctly", {
  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "E1"
    )
  )))

  schema <- suppressMessages(suppressWarnings(hh$default_schema()))

  expect_type(schema, "list")
  expect_true(all(c("required", "types", "allowed_values") %in% names(schema)))
})

test_that("default_schema errors clearly when schema template missing", {
  fake <- suppressMessages(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "E1"
    )
  ))

  # Temporarily override system.file to break it
  mockery::stub(
    fake$default_schema,
    "system.file",
    "INVALID_PATH_DOES_NOT_EXIST"
  )

  expect_error(fake$default_schema(), regexp = "not found")
})


# Household Data Initialization ####

test_that("HouseholdData initializes with expected required columns", {
  hh <- suppressMessages(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "E1"
    )
  ))

  expect_true(all(
    c("uuid", "consent", "interview_date", "enumerator_id") %in%
      hh$required_columns
  ))
})

test_that("optional household columns are included in optional_columns", {
  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "E1",
      cluster_id = "A",
      strata = "Urban",
      weight = 1.3,
      adm1 = "X",
      adm2 = "Y",
      gps_lat = 10,
      gps_lon = 20
    )
  )))

  expect_true(all(
    c(
      "cluster_id",
      "strata",
      "weight",
      "adm1",
      "adm2",
      "gps_lat",
      "gps_lon"
    ) %in%
      hh$optional_columns
  ))
})

test_that("HouseholdData initialization fails when required columns missing", {
  expect_error(
    suppressMessages(HouseholdData$new(
      data = tibble(consent = "yes"), # uuid missing
      metadata = NULL
    )),
    regexp = "uuid", # Data initializer catches it
    fixed = FALSE
  )
})

test_that("variable_map overrides default household field names", {
  df <- tibble(
    id_col = "1",
    cns = "yes",
    date_int = Sys.Date(),
    enumid = "A"
  )

  hh <- suppressMessages(HouseholdData$new(
    data = df,
    variable_map = list(
      uuid = "id_col",
      consent = "cns",
      date_survey = "date_int",
      enum_id = "enumid"
    )
  ))

  expect_true(all(
    c("id_col", "cns", "date_int", "enumid") %in% hh$required_columns
  ))
})


test_that("HouseholdData cannot initialize with empty data", {
  expect_error(
    suppressMessages(HouseholdData$new(data = tibble())),
    regexp = "uuid"
  )
})

# Household Data Post-Validate ####

test_that("post_validate warns on invalid GPS coordinates", {
  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "A",
      gps_lat = 200, # invalid latitude
      gps_lon = 400 # invalid longitude
    ),
    variable_map = list(
      gps_lat = "gps_lat",
      gps_lon = "gps_lon"
    )
  )))

  # Extract the raw data to pass into post_validate()
  df <- suppressMessages(suppressWarnings(hh$get_data()))

  # Expect GPS warning
  expect_warning(
    suppressMessages(hh$post_validate(df)),
    regexp = "GPS"
  )
})


test_that("post_validate warns on invalid weights", {
  suppressWarnings(
    hh <- suppressMessages(suppressWarnings(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("1", "2", "3"),
          consent = c("yes", "yes", "yes"),
          interview_date = rep(Sys.Date(), 3),
          enumerator_id = c("A", "B", "C"),
          weight = c(1, NA, -5)
        )
      )
    ))
  )

  # post_validate() is invoked INSIDE validate(), so we test validate()
  suppressWarnings(expect_warning(
    suppressMessages(hh$validate("raw")),
    regexp = "Missing values",
    ignore.case = TRUE
  ))

  suppressWarnings(expect_warning(
    suppressMessages(hh$validate("raw")),
    regexp = "Negative weights",
    ignore.case = TRUE
  ))
})

test_that("post_validate warns on missing strata", {
  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = c("1", "2"),
      consent = c("yes", "yes"),
      interview_date = Sys.Date(),
      enumerator_id = c("A", "A"),
      strata = c("Urban", NA)
    ),
    variable_map = list(
      strata = "strata"
    )
  )))

  expect_warning(
    suppressMessages(hh$post_validate(hh$raw_data)),
    regexp = "Missing strata"
  )
})


test_that("get_survey_design errors when srvyr is not installed", {
  skip_if(
    requireNamespace("srvyr", quietly = TRUE),
    "srvyr installed — skipping artificial-fail test"
  )

  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "A",
      cluster_id = "C1",
      weight = 1
    )
  )))

  expect_error(
    suppressMessages(suppressWarnings(hh$get_survey_design())),
    regexp = "srvyr"
  )
})

# Household Data Get Survey Design ####

test_that("get_survey_design warns and returns NULL when cluster_id/weight missing", {
  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = "1",
      consent = "yes",
      interview_date = Sys.Date(),
      enumerator_id = "A"
    )
  )))

  suppressWarnings(expect_warning(
    suppressMessages(hh$get_survey_design()),
    regexp = "data available to create survey design"
  ))
  expect_null(hh$survey_design)
})

test_that("get_survey_design succeeds when required fields exist", {
  skip_if_not_installed("srvyr")

  hh <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble(
      uuid = c("1", "2"),
      consent = c("yes", "yes"),
      interview_date = Sys.Date(),
      enumerator_id = "A",
      cluster_id = c("C1", "C2"), # TWO PSUs — required
      weight = c(1, 1)
    )
  )))

  design <- suppressMessages(suppressWarnings(hh$get_survey_design("raw")))

  expect_s3_class(design, "survey.design")
})


# Tests for linked dataset aggregation methods ####

test_that("NutritionIndividualData aggregates children by age groups", {
  # Create household data
  hh_data <- suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  )

  # Create nutrition data with calc_age_months (as would be output by add_standardized_age)
  suppressWarnings(
    nutr_data <- suppressMessages(
      NutritionIndividualData$new(
        data = tibble::tibble(
          person_id = c("n1", "n2", "n3", "n4", "n5", "n6"),
          hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3", "hh3"),
          calc_age_months = c(10, 30, 48, 72, 15, 20) # <24, 24-59, 24-59, >60, <24, <24
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("nutrition", nutr_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated columns exist
  std_data <- hh_data$standardized_data
  expect_true("linked_nutrition_children_under2" %in% names(std_data))
  expect_true("linked_nutrition_children_2to5" %in% names(std_data))
  expect_true("linked_nutrition_children_under5" %in% names(std_data))

  # Check values for hh1: 1 under 2 years (10 months), 1 2to5 (30 months), 2 <5
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_nutrition_children_under2, 1)
  expect_equal(hh1_row$linked_nutrition_children_2to5, 1)
  expect_equal(hh1_row$linked_nutrition_children_under5, 2)

  # Check values for hh2: 0 under 2 (48, 72 months -> 1 24-59, 1 >=60), 1 2to5, 1 <5
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_nutrition_children_under2, 0)
  expect_equal(hh2_row$linked_nutrition_children_2to5, 1)
  expect_equal(hh2_row$linked_nutrition_children_under5, 1)

  # Check values for hh3: 2 under 2 years (15, 20 months), 0 2to5, 2 <5
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_nutrition_children_under2, 2)
  expect_equal(hh3_row$linked_nutrition_children_2to5, 0)
  expect_equal(hh3_row$linked_nutrition_children_under5, 2)
})

test_that("HealthIndividualData aggregates number of people recorded", {
  # Create household data
  suppressWarnings(
    hh_data <- suppressMessages(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("hh1", "hh2", "hh3"),
          consent = c("yes", "yes", "yes"),
          interview_date = Sys.Date(),
          enumerator_id = c("E1", "E2", "E3")
        )
      )
    )
  )

  # Create health data
  suppressWarnings(
    health_data <- suppressMessages(
      HealthIndividualData$new(
        data = tibble::tibble(
          person_id = c("h1", "h2", "h3", "h4", "h5"),
          hh_uuid = c("hh1", "hh1", "hh1", "hh2", "hh3"),
          age = c(10, 20, 30, 25, 40),
          sex = c("M", "F", "M", "F", "M")
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("health", health_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated column exists
  std_data <- hh_data$standardized_data
  expect_true("linked_health_num_people_recorded" %in% names(std_data))

  # Check values
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_health_num_people_recorded, 3)

  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_health_num_people_recorded, 1)

  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_health_num_people_recorded, 1)
})

test_that("IndividualData (roster) still uses aggregate_roster_data method", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2")
      )
    )
  ))

  # Create roster data
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("r1", "r2", "r3"),
        hh_uuid = c("hh1", "hh1", "hh2"),
        sex = c("M", "F", "M"),
        age = c(25, 30, 40)
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check that roster aggregation produces household_size
  std_data <- hh_data$standardized_data
  expect_true("linked_roster_household_size" %in% names(std_data))

  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_roster_household_size, 2)

  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_roster_household_size, 1)
})

test_that("aggregate_roster_data adds linked_roster_birth_months from calc_month_birth", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  ))

  # Create roster data with calc_month_birth column
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("r1", "r2", "r3", "r4"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh3"),
        sex = c("M", "F", "M", "F"),
        age = c(25, 30, 40, 5),
        calc_month_birth = c("2001-01", "1996-05", "1986-03", NA)
      )
    )
  ))

  # Link and standardize
  suppressWarnings(suppressMessages(hh_data$add_linked_dataset(
    "roster",
    roster_data
  )))
  suppressWarnings(suppressMessages(hh_data$standardize()))

  std_data <- hh_data$standardized_data
  expect_true("linked_roster_birth_months" %in% names(std_data))

  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_roster_birth_months, "2001-01, 1996-05")

  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_roster_birth_months, "1986-03")

  # hh3 has NA calc_month_birth, so should be empty string
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_roster_birth_months, "")
})

test_that("aggregate_deaths_data adds linked_deaths_death_month from calc_month_death", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  ))

  # Create death data with calc_month_death column
  death_data <- suppressWarnings(suppressMessages(
    DeathIndividualData$new(
      data = tibble::tibble(
        death_id = c("d1", "d2", "d3"),
        hh_uuid = c("hh1", "hh1", "hh2"),
        sex = c("M", "F", "M"),
        age = c(60, 45, 70),
        calc_month_death = c("2024-03", "2024-06", "2024-01")
      ),
      recall_date = "2022-01-01",
    )
  ))

  # Link and standardize
  suppressWarnings(suppressMessages(hh_data$add_linked_dataset(
    "deaths",
    death_data
  )))
  suppressWarnings(suppressMessages(hh_data$standardize()))

  std_data <- hh_data$standardized_data
  expect_true("linked_deaths_death_month" %in% names(std_data))

  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_deaths_death_month, "2024-03, 2024-06")

  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_deaths_death_month, "2024-01")

  # hh3 has no deaths, should be empty string
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_deaths_death_month, "")
})

test_that("Household variables are merged to linked datasets during standardize", {
  # Create household data with various household-level variables
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3"),
        enum_id = c("ENUM-1", "ENUM-2", "ENUM-3"),
        device_id = c("DEV-1", "DEV-2", "DEV-3"),
        weight = c(1.5, 2.0, 1.2),
        stratum = c("Urban", "Rural", "Urban"),
        cluster_id = c("C1", "C2", "C3"),
        site = c("Site1", "Site2", "Site3"),
        admin1 = c("Admin1_A", "Admin1_B", "Admin1_A"),
        admin2 = c("Admin2_1", "Admin2_2", "Admin2_3"),
        admin3 = c("Admin3_X", "Admin3_Y", "Admin3_Z"),
        admin4 = c("Admin4_P", "Admin4_Q", "Admin4_R"),
        date_survey = Sys.Date()
      )
    )
  ))

  # Create nutrition data without household-level variables
  nutr_data <- suppressWarnings(suppressMessages(
    NutritionIndividualData$new(
      data = tibble::tibble(
        person_id = c("n1", "n2", "n3", "n4"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh3"),
        calc_age_months = c(10, 30, 48, 15)
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("nutrition", nutr_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check that nutrition data now has household-level variables
  nutr_std_data <- nutr_data$standardized_data

  # Check that household variables were added
  expect_true("enum_id" %in% names(nutr_std_data))
  expect_true("device_id" %in% names(nutr_std_data))
  expect_true("weight" %in% names(nutr_std_data))
  expect_true("stratum" %in% names(nutr_std_data))
  expect_true("cluster_id" %in% names(nutr_std_data))
  expect_true("site" %in% names(nutr_std_data))
  expect_true("admin1" %in% names(nutr_std_data))
  expect_true("admin2" %in% names(nutr_std_data))
  expect_true("admin3" %in% names(nutr_std_data))
  expect_true("admin4" %in% names(nutr_std_data))
  expect_true("date_survey" %in% names(nutr_std_data))

  # Check values are correct for hh1 records
  hh1_records <- nutr_std_data[nutr_std_data$hh_uuid == "hh1", ]
  expect_equal(unique(hh1_records$enum_id), "ENUM-1")
  expect_equal(unique(hh1_records$device_id), "DEV-1")
  expect_equal(unique(hh1_records$weight), 1.5)
  expect_equal(unique(hh1_records$stratum), "Urban")
  expect_equal(unique(hh1_records$cluster_id), "C1")
  expect_equal(unique(hh1_records$site), "Site1")
  expect_equal(unique(hh1_records$admin1), "Admin1_A")
  expect_equal(unique(hh1_records$admin2), "Admin2_1")
  expect_equal(unique(hh1_records$admin3), "Admin3_X")
  expect_equal(unique(hh1_records$admin4), "Admin4_P")

  # Check values are correct for hh2 records
  hh2_records <- nutr_std_data[nutr_std_data$hh_uuid == "hh2", ]
  expect_equal(unique(hh2_records$enum_id), "ENUM-2")
  expect_equal(unique(hh2_records$device_id), "DEV-2")
  expect_equal(unique(hh2_records$weight), 2.0)
  expect_equal(unique(hh2_records$stratum), "Rural")
  expect_equal(unique(hh2_records$cluster_id), "C2")
})

test_that("Household variables overwrite existing variables in linked datasets", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2"),
        enum_id = c("ENUM-CORRECT-1", "ENUM-CORRECT-2"),
        weight = c(1.5, 2.0),
        cluster_id = c("C1", "C2")
      )
    )
  ))

  # Create nutrition data with existing enum_id and weight that should be overwritten
  nutr_data <- suppressWarnings(suppressMessages(
    NutritionIndividualData$new(
      data = tibble::tibble(
        person_id = c("n1", "n2", "n3"),
        hh_uuid = c("hh1", "hh1", "hh2"),
        calc_age_months = c(10, 30, 48),
        enum_id = c("ENUM-OLD-1", "ENUM-OLD-1", "ENUM-OLD-2"),
        weight = c(999, 999, 999),
        cluster_id = c("OLD-C1", "OLD-C1", "OLD-C2")
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("nutrition", nutr_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check that nutrition data has household values (overwritten)
  nutr_std_data <- nutr_data$standardized_data

  # Check hh1 records have correct household values
  hh1_records <- nutr_std_data[nutr_std_data$hh_uuid == "hh1", ]
  expect_equal(unique(hh1_records$enum_id), "ENUM-CORRECT-1")
  expect_equal(unique(hh1_records$weight), 1.5)
  expect_equal(unique(hh1_records$cluster_id), "C1")

  # Check hh2 records have correct household values
  hh2_records <- nutr_std_data[nutr_std_data$hh_uuid == "hh2", ]
  expect_equal(unique(hh2_records$enum_id), "ENUM-CORRECT-2")
  expect_equal(unique(hh2_records$weight), 2.0)
  expect_equal(unique(hh2_records$cluster_id), "C2")
})


test_that("Only available household variables are merged to linked datasets", {
  # Create household data with only some variables
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2"),
        enum_id = c("ENUM-1", "ENUM-2"),
        cluster_id = c("C1", "C2")
        # Note: no weight, stratum, site, admin variables
      )
    )
  ))

  # Create nutrition data
  suppressWarnings(
    nutr_data <- suppressMessages(
      NutritionIndividualData$new(
        data = tibble::tibble(
          person_id = c("n1", "n2"),
          hh_uuid = c("hh1", "hh2"),
          calc_age_months = c(10, 30)
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("nutrition", nutr_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check that nutrition data has only the available household variables
  nutr_std_data <- nutr_data$standardized_data

  expect_true("enum_id" %in% names(nutr_std_data))
  expect_true("cluster_id" %in% names(nutr_std_data))

  # Check that unavailable variables are NOT added
  expect_false("weight" %in% names(nutr_std_data))
  expect_false("stratum" %in% names(nutr_std_data))
  expect_false("site" %in% names(nutr_std_data))
  expect_false("admin1" %in% names(nutr_std_data))
})


test_that("WaterContainerData aggregates total litres correctly with British spelling", {
  # Create household data
  suppressWarnings(suppressMessages(
    hh_data <- HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  ))

  # Create water container data with British spelling (litres)
  suppressWarnings(suppressMessages(
    water_data <- WaterContainerData$new(
      data = tibble::tibble(
        container_id = c("c1", "c2", "c3", "c4", "c5"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3"),
        wash_container_total_litres = c(20, 30, 15, 25, 50) # British spelling
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("water_containers", water_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated column exists
  std_data <- hh_data$standardized_data
  expect_true(
    "linked_water_containers_wash_container_total_liters" %in% names(std_data)
  )

  # Check values for hh1: 20 + 30 = 50
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_water_containers_wash_container_total_liters, 50)

  # Check values for hh2: 15 + 25 = 40
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_water_containers_wash_container_total_liters, 40)

  # Check values for hh3: 50
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_water_containers_wash_container_total_liters, 50)
})


test_that("WaterContainerData aggregates total liters correctly with American spelling", {
  # Create household data
  suppressWarnings(
    hh_data <- suppressMessages(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("hh1", "hh2"),
          consent = c("yes", "yes"),
          interview_date = Sys.Date(),
          enumerator_id = c("E1", "E2")
        )
      )
    )
  )

  # Create water container data with American spelling (liters)
  suppressWarnings(
    water_data <- suppressMessages(
      WaterContainerData$new(
        data = tibble::tibble(
          container_id = c("c1", "c2", "c3"),
          hh_uuid = c("hh1", "hh1", "hh2"),
          wash_container_total_liters = c(10, 20, 30) # American spelling
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("water_containers", water_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated column exists
  std_data <- hh_data$standardized_data
  expect_true(
    "linked_water_containers_wash_container_total_liters" %in% names(std_data)
  )

  # Check values for hh1: 10 + 20 = 30
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_water_containers_wash_container_total_liters, 30)

  # Check values for hh2: 30
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_water_containers_wash_container_total_liters, 30)
})


test_that("WaterContainerData aggregates container counts correctly", {
  # Create household data
  suppressWarnings(suppressMessages(
    hh_data <- HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  ))

  # Create water container data
  suppressWarnings(suppressMessages(
    water_data <- WaterContainerData$new(
      data = tibble::tibble(
        container_id = c("c1", "c2", "c3", "c4", "c5"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3"),
        wash_container_total_liters = c(20, 30, 15, 25, 50)
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("water_containers", water_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated column exists
  std_data <- hh_data$standardized_data
  expect_true("linked_water_containers_num_containers" %in% names(std_data))

  # Check values for hh1: 2 containers
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_water_containers_num_containers, 2)

  # Check values for hh2: 2 containers
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_water_containers_num_containers, 2)

  # Check values for hh3: 1 container
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_water_containers_num_containers, 1)
})


test_that("WaterContainerData aggregates both liters and counts with British spelling", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2", "hh3"),
        consent = c("yes", "yes", "yes"),
        interview_date = Sys.Date(),
        enumerator_id = c("E1", "E2", "E3")
      )
    )
  ))

  # Create water container data with British spelling (litres)
  water_data <- suppressWarnings(suppressMessages(
    WaterContainerData$new(
      data = tibble::tibble(
        container_id = c("c1", "c2", "c3", "c4", "c5"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3"),
        wash_container_total_litres = c(20, 30, 15, 25, 50) # British spelling
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("water_containers", water_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check both aggregated columns exist
  std_data <- hh_data$standardized_data
  expect_true(
    "linked_water_containers_wash_container_total_liters" %in% names(std_data)
  )
  expect_true("linked_water_containers_num_containers" %in% names(std_data))

  # Check values for hh1: 50 liters, 2 containers
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_water_containers_wash_container_total_liters, 50)
  expect_equal(hh1_row$linked_water_containers_num_containers, 2)

  # Check values for hh2: 40 liters, 2 containers
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_water_containers_wash_container_total_liters, 40)
  expect_equal(hh2_row$linked_water_containers_num_containers, 2)

  # Check values for hh3: 50 liters, 1 container
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_water_containers_wash_container_total_liters, 50)
  expect_equal(hh3_row$linked_water_containers_num_containers, 1)
})


test_that("Household raw_data is preserved even when aggregation encounters errors", {
  # Create household data
  suppressWarnings(suppressMessages(
    hh_data <- HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        enum_id = c("E1", "E2")
      )
    )
  ))

  # Verify raw_data exists initially
  expect_false(is.null(hh_data$raw_data))
  expect_equal(nrow(hh_data$raw_data), 2)

  # Create a linked dataset with intentionally problematic data
  # (missing hh_uuid column to trigger aggregation error)
  problem_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("p1", "p2", "p3"),
        # Note: no hh_uuid column - this will cause aggregation to fail
        age_years = c(10, 20, 30)
      )
    )
  ))

  # Add the problematic linked dataset
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", problem_data)
  ))

  # Standardize household data.
  # Aggregation may warn; we just want to ensure it does not hard-error and that raw_data remains.
  suppressWarnings(suppressMessages(
    expect_error(hh_data$standardize(), regexp = NA)
  ))

  # CRITICAL TEST: Verify raw_data is NOT NULL after failed aggregation
  expect_false(is.null(hh_data$raw_data))
  expect_equal(nrow(hh_data$raw_data), 2)
  expect_true("uuid" %in% names(hh_data$raw_data))
  expect_true("enum_id" %in% names(hh_data$raw_data))

  # Verify household is still standardized despite aggregation warning
  expect_true(hh_data$standardized)
  expect_false(is.null(hh_data$standardized_data))

  # Verify the household data itself is accessible
  expect_equal(nrow(hh_data$standardized_data), 2)
})


test_that("Household standardization succeeds with valid linked datasets", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        enum_id = c("E1", "E2")
      )
    )
  ))

  # Create a properly structured linked dataset
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("p1", "p2", "p3", "p4"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh2"),
        age_years = c(10, 15, 25, 30)
      )
    )
  ))

  # Add the linked dataset (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))

  # Standardize household data - should succeed
  suppressWarnings(suppressMessages(
    expect_no_error(hh_data$standardize())
  ))

  # Verify raw_data is preserved
  expect_false(is.null(hh_data$raw_data))
  expect_equal(nrow(hh_data$raw_data), 2)

  # Verify standardized data exists and has aggregated columns
  expect_true(hh_data$standardized)
  expect_false(is.null(hh_data$standardized_data))
  expect_true(
    "linked_roster_household_size" %in% names(hh_data$standardized_data)
  )

  # Verify aggregation worked correctly
  std_data <- hh_data$standardized_data
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_roster_household_size, 2)

  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_roster_household_size, 2)
})

test_that("Deaths data aggregates births in recall period when calc_date_birth_final is available", {
  # Create household data with survey date
  suppressWarnings(
    hh_data <- suppressMessages(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("hh1", "hh2"),
          consent = c("yes", "yes"),
          interview_date = as.Date(c("2023-12-31", "2023-12-31")),
          enumerator_id = c("E1", "E2")
        ),
        variable_map = list(
          date_survey = "interview_date"
        )
      )
    )
  )

  # Create deaths data with births in recall period
  # Recall date is 2023-01-01, survey date is 2023-12-31
  # d1: birth in recall period (2023-06-01)
  # d2: birth in recall period (2023-03-15)
  # d3: birth before recall period (2022-12-01)
  suppressWarnings(
    deaths_data <- suppressMessages(
      DeathIndividualData$new(
        data = tibble::tibble(
          death_id = c("d1", "d2", "d3"),
          hh_uuid = c("hh1", "hh1", "hh2"),
          age_years = c(0.5, 1, 0.3),
          sex = c("M", "F", "M"),
          recall_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
          calc_date_birth_final = as.Date(c(
            "2023-06-01",
            "2023-03-15",
            "2022-12-01"
          ))
        ),
        recall_date = as.Date("2023-01-01")
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("deaths", deaths_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check births_in_recall column exists
  std_data <- hh_data$standardized_data
  expect_true("linked_deaths_death_birth" %in% names(std_data))

  # Check values for hh1: 2 births in recall period
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_deaths_death_birth, 2)

  # Check values for hh2: 0 births in recall period
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_deaths_death_birth, 0)
})


test_that("Roster data aggregates births in recall period when recall_date column is available", {
  # Create household data with recall date and survey date
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes"),
        interview_date = as.Date(c("2023-12-31", "2023-12-31")),
        enumerator_id = c("E1", "E2"),
        recall_date = as.Date(c("2023-01-01", "2023-01-01"))
      ),
      variable_map = list(
        date_data_collection = "interview_date",
        recall_date = "recall_date"
      )
    )
  ))

  # Create roster data with births
  # r1: birth in recall period (2023-06-01)
  # r2: birth before recall period (1998-01-01)
  # r3: birth before recall period (1993-01-01)
  # r4: birth before recall period (2022-12-15)
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("r1", "r2", "r3", "r4"),
        hh_uuid = c("hh1", "hh1", "hh2", "hh2"),
        sex = c("M", "F", "M", "F"),
        age = c(0.5, 25, 30, 0.8),
        recall_date = as.Date(c(
          "2023-01-01",
          "2023-01-01",
          "2023-01-01",
          "2023-01-01"
        )),
        calc_date_birth_final = as.character(c(
          "2023-06-01",
          "1998-01-01",
          "1993-01-01",
          "2022-12-15"
        ))
      )
    )
  ))

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))
  suppressMessages(
    hh_data$standardize()
  )

  # Check births_in_recall column exists
  std_data <- hh_data$standardized_data
  expect_true("linked_roster_birth" %in% names(std_data))

  # Check values for hh1: 1 birth in recall period
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_roster_birth, 1)

  # Check values for hh2: 0 births in recall period
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_roster_birth, 0)
})

test_that("Roster data aggregates canonical columns when they exist", {
  # Create household data
  suppressWarnings(
    hh_data <- suppressMessages(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("hh1", "hh2", "hh3"),
          consent = c("yes", "yes", "yes"),
          interview_date = Sys.Date(),
          enumerator_id = c("E1", "E2", "E3")
        )
      )
    )
  )

  # Create roster data with canonical columns (as would be created by add_standardized_roster_demographics)
  suppressWarnings(
    roster_data <- suppressMessages(
      IndividualData$new(
        data = tibble::tibble(
          person_id = c("r1", "r2", "r3", "r4", "r5", "r6"),
          hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3", "hh3"),
          calc_age_years = c(1, 25, 4, 40, 17, 50),
          sex = c("M", "F", "F", "M", "F", "F"),
          roster_child_under2 = c(1, 0, 0, 0, 0, 0),
          roster_child_under5 = c(1, 0, 1, 0, 0, 0),
          roster_male = c(1, 0, 0, 1, 0, 0),
          roster_female = c(0, 1, 1, 0, 1, 1),
          roster_woman_15to49 = c(0, 1, 0, 0, 1, 0)
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated columns exist
  std_data <- hh_data$standardized_data
  expect_true("linked_roster_household_size" %in% names(std_data))
  expect_true("linked_roster_child_under2" %in% names(std_data))
  expect_true("linked_roster_child_under5" %in% names(std_data))
  expect_true("linked_roster_male" %in% names(std_data))
  expect_true("linked_roster_female" %in% names(std_data))
  expect_true("linked_roster_woman_15to49" %in% names(std_data))

  # Check values for hh1: 2 people, 1 child <2, 1 child <5, 1 male, 1 female, 1 woman 15-49
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_roster_household_size, 2)
  expect_equal(hh1_row$linked_roster_child_under2, 1)
  expect_equal(hh1_row$linked_roster_child_under5, 1)
  expect_equal(hh1_row$linked_roster_male, 1)
  expect_equal(hh1_row$linked_roster_female, 1)
  expect_equal(hh1_row$linked_roster_woman_15to49, 1)

  # Check values for hh2: 2 people, 0 child <2, 1 child <5, 1 male, 1 female, 0 women 15-49
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_roster_household_size, 2)
  expect_equal(hh2_row$linked_roster_child_under2, 0)
  expect_equal(hh2_row$linked_roster_child_under5, 1)
  expect_equal(hh2_row$linked_roster_male, 1)
  expect_equal(hh2_row$linked_roster_female, 1)
  expect_equal(hh2_row$linked_roster_woman_15to49, 0)

  # Check values for hh3: 2 people, 0 child <2, 0 child <5, 0 male, 2 female, 1 woman 15-49
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_roster_household_size, 2)
  expect_equal(hh3_row$linked_roster_child_under2, 0)
  expect_equal(hh3_row$linked_roster_child_under5, 0)
  expect_equal(hh3_row$linked_roster_male, 0)
  expect_equal(hh3_row$linked_roster_female, 2)
  expect_equal(hh3_row$linked_roster_woman_15to49, 1)
})

test_that("Nutrition data aggregates canonical columns when they exist", {
  # Create household data
  suppressWarnings(
    hh_data <- suppressMessages(
      HouseholdData$new(
        data = tibble::tibble(
          uuid = c("hh1", "hh2", "hh3"),
          consent = c("yes", "yes", "yes"),
          interview_date = Sys.Date(),
          enumerator_id = c("E1", "E2", "E3")
        )
      )
    )
  )

  # Create nutrition data with canonical columns (as would be created by add_standardized_nutrition_demographics)
  suppressWarnings(
    nutr_data <- suppressMessages(
      NutritionIndividualData$new(
        data = tibble::tibble(
          person_id = c("n1", "n2", "n3", "n4", "n5", "n6"),
          hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3", "hh3"),
          calc_age_years = c(0.8, 2.5, 4, 6, 1.2, 1.6),
          nutrition_child_under2 = c(1, 0, 0, 0, 1, 1),
          nutrition_child_2to5 = c(0, 1, 1, 0, 0, 0),
          nutrition_child_under5 = c(1, 1, 1, 0, 1, 1)
        )
      )
    )
  )

  # Link and standardize (may message/warn)
  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("nutrition", nutr_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Check aggregated columns exist
  std_data <- hh_data$standardized_data
  expect_true("linked_nutrition_nutrition_child_under2" %in% names(std_data))
  expect_true("linked_nutrition_nutrition_child_2to5" %in% names(std_data))
  expect_true("linked_nutrition_nutrition_child_under5" %in% names(std_data))

  # Check values for hh1
  hh1_row <- std_data[std_data$uuid == "hh1", ]
  expect_equal(hh1_row$linked_nutrition_nutrition_child_under2, 1)
  expect_equal(hh1_row$linked_nutrition_nutrition_child_2to5, 1)
  expect_equal(hh1_row$linked_nutrition_nutrition_child_under5, 2)

  # Check values for hh2
  hh2_row <- std_data[std_data$uuid == "hh2", ]
  expect_equal(hh2_row$linked_nutrition_nutrition_child_under2, 0)
  expect_equal(hh2_row$linked_nutrition_nutrition_child_2to5, 1)
  expect_equal(hh2_row$linked_nutrition_nutrition_child_under5, 1)

  # Check values for hh3
  hh3_row <- std_data[std_data$uuid == "hh3", ]
  expect_equal(hh3_row$linked_nutrition_nutrition_child_under2, 2)
  expect_equal(hh3_row$linked_nutrition_nutrition_child_2to5, 0)
  expect_equal(hh3_row$linked_nutrition_nutrition_child_under5, 2)
})

test_that("generate_cleaning_log propagates to linked data objects", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes")
      )
    )
  ))

  # Create a linked roster dataset
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("p1", "p2"),
        hh_uuid = c("hh1", "hh2"),
        age_years = c(10, 25)
      )
    )
  ))

  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Calling generate_cleaning_log on household should not error and should also
  # attempt to run on the linked dataset
  suppressMessages(
    expect_no_error(hh_data$generate_cleaning_log())
  )

  # The linked dataset's generate_cleaning_log should have been called, which
  # means its cleaning_log field should now be a CleaningLog object (initialised
  # as part of the Data class construction, so it should still be a CleaningLog)
  expect_true(inherits(roster_data$cleaning_log, "CleaningLog"))
})

test_that("clean propagates to linked data objects", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("hh1", "hh2"),
        consent = c("yes", "yes")
      )
    )
  ))

  # Create a linked roster dataset
  roster_data <- suppressWarnings(suppressMessages(
    IndividualData$new(
      data = tibble::tibble(
        person_id = c("p1", "p2"),
        hh_uuid = c("hh1", "hh2"),
        age_years = c(10, 25)
      )
    )
  ))

  suppressWarnings(suppressMessages(
    hh_data$add_linked_dataset("roster", roster_data)
  ))
  suppressWarnings(suppressMessages(
    hh_data$standardize()
  ))

  # Calling clean on household should also clean the linked dataset
  suppressWarnings(suppressMessages(
    expect_no_error(hh_data$clean())
  ))

  # The household dataset itself should be cleaned
  expect_true(hh_data$cleaned)

  # The linked dataset should also be cleaned
  expect_true(roster_data$cleaned)
  expect_false(is.null(roster_data$clean_data))
})

test_that("clean and generate_cleaning_log skip non-Data linked objects gracefully", {
  # Create household data
  hh_data <- suppressWarnings(suppressMessages(HouseholdData$new(
    data = tibble::tibble(
      uuid = c("hh1"),
      consent = c("yes")
    )
  )))

  # Manually inject a non-Data linked object to verify graceful handling
  hh_data$linked_objects[["bad_link"]] <- list(object = list(not_a_data = TRUE))

  # Both methods should warn but not abort
  expect_no_error(suppressWarnings(suppressMessages(hh_data$generate_cleaning_log(
    stage = "raw"
  ))))
  expect_no_error(suppressWarnings(suppressMessages(hh_data$clean())))
})


# generate_data_analytics mortality ####

test_that("generate_data_analytics returns MortalityDataAnalytics without error (no linked data)", {
  hh_data <- suppressWarnings(suppressMessages(HouseholdData$new(
    data = tibble::tibble(
      uuid = c("hh1", "hh2"),
      consent = c("yes", "yes")
    )
  )))

  suppressWarnings(suppressMessages(hh_data$standardize()))

  result <- suppressWarnings(suppressMessages(
    hh_data$generate_data_analytics(stage = "standardized", type = "mortality")
  ))

  expect_s3_class(result, "MortalityDataAnalytics")
})

test_that("generate_data_analytics with mortality type succeeds when linked roster and deaths data are present", {
  hh_data <- suppressMessages(suppressWarnings(HouseholdData$new(
    data = tibble::tibble(
      uuid = c("hh1", "hh2"),
      consent = c("yes", "yes")
    )
  )))

  roster_data <- suppressMessages(suppressWarnings(IndividualData$new(
    data = tibble::tibble(
      person_id = c("p1", "p2", "p3"),
      hh_uuid = c("hh1", "hh1", "hh2"),
      age = c(10, 25, 5),
      sex = c("M", "F", "M")
    )
  )))

  deaths_data <- suppressMessages(suppressWarnings(DeathIndividualData$new(
    data = tibble::tibble(
      death_id = c("d1"),
      hh_uuid = c("hh1"),
      age_years = c(2),
      sex = c("M")
    ),
    recall_date = "2025-01-01"
  )))

  suppressMessages(suppressWarnings(hh_data$add_linked_dataset(
    "roster",
    roster_data
  )))
  suppressMessages(suppressWarnings(hh_data$add_linked_dataset(
    "deaths",
    deaths_data
  )))
  suppressMessages(suppressWarnings(hh_data$standardize()))

  result <- suppressWarnings(suppressMessages(
    hh_data$generate_data_analytics(stage = "standardized", type = "mortality")
  ))

  expect_s3_class(result, "MortalityDataAnalytics")
  expect_false(is.null(result$data))
  expect_false(is.null(result$linked_ind_roster_data))
  expect_false(is.null(result$linked_ind_deaths_data))
})


# generate_weights ####

test_that("generate_weights skips when no SamplingFrame is available", {
  hh <- suppressWarnings(suppressMessages(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("1", "2"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        stratum = c("A", "B")
      ),
      variable_map = list(stratum = "stratum")
    )
  ))

  # No sampling_frame set, no argument provided — should skip silently
  suppressMessages(expect_message(
    suppressWarnings(hh$generate_weights()),
    regexp = "No SamplingFrame available"
  ))
})

test_that("generate_weights skips when stratum not mapped in variable_map", {
  sf_df <- tibble::tibble(
    stratum = c("A", "B"),
    psu = c("PSU1", "PSU2"),
    population_size = c(1000, 2000),
    inclusion = c(TRUE, TRUE),
    sampled_psu = NA_character_,
    allocated_sample = NA_real_
  )
  suppressMessages(sf <- SamplingFrame$new(log_df = sf_df))

  hh <- suppressMessages(suppressWarnings(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("1", "2"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        strataz = c("A", "B")
      )
      # no stratum mapped in variable_map
    )
  ))

  suppressWarnings(suppressMessages(expect_message(
    hh$generate_weights(
      sampling_frame = sf,
      stage = "raw"
    ),
    regexp = "No stratum role"
  )))
})

test_that("generate_weights computes correct weights and writes to 'survey_weight' when no weight mapped", {
  sf_df <- tibble::tibble(
    stratum = c("Urban", "Rural"),
    psu = c("PSU1", "PSU2"),
    population_size = c(1000, 3000),
    inclusion = c(TRUE, TRUE),
    sampled_psu = NA_character_,
    allocated_sample = NA_real_
  )
  suppressMessages(sf <- SamplingFrame$new(log_df = sf_df))

  hh <- suppressMessages(suppressWarnings(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("h1", "h2", "h3", "h4"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        stratum = c("Urban", "Urban", "Rural", "Rural")
      ),
      variable_map = list(stratum = "stratum")
    )
  ))

  suppressMessages(suppressWarnings(
    hh$generate_weights(sampling_frame = sf, stage = "raw")
  ))

  df <- suppressMessages(suppressWarnings(hh$raw_data))
  expect_true("survey_weight" %in% names(df))
  # N=4000, n=4
  # Urban: (1000/4000) / (2/4) = 0.25 / 0.5 = 0.5
  expect_equal(df$survey_weight[df$stratum == "Urban"], c(0.5, 0.5))
  # Rural: (3000/4000) / (2/4) = 0.75 / 0.5 = 1.5
  expect_equal(df$survey_weight[df$stratum == "Rural"], c(1.5, 1.5))
  # variable_map should be updated
  expect_equal(hh$variable_map$weight, "survey_weight")
})

test_that("generate_weights uses existing mapped weight column when available", {
  sf_df <- tibble::tibble(
    stratum = c("Urban", "Rural"),
    psu = c("PSU1", "PSU2"),
    population_size = c(800, 2000),
    inclusion = c(TRUE, TRUE),
    sampled_psu = NA_character_,
    allocated_sample = NA_real_
  )
  suppressMessages(sf <- SamplingFrame$new(log_df = sf_df))

  hh <- suppressMessages(suppressWarnings(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("h1", "h2", "h3"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        stratum = c("Urban", "Rural", "Rural"),
        wt = c(1, 1, 1) # pre-existing weight column
      ),
      variable_map = list(stratum = "stratum", weight = "wt")
    )
  ))

  suppressMessages(suppressWarnings(
    hh$generate_weights(sampling_frame = sf, stage = "raw")
  ))

  df <- suppressMessages(suppressWarnings(hh$raw_data))
  # Should have written into 'wt', not created 'survey_weight'
  expect_false("survey_weight" %in% names(df))
  expect_equal(hh$variable_map$weight, "wt")
  # N=2800, n=3
  # Urban: (800/2800) / (1/3) = (2/7) / (1/3) = 6/7
  expect_equal(df$wt[df$stratum == "Urban"], 6 / 7)
  # Rural: (2000/2800) / (2/3) = (5/7) / (2/3) = 15/14
  expect_equal(df$wt[df$stratum == "Rural"], c(15 / 14, 15 / 14))
})

test_that("generate_weights produces NA and warns for strata not in SamplingFrame", {
  sf_df <- tibble::tibble(
    stratum = "Urban",
    psu = "PSU1",
    population_size = 500,
    inclusion = TRUE,
    sampled_psu = NA_character_,
    allocated_sample = NA_real_
  )
  suppressMessages(sf <- SamplingFrame$new(log_df = sf_df))

  hh <- suppressMessages(suppressWarnings(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("h1", "h2"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        stratum = c("Urban", "Rural") # Rural not in SamplingFrame
      ),
      variable_map = list(stratum = "stratum")
    )
  ))

  suppressWarnings(expect_warning(
    suppressMessages(hh$generate_weights(
      sampling_frame = sf,
      stage = "raw"
    )),
    regexp = "Rural"
  ))

  df <- suppressMessages(suppressWarnings(hh$raw_data))
  # Urban: N=500 (only stratum), N_s=500, n=2 valid rows, n_s=1 -> (500/500)/(1/2) = 2
  expect_equal(df$survey_weight[df$stratum == "Urban"], 2)
  expect_true(is.na(df$survey_weight[df$stratum == "Rural"]))
})

test_that("generate_weights is called automatically inside pre_standardize when sampling_frame is set", {
  sf_df <- tibble::tibble(
    stratum = c("A", "B"),
    psu = c("PSU1", "PSU2"),
    population_size = c(600, 900),
    inclusion = c(TRUE, TRUE),
    sampled_psu = NA_character_,
    allocated_sample = NA_real_
  )
  suppressMessages(sf <- SamplingFrame$new(log_df = sf_df))

  hh <- suppressMessages(suppressWarnings(
    HouseholdData$new(
      data = tibble::tibble(
        uuid = c("h1", "h2", "h3"),
        consent = "yes",
        interview_date = Sys.Date(),
        enumerator_id = "E1",
        stratum = c("A", "B", "B")
      ),
      variable_map = list(stratum = "stratum")
    )
  ))

  hh$sampling_frame <- sf

  suppressMessages(suppressWarnings(hh$standardize()))

  df <- suppressMessages(suppressWarnings(hh$standardized_data))
  expect_true("survey_weight" %in% names(df))
  # N=1500, n=3
  # A: (600/1500) / (1/3) = 0.4 / (1/3) = 1.2
  expect_equal(df$survey_weight[df$stratum == "A"], 1.2)
  # B: (900/1500) / (2/3) = 0.6 / (2/3) = 0.9
  expect_equal(df$survey_weight[df$stratum == "B"], c(0.9, 0.9))
})
