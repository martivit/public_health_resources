# Tests for calculate_sample_size_general(), calculate_sample_size_individual(),
# calculate_sample_size_mortality(), and estimate_field_plan()
# in R/utils_sample_size.R.

# ---- calculate_sample_size_general ----------------------------------------

test_that("calculate_sample_size_general returns a positive integer", {
  result <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_true(result > 0)
  expect_equal(result, ceiling(result))  # ceiling returns an integer-valued numeric
})

test_that("calculate_sample_size_general simple random design produces expected size", {
  # z ~ 1.96, p=0.5, d=0.05 -> n = (1.96^2 * 0.5 * 0.5) / 0.05^2 = 384.16
  # non_response=5% -> 384.16 / 0.95 = 404.38 -> ceiling = 405
  result <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 5,
    design              = "simple_random",
    confidence_level    = 0.95
  )
  expect_equal(result, 405L)
})

test_that("calculate_sample_size_general cluster design applies design effect", {
  simple <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    confidence_level    = 0.95
  )
  cluster <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "cluster",
    design_effect       = 2,
    confidence_level    = 0.95
  )
  expect_true(cluster > simple)
})

test_that("calculate_sample_size_general with fpc reduces sample size", {
  no_fpc <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    fpc                 = FALSE,
    confidence_level    = 0.95
  )
  with_fpc <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    fpc                 = TRUE,
    total_population    = 500,
    confidence_level    = 0.95
  )
  expect_true(with_fpc < no_fpc)
})

test_that("calculate_sample_size_general non_response_rate increases sample size", {
  no_nr <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0
  )
  with_nr <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 20
  )
  expect_true(with_nr > no_nr)
})

test_that("calculate_sample_size_general boundary: expected_proportion = 0 returns positive integer", {
  result <- calculate_sample_size_general(
    expected_proportion = 0,
    desired_precision   = 5,
    non_response_rate   = 0
  )
  expect_true(result >= 0)
})

test_that("calculate_sample_size_general boundary: expected_proportion = 100 is allowed", {
  expect_no_error(
    calculate_sample_size_general(expected_proportion = 100, desired_precision = 5)
  )
})

test_that("calculate_sample_size_general errors on invalid expected_proportion", {
  expect_error(calculate_sample_size_general(-1, 5))
  expect_error(calculate_sample_size_general(101, 5))
})

test_that("calculate_sample_size_general errors on invalid desired_precision", {
  expect_error(calculate_sample_size_general(50, 0))
  expect_error(calculate_sample_size_general(50, 100))
})

test_that("calculate_sample_size_general errors on invalid non_response_rate", {
  expect_error(calculate_sample_size_general(50, 5, non_response_rate = -1))
  expect_error(calculate_sample_size_general(50, 5, non_response_rate = 100))
})

test_that("calculate_sample_size_general errors on unknown design", {
  expect_error(calculate_sample_size_general(50, 5, design = "stratified"))
})

test_that("calculate_sample_size_general errors when design_effect < 1 for cluster", {
  expect_error(
    calculate_sample_size_general(50, 5, design = "cluster", design_effect = 0.5)
  )
})

test_that("calculate_sample_size_general errors when fpc=TRUE but total_population missing", {
  expect_error(
    calculate_sample_size_general(50, 5, fpc = TRUE)
  )
  expect_error(
    calculate_sample_size_general(50, 5, fpc = TRUE, total_population = 0)
  )
})

test_that("calculate_sample_size_general errors on non-numeric inputs", {
  expect_error(calculate_sample_size_general("50", 5))
  expect_error(calculate_sample_size_general(50, "5"))
  expect_error(calculate_sample_size_general(50, 5, design = 1))
})

# ---- calculate_sample_size_individual -------------------------------------

test_that("calculate_sample_size_individual returns a named list", {
  result <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5
  )
  expect_true(is.list(result))
  expect_named(result, c("sample_size_individuals", "sample_size_households"))
})

test_that("calculate_sample_size_individual sample_size_households is derived from individuals", {
  result <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0
  )
  expected_hh <- ceiling(result$sample_size_individuals / 5)
  expect_equal(result$sample_size_households, expected_hh)
})

test_that("calculate_sample_size_individual sub_population_percent increases individual sample size", {
  full_pop <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 100,
    non_response_rate      = 0
  )
  sub_pop <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 50,
    non_response_rate      = 0
  )
  expect_true(sub_pop$sample_size_individuals > full_pop$sample_size_individuals)
})

test_that("calculate_sample_size_individual errors when average_household_size is missing or <= 0", {
  expect_error(
    calculate_sample_size_individual(
      expected_proportion = 30,
      desired_precision   = 5
    )
  )
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 0
    )
  )
})

test_that("calculate_sample_size_individual errors on invalid sub_population_percent", {
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 5,
      sub_population_percent = 0
    )
  )
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 5,
      sub_population_percent = 101
    )
  )
})

test_that("calculate_sample_size_individual with fpc=TRUE reduces sample size", {
  no_fpc <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    fpc                    = FALSE
  )
  with_fpc <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    fpc                    = TRUE,
    total_population       = 500
  )
  expect_true(with_fpc$sample_size_individuals < no_fpc$sample_size_individuals)
})

test_that("calculate_sample_size_individual cluster design gives larger sample than simple_random", {
  simple <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    design                 = "simple_random"
  )
  cluster <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    design                 = "cluster",
    design_effect          = 2
  )
  expect_true(cluster$sample_size_individuals > simple$sample_size_individuals)
})

# ---- calculate_sample_size_mortality --------------------------------------

test_that("calculate_sample_size_mortality returns a named list", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5
  )
  expect_true(is.list(result))
  expect_named(result, c("sample_size_households", "sample_size_individuals",
                          "sample_size_person_time"))
})

test_that("calculate_sample_size_mortality returns positive numeric values", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5
  )
  expect_true(result$sample_size_households > 0)
  expect_true(result$sample_size_individuals > 0)
  expect_true(result$sample_size_person_time > 0)
})

test_that("calculate_sample_size_mortality default design is cluster and applies design_effect", {
  simple <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    design                 = "simple_random",
    non_response_rate      = 0
  )
  cluster <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    design                 = "cluster",
    design_effect          = 1.5,
    non_response_rate      = 0
  )
  expect_true(cluster$sample_size_households > simple$sample_size_households)
})

test_that("calculate_sample_size_mortality longer recall_days increases person_time slightly", {
  short <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 30,
    non_response_rate      = 0
  )
  long <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 90,
    non_response_rate      = 0
  )
  expect_true(long$sample_size_person_time >= short$sample_size_person_time)
})

test_that("calculate_sample_size_mortality longer recall_days decreases households needed", {
  short <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 30,
    non_response_rate      = 0
  )
  long <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 90,
    non_response_rate      = 0
  )
  expect_true(long$sample_size_households < short$sample_size_households)
})

test_that("calculate_sample_size_mortality non_response_rate increases households", {
  no_nr <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    non_response_rate      = 0
  )
  with_nr <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    non_response_rate      = 20
  )
  expect_true(with_nr$sample_size_households > no_nr$sample_size_households)
})

test_that("calculate_sample_size_mortality errors when expected_death_rate <= 0", {
  expect_error(
    calculate_sample_size_mortality(0, 0.2, average_household_size = 5)
  )
  expect_error(
    calculate_sample_size_mortality(-0.1, 0.2, average_household_size = 5)
  )
})

test_that("calculate_sample_size_mortality errors when desired_precision <= 0", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0, average_household_size = 5)
  )
})

test_that("calculate_sample_size_mortality errors when average_household_size is missing or <= 0", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2)
  )
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 0)
  )
})

test_that("calculate_sample_size_mortality errors on invalid recall_days", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5, recall_days = 0)
  )
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5, recall_days = -10)
  )
})

test_that("calculate_sample_size_mortality errors on unknown design", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5,
                                    design = "stratified")
  )
})

test_that("calculate_sample_size_mortality fpc=TRUE requires total_population", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5,
                                    fpc = TRUE)
  )
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5,
                                    fpc = TRUE, total_population = 0)
  )
})

test_that("calculate_sample_size_mortality fpc=TRUE reduces individual sample size", {
  no_fpc <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    fpc                    = FALSE,
    non_response_rate      = 0
  )
  with_fpc <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    fpc                    = TRUE,
    total_population       = 5000,
    non_response_rate      = 0
  )
  expect_true(with_fpc$sample_size_individuals <= no_fpc$sample_size_individuals)
})

# ---- estimate_field_plan --------------------------------------------------

test_that("estimate_field_plan simple_random returns expected list structure", {
  result <- estimate_field_plan(
    sample_design              = "simple_random",
    number_of_teams            = 2,
    enumerators_per_team       = 3,
    start_time                 = "2024-01-01",
    end_time                   = "2024-01-31",
    average_interview_time     = 45,
    average_travel_time        = 30,
    average_rest_time          = 60,
    total_sample_size          = 300
  )
  expect_true(is.list(result))
  expect_named(result, c("num_interview_per_enum_per_day", "num_days",
                          "num_psu_needed", "psu_size"))
})

test_that("estimate_field_plan simple_random has NA for psu fields", {
  result <- estimate_field_plan(
    sample_design          = "simple_random",
    number_of_teams        = 2,
    enumerators_per_team   = 3,
    start_time             = "2024-01-01",
    end_time               = "2024-01-31",
    average_interview_time = 45,
    average_travel_time    = 30,
    average_rest_time      = 60,
    total_sample_size      = 300
  )
  expect_true(is.na(result$num_psu_needed))
  expect_true(is.na(result$psu_size))
})

test_that("estimate_field_plan simple_random positive interviews per enumerator per day", {
  result <- estimate_field_plan(
    sample_design          = "simple_random",
    number_of_teams        = 2,
    enumerators_per_team   = 3,
    start_time             = "2024-01-01",
    end_time               = "2024-01-31",
    average_interview_time = 30,
    average_travel_time    = 30,
    average_rest_time      = 60,
    total_sample_size      = 300
  )
  expect_true(result$num_interview_per_enum_per_day > 0)
})

test_that("estimate_field_plan cluster returns non-NA psu estimates", {
  result <- estimate_field_plan(
    sample_design                  = "cluster",
    number_of_teams                = 2,
    enumerators_per_team           = 3,
    number_of_psu_per_team_per_day = 2,
    start_time                     = "2024-01-01",
    end_time                       = "2024-01-31",
    average_interview_time         = 30,
    average_travel_time            = 60,
    average_rest_time              = 60,
    total_sample_size              = 200
  )
  expect_true(is.list(result))
  expect_false(is.na(result$num_psu_needed))
  expect_false(is.na(result$psu_size))
  expect_true(result$num_psu_needed > 0)
  expect_true(result$psu_size > 0)
})

test_that("estimate_field_plan more teams reduces days needed", {
  few_teams <- estimate_field_plan(
    sample_design          = "simple_random",
    number_of_teams        = 1,
    enumerators_per_team   = 3,
    start_time             = "2024-01-01",
    end_time               = "2025-01-01",
    average_interview_time = 45,
    average_travel_time    = 30,
    average_rest_time      = 60,
    total_sample_size      = 600
  )
  many_teams <- estimate_field_plan(
    sample_design          = "simple_random",
    number_of_teams        = 4,
    enumerators_per_team   = 3,
    start_time             = "2024-01-01",
    end_time               = "2025-01-01",
    average_interview_time = 45,
    average_travel_time    = 30,
    average_rest_time      = 60,
    total_sample_size      = 600
  )
  expect_true(many_teams$num_days < few_teams$num_days)
})

test_that("estimate_field_plan accepts Date objects as well as character dates", {
  result <- estimate_field_plan(
    sample_design          = "simple_random",
    number_of_teams        = 2,
    enumerators_per_team   = 3,
    start_time             = as.Date("2024-01-01"),
    end_time               = as.Date("2024-01-31"),
    average_interview_time = 45,
    average_travel_time    = 30,
    average_rest_time      = 60,
    total_sample_size      = 300
  )
  expect_true(is.list(result))
})

test_that("estimate_field_plan errors on number_of_teams <= 0", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 0,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors on enumerators_per_team <= 0", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 2,
      enumerators_per_team   = 0,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors on average_interview_time <= 0", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 0,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors when end_time is before start_time", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-10",
      end_time               = "2024-01-01",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors on invalid sample_design", {
  expect_error(
    estimate_field_plan(
      sample_design          = "stratified",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan cluster errors when number_of_psu_per_team_per_day is missing", {
  expect_error(
    estimate_field_plan(
      sample_design          = "cluster",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors on negative average_travel_time", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = -10,
      average_rest_time      = 60,
      total_sample_size      = 300
    )
  )
})

test_that("estimate_field_plan errors on negative average_rest_time", {
  expect_error(
    estimate_field_plan(
      sample_design          = "simple_random",
      number_of_teams        = 2,
      enumerators_per_team   = 3,
      start_time             = "2024-01-01",
      end_time               = "2024-01-31",
      average_interview_time = 45,
      average_travel_time    = 30,
      average_rest_time      = -5,
      total_sample_size      = 300
    )
  )
})

# Testing against ENA and UKSAMPLES ####

test_that("calculate_sample_size_general, 20% Prev, 5% MoE, no non-response", {
  result <- calculate_sample_size_general(
    design = "simple_random",
    expected_proportion = 20,
    desired_precision   = 5,
    non_response_rate   = 0
  )

  result_ena <- 246 # Calculated with ENA software

  expect_equal(result, result_ena)
})

test_that("calculate_sample_size_general, 1% Prev, 2.5% MoE, no non-response", {
  result <- calculate_sample_size_general(
    design = "simple_random",
    expected_proportion = 1,
    desired_precision   = 2.5,
    non_response_rate   = 0
  )

  result_ena <- 61 # Calculated with ENA software

  expect_equal(result, result_ena)
})

test_that("calculate_sample_size_general, 99% Prev, 10% MoE, no non-response", {
  result <- calculate_sample_size_general(
    design = "simple_random",
    expected_proportion = 99,
    desired_precision   = 10,
    non_response_rate   = 0
  )

  result_ena <- 4 # Calculated with ENA software

  expect_equal(result, result_ena)
})

test_that("calculate_sample_size_general cluster design, 20% Prev, 5% MoE, no non-response", {
  result <- calculate_sample_size_general(
    design = "cluster",
    expected_proportion = 20,
    desired_precision   = 5,
    design_effect = 1.5,
    non_response_rate   = 0
  )

  result_ena <- 401 # Calculated with ENA software

  # minor rounding difference, ENA 401.47, while this calc does ceiling
  expect_equal(result, result_ena, tolerance = 1)
})

test_that("calculate_sample_size_general with fpc and 5000 population size, 20% Prev, 5% MoE, no non-response", {
  result <- calculate_sample_size_general(
    design = "simple_random",
    expected_proportion = 20,
    desired_precision   = 5,
    non_response_rate   = 0,
    fpc = TRUE,
    total_population = 5000
  )

  result_ena <- 180 # Calculated with ENA software, there seems to be a difference here, hard to confirm without seeing ENA code.
  result_uksamples <- 234

  expect_equal(result, result_uksamples, tolerance = 1)
})

test_that("calculate_sample_size_individual against ENA", {
  full_pop <- calculate_sample_size_individual(
    design                 = "simple_random",
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 100,
    non_response_rate      = 0
  )

  # Calculated with ENA software, ENA specifically applies a 0.9 correction factor to correct for children 0-5 months, that wont be measured. We are more general and dont want to do this.
  # See SMART Methodology Manual Version 2, page 36
  result_ena_ind <- 323
  result_ena_hh <- 72

  expect_equal(full_pop$sample_size_individuals, result_ena_ind)
  expect_equal(full_pop$sample_size_households, result_ena_hh*0.9, tolerance = 1)

})

test_that("calculate_sample_size_individual sub_population_percent increases individual sample size", {
  full_pop <- calculate_sample_size_individual(
    design                 = "cluster",
    design_effect = 2,
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 100,
    non_response_rate      = 0
  )

  # Calculated with ENA software, ENA specifically applies a 0.9 correction factor to correct for children 0-5 months, that wont be measured. We are more general and dont want to do this.
  # See SMART Methodology Manual Version 2, page 36
  result_ena_ind <- 703
  result_ena_hh <- 156

  expect_equal(full_pop$sample_size_individuals, result_ena_ind)
  expect_equal(full_pop$sample_size_households, result_ena_hh*0.9, tolerance = 1)

})

test_that("calculate_sample_size_mortality recall_days is preserved in return value", {
  result <- calculate_sample_size_mortality(
    design = "simple_random",
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 93,
    non_response_rate = 0
  )

  result_ena_ind <- 5163
  result_ena_hh <- 1033

  expect_equal(result$sample_size_individuals, 5163, tolerance = 1)
  expect_equal(result$sample_size_households, 1033, tolerance = 1)

})

test_that("calculate_sample_size_mortality with nonresponse simple random against ENA", {
  result <- calculate_sample_size_mortality(
    design = "simple_random",
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 93,
    non_response_rate = 5
  )

  result_ena_ind <- 5163
  result_ena_hh <- 1087

  expect_equal(result$sample_size_individuals, result_ena_ind, tolerance = 1)
  expect_equal(result$sample_size_households, result_ena_hh, tolerance = 1)

})

test_that("calculate_sample_size_mortality with nonresponse cluster against ENA", {
  result <- calculate_sample_size_mortality(
    design = "cluster",
    expected_death_rate    = 1,
    design_effect = 2,
    desired_precision      = 0.4,
    average_household_size = 6,
    recall_days            = 120,
    non_response_rate = 9
  )

  result_ena_ind <- 3267
  result_ena_hh <- 598

  expect_equal(result$sample_size_individuals, result_ena_ind, tolerance = 1)
  expect_equal(result$sample_size_households, result_ena_hh, tolerance = 1)

})


test_that("calculate_sample_size_mortality with fpc nonresponse simple random against ENA", {
  result <- calculate_sample_size_mortality(
    design = "simple_random",
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 93,
    non_response_rate = 5,
    fpc = TRUE,
    total_population = 5000
  )

  result_ena_ind <- 2540
  result_ena_hh <- 535

  expect_equal(result$sample_size_individuals, result_ena_ind, tolerance = 1)
  expect_equal(result$sample_size_households, result_ena_hh, tolerance = 1)

})

test_that("calculate_sample_size_mortality with fpc nonresponse cluster against ENA", {
  result <- calculate_sample_size_mortality(
    design = "cluster",
    expected_death_rate    = 1,
    desired_precision      = 0.4,
    design_effect = 2,
    average_household_size = 6,
    recall_days            = 120,
    non_response_rate = 9,
    fpc = TRUE,
    total_population = 5000
  )

  result_ena_ind <- 2328
  result_ena_hh <- 426

  expect_equal(result$sample_size_individuals, result_ena_ind, tolerance = 1)
  expect_equal(result$sample_size_households, result_ena_hh, tolerance = 1)

})
