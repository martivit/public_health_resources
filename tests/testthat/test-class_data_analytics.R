library(testthat)
library(tibble)

# ============================================================================
# DataAnalytics Base Class Tests
# ============================================================================

test_that("DataAnalytics initializes with valid data", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))

  # Suppress expected, non-consequential startup output (warnings/messages)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "TestDA")
    )
  )

  expect_s3_class(da, "DataAnalytics")
  expect_equal(da$dataset_name, "TestDA")
  expect_equal(nrow(da$data), 10)
  expect_null(da$parent_data_object)

  # Quality fields initialized
  expect_true(is.list(da$quality_schema))
  expect_true(is.list(da$plausibility_results))

  # Unified outputs schema initialized
  expect_true(is.list(da$outputs_schema))

  # Analysis fields initialized
  expect_s3_class(da$data_analysis_plan, "QuantDataAnalysisPlanLog")
  expect_true(tibble::is_tibble(da$analysis_plan_issue_log))
  expect_true(is.list(da$analysis_results))

  # Shared output containers
  expect_true(is.list(da$visualizations))
  expect_true(is.list(da$tables))
})

test_that("DataAnalytics can initialize without data", {
  # Should not error; data is optional (NULL)
  da <- DataAnalytics$new(dataset_name = "EmptyDA")
  expect_s3_class(da, "DataAnalytics")
  expect_null(da$data)
  expect_null(da$survey_design)
})

test_that("DataAnalytics errors when data is not a data frame", {
  expect_error(
    DataAnalytics$new(data = c(1, 2, 3), dataset_name = "NotDF"),
    regexp = "Expected a data frame"
  )
})

test_that("DataAnalytics has separate quality_schema and analysis_schema", {
  df <- tibble::tibble(id = 1:5, val = 1:5)

  # Suppress expected, non-consequential startup output (warnings/messages)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "SchemaSepDA")
  )

  expect_true(is.list(da$quality_schema))
  expect_true(
    is.null(da$analysis_schema) ||
      tibble::is_tibble(da$analysis_schema) ||
      is.data.frame(da$analysis_schema)
  )
})

test_that("DataAnalytics set_quality_schema works", {
  df <- tibble::tibble(id = 1:5, val = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "SetSchemaDA")
  )

  custom_schema <- list(
    test_check = list(
      check_name = "test_check",
      check_label = "Test Check",
      statistical_test = "range_violation",
      variables = c("val"),
      thresholds = list(
        list(expression = "test_statistic <= 5", penalty = 0),
        list(expression = "test_statistic > 5", penalty = 5)
      ),
      test_params = list(min_value = 0, max_value = 10)
    )
  )

  # Suppress expected output from setting schema as well (if it messages)
  suppressMessages(
    da$set_quality_schema(custom_schema)
  )

  expect_equal(length(da$quality_schema), 1)
  expect_equal(names(da$quality_schema), "test_check")
})

test_that("DataAnalytics get_quality_schema returns correct schema", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "GetSchemaDA")
  )

  schema <- da$get_quality_schema()
  expect_true(is.list(schema))
})

test_that("DataAnalytics add_indicator_dap works", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "AddIndicatorDA")
  )

  # Clear any auto-generated plan rows
  da$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = character(),
    calculation = character(),
    var_name = character(),
    denom_var = character(),
    disaggregation = character(),
    multiplier = numeric(),
    indicator_unit = character()
  )

  # Suppress expected messages emitted by add_indicator_dap()
  suppressMessages(
    da$add_indicator_dap(
      indicator_name = "Test",
      calculation = "prop",
      var_name = "x"
    )
  )

  expect_equal(nrow(da$data_analysis_plan$log_df), 1)
  expect_equal(da$data_analysis_plan$log_df$indicator_name, "Test")
})

test_that("DataAnalytics remove_indicator_dap works", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10), y = rnorm(10))

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "RemoveIndicatorDA")
  )

  da$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = character(),
    calculation = character(),
    var_name = character(),
    denom_var = character(),
    disaggregation = character(),
    multiplier = numeric(),
    indicator_unit = character()
  )

  # Suppress expected messages emitted by add/remove calls
  suppressMessages(da$add_indicator_dap("A", "prop", "x"))
  suppressMessages(da$add_indicator_dap("B", "mean", "y"))
  suppressMessages(da$remove_indicator_dap("A"))

  expect_equal(nrow(da$data_analysis_plan$log_df), 1)
  expect_equal(da$data_analysis_plan$log_df$indicator_name, "B")
})

test_that("DataAnalytics validate_plan catches invalid calculation", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "ValidatePlanDA")
  )

  da$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "Bad",
    calculation = "invalid_type",
    var_name = "x",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  # validate_plan() is expected to warn when it finds issues; suppress console output
  suppressMessages(
    da$validate_plan()
  )

  expect_gt(nrow(da$analysis_plan_issue_log), 0)
})

test_that("DataAnalytics results_to_table returns empty tibble when no results", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "EmptyResultsDA")
  )

  tbl <- da$results_to_table()
  expect_true(tibble::is_tibble(tbl))
  expect_equal(nrow(tbl), 0)
})

test_that("DataAnalytics summary returns expected keys", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "SummaryDA")
  )

  s <- da$summary()
  expect_true("dataset_name" %in% names(s))
  expect_true("n_records" %in% names(s))
  expect_true("overall_score" %in% names(s))
  expect_true("n_checks" %in% names(s))
})

test_that("DataAnalytics analysis_results is separate from quality results", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "SeparateResultsDA")
  )

  # Quality results stored in self$plausibility_results
  expect_true(is.list(da$plausibility_results))

  # Analysis results stored in self$analysis_results
  expect_true(is.list(da$analysis_results))
})

test_that("DataAnalytics export_state_object captures both result types", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "StateDA")
  )

  state <- da$export_state_object()
  expect_true("plausibility_results" %in% names(state)) # quality results
  expect_true("analysis_results" %in% names(state)) # analysis results
  expect_true("quality_schema" %in% names(state))
  expect_true("analysis_schema" %in% names(state))
})

test_that("DataAnalytics load_state_object restores state", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  da <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "LoadStateDA")
  )

  state <- da$export_state_object()

  # Suppress expected, non-consequential initialization chatter again for the second object
  da2 <- suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "LoadStateDA2")
  )

  # load_state_object may also message; suppress if it does
  suppressMessages(
    da2$load_state_object(state)
  )

  expect_equal(da2$analysis_schema, da$analysis_schema)
})

# ============================================================================
# FSLDataAnalytics Subclass Tests
# ============================================================================

test_that("FSLDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    FSLDataAnalytics$new(data = df, dataset_name = "FSLTest")
  )

  expect_s3_class(da, "FSLDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
  expect_equal(da$dataset_name, "FSLTest")
})

# ============================================================================
# WASHDataAnalytics Subclass Tests
# ============================================================================

test_that("WASHDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    WASHDataAnalytics$new(data = df, dataset_name = "WASHTest")
  )

  expect_s3_class(da, "WASHDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
  expect_null(da$linked_containers_data)
})

test_that("WASHDataAnalytics stores linked containers data", {
  df <- tibble::tibble(id = 1:5)
  containers <- tibble::tibble(hh_id = 1:3, volume = c(10, 20, 30))

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    WASHDataAnalytics$new(
      data = df,
      dataset_name = "WASHLinkedTest",
      linked_containers_data = containers
    )
  )

  expect_equal(nrow(da$linked_containers_data), 3)
})

# ============================================================================
# HealthDataAnalytics Subclass Tests
# ============================================================================

test_that("HealthDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  da <- suppressMessages(
    HealthDataAnalytics$new(data = df, dataset_name = "HealthTest")
  )

  expect_s3_class(da, "HealthDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
  expect_null(da$linked_ind_health_data)
})

# ============================================================================
# MortalityDataAnalytics Subclass Tests
# ============================================================================

test_that("MortalityDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      MortalityDataAnalytics$new(data = df, dataset_name = "MortTest")
    )
  )

  expect_s3_class(da, "MortalityDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
  expect_null(da$linked_ind_roster_data)
  expect_null(da$linked_ind_deaths_data)
})

# ============================================================================
# DemographicsDataAnalytics Subclass Tests
# ============================================================================

test_that("DemographicsDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DemographicsDataAnalytics$new(data = df, dataset_name = "DemogTest")
    )
  )

  expect_s3_class(da, "DemographicsDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
})

# ============================================================================
# GeneralDataAnalytics Subclass Tests
# ============================================================================

test_that("GeneralDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      GeneralDataAnalytics$new(data = df, dataset_name = "GeneralTest")
    )
  )

  expect_s3_class(da, "GeneralDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
})

# ============================================================================
# Diagnose Method Tests
# ============================================================================

test_that("DataAnalytics initializes diagnose log fields as empty tibbles", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "DiagInit")
    )
  )

  expect_true(tibble::is_tibble(da$quality_issues_log))
  expect_true(tibble::is_tibble(da$analysis_plan_issues_log))
  expect_true(tibble::is_tibble(da$outputs_issues_log))
})

test_that("quality_diagnose returns empty tibble when no quality_schema", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "QDiagEmpty")
    )
  )

  # Ensure schema is empty
  da$quality_schema <- list()

  suppressWarnings(
    result <- suppressMessages(
      da$quality_diagnose()
    )
  )
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_true(tibble::is_tibble(da$quality_issues_log))
})

test_that("quality_diagnose detects present and missing variables", {
  df <- tibble::tibble(id = 1:5, fcs_score = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "QDiagVars")
    )
  )

  da$quality_schema <- list(
    check_ok = list(
      check_name = "check_ok",
      check_label = "Present var",
      statistical_test = "range_violation",
      variables = c("fcs_score"),
      thresholds = list(),
      test_params = list()
    ),
    check_bad = list(
      check_name = "check_bad",
      check_label = "Missing var",
      statistical_test = "range_violation",
      variables = c("nonexistent_col"),
      thresholds = list(),
      test_params = list()
    )
  )

  suppressWarnings(
    result <- suppressMessages(
      da$quality_diagnose()
    )
  )
  expect_equal(nrow(result), 2)
  expect_true("status" %in% names(result))
  expect_true("variables_in_data" %in% names(result))
  expect_true("function_available" %in% names(result))

  ok_row <- result[result$check_name == "check_ok", ]
  bad_row <- result[result$check_name == "check_bad", ]
  expect_true(ok_row$variables_in_data)
  expect_false(bad_row$variables_in_data)
  expect_equal(ok_row$status, "ok")
  expect_false(bad_row$status == "ok")

  # Also stored in field
  expect_equal(nrow(da$quality_issues_log), 2)
})

test_that("quality_diagnose flags unavailable test function", {
  df <- tibble::tibble(id = 1:5, val = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "QDiagFunc")
    )
  )

  da$quality_schema <- list(
    check_fake = list(
      check_name = "check_fake",
      check_label = "Fake test",
      statistical_test = "this_function_does_not_exist_xyz",
      variables = c("val"),
      thresholds = list(),
      test_params = list()
    )
  )

  suppressWarnings(
    result <- suppressMessages(
      da$quality_diagnose()
    )
  )
  expect_false(result$function_available[1])
  expect_false(result$status[1] == "ok")
})

test_that("analysis_diagnose returns empty tibble when no analysis schema", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "ADiagEmpty")
    )
  )

  da$analysis_schema <- tibble::tibble(
    indicator_name = character(),
    calculation = character(),
    var_name = character(),
    denom_var = character(),
    disaggregation = character(),
    multiplier = numeric(),
    indicator_unit = character()
  )

  suppressWarnings(
    result <- suppressMessages(
      da$analysis_diagnose()
    )
  )
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
})

test_that("analysis_diagnose correctly identifies valid and invalid indicators", {
  df <- tibble::tibble(
    id = 1:10,
    fcs_score = rnorm(10),
    group = rep(c("A", "B"), 5)
  )

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "ADiagVars")
    )
  )

  da$analysis_schema <- tibble::tibble(
    indicator_name = c("Valid", "Bad var", "Bad calc", "Select Multiple"),
    calculation = c("mean", "mean", "invalid_type", "select_multiple_cat"),
    var_name = c("fcs_score", "nonexistent", "fcs_score", "fcs_score"),
    denom_var = c(NA_character_, NA_character_, NA_character_, NA_character_),
    disaggregation = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    multiplier = c(1, 1, 1, 1),
    indicator_unit = c("%", "%", "%", "%")
  )

  suppressWarnings(
    result <- suppressMessages(
      da$analysis_diagnose()
    )
  )
  expect_equal(nrow(result), 4)
  expect_true("var_name_in_data" %in% names(result))
  expect_true("calculation_valid" %in% names(result))
  expect_true("status" %in% names(result))

  expect_equal(result$status[result$indicator_name == "Valid"], "ok")
  expect_false(result$status[result$indicator_name == "Bad var"] == "ok")
  expect_false(result$status[result$indicator_name == "Bad calc"] == "ok")
  expect_equal(result$status[result$indicator_name == "Select Multiple"], "ok")

  # Stored in field
  expect_equal(nrow(da$analysis_plan_issues_log), 4)
})

test_that("outputs_diagnose returns empty tibble when no schema", {
  df <- tibble::tibble(id = 1:5, x = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "OQDiagEmpty")
    )
  )

  da$outputs_schema <- list()

  suppressWarnings(
    result <- suppressMessages(
      da$outputs_diagnose()
    )
  )
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_true(tibble::is_tibble(da$outputs_issues_log))
})

test_that("outputs_diagnose flags unavailable function", {
  df <- tibble::tibble(id = 1:5, val = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "OQDiagFunc")
    )
  )

  da$outputs_schema <- list(
    out1 = list(
      output_title = "out1",
      output_name = "Out 1",
      output_func_name = "this_func_does_not_exist_xyz",
      output_type = "visualization",
      variables = c("val"),
      test_params = list(),
      outputs_group = NULL
    )
  )

  suppressWarnings(
    result <- suppressMessages(
      da$outputs_diagnose()
    )
  )
  expect_equal(nrow(result), 1)
  expect_false(result$function_available[1])
  expect_false(result$status[1] == "ok")
})

test_that("outputs_diagnose flags missing variable", {
  df <- tibble::tibble(id = 1:5, val = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "OADiagVar")
    )
  )

  da$outputs_schema <- list(
    out_missing = list(
      output_title = "out_missing",
      output_name = "Missing var output",
      output_func_name = "plot_stacked_bar",
      output_type = "visualization",
      variables = c("nonexistent_col"),
      test_params = list(),
      outputs_group = NULL
    )
  )

  suppressWarnings(
    result <- suppressMessages(
      da$outputs_diagnose()
    )
  )
  expect_equal(nrow(result), 1)
  expect_false(result$variables_in_data[1])
  expect_false(result$status[1] == "ok")

  # Stored in field
  expect_equal(nrow(da$outputs_issues_log), 1)
})

test_that("analysis_diagnose resolves canonical var names via variable_map", {
  df <- tibble::tibble(id = 1:5, actual_score = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "ADiagVarMap")
    )
  )

  da$variable_map <- list(canonical_score = "actual_score")

  da$analysis_schema <- tibble::tibble(
    indicator_name = c("Mapped var"),
    calculation = c("mean"),
    var_name = c("canonical_score"),
    denom_var = c(NA_character_),
    disaggregation = c(NA_character_),
    multiplier = c(1),
    indicator_unit = c("%")
  )

  suppressWarnings(
    result <- suppressMessages(
      da$analysis_diagnose()
    )
  )
  expect_equal(nrow(result), 1)
  expect_true(result$var_name_in_data[1])
  expect_equal(result$status[1], "ok")
})

test_that("outputs_diagnose resolves canonical var names via variable_map", {
  df <- tibble::tibble(id = 1:5, actual_col = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "ODiagVarMap")
    )
  )

  da$variable_map <- list(canonical_col = "actual_col")

  da$outputs_schema <- list(
    out_mapped = list(
      output_title = "out_mapped",
      output_name = "Mapped var output",
      output_func_name = "plot_stacked_bar",
      output_type = "visualization",
      variables = c("canonical_col"),
      test_params = list(),
      outputs_group = NULL
    )
  )

  result <- suppressMessages(
    da$outputs_diagnose()
  )
  expect_equal(nrow(result), 1)
  expect_true(result$variables_in_data[1])
})

# ============================================================================
# Schema Accessor Methods Tests
# ============================================================================

test_that("set_analysis_schema and get_analysis_schema work", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "SASchemaDA")
    )
  )

  new_schema <- tibble::tibble(
    indicator_name = "test_ind",
    calculation = "mean",
    var_name = "id",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 1,
    indicator_unit = "%"
  )

  # set_analysis_schema() may message; suppress if it does
  suppressMessages(
    da$set_analysis_schema(new_schema)
  )

  result <- da$get_analysis_schema()
  expect_equal(nrow(result), 1)
  expect_equal(result$indicator_name, "test_ind")
})

test_that("set_outputs_schema and get_outputs_schema work", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "SAOSchemaDA")
    )
  )

  new_schema <- list(
    test_out = list(
      output_title = "test_out",
      output_name = "Test output",
      output_subtitle = NA_character_,
      variables = c("id"),
      disaggregation = NULL,
      output_func_name = "plot_stacked_bar",
      test_params = list(),
      output_type = "visualization",
      outputs_group = NULL,
      outputs_per_group = NULL
    )
  )

  # set_outputs_schema() may message; suppress if it does
  suppressMessages(
    da$set_outputs_schema(new_schema)
  )

  result <- da$get_outputs_schema()
  expect_equal(length(result), 1)
  expect_equal(result$test_out$output_title, "test_out")
})

test_that("get_quality_schema returns current quality schema", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "GQSchemaDA")
    )
  )

  result <- da$get_quality_schema()
  expect_true(is.list(result))
})

test_that("get_analysis_schema returns current analysis schema", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "GASchemaDA")
    )
  )

  result <- da$get_analysis_schema()
  # Can be NULL or a tibble/data.frame
  expect_true(is.null(result) || is.data.frame(result))
})

# ============================================================================
# WaterContainerDataAnalytics Subclass Tests
# ============================================================================

test_that("WaterContainerDataAnalytics initializes correctly", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      WaterContainerDataAnalytics$new(data = df, dataset_name = "WCTest")
    )
  )

  expect_s3_class(da, "WaterContainerDataAnalytics")
  expect_s3_class(da, "DataAnalytics")
  expect_equal(da$dataset_name, "WCTest")
})

# ============================================================================
# plausibility_results field Tests
# ============================================================================

test_that("DataAnalytics plausibility_results field is initialized as empty list", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "PRInit")
    )
  )

  expect_true(is.list(da$plausibility_results))
  expect_equal(length(da$plausibility_results), 0)
})

test_that("export_state_object uses plausibility_results key", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "PRExport")
    )
  )

  state <- suppressMessages(
    da$export_state_object()
  )

  expect_true("plausibility_results" %in% names(state))
  expect_false("results" %in% names(state))
})

test_that("load_state_object restores plausibility_results", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected, non-consequential initialization chatter (messages + warnings)
  suppressWarnings(
    da <- suppressMessages(
      DataAnalytics$new(data = df, dataset_name = "PRLoad")
    )
  )

  state <- suppressMessages(
    da$export_state_object()
  )

  da2 <- suppressWarnings(suppressMessages(
    DataAnalytics$new(data = df, dataset_name = "PRLoad2")
  ))

  suppressMessages(
    da2$load_state_object(state)
  )

  expect_equal(da2$plausibility_results, da$plausibility_results)
})

# ============================================================================
# NutritionDataAnalytics – quality_diagnose
# ============================================================================

test_that("NutritionDataAnalytics$quality_diagnose returns a tibble covering both schemas", {
  df <- tibble::tibble(
    id = 1:5,
    age_months = 6:10,
    weight_kg = c(7, 8, 9, 10, 11)
  )

  # Suppress expected, non-consequential initialization chatter (messages + warning)
  nut <- suppressWarnings(suppressMessages(
    NutritionDataAnalytics$new(data = df, dataset_name = "NutQDiag")
  ))

  nut$quality_schema_anthro <- list(
    check_anthro = list(
      check_name = "check_anthro",
      check_label = "Anthro check",
      statistical_test = "range_violation",
      variables = c("age_months"),
      thresholds = list(),
      test_params = list()
    )
  )
  nut$quality_schema_iycf <- list(
    check_iycf = list(
      check_name = "check_iycf",
      check_label = "IYCF check",
      statistical_test = "range_violation",
      variables = c("weight_kg"),
      thresholds = list(),
      test_params = list()
    )
  )

  # quality_diagnose() messages; suppress if it does
  suppressWarnings(
    result <- suppressMessages(
      nut$quality_diagnose()
    )
  )

  expect_true(tibble::is_tibble(result))
  expect_true(nrow(result) >= 2)
  expect_true("schema_type" %in% names(result))
  expect_true("anthropometric" %in% result$schema_type)
  expect_true("iycf" %in% result$schema_type)
  expect_true(tibble::is_tibble(nut$quality_issues_log))
})

test_that("NutritionDataAnalytics$quality_diagnose does not error when schemas are empty", {
  df <- tibble::tibble(id = 1:5)

  # Suppress expected initialization chatter (messages + warnings)
  nut <- suppressWarnings(suppressMessages(
    NutritionDataAnalytics$new(data = df, dataset_name = "NutQDiagEmpty")
  ))

  nut$quality_schema_anthro <- list()
  nut$quality_schema_iycf <- list()

  # Do NOT suppressWarnings() here, otherwise expect_warning can't observe them.
  expect_warning(
    expect_warning(
      result <- suppressMessages(
        nut$quality_diagnose()
      ),
      "anthro|anthropometric"
    ),
    "iycf"
  )

  expect_true(is.null(result) || tibble::is_tibble(result))
})

# ============================================================================
# NutritionDataAnalytics – post_run_analysis / MUAC age-weighted analysis
# ============================================================================

test_that("NutritionDataAnalytics initializes without weights_muac_alt field", {
  df <- tibble::tibble(
    id = 1:5,
    age_months = c(6, 12, 18, 30, 48),
    weight = rep(1, 5)
  )

  nut <- suppressWarnings(suppressMessages(
    NutritionDataAnalytics$new(
      data = df,
      dataset_name = "NoWeightFieldTest",
      variable_map = list(age_months = "age_months", weight = "weight")
    )
  ))

  # weights_muac_alt should NOT be a public field any longer
  expect_false("weights_muac_alt" %in% names(nut))
  # and the column should NOT be added to data during initialization
  expect_false("weights_muac_alt" %in% names(nut$data))
})

test_that("NutritionDataAnalytics$post_run_analysis no-ops when muac_age_weights = FALSE", {
  df <- tibble::tibble(
    id = 1:10,
    age_months = c(rep(12, 5), rep(36, 5)),
    nut_muac_cat = c(rep(0L, 7), rep(1L, 3)),
    weight = rep(1, 10)
  )

  nut <- suppressWarnings(suppressMessages(
    NutritionDataAnalytics$new(
      data = df,
      dataset_name = "PostAnalysisNoOpTest",
      variable_map = list(
        age_months = "age_months",
        weight = "weight",
        nut_muac_cat = "nut_muac_cat"
      )
    )
  ))

  nut$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "MUAC cat prop",
    calculation = "prop",
    var_name = "nut_muac_cat",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  suppressMessages(nut$run_analysis())

  # run_analysis calls post_run_analysis(muac_age_weights = FALSE) by default;
  # no 'muac_weighted' key should be present
  expect_null(nut$analysis_results[["muac_weighted"]])
})

test_that("NutritionDataAnalytics$post_run_analysis skips when no muac vars in plan", {
  df <- tibble::tibble(
    id = 1:10,
    age_months = c(rep(12, 5), rep(36, 5)),
    other_var = as.integer(c(rep(0L, 5), rep(1L, 5))),
    weight = rep(1, 10)
  )

  suppressWarnings(
    nut <- suppressMessages(
      NutritionDataAnalytics$new(
        data = df,
        dataset_name = "PostAnalysisNoMuacTest",
        variable_map = list(age_months = "age_months", weight = "weight")
      )
    )
  )

  nut$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "Other prop",
    calculation = "prop",
    var_name = "other_var",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  suppressMessages(
    nut$post_run_analysis(muac_age_weights = TRUE)
  )

  # No muac variable in plan → muac_weighted should not be populated
  expect_null(nut$analysis_results[["muac_weighted"]])
})

test_that("NutritionDataAnalytics$post_run_analysis stores muac_weighted results", {
  set.seed(1)
  n <- 90
  df <- tibble::tibble(
    id = seq_len(n),
    age_months = c(rep(12, 60), rep(36, 30)),
    nut_muac_cat = c(rep(0L, 60), rep(1L, 30)),
    other_var = as.integer(c(rep(0L, 45), rep(1L, 45))),
    weight = rep(1, n)
  )

  suppressWarnings(
    nut <- suppressMessages(
      NutritionDataAnalytics$new(
        data = df,
        dataset_name = "PostAnalysisMuacTest",
        variable_map = list(
          age_months = "age_months",
          weight = "weight",
          nut_muac_cat = "nut_muac_cat"
        )
      )
    )
  )

  nut$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = c("MUAC cat prop", "Other prop"),
    calculation = c("prop", "prop"),
    var_name = c("nut_muac_cat", "other_var"),
    denom_var = c(NA_character_, NA_character_),
    disaggregation = c(NA_character_, NA_character_),
    multiplier = c(100, 100),
    indicator_unit = c("%", "%")
  )

  # Run standard analysis first so analysis_results is populated
  suppressMessages(nut$run_analysis())

  # Now call post_run_analysis with muac_age_weights = TRUE
  suppressMessages(
    nut$post_run_analysis(muac_age_weights = TRUE)
  )

  # 'muac_weighted' key should be present and contain only the muac row
  expect_true(!is.null(nut$analysis_results[["muac_weighted"]]))
  res <- nut$analysis_results[["muac_weighted"]]
  expect_true(any(grepl("MUAC", res$indicator_name, ignore.case = TRUE)))
  # 'other_var' should NOT be in the muac_weighted results
  expect_false(any(grepl("Other", res$indicator_name, ignore.case = TRUE)))
})

test_that("NutritionDataAnalytics$post_run_analysis uses 0-23 vs 24-59 age ranges", {
  # age = 0  -> in 0-23 group
  # age = 23 -> in 0-23 group
  # age = 24 -> in 24-59 group
  # age = 59 -> in 24-59 group
  # age = 60 -> outside both groups (should get NA weight)
  df <- tibble::tibble(
    id = 1:6,
    age_months = c(0, 23, 24, 59, 60, NA_real_),
    nut_muac_cat = c(0L, 0L, 1L, 1L, 0L, 0L),
    weight = rep(1, 6)
  )

  suppressWarnings(
    nut <- suppressMessages(
      NutritionDataAnalytics$new(
        data = df,
        dataset_name = "AgeRangeBoundaryTest",
        variable_map = list(
          age_months = "age_months",
          weight = "weight",
          nut_muac_cat = "nut_muac_cat"
        )
      )
    )
  )

  nut$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "MUAC cat prop",
    calculation = "prop",
    var_name = "nut_muac_cat",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  # Verify the private helper returns correct weight values for boundary ages.
  # Valid rows: ages 0 & 23 → 0-23 group (n=2), ages 24 & 59 → 24-59 group (n=2)
  # sample_prop_0_23  = 0.5, expected_prop_0_23  = 1/3 => weight = (1/3) / 0.5 = 2/3
  # sample_prop_24_59 = 0.5, expected_prop_24_59 = 2/3 => weight = (2/3) / 0.5 = 4/3
  wts <- suppressMessages(
    nut$.__enclos_env__$private$.compute_weights_muac_alt(1 / 3)
  )

  expect_equal(length(wts), nrow(df))
  expect_true(all(abs(wts[df$age_months %in% c(0, 23)] - (2 / 3)) < 1e-10))
  expect_true(all(abs(wts[df$age_months %in% c(24, 59)] - (4 / 3)) < 1e-10))
  expect_true(all(is.na(wts[df$age_months == 60 & !is.na(df$age_months)])))
  expect_true(is.na(wts[is.na(df$age_months)]))

  # Also verify post_run_analysis stores results
  suppressMessages(nut$run_analysis())
  suppressWarnings(suppressMessages(
    nut$post_run_analysis(muac_age_weights = TRUE)
  ))

  expect_true(!is.null(nut$analysis_results[["muac_weighted"]]))
})

# ============================================================================
# add_all_to_dap
# ============================================================================

test_that("add_all_to_dap classifies columns and amends the plan", {
  df <- tibble::tibble(
    hh_uuid = sprintf("uuid-%030d", 1:30), # skipped (>20 unique char)
    stratum_col = rep(c("north", "south", "east"), 10),
    fcs_score = seq(1, 30), # already in map + dap
    wash_soap = rep(c(0, 1), 15), # prop
    edu_level = rep(c("primary", "secondary", "none"), 10), # cat
    water_src = rep(c("piped tap", "well,spring", "river"), 10) # select_multiple_cat
  )

  suppressWarnings(
    da <- suppressMessages(DataAnalytics$new(
      data = df,
      dataset_name = "AddAllDA",
      variable_map = list(stratum = "stratum_col", fcs = "fcs_score")
    ))
  )
  suppressMessages(da$add_indicator_dap(
    indicator_name = "FCS mean",
    calculation = "mean",
    var_name = "fcs_score",
    multiplier = 1,
    indicator_unit = "score"
  ))

  suppressWarnings(suppressMessages(da$add_all_to_dap()))
  plan <- da$data_analysis_plan$log_df

  # Skipped high-cardinality character column
  expect_false("hh_uuid" %in% plan$var_name)
  expect_false("hh_uuid" %in% unlist(da$variable_map))

  # Column already in map + dap is not duplicated
  expect_equal(sum(plan$var_name == "fcs_score"), 1)

  # In map but not dap: added using existing variable_map role as indicator name
  expect_true("stratum" %in% plan$indicator_name)
  expect_equal(plan$calculation[plan$var_name == "stratum_col"], "cat")

  # Novel columns added to variable_map and dap with guessed calculations
  expect_equal(da$variable_map[["wash_soap"]], "wash_soap")
  expect_equal(unique(plan$calculation[plan$var_name == "wash_soap"]), "prop")
  expect_equal(unique(plan$calculation[plan$var_name == "edu_level"]), "cat")
  expect_equal(
    unique(plan$calculation[plan$var_name == "water_src"]),
    "select_multiple_cat"
  )

  # prop/cat/select_multiple_cat rows use multiplier 100 and unit %
  new_rows <- plan[
    plan$var_name %in% c("wash_soap", "edu_level", "water_src"),
  ]
  expect_true(all(new_rows$multiplier == 100))
  expect_true(all(new_rows$indicator_unit == "%"))

  # Valid stratum in variable_map: new rows replicated with stratum disaggregation
  soap_rows <- plan[plan$var_name == "wash_soap", ]
  expect_equal(nrow(soap_rows), 2)
  expect_true("stratum_col" %in% soap_rows$disaggregation)
  # The stratum column itself is not disaggregated by itself
  expect_equal(nrow(plan[plan$var_name == "stratum_col", ]), 1)
})

test_that("add_all_to_dap keeps character columns with exactly 20 unique values", {
  df <- tibble::tibble(cat20 = paste0("opt", 1:20))
  da <- suppressWarnings(suppressMessages(DataAnalytics$new(
    data = df,
    dataset_name = "Cat20DA"
  )))

  suppressWarnings(suppressMessages(da$add_all_to_dap()))
  plan <- da$data_analysis_plan$log_df

  expect_equal(plan$calculation[plan$var_name == "cat20"], "cat")

  # One more unique value crosses the threshold and the column is skipped
  df21 <- tibble::tibble(cat21 = paste0("opt", 1:21))
  da21 <- suppressWarnings(suppressMessages(DataAnalytics$new(
    data = df21,
    dataset_name = "Cat21DA"
  )))

  suppressWarnings(suppressMessages(da21$add_all_to_dap()))
  expect_false("cat21" %in% da21$data_analysis_plan$log_df$var_name)
})

test_that("add_all_to_dap guesses mean for numeric columns beyond 0/1", {
  df <- tibble::tibble(score = c(0, 1, 2.5, 7))
  da <- suppressWarnings(suppressMessages(DataAnalytics$new(
    data = df,
    dataset_name = "MeanDA"
  )))

  suppressWarnings(suppressMessages(da$add_all_to_dap()))
  plan <- da$data_analysis_plan$log_df

  expect_equal(plan$calculation[plan$var_name == "score"], "mean")
  expect_equal(plan$multiplier[plan$var_name == "score"], 1)
  expect_equal(plan$indicator_unit[plan$var_name == "score"], "score")
})

test_that("add_all_to_dap iterates over multiple field sets from the pre-hook", {
  MultiDA <- R6::R6Class(
    "MultiDA",
    inherit = DataAnalytics,
    public = list(
      data2 = NULL,
      variable_map2 = NULL,
      data_analysis_plan2 = NULL,
      pre_add_all_to_dap = function() {
        list(
          main = list(
            data = "data",
            variable_map = "variable_map",
            data_analysis_plan = "data_analysis_plan"
          ),
          second = list(
            data = "data2",
            variable_map = "variable_map2",
            data_analysis_plan = "data_analysis_plan2"
          )
        )
      }
    )
  )

  suppressWarnings(
    da <- suppressMessages(MultiDA$new(
      data = tibble::tibble(a_bin = c(0, 1, 1, 0)),
      dataset_name = "MultiDA"
    ))
  )
  da$data2 <- tibble::tibble(b_txt = c("x", "y", "x", "y"))
  da$variable_map2 <- list()

  suppressWarnings(suppressMessages(da$add_all_to_dap()))

  expect_equal(da$data_analysis_plan$log_df$var_name, "a_bin")
  expect_equal(da$data_analysis_plan$log_df$calculation, "prop")

  expect_s3_class(da$data_analysis_plan2, "QuantDataAnalysisPlanLog")
  expect_equal(da$data_analysis_plan2$log_df$var_name, "b_txt")
  expect_equal(da$data_analysis_plan2$log_df$calculation, "cat")
  expect_equal(da$variable_map2[["b_txt"]], "b_txt")
})
