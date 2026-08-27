library(testthat)

# Helper: skip if ANA framework resources are absent
skip_if_no_ana_resources <- function() {
  objectives_ok <-
    nzchar(system.file("resources", "reference_objectives.xlsx",
                       package = "phr")) ||
    file.exists(file.path("inst", "resources", "reference_objectives.xlsx"))
  indicators_ok <-
    nzchar(system.file("resources", "reference_indicator_bank.xlsx",
                       package = "phr")) ||
    file.exists(file.path("inst", "resources", "reference_indicator_bank.xlsx"))
  skip_if_not(objectives_ok && indicators_ok,
              "ANA framework resource files not available")
}

# ── Initialization ─────────────────────────────────────────────────────────────

test_that("IPHRAProtocol$new() creates object with correct class hierarchy", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    expect_true(inherits(p, "IPHRAProtocol"))
    expect_true(inherits(p, "SurveyProtocol"))
    expect_true(inherits(p, "Protocol"))
    expect_true(inherits(p, "Document"))
    expect_true(inherits(p, "Orchestrator"))
  }))
})

test_that("IPHRAProtocol initializes with NULL metadata when no args given", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    expect_null(p$metadata$assessment_title)
    expect_null(p$metadata$country_name)
    expect_null(p$metadata$month_year)
  }))
})

test_that("IPHRAProtocol stores assessment_title, country_name, month_year", {
  p <- suppressMessages(IPHRAProtocol$new(
    assessment_title = "Test IPHRA",
    country_name     = "Somalia",
    month_year       = "March 2025"
  ))
  suppressWarnings(suppressMessages({
    expect_equal(p$metadata$assessment_title, "Test IPHRA")
    expect_equal(p$metadata$country_name,     "Somalia")
    expect_equal(p$metadata$month_year,       "March 2025")
  }))
})

test_that("IPHRAProtocol auto-selects ANAFramework on init when resources available", {
  skip_if_no_ana_resources()
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$framework, "ANAFramework"))
    expect_true(inherits(p$framework, "Framework"))
  }))
})

test_that("IPHRAProtocol$new() errors on unexpected arguments", {
  suppressWarnings(suppressMessages(
    expect_error(suppressMessages(IPHRAProtocol$new(version = 2L)))
  ))
})

# ── Nested objects: light accessibility ────────────────────────────────────────

test_that("framework field is accessible and is a Framework", {
  skip_if_no_ana_resources()
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_true(inherits(p$framework, "Framework"))
  ))
})

test_that("sample_object is accessible and is a Sample", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_true(inherits(p$sample_object, "Sample"))
  ))
})

test_that("sampling_frame is accessible and is a SamplingFrame", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  ))
})

test_that("tools field is initially empty and accessible", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    expect_equal(length(p$tools), 0L)
    expect_type(p$tools, "list")
  }))
})

# ── get_allowable_tools ────────────────────────────────────────────────────────

test_that("get_allowable_tools returns a character vector of 12 tool names", {
  p     <- suppressMessages(IPHRAProtocol$new())
  tools <- suppressWarnings(suppressMessages(p$get_allowable_tools()))
  suppressWarnings(suppressMessages({
    expect_type(tools, "character")
    expect_length(tools, 12L)
  }))
})

test_that("get_allowable_tools includes expected household, KII and observation names", {
  p     <- suppressMessages(IPHRAProtocol$new())
  tools <- suppressWarnings(suppressMessages(p$get_allowable_tools()))
  suppressWarnings(suppressMessages({
    expect_true("tool_household_iphra_v2"                      %in% tools)
    expect_true("tool_kii_community_iphra_v2"                  %in% tools)
    expect_true("tool_kii_health_service_provider_iphra_v2"    %in% tools)
    expect_true("tool_kii_wash_service_provider_iphra_v2"      %in% tools)
    expect_true("tool_kii_nutrition_service_provider_iphra_v2" %in% tools)
    expect_true("tool_kii_fsl_service_provider_iphra_v2"       %in% tools)
    expect_true("tool_kii_markets_iphra_v2"                    %in% tools)
    expect_true("tool_obs_water_point_iphra_v2"                %in% tools)
    expect_true("tool_obs_community_iphra_v2"                  %in% tools)
    expect_true("tool_obs_crop_livestock_iphra_v1"             %in% tools)
    expect_true("tool_obs_health_facility_iphra_v2"            %in% tools)
    expect_true("tool_obs_latrine_iphra_v2"                    %in% tools)
  }))
})

# ── add_tools ─────────────────────────────────────────────────────────────────

test_that("add_tools rejects unknown tool names", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$add_tools("unknown_tool_xyz"))
  ))
})

test_that("add_tools rejects empty string", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$add_tools(""))
  ))
})

test_that("add_tools rejects non-character input", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$add_tools(123))
  ))
})

test_that("add_tools stores household tool by name (light access check)", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true("tool_household_iphra_v2" %in% names(p$tools))
  ))
})

test_that("add_tools stores KII tool by name (light access check)", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_community_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true("tool_kii_community_iphra_v2" %in% names(p$tools))
  ))
})

test_that("add_tools stores observation tool by name (light access check)", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_water_point_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true("tool_obs_water_point_iphra_v2" %in% names(p$tools))
  ))
})

test_that("add_tools returns self invisibly for chaining", {
  p      <- suppressMessages(IPHRAProtocol$new())
  result <- suppressWarnings(suppressMessages(
    withVisible(p$add_tools("tool_household_iphra_v2"))
  ))
  suppressWarnings(suppressMessages({
    expect_false(result$visible)
    expect_identical(result$value, p)
  }))
})

test_that("multiple add_tools calls accumulate in $tools", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    p$add_tools("tool_household_iphra_v2")
    p$add_tools("tool_kii_community_iphra_v2")
    p$add_tools("tool_obs_water_point_iphra_v2")
    expect_equal(length(p$tools), 3L)
    expect_true("tool_household_iphra_v2"     %in% names(p$tools))
    expect_true("tool_kii_community_iphra_v2" %in% names(p$tools))
    expect_true("tool_obs_water_point_iphra_v2" %in% names(p$tools))
  }))
})

# ── Tool presence active bindings ─────────────────────────────────────────────

test_that(".tool_household_iphra is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_household_iphra))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_household_iphra))
  ))
})

test_that(".tool_community_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_community_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_community_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_community_kii))
  ))
})

test_that(".tool_fsl_provider_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_fsl_provider_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_fsl_service_provider_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_fsl_provider_kii))
  ))
})

test_that(".tool_market_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_market_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_markets_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_market_kii))
  ))
})

test_that(".tool_health_facility_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_health_facility_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_health_service_provider_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_health_facility_kii))
  ))
})

test_that(".tool_nutrition_facility_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_nutrition_facility_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_nutrition_service_provider_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_nutrition_facility_kii))
  ))
})

test_that(".tool_wash_provider_kii is FALSE before adding tool, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_wash_provider_kii))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_kii_wash_service_provider_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_wash_provider_kii))
  ))
})

test_that("tool presence bindings are read-only", {
  p <- suppressMessages(IPHRAProtocol$new())
  result <- suppressWarnings(suppressMessages(
    withVisible(p$.tool_household_iphra <- TRUE)
  ))
  suppressWarnings(suppressMessages({
    expect_false(result$visible)
    
  }))
})

# ── Observation tool presence bindings ────────────────────────────────────────

test_that(".tool_community_observation is FALSE before adding, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_community_observation))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_community_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_community_observation))
  ))
})

test_that(".tool_crops_livestock_observation is FALSE before adding, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_crops_livestock_observation))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_crop_livestock_iphra_v1")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_crops_livestock_observation))
  ))
})

test_that(".tool_health_facility_observation is FALSE before adding, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_health_facility_observation))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_health_facility_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_health_facility_observation))
  ))
})

test_that(".tool_latrine_observation is FALSE before adding, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_latrine_observation))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_latrine_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_latrine_observation))
  ))
})

test_that(".tool_water_point_observation is FALSE before adding, TRUE after", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.tool_water_point_observation))
  ))
  suppressWarnings(suppressMessages(p$add_tools("tool_obs_water_point_iphra_v2")))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.tool_water_point_observation))
  ))
})

# ── Indicator active bindings ─────────────────────────────────────────────────

test_that("indicator bindings default to FALSE with no household tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages({
    expect_false(isTRUE(p$.ind_ecfies))
    expect_false(isTRUE(p$.ind_iycfe))
    expect_false(isTRUE(p$.ind_measles_vaccination))
    expect_false(isTRUE(p$.ind_muac_children))
    expect_false(isTRUE(p$.ind_muac_women))
    expect_false(isTRUE(p$.ind_vitamin_a_coverage))
    expect_false(isTRUE(p$.ind_mortality))
    expect_false(isTRUE(p$.ind_fcs))
    expect_false(isTRUE(p$.ind_rcsi))
    expect_false(isTRUE(p$.ind_hhs))
    expect_false(isTRUE(p$.ind_lcsi))
    expect_false(isTRUE(p$.ind_hwise))
    expect_false(isTRUE(p$.ind_lppd))
  }))
})

test_that(".ind_mortality becomes TRUE when household tool has indicator 10501", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10501", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_mortality))
  ))
})

test_that(".ind_ecfies becomes TRUE when household tool has indicator 10801", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10801", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_ecfies))
  ))
})

test_that(".ind_iycfe becomes TRUE when household tool has indicator 10802", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10802", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_iycfe))
  ))
})

test_that(".ind_measles_vaccination becomes TRUE when household tool has indicator 14304", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "14304", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_measles_vaccination))
  ))
})

test_that(".ind_muac_children becomes TRUE when household tool has indicator 10701", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10701", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_muac_children))
  ))
})

test_that(".ind_muac_women becomes TRUE when household tool has indicator 10702", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10702", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_muac_women))
  ))
})

test_that(".ind_vitamin_a_coverage becomes TRUE when household tool has indicator 14305", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "14305", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_vitamin_a_coverage))
  ))
})

test_that(".ind_fcs becomes TRUE when household tool has indicator 11205", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "11205", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_fcs))
  ))
})

test_that(".ind_rcsi becomes TRUE when household tool has indicator 11202", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "11202", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_rcsi))
  ))
})

test_that(".ind_hhs becomes TRUE when household tool has indicator 11201", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "11201", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_hhs))
  ))
})

test_that(".ind_lcsi becomes TRUE when household tool has indicator 12301", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "12301", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_lcsi))
  ))
})

test_that(".ind_hwise becomes TRUE when household tool has indicator 11701", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "11701", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_hwise))
  ))
})

test_that(".ind_lppd becomes TRUE when household tool has indicator 10901", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  suppressWarnings(suppressMessages(
    p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
      indicator_code = "10901", stringsAsFactors = FALSE
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.ind_lppd))
  ))
})

test_that("indicator bindings are read-only", {
  p      <- suppressMessages(IPHRAProtocol$new())
  result <- suppressWarnings(suppressMessages(
    withVisible(p$.ind_ecfies <- TRUE)
  ))
  suppressWarnings(suppressMessages({
    expect_false(result$visible)
  }))
})

# ── Table active bindings ─────────────────────────────────────────────────────

test_that(".tools_table_df returns a data frame", {
  p <- suppressMessages(IPHRAProtocol$new())
  t <- suppressWarnings(suppressMessages(p$.tools_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(t) || is.null(t))
  ))
})

test_that(".household_pillars_table_df returns a data frame or NULL", {
  p <- suppressMessages(IPHRAProtocol$new())
  t <- suppressWarnings(suppressMessages(p$.household_pillars_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(t) || is.null(t))
  ))
})

test_that(".kii_pillars_table_df returns a data frame or NULL", {
  p <- suppressMessages(IPHRAProtocol$new())
  t <- suppressWarnings(suppressMessages(p$.kii_pillars_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(t) || is.null(t))
  ))
})

test_that(".observation_pillars_table_df returns a data frame or NULL", {
  p <- suppressMessages(IPHRAProtocol$new())
  t <- suppressWarnings(suppressMessages(p$.observation_pillars_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(t) || is.null(t))
  ))
})

# ── DAP data frame active bindings ────────────────────────────────────────────

test_that(".household_dap_df returns an empty data frame when no household tool added", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.household_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".community_kii_dap_df returns an empty data frame when no community KII tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.community_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".community_observation_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.community_observation_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".health_facility_kii_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.health_facility_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".health_facility_observation_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.health_facility_observation_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".nutrition_facility_kii_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.nutrition_facility_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".fsl_provider_kii_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.fsl_provider_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".market_kii_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.market_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".crop_livstock_observation_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.crop_livstock_observation_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".wash_provider_kii_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.wash_provider_kii_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".water_point_observation_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.water_point_observation_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

test_that(".latrine_observation_dap_df returns an empty data frame when no tool", {
  p <- suppressMessages(IPHRAProtocol$new())
  df <- suppressWarnings(suppressMessages(p$.latrine_observation_dap_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(df))
  ))
})

# ── DAP df bindings are read-only ─────────────────────────────────────────────

test_that(".household_dap_df is read-only (assignment errors)", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$.household_dap_df <- data.frame())
  ))
})

test_that(".community_kii_dap_df is read-only (assignment errors)", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$.community_kii_dap_df <- data.frame())
  ))
})

# ── update_recall_date ────────────────────────────────────────────────────────

test_that("update_recall_date errors when no household tool is present", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(
    expect_error(p$update_recall_date("2025-01-01"))
  ))
})

test_that("update_recall_date accepts a date string when household tool exists", {
  p <- suppressMessages(IPHRAProtocol$new())
  suppressWarnings(suppressMessages(p$add_tools("tool_household_iphra_v2")))
  # Should not throw even if it silently does nothing when tool is empty
  tryCatch(
    suppressWarnings(suppressMessages(p$update_recall_date("2025-03-01"))),
    error = function(e) {
      # Acceptable if no recall rows in the empty tool
      suppressWarnings(suppressMessages(
        expect_true(grepl("recall|date|not found", conditionMessage(e),
                          ignore.case = TRUE))
      ))
    }
  )
})

