#!/usr/bin/env Rscript

#' Manual Test Script for Protocol and Tool Workflow

rm(list = ls())

# library(phr)
devtools::load_all()
library(tibble)
library(dplyr)
library(rsvg)

# SETUP: Dummy Assessment Information

assessment_title <- "Multi-Sector Humanitarian Needs Assessment – Dummy Country"
country_name <- "Dummy Country"
month_year <- "March 2025"

# Test 1: Create an IPHRAProtocol Object ####

protocol <- IPHRAProtocol$new(
  assessment_title = assessment_title,
  country_name = country_name,
  month_year = month_year
)

# Inspect initial state
protocol$metadata

# Validate the objective schema via the protocol method
# (protocol$validate_objective_schema(protocol$framework$master_schema))

# Test 2: Framework ####

protocol$access_nested(field = "framework", member = "master_objectives_schema")

protocol$access_nested(
  field = "framework",
  member = "render_framework_svg",
  version = "master"
)

protocol$access_nested(
  field = "framework",
  member = "set_primary_objectives",
  objective_codes = c(101, 105, 106, 109, 112)
)

# 108, 112, 113, 114, 115, 118, 147

protocol$access_nested(
  "framework",
  member = "set_secondary_objectives",
  objective_codes = c(105, 107, 112)
)

protocol$access_nested(
  "framework",
  member = "modify_adjusted_schema",
  objective_codes = c(101, 105, 106, 109, 112)
)

protocol$access_nested(
  "framework",
  member = "modify_indicator_bank",
  objective_codes = c(101, 105, 106, 109, 112)
)

(protocol$access_nested(
  field = "framework",
  member = "modified_indicator_bank"
))

protocol$access_nested("framework", member = "modify_adjusted_svg")

protocol$access_nested(
  field = "framework",
  member = "render_framework_svg",
  version = "adjusted"
)
(protocol$access_nested(
  field = "framework",
  member = "modified_objectives_schema"
))

# Test 3: Define Strata and Sample Sizes ####

protocol$access_nested(
  field = "sample_object",
  member = "get_sample_table"
)

protocol$access_nested(
  field = "sample_object",
  member = "add_stratum",
  stratum_id = "strata_A",
  stratum_name = "strata_A",
  sampling_method_site = "systematic"

)

protocol$access_nested(
  field = "sample_object",
  member = "add_stratum",
  stratum_id = "strata_A",
  stratum_name = "Urban North",
  pop_indicator = "Food Consumption Score",
  population_size = 45000,
  pop_design_effect = 1.5,
  pop_precision = 10,
  pop_expected_prevalence = 50,
  pop_nonresponse = 10,
  ind_indicator = "wasting_prevalence",
  ind_expected_prevalence = 15,
  ind_precision = 5,
  ind_nonresponse = 10,
  ind_design_effect = 1.5,
  ind_avg_hh_size = 5.2,
  ind_subpop_prop = 20,
  rate_indicator = "crude_death_rate",
  rate_expected_rate = 0.5,
  rate_precision = 0.5,
  rate_avg_hh_size = 5.2,
  rate_design_effect = 2,
  rate_fpc = FALSE,
  rate_nonresponse = 10,
  teams = 5,
  enumerators_per_team = 1,
  start_time = "10:00",
  end_time = "18:00",
  clusters_per_day = 2,
  avg_interview_time = 30,
  avg_rest_time = 30,
  avg_travel_time = 60,
  sampling_method_site = "systematic",
  sampling_method_hh = "systematic",
  n_sites = 10
)


protocol$access_nested(
  field = "sample_object",
  member = "add_stratum",
  stratum_id = "strata_B",
  stratum_name = "Peri-Urban East",
  sampling_method_site = "simple_random",
  population_size = 28000,
  pop_indicator = "Food Consumption Score",
  pop_design_effect = 1.8,
  pop_precision = 5
  # pop_expected_prevalence = 50,
  # pop_nonresponse = 10,
  # ind_indicator = "wasting_prevalence",
  # rate_indicator = "crude_death_rate",
  # rate_expected_rate = 0.2,
  # rate_precision = 0.5,
  # rate_avg_hh_size = 5.2,
  # rate_design_effect = 2,
  # rate_fpc = FALSE,
  # rate_nonresponse = 10,
  # sampling_method_site = "proportional",
  # sampling_method_hh = "systematic",
  # n_sites = 30
)

protocol$access_nested(
  field = "sample_object",
  member = "add_stratum",
  stratum_id = "strata_C",
  stratum_name = "Rural South",
  population_size = 17000,
  pop_indicator = "Food Consumption Score",
  pop_design_effect = 2.0,
  pop_precision = 7,
  pop_expected_prevalence = 50,
  pop_nonresponse = 10,
  ind_indicator = "wasting_prevalence",
  rate_indicator = "crude_death_rate",
  sampling_method_site = "simple_random",
  sampling_method_hh = "rlc",
  n_sites = 10
)

protocol$access_nested(
  field = "sample_object",
  member = "calculate_sample_sizes"
)
(protocol$access_nested(
  field = "sample_object",
  member = "get_sample_table"
))
# Test 4: Build and Validate a Sampling Frame ####

set.seed(42)

make_psu_frame <- function(stratum_id, n_psu, pop_range) {
  tibble::tibble(
    stratum = stratum_id,
    psu = paste0(stratum_id, "_v", seq_len(n_psu)),
    population_size = sample(pop_range[1]:pop_range[2], n_psu, replace = TRUE),
    inclusion = TRUE,
    sampled_psu = NA,
    allocated_sample = NA
  )
}

frame_A <- make_psu_frame("strata_A", n_psu = 60, pop_range = c(400, 1200))
frame_B <- make_psu_frame("strata_B", n_psu = 45, pop_range = c(250, 800))
frame_C <- make_psu_frame("strata_C", n_psu = 30, pop_range = c(100, 500))

sampling_frame <- dplyr::bind_rows(frame_A, frame_B, frame_C)

protocol$sampling_frame$set("log_df", sampling_frame)

protocol$sampling_frame$get("log_df")

protocol$sampling_frame$draw_sample(strata_table = protocol$sample_object$sample_table)

protocol$sampling_frame$validate()
protocol$sampling_frame$validated

# Test 5: Draw Sample ####

protocol$access_nested(
  field = "sampling_frame",
  member = "draw_sample",
  strata_table = protocol$get_sample_table(),
  seed = 788
)

(protocol$access_nested(
  field = "sampling_frame",
  member = "drawn_sample"
))

(protocol$access_nested(
  field = "sampling_frame",
  member = "drawn_sample_full"
))

# protocol$sampling_frame$draw_sample(
#   strata_table = protocol$get_sample_table(),
#   seed = 788
# )

(protocol$drawn_sample_full)

# Test 6: Testing Tools ####

print(protocol$get_allowable_tools())

# Household Tool ####
protocol$add_tools(tool_name = "tool_household_iphra_v2")

protocol$remove_tools(tool_name = "tool_household_iphra_v2")

protocol$add_tools(tool_name = "tool_household_iphra_v2")

# protocol$tools$tool_household_iphra_v2$change_default_language(
#   language = "Arabic"
# )

protocol$access_nested(
  field = "tools",
  name = "tool_household_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = unique(as.character(as.integer(
    protocol$access_nested(
      field = "framework",
      member = "modified_indicator_bank"
    )$indicator_code
  )))
)


# protocol$tools$tool_household_iphra_v2$validate() # this part still needs heavy development
# protocol$tools$tool_household_iphra_v2$get_validation_errors() # this part still needs heavy development

# Community KII Tool ####
protocol$add_tools("tool_kii_community_iphra_v2")

protocol$access_nested(
  field = "tools",
  name = "tool_kii_community_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = unique(as.character(
    protocol$access_nested(
      field = "framework",
      member = "modified_indicator_bank"
    )$indicator_code
  ))
)

(protocol$access_nested(
  field = "tools",
  name = "tool_kii_community_iphra_v2",
  member = "revised_survey",
))

# protocol$tools$tool_kii_community_iphra_v2$validate() # this part still needs heavy development
# protocol$tools$tool_kii_community_iphra_v2$get_validation_errors() # this part still needs heavy development

# Community Obseration Tool ####

protocol$add_tools(tool_name = "tool_obs_community_iphra_v2")

protocol$access_nested(
  field = "tools",
  name = "tool_kii_community_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = unique(as.character(
    protocol$access_nested(
      field = "framework",
      member = "modified_indicator_bank"
    )$indicator_code
  ))
)

# protocol$tools$tool_obs_community_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
# protocol$tools$tool_obs_community_iphra_v2$get_validation_errors() # this part still needs heavy development

# FSL Service Provider KII Tool ####

protocol$add_tools(tool_name = "tool_kii_fsl_service_provider_iphra_v2")


protocol$access_nested(
  field = "tools",
  name = "tool_kii_fsl_service_provider_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = unique(as.character(
    protocol$access_nested(
      field = "framework",
      member = "modified_indicator_bank"
    )$indicator_code
  ))
)

# WASH Service Provider KII Tool ####

protocol$add_tools(tool_name = "tool_kii_wash_service_provider_iphra_v2")

# Market Vendor KII Tool ####

protocol$add_tools(tool_name = "tool_kii_markets_iphra_v2")

# Nutrition Facility KII Tool ####

protocol$add_tools(tool_name = "tool_kii_nutrition_service_provider_iphra_v2")

# Protocol Coherence Checks ####

(protocol$diagnose_coherence())
(protocol$issues_coherence)


# Test 3: Generate Word Report from IPHRAProtocol ####

# generate_doc() uses officer + flextable (bundled template used when present).
# The standalone wrapper generate_protocol_report() dispatches to the method.

# -- 3a: Generate report with no tools (tools section shows placeholder text) --

protocol$access_nested(field = "metadata", role = "research_cycle_id")

protocol$set(field = "metadata", role = "research_cycle_id", value = "RC-2025-001")

protocol$metadata$research_cycle_id <- "RC-2025-001"
protocol$metadata$country <- "Switzerland"
protocol$metadata$release_date <- Sys.Date()
protocol$metadata$version_number <- "1.1"
protocol$metadata$type_emergency <- "Protracted"
protocol$metadata$type_crisis <- "Conflict"
protocol$metadata$population <- "Internally Displaced Persons"
protocol$metadata$rationale <- "Recent population movements from conflict area, populations not served."
protocol$metadata$date_pilot_training <- "2025-04-15"
protocol$metadata$date_data_collection_start <- "2025-05-01"
protocol$metadata$date_data_collection_end <- "2025-05-15"
protocol$metadata$date_data_analysis <- "2025-05-20"
protocol$metadata$date_data_validation <- "2025-05-18"
protocol$metadata$date_preliminary_presentation <- "2025-05-25"
protocol$metadata$date_outputs_validation <- "2025-05-30"
protocol$metadata$date_outputs_publication <- "2025-06-05"
protocol$metadata$date_final_presentation <- "2025-06-10"
protocol$metadata$audience_type_cluster <- "Life-Saving Clusters"
protocol$metadata$expected_output_cluster <- "Preliminary Presentation, Technical Report"
protocol$metadata$expected_output_donor <- "Brief"
protocol$metadata$expected_output_operational_actor <- "Technical Report, Factsheet"
protocol$metadata$expected_output_other <- "Not applicable"
protocol$metadata$dissemination_strategy_cluster <- "In-Person, Email"
protocol$metadata$dissemination_strategy_donor <- "Email"
protocol$metadata$dissemination_strategy_operational_actor <- "Remote, Email"
protocol$metadata$dissemination_strategy_other <- "Not applicable"
protocol$metadata$access_cluster <- "Public"
protocol$metadata$access_donor <- "Bilateral, Restricted"
protocol$metadata$access_operational_actor <- "Restricted"
protocol$metadata$access_other <- "Not applicable"
protocol$metadata$visibility_cluster <- "Public"
protocol$metadata$visibility_donor <- "Restricted"
protocol$metadata$visibility_operational_actor <- "Restricted"
protocol$metadata$visibility_other <- "Not applicable"

protocol$metadata$month_year <- "June 2025"
protocol$metadata$country_name <- "Switzerland"
protocol$metadata$assessment_title <- "Integrated Public Health Rapid Assessment - Switzerland"
protocol$metadata$protocol_version <- "1.0"
protocol$metadata$version <- 1L
# Text metadata fields
protocol$metadata$mandating_body <- "IMPACT Initiatives"
protocol$metadata$project_code <- "98BSY"

# Secondary data sources
protocol$access_nested(
  field = "framework",
  member = "add_secondary_data_source",
  objective = 105,
  source = "UNHCR Population Statistics",
  purpose = "To provide context on population movements and displacement trends."
)

protocol$access_nested(
  field = "framework",
  member = "add_secondary_data_source",
  objective = "To understand nutritional status of population",
  source = "SMART Survey",
  purpose = "Ascertain severity of nutrition in population"
)

# Generate ToR ####

protocol$generate_quarto_doc(
  output_file = "test_manual_protocol_workflow_tor.docx"
)
