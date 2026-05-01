#!/usr/bin/env Rscript

#' Manual Test Script for Protocol and Tool Workflow
#'
#' This script demonstrates the complete workflow for the Protocol class
#' hierarchy (IPHRAProtocol and SurveyProtocol subclass) together with
#' the Tool class hierarchy (Tool, HouseholdTool, KeyInformantTool, ObservationTool)
#' and the Framework class hierarchy (Framework base class and ANAFramework subclass).
#'
#' Steps covered:
#'  1.  Create an IPHRAProtocol object (no sampling)
#'  2.  Define objectives and manage them via the built-in ANAFramework
#'  3.  Generate a Word report from the IPHRAProtocol (no tools yet, then with tools)
#'  4.  Create a SurveyProtocol object (with sampling support)
#'  5.  Define strata and sample sizes on the SurveyProtocol
#'  6.  Build and validate a sampling frame
#'  7.  Draw samples from the sampling frame
#'  8.  Instantiate and inspect Tool subclasses (Household, KII, Observation)
#'  9.  Modify tools: filter by modules, update choice lists, change language
#' 10.  Attach tools to the SurveyProtocol
#' 11.  Finalise and inspect the SurveyProtocol (export, save/load)
#' 12.  Generate a Word report from the SurveyProtocol
#' 13.  Framework and ANAFramework workflow (create, preset selection, SVG
#'      highlighting, Protocol integration, export/restore round-trip)
#'
#' Author: Auto-generated for iphRa protocol / tool manual testing
#' Date: 2025-01-01 (revised 2025-04-29)

rm(list = ls())

# library(phr)
devtools::load_all()
library(tibble)
library(dplyr)
library(rsvg)


# =============================================================================
# SETUP: Dummy Assessment Information
# =============================================================================

assessment_title <- "Multi-Sector Humanitarian Needs Assessment – Dummy Country"
country_name     <- "Dummy Country"
month_year       <- "March 2025"


# =============================================================================
# Test 1: Create an IPHRAProtocol Object ####
# =============================================================================
# IPHRAProtocol$new() automatically creates an ANAFramework and restricts
# add_tools() to the approved IPHRA tool list.

protocol <- IPHRAProtocol$new(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)

# Inspect initial state
protocol$metadata
protocol$get_protocol_summary()

# The ANAFramework is already attached – inspect it directly
head(protocol$framework$master_schema)

View(protocol$framework$filter_schema_by_pillar(pillars = c("Demographics")))

nrow(protocol$framework$master_schema)
names(protocol$framework$master_schema)
head(protocol$framework$master_schema)

# Validate the objective schema via the protocol method
protocol$validate_objective_schema(protocol$framework$master_schema)


# =============================================================================
# Test 2: Define Objectives via Framework ####
# =============================================================================
# Objectives are now managed through the Framework's adjusted_schema.
# Use Framework$add_objective_row() / remove_objective_row() / update_adjusted_schema().

# -- 2a: Select objectives from the ANAFramework using a preset --
fsl_preset <- protocol$framework$get_preset_objectives("fsl")
cat("FSL preset objectives:", length(fsl_preset), "\n")

# Set adjusted_schema to the FSL preset selection
protocol$framework$update_adjusted_schema(fsl_preset)
stopifnot(is.data.frame(protocol$framework$adjusted_schema))
stopifnot(nrow(protocol$framework$adjusted_schema) > 0)
cat("Adjusted schema rows after FSL preset:", nrow(protocol$framework$adjusted_schema), "\n")

# -- 2b: Add an objective row directly to the adjusted schema --
protocol$framework$add_objective_row(list(
  sector          = "General",
  pillar          = "Context",
  sub_pillar      = "Background",
  short_objective = "GEN-01",
  text_objective  = "Review secondary data on humanitarian conditions in the study area"
))
cat("Adjusted schema rows after add_objective_row:", nrow(protocol$framework$adjusted_schema), "\n")

# -- 2c: Remove an objective row from the adjusted schema --
protocol$framework$remove_objective_row("GEN-01")
cat("Adjusted schema rows after remove_objective_row:", nrow(protocol$framework$adjusted_schema), "\n")


# =============================================================================
# Test 3: Generate Word Report from IPHRAProtocol ####
# =============================================================================
# generate_reach_tor() uses officer + flextable (bundled template used when present).
# The standalone wrapper generate_protocol_report() dispatches to the method.

# -- 3a: Generate report with no tools (tools section shows placeholder text) --

report_no_tools <- tempfile(fileext = ".docx")
protocol$generate_reach_tor(output_file = report_no_tools)
stopifnot(file.exists(report_no_tools))
cat("IPHRAProtocol report (no tools) written to:", report_no_tools, "\n")

# -- 3b: Add IPHRA tools to the protocol and verify they appear in the report --
# IPHRAProtocol$add_tools() takes an approved IPHRA tool name only.

protocol$add_tools("tool_household_iphra_v2")
protocol$add_tools("tool_kii_community_iphra_v2")

# Tools are stored by name in the named list
stopifnot("tool_household_iphra_v2" %in% names(protocol$tools))
stopifnot("tool_kii_community_iphra_v2" %in% names(protocol$tools))
stopifnot(inherits(protocol$tools[["tool_household_iphra_v2"]], "HouseholdTool"))
stopifnot(inherits(protocol$tools[["tool_kii_community_iphra_v2"]], "KeyInformantTool"))

cat("Allowable IPHRA tools:\n")
print(protocol$get_allowable_tools())

report_with_tools <- tempfile(fileext = ".docx")
generate_protocol_report(protocol, output_file = report_with_tools)
stopifnot(file.exists(report_with_tools))
cat("IPHRAProtocol report (with tools) written to:", report_with_tools, "\n")

# Confirm summary reflects tools
base_summary <- protocol$get_protocol_summary()
base_summary$num_tools    # 2
base_summary$num_strata   # NULL — not a SurveyProtocol
stopifnot(is.null(base_summary$num_strata))

# Remove the dummy tools so we start fresh on the SurveyProtocol below
protocol$tools <- list()


# =============================================================================
# Test 4: Create a SurveyProtocol Object ####
# =============================================================================
# SurveyProtocol inherits all Protocol capabilities and adds sampling support.

survey_protocol <- create_survey_protocol(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)
stopifnot(inherits(survey_protocol, "SurveyProtocol"))
stopifnot(inherits(survey_protocol, "Protocol"))

# Attach the same ANAFramework (with adjusted objectives) to the survey protocol
survey_protocol$framework <- protocol$framework

# Add target strata to metadata
survey_protocol$add_target_stratum("strata_A", "Urban North")
survey_protocol$add_target_stratum("strata_B", "Peri-Urban East")
survey_protocol$add_target_stratum("strata_C", "Rural South")

survey_protocol$metadata$target_strata


# =============================================================================
# Test 5: Define Strata and Sample Sizes ####
# =============================================================================

# -- 5a: Add strata to the master sample table --
# ind_indicator and mort_indicator are required columns

survey_protocol$add_stratum(
  stratum_id              = "strata_A",
  stratum_name            = "Urban North",
  population_size         = 45000,
  pop_design_effect       = 1.5,
  pop_precision           = 5,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  ind_expected_prevalence = 15,
  ind_precision           = 5,
  ind_nonresponse         = 10,
  ind_design_effect       = 1.5,
  ind_avg_hh_size         = 5.2,
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

survey_protocol$add_stratum(
  stratum_id              = "strata_B",
  stratum_name            = "Peri-Urban East",
  population_size         = 28000,
  pop_design_effect       = 1.8,
  pop_precision           = 5,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

survey_protocol$add_stratum(
  stratum_id              = "strata_C",
  stratum_name            = "Rural South",
  population_size         = 17000,
  pop_design_effect       = 2.0,
  pop_precision           = 7,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

survey_protocol$calculate_sample_sizes()

survey_protocol$sample_table

# Inspect master sample table before sample sizes are calculated
survey_protocol$get_sample_table()
names(survey_protocol$get_sample_table())

# Validate the master strata table structure
strata_validation <- survey_protocol$validate_strata_table()
strata_validation$valid
strata_validation$message

# Also validate via standalone function
validate_strata_table(survey_protocol$sample_table)

# -- 5b: Calculate sample sizes via calculate_sample_sizes() method --
# This replaces the old manual approach of calling calculate_sample_size_general() per stratum
# and manually writing results back.

survey_protocol$calculate_sample_sizes()
survey_protocol$get_sample_table()

# Inspect the new result columns
st <- survey_protocol$sample_table
cat("Column names:\n"); print(names(st))
cat("General_HH_Sample_Size:", st$General_HH_Sample_Size, "\n")
cat("Ind_HH_Sample_Size:",     st$Ind_HH_Sample_Size, "\n")
cat("Final_HH_Sample_Size:",   st$Final_HH_Sample_Size, "\n")
cat("Total planned sample size:", sum(st$General_HH_Sample_Size, na.rm = TRUE), "\n")

# -- 5c: Individual-level calculation for nutrition screening (standalone) --

ss_nut <- calculate_sample_size_individual(
  expected_proportion    = 15,
  desired_precision      = 5,
  non_response_rate      = 10,
  design                 = "cluster",
  design_effect          = 1.5,
  average_household_size = 5.2,
  sub_population_percent = 20,
  confidence_level       = 0.95
)
cat("Nutrition screening – individuals needed:", ss_nut$sample_size_individuals, "\n")
cat("Nutrition screening – households needed: ", ss_nut$sample_size_households,  "\n")

# Summary includes sampling fields for SurveyProtocol
sp_summary <- survey_protocol$get_protocol_summary()
stopifnot("num_strata" %in% names(sp_summary))
stopifnot(sp_summary$num_strata == 3L)


# =============================================================================
# Test 6: Build and Validate a Sampling Frame ####
# =============================================================================

# -- 6a: Generate a dummy sampling frame (PSUs / enumeration areas) --
# Standard column order: stratum, psu, population_size, inclusion
# (village_name removed — represented by psu)

set.seed(42)

make_psu_frame <- function(stratum_id, n_psu, pop_range) {
  tibble::tibble(
    stratum         = stratum_id,
    psu             = paste0(stratum_id, "_v", seq_len(n_psu)),
    population_size = sample(pop_range[1]:pop_range[2], n_psu, replace = TRUE),
    inclusion       = TRUE
  )
}

frame_A <- make_psu_frame("strata_A", n_psu = 60, pop_range = c(400, 1200))
frame_B <- make_psu_frame("strata_B", n_psu = 45, pop_range = c(250, 800))
frame_C <- make_psu_frame("strata_C", n_psu = 30, pop_range = c(100, 500))

sampling_frame <- dplyr::bind_rows(frame_A, frame_B, frame_C)

cat("Sampling frame: ", nrow(sampling_frame), "PSUs across",
    length(unique(sampling_frame$stratum)), "strata\n")
head(sampling_frame)

# -- 6b: Validate the sampling frame using utility function --

frame_validation <- validate_sampling_frame(sampling_frame)
frame_validation$valid
frame_validation$message
frame_validation$summary

# -- 6c: Set sampling frame on the SurveyProtocol --

survey_protocol$set_sampling_frame(sampling_frame)

# Check protocol issues after setting frame (strata alignment)
survey_protocol$get_issues()


# =============================================================================
# Test 7: Draw Sample ####
# =============================================================================

# draw_sample() reads Sampling_Method, Final_HH_Sample_Size, n_psu,
# n_clusters, cluster_size, and n_sites from the strata table.

# -- 7a: Final_HH_Sample_Size is already set by calculate_sample_sizes() above.
# Verify it was computed correctly:

cat("Final_HH_Sample_Size values:", survey_protocol$sample_table$Final_HH_Sample_Size, "\n")
stopifnot(!all(is.na(survey_protocol$sample_table$Final_HH_Sample_Size)))

# -- 7b: Draw using proportional method (reads from Sampling_Method column) --

survey_protocol$draw_sample(seed = 789)
nrow(survey_protocol$drawn_sample)       # selected PSUs
nrow(survey_protocol$drawn_sample_full)  # full frame with sampled_psu column
head(survey_protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 7c: Override to pps_cluster --

survey_protocol$sample_table$Sampling_Method <- "pps_cluster"
survey_protocol$sample_table$n_clusters      <- 30
survey_protocol$sample_table$cluster_size    <- 20

survey_protocol$draw_sample(seed = 456)
nrow(survey_protocol$drawn_sample)
head(survey_protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 7d: Purposive sampling — no PSUs auto-selected --

survey_protocol$sample_table$Sampling_Method <- "purposive"
survey_protocol$draw_sample(seed = 42)
nrow(survey_protocol$drawn_sample)                              # 0
sum(is.na(survey_protocol$drawn_sample_full$sampled_psu))       # all NA

# Restore proportional for remaining tests
survey_protocol$sample_table$Sampling_Method <- "proportional"
survey_protocol$draw_sample(seed = 789)

# -- 7e: Pass an explicit frame and strata_table --

custom_frame  <- sampling_frame[sampling_frame$stratum == "strata_A", ]
custom_frame$inclusion <- TRUE
custom_strata <- survey_protocol$sample_table[survey_protocol$sample_table$stratum_id == "strata_A", ]
custom_strata$Sampling_Method <- "srs"
custom_strata$n_psu            <- 15

survey_protocol$draw_sample(frame = custom_frame, strata_table = custom_strata, seed = 111)
nrow(survey_protocol$drawn_sample)

# Restore full proportional draw for report generation later
survey_protocol$draw_sample(seed = 789)

# -- 7f: Standalone PSU utility functions --

sample_result_A <- draw_sample_psu_pps_cluster(
  frame        = frame_A,
  n_clusters   = 12,
  cluster_size = 20,
  seed         = 789
)
nrow(sample_result_A)
sum(!is.na(sample_result_A$sampled_psu))
head(sample_result_A[!is.na(sample_result_A$sampled_psu), ])

purposive_result <- draw_sample_psu_purposive(frame_A)
sum(is.na(purposive_result$sampled_psu))  # all NA

sample_srs_A <- draw_sample_srs(frame_A, n = 100, seed = 789)
head(sample_srs_A)
attr(sample_srs_A, "sampling_method")
attr(sample_srs_A, "n_drawn")


# =============================================================================
# Test 8: Household Tool ####
# =============================================================================

# -- 8a: Instantiate via SurveyProtocol$add_tools() --

survey_protocol$add_tools(tool_type = "household", tool_name = "Main Household Survey")

length(survey_protocol$tools)
survey_protocol$tools[["Main Household Survey"]]$get_name()
survey_protocol$tools[["Main Household Survey"]]$get_tool_type()

# -- 8b: Directly instantiate a HouseholdTool --

hh_tool <- HouseholdTool$new(name = "Household Survey Tool – Strata A & B")
hh_tool$get_name()
hh_tool$get_tool_type()
hh_tool$has_roster()

master_survey  <- hh_tool$get_master_survey()
master_choices <- hh_tool$get_master_choices()
nrow(master_survey)
nrow(master_choices)
head(master_survey)

# -- 8c: Inspect settings --

hh_settings <- hh_tool$get_settings()
hh_settings

# -- 8d: Change default language --

hh_tool$change_default_language("French")
hh_tool$get_settings()

hh_tool$change_default_language("English")
hh_tool$get_settings()

# -- 8e: Filter survey by modules --

head(master_survey[[1]], 30)
unique(master_survey[[1]])

hh_tool$filter_survey_by_modules(modules = c("core", "FSL", "WASH"))

modified_survey <- hh_tool$get_modified_survey()
nrow(modified_survey)
head(modified_survey)

# -- 8f: Filter choices to match modified survey --

hh_tool$filter_choices_from_survey()

modified_choices <- hh_tool$get_modified_choices()
nrow(modified_choices)
unique(modified_choices$list_name)

# -- 8g: Validate tool --

is_valid <- hh_tool$validate_tool()
cat("Household tool valid:", is_valid, "\n")
hh_tool$get_validation_errors()

# -- 8h: Update a choice list with custom values --

new_admin1_choices <- data.frame(
  name  = c("north", "east", "south", "west"),
  label = c("North Region", "East Region", "South Region", "West Region"),
  stringsAsFactors = FALSE
)

hh_tool$update_choice_list(
  list_name   = "admin1",
  new_choices = new_admin1_choices
)

updated_choices <- hh_tool$get_modified_choices()
updated_choices[updated_choices$list_name == "admin1", ]

# -- 8i: Inspect roster groups --

roster_groups <- hh_tool$get_roster_groups()
nrow(roster_groups)
head(roster_groups)

if (nrow(roster_groups) > 0) {
  first_roster <- roster_groups$name[1]
  cat("First roster group:", first_roster, "\n")
  roster_qs <- hh_tool$get_roster_questions(first_roster)
  head(roster_qs)
}

# -- 8j: Add / update / remove a custom question --

hh_tool$add_question(
  type     = "integer",
  name     = "hh_monthly_income",
  label    = "Estimated monthly household income (USD)",
  required = FALSE
)

hh_tool$has_question("hh_monthly_income")
hh_tool$get_question("hh_monthly_income")

hh_tool$update_question("hh_monthly_income", label = "Monthly household income in USD (estimated)")
hh_tool$get_question("hh_monthly_income")

hh_tool$remove_question("hh_monthly_income")
hh_tool$has_question("hh_monthly_income")

# -- 8k: Update settings --

hh_tool$update_settings("form_title", "IPHRA Household Survey – Dummy Country 2025")
hh_tool$update_settings("version",    "2025-03-01")
hh_tool$get_settings()

# -- 8l: Set selected indicators --

hh_tool$set_selected_indicators(c("hdds_score", "fcs_score", "water_source"))
hh_tool$get_selected_indicators()


# =============================================================================
# Test 9: Key Informant Interview (KII) Tool ####
# =============================================================================

# -- 9a: Instantiate via add_tools() --

survey_protocol$add_tools(tool_type = "key_informant", tool_name = "Community KII")

survey_protocol$tools[["Community KII"]]$get_name()
survey_protocol$tools[["Community KII"]]$get_tool_type()

# -- 9b: Directly instantiate a KeyInformantTool --

kii_tool <- KeyInformantTool$new(
  name     = "Health Facility KII",
  kii_type = "health"
)

kii_tool$get_name()
kii_tool$get_tool_type()

kii_survey  <- kii_tool$get_master_survey()
kii_choices <- kii_tool$get_master_choices()
nrow(kii_survey)
nrow(kii_choices)
head(kii_survey)

kii_valid <- kii_tool$validate_tool()
cat("KII tool valid:", kii_valid, "\n")
kii_tool$get_validation_errors()

kii_tool$add_question(
  type     = "text",
  name     = "facility_name",
  label    = "Name of the health facility",
  required = TRUE
)

kii_tool$has_question("facility_name")
kii_tool$get_question("facility_name")


# =============================================================================
# Test 10: Observation and Generic Tools ####
# =============================================================================

# -- 10a: Observation Tool --

survey_protocol$add_tools(tool_type = "observation", tool_name = "Water Point Observation")

survey_protocol$tools[["Water Point Observation"]]$get_name()
survey_protocol$tools[["Water Point Observation"]]$get_tool_type()

obs_tool <- ObservationTool$new(
  name             = "Sanitation Facility Observation",
  observation_type = "latrine"
)

obs_tool$get_name()
obs_tool$get_tool_type()

obs_survey  <- obs_tool$get_master_survey()
obs_choices <- obs_tool$get_master_choices()
nrow(obs_survey)
nrow(obs_choices)

obs_valid <- obs_tool$validate_tool()
cat("Observation tool valid:", obs_valid, "\n")
obs_tool$get_validation_errors()

obs_tool$add_question(
  type     = "select_one yes_no",
  name     = "handwashing_station_present",
  label    = "Is a handwashing station present at the facility?"
)

obs_tool$has_question("handwashing_station_present")

# -- 10b: Generic Tool --

survey_protocol$add_tools(tool_type = "generic", tool_name = "Market Assessment Form")

survey_protocol$tools[["Market Assessment Form"]]$get_name()
survey_protocol$tools[["Market Assessment Form"]]$get_tool_type()

generic_tool <- Tool$new(name = "Focus Group Discussion Guide")
generic_tool$get_tool_type()

generic_tool$add_question(type = "text",    name = "community_name",    label = "Name of the community")
generic_tool$add_question(type = "integer", name = "participants_count", label = "Number of participants")
generic_tool$add_question(type = "select_one yes_no", name = "consent_obtained", label = "Was group consent obtained?")

nrow(generic_tool$get_survey())
generic_tool$has_question("community_name")

generic_valid <- generic_tool$validate_tool()
cat("Generic tool valid:", generic_valid, "\n")
generic_tool$get_validation_errors()


# =============================================================================
# Test 11: Finalise and Inspect the SurveyProtocol ####
# =============================================================================

# -- 11a: Check issues --

survey_protocol$get_issues()

# -- 11b: Summary includes sampling fields --

sp_full_summary <- survey_protocol$get_protocol_summary()
sp_full_summary
sp_full_summary$num_objectives
sp_full_summary$num_strata        # SurveyProtocol-only field
sp_full_summary$total_sample_size

# -- 11c: Export full protocol --

exported <- survey_protocol$export_protocol()

names(exported)
exported$metadata
exported$summary

count_objectives(exported$objectives)

# Sampling fields present in export
exported$sample_table
names(exported$sample_table)

# Confirm new sample size column names
stopifnot("General_HH_Sample_Size" %in% names(exported$sample_table))
stopifnot("Ind_HH_Sample_Size" %in% names(exported$sample_table))
stopifnot("Mort_HH_Sample_Size" %in% names(exported$sample_table))
stopifnot("Ind_Sample_Size" %in% names(exported$sample_table))
stopifnot("Mort_Ind_Sample_Size" %in% names(exported$sample_table))
stopifnot("Mort_PT_Sample_Size" %in% names(exported$sample_table))
stopifnot("Final_HH_Sample_Size" %in% names(exported$sample_table))

nrow(exported$drawn_sample)
nrow(exported$drawn_sample_full)

# Tools
length(exported$tools)
sapply(exported$tools, function(t) t$get_name())

# -- 11d: print_protocol_summary() works for both classes --

print_protocol_summary(protocol)          # IPHRAProtocol (no num_strata)
print_protocol_summary(survey_protocol)   # SurveyProtocol (num_strata = 3)

# -- 11e: Save and reload via RDS --

tmp_file <- tempfile(fileext = ".rds")
save_protocol(survey_protocol, file = tmp_file)

reloaded <- load_protocol(tmp_file)
names(reloaded)
reloaded$metadata$assessment_title
reloaded$summary

# -- 11f: restore_protocol() auto-selects SurveyProtocol when sampling data present --

restored_sp <- restore_protocol(exported)
stopifnot(inherits(restored_sp, "SurveyProtocol"))

# IPHRAProtocol restores as a base Protocol (no sampling data in its export)
restored_base <- restore_protocol(protocol$export_protocol())
stopifnot(inherits(restored_base, "Protocol"))
stopifnot(!inherits(restored_base, "SurveyProtocol"))


# =============================================================================
# Test 12: Report Generation ####
# =============================================================================
# generate_reach_tor() / generate_protocol_report() write a .docx file.
# base Protocol: metadata + objectives + tools + sampling placeholder
# SurveyProtocol: adds full Sampling Design section (strata table + selected PSUs)

# -- 12a: SurveyProtocol report after draw_sample() was called --

report_survey_path <- tempfile(fileext = ".docx")
survey_protocol$generate_reach_tor(output_file = report_survey_path)
stopifnot(file.exists(report_survey_path))
cat("SurveyProtocol report written to:", report_survey_path, "\n")

# Using the standalone wrapper
report_survey_path2 <- tempfile(fileext = ".docx")
generate_protocol_report(survey_protocol, output_file = report_survey_path2)
stopifnot(file.exists(report_survey_path2))
cat("SurveyProtocol report (wrapper) written to:", report_survey_path2, "\n")

# -- 12b: SurveyProtocol report when no sample has been drawn yet --

sp_no_draw <- create_survey_protocol(
  assessment_title = "Draft Survey – No Sample",
  country_name     = country_name,
  month_year       = "April 2026"
)
sp_no_draw$add_stratum(
  stratum_id      = "pilot",
  stratum_name    = "Pilot Area",
  population_size = 5000,
  ind_indicator   = "wasting_prevalence",
  mort_indicator  = "crude_death_rate",
  sampling_method = "srs",
  n_psu           = 10
)
report_no_draw <- tempfile(fileext = ".docx")
sp_no_draw$generate_reach_tor(output_file = report_no_draw)
stopifnot(file.exists(report_no_draw))
cat("SurveyProtocol report (no draw) written to:", report_no_draw, "\n")

# -- 12c: Base Protocol report using objectives from Framework --

base_only <- Protocol$new(
  assessment_title = "Secondary Data Review",
  country_name     = "Country X",
  month_year       = "April 2025"
)
# Use a custom Framework to define objectives
base_only_fw <- Framework$new()
base_only_fw$add_objective_row(list(
  sector          = "General",
  pillar          = "Context",
  sub_pillar      = "Background",
  short_objective = "GEN-01",
  text_objective  = "Review secondary data on humanitarian conditions"
))
base_only$framework <- base_only_fw

report_base_path <- tempfile(fileext = ".docx")
generate_protocol_report(base_only, output_file = report_base_path)
stopifnot(file.exists(report_base_path))
cat("Base Protocol report written to:", report_base_path, "\n")

# -- 12d: validate_protocol() works on both classes --

validate_protocol(protocol)
validate_protocol(survey_protocol)

cat("\n=== Protocol and Tool workflow test completed successfully ===\n")


# =============================================================================
# Test 13: Framework and ANAFramework Workflow ####
# =============================================================================

cat("\n--- Test 13: Framework and ANAFramework ---\n")

# -- 13a: Create a base Framework and set a manual schema and SVG --

fw_base <- create_framework()
stopifnot(inherits(fw_base, "Framework"))
stopifnot(is.null(fw_base$master_schema))
stopifnot(is.null(fw_base$master_svg))

manual_schema <- data.frame(
  sector          = c("FSL", "FSL", "WASH", "WASH"),
  pillar          = c("Food Security", "Food Security", "Water", "Water"),
  sub_pillar      = c("FoodSecurity", "FoodSecurity", "WaterSecurity", "WaterSecurity"),
  short_objective = c("FSL-01", "FSL-01", "WASH-01", "WASH-01"),
  # same objective text per short_objective is intentional — rows differ by indicator
  text_objective  = c("Estimate food insecurity", "Estimate food insecurity", "Assess water access", "Assess water access"),
  indicator       = c("FCS", "HDDS", "Water Source", "Water Quality"),
  core            = c("Core", "Core", NA, NA),
  extended        = c(NA, NA, "Extended", "Extended"),
  stringsAsFactors = FALSE
)

# Duplicate short_objectives are now ALLOWED (multiple indicators per objective)
fw_base$set_master_schema(manual_schema)
stopifnot(is.data.frame(fw_base$master_schema))
stopifnot(nrow(fw_base$master_schema) == 4)
validate_objective_schema(fw_base$master_schema)  # should pass without warnings

fw_base$set_master_svg('<svg><g id="FoodSecurity"><rect fill="white" stroke="black"/></g><g id="WaterSecurity"><rect fill="white" stroke="black"/></g></svg>')
stopifnot(!is.null(fw_base$master_svg))
cat("Base Framework created with manual schema (3 rows) and SVG.\n")

# -- 13b: update_adjusted_schema() filters to selected objectives --

fw_base$update_adjusted_schema(c("FSL-01", "WASH-01"))
stopifnot(is.data.frame(fw_base$adjusted_schema))
stopifnot(nrow(fw_base$adjusted_schema) == 4)  # 2 rows each for FSL-01 and WASH-01
cat("Adjusted schema rows:", nrow(fw_base$adjusted_schema), "(expected 4, 2 per objective)\n")

# -- 13c: Base Framework update_adjusted_svg() hides non-selected elements --

fw_base$update_adjusted_svg()
stopifnot(!is.null(fw_base$adjusted_svg))
cat("Base Framework adjusted SVG generated.\n")

# -- 13d: Create ANAFramework and verify auto-loaded resources --

af <- ANAFramework$new()
stopifnot(inherits(af, "ANAFramework"))
stopifnot(inherits(af, "Framework"))

# master_schema loaded from reference.xlsx
stopifnot(!is.null(af$master_schema) && is.data.frame(af$master_schema))
stopifnot(nrow(af$master_schema) > 0)
cat("ANAFramework master_schema rows:", nrow(af$master_schema), "\n")
cat("ANAFramework master_schema cols:", paste(names(af$master_schema), collapse = ", "), "\n")

# master_svg loaded from ana_framework.svg
stopifnot(!is.null(af$master_svg))
stopifnot(nzchar(af$master_svg))
stopifnot(grepl("<svg", af$master_svg))
cat("ANAFramework master_svg loaded (", nchar(af$master_svg), "chars)\n")

# -- 13e: filter_schema_by_pillar() --

available_pillars <- unique(af$master_schema$pillar)
cat("Available pillars:", paste(available_pillars, collapse = ", "), "\n")

if (length(available_pillars) > 0) {
  pillar_subset <- af$filter_schema_by_pillar(available_pillars[1])
  stopifnot(is.data.frame(pillar_subset))
  stopifnot(nrow(pillar_subset) > 0)
  stopifnot(all(pillar_subset$pillar == available_pillars[1]))
  cat("filter_schema_by_pillar('", available_pillars[1], "'): ",
      nrow(pillar_subset), "rows\n")
}

# -- 13f: get_preset_objectives() for all named presets --

for (preset_name in c("core", "extended", "outcomes", "fsl", "wash", "health")) {
  preset_objs <- af$get_preset_objectives(preset_name)
  stopifnot(is.character(preset_objs))
  cat("Preset '", preset_name, "': ", length(preset_objs), "objective(s)\n")
}

# -- 13g: update_adjusted_schema() + update_adjusted_svg() with preset --

fsl_objs <- af$get_preset_objectives("fsl")
cat("FSL preset objectives:", length(fsl_objs), "\n")

af$update_adjusted_schema(fsl_objs)
stopifnot(is.data.frame(af$adjusted_schema))
stopifnot(nrow(af$adjusted_schema) > 0)
cat("Adjusted schema for FSL preset:", nrow(af$adjusted_schema), "rows\n")

af$update_adjusted_svg(highlight_colour = "lightgreen", default_colour = "white")
stopifnot(!is.null(af$adjusted_svg))
stopifnot(nzchar(af$adjusted_svg))
cat("ANAFramework adjusted SVG generated (",
    nchar(af$adjusted_svg), "chars)\n")

# -- 13h: Verify highlight colour appears in adjusted SVG --

fsl_sub_pillars <- unique(af$adjusted_schema$sub_pillar)
fsl_sub_pillars <- fsl_sub_pillars[!is.na(fsl_sub_pillars) & nzchar(fsl_sub_pillars)]
cat("FSL sub_pillars (SVG block IDs):", paste(fsl_sub_pillars, collapse = ", "), "\n")

# Check that at least one highlighted block appears in the SVG
if (length(fsl_sub_pillars) > 0) {
  # Look for the first block ID that actually exists in master_svg
  blocks_in_svg <- fsl_sub_pillars[
    sapply(fsl_sub_pillars, function(b) grepl(paste0('id="', b, '"'), af$master_svg))
  ]
  if (length(blocks_in_svg) > 0) {
    stopifnot(grepl("lightgreen", af$adjusted_svg))
    cat("Highlight colour 'lightgreen' confirmed in adjusted SVG.\n")
  }
}

# -- 13i: Attach framework to an IPHRAProtocol and round-trip via export/restore --

fw_protocol <- IPHRAProtocol$new(
  assessment_title = "ANA Framework Integration Test",
  country_name     = "Test Country",
  month_year       = "April 2026"
)

# Replace the auto-created ANAFramework with the FSL-preset one from above
fw_protocol$framework <- af
stopifnot(inherits(fw_protocol$framework, "ANAFramework"))
stopifnot(inherits(fw_protocol$framework, "Framework"))

exported_fw_protocol <- fw_protocol$export_protocol()
stopifnot("framework" %in% names(exported_fw_protocol))
stopifnot(is.list(exported_fw_protocol$framework))
cat("Protocol export includes framework field (class =",
    exported_fw_protocol$framework$class, ")\n")

restored_fw_protocol <- restore_protocol(exported_fw_protocol)
stopifnot(inherits(restored_fw_protocol$framework, "ANAFramework"))
stopifnot(!is.null(restored_fw_protocol$framework$master_schema))
cat("Restored Protocol framework class:",
    class(restored_fw_protocol$framework)[1], "\n")
cat("Restored framework master_schema rows:",
    nrow(restored_fw_protocol$framework$master_schema), "\n")

# -- 13j: restore_framework() directly --

exported_fw <- af$export_framework()
stopifnot(is.list(exported_fw))
stopifnot(exported_fw$class == "ANAFramework")

restored_fw <- restore_framework(exported_fw)
stopifnot(inherits(restored_fw, "ANAFramework"))
stopifnot(inherits(restored_fw, "Framework"))
stopifnot(is.data.frame(restored_fw$master_schema))
stopifnot(nrow(restored_fw$master_schema) > 0)
cat("restore_framework() restored ANAFramework with",
    nrow(restored_fw$master_schema), "schema rows.\n")

# -- 13k: update_adjusted_svg() with custom highlight colour --

af2 <- ANAFramework$new()
wash_objs <- af2$get_preset_objectives("wash")
af2$update_adjusted_schema(wash_objs)
af2$update_adjusted_svg(highlight_colour = "lightblue", default_colour = "white")
stopifnot(!is.null(af2$adjusted_svg))
cat("WASH preset adjusted SVG generated with lightblue highlight.\n")

# -- 13l: Framework$add_objective_row() and remove_objective_row() --

fw_edit <- Framework$new()
fw_edit$add_objective_row(list(
  sector          = "FSL",
  pillar          = "Food Security",
  sub_pillar      = "FCS",
  short_objective = "FSL-01",
  text_objective  = "Estimate food insecurity prevalence"
))
fw_edit$add_objective_row(list(
  sector          = "FSL",
  pillar          = "Food Security",
  sub_pillar      = "FCS",
  short_objective = "FSL-01",
  text_objective  = "Estimate food insecurity prevalence",
  indicator       = "HDDS score"
))
stopifnot(nrow(fw_edit$adjusted_schema) == 2L)
cat("Framework adjusted_schema rows after 2 add_objective_row calls:", nrow(fw_edit$adjusted_schema), "\n")

fw_edit$remove_objective_row("FSL-01")
stopifnot(is.null(fw_edit$adjusted_schema) || nrow(fw_edit$adjusted_schema) == 0L)
cat("Framework adjusted_schema rows after remove_objective_row:", nrow(fw_edit$adjusted_schema), "\n")

# -- 13m: render_framework_svg() on the Framework (renders to active graphics device) --
# render_framework_svg() is a Framework method that displays the SVG in the
# RStudio Plots pane.  To also save the raw SVG to disk, write the content directly.

fw_protocol$framework$update_adjusted_svg()
svg_file <- tempfile(fileext = ".svg")
writeLines(
  fw_protocol$framework$adjusted_svg %||% fw_protocol$framework$master_svg,
  con = svg_file
)
stopifnot(file.exists(svg_file))
cat("Framework SVG written to:", svg_file, "\n")

# Display in the active graphics device (RStudio Plots pane)
# fw_protocol$framework$render_framework_svg()  # uncomment when running interactively

cat("\n=== Framework and ANAFramework workflow test completed successfully ===\n")
