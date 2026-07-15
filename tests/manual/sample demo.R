# Considerations for Public Health Sampling
# 1) Need to do household, individual, household to individual, rate persontime, rate individuals, and conversion to households.
# 2) Need for tools to support survey planning (how many days, teams, etc. to complete sample)
# 3) Need for programmable scripts to support our efforts for application development.
# 4) Need for greater transparency, and reporting of key statistics (included vs. excluded populations in the sampling frame, etc.)
# 5) Need for sample calculations to match and use similar assumptions as well accepted external methods, like SMART. (e.g. SMART uses T-value of 2.045 for cluster sample size calculations as an approximation.). General need for comparability here.
# 6) Need to be able to incorporate FPC for smaller populations and assessments.
# 7) Our use cases are not just for MSNAs, we need to be able to use and draw on accepted sampling methods in Public Health, even if not used for MSNAs.
#              E.g. Random Location Cluster sampling from UNHCR, LQAS in the future for WASH and Health service coverage assessments?
# 8) For cluster designs, preference to utilize estimated Design Effects, such as from past surveys or literature, for sample size calculations.
# 9) Need for sampling reserve clusters at the same time as drawing the sample, to be used in case of non-response or other issues with selected clusters.
# 10) Need to have standard of 95% confidence level for public health - it is difficult to defend other confidence levels as this is the standard for comparability. and interpretation.

# the below are a first draft of sampling scripts developed for phr resources and IPHRA app.
# They contain (a) R6 class 'Sample' object  with standard fields and methods/functions, (b) R6 class 'SamplingFrame' object with standard fields and methods/functions,
# (c) utility functions for calculating sample sizes, and (d) utility functions for drawing samples from SamplingFrame.
# sample size calculation utilities have been unit tested against UKSamples and Emergency Nutrition Assessment (ENA) calculators.
# Allowed methods currently are: "simple_random", "systematic", "proportional", "rlc" (random location cluster), and PPS with replacement.

rm(list = ls())

devtools::load_all()

sample <- Sample$new()

sample$add_stratum(
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

sample$add_stratum(
  stratum_id = "strata_B",
  stratum_name = "Peri-Urban East",
  population_size = 28000,
  pop_indicator = "Food Consumption Score",
  pop_design_effect = 1.8,
  pop_precision = 5,
  pop_expected_prevalence = 50,
  pop_nonresponse = 10,
  ind_indicator = "wasting_prevalence",
  rate_indicator = "crude_death_rate",
  rate_expected_rate = 0.2,
  rate_precision = 0.5,
  rate_avg_hh_size = 5.2,
  rate_design_effect = 2,
  rate_fpc = FALSE,
  rate_nonresponse = 10,
  sampling_method_site = "proportional",
  sampling_method_hh = "systematic",
  n_sites = 30
)

sample$add_stratum(
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

sample$calculate_sample_sizes()

View(sample$sample_table)

# Initiate Sampling Frame

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

sf <- dplyr::bind_rows(frame_A, frame_B, frame_C)

sampling_frame <- SamplingFrame$new(log_df = sf)

View(sampling_frame$log_df)

# Drawing Sample

sampling_frame$draw_sample(
  strata_table = sample$get_sample_table(),
  seed = 788
)

View(sampling_frame$drawn_sample)

View(sampling_frame$drawn_sample_full)
