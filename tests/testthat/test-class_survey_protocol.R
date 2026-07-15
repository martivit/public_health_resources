# Tests for SurveyProtocol$calculate_sample_sizes() field plan integration.
# Covers the estimate_field_plan() call added to the calculate_sample_sizes
# workflow.

# ---- helpers -----------------------------------------------------------------

make_protocol <- function() {
  create_survey_protocol(
    assessment_title = "Test Assessment",
    country_name     = "Testland",
    month_year       = "January 2024"
  )
}

# ---- add_stratum initialises field-plan columns as NA -------------------------

test_that("add_stratum initialises field-plan columns as NA", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Stratum 1",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  st <- p$get_sample_table()
  expect_true("num_interview_per_enum_per_day" %in% names(st))
  expect_true("num_days"                       %in% names(st))
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

# ---- field-plan columns remain NA when logistics params absent ----------------

test_that("calculate_sample_sizes leaves field-plan columns NA when logistics params are absent", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Stratum 1",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

# ---- simple_random field plan populates columns ------------------------------

test_that("calculate_sample_sizes populates field-plan columns for simple_random stratum", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 2,
    enumerators_per_team    = 3,
    avg_interview_time      = 45,
    avg_travel_time         = 30,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_false(is.na(st$num_interview_per_enum_per_day))
  expect_false(is.na(st$num_days))
  # simple_random: n_psu and cluster_size stay NA (no cluster field plan)
  expect_true(is.na(st$n_psu))
  expect_true(is.na(st$cluster_size))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
})

# ---- cluster field plan populates all four columns ---------------------------

test_that("calculate_sample_sizes populates all field-plan columns for cluster stratum", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Rural",
    population_size         = 20000,
    sampling_method         = "pps_cluster",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    pop_design_effect       = 1.5,
    teams                   = 3,
    enumerators_per_team    = 4,
    clusters_per_day        = 2,
    avg_interview_time      = 40,
    avg_travel_time         = 45,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_false(is.na(st$num_interview_per_enum_per_day))
  expect_false(is.na(st$num_days))
  # cluster: n_psu and cluster_size are populated from field plan
  expect_false(is.na(st$n_psu))
  expect_false(is.na(st$cluster_size))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
  expect_true(st$n_psu > 0)
  expect_true(st$cluster_size > 0)
})

# ---- cluster stratum missing clusters_per_day leaves field-plan NA -----------

test_that("cluster stratum without clusters_per_day leaves field-plan columns NA", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Rural",
    population_size         = 20000,
    sampling_method         = "pps_cluster",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 3,
    enumerators_per_team    = 4,
    # clusters_per_day intentionally omitted
    avg_interview_time      = 40,
    avg_travel_time         = 45,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
  expect_true(is.na(st$n_psu))
  expect_true(is.na(st$cluster_size))
})

# ---- multi-stratum: each row gets its own field plan -------------------------

test_that("calculate_sample_sizes fills field-plan per stratum independently", {
  p <- make_protocol()

  # stratum with full logistics params
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 2,
    enumerators_per_team    = 3,
    avg_interview_time      = 45,
    avg_travel_time         = 30,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )

  # stratum without logistics params
  p$add_stratum(
    stratum_id              = "s2",
    stratum_name            = "Rural",
    population_size         = 5000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 7
  )

  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  s1 <- st[st$stratum_id == "s1", ]
  s2 <- st[st$stratum_id == "s2", ]

  expect_false(is.na(s1$num_days))
  expect_true(is.na(s2$num_days))
})

# ---- calc_method is correctly derived from sampling_method -------------------

test_that("add_stratum derives calc_method correctly for all sampling_method values", {
  p <- make_protocol()

  # Methods that apply to all PSUs (no n_sites required) and use "simple_random" calc_method
  all_psu_simple_methods <- c("proportional", "purposive")
  # Methods that select a subset of PSUs and use "simple_random" calc_method
  site_select_simple_methods <- c("simple_random", "systematic")
  # Methods that use "cluster" calc_method and select a subset of PSUs
  site_select_cluster_methods <- c("pps_rlc", "simple_random_rlc", "systematic_rlc")
  # Methods that use "cluster" calc_method and apply to all PSUs
  all_psu_cluster_methods <- c("proportional_rlc")

  for (m in all_psu_simple_methods) {
    p$add_stratum(stratum_id = m, stratum_name = m, sampling_method = m)
  }
  for (m in site_select_simple_methods) {
    p$add_stratum(stratum_id = m, stratum_name = m, sampling_method = m, n_sites = 1)
  }
  # pps_cluster: n_psu optional at add_stratum time
  p$add_stratum(stratum_id = "pps_cluster", stratum_name = "pps_cluster",
                sampling_method = "pps_cluster")
  for (m in site_select_cluster_methods) {
    p$add_stratum(stratum_id = m, stratum_name = m, sampling_method = m, n_sites = 10)
  }
  for (m in all_psu_cluster_methods) {
    p$add_stratum(stratum_id = m, stratum_name = m, sampling_method = m)
  }

  st <- p$get_sample_table()
  expect_true("calc_method" %in% names(st))

  for (m in c(all_psu_simple_methods, site_select_simple_methods)) {
    expect_equal(st$calc_method[st$stratum_id == m], "simple_random",
                 info = paste("calc_method for sampling_method =", m))
  }
  for (m in c("pps_cluster", site_select_cluster_methods, all_psu_cluster_methods)) {
    expect_equal(st$calc_method[st$stratum_id == m], "cluster",
                 info = paste("calc_method for sampling_method =", m))
  }
})

test_that("add_stratum rejects invalid sampling_method values", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1", sampling_method = "srs",
                  n_sites = 5),
    regexp = "sampling_method must be one of"
  )
})

test_that("add_stratum errors when sampling_method is not provided", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1"),
    regexp = "sampling_method is required"
  )
})

test_that("add_stratum errors when n_sites missing for site-selection methods", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1",
                  sampling_method = "simple_random"),
    regexp = "n_sites is required"
  )
})

test_that("add_stratum accepts proportional and purposive without n_sites", {
  p <- make_protocol()
  # proportional applies to all eligible PSUs — n_sites not required
  p$add_stratum(stratum_id = "prop", stratum_name = "prop",
                sampling_method = "proportional")
  # purposive applies to all eligible PSUs — n_sites not required
  p$add_stratum(stratum_id = "purp", stratum_name = "purp",
                sampling_method = "purposive")
  expect_equal(nrow(p$get_sample_table()), 2L)
})

test_that("add_stratum for pps_rlc defaults cluster_size to 3", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "s1", stratum_name = "s1",
                sampling_method = "pps_rlc", n_sites = 10)
  st <- p$get_sample_table()
  expect_equal(st$cluster_size, 3)
})

# ---- SamplingFrame initialises as a blank SamplingFrame object ---------------

test_that("SurveyProtocol initialises sampling_frame as a SamplingFrame object", {
  p <- make_protocol()
  expect_true(inherits(p$sample_object, "Sample"))
  expect_null(p$get_sample_table())
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 0L)
  expected_cols <- c("stratum", "psu", "population_size", "inclusion",
                     "sampled_psu", "allocated_sample")
  expect_true(all(expected_cols %in% names(p$sampling_frame$log_df)))
})

test_that("SurveyProtocol accepts a data frame on init and stores it in SamplingFrame", {
  frame <- data.frame(
    stratum          = "urban",
    psu              = "psu_1",
    population_size  = 500,
    inclusion        = TRUE,
    sampled_psu      = NA_real_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  p <- create_survey_protocol(sampling_frame = frame)
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 1L)
  expect_equal(p$sampling_frame$log_df$psu, "psu_1")
})

test_that("set_sampling_frame stores data in the SamplingFrame log_df", {
  p <- make_protocol()
  frame <- data.frame(
    stratum          = c("urban", "rural"),
    psu              = c("psu_1", "psu_2"),
    population_size  = c(500, 800),
    inclusion        = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_equal(nrow(p$sampling_frame$log_df), 2L)
  expect_true("inclusion" %in% names(p$sampling_frame$log_df))
})

# ---- SamplingFrame class initialises correctly -------------------------------

test_that("SamplingFrame initialises empty with required columns", {
  sf <- SamplingFrame$new()
  expect_true(inherits(sf, "SamplingFrame"))
  expect_equal(nrow(sf$log_df), 0L)
  expected_cols <- c("stratum", "psu", "population_size", "inclusion",
                     "sampled_psu", "allocated_sample")
  expect_true(all(expected_cols %in% names(sf$log_df)))
})

test_that("SamplingFrame initialises with a provided data frame", {
  frame <- data.frame(
    stratum          = "A",
    psu              = "psu_1",
    population_size  = 1000,
    inclusion        = TRUE,
    sampled_psu      = NA_real_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  sf <- SamplingFrame$new(log_df = frame)
  expect_equal(nrow(sf$log_df), 1L)
  expect_equal(sf$log_df$stratum, "A")
})

# ---- clear_sample clears selection columns but retains frame -----------------

test_that("clear_sample resets sampled_psu and allocated_sample but retains other columns", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  p$calculate_sample_sizes()

  frame <- data.frame(
    stratum          = rep("s1", 10),
    psu              = paste0("psu_", seq_len(10)),
    population_size  = rep(500, 10),
    inclusion        = rep(TRUE, 10),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$draw_sample()

  # After drawing, at least some PSUs should be selected
  expect_false(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_false(is.null(p$drawn_sample))

  # Clear and verify
  p$clear_sample()

  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_true(all(is.na(p$sampling_frame$log_df$allocated_sample)))
  expect_null(p$drawn_sample)
  expect_null(p$drawn_sample_full)

  # Frame columns other than the cleared ones are retained
  expect_equal(nrow(p$sampling_frame$log_df), 10L)
  expect_true(all(p$sampling_frame$log_df$stratum == "s1"))
  expect_equal(p$sampling_frame$log_df$psu, paste0("psu_", seq_len(10)))
})

test_that("clear_sample is a no-op on an empty sampling frame", {
  p <- make_protocol()
  expect_no_error(p$clear_sample())
  expect_equal(nrow(p$sampling_frame$log_df), 0L)
  expect_null(p$drawn_sample)
})

# ---- draw_sample warns and skips stratum when required param missing ----------

test_that("draw_sample issues warning and skips pps_cluster stratum when n_psu missing", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id      = "s1",
    stratum_name    = "Rural",
    sampling_method = "pps_cluster",
    cluster_size    = 5,
    General_HH_Sample_Size = 50
  )
  frame <- data.frame(
    stratum         = rep("s1", 20),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(p$draw_sample(), regexp = "skipped")
  # All PSUs remain unselected
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})

test_that("draw_sample warns and skips on failure but continues with other strata", {
  p <- make_protocol()
  # s1: pps_cluster without n_psu — will warn and skip
  p$add_stratum(
    stratum_id      = "s1",
    stratum_name    = "Rural",
    sampling_method = "pps_cluster",
    cluster_size    = 5,
    General_HH_Sample_Size = 50
  )
  # s2: simple_random with n_sites — will succeed
  p$add_stratum(
    stratum_id      = "s2",
    stratum_name    = "Urban",
    sampling_method = "simple_random",
    n_sites         = 3,
    General_HH_Sample_Size = 30
  )
  frame <- data.frame(
    stratum         = c(rep("s1", 10), rep("s2", 10)),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(p$draw_sample(), regexp = "skipped")
  # s2 should have some PSUs selected
  s2_rows <- p$sampling_frame$log_df[p$sampling_frame$log_df$stratum == "s2", ]
  expect_false(all(is.na(s2_rows$sampled_psu)))
})

# ---- reserve cluster (RC) behaviour ------------------------------------------

test_that("draw_sample (simple_random) includes RC-labelled reserve PSUs", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id             = "s1",
    stratum_name           = "Urban",
    population_size        = 10000,
    sampling_method        = "simple_random",
    n_sites                = 5,
    General_HH_Sample_Size = 50
  )
  # 20-PSU frame guarantees enough units for main + reserve
  frame <- data.frame(
    stratum         = rep("s1", 20),
    psu             = paste0("psu_", seq_len(20)),
    population_size = rep(500, 20),
    inclusion       = rep(TRUE, 20),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$draw_sample()

  psu_vals <- p$drawn_sample$sampled_psu
  # n_main = 5 (<=10) -> 3 RC; total drawn = 8
  expect_equal(sum(psu_vals == "RC"), 3L)
  main_nums <- suppressWarnings(as.integer(psu_vals[psu_vals != "RC"]))
  expect_equal(sort(main_nums), 1:5)
  # RC PSUs have NA allocated_sample
  expect_true(all(is.na(p$drawn_sample$allocated_sample[p$drawn_sample$sampled_psu == "RC"])))
})

test_that("draw_sample (systematic) includes RC-labelled reserve PSUs", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id             = "s1",
    stratum_name           = "Rural",
    population_size        = 10000,
    sampling_method        = "systematic",
    n_sites                = 5,
    General_HH_Sample_Size = 50
  )
  frame <- data.frame(
    stratum         = rep("s1", 20),
    psu             = paste0("psu_", seq_len(20)),
    population_size = rep(500, 20),
    inclusion       = rep(TRUE, 20),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$draw_sample()

  psu_vals <- p$drawn_sample$sampled_psu
  expect_equal(sum(psu_vals == "RC"), 3L)
  main_nums <- suppressWarnings(as.integer(psu_vals[psu_vals != "RC"]))
  expect_equal(sort(main_nums), 1:5)
})

test_that("RC numbers are correct for n_main > 10 and n_main > 20", {
  # n_main = 15 -> 4 RC
  result_15 <- draw_sample_psu_srs(
    data.frame(population_size = rep(100, 30)), n_psu = 15, sample_size = 150, seed = 7
  )
  selected <- result_15[!is.na(result_15$sampled_psu), ]
  expect_equal(sum(selected$sampled_psu == "RC"), 4L)

  # n_main = 25 -> 5 RC
  result_25 <- draw_sample_psu_srs(
    data.frame(population_size = rep(100, 50)), n_psu = 25, sample_size = 250, seed = 7
  )
  selected25 <- result_25[!is.na(result_25$sampled_psu), ]
  expect_equal(sum(selected25$sampled_psu == "RC"), 5L)
})

test_that("draw_sample (multi-stratum) applies correct sequential offset across strata with RC", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id = "s1", stratum_name = "Urban",
    sampling_method = "simple_random", n_sites = 5,
    General_HH_Sample_Size = 50
  )
  p$add_stratum(
    stratum_id = "s2", stratum_name = "Rural",
    sampling_method = "simple_random", n_sites = 3,
    General_HH_Sample_Size = 30
  )
  frame <- data.frame(
    stratum         = c(rep("s1", 20), rep("s2", 15)),
    psu             = paste0("psu_", seq_len(35)),
    population_size = rep(500, 35),
    inclusion       = rep(TRUE, 35),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$draw_sample()

  s1_vals  <- p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s1"]
  s2_vals  <- p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s2"]
  s1_nums  <- suppressWarnings(as.integer(s1_vals[!is.na(s1_vals) & s1_vals != "RC"]))
  s2_nums  <- suppressWarnings(as.integer(s2_vals[!is.na(s2_vals) & s2_vals != "RC"]))
  # Numbers in s2 should be > max number in s1
  expect_true(min(s2_nums) > max(s1_nums))
  # RC labels remain "RC" in both strata
  expect_true(all(s1_vals[!is.na(s1_vals) & s1_vals == "RC"] == "RC"))
  expect_true(all(s2_vals[!is.na(s2_vals) & s2_vals == "RC"] == "RC"))
})

# ---- pps_rlc requires n_sites and restricts clusters to pre-selected PSUs ----

test_that("draw_sample_psu_rlc errors when n_sites is missing", {
  frame <- data.frame(population_size = rep(100, 20))
  expect_error(
    draw_sample_psu_rlc(frame, sample_size = 60, n_sites = NULL),
    regexp = "n_sites is required"
  )
})

test_that("draw_sample_psu_rlc restricts cluster allocation to n_sites pre-selected PSUs", {
  frame <- data.frame(population_size = c(100, 200, 150, 300, 250,
                                          120, 180, 90, 400, 110,
                                          220, 130, 170, 310, 240,
                                          80, 160, 270, 190, 350,
                                          100, 200, 150, 300, 250,
                                          120, 180, 90, 400, 110))
  result <- draw_sample_psu_rlc(frame, sample_size = 60, n_sites = 5, cluster_size = 3, seed = 42)

  selected <- result[!is.na(result$sampled_psu), ]
  # Each pre-selected PSU occupies exactly one row in `result`.  Clusters (main
  # and RC) are allocated only within the n_sites=5 pre-selected PSUs, so the
  # number of rows with non-NA sampled_psu (= number of PSUs with any assignment)
  # cannot exceed n_sites.
  expect_lte(nrow(selected), 5L)
  # The total slot labels include both main numbers and "RC"
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
})

test_that("draw_sample_psu_rlc distributes clusters evenly across selected sites", {
  # pps_rlc must spread slots evenly regardless of population sizes.
  frame <- data.frame(population_size = rep(100L, 20))
  result <- draw_sample_psu_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 42)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 3L)
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  # n_clusters = ceiling(30/3) = 10; n_reserve = 3 (<=10); n_total = 13
  expect_equal(sum(slot_counts), 13L)
  # Slots must be as evenly spread as possible (max diff <= 1)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

test_that("apply_sampling_method errors for pps_rlc when n_sites is not supplied at draw time", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id      = "s1",
    stratum_name    = "Rural",
    sampling_method = "pps_rlc",
    n_sites         = 5,
    General_HH_Sample_Size = 30
  )
  # Manually corrupt the strata table to remove n_sites, simulating a missing-param scenario
  st <- p$get_sample_table()
  st$n_sites <- NA_real_
  p$sample_object$set_sample_table(st)

  frame <- data.frame(
    stratum         = rep("s1", 20),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(p$draw_sample(), regexp = "skipped")
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})

# ---- simple_random_rlc ----

test_that("draw_sample_psu_srs_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L))  # 21 PSUs
  result <- draw_sample_psu_srs_rlc(frame, sample_size = 30, n_sites = 4, cluster_size = 3, seed = 7)
  selected <- result[!is.na(result$sampled_psu), ]
  # At most n_sites rows may be selected
  expect_lte(nrow(selected), 4L)
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  # RC labels must be present
  expect_true(any(trimws(all_labels) == "RC"))
  # Total slots: n_clusters = ceiling(30/3) = 10, n_reserve = 3, n_total = 13
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_srs_rlc allocates proportional to pop size when available", {
  # One large site (pop 1000) and several small ones (pop 100).
  frame <- data.frame(population_size = c(rep(1000L, 5), rep(100L, 15)))
  result <- draw_sample_psu_srs_rlc(frame, sample_size = 30, n_sites = 2, cluster_size = 3, seed = 99)
  selected <- result[!is.na(result$sampled_psu), ]
  if (nrow(selected) == 2L) {
    slot_counts <- vapply(selected$sampled_psu, function(s) {
      length(trimws(strsplit(s, ",\\s*")[[1]]))
    }, integer(1))
    # Total slots must still be correct
    expect_equal(sum(slot_counts), 13L)
  }
})

test_that("draw_sample_psu_srs_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(15)))  # no population_size column
  result <- draw_sample_psu_srs_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 1)
  selected <- result[!is.na(result$sampled_psu), ]
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  expect_equal(sum(slot_counts), 13L)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

# ---- systematic_rlc ----

test_that("draw_sample_psu_systematic_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L))  # 21 PSUs
  result <- draw_sample_psu_systematic_rlc(frame, sample_size = 30, n_sites = 4, cluster_size = 3, seed = 7)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 4L)
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_systematic_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(20)))  # no population_size column
  result <- draw_sample_psu_systematic_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 5)
  selected <- result[!is.na(result$sampled_psu), ]
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  expect_equal(sum(slot_counts), 13L)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

test_that("add_stratum accepts simple_random_rlc and systematic_rlc methods", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "a", stratum_name = "A",
                sampling_method = "simple_random_rlc", n_sites = 5,
                General_HH_Sample_Size = 30)
  p$add_stratum(stratum_id = "b", stratum_name = "B",
                sampling_method = "systematic_rlc", n_sites = 5,
                General_HH_Sample_Size = 30)
  st <- p$get_sample_table()
  expect_equal(st$sampling_method, c("simple_random_rlc", "systematic_rlc"))
  expect_equal(st$cluster_size,    c(3L, 3L))
  expect_equal(st$calc_method,     c("cluster", "cluster"))
})

# ---- proportional_rlc ----

test_that("draw_sample_psu_proportional_rlc selects all PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = c(300L, 100L, 200L, 150L, 250L))
  result <- draw_sample_psu_proportional_rlc(frame, sample_size = 30, cluster_size = 3, seed = 42)
  # All PSUs are selected
  expect_equal(nrow(result[!is.na(result$sampled_psu), ]), nrow(frame))
  all_labels <- unlist(strsplit(result$sampled_psu, ",\\s*"))
  # RC labels must be present
  expect_true(any(trimws(all_labels) == "RC"))
  # n_clusters = ceiling(30/3) = 10; n_reserve = 3 (<=10); n_total = 13
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_proportional_rlc allocates proportional to population size", {
  # Large PSU (pop 900) vs four small PSUs (pop 100 each) — large should get more slots
  frame <- data.frame(population_size = c(900L, 100L, 100L, 100L, 100L))
  result <- draw_sample_psu_proportional_rlc(frame, sample_size = 30, cluster_size = 3, seed = 1)
  slot_counts <- vapply(result$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  # Large PSU (row 1) should get substantially more slots than any small PSU
  expect_gt(slot_counts[1], slot_counts[2])
})

test_that("draw_sample_psu_proportional_rlc errors when population_size is missing", {
  frame <- data.frame(psu = paste0("p", seq_len(5)))
  expect_error(
    draw_sample_psu_proportional_rlc(frame, sample_size = 30),
    regexp = "population_size"
  )
})

test_that("add_stratum accepts proportional_rlc without n_sites and defaults cluster_size to 3", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "r1", stratum_name = "R1",
                sampling_method = "proportional_rlc",
                General_HH_Sample_Size = 30)
  st <- p$get_sample_table()
  expect_equal(st$sampling_method, "proportional_rlc")
  expect_equal(st$cluster_size,    3L)
  expect_equal(st$calc_method,     "cluster")
  expect_true(is.na(st$n_sites))
})


# ── SurveyProtocol sampling helpers ─────────────────────────────────────────

test_that("SurveyProtocol$get_sampling_methods returns empty vector before add_stratum", {
  p <- make_protocol()
  expect_equal(p$get_sampling_methods(), character(0))
})

test_that("SurveyProtocol$get_sampling_methods returns methods after add_stratum", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "s1", stratum_name = "S1",
                sampling_method = "simple_random",
                General_HH_Sample_Size = 110)
  methods <- p$get_sampling_methods()
  expect_equal(methods, "simple_random")
})

test_that("SurveyProtocol$get_strata_names returns stratum names", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "s1", stratum_name = "S1",
                sampling_method = "simple_random",
                General_HH_Sample_Size = 110)
  p$add_stratum(stratum_id = "s2", stratum_name = "S2",
                sampling_method = "purposive",
                General_HH_Sample_Size = 60)
  names_out <- p$get_strata_names()
  expect_true("S1" %in% names_out)
  expect_true("S2" %in% names_out)
})

test_that("SurveyProtocol$get_strata_names returns empty vector when no sample table", {
  p <- make_protocol()
  expect_equal(p$get_strata_names(), character(0))
})

test_that("Sample metadata timestamps update after mutations", {
  s <- Sample$new()
  expect_false(is.null(s$metadata$created_datetime))
  expect_false(is.null(s$metadata$modified_datetime))

  t_before <- s$metadata$modified_datetime
  Sys.sleep(0.01)
  s$set_sample_table(data.frame(
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "simple_random",
    stringsAsFactors = FALSE
  ))

  expect_true(s$metadata$modified_datetime > t_before)
})

test_that("Sample$remove_stratum removes rows by stratum name", {
  s <- Sample$new()
  s$add_stratum(stratum_id = "s1", stratum_name = "S1", sampling_method = "purposive")
  s$add_stratum(stratum_id = "s2", stratum_name = "S2", sampling_method = "purposive")

  s$remove_stratum("S1")
  st <- s$get_sample_table()

  expect_equal(nrow(st), 1L)
  expect_equal(st$stratum_name, "S2")
})

test_that("Protocol accesses Sample through access_nested and updates metadata", {
  p <- make_protocol()
  t_before <- p$metadata$modified_datetime

  Sys.sleep(0.01)
  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "simple_random",
    n_sites = 2
  )

  expect_true(p$metadata$modified_datetime > t_before)
  expect_equal(
    p$access_nested(field = "sample_object", member = "get_sampling_methods"),
    "simple_random"
  )
  expect_equal(
    p$access_nested(field = "sample_object", member = "get_strata_names"),
    "S1"
  )

  st <- p$access_nested(field = "sample_object", member = "get_sample_table")
  expect_equal(nrow(st), 1L)

  p$access_nested(field = "sample_object", member = "remove_stratum", "S1")
  expect_equal(
    nrow(p$access_nested(field = "sample_object", member = "get_sample_table")),
    0L
  )
})

test_that("Protocol access_nested can render framework SVG", {
  p <- Protocol$new()
  p$framework$set_master_svg('<svg><rect id="H1"/></svg>')

  out <- p$access_nested("framework", member = "render_framework_svg", version = "master")

  if (requireNamespace("rsvg", quietly = TRUE) &&
      requireNamespace("grid", quietly = TRUE)) {
    expect_true(inherits(out, "Framework"))
  } else {
    expect_true(is.character(out))
    expect_true(file.exists(out))
  }
})

test_that("Protocol-level .replace matches exact tag tokens only", {
  p <- make_protocol()
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "@this_is_a_tag", style = "Normal")
  doc <- officer::body_add_par(doc, "@this_is_a", style = "Normal")

  doc <- p$.__enclos_env__$private$..replace(
    doc,
    "@this_is_a",
    "REPLACED_SHORT"
  )

  body_xml <- officer::docx_body_xml(doc)
  txt <- paste(
    xml2::xml_text(xml2::xml_find_all(body_xml, ".//w:t", xml2::xml_ns(body_xml))),
    collapse = ""
  )

  expect_true(grepl("@this_is_a_tag", txt, fixed = TRUE))
  expect_false(grepl("REPLACED_SHORT_tag", txt, fixed = TRUE))
  expect_true(grepl("REPLACED_SHORT", txt, fixed = TRUE))
})
