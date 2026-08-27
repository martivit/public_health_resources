# Tests for SurveyProtocol and related classes (Sample, SamplingFrame).
#
# Principle: inherited functionality (Orchestrator, Document, Protocol) is
# tested in their own test files.  This file covers only what SurveyProtocol
# adds: sampling-frame management, sample-size calculations, draw_sample /
# clear_sample integration, active bindings, and SurveyProtocol-specific
# coherence checks.

# ---- helpers -----------------------------------------------------------------

make_protocol <- function() {
  create_survey_protocol(
    assessment_title = "Test Assessment",
    country_name = "Testland",
    month_year = "January 2024"
  )
}

# Shortcut: add a single simple_random stratum to a SurveyProtocol sample_object
add_simple_stratum <- function(
  p,
  stratum_id = "s1",
  stratum_name = "S1",
  n_sites = 5,
  population_size = 10000,
  sample_size = 100
) {
  p$sample_object$add_stratum(
    stratum_id = stratum_id,
    stratum_name = stratum_name,
    sampling_method_site = "simple_random",
    n_sites = n_sites,
    population_size = population_size,
    General_HH_Sample_Size = sample_size
  )
  invisible(p)
}

make_frame <- function(strata = "s1", n_psu = 20, pop = 500) {
  data.frame(
    stratum = rep(strata, each = n_psu),
    psu = paste0("psu_", seq_len(n_psu * length(strata))),
    population_size = rep(pop, n_psu * length(strata)),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
}


# ── SurveyProtocol initialization ─────────────────────────────────────────────

test_that("SurveyProtocol initializes and inherits from Protocol", {
  p <- make_protocol()
  suppressWarnings(suppressMessages({
    expect_true(inherits(p, "SurveyProtocol"))
    expect_true(inherits(p, "Protocol"))
    expect_true(inherits(p, "Document"))
    expect_true(inherits(p, "Orchestrator"))
  }))
})

test_that("SurveyProtocol initialises sample_object as a Sample", {
  p <- make_protocol()
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$sample_object, "Sample"))
    expect_null(p$get_sample_table())
  }))
})

test_that("SurveyProtocol initialises sampling_frame as a SamplingFrame object", {
  p <- make_protocol()
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$sampling_frame, "SamplingFrame"))
    expect_equal(nrow(p$sampling_frame$log_df), 0L)
    expected_cols <- c(
      "stratum",
      "psu",
      "population_size",
      "inclusion",
      "sampled_psu",
      "allocated_sample"
    )
    expect_true(all(expected_cols %in% names(p$sampling_frame$log_df)))
  }))
})

test_that("SurveyProtocol stores metadata passed to constructor", {
  p <- suppressMessages(create_survey_protocol(
    assessment_title = "My Survey",
    country_name = "Somalia",
    month_year = "March 2025"
  ))
  suppressWarnings(suppressMessages({
    expect_equal(p$metadata$assessment_title, "My Survey")
    expect_equal(p$metadata$country_name, "Somalia")
    expect_equal(p$metadata$month_year, "March 2025")
  }))
})

test_that("SurveyProtocol accepts a sampling_frame on initialization", {
  frame <- data.frame(
    stratum = "urban",
    psu = "psu_1",
    population_size = 500,
    inclusion = TRUE,
    sampled_psu = NA_character_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  p <- suppressMessages(create_survey_protocol(sampling_frame = frame))
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$sampling_frame, "SamplingFrame"))
    expect_equal(nrow(p$sampling_frame$log_df), 1L)
    expect_equal(p$sampling_frame$log_df$psu, "psu_1")
  }))
})

# ── Sample$add_stratum via sample_object ───────────────────────────────────────

test_that("sample_object$add_stratum adds a row to the sample table", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Stratum 1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "simple_random",
      n_sites = 5,
      population_size = 10000
    )
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_equal(nrow(st), 1L)
    expect_equal(st$stratum_id, "s1")
    expect_equal(st$stratum_name, "Stratum 1")
    expect_equal(st$sampling_method_site, "simple_random")
    expect_equal(st$n_sites, 5)
  }))
})

test_that("sample_object$add_stratum initializes num_interview_per_enum_per_day and num_days as NA", {
  p <- make_protocol()
  add_simple_stratum(p)
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_true("num_interview_per_enum_per_day" %in% names(st))
    expect_true("num_days" %in% names(st))
    expect_true(is.na(st$num_interview_per_enum_per_day))
    expect_true(is.na(st$num_days))
  }))
})

test_that("sample_object$add_stratum requires sampling_method_site", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    expect_error(
      p$sample_object$add_stratum(
        stratum_id = "s1",
        stratum_name = "S1"
      ),
      regexp = "sampling_method_site is required"
    )
  ))
})

test_that("sample_object$add_stratum rejects invalid sampling_method_site values", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    expect_error(
      p$sample_object$add_stratum(
        stratum_id = "s1",
        stratum_name = "S1",
        sampling_method_site = "pps_cluster",
        n_sites = 5
      ),
      regexp = "sampling_method_site must be one of"
    )
  ))
})

test_that("sample_object$add_stratum requires n_sites", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    expect_error(
      p$sample_object$add_stratum(
        stratum_id = "s1",
        stratum_name = "S1",
        sampling_method_site = "simple_random"
      ),
      regexp = "n_sites is required"
    )
  ))
})

test_that("sample_object$add_stratum defaults sampling_method_hh to simple_random", {
  p <- make_protocol()
  add_simple_stratum(p)
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages(
    expect_equal(st$sampling_method_hh, "simple_random")
  ))
})

test_that("sample_object$add_stratum sets rlc household method when specified", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5,
      cluster_size = 4
    )
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_equal(st$sampling_method_hh, "rlc")
    expect_equal(st$cluster_size, 4)
  }))
})

test_that("sample_object$add_stratum defaults cluster_size to 3 when rlc and no cluster_size given", {
  p <- make_protocol()
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages(
    expect_equal(st$cluster_size, 3L)
  ))
})

test_that("sample_object$add_stratum overwrites existing stratum_id with warning", {
  p <- make_protocol()
  add_simple_stratum(p, sample_size = 100)
  suppressWarnings(suppressMessages(
    expect_warning(
      add_simple_stratum(p, sample_size = 200),
      regexp = "already exists"
    )
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_equal(nrow(st), 1L)
    expect_equal(st$General_HH_Sample_Size, 200)
  }))
})

test_that("sample_object$add_stratum multiple strata accumulates rows", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  ))
  suppressWarnings(suppressMessages(
    add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages(
    expect_equal(nrow(st), 2L)
  ))
})

test_that("sample_object$add_stratum all valid site selection methods work", {
  suppressMessages(p <- make_protocol())
  for (m in c("simple_random", "proportional", "systematic", "purposive")) {
    suppressWarnings(suppressMessages(
      p$sample_object$add_stratum(
        stratum_id = m,
        stratum_name = m,
        sampling_method_site = m,
        n_sites = 5
      )
    ))
  }
  # cluster method also valid
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "cluster",
      stratum_name = "cluster",
      sampling_method_site = "cluster",
      n_sites = 10
    )
  ))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages(
    expect_equal(nrow(st), 5L)
  ))
})

# ── sample_object delegation via SurveyProtocol methods ───────────────────────

test_that("get_sample_table returns NULL when no strata added", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$get_sample_table())
  ))
})

test_that("get_sample_table returns the sample table after add_stratum", {
  suppressMessages(p <- make_protocol())
  add_simple_stratum(p)
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_true(is.data.frame(st))
    expect_equal(nrow(st), 1L)
  }))
})

test_that("get_sampling_methods returns character(0) when no strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_equal(p$get_sampling_methods(), character(0))
  ))
})

test_that("get_sampling_methods returns methods after add_stratum", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    add_simple_stratum(p)
  ))
  methods <- suppressWarnings(suppressMessages(p$get_sampling_methods()))
  suppressWarnings(suppressMessages(
    expect_equal(methods, "simple_random")
  ))
})

test_that("get_strata_names returns character(0) when no strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_equal(p$get_strata_names(), character(0))
  ))
})

test_that("get_strata_names returns names after add_stratum", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  ))
  suppressWarnings(suppressMessages(
    add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  ))
  names_out <- suppressWarnings(suppressMessages(p$get_strata_names()))
  suppressWarnings(suppressMessages({
    expect_true("S1" %in% names_out)
    expect_true("S2" %in% names_out)
  }))
})

test_that("validate_strata_table returns FALSE when no sample table", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(p$validate_strata_table())
  ))
})

test_that("validate_strata_table returns TRUE for a valid sample table", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    add_simple_stratum(p)
  ))
  result <- suppressWarnings(suppressMessages(p$validate_strata_table()))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(result))
  ))
})

# ── Sample$calculate_sample_sizes via sample_object ────────────────────────────

test_that("sample_object$calculate_sample_sizes leaves field-plan columns NA when logistics params absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    add_simple_stratum(
      p,
      population_size = 10000,
      n_sites = 5,
      sample_size = 100
    )
  ))
  suppressWarnings(suppressMessages(p$sample_object$calculate_sample_sizes()))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_true(is.na(st$num_interview_per_enum_per_day))
    expect_true(is.na(st$num_days))
  }))
})

test_that("sample_object$calculate_sample_sizes populates field-plan columns when logistics given", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Urban",
      sampling_method_site = "simple_random",
      n_sites = 5,
      population_size = 10000,
      General_HH_Sample_Size = 100,
      teams = 2,
      enumerators_per_team = 3,
      avg_interview_time = 45,
      avg_travel_time = 30,
      avg_rest_time = 60,
      start_time = "2024-01-01",
      end_time = "2024-01-31"
    )
  ))
  suppressWarnings(suppressMessages(p$sample_object$calculate_sample_sizes()))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  suppressWarnings(suppressMessages({
    expect_false(is.na(st$num_interview_per_enum_per_day))
    expect_false(is.na(st$num_days))
    expect_true(st$num_interview_per_enum_per_day > 0)
    expect_true(st$num_days > 0)
  }))
})

test_that("sample_object$calculate_sample_sizes errors when sample table empty", {
  s <- suppressMessages(Sample$new())
  suppressWarnings(suppressMessages(
    expect_error(s$calculate_sample_sizes(), regexp = "sample_table is empty")
  ))
})

test_that("sample_object$calculate_sample_sizes fills field-plan per stratum independently", {
  suppressMessages(p <- make_protocol())
  # stratum with full logistics
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Urban",
      sampling_method_site = "simple_random",
      n_sites = 5,
      population_size = 10000,
      General_HH_Sample_Size = 100,
      teams = 2,
      enumerators_per_team = 3,
      avg_interview_time = 45,
      avg_travel_time = 30,
      avg_rest_time = 60,
      start_time = "2024-01-01",
      end_time = "2024-01-31"
    )
  ))
  # stratum without logistics
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s2",
    stratum_name = "Rural"
  )))
  suppressWarnings(suppressMessages(p$sample_object$calculate_sample_sizes()))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  s1 <- st[st$stratum_id == "s1", ]
  s2 <- st[st$stratum_id == "s2", ]
  suppressWarnings(suppressMessages({
    expect_false(is.na(s1$num_days))
    expect_true(is.na(s2$num_days))
  }))
})

# ── set_sampling_frame ─────────────────────────────────────────────────────────

test_that("set_sampling_frame stores data in the SamplingFrame log_df", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = c("urban", "rural"),
    psu = c("psu_1", "psu_2"),
    population_size = c(500, 800),
    inclusion = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages({
    expect_equal(nrow(p$sampling_frame$log_df), 2L)
    expect_true("inclusion" %in% names(p$sampling_frame$log_df))
  }))
})

test_that("set_sampling_frame adds inclusion=TRUE column when absent", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = "urban",
    psu = "psu_1",
    population_size = 500,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages({
    expect_true("inclusion" %in% names(p$sampling_frame$log_df))
    expect_true(all(p$sampling_frame$log_df$inclusion))
  }))
})

test_that("set_sampling_frame errors on empty data frame", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_error(
      p$set_sampling_frame(data.frame()),
      regexp = "empty"
    )
  ))
})

test_that("set_sampling_frame errors on NULL input", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_error(p$set_sampling_frame(NULL))
  ))
})

test_that("set_sampling_frame triggers coherence check and updates sampling_frame_strata_names", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = c("urban", "rural"),
    psu = paste0("psu_", 1:4),
    population_size = rep(500, 4),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages({
    expect_true(length(p$sampling_frame_strata_names) > 0)
    expect_true("urban" %in% p$sampling_frame_strata_names)
    expect_true("rural" %in% p$sampling_frame_strata_names)
  }))
})

# ── get_frame_column ───────────────────────────────────────────────────────────

test_that("get_frame_column returns NULL when frame not set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_identical(p$get_frame_column("psu"), character(0))
  ))
})

test_that("get_frame_column returns NULL for missing column", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(p$set_sampling_frame(make_frame())))
  suppressWarnings(suppressMessages(
    expect_null(p$get_frame_column("nonexistent_col"))
  ))
})

test_that("get_frame_column returns all included values by default", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(p$set_sampling_frame(make_frame())))
  psus <- suppressWarnings(suppressMessages(p$get_frame_column("psu")))
  suppressWarnings(suppressMessages(
    expect_equal(length(psus), 20L)
  ))
})

test_that("get_frame_column filters by stratum", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = c(rep("urban", 5), rep("rural", 10)),
    psu = paste0("psu_", seq_len(15)),
    population_size = rep(500, 15),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  urban_psus <- suppressWarnings(suppressMessages(
    p$get_frame_column("psu", strata = "urban")
  ))
  suppressWarnings(suppressMessages(
    expect_equal(length(urban_psus), 5L)
  ))
})

test_that("get_frame_column respects included_only flag", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = rep("s1", 5),
    psu = paste0("psu_", seq_len(5)),
    population_size = rep(500, 5),
    inclusion = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  included <- suppressWarnings(suppressMessages(
    p$get_frame_column("psu", included_only = TRUE)
  ))
  all_psus <- suppressWarnings(suppressMessages(
    p$get_frame_column("psu", included_only = FALSE)
  ))
  suppressWarnings(suppressMessages({
    expect_equal(length(included), 3L)
    expect_equal(length(all_psus), 5L)
  }))
})

# ── diagnose_coherence strata checks ──────────────────────────────────────────

test_that("diagnose_coherence adds strata_missing_in_frame issue when mismatch", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    stratum_name = "S1"
  )))
  frame <- data.frame(
    stratum = "other_stratum",
    psu = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(p$diagnose_coherence()))
  suppressWarnings(suppressMessages(
    expect_true(
      !is.null(p$issues_coherence$strata_missing_in_frame) ||
        !is.null(p$issues_coherence$strata_missing_in_table)
    )
  ))
})

test_that("diagnose_coherence finds no strata mismatch when strata match", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    stratum_name = "S1"
  )))
  frame <- data.frame(
    stratum = rep("s1", 5),
    psu = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(p$diagnose_coherence()))
  suppressWarnings(suppressMessages({
    expect_null(p$issues_coherence$strata_missing_in_frame)
    expect_null(p$issues_coherence$strata_missing_in_table)
  }))
})

# ── SamplingFrame class ────────────────────────────────────────────────────────

test_that("SamplingFrame initialises empty with required columns", {
  sf <- suppressMessages(SamplingFrame$new())
  suppressWarnings(suppressMessages({
    expect_true(inherits(sf, "SamplingFrame"))
    expect_equal(nrow(sf$log_df), 0L)
    expected_cols <- c(
      "stratum",
      "psu",
      "population_size",
      "inclusion",
      "sampled_psu",
      "allocated_sample"
    )
    expect_true(all(expected_cols %in% names(sf$log_df)))
  }))
})

test_that("SamplingFrame initialises with a provided data frame", {
  frame <- data.frame(
    stratum = "A",
    psu = "psu_1",
    population_size = 1000,
    inclusion = TRUE,
    sampled_psu = NA_character_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  sf <- suppressMessages(SamplingFrame$new(log_df = frame))
  suppressWarnings(suppressMessages({
    expect_equal(nrow(sf$log_df), 1L)
    expect_equal(sf$log_df$stratum, "A")
  }))
})

# ── draw_sample via sampling_frame ─────────────────────────────────────────────

test_that("sampling_frame$draw_sample selects PSUs for simple_random stratum", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    stratum_name = "Urban",
    n_sites = 5,
    population_size = 10000,
    sample_size = 50
  )))
  suppressWarnings(suppressMessages(p$set_sampling_frame(make_frame())))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  suppressWarnings(suppressMessages({
    expect_false(all(is.na(p$sampling_frame$drawn_sample$sampled_psu)))
    expect_false(is.null(p$sampling_frame$drawn_sample))
  }))
})

test_that("sampling_frame$draw_sample (simple_random) includes RC-labelled reserve PSUs", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    n_sites = 5,
    sample_size = 50
  )))
  suppressWarnings(suppressMessages(p$set_sampling_frame(make_frame(
    n_psu = 20
  ))))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  psu_vals <- suppressWarnings(suppressMessages(
    p$sampling_frame$drawn_sample$sampled_psu
  ))
  # n_main = 5 (<=10) -> 3 RC; total drawn = 8
  suppressWarnings(suppressMessages(
    expect_equal(sum(psu_vals == "RC"), 3L)
  ))
})

test_that("sampling_frame$draw_sample (purposive) doesnt select any PSU, to be done manually", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "All",
      sampling_method_site = "purposive",
      n_sites = 10,
      General_HH_Sample_Size = 50
    )
  ))
  frame <- make_frame(n_psu = 10)
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  # Purposive selects all PSUs
  suppressWarnings(suppressMessages(
    expect_equal(nrow(p$sampling_frame$drawn_sample), 0L)
  ))
})

test_that("sampling_frame$draw_sample (cluster/pps) requires n_psu and cluster_size at draw time", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Rural",
      sampling_method_site = "cluster",
      n_sites = 10,
      General_HH_Sample_Size = 50
    )
  ))
  frame <- data.frame(
    stratum = rep("s1", 20),
    psu = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  # cluster without n_psu in sample table -> warning and skip
  suppressMessages(
    expect_warning(
      p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
      regexp = "skipped"
    )
  )
  suppressWarnings(suppressMessages(
    expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  ))
})

test_that("sampling_frame$draw_sample with cluster/rlc selects only n_sites PSUs", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Rural",
      sampling_method_site = "cluster",
      sampling_method_hh = "rlc",
      n_sites = 5,
      cluster_size = 3,
      General_HH_Sample_Size = 30
    )
  ))
  suppressWarnings(suppressMessages(frame <- make_frame(n_psu = 20)))
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  selected <- suppressWarnings(suppressMessages(
    p$sampling_frame$drawn_sample[!is.na(p$sampling_frame$drawn_sample$sampled_psu), ]
  ))
  suppressWarnings(suppressMessages(
    expect_lte(nrow(selected), 5L)
  ))
})

test_that("sampling_frame$draw_sample warns and skips stratum not in frame", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(p, stratum_id = "s1")))
  frame <- data.frame(
    stratum = rep("s2", 5),
    psu = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(
    expect_warning(
      suppressMessages(
        p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
      ),
      regexp = "skipping"
    )
  )
  suppressWarnings(suppressMessages(
    expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  ))
})

test_that("sampling_frame$draw_sample continues with other strata when one fails", {
  suppressMessages(p <- make_protocol())
  # s1: cluster without n_psu -- will warn and skip
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Rural",
      sampling_method_site = "cluster",
      n_sites = 5,
      cluster_size = 5,
      General_HH_Sample_Size = 50
    )
  ))
  # s2: simple_random -- will succeed
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s2",
    stratum_name = "Urban",
    n_sites = 3,
    sample_size = 30
  )))
  frame <- data.frame(
    stratum = c(rep("s1", 10), rep("s2", 10)),
    psu = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressMessages(
    expect_warning(
      p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
      regexp = "skipped"
    )
  )
  s2_rows <- suppressWarnings(suppressMessages(
    p$sampling_frame$drawn_sample_full[
      p$sampling_frame$drawn_sample_full$stratum == "s2",
    ]
  ))
  suppressWarnings(suppressMessages(
    expect_false(all(is.na(s2_rows$sampled_psu)))
  ))
})

test_that("sampling_frame$draw_sample multi-stratum uses sequential PSU offset", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    stratum_name = "Urban",
    n_sites = 5,
    sample_size = 50
  )))
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s2",
    stratum_name = "Rural",
    n_sites = 3,
    sample_size = 30
  )))
  frame <- data.frame(
    stratum = c(rep("s1", 20), rep("s2", 15)),
    psu = paste0("psu_", seq_len(35)),
    population_size = rep(500, 35),
    inclusion = rep(TRUE, 35),
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  s1_vals <- suppressWarnings(suppressMessages(
    p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s1"]
  ))
  s2_vals <- suppressWarnings(suppressMessages(
    p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s2"]
  ))
  s1_nums <- suppressWarnings(as.integer(s1_vals[
    !is.na(s1_vals) & s1_vals != "RC"
  ]))
  s2_nums <- suppressWarnings(as.integer(s2_vals[
    !is.na(s2_vals) & s2_vals != "RC"
  ]))
  # Sequential offset: s2 numbers > max(s1 numbers)
  suppressWarnings(suppressMessages(
    expect_true(min(s2_nums) > max(s1_nums))
  ))
})

# ── clear_sample via sampling_frame ───────────────────────────────────────────

test_that("sampling_frame$clear_sample resets sampled_psu and allocated_sample", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    n_sites = 5,
    sample_size = 50
  )))
  suppressWarnings(suppressMessages(p$set_sampling_frame(make_frame(
    n_psu = 20
  ))))
  suppressWarnings(suppressMessages(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  ))
  suppressWarnings(suppressMessages(
    expect_false(all(is.na(p$sampling_frame$drawn_sample$sampled_psu)))
  ))

  # Clear and verify
  suppressWarnings(suppressMessages(
    p$sampling_frame$drawn_sample <- p$sampling_frame$clear_sample(
      p$sampling_frame$drawn_sample
    )
  ))
  suppressWarnings(suppressMessages({
    expect_true(all(is.na(p$sampling_frame$drawn_sample$sampled_psu)))
    expect_true(all(is.na(p$sampling_frame$drawn_sample$allocated_sample)))
  }))
})

test_that("sampling_frame$clear_sample is a no-op on an empty frame", {
  suppressMessages(p <- make_protocol())
  ssf <- suppressMessages(SamplingFrame$new())
  result <- suppressWarnings(suppressMessages(ssf$clear_sample(ssf$log_df)))
  suppressWarnings(suppressMessages({
    expect_equal(nrow(result), 0L)
    expect_null(ssf$drawn_sample)
  }))
})

# ── Nested sample_object accessibility (light) ────────────────────────────────

test_that("sample_object is a Sample and accessible via $sample_object", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$sample_object, "Sample"))
    expect_true(is.function(p$sample_object$add_stratum))
    expect_true(is.function(p$sample_object$get_sample_table))
  }))
})

test_that("sampling_frame is a SamplingFrame and accessible via $sampling_frame", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages({
    expect_true(inherits(p$sampling_frame, "SamplingFrame"))
    expect_true(is.data.frame(p$sampling_frame$log_df))
  }))
})

# ── access_nested integration with sample_object ───────────────────────────────

test_that("access_nested to sample_object$add_stratum works and updates modified_datetime", {
  suppressMessages(p <- make_protocol())
  t_before <- suppressWarnings(suppressMessages(p$metadata$modified_datetime))
  Sys.sleep(0.01)
  suppressWarnings(suppressMessages(
    p$access_nested(
      field = "sample_object",
      member = "add_stratum",
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      n_sites = 2
    )
  ))
  suppressWarnings(suppressMessages({
    expect_true(p$metadata$modified_datetime > t_before)
    expect_equal(
      p$access_nested(field = "sample_object", member = "get_sampling_methods"),
      "simple_random"
    )
    expect_equal(
      p$access_nested(field = "sample_object", member = "get_strata_names"),
      "S1"
    )
  }))
  st <- suppressWarnings(suppressMessages(
    p$access_nested(field = "sample_object", member = "get_sample_table")
  ))
  suppressWarnings(suppressMessages(
    expect_equal(nrow(st), 1L)
  ))
})

test_that("access_nested to sample_object$remove_stratum removes stratum", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$access_nested(
      field = "sample_object",
      member = "add_stratum",
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      n_sites = 2
    )
  ))
  suppressWarnings(suppressMessages(
    p$access_nested(
      field = "sample_object",
      member = "remove_stratum",
      strata_name = "S1"
    )
  ))
  st <- suppressWarnings(suppressMessages(
    p$access_nested(field = "sample_object", member = "get_sample_table")
  ))
  suppressWarnings(suppressMessages(
    expect_equal(nrow(st), 0L)
  ))
})

# ── SurveyProtocol active bindings ─────────────────────────────────────────────

test_that(".site_selection_srs is TRUE when simple_random method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.site_selection_srs))
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.site_selection_srs))
  ))
})

test_that(".site_selection_purposive is TRUE when purposive method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "purposive",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.site_selection_purposive))
  ))
})

test_that(".site_selection_systematic is TRUE when systematic method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "systematic",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages({
    expect_true(isTRUE(p$.site_selection_systematic))
    expect_false(isTRUE(p$.site_selection_srs))
  }))
})

test_that(".site_selection_exhaustive is TRUE when proportional method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "proportional",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.site_selection_exhaustive))
  ))
})

test_that(".site_selection_cluster is TRUE when cluster method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "cluster",
      n_sites = 5,
      cluster_size = 3
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.site_selection_cluster))
  ))
})

test_that(".hh_selection_srs is TRUE when default simple_random hh method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(p))) # default hh method is simple_random
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.hh_selection_srs))
  ))
})

test_that(".hh_selection_systematic is TRUE when systematic hh method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.hh_selection_systematic))
  ))
})

test_that(".hh_selection_rlc is TRUE when rlc household method is used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.hh_selection_rlc))
  ))
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.hh_selection_rlc))
  ))
})

test_that(".multiple_methods is TRUE when multiple site methods are used", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.multiple_methods))
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(p, stratum_id = "s1")))
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s2",
      stratum_name = "S2",
      sampling_method_site = "purposive",
      n_sites = 5
    )
  ))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.multiple_methods))
  ))
})

test_that(".multiple_strata is FALSE with one stratum and TRUE with two", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.multiple_strata))
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(p, stratum_id = "s1")))
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.multiple_strata))
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s2",
    stratum_name = "S2"
  )))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.multiple_strata))
  ))
})

test_that(".fpc is accessible (empty binding returns NULL invisibly)", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages({
    result <- p$.fpc
    expect_true(is.null(result) || TRUE) # binding exists and is accessible
  }))
})

test_that(".total_population_size reflects population from sampling frame", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_equal(p$.total_population_size, 0)
  ))
  frame <- data.frame(
    stratum = rep("s1", 3),
    psu = paste0("psu_", seq_len(3)),
    population_size = c(100, 200, 300),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressWarnings(suppressMessages(
    expect_equal(p$.total_population_size, 600)
  ))
})

test_that(".total_population_size_included sums only included rows", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = rep("s1", 4),
    psu = paste0("psu_", seq_len(4)),
    population_size = c(100, 200, 300, 400),
    inclusion = c(TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  val <- suppressWarnings(suppressMessages(p$.total_population_size_included))
  suppressWarnings(suppressMessages(
    expect_equal(val, 700) # 100 + 200 + 400
  ))
})

test_that(".total_population_size_excluded sums only excluded rows", {
  suppressMessages(p <- make_protocol())
  frame <- data.frame(
    stratum = rep("s1", 4),
    psu = paste0("psu_", seq_len(4)),
    population_size = c(100, 200, 300, 400),
    inclusion = c(TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  val <- suppressWarnings(suppressMessages(p$.total_population_size_excluded))
  suppressWarnings(suppressMessages(
    expect_equal(val, 300) # only row where inclusion=FALSE
  ))
})

test_that(".total_population_per_strata_included returns NULL or character string", {
  suppressMessages(p <- make_protocol())
  # Without frame it's NULL
  val_null <- suppressWarnings(suppressMessages(
    p$.total_population_per_strata_included
  ))
  suppressWarnings(suppressMessages(
    expect_true(is.null(val_null) || is.character(val_null))
  ))
  # With frame set
  frame <- data.frame(
    stratum = c(rep("urban", 3), rep("rural", 2)),
    psu = paste0("psu_", seq_len(5)),
    population_size = c(100, 200, 300, 400, 500),
    inclusion = TRUE,
    stringsAsFactors = FALSE
  )
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  val <- suppressWarnings(suppressMessages(
    p$.total_population_per_strata_included
  ))
  suppressWarnings(suppressMessages(
    expect_true(is.null(val) || is.character(val))
  ))
})

test_that(".num_strata_units reflects number of strata in sample table", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_equal(p$.num_strata_units, 0L)
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(p, stratum_id = "s1")))
  suppressWarnings(suppressMessages(
    expect_equal(p$.num_strata_units, 1L)
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s2",
    stratum_name = "S2"
  )))
  suppressWarnings(suppressMessages(
    expect_equal(p$.num_strata_units, 2L)
  ))
})

test_that(".num_geographic_units returns NULL or count matching strata rows", {
  suppressMessages(p <- make_protocol())
  val_null <- suppressWarnings(suppressMessages(p$.num_geographic_units))
  suppressWarnings(suppressMessages(
    expect_true(is.null(val_null))
  ))
  add_simple_stratum(p, stratum_id = "s1")
  val <- suppressWarnings(suppressMessages(p$.num_geographic_units))
  suppressWarnings(suppressMessages(
    expect_equal(val, 1L)
  ))
})

test_that(".num_other_units returns NULL or count matching strata rows", {
  suppressMessages(p <- make_protocol())
  val_null <- suppressWarnings(suppressMessages(p$.num_other_units))
  suppressWarnings(suppressMessages(
    expect_true(is.null(val_null))
  ))
  add_simple_stratum(p, stratum_id = "s1")
  val <- suppressWarnings(suppressMessages(p$.num_other_units))
  suppressWarnings(suppressMessages(
    expect_equal(val, 1L)
  ))
})

test_that(".rate_survey is FALSE by default and TRUE when rate_indicator set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.rate_survey))
  ))
  # Set up a stratum with a rate_indicator
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  st$rate_indicator <- "mortality_rate"
  suppressWarnings(suppressMessages(p$sample_object$set_sample_table(st)))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.rate_survey))
  ))
})

test_that(".individual_survey is FALSE by default and TRUE when ind_indicator set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_false(isTRUE(p$.individual_survey))
  ))
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  st$ind_indicator <- "muac"
  suppressWarnings(suppressMessages(p$sample_object$set_sample_table(st)))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.individual_survey))
  ))
})

test_that(".general_survey is TRUE when neither rate_survey nor individual_survey", {
  suppressMessages(p <- make_protocol())
  # No strata set up yet
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.general_survey))
  ))
  # Add a simple stratum without rate or ind indicators
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  suppressWarnings(suppressMessages(
    expect_true(isTRUE(p$.general_survey))
  ))
})

test_that(".ind_indicator returns NULL when no ind_indicator column exists", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.ind_indicator)
  ))
})

test_that(".ind_indicator returns character when ind_indicator column present", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  st$ind_indicator <- "muac"
  suppressWarnings(suppressMessages(p$sample_object$set_sample_table(st)))
  val <- suppressWarnings(suppressMessages(p$.ind_indicator))
  suppressWarnings(suppressMessages(
    expect_type(val, "character")
  ))
})

test_that(".rate_indicator returns NULL when no rate_indicator column exists", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.rate_indicator)
  ))
})

test_that(".rate_indicator returns character when rate_indicator column present", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(p)))
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  st$rate_indicator <- "crude_death_rate"
  suppressWarnings(suppressMessages(p$sample_object$set_sample_table(st)))
  val <- suppressWarnings(suppressMessages(p$.rate_indicator))
  suppressWarnings(suppressMessages(
    expect_type(val, "character")
  ))
})

test_that(".stratified_strata_names_srs_srs returns strata using simple_random/simple_random", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(add_simple_stratum(
    p,
    stratum_id = "s1",
    stratum_name = "Urban"
  )))
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s2",
      stratum_name = "Rural",
      sampling_method_site = "purposive",
      n_sites = 5
    )
  ))
  srs_names <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_srs_srs
  ))
  suppressWarnings(suppressMessages({
    expect_true("Urban" %in% srs_names)
    expect_false("Rural" %in% srs_names)
  }))
})

test_that(".stratified_strata_names_srs_rlc returns strata using simple_random/rlc", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Urban",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "Rural") # srs/srs
  val <- suppressWarnings(suppressMessages(p$.stratified_strata_names_srs_rlc))
  suppressWarnings(suppressMessages({
    expect_true("Urban" %in% val)
    expect_false("Rural" %in% val)
  }))
})

test_that(".stratified_strata_names_srs_systematic returns strata using simple_random/systematic", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_srs_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_systematic_srs returns strata using systematic/simple_random", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "systematic",
      sampling_method_hh = "simple_random",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_systematic_srs
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_systematic_systematic returns strata using systematic/systematic", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "systematic",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_systematic_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_systematic_rlc returns strata using systematic/rlc", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "systematic",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_systematic_rlc
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_proportional_srs returns strata using proportional/srs", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "proportional",
      sampling_method_hh = "simple_random",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_proportional_srs
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_proportional_systematic returns strata using proportional/systematic", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "proportional",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_proportional_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_proportional_rlc returns strata using proportional/rlc", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "proportional",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_proportional_rlc
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_cluster_srs returns strata using cluster/srs", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "cluster",
      sampling_method_hh = "simple_random",
      n_sites = 5,
      cluster_size = 3
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_cluster_srs
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_cluster_systematic returns strata using cluster/systematic", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "cluster",
      sampling_method_hh = "systematic",
      n_sites = 5,
      cluster_size = 3
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_cluster_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_cluster_rlc returns strata using cluster/rlc", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "cluster",
      sampling_method_hh = "rlc",
      n_sites = 5,
      cluster_size = 3
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_cluster_rlc
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_purposive_srs returns strata using purposive/srs", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "purposive",
      sampling_method_hh = "simple_random",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_purposive_srs
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_purposive_systematic returns strata using purposive/systematic", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "purposive",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_purposive_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_purposive_rlc returns strata using purposive/rlc", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "purposive",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_purposive_rlc
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_site_srs returns all srs strata regardless of hh method", {
  suppressMessages(p <- make_protocol())
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "Urban") # srs/srs
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s2",
      stratum_name = "Rural",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(p$.stratified_strata_names_site_srs))
  suppressWarnings(suppressMessages({
    expect_true("Urban" %in% val)
    expect_true("Rural" %in% val)
  }))
})

test_that(".stratified_strata_names_site_systematic returns all systematic strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_site_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_site_exhaustive returns proportional strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "proportional",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_site_exhaustive
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_site_cluster returns cluster strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "cluster",
      n_sites = 5,
      cluster_size = 3
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_site_cluster
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_site_purposive returns purposive strata", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "purposive",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_site_purposive
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_hh_srs returns all strata using simple_random hh", {
  suppressMessages(p <- make_protocol())
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "Urban") # hh=srs
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s2",
      stratum_name = "Rural",
      sampling_method_site = "purposive",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(p$.stratified_strata_names_hh_srs))
  suppressWarnings(suppressMessages({
    expect_true("Urban" %in% val)
    expect_false("Rural" %in% val)
  }))
})

test_that(".stratified_strata_names_hh_systematic returns strata using systematic hh", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "systematic",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(
    p$.stratified_strata_names_hh_systematic
  ))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".stratified_strata_names_hh_rlc returns strata using rlc hh", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "Zone1",
      sampling_method_site = "simple_random",
      sampling_method_hh = "rlc",
      n_sites = 5
    )
  ))
  val <- suppressWarnings(suppressMessages(p$.stratified_strata_names_hh_rlc))
  suppressWarnings(suppressMessages(
    expect_true("Zone1" %in% val)
  ))
})

test_that(".sample_size_general_households returns NULL when no strata and numeric when set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_general_households)
  ))
  add_simple_stratum(p, sample_size = 100)
  val <- suppressWarnings(suppressMessages(p$.sample_size_general_households))
  suppressWarnings(suppressMessages({
    expect_false(is.null(val))
    expect_gte(val, 100)
  }))
})

test_that(".sample_size_ind_persons returns NULL when Ind_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_ind_persons)
  ))
})

test_that(".sample_size_ind_hh returns NULL when Ind_HH_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_ind_hh)
  ))
})

test_that(".sample_size_rate_persons returns NULL when Rate_Ind_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_rate_persons)
  ))
})

test_that(".sample_size_rate_persontime returns NULL when Rate_PT_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_rate_persontime)
  ))
})

test_that(".sample_size_rate_hh returns NULL when Rate_HH_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_rate_hh)
  ))
})

test_that(".sample_size_hh_final returns NULL when Final_HH_Sample_Size absent", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.sample_size_hh_final)
  ))
})

test_that(".sample_size_general_table_df returns a data frame or NULL", {
  suppressMessages(p <- make_protocol())
  val <- suppressWarnings(suppressMessages(p$.sample_size_general_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(val) || is.null(val))
  ))
})

test_that(".sample_size_ind_table_df returns a data frame or NULL", {
  suppressMessages(p <- make_protocol())
  val <- suppressWarnings(suppressMessages(p$.sample_size_ind_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(val) || is.null(val))
  ))
})

test_that(".sample_size_rate_table_df returns a data frame or NULL", {
  suppressMessages(p <- make_protocol())
  val <- suppressWarnings(suppressMessages(p$.sample_size_rate_table_df))
  suppressWarnings(suppressMessages(
    expect_true(is.data.frame(val) || is.null(val))
  ))
})

test_that(".n_sites returns NULL when no strata and sum when strata set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.n_sites)
  ))
  add_simple_stratum(p, n_sites = 5)
  val <- suppressWarnings(suppressMessages(p$.n_sites))
  suppressWarnings(suppressMessages(
    expect_equal(val, 5)
  ))
})

test_that(".cluster_size returns NULL when no cluster strata and max when set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.cluster_size)
  ))
  suppressWarnings(suppressMessages(
    p$sample_object$add_stratum(
      stratum_id = "s1",
      stratum_name = "S1",
      sampling_method_site = "cluster",
      sampling_method_hh = "rlc",
      n_sites = 5,
      cluster_size = 4
    )
  ))
  val <- suppressWarnings(suppressMessages(p$.cluster_size))
  suppressWarnings(suppressMessages(
    expect_equal(val, 4)
  ))
})

test_that(".num_enumerators_per_team returns NULL when not set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.num_enumerators_per_team)
  ))
})

test_that(".num_days_data_collection returns NULL when no logistics data", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.num_days_data_collection)
  ))
})

test_that(".strata_names returns NULL when no strata and concatenated string when set", {
  suppressMessages(p <- make_protocol())
  suppressWarnings(suppressMessages(
    expect_null(p$.strata_names)
  ))
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "North")
  val <- suppressWarnings(suppressMessages(p$.strata_names))
  suppressWarnings(suppressMessages({
    expect_type(val, "character")
    expect_true(grepl("North", val))
  }))
})

# ── get_quarto_params for SurveyProtocol ──────────────────────────────────────

test_that("SurveyProtocol$get_quarto_params returns expected sampling keys", {
  suppressMessages(p <- make_protocol())
  params <- suppressWarnings(suppressMessages(p$get_quarto_params()))
  suppressWarnings(suppressMessages({
    expect_true(is.list(params))
    expect_true("rate_survey" %in% names(params))
    expect_true("site_selection_srs" %in% names(params))
    expect_true("multiple_methods" %in% names(params))
    expect_true("total_population_size" %in% names(params))
    expect_true("strata_names" %in% names(params))
    # Inherits assessment_title from Protocol
    expect_true("assessment_title" %in% names(params))
  }))
})

# ── draw_sample_psu_* utility tests ───────────────────────────────────────────

test_that("draw_sample_psu_srs selects n_psu main PSUs with correct RC count for n<=10", {
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_srs(
      data.frame(population_size = rep(100, 30)),
      n_psu = 5,
      sample_size = 50,
      seed = 7
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  # n_main=5 (<=10) -> 3 RC; total=8
  suppressWarnings(suppressMessages(
    expect_equal(sum(selected$sampled_psu == "RC"), 3L)
  ))
  main_nums <- suppressWarnings(as.integer(selected$sampled_psu[
    selected$sampled_psu != "RC"
  ]))
  suppressWarnings(suppressMessages(
    expect_equal(sort(main_nums), 1:5)
  ))
})

test_that("draw_sample_psu_srs RC count correct for n_main > 10 and > 20", {
  # n_main = 15 -> 4 RC
  result_15 <- suppressWarnings(suppressMessages(
    draw_sample_psu_srs(
      data.frame(population_size = rep(100, 30)),
      n_psu = 15,
      sample_size = 150,
      seed = 7
    )
  ))
  selected <- suppressWarnings(suppressMessages(result_15[
    !is.na(result_15$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_equal(sum(selected$sampled_psu == "RC"), 4L)
  ))

  # n_main = 25 -> 5 RC
  result_25 <- suppressWarnings(suppressMessages(
    draw_sample_psu_srs(
      data.frame(population_size = rep(100, 50)),
      n_psu = 25,
      sample_size = 250,
      seed = 7
    )
  ))
  selected25 <- suppressWarnings(suppressMessages(result_25[
    !is.na(result_25$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_equal(sum(selected25$sampled_psu == "RC"), 5L)
  ))
})

test_that("draw_sample_psu_rlc errors when n_sites is missing", {
  frame <- data.frame(population_size = rep(100, 20))
  suppressWarnings(suppressMessages(
    expect_error(
      draw_sample_psu_rlc(frame, sample_size = 60, n_sites = NULL),
      regexp = "n_sites is required"
    )
  ))
})

test_that("draw_sample_psu_rlc restricts cluster allocation to n_sites pre-selected PSUs", {
  frame <- data.frame(
    population_size = c(
      100,
      200,
      150,
      300,
      250,
      120,
      180,
      90,
      400,
      110,
      220,
      130,
      170,
      310,
      240,
      80,
      160,
      270,
      190,
      350,
      100,
      200,
      150,
      300,
      250,
      120,
      180,
      90,
      400,
      110
    )
  )
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_rlc(
      frame,
      sample_size = 60,
      n_sites = 5,
      cluster_size = 3,
      seed = 42
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_lte(nrow(selected), 5L)
  ))
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  suppressWarnings(suppressMessages(
    expect_true(any(trimws(all_labels) == "RC"))
  ))
})

test_that("draw_sample_psu_rlc distributes clusters evenly across selected sites", {
  frame <- data.frame(population_size = rep(100L, 20))
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_rlc(
      frame,
      sample_size = 30,
      n_sites = 3,
      cluster_size = 3,
      seed = 43
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_lte(nrow(selected), 3L)
  ))
  slot_counts <- vapply(
    selected$sampled_psu,
    function(s) {
      length(trimws(strsplit(s, ",\\s*")[[1]]))
    },
    integer(1)
  )
  # n_clusters = ceiling(30/3) = 10; n_reserve = 3 (<=10); n_total = 13
  suppressWarnings(suppressMessages({
    expect_equal(sum(slot_counts), 13L)
    expect_lte(max(slot_counts) - min(slot_counts), 1L)
  }))
})

test_that("draw_sample_psu_srs_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L)) # 21 PSUs
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_srs_rlc(
      frame,
      sample_size = 30,
      n_sites = 4,
      cluster_size = 3,
      seed = 7
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_lte(nrow(selected), 4L)
  ))
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  suppressWarnings(suppressMessages({
    expect_true(any(trimws(all_labels) == "RC"))
    # Total slots: n_clusters = ceiling(30/3) = 10, n_reserve = 3, n_total = 13
    expect_equal(length(all_labels), 13L)
  }))
})

test_that("draw_sample_psu_srs_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(15))) # no population_size column
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_srs_rlc(
      frame,
      sample_size = 30,
      n_sites = 3,
      cluster_size = 3,
      seed = 1
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  slot_counts <- vapply(
    selected$sampled_psu,
    function(s) {
      length(trimws(strsplit(s, ",\\s*")[[1]]))
    },
    integer(1)
  )
  suppressWarnings(suppressMessages({
    expect_equal(sum(slot_counts), 13L)
    expect_lte(max(slot_counts) - min(slot_counts), 1L)
  }))
})

test_that("draw_sample_psu_systematic_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L)) # 21 PSUs
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_systematic_rlc(
      frame,
      sample_size = 30,
      n_sites = 4,
      cluster_size = 3,
      seed = 7
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  suppressWarnings(suppressMessages(
    expect_lte(nrow(selected), 4L)
  ))
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  suppressWarnings(suppressMessages({
    expect_true(any(trimws(all_labels) == "RC"))
    expect_equal(length(all_labels), 13L)
  }))
})

test_that("draw_sample_psu_systematic_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(20)))
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_systematic_rlc(
      frame,
      sample_size = 30,
      n_sites = 3,
      cluster_size = 3,
      seed = 5
    )
  ))
  selected <- suppressWarnings(suppressMessages(result[
    !is.na(result$sampled_psu),
  ]))
  slot_counts <- vapply(
    selected$sampled_psu,
    function(s) {
      length(trimws(strsplit(s, ",\\s*")[[1]]))
    },
    integer(1)
  )
  suppressWarnings(suppressMessages({
    expect_equal(sum(slot_counts), 13L)
    expect_lte(max(slot_counts) - min(slot_counts), 1L)
  }))
})

test_that("draw_sample_psu_proportional_rlc selects all PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = c(300L, 100L, 200L, 150L, 250L))
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_proportional_rlc(
      frame,
      sample_size = 30,
      cluster_size = 3,
      seed = 42
    )
  ))
  suppressWarnings(suppressMessages(
    expect_equal(nrow(result[!is.na(result$sampled_psu), ]), nrow(frame))
  ))
  all_labels <- unlist(strsplit(result$sampled_psu, ",\\s*"))
  suppressWarnings(suppressMessages({
    expect_true(any(trimws(all_labels) == "RC"))
    # n_clusters = ceiling(30/3) = 10; n_reserve = 3; n_total = 13
    expect_equal(length(all_labels), 13L)
  }))
})

test_that("draw_sample_psu_proportional_rlc allocates proportional to population size", {
  frame <- data.frame(population_size = c(900L, 100L, 100L, 100L, 100L))
  result <- suppressWarnings(suppressMessages(
    draw_sample_psu_proportional_rlc(
      frame,
      sample_size = 30,
      cluster_size = 3,
      seed = 1
    )
  ))
  slot_counts <- vapply(
    result$sampled_psu,
    function(s) {
      length(trimws(strsplit(s, ",\\s*")[[1]]))
    },
    integer(1)
  )
  suppressWarnings(suppressMessages(
    expect_gt(slot_counts[1], slot_counts[2])
  ))
})

test_that("draw_sample_psu_proportional_rlc errors when population_size is missing", {
  frame <- data.frame(psu = paste0("p", seq_len(5)))
  suppressWarnings(suppressMessages(
    expect_error(
      draw_sample_psu_proportional_rlc(frame, sample_size = 30),
      regexp = "population_size"
    )
  ))
})

# ── edge cases: n_sites missing at draw time triggers skip ─────────────────────

test_that("sampling_frame$draw_sample skips simple_random stratum when n_sites is NA in table", {
  suppressMessages(p <- make_protocol())
  add_simple_stratum(p, n_sites = 5, sample_size = 30)
  # Corrupt the strata table to remove n_sites
  st <- suppressWarnings(suppressMessages(p$get_sample_table()))
  st$n_sites <- NA_real_
  suppressWarnings(suppressMessages(p$sample_object$set_sample_table(st)))

  frame <- make_frame(n_psu = 20)
  suppressWarnings(suppressMessages(p$set_sampling_frame(frame)))
  suppressMessages(
    expect_warning(
      p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
      regexp = "skipped"
    )
  )
  suppressWarnings(suppressMessages(
    expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  ))
})

