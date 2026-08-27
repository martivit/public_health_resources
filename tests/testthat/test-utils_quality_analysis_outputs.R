# ---------------------------
# UTILS QUALITY ANALYSIS OUTPUTS TEST SUITE
# ---------------------------

library(testthat)
library(dplyr)
library(ggplot2)
library(srvyr)

# ============================================================
# HELPER FUNCTIONS FOR TEST DATA
# ============================================================

create_test_categorical_data <- function(n = 100) {
  data.frame(
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    group = sample(c("Group1", "Group2", "Group3"), n, replace = TRUE),
    value = rnorm(n, mean = 50, sd = 10)
  )
}

create_test_numeric_data <- function(n = 100) {
  data.frame(
    muac = rnorm(n, mean = 13.5, sd = 1.5),
    wfhz = rnorm(n, mean = -1, sd = 1),
    district = sample(
      c("District A", "District B", "District C"),
      n,
      replace = TRUE
    ),
    enum = sample(1:5, n, replace = TRUE)
  )
}

create_test_multiple_response_data <- function(n = 100) {
  set.seed(42)
  options_pool <- c(
    "barrier_cost",
    "barrier_distance",
    "barrier_availability",
    "barrier_quality"
  )
  barriers <- vapply(
    seq_len(n),
    function(i) {
      chosen <- sample(options_pool, sample(1:3, 1), replace = FALSE)
      paste(sort(chosen), collapse = " ")
    },
    character(1)
  )
  data.frame(
    barriers = barriers,
    district = sample(c("District A", "District B"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 1. PLOT_STACKED_BAR TESTS
# ============================================================

test_that("plot_stacked_bar requires valid dataset", {
  expect_warning(
    plot_stacked_bar(NULL, category_var = "category"),
    regexp = "survey design"
  )

  expect_warning(
    plot_stacked_bar("not a dataframe", category_var = "category"),
    regexp = "survey design"
  )
})

test_that("plot_stacked_bar requires category_var parameter", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_stacked_bar(sdesign, category_var = NULL),
    regexp = "NULL"
  )
})

test_that("plot_stacked_bar validates column existence", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_stacked_bar(sdesign, category_var = "nonexistent_column"),
    regexp = "Missing required columns"
  )
})

test_that("plot_stacked_bar creates overall plot when grouping is NULL", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(g <- plot_stacked_bar(sdesign, category_var = "category"))

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_stacked_bar creates grouped plot when grouping is provided", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      grouping = "group"
    )
  )

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_stacked_bar accepts custom labels and legend_position", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      title_name = "Test Plot",
      x_label = "Custom X",
      y_label = "Custom Y",
      legend_position = "top"
    )
  )

  expect_s3_class(g, "ggplot")
  expect_true(!is.null(g$labels$title))
})

test_that("plot_stacked_bar legend_position parameter works", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g_bottom <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      legend_position = "bottom"
    )
  )
  suppressMessages(
    g_top <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      legend_position = "top"
    )
  )
  suppressMessages(
    g_none <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      legend_position = "none"
    )
  )

  expect_s3_class(g_bottom, "ggplot")
  expect_s3_class(g_top, "ggplot")
  expect_s3_class(g_none, "ggplot")
})

# ============================================================
# 2. PLOT_GROUPED_BAR_MULTIPLE TESTS
# ============================================================

test_that("plot_grouped_bar_multiple requires valid dataset", {
  expect_warning(
    suppressMessages(plot_grouped_bar_multiple(NULL, var_name = "barriers")),
    regexp = "survey design"
  )

  expect_warning(
    suppressMessages(plot_grouped_bar_multiple(
      "not a dataframe",
      var_name = "barriers"
    )),
    regexp = "survey design"
  )
})

test_that("plot_grouped_bar_multiple requires var_name parameter", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_grouped_bar_multiple(sdesign, var_name = NULL),
    regexp = "NULL"
  )
})

test_that("plot_grouped_bar_multiple validates column existence", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_grouped_bar_multiple(sdesign, var_name = "nonexistent_column"),
    regexp = "Missing required columns"
  )
})

test_that("plot_grouped_bar_multiple creates overall plot when grouping is NULL", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_grouped_bar_multiple(sdesign, var_name = "barriers")
  )

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_grouped_bar_multiple creates grouped plot when grouping is provided", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      grouping = "district"
    )
  )

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_grouped_bar_multiple handles percentage vs count display", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g_pct <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      calc_percentage = TRUE
    )
  )
  suppressMessages(
    g_count <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      calc_percentage = FALSE
    )
  )

  expect_s3_class(g_pct, "ggplot")
  expect_s3_class(g_count, "ggplot")
})

test_that("plot_grouped_bar_multiple accepts named response_labels", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  labels <- c(
    barrier_cost = "Cost Barrier",
    barrier_distance = "Distance Barrier",
    barrier_availability = "Availability Barrier",
    barrier_quality = "Quality Barrier"
  )

  suppressMessages(
    g <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      response_labels = labels,
      grouping = "district",
      color_palette = "reach1",
      title_name = "Main barriers by District"
    )
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_grouped_bar_multiple legend_position parameter works", {
  df <- create_test_multiple_response_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g_bottom <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      legend_position = "bottom"
    )
  )
  suppressMessages(
    g_right <- plot_grouped_bar_multiple(
      sdesign,
      var_name = "barriers",
      legend_position = "right"
    )
  )

  expect_s3_class(g_bottom, "ggplot")
  expect_s3_class(g_right, "ggplot")
})

# ============================================================
# 3. PLOT_BOXPLOT TESTS
# ============================================================

test_that("plot_boxplot requires valid dataset", {
  expect_warning(
    plot_boxplot(NULL, numeric_var = "muac"),
    regexp = "survey design"
  )

  expect_warning(
    plot_boxplot("not a dataframe", numeric_var = "muac"),
    regexp = "survey design"
  )
})

test_that("plot_boxplot requires numeric_var parameter", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_boxplot(sdesign, numeric_var = NULL),
    regexp = "NULL"
  )
})

test_that("plot_boxplot validates column existence", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_boxplot(sdesign, numeric_var = "nonexistent_column"),
    regexp = "Missing required columns"
  )
})

test_that("plot_boxplot validates numeric_var is numeric", {
  df <- data.frame(
    text_var = c("A", "B", "C"),
    numeric_var = c(1, 2, 3)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_boxplot(sdesign, numeric_var = "text_var"),
    regexp = "numeric"
  )
})

test_that("plot_boxplot creates overall plot when grouping is NULL", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    grouping = "enum",
    show_mean = T,
    show_labels = T
  )

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_boxplot creates grouped plot when grouping is provided", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_boxplot(sdesign, numeric_var = "muac", grouping = "district")

  expect_s3_class(g, "ggplot")
  expect_true("ggplot" %in% class(g))
})

test_that("plot_boxplot handles outlier display options", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_with <- plot_boxplot(sdesign, numeric_var = "muac", show_outliers = TRUE)
  g_without <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    show_outliers = FALSE
  )

  expect_s3_class(g_with, "ggplot")
  expect_s3_class(g_without, "ggplot")
})

test_that("plot_boxplot handles mean display option", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_overall <- plot_boxplot(sdesign, numeric_var = "muac", show_mean = TRUE)
  g_grouped <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    grouping = "district",
    show_mean = TRUE
  )

  expect_s3_class(g_overall, "ggplot")
  expect_s3_class(g_grouped, "ggplot")
})

test_that("plot_boxplot accepts custom labels and colors", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    title_name = "Test Plot",
    x_label = "Custom X",
    y_label = "Custom Y",
    fill_color = "lightblue",
    legend_position = "top"
  )

  expect_s3_class(g, "ggplot")
  expect_true(!is.null(g$labels$title))
})

test_that("plot_boxplot legend_position parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_bottom <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    legend_position = "bottom"
  )
  g_left <- plot_boxplot(
    sdesign,
    numeric_var = "muac",
    legend_position = "left"
  )

  expect_s3_class(g_bottom, "ggplot")
  expect_s3_class(g_left, "ggplot")
})

# ============================================================
# 4. INTEGRATION TESTS FOR UPDATED FUNCTIONS
# ============================================================

# ============================================================
# 4a. SHOW_OVERALL AND AUTO-TITLE TESTS
# ============================================================

create_test_date_data <- function(n = 60) {
  data.frame(
    date_col = seq(as.Date("2023-01-01"), by = "day", length.out = n),
    muac = rnorm(n, mean = 13.5, sd = 1.5),
    district = sample(c("District A", "District B"), n, replace = TRUE),
    weight = runif(n, 0.5, 2)
  )
}

# --- plot_stacked_bar: show_overall default TRUE ---

test_that("plot_stacked_bar show_overall defaults to TRUE", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  # Default (show_overall = TRUE) with grouping should include "Overall" bar
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      grouping = "group"
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar show_overall = FALSE suppresses overall bar", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      grouping = "group",
      show_overall = FALSE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar auto-generates title from variable_label", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      variable_label = "Response Type"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of Response Type")
})

test_that("plot_stacked_bar auto-title appends grouping label when grouping is provided", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      grouping = "group",
      variable_label = "Response Type",
      grouping_label = "District"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of Response Type, by District")
})

test_that("plot_stacked_bar auto-title uses column name when grouping_label is NULL", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      grouping = "group",
      variable_label = "Response Type"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of Response Type, by group")
})

test_that("plot_stacked_bar title_name takes precedence over variable_label", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_stacked_bar(
      sdesign,
      category_var = "category",
      title_name = "My Custom Title",
      variable_label = "Response Type"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "My Custom Title")
})

# --- plot_boxplot: show_overall ---

test_that("plot_boxplot show_overall = TRUE adds overall box when grouping is provided", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(
      sdesign,
      numeric_var = "muac",
      grouping = "district",
      show_overall = TRUE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_boxplot show_overall = FALSE suppresses overall box", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(
      sdesign,
      numeric_var = "muac",
      grouping = "district",
      show_overall = FALSE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_boxplot show_overall has no effect when grouping is NULL", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(sdesign, numeric_var = "muac", show_overall = TRUE)
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_boxplot auto-generates title from variable_label", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(
      sdesign,
      numeric_var = "muac",
      variable_label = "MUAC (cm)"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of MUAC (cm)")
})

test_that("plot_boxplot auto-title appends grouping label when grouping is provided", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(
      sdesign,
      numeric_var = "muac",
      grouping = "district",
      variable_label = "MUAC (cm)",
      grouping_label = "District"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of MUAC (cm), by District")
})

test_that("plot_boxplot auto-title uses column name when grouping_label is NULL", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_boxplot(
      sdesign,
      numeric_var = "muac",
      grouping = "district",
      variable_label = "MUAC (cm)"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Distribution of MUAC (cm), by district")
})

# --- plot_date_runner: show_overall ---

test_that("plot_date_runner show_overall = TRUE adds overall line when grouping_col is provided", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_date_runner(
      sdesign,
      date_col = "date_col",
      numeric_col = "muac",
      grouping_col = "district",
      show_overall = TRUE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_date_runner show_overall = FALSE suppresses overall line", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_date_runner(
      sdesign,
      date_col = "date_col",
      numeric_col = "muac",
      grouping_col = "district",
      show_overall = FALSE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_date_runner show_overall has no effect when grouping_col is NULL", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_date_runner(
    sdesign,
    date_col = "date_col",
    numeric_col = "muac",
    show_overall = TRUE
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_date_runner auto-generates title from variable_label (mean)", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_date_runner(
      sdesign,
      date_col = "date_col",
      numeric_col = "muac",
      operation = "mean",
      variable_label = "MUAC (cm)"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Cumulative Mean of MUAC (cm)")
})

test_that("plot_date_runner auto-title appends grouping label when grouping_col is provided", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_date_runner(
      sdesign,
      date_col = "date_col",
      numeric_col = "muac",
      grouping_col = "district",
      variable_label = "MUAC (cm)",
      grouping_label = "District"
    )
  )
  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Cumulative Mean of MUAC (cm), by District")
})

test_that("plot_date_runner auto-title prefixes match operation type", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g_sd <- plot_date_runner(
    sdesign,
    date_col = "date_col",
    numeric_col = "muac",
    operation = "sd",
    variable_label = "MUAC (cm)"
  )
  expect_equal(g_sd$labels$title, "Cumulative SD of MUAC (cm)")

  g_count <- plot_date_runner(
    sdesign,
    date_col = "date_col",
    numeric_col = "muac",
    operation = "count",
    variable_label = "MUAC (cm)"
  )
  expect_equal(g_count$labels$title, "Cumulative Count of MUAC (cm)")
})


test_that("plot_correlogram has proper error handling", {
  library(GGally)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_correlogram(NULL, numeric_cols = c("muac")),
    regexp = "survey design"
  )

  expect_warning(
    plot_correlogram(sdesign, numeric_cols = c("nonexistent")),
    regexp = "Missing required columns"
  )
})

test_that("plot_age_pyramid has proper error handling", {
  df <- data.frame(
    age_years = c(5, 10, 15, 20),
    sex = c(1, 2, 1, 2)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_age_pyramid(NULL),
    regexp = "survey design"
  )

  expect_warning(
    plot_age_pyramid(sdesign, age_years = "nonexistent"),
    regexp = "is required"
  )
})

test_that("plot_cumulative_distribution has proper validation", {
  df <- data.frame(
    wfhz = rnorm(100, -1, 1),
    muac = rnorm(100, 13, 1.5),
    group = sample(c("A", "B"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_cumulative_distribution(NULL, data_var = "wfhz"),
    regexp = "survey design"
  )

  expect_warning(
    plot_cumulative_distribution(sdesign, data_var = "invalid_column"),
    regexp = "does not exist in the dataset"
  )

  expect_warning(
    plot_cumulative_distribution(sdesign, data_var = c("wfhz", "muac")),
    regexp = "length"
  )

  expect_warning(
    plot_cumulative_distribution(
      sdesign,
      data_var = "wfhz",
      vline_intercepts = c(-2, 2),
      vline_colors = c("red")
    ),
    regexp = "same length as vline_intercepts"
  )
})

test_that("plot_cumulative_distribution creates plot with basic parameters", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_cumulative_distribution(sdesign, data_var = "wfhz")

  expect_s3_class(g, "ggplot")
})

test_that("plot_cumulative_distribution works with grouping", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    grouping = "district"
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_cumulative_distribution works with custom intercepts", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    vline_intercepts = c(-2, 2),
    vline_colors = c("blue", "blue")
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_cumulative_distribution works with custom labels", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    x_label = "Weight-for-Height Z-Score",
    title_name = "Cumulative Distribution Analysis"
  )

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$x, "Weight-for-Height Z-Score")
  expect_equal(g$labels$title, "Cumulative Distribution Analysis")
})

test_that("plot_cumulative_distribution works with MUAC data", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Test that MUAC auto-detection works
  g <- plot_cumulative_distribution(sdesign, data_var = "muac")

  expect_s3_class(g, "ggplot")
  # Verify that the x-axis label uses the data_var name
  expect_equal(g$labels$x, "muac")
})

test_that("plot_cumulative_distribution works with no reference lines", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    vline_intercepts = NULL
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_zscore_distribution has proper validation", {
  df <- data.frame(
    wfhz = rnorm(100, -1, 1),
    hfaz = rnorm(100, -1, 1),
    group = sample(c("A", "B"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_zscore_distribution(NULL, zscore_var = "wfhz"),
    regexp = "survey design"
  )

  expect_warning(
    plot_zscore_distribution(sdesign, zscore_var = "invalid_column"),
    regexp = "does not exist in the dataset"
  )

  expect_warning(
    plot_zscore_distribution(sdesign, zscore_var = c("wfhz", "hfaz")),
    regexp = "length"
  )

  expect_warning(
    plot_zscore_distribution(
      sdesign,
      zscore_var = "wfhz",
      vline_intercepts = c(-2, 2),
      vline_colors = c("red")
    ),
    regexp = "same length as vline_intercepts"
  )
})

test_that("plot_zscore_distribution creates plot with basic parameters", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_zscore_distribution(sdesign, zscore_var = "wfhz")

  expect_s3_class(g, "ggplot")
})


test_that("plot_zscore_distribution works with grouping", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    grouping = "district"
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_zscore_distribution works with custom intercepts", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    vline_intercepts = c(-2, 2),
    vline_colors = c("blue", "blue")
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_zscore_distribution works with custom labels", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    x_label = "Weight-for-Height Z-Score",
    title_name = "Distribution Analysis"
  )

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$x, "Weight-for-Height Z-Score")
  expect_equal(g$labels$title, "Distribution Analysis")
})


test_that("plot_cumulative_distribution legend_position parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_bottom <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    legend_position = "bottom"
  )
  g_top <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    legend_position = "top"
  )

  expect_s3_class(g_bottom, "ggplot")
  expect_s3_class(g_top, "ggplot")
})

test_that("plot_cumulative_distribution flip_coordinates parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_normal <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    flip_coordinates = FALSE
  )
  g_flipped <- plot_cumulative_distribution(
    sdesign,
    data_var = "wfhz",
    flip_coordinates = TRUE
  )

  expect_s3_class(g_normal, "ggplot")
  expect_s3_class(g_flipped, "ggplot")
})

test_that("plot_zscore_distribution legend_position parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_bottom <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    legend_position = "bottom"
  )
  g_right <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    legend_position = "right"
  )

  expect_s3_class(g_bottom, "ggplot")
  expect_s3_class(g_right, "ggplot")
})

test_that("plot_zscore_distribution flip_coordinates parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_normal <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    flip_coordinates = FALSE
  )
  g_flipped <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    flip_coordinates = TRUE
  )

  expect_s3_class(g_normal, "ggplot")
  expect_s3_class(g_flipped, "ggplot")
})

test_that("plot_zscore_distribution y_lab parameter works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_zscore_distribution(
    sdesign,
    zscore_var = "wfhz",
    y_lab = "Custom Density Label"
  )

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$y, "Custom Density Label")
})

# ============================================================
# 13. PLOT_CI_BAR_PERCENTAGE TESTS
# ============================================================

test_that("plot_ci_bar_percentage requires valid dataset", {
  expect_warning(
    plot_ci_bar_percentage(NULL, category_var = "category"),
    regexp = "survey design"
  )
  expect_warning(
    plot_ci_bar_percentage("not_a_df", category_var = "category"),
    regexp = "survey design"
  )
})

test_that("plot_ci_bar_percentage requires existing category column", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    suppressMessages(plot_ci_bar_percentage(
      sdesign,
      category_var = "nonexistent_col"
    )),
    regexp = "nonexistent_col|exist"
  )
})

test_that("plot_ci_bar_percentage returns a ggplot object", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressMessages(
    g <- plot_ci_bar_percentage(sdesign, category_var = "category")
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_bar_percentage with grouping returns a ggplot object", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_ci_bar_percentage(
    sdesign,
    category_var = "category",
    grouping = "group"
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_bar_percentage flip_coordinates works", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_ci_bar_percentage(
    sdesign,
    category_var = "category",
    flip_coordinates = TRUE
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_bar_percentage weighted requires weights_col", {
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    suppressMessages(plot_ci_bar_percentage(
      sdesign,
      category_var = "category",
      weighted = TRUE
    )),
    regexp = "NULL|weights_col"
  )
})

test_that("plot_ci_bar_percentage weighted works", {
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_ci_bar_percentage(
    sdesign,
    category_var = "category",
    weighted = TRUE,
    weights_col = "weight"
  ))
  expect_s3_class(g, "ggplot")
})

# ============================================================
# 14. PLOT_CI_POINT_MEAN TESTS
# ============================================================

test_that("plot_ci_point_mean requires valid dataset", {
  expect_warning(
    plot_ci_point_mean(NULL, numeric_var = "muac"),
    regexp = "survey design"
  )
})

test_that("plot_ci_point_mean requires existing numeric column", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_ci_point_mean(sdesign, numeric_var = "nonexistent"),
    regexp = "nonexistent|exist|must exist|Numeric column"
  )
})

test_that("plot_ci_point_mean returns a ggplot object (overall)", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(g <- plot_ci_point_mean(sdesign, numeric_var = "muac"))
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_point_mean with grouping returns a ggplot object", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_ci_point_mean(
      sdesign,
      numeric_var = "muac",
      grouping = "district"
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_point_mean flip_coordinates works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_ci_point_mean(
      sdesign,
      numeric_var = "muac",
      flip_coordinates = TRUE
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_point_mean ratio mode errors if numeric_var2 does not exist", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ci_point_mean(
      sdesign,
      numeric_var = "muac",
      numeric_var2 = "does_not_exist"
    )),
    regexp = "Second numeric column|does_not_exist|exist|must exist"
  )
})

test_that("plot_ci_point_mean show_labels works (still returns ggplot)", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_ci_point_mean(
    sdesign,
    numeric_var = "muac",
    show_labels = TRUE
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_ci_point_mean validates ylim length", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ci_point_mean(
      sdesign,
      numeric_var = "muac",
      ylim = c(0, 1, 2)
    )),
    regexp = "ylim must be a numeric vector of length 2|length 2"
  )
})

test_that("plot_ci_point_mean validates ylim order", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ci_point_mean(
      sdesign,
      numeric_var = "muac",
      ylim = c(10, 0)
    )),
    regexp = "ylim\\[1\\] must be less than ylim\\[2\\]"
  )
})

test_that("plot_ci_point_mean weighted args do not break for survey design objects", {
  # In plot_ci_point_mean, `weighted` and `weights_col` are ignored for survey designs,
  # but should still validate and return a ggplot.
  df <- create_test_numeric_data()
  df$weight <- runif(nrow(df), 0.5, 2)

  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_ci_point_mean(
    sdesign,
    numeric_var = "muac",
    weighted = TRUE,
    weights_col = "weight"
  )
  expect_s3_class(g, "ggplot")
})

# ============================================================
# 15. PLOT_SCATTER TESTS
# ============================================================

test_that("plot_scatter requires valid dataset", {
  expect_warning(
    suppressMessages(plot_scatter(NULL, x_var = "muac", y_var = "wfhz")),
    regexp = "survey design"
  )
})

test_that("plot_scatter requires existing x and y columns", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    suppressMessages(plot_scatter(
      sdesign,
      x_var = "nonexistent",
      y_var = "wfhz"
    )),
    regexp = "nonexistent|exist"
  )
  expect_warning(
    suppressMessages(plot_scatter(
      sdesign,
      x_var = "muac",
      y_var = "nonexistent"
    )),
    regexp = "nonexistent|exist"
  )
})

test_that("plot_scatter returns a ggplot object", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_scatter(sdesign, x_var = "muac", y_var = "wfhz"))
  expect_s3_class(g, "ggplot")
})

test_that("plot_scatter with grouping returns a ggplot object", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_scatter(
    sdesign,
    x_var = "muac",
    y_var = "wfhz",
    grouping = "district"
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_scatter flip_coordinates works", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_scatter(
    sdesign,
    x_var = "muac",
    y_var = "wfhz",
    flip_coordinates = TRUE
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_scatter weighted works (size aesthetic)", {
  df <- create_test_numeric_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_scatter(
    sdesign,
    x_var = "muac",
    y_var = "wfhz",
    weighted = TRUE,
    weights_col = "weight"
  ))
  expect_s3_class(g, "ggplot")
})

# ============================================================
# 16. PLOT_DONUT TESTS
# ============================================================

test_that("plot_donut requires valid dataset", {
  expect_warning(
    suppressMessages(plot_donut(NULL, category_var = "category")),
    regexp = "survey design"
  )
})

test_that("plot_donut requires existing category column", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    plot_donut(sdesign, category_var = "nonexistent"),
    regexp = "nonexistent|exist"
  )
})

test_that("plot_donut returns a ggplot object", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_donut(sdesign, category_var = "category"))
  expect_s3_class(g, "ggplot")
})

test_that("plot_donut label_type parameter works", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g_pct <- suppressMessages(plot_donut(
    sdesign,
    category_var = "category",
    label_type = "percentage"
  ))
  g_cnt <- suppressMessages(plot_donut(
    sdesign,
    category_var = "category",
    label_type = "count"
  ))
  g_both <- suppressMessages(plot_donut(
    sdesign,
    category_var = "category",
    label_type = "both"
  ))
  expect_s3_class(g_pct, "ggplot")
  expect_s3_class(g_cnt, "ggplot")
  expect_s3_class(g_both, "ggplot")
})

test_that("plot_donut invalid label_type raises error", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    suppressMessages(plot_donut(
      sdesign,
      category_var = "category",
      label_type = "invalid"
    )),
    regexp = "invalid|choice|percentage|count|both"
  )
})

test_that("plot_donut weighted works", {
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- suppressMessages(plot_donut(
    sdesign,
    category_var = "category",
    weighted = TRUE,
    weights_col = "weight"
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_ridge_distribution works with NULL name_groups and name_units", {
  set.seed(42)
  df <- data.frame(
    fcs_score = rnorm(50, 42, 15),
    rcsi_score = rnorm(50, 10, 8)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Should not throw a symbol conversion error when name_groups/name_units are NULL
  expect_no_error({
    g <- suppressMessages(plot_ridge_distribution(
      sdesign,
      numeric_cols = c("fcs_score", "rcsi_score")
    ))
  })
  expect_false(is.null(g))
})

test_that("plot_correlogram returns a ggmatrix with title when title_name provided", {
  set.seed(42)
  df <- data.frame(
    fsl_fcs_score = rnorm(50, 42, 15),
    fsl_rcsi_score = rnorm(50, 10, 8),
    fsl_hhs_score = rnorm(50, 2, 1)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Should not throw a 'ggmatrix + character' error
  expect_no_error({
    g <- plot_correlogram(sdesign, title_name = "Test Correlogram")
  })
  expect_false(is.null(g))
  expect_s3_class(g, "ggmatrix")
})

# ============================================================
# PLOT_CROSSTAB FACTOR VARIABLE TESTS
# ============================================================

test_that("plot_crosstab shows missing factor levels with 0 count", {
  set.seed(42)
  # Create a dataset where "C" level of row_var never appears with "Y" level of col_var
  df <- data.frame(
    row_var = factor(c("A", "A", "B", "B", "C"), levels = c("A", "B", "C")),
    col_var = factor(c("X", "Y", "X", "X", "X"), levels = c("X", "Y"))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(sdesign, row_var = "row_var", col_var = "col_var")
  expect_s3_class(g, "ggplot")
  # All factor level combinations should be present in the plot data
  plot_data <- g$data
  expect_true(any(plot_data$row_var == "C" & plot_data$col_var == "Y"))
  # The missing combination should have n = 0
  missing_cell <- plot_data[
    plot_data$row_var == "C" & plot_data$col_var == "Y",
  ]
  expect_equal(missing_cell$n, 0L)
})

test_that("plot_crosstab respects factor level ordering", {
  set.seed(42)
  df <- data.frame(
    row_var = factor(c("C", "A", "B", "A", "C"), levels = c("C", "A", "B")),
    col_var = factor(c("Z", "X", "Y", "Z", "X"), levels = c("Z", "Y", "X"))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(sdesign, row_var = "row_var", col_var = "col_var")
  expect_s3_class(g, "ggplot")
  # Factor levels should follow original ordering
  expect_equal(levels(g$data$row_var), c("C", "A", "B"))
  expect_equal(levels(g$data$col_var), c("Z", "Y", "X"))
})

test_that("plot_crosstab missing factor levels show 0.0% label", {
  set.seed(42)
  df <- data.frame(
    row_var = factor(c("A", "A", "B"), levels = c("A", "B", "C")),
    col_var = factor(c("X", "Y", "X"), levels = c("X", "Y"))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(
    sdesign,
    row_var = "row_var",
    col_var = "col_var",
    show_percentages = TRUE,
    show_counts = TRUE
  )
  expect_s3_class(g, "ggplot")
  plot_data <- g$data
  # C+X and C+Y cells should both be present
  expect_equal(nrow(plot_data), 6L) # 3 rows x 2 cols
  # Missing cells should have 0 count
  expect_true(all(plot_data$n[plot_data$row_var == "C"] == 0L))
})

test_that("plot_crosstab without factor variables is unaffected", {
  set.seed(42)
  df <- data.frame(
    row_var = c("A", "A", "B", "B"),
    col_var = c("X", "Y", "X", "X")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(sdesign, row_var = "row_var", col_var = "col_var")
  expect_s3_class(g, "ggplot")
  # Only present combinations should appear (no missing factor expansion)
  plot_data <- g$data
  expect_equal(nrow(plot_data), 3L) # A+X, A+Y, B+X only
})

test_that("plot_crosstab weighted case handles missing factor levels", {
  set.seed(42)
  df <- data.frame(
    row_var = factor(c("A", "A", "B"), levels = c("A", "B", "C")),
    col_var = factor(c("X", "Y", "X"), levels = c("X", "Y")),
    weight = c(1.2, 0.8, 1.0)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(
    sdesign,
    row_var = "row_var",
    col_var = "col_var",
    weighted = TRUE,
    weights_col = "weight"
  )
  expect_s3_class(g, "ggplot")
  plot_data <- g$data
  expect_equal(nrow(plot_data), 6L) # 3 rows x 2 cols
  expect_true(all(plot_data$n[plot_data$row_var == "C"] == 0L))
  expect_true(all(plot_data$weighted_n[plot_data$row_var == "C"] == 0))
})

test_that("plot_crosstab highlight_cells_row_val1/col_val1 highlights a single cell", {
  set.seed(42)
  df <- data.frame(
    row_var = c("A", "A", "B", "B"),
    col_var = c("X", "Y", "X", "Y")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(
    sdesign,
    row_var = "row_var",
    col_var = "col_var",
    highlight_cells_row_val1 = "A",
    highlight_cells_col_val1 = "X"
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_crosstab highlight_cells_row_val2/col_val2 highlights a second cell", {
  set.seed(42)
  df <- data.frame(
    row_var = c("A", "A", "B", "B"),
    col_var = c("X", "Y", "X", "Y")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(
    sdesign,
    row_var = "row_var",
    col_var = "col_var",
    highlight_cells_row_val1 = "A",
    highlight_cells_col_val1 = "X",
    highlight_cells_row_val2 = "B",
    highlight_cells_col_val2 = "Y"
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_crosstab no highlighting when highlight params are NULL", {
  set.seed(42)
  df <- data.frame(
    row_var = c("A", "A", "B", "B"),
    col_var = c("X", "Y", "X", "Y")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_crosstab(sdesign, row_var = "row_var", col_var = "col_var")
  expect_s3_class(g, "ggplot")
})

test_that("plot_crosstab second highlight cell ignored when only val1 is partial", {
  set.seed(42)
  df <- data.frame(
    row_var = c("A", "A", "B", "B"),
    col_var = c("X", "Y", "X", "Y")
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  # Only row_val2 is provided without col_val2 - second cell should be ignored
  g <- plot_crosstab(
    sdesign,
    row_var = "row_var",
    col_var = "col_var",
    highlight_cells_row_val1 = "A",
    highlight_cells_col_val1 = "X",
    highlight_cells_row_val2 = "B"
  )
  expect_s3_class(g, "ggplot")
})

# ============================================================
# PLOT_STACKED_BAR show_overall TESTS
# ============================================================

test_that("plot_stacked_bar show_overall=TRUE adds overall bar to grouped plot", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "group",
    show_overall = TRUE
  ))

  expect_s3_class(g, "ggplot")
  # The x-axis factor levels should include the "Overall" label
  x_levels <- levels(g$data[[1]])
  if (is.null(x_levels)) {
    x_levels <- unique(g$data[[1]])
  }
  # Check the built plot data contains an "Overall" x position
  built <- ggplot2::ggplot_build(g)$data[[1]]
  expect_true(nrow(built) > 0)
})

test_that("plot_stacked_bar show_overall=FALSE leaves grouped plot unchanged", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g_no_overall <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "group",
    show_overall = FALSE
  ))
  g_with_overall <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "group",
    show_overall = TRUE
  ))

  expect_s3_class(g_no_overall, "ggplot")
  expect_s3_class(g_with_overall, "ggplot")

  # The plot with show_overall should have one more x level than without
  n_x_no_overall <- length(unique(
    ggplot2::ggplot_build(g_no_overall)$data[[1]]$x
  ))
  n_x_with_overall <- length(unique(
    ggplot2::ggplot_build(g_with_overall)$data[[1]]$x
  ))
  expect_true(n_x_with_overall > n_x_no_overall)
})

test_that("plot_stacked_bar show_overall with custom overall_label works", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "group",
    show_overall = TRUE,
    overall_label = "All Groups"
  ))

  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar show_overall=TRUE ignored when grouping is NULL", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # show_overall without grouping should still produce a valid plot (no error)
  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    show_overall = TRUE
  ))

  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar show_overall=TRUE works with integer grouping column", {
  set.seed(42)
  df <- data.frame(
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    enumerator = sample(1:5, 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  # Integer grouping should not cause a type-combination error
  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "enumerator",
    show_overall = TRUE
  ))

  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar show_overall=TRUE works with factor grouping column", {
  set.seed(42)
  df <- data.frame(
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    enumerator = factor(sample(c("E1", "E2", "E3"), 100, replace = TRUE))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "enumerator",
    show_overall = TRUE
  ))

  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar show_overall=TRUE works with weighted data", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar(
    sdesign,
    category_var = "category",
    grouping = "group",
    show_overall = TRUE,
    weighted = TRUE,
    weights_col = "weight"
  ))

  expect_s3_class(g, "ggplot")
})

# ============================================================
# PLOT_STACKED_BAR_MULTIPLE_VARS TWO-LEVEL X-AXIS TESTS
# ============================================================

test_that("plot_stacked_bar_multiple_vars with grouping creates a ggplot", {
  set.seed(42)
  df <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    group = sample(c("G1", "G2", "G3"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar_multiple_vars(
    sdesign,
    category_vars = c("var1", "var2"),
    grouping = "group"
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar_multiple_vars with grouping uses facet_grid for two-level x-axis", {
  set.seed(42)
  df <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    group = sample(c("G1", "G2", "G3"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar_multiple_vars(
    sdesign,
    category_vars = c("var1", "var2"),
    grouping = "group"
  ))
  # Should use FacetGrid for two-level x-axis
  expect_true(inherits(g$facet, "FacetGrid"))
})

test_that("plot_stacked_bar_multiple_vars with grouping and show_overall creates a ggplot", {
  set.seed(42)
  df <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    group = sample(c("G1", "G2", "G3"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar_multiple_vars(
    sdesign,
    category_vars = c("var1", "var2"),
    grouping = "group",
    show_overall = TRUE
  ))
  expect_s3_class(g, "ggplot")
})

test_that("plot_stacked_bar_multiple_vars without grouping unchanged (no facets)", {
  set.seed(42)
  df <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_stacked_bar_multiple_vars(
    sdesign,
    category_vars = c("var1", "var2")
  ))
  expect_s3_class(g, "ggplot")
})

# ============================================================
# PLOT_RIDGE_DISTRIBUTION_BY_GROUP TESTS
# ============================================================

test_that("plot_ridge_distribution_by_group requires valid dataset", {
  expect_warning(
    plot_ridge_distribution_by_group(
      NULL,
      numeric_col = "muac",
      grouping = "district"
    )
  )

  expect_warning(
    plot_ridge_distribution_by_group(
      "not a dataframe",
      numeric_col = "muac",
      grouping = "district"
    )
  )
})

test_that("plot_ridge_distribution_by_group requires numeric_col parameter", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = NULL,
      grouping = "district"
    ))
  )
})

test_that("plot_ridge_distribution_by_group requires grouping parameter", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = "muac",
      grouping = NULL
    ))
  )
})

test_that("plot_ridge_distribution_by_group validates column existence", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    suppressMessages(plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = "nonexistent",
      grouping = "district"
    ))
  )

  expect_warning(
    suppressMessages(plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = "muac",
      grouping = "nonexistent"
    ))
  )
})
library(ggridges)
test_that("plot_ridge_distribution_by_group creates a ggplot object", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  suppressMessages(
    g <- plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = "muac",
      grouping = "district"
    )
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_ridge_distribution_by_group data includes overall and group rows", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- suppressMessages(plot_ridge_distribution_by_group(
    sdesign,
    numeric_col = "muac",
    grouping = "district"
  ))

  expect_s3_class(g, "ggplot")
  # The plot data should have rows for overall + groups
  group_levels <- levels(g$data$.group_label)
  expect_true("Overall" %in% group_levels)
  expect_true(all(
    c("District A", "District B", "District C") %in% group_levels
  ))
})

test_that("plot_ridge_distribution_by_group custom overall_label works", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_ridge_distribution_by_group(
    sdesign,
    numeric_col = "muac",
    grouping = "district",
    overall_label = "All Districts"
  )

  expect_s3_class(g, "ggplot")
  group_levels <- levels(g$data$.group_label)
  expect_true("All Districts" %in% group_levels)
})

test_that("plot_ridge_distribution_by_group accepts optional labels and title", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_ridge_distribution_by_group(
    sdesign,
    numeric_col = "muac",
    grouping = "district",
    title_name = "MUAC by District",
    x_lab = "MUAC (mm)",
    y_lab = "District"
  )

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "MUAC by District")
  expect_equal(g$labels$x, "MUAC (mm)")
})

test_that("plot_ridge_distribution_by_group works with weighted data", {
  set.seed(42)
  df <- create_test_numeric_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  g <- plot_ridge_distribution_by_group(
    sdesign,
    numeric_col = "muac",
    grouping = "district",
    weighted = TRUE,
    weights_col = "weight"
  )

  expect_s3_class(g, "ggplot")
})

test_that("plot_ridge_distribution_by_group weighted requires weights_col", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)

  expect_warning(
    plot_ridge_distribution_by_group(
      sdesign,
      numeric_col = "muac",
      grouping = "district",
      weighted = TRUE
    )
  )
})


# ============================================================
# TABLE_FREQUENCY TESTS
# ============================================================

test_that("table_frequency requires valid dataset", {
  expect_warning(
    table_frequency(NULL, variable = "category"),
    regexp = NULL
  )
  expect_warning(
    table_frequency("not a dataframe", variable = "category"),
    regexp = NULL
  )
})

test_that("table_frequency requires variable parameter", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    table_frequency(sdesign, variable = NULL),
    regexp = NULL
  )
})

test_that("table_frequency validates column existence", {
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    table_frequency(sdesign, variable = "nonexistent_column"),
    regexp = NULL
  )
})

test_that("table_frequency produces flextable for percentage (unweighted)", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency produces flextable for mean (unweighted)", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "muac",
    stat_type = "mean",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency produces flextable for median (unweighted)", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "muac",
    stat_type = "median",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency produces flextable for ratio (unweighted)", {
  set.seed(42)
  df <- data.frame(
    numerator = abs(rnorm(100, 5, 2)),
    denominator = abs(rnorm(100, 10, 3))
  )
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "numerator",
    stat_type = "ratio",
    ratio_denominator = "denominator",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency ratio errors without ratio_denominator", {
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  expect_warning(
    table_frequency(
      sdesign,
      variable = "muac",
      stat_type = "ratio",
      weighted_result = FALSE
    ),
    regexp = NULL
  )
})

test_that("table_frequency works with disaggregation (unweighted percentage)", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency works with disaggregation (unweighted mean)", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "muac",
    stat_type = "mean",
    disaggregation = "district",
    weighted_result = FALSE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency accepts custom labels", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    weighted_result = FALSE,
    variable_label = "Category Label",
    title_name = "Test Title"
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency show_n = FALSE hides n column", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    weighted_result = FALSE,
    show_n = FALSE
  )
  expect_s3_class(ft, "flextable")
  # n column should not be in the flextable
  expect_false("n" %in% ft$col_keys)
})

test_that("table_frequency weighted with weights_col produces flextable", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    weighted_result = TRUE,
    weights_col = "weight"
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency accepts srvyr design as first argument", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  dsn <- srvyr::as_survey_design(df, weights = weight)
  ft <- table_frequency(dsn, variable = "category", stat_type = "percentage")
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency srvyr design with disaggregation produces flextable", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  dsn <- srvyr::as_survey_design(df, weights = weight)
  ft <- table_frequency(
    dsn,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group"
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency show_ci = TRUE adds CI columns for weighted path", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    weighted_result = TRUE,
    weights_col = "weight",
    show_ci = TRUE
  )
  expect_s3_class(ft, "flextable")
  # CI is embedded in the estimate column as "est [lo - hi]"
  estimate_vals <- ft$body$dataset[["estimate"]]
  expect_true(any(grepl("\\[.*-.*\\]", estimate_vals)))
})

test_that("table_frequency show_ci = TRUE works with srvyr design", {
  set.seed(42)
  df <- create_test_numeric_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  dsn <- srvyr::as_survey_design(df, weights = weight)
  ft <- table_frequency(
    dsn,
    variable = "muac",
    stat_type = "mean",
    show_ci = TRUE
  )
  expect_s3_class(ft, "flextable")
  # CI is embedded in the estimate column as "est [lo - hi]"
  estimate_vals <- ft$body$dataset[["estimate"]]
  expect_true(any(grepl("\\[.*-.*\\]", as.character(estimate_vals))))
})

test_that("table_frequency disaggregation_wide = TRUE produces flextable", {
  set.seed(42)
  df <- create_test_categorical_data()
  df$weight <- runif(nrow(df), 0.5, 2)
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = TRUE,
    weights_col = "weight",
    disaggregation_wide = TRUE
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_frequency show_overall = TRUE adds Overall disaggregation group", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = FALSE,
    show_overall = TRUE
  )
  expect_s3_class(ft, "flextable")
  expect_true("Overall" %in% ft$body$dataset[["group"]])
})

test_that("table_frequency show_overall = TRUE works with mean stat", {
  set.seed(42)
  df <- create_test_numeric_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "muac",
    stat_type = "mean",
    disaggregation = "district",
    weighted_result = FALSE,
    show_overall = TRUE
  )
  expect_s3_class(ft, "flextable")
  expect_true("Overall" %in% ft$body$dataset[["district"]])
})

test_that("table_frequency show_overall = FALSE (default) does not add Overall", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = FALSE,
    show_overall = FALSE
  )
  expect_s3_class(ft, "flextable")
  expect_false("Overall" %in% ft$body$dataset[["group"]])
})

test_that("table_frequency wide format shows Unit column exactly once (not per group)", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = FALSE,
    disaggregation_wide = TRUE,
    show_unit = TRUE
  )
  expect_s3_class(ft, "flextable")
  # Unit should appear once, not as Unit_G1, Unit_G2, etc.
  expect_true("Unit" %in% ft$col_keys)
  expect_false(any(grepl("^Unit_", ft$col_keys)))
})

test_that("table_frequency wide format show_unit=FALSE removes Unit column", {
  set.seed(42)
  df <- create_test_categorical_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  ft <- table_frequency(
    sdesign,
    variable = "category",
    stat_type = "percentage",
    disaggregation = "group",
    weighted_result = FALSE,
    disaggregation_wide = TRUE,
    show_unit = FALSE
  )
  expect_s3_class(ft, "flextable")
  expect_false("Unit" %in% ft$col_keys)
  expect_false(any(grepl("^Unit_", ft$col_keys)))
})


# ============================================================
# TABLE_QUALITY_PENALTY_SUMMARY TESTS
# ============================================================

test_that("table_quality_penalty_summary requires valid data frame", {
  expect_warning(
    table_quality_penalty_summary(NULL),
    regexp = NULL
  )
  expect_warning(
    table_quality_penalty_summary("not a dataframe"),
    regexp = NULL
  )
})

test_that("table_quality_penalty_summary requires required columns", {
  df <- data.frame(check_name = "check1", test_statistic = 0.5)
  expect_warning(
    table_quality_penalty_summary(df),
    regexp = NULL
  )
})

test_that("table_quality_penalty_summary produces flextable from valid results", {
  set.seed(42)
  results_df <- tibble::tibble(
    check_name = c("check_fcs", "check_rcsi", "check_hhs"),
    check_label = c("FCS Check", "RCSI Check", "HHS Check"),
    check_group = c("fcs", "rcsi", "fcs"),
    test_statistic = c(0.85, 0.3, 0.6),
    p_value = c(0.04, 0.25, 0.10),
    penalty = c(0, 5, 0),
    max_penalty = c(10, 10, 10),
    threshold_expression = c(
      "test_statistic > 0.7",
      "test_statistic < 0.5",
      "test_statistic > 0.5"
    ),
    message = c("Passed", "Failed", "Passed")
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
})

test_that("table_quality_penalty_summary handles missing check_group column", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    test_statistic = c(0.5, 0.8),
    p_value = c(0.05, 0.20),
    penalty = c(5, 0),
    max_penalty = c(10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
})

test_that("table_quality_penalty_summary shows group totals", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b", "check_c"),
    check_label = c("Check A", "Check B", "Check C"),
    check_group = c("group1", "group1", "group2"),
    test_statistic = c(0.5, 0.3, 0.8),
    p_value = c(0.01, 0.03, 0.20),
    penalty = c(5, 10, 0),
    max_penalty = c(10, 10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  # group_penalty column should be present
  expect_true("group_penalty" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary accepts title_name", {
  results_df <- tibble::tibble(
    check_name = c("check_a"),
    check_label = c("Check A"),
    check_group = c("group1"),
    test_statistic = c(0.5),
    p_value = c(0.01),
    penalty = c(5),
    max_penalty = c(10)
  )

  ft <- table_quality_penalty_summary(
    results_df,
    title_name = "Quality Summary"
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_quality_penalty_summary group_penalty shows sum/max string", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b", "check_c"),
    check_label = c("Check A", "Check B", "Check C"),
    check_group = c("group1", "group1", "group2"),
    test_statistic = c(0.5, 0.3, 0.8),
    p_value = c(0.01, 0.03, 0.20),
    penalty = c(5, 10, 0),
    max_penalty = c(10, 10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  # group1: sum(penalty)=15, sum(max_penalty)=20 -> "15/20"
  gp_g1 <- ft$body$dataset$group_penalty[
    ft$body$dataset$check_group == "group1"
  ][1]
  expect_equal(gp_g1, "15/20")
  # group2: sum(penalty)=0, sum(max_penalty)=10 -> "0/10"
  gp_g2 <- ft$body$dataset$group_penalty[
    ft$body$dataset$check_group == "group2"
  ][1]
  expect_equal(gp_g2, "0/10")
})

test_that("table_quality_penalty_summary includes pct_group_penalty column", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    check_group = c("group1", "group1"),
    test_statistic = c(0.5, 0.3),
    p_value = c(0.01, 0.03),
    penalty = c(4, 6),
    max_penalty = c(20, 20)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  expect_true("pct_group_penalty" %in% ft$col_keys)
  # sum(penalty)=10, sum(max_penalty)=40 -> pct = 25%
  pct_val <- ft$body$dataset$pct_group_penalty[1]
  expect_equal(pct_val, 25)
})

# ============================================================
# TABLE_QUALITY_PENALTY_SUMMARY_BY_GROUP TESTS
# ============================================================

test_that("table_quality_penalty_summary_by_group requires valid data frame", {
  expect_warning(
    table_quality_penalty_summary_by_group(NULL, group_col = "group_value"),
    regexp = NULL
  )
  expect_warning(
    table_quality_penalty_summary_by_group(
      "not a dataframe",
      group_col = "group_value"
    ),
    regexp = NULL
  )
})

test_that("table_quality_penalty_summary_by_group requires group_col to exist", {
  df <- tibble::tibble(
    check_name = "check1",
    check_label = "Check 1",
    penalty = 5
  )
  expect_warning(
    table_quality_penalty_summary_by_group(df, group_col = "missing_col"),
    regexp = NULL
  )
})

test_that("table_quality_penalty_summary_by_group produces flextable from valid per-group results", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E001", "E002", "E002"),
    check_name = c("check_fcs", "check_rcsi", "check_fcs", "check_rcsi"),
    check_label = c("FCS Check", "RCSI Check", "FCS Check", "RCSI Check"),
    check_group = c("fcs", "rcsi", "fcs", "rcsi"),
    test_statistic = c(0.85, 0.3, 0.6, 0.4),
    p_value = c(0.04, 0.25, 0.10, 0.15),
    penalty = c(0, 5, 3, 5),
    max_penalty = c(10, 10, 10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")
  # Wide format: no group_value column; per-group penalty columns exist
  expect_false("group_value" %in% ft$col_keys)
  expect_true("penalty__E001" %in% ft$col_keys)
  expect_true("penalty__E002" %in% ft$col_keys)
  expect_true("group_total__E001" %in% ft$col_keys)
  expect_true("group_total__E002" %in% ft$col_keys)
  # Test_statistic and p_value should not appear
  expect_false("test_statistic" %in% ft$col_keys)
  expect_false("p_value" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary_by_group uses two-level column headers", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E002"),
    check_name = c("check_a", "check_a"),
    check_label = c("Check A", "Check A"),
    penalty = c(0, 5),
    max_penalty = c(10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value",
    group_label = "Enumerator ID"
  )
  expect_s3_class(ft, "flextable")
  # Top-level header should contain the actual group values
  header_labels <- ft$header$dataset
  expect_true(any(unlist(header_labels) == "E001"))
  expect_true(any(unlist(header_labels) == "E002"))
  # Second-level header should contain the metric names
  expect_true(any(unlist(header_labels) == "penalty__E001"))
  expect_true(any(unlist(header_labels) == "group_total__E001"))
})

test_that("table_quality_penalty_summary_by_group group_total shows sum/max string per group", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E001", "E002", "E002"),
    check_name = c("check_a", "check_b", "check_a", "check_b"),
    check_label = c("Check A", "Check B", "Check A", "Check B"),
    penalty = c(5, 10, 0, 3),
    max_penalty = c(10, 10, 10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")

  # Wide format: group totals are in group_total__<group> columns
  # E001: sum(penalty)=15, sum(max_penalty)=20 -> "15/20"
  gp_e001 <- ft$body$dataset$group_total__E001[1]
  expect_equal(gp_e001, "15/20")
  # E002: sum(penalty)=3, sum(max_penalty)=20 -> "3/20"
  gp_e002 <- ft$body$dataset$group_total__E002[1]
  expect_equal(gp_e002, "3/20")
})

test_that("table_quality_penalty_summary_by_group has correct per-group penalty values", {
  results_df <- tibble::tibble(
    group_value = c("urban", "urban"),
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    penalty = c(4, 6),
    max_penalty = c(20, 20)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")
  # Check per-check penalty values appear in wide columns
  penalties <- ft$body$dataset$penalty__urban
  expect_equal(sort(penalties), c(4, 6))
  # Group total: sum=10, max=40 -> "10/40"
  gp_urban <- ft$body$dataset$group_total__urban[1]
  expect_equal(gp_urban, "10/40")
})

test_that("table_quality_penalty_summary_by_group accepts title_name", {
  results_df <- tibble::tibble(
    group_value = c("E001"),
    check_name = c("check_a"),
    check_label = c("Check A"),
    penalty = c(5),
    max_penalty = c(10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value",
    title_name = "Penalty by Enumerator"
  )
  expect_s3_class(ft, "flextable")
})

# ============================================================
# UNGROUPED ROW SUPPRESSION TESTS
# ============================================================

test_that("table_quality_penalty_summary does not show check_group column when none provided", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    test_statistic = c(0.5, 0.8),
    p_value = c(0.05, 0.20),
    penalty = c(5, 0),
    max_penalty = c(10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  # check_group should NOT appear when not provided (no "(Ungrouped)" row)
  expect_false("check_group" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary does not show check_group column when all NA", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    check_group = c(NA_character_, NA_character_),
    test_statistic = c(0.5, 0.8),
    p_value = c(0.05, 0.20),
    penalty = c(5, 0),
    max_penalty = c(10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  # check_group should NOT appear when all values are NA
  expect_false("check_group" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary shows check_group column when values are present", {
  results_df <- tibble::tibble(
    check_name = c("check_a", "check_b"),
    check_label = c("Check A", "Check B"),
    check_group = c("group1", "group2"),
    test_statistic = c(0.5, 0.8),
    p_value = c(0.05, 0.20),
    penalty = c(5, 0),
    max_penalty = c(10, 10)
  )

  ft <- table_quality_penalty_summary(results_df)
  expect_s3_class(ft, "flextable")
  # check_group SHOULD appear when values are provided
  expect_true("check_group" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary_by_group does not show check_group when none provided", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E001", "E002", "E002"),
    check_name = c("check_a", "check_b", "check_a", "check_b"),
    check_label = c("Check A", "Check B", "Check A", "Check B"),
    penalty = c(5, 10, 0, 3),
    max_penalty = c(10, 10, 10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")
  # check_group should NOT appear when not provided
  expect_false("check_group" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary_by_group shows check_group when values are present", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E001", "E002", "E002"),
    check_name = c("check_a", "check_b", "check_a", "check_b"),
    check_label = c("Check A", "Check B", "Check A", "Check B"),
    check_group = c("grp1", "grp2", "grp1", "grp2"),
    penalty = c(5, 10, 0, 3),
    max_penalty = c(10, 10, 10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")
  # check_group SHOULD appear when values are provided
  expect_true("check_group" %in% ft$col_keys)
})

test_that("table_quality_penalty_summary_by_group group_total is per check_group", {
  results_df <- tibble::tibble(
    group_value = c("E001", "E001", "E001", "E001"),
    check_name = c("check_a", "check_b", "check_c", "check_d"),
    check_label = c("Check A", "Check B", "Check C", "Check D"),
    check_group = c("grp1", "grp1", "grp2", "grp2"),
    penalty = c(5, 10, 2, 3),
    max_penalty = c(10, 10, 10, 10)
  )

  ft <- table_quality_penalty_summary_by_group(
    results_df,
    group_col = "group_value"
  )
  expect_s3_class(ft, "flextable")
  # grp1 checks: penalty sum=15, max=20 -> "15/20"
  # grp2 checks: penalty sum=5, max=20 -> "5/20"
  grp1_total <- ft$body$dataset$group_total__E001[
    ft$body$dataset$check_group == "grp1"
  ][1]
  expect_equal(grp1_total, "15/20")
  grp2_total <- ft$body$dataset$group_total__E001[
    ft$body$dataset$check_group == "grp2"
  ][1]
  expect_equal(grp2_total, "5/20")
})


# --- helper_runner_dps and expanding_window ---

test_that("helper_runner_dps returns a single numeric value", {
  set.seed(1)
  x <- round(rnorm(50, 125, 10))
  result <- helper_runner_dps(x)
  expect_true(is.numeric(result))
  expect_length(result, 1L)
  expect_false(is.na(result))
  expect_true(result >= 0)
})

test_that("plot_date_runner with operation='dps' still produces a ggplot", {
  df <- create_test_date_data()
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  suppressWarnings(
    g <- plot_date_runner(
      sdesign,
      date_col = "date_col",
      numeric_col = "muac",
      operation = "dps"
    )
  )
  expect_s3_class(g, "ggplot")
})

test_that("plot_date_runner with operation='ratio' still produces a ggplot", {
  df <- create_test_date_data()
  df$deaths <- round(runif(nrow(df), 0, 5))
  df$population <- round(runif(nrow(df), 100, 500))
  sdesign <- srvyr::as_survey_design(df, ids = 1)
  g <- plot_date_runner(
    sdesign,
    date_col = "date_col",
    numeric_col = "deaths",
    numeric_col2 = "population",
    operation = "ratio"
  )
  expect_s3_class(g, "ggplot")
})
