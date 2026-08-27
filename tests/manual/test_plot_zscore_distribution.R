# Manual Test: plot_zscore_distribution Function
#
# This script demonstrates the usage of the refactored plot_zscore_distribution
# function with simple examples using dummy data.
#
# The function has been refactored to:
# - Accept any z-score column (not just hardcoded anthropometric indices)
# - Remove the 'flags' parameter (use specific column names instead)
# - Allow dynamic vertical reference lines
# - Support custom labels

library(phr)
library(dplyr)
library(ggplot2)

# Set output directory
output_dir <- "tests/manual/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("=== Manual Testing: plot_zscore_distribution ===\n\n")

# ============================================================================
# EXAMPLE 1: Basic Usage with Simple Z-Score Data
# ============================================================================
cat("Example 1: Basic usage with a single z-score column\n")
cat(strrep("-", 60), "\n")

# Create simple dummy data with z-scores
set.seed(123)

df_basic <- data.frame(
  id = 1:200,
  wfhz = rnorm(200, mean = -1, sd = 1.2), # Weight-for-height z-scores
  hfaz = rnorm(200, mean = -0.8, sd = 1.1), # Height-for-age z-scores
  district = sample(
    c("District A", "District B", "District C"),
    200,
    replace = TRUE
  )
)

# Minimal change: wrap df_basic in a srvyr survey design
sdesign_basic <- srvyr::as_survey_design(
  df_basic,
  ids = 1
)


# Basic plot with default settings
p1 <- plot_zscore_distribution(
  survey_design = sdesign_basic,
  zscore_var = "wfhz"
)

print(p1)
ggsave(
  file.path(output_dir, "01_basic_zscore_plot.png"),
  p1,
  width = 8,
  height = 6
)
cat("✓ Saved: 01_basic_zscore_plot.png\n\n")

# ============================================================================
# EXAMPLE 2: Custom Title and Labels
# ============================================================================
cat("Example 2: Adding custom title and x-axis label\n")
cat(strrep("-", 60), "\n")

p2 <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  x_label = "Weight-for-Height Z-Score",
  title_name = "Distribution of Weight-for-Height Z-Scores",
  subtitle = "Survey 2024"
)

print(p2)
ggsave(file.path(output_dir, "02_custom_labels.png"), p2, width = 8, height = 6)
cat("✓ Saved: 02_custom_labels.png\n\n")

# ============================================================================
# EXAMPLE 3: With Grouping Variable
# ============================================================================
cat("Example 3: Grouped by district\n")
cat(strrep("-", 60), "\n")

p3 <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  grouping = "district",
  title_name = "Z-Score Distribution by District",
  x_label = "Weight-for-Height Z-Score",
  color_palette = "reach1"
)

print(p3)
ggsave(
  file.path(output_dir, "03_grouped_by_district.png"),
  p3,
  width = 10,
  height = 6
)
cat("✓ Saved: 03_grouped_by_district.png\n\n")

# ============================================================================
# EXAMPLE 4: Custom Vertical Reference Lines
# ============================================================================
cat("Example 4: Custom vertical reference lines (only ±2 SD)\n")
cat(strrep("-", 60), "\n")

p4 <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  vline_intercepts = c(-2, 2),
  vline_colors = c("blue", "blue"),
  title_name = "Distribution with ±2 SD Reference Lines",
  x_label = "Weight-for-Height Z-Score"
)

print(p4)
ggsave(
  file.path(output_dir, "04_custom_reference_lines.png"),
  p4,
  width = 8,
  height = 6
)
cat("✓ Saved: 04_custom_reference_lines.png\n\n")

# ============================================================================
# EXAMPLE 5: No Reference Lines
# ============================================================================
cat("Example 5: Plot without vertical reference lines\n")
cat(strrep("-", 60), "\n")

p5 <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "hfaz",
  vline_intercepts = NULL,
  title_name = "Height-for-Age Distribution (No Reference Lines)",
  x_label = "Height-for-Age Z-Score"
)

print(p5)
ggsave(
  file.path(output_dir, "05_no_reference_lines.png"),
  p5,
  width = 8,
  height = 6
)
cat("✓ Saved: 05_no_reference_lines.png\n\n")

# ============================================================================
# EXAMPLE 6: Simulating Flagged vs Non-Flagged Data
# ============================================================================
cat("Example 6: Comparing data with and without flagged values\n")
cat(strrep("-", 60), "\n")

# Create data with some extreme outliers (flagged values)
df_with_flags <- data.frame(
  id = 1:250,
  wfhz = c(
    rnorm(230, mean = -1, sd = 1), # Normal values
    runif(10, -8, -6), # Extreme low outliers
    runif(10, 6, 8) # Extreme high outliers
  ),
  wfhz_noflag = c(
    rnorm(230, mean = -1, sd = 1), # Normal values
    rep(NA, 20) # Flagged values removed
  )
)

# Plot with flagged values
p6a <- plot_zscore_distribution(
  df = df_with_flags,
  zscore_var = "wfhz",
  title_name = "Distribution Including Flagged Values",
  x_label = "Weight-for-Height Z-Score"
)

# Plot without flagged values
p6b <- plot_zscore_distribution(
  df = df_with_flags,
  zscore_var = "wfhz_noflag",
  title_name = "Distribution Excluding Flagged Values",
  x_label = "Weight-for-Height Z-Score"
)

print(p6a)
print(p6b)
ggsave(file.path(output_dir, "06a_with_flags.png"), p6a, width = 8, height = 6)
ggsave(
  file.path(output_dir, "06b_without_flags.png"),
  p6b,
  width = 8,
  height = 6
)
cat("✓ Saved: 06a_with_flags.png and 06b_without_flags.png\n\n")

# ============================================================================
# EXAMPLE 7: Non-Anthropometric Z-Scores
# ============================================================================
cat("Example 7: Using non-anthropometric z-scores (income example)\n")
cat(strrep("-", 60), "\n")

# Create dummy income z-score data
df_income <- data.frame(
  household_id = 1:300,
  income_zscore = rnorm(300, mean = 0, sd = 1.5),
  region = sample(c("Urban", "Rural"), 300, replace = TRUE)
)

p7 <- plot_zscore_distribution(
  df = df_income,
  zscore_var = "income_zscore",
  vline_intercepts = c(-1.96, 0, 1.96),
  vline_colors = c("green", "black", "green"),
  x_label = "Household Income Z-Score",
  title_name = "Distribution of Household Income Z-Scores",
  grouping = "region"
)

print(p7)
ggsave(
  file.path(output_dir, "07_income_zscore.png"),
  p7,
  width = 10,
  height = 6
)
cat("✓ Saved: 07_income_zscore.png\n\n")

# ============================================================================
# EXAMPLE 8: Different Color Palettes
# ============================================================================
cat("Example 8: Using different color palettes\n")
cat(strrep("-", 60), "\n")

p8a <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  grouping = "district",
  color_palette = "reach1",
  title_name = "Color Palette: reach1"
)

p8b <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  grouping = "district",
  color_palette = "reach2",
  title_name = "Color Palette: reach2"
)

print(p8a)
print(p8b)
ggsave(
  file.path(output_dir, "08a_palette_reach1.png"),
  p8a,
  width = 10,
  height = 6
)
ggsave(
  file.path(output_dir, "08b_palette_reach2.png"),
  p8b,
  width = 10,
  height = 6
)
cat("✓ Saved: 08a_palette_reach1.png and 08b_palette_reach2.png\n\n")

# ============================================================================
# EXAMPLE 9: Multiple Reference Lines with Different Colors
# ============================================================================
cat("Example 9: Multiple custom reference lines\n")
cat(strrep("-", 60), "\n")

p9 <- plot_zscore_distribution(
  df = df_basic,
  zscore_var = "wfhz",
  vline_intercepts = c(-3, -2, -1, 0, 1, 2, 3),
  vline_colors = c(
    "red",
    "orange",
    "yellow",
    "black",
    "yellow",
    "orange",
    "red"
  ),
  title_name = "Distribution with Multiple Reference Lines",
  x_label = "Weight-for-Height Z-Score"
)

print(p9)
ggsave(
  file.path(output_dir, "09_multiple_reference_lines.png"),
  p9,
  width = 8,
  height = 6
)
cat("✓ Saved: 09_multiple_reference_lines.png\n\n")

# ============================================================================
# EXAMPLE 10: Realistic Anthropometric Data Simulation
# ============================================================================
cat("Example 10: Realistic anthropometric data with multiple indices\n")
cat(strrep("-", 60), "\n")

# Simulate realistic anthropometric data
set.seed(456)
df_anthro <- data.frame(
  child_id = 1:500,
  wfhz = rnorm(500, mean = -0.5, sd = 1.1),
  hfaz = rnorm(500, mean = -1.2, sd = 1.3),
  wfaz = rnorm(500, mean = -0.8, sd = 1.2),
  muac_zscore = rnorm(500, mean = -0.3, sd = 0.9),
  age_group = sample(c("6-23 months", "24-59 months"), 500, replace = TRUE),
  survey_round = sample(c("Baseline", "Endline"), 500, replace = TRUE)
)

# Plot each index
p10a <- plot_zscore_distribution(
  df = df_anthro,
  zscore_var = "wfhz",
  grouping = "age_group",
  title_name = "Weight-for-Height by Age Group",
  x_label = "WHZ"
)

p10b <- plot_zscore_distribution(
  df = df_anthro,
  zscore_var = "hfaz",
  grouping = "age_group",
  title_name = "Height-for-Age by Age Group",
  x_label = "HAZ"
)

p10c <- plot_zscore_distribution(
  df = df_anthro,
  zscore_var = "wfaz",
  grouping = "survey_round",
  title_name = "Weight-for-Age by Survey Round",
  x_label = "WAZ"
)

print(p10a)
print(p10b)
print(p10c)
ggsave(
  file.path(output_dir, "10a_wfhz_by_age.png"),
  p10a,
  width = 10,
  height = 6
)
ggsave(
  file.path(output_dir, "10b_hfaz_by_age.png"),
  p10b,
  width = 10,
  height = 6
)
ggsave(
  file.path(output_dir, "10c_wfaz_by_round.png"),
  p10c,
  width = 10,
  height = 6
)
cat(
  "✓ Saved: 10a_wfhz_by_age.png, 10b_hfaz_by_age.png, 10c_wfaz_by_round.png\n\n"
)

# ============================================================================
# Summary
# ============================================================================
cat("\n")
cat(strrep("=", 60), "\n")
cat("SUMMARY: Manual Testing Complete\n")
cat(strrep("=", 60), "\n")
cat("\nAll plots have been saved to:", output_dir, "\n")
cat("\nKey improvements demonstrated:\n")
cat("  • Accepts any z-score column (not limited to wfhz/hfaz/wfaz/mfaz)\n")
cat("  • No more 'flags' parameter - use specific column names instead\n")
cat("  • Dynamic vertical reference lines (customize or disable)\n")
cat("  • Custom x-axis labels and titles\n")
cat("  • Works with grouping variables\n")
cat("  • Supports different color palettes\n")
cat("  • Can handle non-anthropometric z-scores\n")
cat("\n")

# List all generated files
cat("Generated files:\n")
files <- list.files(output_dir, pattern = "*.png", full.names = FALSE)
for (f in files) {
  cat("  -", f, "\n")
}

cat("\n✓ Manual testing complete!\n")
