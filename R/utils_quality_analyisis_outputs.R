#' Plot Correlogram
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param numeric_cols a vector of the fsl indicator scores.
#' By default: c("fsl_fcs_score",  "fsl_rcsi_score",  "fsl_hhs_score")
#' @param title_name Title of the plot
#' @param variable_label Optional character string. Human-readable label for the variables, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Optional character string appended to the auto-generated n count subtitle. Default: NULL.
#'
#' @return a Correlogram plot
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_correlogram(survey_design)
#' }

plot_correlogram <- function(
  survey_design,
  numeric_cols = c("fsl_fcs_score", "fsl_rcsi_score", "fsl_hhs_score"),
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL
) {
  origin <- "plot_correlogram"

  phrutils::phr_try(
    {
      if (!requireNamespace("GGally", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'GGally' is required for plot_correlogram(). Install it with: install.packages('GGally')"
          ),
          origin = origin
        )
      }

      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_not_null(
        numeric_cols,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_vector_length(
        numeric_cols,
        min_length = 1,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        numeric_cols,
        origin = origin,
        hint = phr_txt("Ensure all specified columns exist in the dataset"),
        soft = FALSE
      )

      print(numeric_cols)

      # Create subtitle with n
      total_n <- nrow(df)
      auto_subtitle <- sprintf("n = %d", total_n)
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      g <- GGally::ggpairs(data = df, columns = numeric_cols)
      if (!is.null(title_name)) {
        g$title <- paste0(title_name, "\n", final_subtitle)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Correlation Matrix"), "-", variable_label)
        g$title <- paste0(auto_title, "\n", final_subtitle)
      } else {
        g$title <- final_subtitle
      }
      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Age Pyramid
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param sex_col (Required) The variable name in the Data indicating the sex of the individual.
#' @param age_years_col (Required) The variable name in the Data indicating the age of the individual.
#' @param age_grouping If TRUE, user is using age_grouping in data with variable name
#' specified in age_group_col. If FALSE (default), age_years_col will be grouped as follows:
#' 0-4/5-9/10-14/15-19/20-24/25-29/30-34/35-39/40-44/
#' 45-49/50-54/55-59/60-64/65-69/70-74/75-79/80-84/85+
#' @param age_group_col The variable name for pre-grouped age data.
#' By default: NULL. Only used when age_grouping = TRUE.
#' @param sex_male_val The value in the sex_col that represents males. By default: NULL (falls back to "1")
#' @param sex_female_val The value in the sex_col that represents females. By default: NULL (falls back to "2")
#' @param sex_male_lab The label to display for males in the legend. By default: NULL (falls back to "Male")
#' @param sex_female_lab The label to display for females in the legend. By default: NULL (falls back to "Female")
#' @param weights_col The variable name for survey weights. By default: NULL (no weights applied).
#' If provided, must be numeric and non-NA values will be used for weighting.
#' @param weighted_result Logical. If TRUE and weights_col is provided, uses weighted counts.
#' If FALSE or weights_col is NULL, uses unweighted counts. By default: FALSE.
#' @param proportional Logical. If TRUE (default), shows proportions. If FALSE, shows counts.
#' @param color_palette Inputs an optional character value specifying the color palette to use.
#'   Options: "sex" (default for male/female comparison), "reach1", "reach2", "reach3", "reach4", "traffic_light", "default".
#' @param title_name Title of the plot. By default: NULL.
#' @param variable_label Optional character string. Human-readable label for the variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Inputs an optional character value for the subtitle of the plot.
#'   If NULL, automatically displays n by sex (e.g., "Male (n=X); Female (n=Y)"). Custom subtitle will be appended to n display.
#' @param x_lab Label for the x-axis. By default: NULL.
#' If NULL, uses automatic default: "Proportion of Population" or "Count" based on proportional parameter.
#' @param y_lab Label for the y-axis. By default: NULL (falls back to "Age Group").
#'
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @return An Age Pyramid plot
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_age_pyramid(survey_design = hh_roster, sex_col = "sex", age_years_col = "age_years")
#'   plot_age_pyramid(survey_design = hh_roster, sex_col = "gender", age_years_col = "age",
#'                    sex_male_val = "m", sex_female_val = "f")
#'   plot_age_pyramid(survey_design = hh_roster, sex_col = "sex", age_years_col = "age_years",
#'                    weights_col = "survey_weight", weighted_result = TRUE)
#' }
plot_age_pyramid <- function(
  survey_design,
  sex_col,
  age_years_col,
  age_grouping = FALSE,
  age_group_col = NULL,
  sex_male_val = NULL,
  sex_female_val = NULL,
  sex_male_lab = NULL,
  sex_female_lab = NULL,
  weights_col = NULL,
  weighted_result = FALSE,
  proportional = TRUE,
  color_palette = "sex",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "bottom"
) {
  origin <- "plot_age_pyramid"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_assert(
        !missing(sex_col) && !is.null(sex_col),
        phr_txt("sex_col is required and cannot be NULL"),
        origin = origin
      )
      phrutils::phr_assert(
        !missing(age_years_col) && !is.null(age_years_col),
        phr_txt("age_years_col is required and cannot be NULL"),
        origin = origin
      )

      # Ensure fallback values using ensure_value function for values and labels only
      sex_male_val <- ensure_value(sex_male_val, "1")
      sex_female_val <- ensure_value(sex_female_val, "2")
      sex_male_lab <- ensure_value(sex_male_lab, "Male")
      sex_female_lab <- ensure_value(sex_female_lab, "Female")
      y_lab <- ensure_value(y_lab, "Age Group")

      # Create a working copy
      plot_data <- df

      # Handle age grouping
      if (age_grouping == FALSE) {
        phrutils::phr_validate_columns(
          plot_data,
          age_years_col,
          origin = origin,
          hint = phr_txt("Ensure age_years_col column exists in the dataset"),
          soft = FALSE
        )
        plot_data <- plot_data |>
          dplyr::mutate(
            age_group_plot = cut(
              as.numeric(!!rlang::sym(age_years_col)),
              breaks = c(
                -1,
                4,
                9,
                14,
                19,
                24,
                29,
                34,
                39,
                44,
                49,
                54,
                59,
                64,
                69,
                74,
                79,
                84,
                Inf
              ),
              labels = c(
                "0-4",
                "5-9",
                "10-14",
                "15-19",
                "20-24",
                "25-29",
                "30-34",
                "35-39",
                "40-44",
                "45-49",
                "50-54",
                "55-59",
                "60-64",
                "65-69",
                "70-74",
                "75-79",
                "80-84",
                "85+"
              )
            )
          )
      } else {
        phrutils::phr_validate_not_null(
          age_group_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          plot_data,
          age_group_col,
          origin = origin,
          hint = phr_txt(paste0(
            "When age_grouping=TRUE, '",
            age_group_col,
            "' column must exist in the dataset"
          )),
          soft = FALSE
        )
        plot_data <- plot_data |>
          dplyr::mutate(age_group_plot = !!rlang::sym(age_group_col))
      }

      # Validate sex column
      phrutils::phr_validate_columns(
        plot_data,
        sex_col,
        origin = origin,
        hint = phr_txt("Ensure sex_col column exists in the dataset"),
        soft = FALSE
      )

      # Convert sex column to character for matching
      plot_data <- plot_data |>
        dplyr::mutate(sex_plot = as.character(!!rlang::sym(sex_col)))

      # Handle weights column if provided
      use_weights <- FALSE
      if (!is.null(weights_col)) {
        phrutils::phr_validate_columns(
          plot_data,
          weights_col,
          origin = origin,
          hint = phr_txt("Ensure weights_col column exists in the dataset"),
          soft = FALSE
        )

        # Validate that weights column is numeric
        weights_values <- plot_data[[weights_col]]
        phrutils::phr_validate_all_numeric(
          weights_values,
          origin = origin,
          soft = FALSE
        )

        # Filter out NA weights and inform user
        n_na_weights <- sum(is.na(weights_values))
        if (n_na_weights > 0) {
          phrutils::phr_message(
            phr_txt(paste0(
              "Removing ",
              n_na_weights,
              " rows with NA values in weights_col '",
              weights_col,
              "'"
            )),
            origin = origin
          )
          plot_data <- plot_data |>
            dplyr::filter(!is.na(!!rlang::sym(weights_col)))
        }

        # Rename weight column for use in calculations
        plot_data <- plot_data |>
          dplyr::mutate(weight_plot = !!rlang::sym(weights_col))

        use_weights <- weighted_result

        if (weighted_result) {
          phrutils::phr_message(
            phr_txt(paste0(
              "Using weighted counts from '",
              weights_col,
              "' column"
            )),
            origin = origin
          )
        }
      } else {
        if (weighted_result) {
          phrutils::phr_message(
            phr_txt(
              "weighted_result=TRUE but no weights_col provided. Using unweighted counts."
            ),
            origin = origin
          )
        }
        # Create a weight column with value 1 for all rows (unweighted)
        plot_data <- plot_data |>
          dplyr::mutate(weight_plot = 1)
      }

      # Get unique sex values from data
      unique_sex <- unique(plot_data$sex_plot[!is.na(plot_data$sex_plot)])

      # Validate that sex_male_val exists in the data
      if (!as.character(sex_male_val) %in% unique_sex) {
        phrutils::phr_assert(
          FALSE,
          phr_txt(paste0(
            "sex_male_val '",
            sex_male_val,
            "' not found in ",
            sex_col,
            " column. Available values: ",
            paste(unique_sex, collapse = ", ")
          )),
          origin = origin
        )
      }

      # Validate that sex_female_val exists in the data
      if (!as.character(sex_female_val) %in% unique_sex) {
        phrutils::phr_assert(
          FALSE,
          phr_txt(paste0(
            "sex_female_val '",
            sex_female_val,
            "' not found in ",
            sex_col,
            " column. Available values: ",
            paste(unique_sex, collapse = ", ")
          )),
          origin = origin
        )
      }

      # Check if there are other sex values in the data
      other_sex_values <- setdiff(
        unique_sex,
        c(as.character(sex_male_val), as.character(sex_female_val))
      )
      if (length(other_sex_values) > 0) {
        phrutils::phr_message(
          phr_txt(paste0(
            "Note: Additional sex values found in data that will be excluded from plot: ",
            paste(other_sex_values, collapse = ", ")
          )),
          origin = origin
        )
      }

      # Filter data to only include specified sex values and remove NA values
      plot_data <- plot_data |>
        dplyr::filter(
          .data$sex_plot %in%
            c(as.character(sex_male_val), as.character(sex_female_val))
        ) |>
        dplyr::filter(!is.na(.data$age_group_plot), !is.na(.data$sex_plot))

      # Check if data is empty after filtering
      if (nrow(plot_data) == 0) {
        phrutils::phr_assert(
          FALSE,
          phr_txt(
            "No valid data remains after filtering for specified sex values and removing NAs"
          ),
          origin = origin
        )
      }

      # Create sex_values mapping for processing
      sex_values <- setNames(
        c(sex_male_lab, sex_female_lab),
        c(as.character(sex_male_val), as.character(sex_female_val))
      )

      # Calculate counts/weighted counts by age group and sex
      if (use_weights) {
        # Weighted counts
        age_sex_counts <- plot_data |>
          dplyr::group_by(.data$age_group_plot, .data$sex_plot) |>
          dplyr::summarise(
            count = sum(.data$weight_plot, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # Unweighted counts
        age_sex_counts <- plot_data |>
          dplyr::group_by(.data$age_group_plot, .data$sex_plot) |>
          dplyr::summarise(count = dplyr::n(), .groups = "drop")
      }

      # Calculate percentage across ALL age and sex categories (sums to 100% total)
      age_sex_counts <- age_sex_counts |>
        dplyr::mutate(
          pct = .data$count / sum(.data$count, na.rm = TRUE) * 100
        ) |>
        dplyr::mutate(
          pct = ifelse(
            .data$sex_plot == as.character(sex_male_val),
            -.data$pct,
            .data$pct
          )
        )

      # Apply sex labels
      age_sex_counts <- age_sex_counts |>
        dplyr::mutate(sex_label = dplyr::recode(.data$sex_plot, !!!sex_values))

      # Determine what to plot: percentages or counts
      if (proportional) {
        age_sex_counts <- age_sex_counts |>
          dplyr::mutate(plot_value = .data$pct)
      } else {
        # For counts, make male counts negative
        age_sex_counts <- age_sex_counts |>
          dplyr::mutate(
            plot_value = ifelse(
              .data$sex_plot == as.character(sex_male_val),
              -.data$count,
              .data$count
            )
          )
      }

      # Create subtitle with n by sex (unweighted count for n display)
      n_by_sex <- plot_data |>
        dplyr::group_by(.data$sex_plot) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(
          sex_label = dplyr::recode(.data$sex_plot, !!!sex_values)
        ) |>
        dplyr::mutate(
          sex_label = sprintf("%s (n=%d)", .data$sex_label, .data$n)
        ) |>
        dplyr::arrange(.data$sex_plot)

      auto_subtitle <- paste(n_by_sex$sex_label, collapse = "; ")

      # Add weighted note to subtitle if using weights
      if (use_weights) {
        auto_subtitle <- paste0(auto_subtitle, "; Weighted")
      }

      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Get colors
      colors <- phrutils::get_color_palette(type = color_palette, n = 2)
      names(colors) <- c(sex_male_lab, sex_female_lab)

      # Set default x_lab if not provided
      if (is.null(x_lab) || length(x_lab) == 0) {
        if (proportional) {
          x_lab <- if (use_weights) {
            "Population % (Weighted)"
          } else {
            "Population % (Unweighted)"
          }
        } else {
          x_lab <- if (use_weights) "Weighted Count" else "Count"
        }
      }

      # Determine axis limits and breaks based on proportional or count
      if (proportional) {
        # For percentages, use fixed breaks
        max_pct <- max(abs(age_sex_counts$plot_value), na.rm = TRUE)
        # Round up to nearest 5
        max_limit <- ceiling(max_pct / 5) * 5

        axis_breaks <- seq(-max_limit, max_limit, by = 5)
        minor_breaks <- seq(-max_limit, max_limit, by = 1)
        axis_labels <- function(x) paste0(abs(x), "%")
      } else {
        # For counts, use pretty breaks
        max_count <- max(abs(age_sex_counts$plot_value), na.rm = TRUE)
        axis_breaks <- pretty(c(-max_count, max_count))
        minor_breaks <- NULL
        axis_labels <- function(x) abs(x)
      }

      # Create the plot
      g <- ggplot2::ggplot(
        age_sex_counts,
        ggplot2::aes(
          x = .data$age_group_plot,
          y = .data$plot_value,
          fill = .data$sex_label
        )
      ) +
        ggplot2::geom_bar(stat = "identity", width = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          labels = axis_labels,
          breaks = axis_breaks,
          minor_breaks = minor_breaks
        ) +
        ggplot2::scale_fill_manual(name = "Sex", values = colors) +
        ggplot2::labs(
          x = y_lab,
          y = x_lab,
          subtitle = final_subtitle
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = if (proportional) {
            ggplot2::element_line(color = "grey90")
          } else {
            ggplot2::element_blank()
          },
          axis.text.y = ggplot2::element_text(size = 10),
          legend.position = legend_position
        )

      if (!is.null(title_name)) {
        g <- g +
          ggplot2::ggtitle(title_name) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              hjust = 0.5,
              size = 14,
              face = "bold"
            )
          )
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Age-Sex Pyramid"), "-", variable_label)
        g <- g +
          ggplot2::ggtitle(auto_title) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              hjust = 0.5,
              size = 14,
              face = "bold"
            )
          )
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Age Distribution
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param by_group_col Column name for grouping/faceting the plots. By default: NULL (no grouping)
#' @param year_or_month Specify whether to use "year" or "month" for age. By default: NULL (uses "year")
#' @param age_years_col Column name for age in years. By default: "age_years"
#' @param age_months_col Column name for age in months. By default: "age_months"
#' @param min_age Minimum age to display. By default: NULL (0 for years, 0 for months)
#' @param max_age Maximum age to display. By default: NULL (5 for years, 59 for months)
#' @param breaks Bin width for histogram. By default: NULL (1 for years, 12 for months)
#' @param color_palette Color palette to use. By default: "reach2"
#' @param title_name Title of the plot. By default: NULL
#' @param variable_label Optional character string. Human-readable label for the age variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Subtitle of the plot. By default: NULL (auto-generates n count)
#' @param x_lab Label for x-axis. By default: NULL (auto-generates based on year_or_month)
#' @param y_lab Label for y-axis. By default: NULL (falls back to "Count")
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#' @return A histogram plot of age distribution
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_age_distribution(survey_design = children_data)
#'   plot_age_distribution(survey_design = children_data, year_or_month = "month",
#'                         min_age = 0, max_age = 59)
#' }
plot_age_distribution <- function(
  survey_design,
  by_group_col = NULL,
  year_or_month = NULL,
  age_years_col = "age_years",
  age_months_col = "age_months",
  min_age = NULL,
  max_age = NULL,
  breaks = NULL,
  color_palette = "reach2",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_age_distribution"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )

      if (!is.null(by_group_col)) {
        phrutils::phr_validate_columns(
          df,
          by_group_col,
          origin = origin,
          hint = phr_txt("Ensure grouping column exists in the dataset"),
          soft = FALSE
        )
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Ensure fallback value for y_lab
      y_lab <- ensure_value(y_lab, if (weighted) "Weighted Count" else "Count")

      # Get color for histogram
      hist_color <- phrutils::get_color_palette(type = color_palette, n = 1)[1]

      if (is.null(year_or_month) | year_or_month == "year") {
        phrutils::phr_validate_columns(
          df,
          age_years_col,
          origin = origin,
          hint = phr_txt("Ensure age_years_col column exists in the dataset"),
          soft = FALSE
        )
        if (is.null(min_age)) {
          min_age <- 0
          phrutils::phr_message(
            phr_txt("No minimum age specified. Defaulting to 0 years."),
            origin = origin
          )
        }
        if (is.null(max_age)) {
          max_age <- 5
          phrutils::phr_message(
            phr_txt("No maximum age specified. Defaulting to 5 years."),
            origin = origin
          )
        }
        if (is.null(breaks)) {
          breaks <- 1
        }

        # Set default x_lab if not provided
        if (is.null(x_lab) || length(x_lab) == 0) {
          x_lab <- "Age (Years)"
        }

        df <- df |>
          dplyr::filter(
            !!rlang::sym(age_years_col) >= min_age &
              !!rlang::sym(age_years_col) <= max_age
          )

        # Create subtitle with n
        if (is.null(by_group_col)) {
          total_n <- nrow(df)
          auto_subtitle <- if (weighted) {
            sprintf(
              "n = %d (weighted n = %.0f)",
              total_n,
              sum(df[[weights_col]], na.rm = TRUE)
            )
          } else {
            sprintf("n = %d", total_n)
          }
          final_subtitle <- if (!is.null(subtitle)) {
            paste0(auto_subtitle, "; ", subtitle)
          } else {
            auto_subtitle
          }

          g <- ggplot2::ggplot(
            data = df,
            if (weighted) {
              ggplot2::aes(
                x = !!rlang::sym(age_years_col),
                weight = !!rlang::sym(weights_col)
              )
            } else {
              ggplot2::aes(x = !!rlang::sym(age_years_col))
            }
          ) +
            ggplot2::geom_histogram(
              binwidth = breaks,
              fill = hist_color,
              color = "white",
              linewidth = 0.5
            ) +
            ggplot2::scale_x_continuous(
              minor_breaks = seq(min_age, max_age, by = 1),
              breaks = seq(min_age, max_age, by = breaks),
              limits = c(min_age, max_age)
            ) +
            ggplot2::labs(x = x_lab, y = y_lab, subtitle = final_subtitle)
        } else {
          # Create subtitle with n by group
          n_by_group <- df |>
            dplyr::group_by(!!rlang::sym(by_group_col)) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(
              group_label = sprintf(
                "%s (n=%d)",
                !!rlang::sym(by_group_col),
                .data$n
              )
            )

          auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
          if (weighted) {
            auto_subtitle <- paste0(auto_subtitle, " (weighted)")
          }
          final_subtitle <- if (!is.null(subtitle)) {
            paste0(auto_subtitle, "; ", subtitle)
          } else {
            auto_subtitle
          }

          g <- ggplot2::ggplot(
            data = df,
            if (weighted) {
              ggplot2::aes(
                x = !!rlang::sym(age_years_col),
                weight = !!rlang::sym(weights_col)
              )
            } else {
              ggplot2::aes(x = !!rlang::sym(age_years_col))
            }
          ) +
            ggplot2::geom_histogram(
              binwidth = breaks,
              fill = hist_color,
              color = "white",
              linewidth = 0.5
            ) +
            ggplot2::scale_x_continuous(
              minor_breaks = seq(min_age, max_age, by = 1),
              breaks = seq(min_age, max_age, by = breaks),
              limits = c(min_age, max_age)
            ) +
            ggplot2::facet_wrap(~ get(by_group_col), ncol = 1) +
            ggplot2::labs(x = x_lab, y = y_lab, subtitle = final_subtitle)
        }
      }

      if (year_or_month == "month") {
        phrutils::phr_validate_columns(
          df,
          age_months_col,
          origin = origin,
          hint = phr_txt("Ensure age_months_col column exists in the dataset"),
          soft = FALSE
        )
        if (is.null(min_age)) {
          min_age <- 0
          phrutils::phr_message(
            phr_txt("No minimum age specified. Defaulting to 0 months."),
            origin = origin
          )
        }
        if (is.null(max_age)) {
          max_age <- 59
          phrutils::phr_message(
            phr_txt("No maximum age specified. Defaulting to 59 months."),
            origin = origin
          )
        }
        if (is.null(breaks)) {
          breaks <- 12
        }

        # Set default x_lab if not provided
        if (is.null(x_lab) || length(x_lab) == 0) {
          x_lab <- "Age (Months)"
        }

        df <- df |>
          dplyr::mutate(
            !!rlang::sym(age_months_col) := as.numeric(
              !!rlang::sym(age_months_col)
            )
          ) |>
          dplyr::filter(
            !!rlang::sym(age_months_col) >= min_age &
              !!rlang::sym(age_months_col) <= max_age
          )

        # Create subtitle with n
        if (is.null(by_group_col)) {
          total_n <- nrow(df)
          auto_subtitle <- if (weighted) {
            sprintf(
              "n = %d (weighted n = %.0f)",
              total_n,
              sum(df[[weights_col]], na.rm = TRUE)
            )
          } else {
            sprintf("n = %d", total_n)
          }
          final_subtitle <- if (!is.null(subtitle)) {
            paste0(auto_subtitle, "; ", subtitle)
          } else {
            auto_subtitle
          }

          g <- ggplot2::ggplot(
            data = df,
            if (weighted) {
              ggplot2::aes(
                x = !!rlang::sym(age_months_col),
                weight = !!rlang::sym(weights_col)
              )
            } else {
              ggplot2::aes(x = !!rlang::sym(age_months_col))
            }
          ) +
            ggplot2::geom_histogram(
              binwidth = breaks,
              fill = hist_color,
              color = "white",
              linewidth = 0.5
            ) +
            ggplot2::scale_x_continuous(
              minor_breaks = seq(min_age, max_age, by = 1),
              breaks = seq(min_age, max_age, by = breaks),
              limits = c(min_age, max_age)
            ) +
            ggplot2::labs(x = x_lab, y = y_lab, subtitle = final_subtitle)
        } else {
          # Create subtitle with n by group
          n_by_group <- df |>
            dplyr::group_by(!!rlang::sym(by_group_col)) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(
              group_label = sprintf(
                "%s (n=%d)",
                !!rlang::sym(by_group_col),
                .data$n
              )
            )

          auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
          if (weighted) {
            auto_subtitle <- paste0(auto_subtitle, " (weighted)")
          }
          final_subtitle <- if (!is.null(subtitle)) {
            paste0(auto_subtitle, "; ", subtitle)
          } else {
            auto_subtitle
          }

          g <- ggplot2::ggplot(
            data = df,
            if (weighted) {
              ggplot2::aes(
                x = !!rlang::sym(age_months_col),
                weight = !!rlang::sym(weights_col)
              )
            } else {
              ggplot2::aes(x = !!rlang::sym(age_months_col))
            }
          ) +
            ggplot2::geom_histogram(
              binwidth = breaks,
              fill = hist_color,
              color = "white",
              linewidth = 0.5
            ) +
            ggplot2::scale_x_continuous(
              minor_breaks = seq(min_age, max_age, by = 1),
              breaks = seq(min_age, max_age, by = breaks),
              limits = c(min_age, max_age)
            ) +
            ggplot2::facet_wrap(~ get(by_group_col), ncol = 1) +
            ggplot2::labs(x = x_lab, y = y_lab, subtitle = final_subtitle)
        }
      }

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Age Distribution of"), variable_label)
        if (!is.null(by_group_col)) {
          group_lbl <- grouping_label %||% by_group_col
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      # Apply consistent theme
      g <- g +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = legend_position)

      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Ridge Distribution
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param numeric_cols a vector of the same fsl indicator score columns
#' By default: NULL.
#' @param numeric_cols_labels Optional vector of labels for numeric_cols. Must be same length as numeric_cols.
#' By default: NULL (uses column names).
#' @param name_groups Name of the groups. By default: "Groups"
#' @param name_units Name of the units. By default: "Units"
#' @param grouping Variable name from the output create_fsl_flags for grouping
#' @param color_palette Color palette to use. By default: "reach3"
#' @param title_name Title of the plot
#' @param variable_label Optional character string. Human-readable label for the numeric variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Subtitle of the plot
#' @param x_lab Label for x-axis. By default: NULL (uses name_units).
#' @param y_lab Label for y-axis. By default: NULL (uses name_groups).
#' @param legend_position Position of the legend. By default: "none". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#'
#' @return A Ridge Plot with the distribution of values
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_ridge_distribution(df, numeric_cols = c("fcs_score", "rcsi_score"),
#'                           numeric_cols_labels = c("FCS", "rCSI"))
#' }

plot_ridge_distribution <- function(
  survey_design,
  numeric_cols = NULL,
  numeric_cols_labels = NULL,
  name_groups = NULL,
  name_units = NULL,
  grouping = NULL,
  color_palette = "reach3",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "none",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_ridge_distribution"

  phrutils::phr_try(
    {
      if (!requireNamespace("ggridges", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'ggridges' is required for plot_ridge_distribution(). Install it with: install.packages('ggridges')"
          ),
          origin = origin
        )
      }

      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(
        numeric_cols,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        numeric_cols,
        origin = origin,
        hint = phr_txt("Ensure all numeric columns exist in the dataset"),
        soft = FALSE
      )

      # Validate numeric_cols_labels if provided
      if (!is.null(numeric_cols_labels)) {
        phrutils::phr_assert(
          length(numeric_cols_labels) == length(numeric_cols),
          phr_txt(paste0(
            "numeric_cols_labels must have the same length as numeric_cols. ",
            "Expected: ",
            length(numeric_cols),
            ", Got: ",
            length(numeric_cols_labels)
          )),
          origin = origin
        )
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Apply defaults for name_groups and name_units if not provided
      if (is.null(name_groups)) {
        name_groups <- "groups"
      }
      if (is.null(name_units)) {
        name_units <- "units"
      }

      a <- 0

      if (is.null(grouping)) {
        df <- df |> dplyr::mutate(group = "All")
        grouping <- "group"
        a <- 1
      } else {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt("Ensure grouping column exists in the dataset"),
          soft = FALSE
        )
      }

      # Create subtitle with n
      if (a == 1) {
        # Overall plot
        total_n <- nrow(df)
        auto_subtitle <- if (weighted) {
          sprintf(
            "n = %d (weighted n = %.0f)",
            total_n,
            sum(df[[weights_col]], na.rm = TRUE)
          )
        } else {
          sprintf("n = %d", total_n)
        }
      } else {
        # Grouped plot
        n_by_group <- df |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(
            group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), .data$n)
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      select_cols <- if (weighted) {
        c(grouping, numeric_cols, weights_col)
      } else {
        c(grouping, numeric_cols)
      }
      df <- df |>
        dplyr::select(dplyr::all_of(select_cols)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(numeric_cols),
          names_to = name_groups,
          values_to = name_units
        )

      # Apply custom labels if provided
      if (!is.null(numeric_cols_labels)) {
        label_mapping <- setNames(numeric_cols_labels, numeric_cols)
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(name_groups) := dplyr::recode(
              !!rlang::sym(name_groups),
              !!!label_mapping
            )
          )
      }

      # Get colors based on number of groups
      n_colors <- length(unique(df[[name_groups]]))
      colors <- phrutils::get_color_palette(type = color_palette, n = n_colors)

      g <- ggplot2::ggplot(
        df,
        if (weighted) {
          ggplot2::aes(
            x = get(name_units),
            y = get(name_groups),
            fill = get(name_groups),
            weight = !!rlang::sym(weights_col)
          )
        } else {
          ggplot2::aes(
            x = get(name_units),
            y = get(name_groups),
            fill = get(name_groups)
          )
        }
      ) +
        ggridges::geom_density_ridges() +
        ggplot2::scale_fill_manual(values = colors) +
        ggridges::theme_ridges() +
        ggplot2::xlab(ensure_value(x_lab, name_units)) +
        ggplot2::ylab(ensure_value(y_lab, name_groups)) +
        ggplot2::theme(
          legend.position = legend_position,
          legend.title = ggplot2::element_text(name_groups)
        ) +
        ggplot2::labs(subtitle = final_subtitle)
      if (a == 0) {
        g <- g + ggplot2::facet_wrap(~ get(grouping))
      }
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Distribution of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Ridge Distribution by Group
#'
#' Creates a ridge plot showing the distribution of a single numeric variable overall and
#' for each level of a grouping variable. The overall distribution is placed at the top,
#' followed by one ridge per group, all vertically stacked (not faceted).
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param numeric_col A character string specifying the name of the numeric column to plot.
#' @param grouping A character string specifying the column name for the grouping variable.
#' @param overall_label Character. Label for the overall distribution ridge. Default: "Overall".
#' @param color_palette Color palette to use. By default: "reach3".
#' @param title_name Optional title of the plot.
#' @param variable_label Optional character string. Human-readable label for the numeric variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Optional subtitle (appended to auto-generated n counts).
#' @param x_lab Optional x-axis label. Defaults to the column name.
#' @param y_lab Optional y-axis label. Defaults to the grouping column name.
#' @param legend_position Position of the legend. By default: "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights.
#'   Required when weighted = TRUE.
#'
#' @return A ggplot2 ridge plot with the overall distribution at the top followed by each group.
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_ridge_distribution_by_group(df, numeric_col = "fcs_score", grouping = "district")
#' }

plot_ridge_distribution_by_group <- function(
  survey_design,
  numeric_col,
  grouping,
  overall_label = "Overall",
  color_palette = "reach3",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "none",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_ridge_distribution_by_group"

  phrutils::phr_try(
    {
      if (!requireNamespace("ggridges", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'ggridges' is required for plot_ridge_distribution_by_group(). Install it with: install.packages('ggridges')"
          ),
          origin = origin
        )
      }

      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_not_null(
        numeric_col,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(grouping, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        numeric_col,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(grouping, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )

      phrutils::phr_validate_columns(
        df,
        numeric_col,
        origin = origin,
        hint = phr_txt("Ensure the numeric column exists in the dataset"),
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        grouping,
        origin = origin,
        hint = phr_txt("Ensure the grouping column exists in the dataset"),
        soft = FALSE
      )

      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Build subtitle
      total_n <- nrow(df)
      n_by_group <- df |>
        dplyr::group_by(!!rlang::sym(grouping)) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(
          group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), .data$n)
        )
      auto_subtitle <- paste(
        c(sprintf("Overall (n=%d)", total_n), n_by_group$group_label),
        collapse = "; "
      )
      if (weighted) {
        auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Create overall rows
      df_overall <- df |>
        dplyr::select(dplyr::all_of(c(
          numeric_col,
          if (weighted) weights_col else character(0)
        ))) |>
        dplyr::mutate(.group_label = overall_label)

      # Create grouped rows
      df_grouped <- df |>
        dplyr::select(dplyr::all_of(c(
          numeric_col,
          grouping,
          if (weighted) weights_col else character(0)
        ))) |>
        dplyr::rename(.group_label = !!rlang::sym(grouping)) |>
        dplyr::mutate(.group_label = as.character(.data$.group_label))

      # Combine overall and grouped data; overall appears as the topmost ridge
      df_combined <- dplyr::bind_rows(df_overall, df_grouped)

      # Factor levels: overall first, then sorted group values so the overall ridge
      # appears at the top when ggridges stacks bottom-to-top.
      group_levels_sorted <- sort(unique(df_grouped$.group_label))
      # ggridges stacks bottom-to-top; first factor level is at the bottom, last at the top.
      # We want: Overall at top, then groups below. So factor order is: groups (bottom up),
      # then Overall at top.
      df_combined$.group_label <- factor(
        df_combined$.group_label,
        levels = c(group_levels_sorted, overall_label)
      )

      # Get colors: one per level (overall + each group)
      n_levels <- length(levels(df_combined$.group_label))
      colors <- phrutils::get_color_palette(type = color_palette, n = n_levels)

      # Build the ridge plot
      g <- ggplot2::ggplot(
        df_combined,
        if (weighted) {
          ggplot2::aes(
            x = !!rlang::sym(numeric_col),
            y = .data$.group_label,
            fill = .data$.group_label,
            weight = !!rlang::sym(weights_col)
          )
        } else {
          ggplot2::aes(
            x = !!rlang::sym(numeric_col),
            y = .data$.group_label,
            fill = .data$.group_label
          )
        }
      ) +
        ggridges::geom_density_ridges() +
        ggplot2::scale_fill_manual(values = colors) +
        ggridges::theme_ridges() +
        ggplot2::xlab(ensure_value(x_lab, numeric_col)) +
        ggplot2::ylab(ensure_value(y_lab, grouping)) +
        ggplot2::theme(legend.position = legend_position) +
        ggplot2::labs(subtitle = final_subtitle)

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        group_lbl <- grouping_label %||% grouping
        auto_title <- paste0(
          phr_txt("Distribution of"),
          " ",
          variable_label,
          phr_txt(", by"),
          " ",
          group_lbl
        )
        g <- g + ggplot2::ggtitle(auto_title)
      }
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot the cumulative distribution of a numeric variable (e.g., MUAC or anthropometric z-scores).
#'
#' @description
#' Creates a cumulative distribution function (CDF) plot for a numeric variable such as MUAC or
#' anthropometric z-scores. Supports optional vertical reference lines (e.g., at clinical cutoffs),
#' grouping by a categorical variable, survey weighting, and axis customisation.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param data_var A character string specifying the column name containing the data to plot.
#' @param vline_intercepts Optional numeric vector specifying positions for vertical reference lines.
#'   For z-scores: default is c(-3, -2) representing severe and moderate cutoffs.
#'   For MUAC: default is c(11.5, 12.5) representing severe and moderate cutoffs.
#'   Can be set to NULL to disable reference lines.
#' @param vline_colors Optional character vector specifying colors for vertical reference lines.
#'   Must be same length as vline_intercepts. Default is c("red", "orange").
#' @param x_label Optional character string for the x-axis label. If NULL, uses data_var name.
#' @param y_label Optional character string for the y-axis label. Default: "\% Cumulative Proportion".
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits. If NULL, auto-detects based on data type.
#' @param breaks_by Optional numeric value for x-axis breaks interval. If NULL, auto-detects based on data type.
#' @param grouping Optional character string specifying a column name to group/color the curves by.
#' @param color_palette Character string specifying the color palette to use (default: "reach2").
#' @param title_name Optional character string for the plot title.
#' @param variable_label Optional character string. Human-readable label for the data variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Optional character string for additional subtitle text (will be appended to auto-generated n counts).
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#'
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with a z-score column
#' plot_cumulative_distribution(df, data_var = "wfhz")
#'
#' # With MUAC data
#' plot_cumulative_distribution(df, data_var = "muac")
#'
#' # With custom intercepts and grouping
#' plot_cumulative_distribution(df, data_var = "hfaz",
#'                             vline_intercepts = c(-2, 2),
#'                             grouping = "district")
#' }
#' @importFrom rlang .data
plot_cumulative_distribution <- function(
  survey_design,
  data_var,
  vline_intercepts = NULL,
  vline_colors = NULL,
  x_label = NULL,
  y_label = "% Cumulative Proportion",
  xlim = NULL,
  breaks_by = NULL,
  grouping = NULL,
  color_palette = "reach2",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_cumulative_distribution"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(data_var, origin = origin, soft = FALSE)
      phrutils::phr_validate_vector_length(
        data_var,
        exact_length = 1,
        origin = origin,
        soft = FALSE
      )

      # Validate column existence
      phrutils::phr_validate_columns(
        df,
        data_var,
        origin = origin,
        hint = phr_txt(
          "The data column '{data_var}' does not exist in the dataset"
        ),
        soft = FALSE
      )

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      n_before <- sum(!is.na(df[[data_var]]))
      df <- df |>
        dplyr::mutate(
          !!rlang::sym(data_var) := as.numeric(!!rlang::sym(data_var))
        )
      n_after <- sum(!is.na(df[[data_var]]))
      if (n_after < n_before) {
        phrutils::phr_warning(
          paste0(
            "Coercing '",
            data_var,
            "' to numeric introduced ",
            n_before - n_after,
            " NA value(s). Check that the column contains valid numbers."
          ),
          origin = origin
        )
      }

      # Set defaults based on data range if not provided
      if (is.null(xlim)) {
        data_range <- range(df[[data_var]], na.rm = TRUE)
        xlim <- data_range
      }

      if (is.null(breaks_by)) {
        breaks_by <- 0.5
      }

      # Set default vline_colors if not provided
      if (is.null(vline_colors) && !is.null(vline_intercepts)) {
        vline_colors <- rep(
          c("red", "orange"),
          length.out = length(vline_intercepts)
        )
      }

      # Validate vline parameters if provided
      if (!is.null(vline_intercepts)) {
        phrutils::phr_validate_all_numeric(
          vline_intercepts,
          origin = origin,
          soft = FALSE
        )
        if (!is.null(vline_colors)) {
          phrutils::phr_assert(
            is.character(vline_colors) &&
              length(vline_colors) == length(vline_intercepts),
            phr_txt(
              "vline_colors must be a character vector with same length as vline_intercepts"
            ),
            origin = origin
          )
        }
      }

      # Set x-axis label
      if (is.null(x_label)) {
        x_label <- data_var
      }

      # Create subtitle with n
      if (missing(grouping) || is.null(grouping)) {
        total_n <- sum(!is.na(df[[data_var]]))
        auto_subtitle <- if (weighted) {
          sprintf(
            "n = %d (weighted n = %.0f)",
            total_n,
            sum(df[[weights_col]], na.rm = TRUE)
          )
        } else {
          sprintf("n = %d", total_n)
        }
      } else {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(
            "Ensure the grouping column '{grouping}' exists in the dataset"
          ),
          soft = FALSE
        )

        n_by_group <- df |>
          dplyr::filter(!is.na(!!rlang::sym(data_var))) |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(
            group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), .data$n)
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Create base plot
      if (weighted) {
        # Compute weighted ECDF manually
        df_ecdf <- df |>
          dplyr::filter(
            !is.na(!!rlang::sym(data_var)) & !is.na(!!rlang::sym(weights_col))
          ) |>
          dplyr::arrange(!!rlang::sym(data_var)) |>
          dplyr::mutate(
            .w = !!rlang::sym(weights_col) /
              sum(!!rlang::sym(weights_col), na.rm = TRUE),
            .ecdf_val = cumsum(.w)
          )
        g <- ggplot2::ggplot(
          df_ecdf,
          ggplot2::aes(
            x = !!rlang::sym(data_var),
            y = .data$.ecdf_val,
            color = "Overall"
          )
        ) +
          ggplot2::geom_step()
      } else {
        g <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = !!rlang::sym(data_var), color = "Overall")
        ) +
          ggplot2::stat_ecdf(geom = "step")
      }

      # Add vertical lines if specified
      if (!is.null(vline_intercepts) && length(vline_intercepts) > 0) {
        for (i in seq_along(vline_intercepts)) {
          g <- g +
            ggplot2::geom_vline(
              xintercept = vline_intercepts[i],
              color = vline_colors[i]
            )
        }
      }

      if (!missing(grouping) && !is.null(grouping)) {
        if (weighted) {
          df_ecdf_grouped <- df |>
            dplyr::filter(
              !is.na(!!rlang::sym(data_var)) & !is.na(!!rlang::sym(weights_col))
            ) |>
            dplyr::group_by(!!rlang::sym(grouping)) |>
            dplyr::arrange(!!rlang::sym(data_var)) |>
            dplyr::mutate(
              .w = !!rlang::sym(weights_col) /
                sum(!!rlang::sym(weights_col), na.rm = TRUE),
              .ecdf_val = cumsum(.w)
            ) |>
            dplyr::ungroup()
          g <- g +
            ggplot2::geom_step(
              data = df_ecdf_grouped,
              ggplot2::aes(
                x = !!rlang::sym(data_var),
                y = .data$.ecdf_val,
                color = as.factor(!!rlang::sym(grouping)),
                group = !!rlang::sym(grouping)
              )
            )
        } else {
          g <- g +
            ggplot2::stat_ecdf(
              geom = "step",
              ggplot2::aes(
                x = !!rlang::sym(data_var),
                color = as.factor(!!rlang::sym(grouping)),
                group = !!rlang::sym(grouping)
              )
            )
        }
        g <- g + ggplot2::labs(color = paste0(grouping))
      }

      # Use color palette
      if (missing(grouping) || is.null(grouping)) {
        colors <- phrutils::get_color_palette(type = color_palette, n = 1)
      } else {
        values <- df |>
          dplyr::select(!!rlang::sym(grouping)) |>
          dplyr::pull() |>
          unique()
        n_colors <- length(values) + 1 # +1 for "Overall"
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )
      }

      g <- g +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = legend_position) +
        ggplot2::xlab(x_label) +
        ggplot2::ylab(y_label) +
        ggplot2::scale_x_continuous(
          limits = xlim,
          breaks = seq(xlim[1], xlim[2], by = breaks_by)
        ) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::labs(subtitle = final_subtitle)

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(
          phr_txt("Cumulative Distribution of"),
          variable_label
        )
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }
      options(warn = 0)

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Z-Score Distribution
#'
#' Plot the distribution of a z-score variable to visualize and assess the distribution
#' against a normal Gaussian curve.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param zscore_var A character string specifying the column name containing z-scores to plot.
#' @param vline_intercepts Optional numeric vector specifying positions for vertical reference lines.
#'   Default is c(-3, -2, 2, 3) representing standard WHO cutoffs.
#' @param vline_colors Optional character vector specifying colors for vertical reference lines.
#'   Must be same length as vline_intercepts. Default is c("red", "orange", "orange", "red").
#' @param x_label Optional character string for the x-axis label. If NULL, uses zscore_var name.
#' @param grouping Optional character string specifying a column name to group/color the density curves by.
#' @param color_palette Character string specifying the color palette to use (default: "reach3").
#' @param title_name Optional character string for the plot title.
#' @param variable_label Optional character string. Human-readable label for the z-score variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Optional character string for additional subtitle text (will be appended to auto-generated n counts).
#' @param y_lab Optional character string for the y-axis label. Default: NULL (auto-generated).
#'
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with a z-score column
#' plot_zscore_distribution(df, zscore_var = "wfhz")
#'
#' # With custom intercepts and grouping
#' plot_zscore_distribution(df, zscore_var = "hfaz",
#'                         vline_intercepts = c(-2, 2),
#'                         grouping = "district")
#' }
#' @importFrom rlang .data
#' @importFrom stats density
plot_zscore_distribution <- function(
  survey_design,
  zscore_var,
  vline_intercepts = c(-3, -2, 2, 3),
  vline_colors = c("red", "orange", "orange", "red"),
  x_label = NULL,
  grouping = NULL,
  color_palette = "reach3",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  y_lab = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_zscore_distribution"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        zscore_var,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_vector_length(
        zscore_var,
        exact_length = 1,
        origin = origin,
        soft = FALSE
      )

      # Validate column existence
      phrutils::phr_validate_columns(
        df,
        zscore_var,
        origin = origin,
        hint = phr_txt(
          "The z-score column '{zscore_var}' does not exist in the dataset"
        ),
        soft = FALSE
      )

      # Validate vline parameters if provided
      if (!is.null(vline_intercepts)) {
        phrutils::phr_validate_all_numeric(
          vline_intercepts,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_assert(
          is.character(vline_colors) &&
            length(vline_colors) == length(vline_intercepts),
          phr_txt(
            "vline_colors must be a character vector with same length as vline_intercepts"
          ),
          origin = origin
        )
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }
      options(warn = -1)

      # Set x-axis label
      if (is.null(x_label)) {
        x_label <- zscore_var
      }
      # Set y-axis label
      y_lab <- phrutils::ensure_value(y_lab, "Density")
      # Create subtitle with n
      if (is.null(grouping)) {
        total_n <- sum(!is.na(df[[zscore_var]]))
        auto_subtitle <- if (weighted) {
          sprintf(
            "n = %d (weighted n = %.0f)",
            total_n,
            sum(df[[weights_col]], na.rm = TRUE)
          )
        } else {
          sprintf("n = %d", total_n)
        }
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }
        data_norm <- data.frame(
          x = seq(from = -6, to = 6, by = 0.12),
          y = stats::dnorm(seq(from = -6, to = 6, by = 0.12), mean = 0, sd = 1)
        )
        # Get colors from palette
        colors <- phrutils::get_color_palette(type = color_palette, n = 2)

        g <- ggplot2::ggplot(df, ggplot2::aes(get(zscore_var))) +
          ggplot2::geom_histogram(
            if (weighted) {
              ggplot2::aes(
                x = get(zscore_var),
                y = ggplot2::after_stat(density),
                weight = !!rlang::sym(weights_col)
              )
            } else {
              ggplot2::aes(
                x = get(zscore_var),
                y = ggplot2::after_stat(density)
              )
            },
            bins = 100,
            fill = "#d3d3d3",
            color = "gray",
            alpha = 0.8
          ) +
          ggplot2::geom_density(
            if (weighted) {
              ggplot2::aes(weight = !!rlang::sym(weights_col))
            } else {
              NULL
            },
            color = colors[1],
            linewidth = 1
          ) +
          ggplot2::xlim(c(-6, 5)) +
          ggplot2::geom_line(
            data = data_norm,
            ggplot2::aes(x = .data$x, y = .data$y),
            color = "darkred",
            linewidth = 1.2
          )
        # Add vertical lines if specified
        if (!is.null(vline_intercepts) && length(vline_intercepts) > 0) {
          for (i in seq_along(vline_intercepts)) {
            g <- g +
              ggplot2::geom_vline(
                xintercept = vline_intercepts[i],
                color = vline_colors[i],
                linewidth = 0.5
              )
          }
        }

        g <- g +
          ggplot2::xlab(x_label) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(subtitle = final_subtitle)
      }
      if (!is.null(grouping)) {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(
            "Ensure the grouping column '{grouping}' exists in the dataset"
          ),
          soft = FALSE
        )

        # Create subtitle with n by group
        n_by_group <- df |>
          dplyr::filter(!is.na(!!rlang::sym(zscore_var))) |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(
            group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), .data$n)
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        if (weighted) {
          auto_subtitle <- paste0(auto_subtitle, " (weighted)")
        }
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        data_norm <- data.frame(
          x = seq(from = -6, to = 6, by = 0.12),
          y = stats::dnorm(seq(from = -6, to = 6, by = 0.12), mean = 0, sd = 1)
        )
        df <- df |> dplyr::rename(group_var = {{ grouping }})

        # Get colors from palette
        n_colors <- length(unique(df$group_var))
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )

        g <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = get(zscore_var), color = as.factor(get("group_var")))
        ) +
          ggplot2::geom_density(
            if (weighted) {
              ggplot2::aes(weight = !!rlang::sym(weights_col))
            } else {
              NULL
            },
            linewidth = 1
          ) +
          ggplot2::xlim(c(-6, 5)) +
          ggplot2::geom_line(
            data = data_norm,
            ggplot2::aes(x = .data$x, y = .data$y),
            color = "darkred",
            linewidth = 1.2
          )

        # Add vertical lines if specified
        if (!is.null(vline_intercepts) && length(vline_intercepts) > 0) {
          for (i in seq_along(vline_intercepts)) {
            g <- g +
              ggplot2::geom_vline(
                xintercept = vline_intercepts[i],
                color = vline_colors[i],
                linewidth = 0.5
              )
          }
        }

        g <- g +
          ggplot2::xlab(x_label) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_color_manual(values = colors) +
          ggplot2::labs(color = paste0(grouping), subtitle = final_subtitle)
      }

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Z-Score Distribution of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }
      options(warn = 0)

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot IYCF Area Graph
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param age_months_col Column name for age in months. By default: "age_months". Must be numeric.
#' @param iycf_ebf_col Column name for exclusive breastfeeding indicator. By default: "iycf_ebf"
#' @param iycf_4_col Column name for currently breastfeeding indicator (IYCF 4). By default: "iycf_4"
#'
#' @param iycf_6a_col Column name for plain water consumption (IYCF 6a). By default: "iycf_6a"
#' @param iycf_6b_col Column name for infant formula consumption frequency (IYCF 6b). By default: "iycf_6b". Must be numeric.
#' @param iycf_6c_col Column name for animal milk consumption frequency (IYCF 6c). By default: "iycf_6c". Must be numeric.
#' @param iycf_6d_col Column name for powdered milk consumption frequency (IYCF 6d). By default: "iycf_6d". Must be numeric.
#' @param iycf_6e_col Column name for juice consumption (IYCF 6e). By default: "iycf_6e"
#' @param iycf_6f_col Column name for broth consumption (IYCF 6f). By default: "iycf_6f"
#' @param iycf_6g_col Column name for yogurt drink consumption (IYCF 6g). By default: "iycf_6g"
#' @param iycf_6h_col Column name for thin porridge consumption (IYCF 6h). By default: "iycf_6h"
#' @param iycf_6i_col Column name for other liquids consumption (IYCF 6i). By default: "iycf_6i"
#' @param iycf_6j_col Column name for tea/coffee consumption (IYCF 6j). By default: "iycf_6j"
#'
#' @param iycf_7a_col Column name for yogurt (solid) consumption frequency (IYCF 7a). By default: "iycf_7a". Must be numeric.
#' @param iycf_7b_col Column name for grains/roots/tubers consumption (IYCF 7b). By default: "iycf_7b"
#' @param iycf_7c_col Column name for legumes/nuts consumption (IYCF 7c). By default: "iycf_7c"
#' @param iycf_7d_col Column name for dairy products consumption (IYCF 7d). By default: "iycf_7d"
#' @param iycf_7e_col Column name for meat/poultry consumption (IYCF 7e). By default: "iycf_7e"
#' @param iycf_7f_col Column name for eggs consumption (IYCF 7f). By default: "iycf_7f"
#' @param iycf_7g_col Column name for vitamin A rich fruits consumption (IYCF 7g). By default: "iycf_7g"
#' @param iycf_7h_col Column name for other fruits consumption (IYCF 7h). By default: "iycf_7h"
#' @param iycf_7i_col Column name for vitamin A rich vegetables consumption (IYCF 7i). By default: "iycf_7i"
#' @param iycf_7j_col Column name for other vegetables consumption (IYCF 7j). By default: "iycf_7j"
#' @param iycf_7k_col Column name for red palm oil consumption (IYCF 7k). By default: "iycf_7k"
#' @param iycf_7l_col Column name for oil/fats consumption (IYCF 7l). By default: "iycf_7l"
#' @param iycf_7m_col Column name for sweets consumption (IYCF 7m). By default: "iycf_7m"
#' @param iycf_7n_col Column name for condiments consumption (IYCF 7n). By default: "iycf_7n"
#' @param iycf_7o_col Column name for insects/grubs consumption (IYCF 7o). By default: "iycf_7o"
#' @param iycf_7p_col Column name for fish/seafood consumption (IYCF 7p). By default: "iycf_7p"
#' @param iycf_7q_col Column name for organ meat consumption (IYCF 7q). By default: "iycf_7q"
#' @param iycf_7r_col Column name for other foods consumption (IYCF 7r). By default: "iycf_7r"
#'
#' @param yes_val Value indicating "yes" in IYCF indicator columns. By default: 1
#' @param no_val Value indicating "no" in IYCF indicator columns. By default: 0
#' @param min_age_months Minimum age in months to include. By default: 0
#' @param max_age_months Maximum age in months to include (exclusive). By default: 24
#' @param color_palette Color palette to use. Options include "iycf_area" (default), "reach1", "reach2", "reach3", "reach4", "traffic_light", "sex", "group", "default". See [get_color_palette()] for all available palettes.
#' @param title_name Title of the plot. By default: NULL
#' @param variable_label Optional character string. Human-readable label for the plot variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Subtitle of the plot. By default: NULL
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#'
#' @return An area graph showing IYCF feeding practices by age group
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_iycf_areagraph(survey_design = iycf_data)
#' }

plot_iycf_areagraph <- function(
  survey_design,
  # Basic columns
  age_months_col = "age_months",
  iycf_ebf_col = "iycf_ebf",
  iycf_4_col = "iycf_4",

  # Liquid columns (IYCF 6a-6j)
  iycf_6a_col = "iycf_6a",
  iycf_6b_col = "iycf_6b",
  iycf_6c_col = "iycf_6c",
  iycf_6d_col = "iycf_6d",
  iycf_6e_col = "iycf_6e",
  iycf_6f_col = "iycf_6f",
  iycf_6g_col = "iycf_6g",
  iycf_6h_col = "iycf_6h",
  iycf_6i_col = "iycf_6i",
  iycf_6j_col = "iycf_6j",

  # Food columns (IYCF 7a-7r)
  iycf_7a_col = "iycf_7a",
  iycf_7b_col = "iycf_7b",
  iycf_7c_col = "iycf_7c",
  iycf_7d_col = "iycf_7d",
  iycf_7e_col = "iycf_7e",
  iycf_7f_col = "iycf_7f",
  iycf_7g_col = "iycf_7g",
  iycf_7h_col = "iycf_7h",
  iycf_7i_col = "iycf_7i",
  iycf_7j_col = "iycf_7j",
  iycf_7k_col = "iycf_7k",
  iycf_7l_col = "iycf_7l",
  iycf_7m_col = "iycf_7m",
  iycf_7n_col = "iycf_7n",
  iycf_7o_col = "iycf_7o",
  iycf_7p_col = "iycf_7p",
  iycf_7q_col = "iycf_7q",
  iycf_7r_col = "iycf_7r",

  # Values and ranges
  yes_val = 1,
  no_val = 0,
  min_age_months = 0,
  max_age_months = 24,

  # Plot customization
  color_palette = "iycf_area",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_iycf_areagraph"

  phrutils::phr_try(
    {
      if (!requireNamespace("ggnewscale", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'ggnewscale' is required for plot_iycf_areagraph(). Install it with: install.packages('ggnewscale')"
          ),
          origin = origin
        )
      }

      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        age_months_col,
        origin = origin,
        hint = phr_txt(paste0(
          "Age in months column '",
          age_months_col,
          "' is required to create the IYCF Area Graph but is not in your data. Please check your input."
        )),
        soft = FALSE
      )

      # Validate age column is numeric or safely coercible to numeric
      phrutils::phr_validate_all_numeric(
        df[[age_months_col]],
        origin = origin,
        soft = FALSE
      )

      # Collect all required columns
      liquid_cols <- c(
        iycf_6a_col,
        iycf_6b_col,
        iycf_6c_col,
        iycf_6d_col,
        iycf_6e_col,
        iycf_6f_col,
        iycf_6g_col,
        iycf_6h_col,
        iycf_6i_col,
        iycf_6j_col
      )

      food_cols <- c(
        iycf_7a_col,
        iycf_7b_col,
        iycf_7c_col,
        iycf_7d_col,
        iycf_7e_col,
        iycf_7f_col,
        iycf_7g_col,
        iycf_7h_col,
        iycf_7i_col,
        iycf_7j_col,
        iycf_7k_col,
        iycf_7l_col,
        iycf_7m_col,
        iycf_7n_col,
        iycf_7o_col,
        iycf_7p_col,
        iycf_7q_col,
        iycf_7r_col
      )

      area_graph_vars <- c(iycf_ebf_col, iycf_4_col, liquid_cols, food_cols)

      # Validate all required IYCF columns exist
      phrutils::phr_validate_columns(
        df,
        area_graph_vars,
        origin = origin,
        hint = phr_txt(
          "You don't have all the required IYCF variables to create the IYCF Area Graph. Please check your input."
        ),
        soft = FALSE
      )

      # Validate frequency columns (6b, 6c, 6d, 7a) are numeric or safely coercible
      frequency_cols <- c(iycf_6b_col, iycf_6c_col, iycf_6d_col, iycf_7a_col)
      frequency_names <- c(
        "iycf_6b (infant formula)",
        "iycf_6c (animal milk)",
        "iycf_6d (powdered milk)",
        "iycf_7a (yogurt)"
      )

      for (i in seq_along(frequency_cols)) {
        col <- frequency_cols[i]
        col_name <- frequency_names[i]
        phrutils::phr_validate_all_numeric(
          df[[col]],
          origin = origin,
          soft = FALSE
        )
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Store original count before filtering
      original_n <- nrow(df)

      # Coerce age to numeric
      df <- df |>
        dplyr::mutate(
          !!rlang::sym(age_months_col) := as.numeric(
            !!rlang::sym(age_months_col)
          )
        )

      # Coerce frequency columns to numeric
      df <- df |>
        dplyr::mutate(
          !!rlang::sym(iycf_6b_col) := as.numeric(!!rlang::sym(iycf_6b_col)),
          !!rlang::sym(iycf_6c_col) := as.numeric(!!rlang::sym(iycf_6c_col)),
          !!rlang::sym(iycf_6d_col) := as.numeric(!!rlang::sym(iycf_6d_col)),
          !!rlang::sym(iycf_7a_col) := as.numeric(!!rlang::sym(iycf_7a_col))
        )

      # Filter by age and create age groups
      df <- df |>
        dplyr::filter(!is.na(!!rlang::sym(age_months_col))) |>
        dplyr::filter(
          !!rlang::sym(age_months_col) >= min_age_months &
            !!rlang::sym(age_months_col) < max_age_months
        ) |>
        mutate(
          age_group = ifelse(
            !!rlang::sym(age_months_col) <= 1,
            "0-1 months",
            ifelse(
              !!rlang::sym(age_months_col) <= 3,
              "2-3 months",
              ifelse(
                !!rlang::sym(age_months_col) <= 5,
                "4-5 months",
                ifelse(
                  !!rlang::sym(age_months_col) <= 7,
                  "6-7 months",
                  ifelse(
                    !!rlang::sym(age_months_col) <= 9,
                    "8-9 months",
                    ifelse(
                      !!rlang::sym(age_months_col) <= 11,
                      "10-11 months",
                      ifelse(
                        !!rlang::sym(age_months_col) <= 13,
                        "12-13 months",
                        ifelse(
                          !!rlang::sym(age_months_col) <= 15,
                          "14-15 months",
                          ifelse(
                            !!rlang::sym(age_months_col) <= 17,
                            "16-17 months",
                            ifelse(
                              !!rlang::sym(age_months_col) <= 19,
                              "18-19 months",
                              ifelse(
                                !!rlang::sym(age_months_col) <= 21,
                                "20-21 months",
                                ifelse(
                                  !!rlang::sym(age_months_col) <= 23,
                                  "22-23 months",
                                  ""
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

      # Store filtered count for subtitle
      filtered_n <- nrow(df)
      if (weighted) {
        total_weighted_n <- sum(df[[weights_col]], na.rm = TRUE)
      }

      # Non-milk liquid columns (6e-6j: exclude water, formula, animal milk, powdered milk)
      non_milk_liquid_cols <- c(
        iycf_6e_col,
        iycf_6f_col,
        iycf_6g_col,
        iycf_6h_col,
        iycf_6i_col,
        iycf_6j_col
      )

      # Create helper variables
      # For frequency columns (6b, 6c, 6d, 7a), treat any value > 0 as "yes"
      df <- df |>
        mutate(
          # Any food consumed (any of 7a-7r)
          # Note: 7a is frequency, so check if > 0
          any_food = ifelse(
            (!!rlang::sym(iycf_7a_col) > 0 &
              !is.na(!!rlang::sym(iycf_7a_col))) |
              !!rlang::sym(iycf_7b_col) == yes_val |
              !!rlang::sym(iycf_7c_col) == yes_val |
              !!rlang::sym(iycf_7d_col) == yes_val |
              !!rlang::sym(iycf_7e_col) == yes_val |
              !!rlang::sym(iycf_7f_col) == yes_val |
              !!rlang::sym(iycf_7g_col) == yes_val |
              !!rlang::sym(iycf_7h_col) == yes_val |
              !!rlang::sym(iycf_7i_col) == yes_val |
              !!rlang::sym(iycf_7j_col) == yes_val |
              !!rlang::sym(iycf_7k_col) == yes_val |
              !!rlang::sym(iycf_7l_col) == yes_val |
              !!rlang::sym(iycf_7m_col) == yes_val |
              !!rlang::sym(iycf_7n_col) == yes_val |
              !!rlang::sym(iycf_7o_col) == yes_val |
              !!rlang::sym(iycf_7p_col) == yes_val |
              !!rlang::sym(iycf_7q_col) == yes_val |
              !!rlang::sym(iycf_7r_col) == yes_val,
            1,
            0
          ),
          any_food = ifelse(is.na(any_food), 0, any_food),

          # No food consumed (none of 7a-7r)
          no_food = ifelse(
            (!!rlang::sym(iycf_7a_col) <= 0 |
              is.na(!!rlang::sym(iycf_7a_col))) &
              !!rlang::sym(iycf_7b_col) != yes_val &
              !!rlang::sym(iycf_7c_col) != yes_val &
              !!rlang::sym(iycf_7d_col) != yes_val &
              !!rlang::sym(iycf_7e_col) != yes_val &
              !!rlang::sym(iycf_7f_col) != yes_val &
              !!rlang::sym(iycf_7g_col) != yes_val &
              !!rlang::sym(iycf_7h_col) != yes_val &
              !!rlang::sym(iycf_7i_col) != yes_val &
              !!rlang::sym(iycf_7j_col) != yes_val &
              !!rlang::sym(iycf_7k_col) != yes_val &
              !!rlang::sym(iycf_7l_col) != yes_val &
              !!rlang::sym(iycf_7m_col) != yes_val &
              !!rlang::sym(iycf_7n_col) != yes_val &
              !!rlang::sym(iycf_7o_col) != yes_val &
              !!rlang::sym(iycf_7p_col) != yes_val &
              !!rlang::sym(iycf_7q_col) != yes_val &
              !!rlang::sym(iycf_7r_col) != yes_val,
            1,
            0
          ),
          no_food = ifelse(is.na(no_food), 0, no_food),

          # No non-milk liquids consumed (none of 6e-6j)
          no_liquid = ifelse(
            !!rlang::sym(iycf_6e_col) != yes_val &
              !!rlang::sym(iycf_6f_col) != yes_val &
              !!rlang::sym(iycf_6g_col) != yes_val &
              !!rlang::sym(iycf_6h_col) != yes_val &
              !!rlang::sym(iycf_6i_col) != yes_val &
              !!rlang::sym(iycf_6j_col) != yes_val,
            1,
            0
          ),
          no_liquid = ifelse(is.na(no_liquid), 0, no_liquid),

          # Currently breastfeeding (IYCF 4)
          bf = ifelse(!!rlang::sym(iycf_4_col) == yes_val, 1, 0),
          bf = ifelse(is.na(bf), 0, bf)
        )

      # Create feeding categories with explicit IYCF code references
      # For frequency columns (6b, 6c, 6d, 7a), check if > 0
      df <- df |>
        mutate(
          category = case_when(
            # Not breastfed
            bf == 0 ~ "Not Breastfed",

            # Breastfed with solid/semi-solid foods (any of 7a-7r)
            bf == 1 & any_food == 1 ~ "Breastfed & Solid or Semi-Solid Foods",

            # Breastfed with animal milk or formula (6b, 6c, 6d, or 7a > 0) but no solid foods
            bf == 1 &
              ((!!rlang::sym(iycf_6b_col) > 0 &
                !is.na(!!rlang::sym(iycf_6b_col))) |
                (!!rlang::sym(iycf_6c_col) > 0 &
                  !is.na(!!rlang::sym(iycf_6c_col))) |
                (!!rlang::sym(iycf_6d_col) > 0 &
                  !is.na(!!rlang::sym(iycf_6d_col))) |
                (!!rlang::sym(iycf_7a_col) > 0 &
                  !is.na(!!rlang::sym(iycf_7a_col)))) &
              no_food == 1 ~ "Breastfed & Animal Milk or Formula",

            # Breastfed with non-milk liquids (6e-6j) but no animal milk/formula and no solid foods
            bf == 1 &
              no_food == 1 &
              ((!!rlang::sym(iycf_6b_col) <= 0 |
                is.na(!!rlang::sym(iycf_6b_col))) &
                (!!rlang::sym(iycf_6c_col) <= 0 |
                  is.na(!!rlang::sym(iycf_6c_col))) &
                (!!rlang::sym(iycf_6d_col) <= 0 |
                  is.na(!!rlang::sym(iycf_6d_col))) &
                (!!rlang::sym(iycf_7a_col) <= 0 |
                  is.na(!!rlang::sym(iycf_7a_col)))) &
              no_liquid == 0 ~ "Breastfed & Non-Milk Liquids",

            # Breastfed with plain water only (6a) - no foods, no other liquids
            bf == 1 &
              no_food == 1 &
              no_liquid == 1 &
              !!rlang::sym(iycf_6a_col) == yes_val ~ "Breastfed & Plain Water",

            # Exclusive breastfeeding - no foods, no liquids, no water
            bf == 1 &
              no_food == 1 &
              no_liquid == 1 &
              !!rlang::sym(iycf_6a_col) != yes_val ~ "Exclusive Breastfed",

            TRUE ~ "Unknown"
          )
        )

      # Summarize by category and age group
      df <- df |>
        dplyr::group_by(.data$category, .data$age_group) |>
        dplyr::summarize(
          n = if (weighted) {
            sum(!!rlang::sym(weights_col), na.rm = TRUE)
          } else {
            sum(!is.na(!!rlang::sym(iycf_4_col)))
          },
          .groups = "drop"
        ) |>
        dplyr::group_by(.data$age_group) |>
        dplyr::mutate(
          percentage = (.data$n / sum(.data$n)) * 100,
          n = NULL
        ) |>
        dplyr::filter(!is.na(.data$category)) |>
        dplyr::arrange(.data$percentage)

      # Expand grid to include all combinations
      df <- merge(
        df,
        expand.grid(
          age_group = unique(df$age_group),
          category = unique(df$category),
          stringsAsFactors = FALSE
        ),
        all.y = TRUE
      )

      # Fill NA values with zeros
      df$percentage[is.na(df$percentage)] <- 0

      # Factor age groups and categories for proper ordering
      df <- df |>
        arrange(percentage) |>
        mutate(
          age_group = factor(
            age_group,
            levels = c(
              "0-1 months",
              "2-3 months",
              "4-5 months",
              "6-7 months",
              "8-9 months",
              "10-11 months",
              "12-13 months",
              "14-15 months",
              "16-17 months",
              "18-19 months",
              "20-21 months",
              "22-23 months"
            )
          ),
          category = factor(
            category,
            levels = c(
              "Unknown",
              "Not Breastfed",
              "Breastfed & Solid or Semi-Solid Foods",
              "Breastfed & Animal Milk or Formula",
              "Breastfed & Non-Milk Liquids",
              "Breastfed & Plain Water",
              "Exclusive Breastfed"
            )
          )
        )

      # Get color palette for IYCF categories
      # Order from lightest (Unknown) to darkest (Exclusive Breastfed)
      category_colors <- phrutils::get_color_palette(type = color_palette)

      # Create plot with fixed color scale
      g <- ggplot(data = df, aes(x = age_group, y = percentage)) +
        geom_area(
          aes(fill = category, group = category),
          alpha = 1,
          linewidth = 0.3,
          colour = "black",
          position = "stack"
        ) +
        scale_fill_manual(
          values = category_colors,
          breaks = names(category_colors), # Ensure order in legend
          drop = FALSE # Show all categories in legend even if not in data
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = legend_position
        )

      # Create subtitle with n
      if (weighted) {
        auto_subtitle <- sprintf(
          "n = %d children (weighted n = %.0f)",
          filtered_n,
          total_weighted_n
        )
      } else {
        auto_subtitle <- sprintf("n = %d children", filtered_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      g <- g + ggplot2::labs(subtitle = final_subtitle)

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        g <- g +
          ggplot2::ggtitle(paste(
            phr_txt("IYCF Area Graph"),
            "-",
            variable_label
          ))
      }

      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Date Runner - Cumulative Statistics Over Time
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param date_col Column name for the date variable. By default: "date_dc_date"
#' @param numeric_col Column name for the primary numeric variable to analyze
#' @param numeric_col2 Column name for a second numeric variable (for ratio operations). By default: NULL
#' @param operation Operation to perform: "mean", "sd", "dps" (digit preference score), "count", "ratio"
#'   By default: "mean"
#' @param grouping_col Column name for grouping/faceting. By default: NULL
#' @param reference_line Optional numeric value for a horizontal reference line. By default: NULL
#' @param color_palette Color palette to use. By default: "reach2"
#' @param show_overall Logical. If TRUE and a grouping column is provided, adds an 'Overall' line showing
#'   the cumulative statistic across all data. Default: TRUE. Only applies when grouping_col is not NULL.
#' @param overall_label Character. Label for the overall group when show_overall = TRUE. Default: "Overall".
#' @param title_name Title of the plot. By default: NULL
#' @param variable_label Character. Optional human-readable label for the numeric variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Character. Optional human-readable label for the grouping variable, appended
#'   to the auto-generated title as ", by <grouping_label>". Defaults to the column name when NULL.
#' @param subtitle Subtitle of the plot. By default: NULL
#' @param x_lab Label for x-axis. By default: "Date"
#' @param y_lab Label for y-axis. By default: NULL (auto-generated based on operation)
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#'   Note: Weighting is only supported for "mean" and "sd" operations.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#'
#' @return A ggplot object showing cumulative statistic over time
#' @export
#'
#' @examples
#' \dontrun{
#'   # Mean MUAC over time
#'   plot_date_runner(df, numeric_col = "muac", operation = "mean")
#'
#'   # SD of weight-for-height z-score by region, with auto-generated title
#'   plot_date_runner(df, numeric_col = "wfhz", operation = "sd",
#'                    grouping_col = "region", reference_line = 1.2,
#'                    variable_label = "WFH Z-Score", grouping_label = "Region")
#'
#'   # Digit preference score for weight
#'   plot_date_runner(df, numeric_col = "weight", operation = "dps")
#'
#'   # Ratio of two variables (e.g., deaths to population)
#'   plot_date_runner(df, numeric_col = "deaths", numeric_col2 = "population",
#'                    operation = "ratio")
#' }

plot_date_runner <- function(
  survey_design,
  date_col = "date_dc_date",
  numeric_col,
  numeric_col2 = NULL,
  operation = "mean",
  grouping_col = NULL,
  reference_line = NULL,
  color_palette = "reach2",
  show_overall = TRUE,
  overall_label = "Overall",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = "Date",
  y_lab = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_date_runner"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_assert(
        !missing(numeric_col) && !is.null(numeric_col),
        phr_txt("numeric_col is required"),
        origin = origin
      )

      # Validate date column exists
      phrutils::phr_validate_columns(
        df,
        date_col,
        origin = origin,
        hint = phr_txt(paste0(
          "Date column '",
          date_col,
          "' must exist in the dataframe"
        )),
        soft = FALSE
      )

      # Validate numeric column exists
      phrutils::phr_validate_columns(
        df,
        numeric_col,
        origin = origin,
        hint = phr_txt(paste0(
          "Numeric column '",
          numeric_col,
          "' must exist in the dataframe"
        )),
        soft = FALSE
      )

      # Validate operation
      valid_operations <- c("mean", "sd", "dps", "count", "ratio")
      phrutils::phr_validate_choice(
        operation,
        choices = valid_operations,
        origin = origin,
        soft = FALSE
      )

      # For ratio operation, validate second numeric column
      if (operation == "ratio") {
        phrutils::phr_validate_not_null(
          numeric_col2,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          numeric_col2,
          origin = origin,
          hint = phr_txt(paste0(
            "Second numeric column '",
            numeric_col2,
            "' must exist in the dataframe"
          )),
          soft = FALSE
        )
      }

      # Validate numeric columns are numeric or safely coercible
      phrutils::phr_validate_all_numeric(
        df[[numeric_col]],
        origin = origin,
        soft = FALSE
      )

      if (!is.null(numeric_col2)) {
        phrutils::phr_validate_all_numeric(
          df[[numeric_col2]],
          origin = origin,
          soft = FALSE
        )
      }

      # Validate grouping column if provided
      if (!is.null(grouping_col)) {
        phrutils::phr_validate_columns(
          df,
          grouping_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Grouping column '",
            grouping_col,
            "' must exist in the dataframe"
          )),
          soft = FALSE
        )
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
        if (!operation %in% c("mean", "sd")) {
          phrutils::phr_message(
            phr_txt(paste0(
              "Weighting is only supported for 'mean' and 'sd' operations. Ignoring weights for '",
              operation,
              "' operation."
            )),
            origin = origin
          )
          weighted <- FALSE
        }
      }

      # Coerce numeric columns to numeric
      df <- df |>
        dplyr::mutate(
          !!rlang::sym(numeric_col) := as.numeric(!!rlang::sym(numeric_col))
        )

      if (!is.null(numeric_col2)) {
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(numeric_col2) := as.numeric(!!rlang::sym(numeric_col2))
          )
      }

      # Handle grouping
      if (is.null(grouping_col)) {
        df <- df |> dplyr::mutate(plot_group = "All")
        grouping_col <- "plot_group"
        has_groups <- FALSE
      } else {
        df <- df |>
          dplyr::mutate(plot_group = as.character(!!rlang::sym(grouping_col)))
        has_groups <- TRUE
      }

      # If show_overall is TRUE and we have actual groups, prepend an "Overall" group
      if (show_overall && has_groups) {
        if (!is.character(overall_label)) {
          overall_label <- "Overall"
        }
        df_overall_rows <- df |> dplyr::mutate(plot_group = overall_label)
        df <- dplyr::bind_rows(df_overall_rows, df)
      }

      # Get unique groups
      groups <- unique(df$plot_group)

      # Define the function to apply based on operation
      if (operation == "mean") {
        runner_func <- mean
        result_col_name <- paste0("mean_", numeric_col)
      } else if (operation == "sd") {
        runner_func <- sd
        result_col_name <- paste0("sd_", numeric_col)
      } else if (operation == "dps") {
        runner_func <- helper_runner_dps
        result_col_name <- paste0("dps_", numeric_col)
      } else if (operation == "count") {
        runner_func <- length
        result_col_name <- paste0("count_", numeric_col)
      } else if (operation == "ratio") {
        # For ratio, we'll handle this separately
        result_col_name <- paste0("ratio_", numeric_col, "_", numeric_col2)
      }

      # Process data for each group
      df_list <- list()

      for (i in seq_along(groups)) {
        group_val <- groups[i]

        df_group <- df |>
          dplyr::filter(.data$plot_group == group_val)

        if (operation == "ratio") {
          # For ratio operation, calculate cumulative sums and then divide
          df_processed <- df_group |>
            dplyr::filter(
              !is.na(!!rlang::sym(numeric_col)) &
                !is.na(!!rlang::sym(numeric_col2))
            ) |>
            dplyr::arrange(!!rlang::sym(date_col)) |>
            dplyr::mutate(
              cumsum_num = cumsum(!!rlang::sym(numeric_col)),
              cumsum_den = cumsum(!!rlang::sym(numeric_col2)),
              !!rlang::sym(result_col_name) := cumsum_num / cumsum_den
            ) |>
            dplyr::select(
              !!rlang::sym(date_col),
              "plot_group",
              !!rlang::sym(result_col_name)
            ) |>
            dplyr::group_by(!!rlang::sym(date_col)) |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()
        } else if (operation == "count") {
          # For count, count non-NA values
          df_processed <- df_group |>
            dplyr::filter(!is.na(!!rlang::sym(numeric_col))) |>
            dplyr::arrange(!!rlang::sym(date_col)) |>
            dplyr::mutate(
              !!rlang::sym(result_col_name) := expanding_window(
                !!rlang::sym(numeric_col),
                runner_func
              )
            ) |>
            dplyr::select(
              !!rlang::sym(date_col),
              "plot_group",
              !!rlang::sym(result_col_name)
            ) |>
            dplyr::group_by(!!rlang::sym(date_col)) |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()
        } else if (weighted && operation %in% c("mean", "sd")) {
          # For weighted mean/sd: use expanding window cumulative calculation
          vals <- df_group |>
            dplyr::filter(
              !is.na(!!rlang::sym(numeric_col)) &
                !is.na(!!rlang::sym(weights_col))
            ) |>
            dplyr::arrange(!!rlang::sym(date_col))

          n_rows <- nrow(vals)
          result_vals <- numeric(n_rows)

          for (j in seq_len(n_rows)) {
            x_j <- vals[[numeric_col]][seq_len(j)]
            w_j <- vals[[weights_col]][seq_len(j)]
            if (operation == "mean") {
              result_vals[j] <- sum(x_j * w_j, na.rm = TRUE) /
                sum(w_j, na.rm = TRUE)
            } else {
              if (j < 2) {
                result_vals[j] <- NA_real_
              } else {
                wm <- sum(x_j * w_j, na.rm = TRUE) / sum(w_j, na.rm = TRUE)
                result_vals[j] <- sqrt(
                  sum(w_j * (x_j - wm)^2, na.rm = TRUE) / sum(w_j, na.rm = TRUE)
                )
              }
            }
          }

          df_processed <- vals |>
            dplyr::mutate(!!rlang::sym(result_col_name) := result_vals) |>
            dplyr::select(
              !!rlang::sym(date_col),
              "plot_group",
              !!rlang::sym(result_col_name)
            ) |>
            dplyr::group_by(!!rlang::sym(date_col)) |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()
        } else {
          # For mean, sd, dps (unweighted)
          df_processed <- df_group |>
            dplyr::filter(!is.na(!!rlang::sym(numeric_col))) |>
            dplyr::arrange(!!rlang::sym(date_col)) |>
            dplyr::mutate(
              !!rlang::sym(result_col_name) := expanding_window(
                !!rlang::sym(numeric_col),
                runner_func
              )
            ) |>
            dplyr::select(
              !!rlang::sym(date_col),
              "plot_group",
              !!rlang::sym(result_col_name)
            ) |>
            dplyr::group_by(!!rlang::sym(date_col)) |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()
        }

        df_list[[i]] <- df_processed
      }

      # Combine all groups
      df_plot <- dplyr::bind_rows(df_list) |>
        dplyr::mutate(
          !!rlang::sym(result_col_name) := as.numeric(
            !!rlang::sym(result_col_name)
          ),
          plot_group = as.factor(.data$plot_group)
        )

      # Ensure "Overall" group appears first in the legend when show_overall is active
      if (show_overall && has_groups) {
        other_groups <- setdiff(levels(df_plot$plot_group), overall_label)
        df_plot$plot_group <- factor(
          df_plot$plot_group,
          levels = c(overall_label, sort(other_groups))
        )
      }

      # Create subtitle with n (excluding the Overall group from per-group counts)
      if (!has_groups || length(groups) == 1) {
        total_n <- nrow(df_plot)
        auto_subtitle <- sprintf("n = %d observations", total_n)
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      } else {
        n_by_group <- df_plot |>
          dplyr::filter(!show_overall | .data$plot_group != overall_label) |>
          dplyr::group_by(.data$plot_group) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(
            group_label = sprintf("%s (n=%d)", .data$plot_group, .data$n)
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Set default y-axis label if not provided
      if (is.null(y_lab)) {
        if (operation == "mean") {
          y_lab <- paste(phr_txt("Cumulative Mean of"), numeric_col)
        } else if (operation == "sd") {
          y_lab <- paste(phr_txt("Cumulative SD of"), numeric_col)
        } else if (operation == "dps") {
          y_lab <- paste(phr_txt("Digit Preference Score of"), numeric_col)
        } else if (operation == "count") {
          y_lab <- paste(phr_txt("Cumulative Count of"), numeric_col)
        } else if (operation == "ratio") {
          y_lab <- paste("Cumulative Ratio:", numeric_col, "/", numeric_col2)
        }
      }

      # Create plot
      g <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = !!rlang::sym(date_col),
          y = !!rlang::sym(result_col_name),
          group = plot_group,
          color = plot_group
        )
      ) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::labs(x = x_lab, y = y_lab, subtitle = final_subtitle) +
        ggplot2::theme_minimal()

      # Add reference line if provided
      if (!is.null(reference_line)) {
        g <- g +
          ggplot2::geom_hline(
            yintercept = reference_line,
            linetype = "dashed",
            color = "black",
            linewidth = 1
          )
      }

      # Apply color palette if there are groups
      n_groups <- length(groups)
      if (n_groups > 1) {
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_groups
        )
        g <- g + ggplot2::scale_color_manual(values = colors, name = "Group")
      } else {
        # For single group, use first color from palette
        colors <- phrutils::get_color_palette(type = color_palette, n = 1)
        g <- g + ggplot2::scale_color_manual(values = colors, guide = "none")
      }

      # Add title if provided, or auto-generate from variable_label
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        operation_prefix <- switch(
          operation,
          "mean" = phr_txt("Cumulative Mean of"),
          "sd" = phr_txt("Cumulative SD of"),
          "dps" = phr_txt("Digit Preference Score of"),
          "count" = phr_txt("Cumulative Count of"),
          "ratio" = phr_txt("Cumulative Ratio of")
        )
        auto_title <- paste(operation_prefix, variable_label)
        if (has_groups) {
          group_lbl <- grouping_label %||% grouping_col
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      g <- g + ggplot2::theme(legend.position = legend_position)

      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Expanding Window Application (internal helper)
#'
#' Applies a function cumulatively over an expanding window: for position `i`,
#' calls `f(x[1:i])` and returns the scalar result. This is equivalent to the
#' expanding-window behaviour of `runner::runner(x, f)` without the `k`
#' argument.
#'
#' @param x Numeric vector (NAs should be removed beforehand).
#' @param f A function that takes a numeric vector and returns a single numeric
#'   value (e.g. `mean`, `sd`, `length`, `helper_runner_dps`).
#'
#' @return A numeric vector of length `length(x)`.
#'
#' @noRd
expanding_window <- function(x, f) {
  vapply(seq_along(x), function(i) as.numeric(f(x[seq_len(i)])), numeric(1L))
}

#' Weighted Sample Quantile (internal helper)
#'
#' Computes weighted quantiles using a weighted empirical CDF, equivalent
#' to \code{Hmisc::wtd.quantile(x, weights = w, probs = p)}.
#' The CDF is defined at midpoints of each weight interval, matching the
#' Hmisc default interpolation method.
#'
#' @param x Numeric vector of values.
#' @param weights Numeric vector of non-negative weights (same length as \code{x}).
#' @param probs Single probability in [0, 1].
#'
#' @return A single numeric quantile value.
#' @noRd
wtd_quantile <- function(x, weights, probs) {
  ok <- !is.na(x) & !is.na(weights)
  x <- x[ok]
  w <- weights[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cdf <- (cumsum(w) - w / 2) / sum(w)
  stats::approx(cdf, x, xout = probs, method = "linear", rule = 2)$y
}

#' Helper Runner DPS
#'
#' Helper function for plot_date_runner function. Calculates digit preference
#' scores over an expanding window.
#'
#' @param x A numeric vector
#'
#' @return A numeric digit preference score
#' @export
#'
#' @examples
#' \dontrun{helper_runner_dps(muac)}
helper_runner_dps <- function(x) {
  x <- as.numeric(format(round(x, 1), nsmall = 1))

  digit_preference_score(x / 10)
}


#' Plot Domain Radar Chart
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param domain_cols Vector of column names representing domains (binary 0/1 values)
#' @param domain_labels Vector of labels for domains. By default: NULL (uses domain_cols)
#' @param grouping Column name for grouping. By default: NULL (single group)
#' @param max_value Maximum value for radar scale. By default: NULL (auto-calculated)
#' @param color_palette Color palette to use. By default: "reach2"
#' @param title_name Title of the plot. By default: NULL
#' @param variable_label Optional character string. Human-readable label for the domain variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Subtitle of the plot. By default: NULL
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#'
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param flip_coordinates Logical. If TRUE, flips the coordinate axes. By default: FALSE.
#' @return A radar chart showing domain coverage
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_domain_radar(survey_design = data,
#'                     domain_cols = c("food", "water", "health"),
#'                     domain_labels = c("Food Security", "WASH", "Health"))
#' }

plot_domain_radar <- function(
  survey_design,
  domain_cols,
  domain_labels = NULL,
  grouping = NULL,
  max_value = NULL,
  color_palette = "reach2",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_domain_radar"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(
        domain_cols,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        domain_cols,
        origin = origin,
        hint = phr_txt("Ensure all domain columns exist in the dataset"),
        soft = FALSE
      )

      if (!requireNamespace("ggradar", quietly = TRUE)) {
        stop(
          "Package 'ggradar' is required for this plot. Install it with: install.packages('ggradar')"
        )
      }

      # Handle grouping
      has_grouping <- !is.null(grouping) && !missing(grouping)

      if (!has_grouping) {
        df <- df |> dplyr::mutate(group = "All")
        grouping_var <- "group"
      } else {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt("Ensure grouping column exists in the dataset"),
          soft = FALSE
        )
        # Don't rename - keep original column name for grouping
        grouping_var <- grouping
      }

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # If domain labels is NULL, set default to domain_cols
      if (is.null(domain_labels)) {
        domain_labels <- domain_cols
      }

      # Check if length domain_cols == domain_labels
      if (!(length(domain_cols) == length(domain_labels))) {
        phr_error(
          phr_txt(
            "The number of entries for domain_cols must equal the number of entries for domain_labels."
          ),
          origin = origin
        )
      }

      # Convert to numeric and handle NAs (ggradar won't work with NAs)
      df[domain_cols] <- lapply(df[domain_cols], as.numeric)
      df[c(domain_cols)][is.na(df[c(domain_cols)])] <- 0

      # Create a summary table of the domain results, % reported for each domain
      if (weighted) {
        summary <- df |>
          dplyr::select(
            !!rlang::sym(grouping_var),
            dplyr::all_of(domain_cols),
            !!rlang::sym(weights_col)
          ) |>
          dplyr::group_by(!!rlang::sym(grouping_var)) |>
          dplyr::summarise(
            dplyr::across(
              dplyr::all_of(domain_cols),
              ~ sum(. * !!rlang::sym(weights_col), na.rm = TRUE) /
                sum(!!rlang::sym(weights_col), na.rm = TRUE),
              .names = "{.col}"
            ),
            .groups = "drop"
          )
      } else {
        summary <- df |>
          dplyr::select(
            !!rlang::sym(grouping_var),
            dplyr::all_of(domain_cols)
          ) |>
          dplyr::group_by(!!rlang::sym(grouping_var)) |>
          dplyr::summarise(
            dplyr::across(dplyr::all_of(domain_cols), mean, .names = "{.col}"),
            .groups = "drop"
          )
      }

      # Rename grouping column to 'group' for ggradar (it expects this name)
      summary <- summary |>
        dplyr::rename(group = !!rlang::sym(grouping_var))

      obs_max <- max(summary[, -1], na.rm = TRUE)

      if (is.null(max_value)) {
        max_value <- obs_max * 1.1
      }

      # Get colors based on number of groups
      n_groups <- nrow(summary)
      colors <- phrutils::get_color_palette(type = color_palette, n = n_groups)

      # Determine if we should show percentage labels (only for single group)
      show_labels <- !has_grouping || n_groups == 1

      # ggradar handles multiple groups internally - only pass valid parameters
      g <- ggradar::ggradar(
        summary,
        grid.max = max_value,
        base.size = 1,
        group.point.size = 3,
        group.line.width = 1,
        grid.line.width = 0.5,
        values.radar = c(
          paste0("0%"),
          paste0(round((max_value / 2) * 100, 1), "%"),
          paste0(round(max_value * 100, 1), "%")
        ),
        grid.label.size = 3,
        axis.label.size = 4, # Domain labels slightly bigger
        legend.position = if (n_groups > 1) legend_position else "none",
        legend.text.size = 10,
        axis.labels = domain_labels,
        grid.mid = max_value / 2,
        grid.min = 0,
        group.colours = colors, # Apply custom color palette
        background.circle.colour = "white"
      )

      # Add percentage labels for single group only
      if (show_labels) {
        # Extract values for labeling
        label_data <- summary |>
          tidyr::pivot_longer(
            cols = -"group",
            names_to = "domain",
            values_to = "value"
          ) |>
          dplyr::mutate(
            percentage_label = paste0(round(.data$value * 100, 1), "%"),
            domain_label = factor(
              .data$domain,
              levels = domain_cols,
              labels = domain_labels
            )
          )

        # Calculate positions for labels (slightly outside the points)
        # ggradar uses polar coordinates, so we need to work with angles
        n_domains <- length(domain_cols)
        angles <- seq(0, 2 * pi, length.out = n_domains + 1)[1:n_domains]

        # Adjust angle to match ggradar's starting position (starts at top, goes clockwise)
        # ggradar starts at 90 degrees (top) and goes clockwise
        angles_adjusted <- (pi / 2) - angles

        # Create label positions (extend slightly beyond the data points)
        label_data <- label_data |>
          dplyr::mutate(
            angle = angles_adjusted[match(.data$domain, domain_cols)],
            # Position labels slightly beyond the actual values
            # Scale to ggradar's coordinate system (0 to grid.max)
            label_radius = .data$value * 1.15
          ) |>
          dplyr::mutate(
            x = .data$label_radius * cos(.data$angle),
            y = .data$label_radius * sin(.data$angle)
          )

        # Add text labels
        g <- g +
          ggplot2::geom_text(
            data = label_data,
            ggplot2::aes(
              x = .data$x,
              y = .data$y,
              label = .data$percentage_label
            ),
            size = 3.5,
            fontface = "bold",
            color = colors[1],
            inherit.aes = FALSE
          )
      }

      # Create subtitle with n
      if (!has_grouping || n_groups == 1) {
        total_n <- nrow(df)
        auto_subtitle <- sprintf("n = %d", total_n)
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      } else {
        # Calculate n per group from original data (before summarizing)
        n_by_group <- df |>
          dplyr::group_by(!!rlang::sym(grouping_var)) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(
            group_label = sprintf(
              "%s (n=%d)",
              !!rlang::sym(grouping_var),
              .data$n
            )
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }

      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Add subtitle and title with adjusted sizes AFTER ggradar call
      g <- g +
        ggplot2::labs(subtitle = final_subtitle) +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(size = 10), # Smaller subtitle
          plot.title = ggplot2::element_text(size = 12) # Smaller title
        )

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Domain Radar"), "-", variable_label)
        if (has_grouping) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Select Multiple Across Domains
#'
#' This functions plots a count distribution of responses for a select multiple question
#' and shows their responses distributed against some domain categorization of those
#' response. E.g A health barriers question with several response options, however those
#' response options can be grouped by barriers related to availability, accessibility, or
#' quality of services. Each response option should only belong to one domain category.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param domain_list A named list where each element represents a domain. Each domain element
#' should be a list with:
#'   - 'responses': character vector of column names for that domain
#'   - 'label': (optional) display label for the domain
#'   - 'response_labels': (optional) named vector mapping column names to display labels
#' Example: list(
#'   availability = list(
#'     responses = c("barrier_no_staff", "barrier_no_medicine"),
#'     label = "Availability Issues",
#'     response_labels = c("barrier_no_staff" = "No Staff", "barrier_no_medicine" = "No Medicine")
#'   )
#' )
#' @param show_percentage Logical. If TRUE, shows percentages instead of counts.
#' By default: FALSE. Note: For select multiple questions, percentages may sum to >100\%.
#' @param flip_coordinates Logical. If TRUE, creates horizontal bars (flipped coordinates).
#' By default: TRUE.
#' @param color_palette Color palette to use. By default: "reach1"
#' @param title_name Title of the plot. By default: NULL
#' @param variable_label Optional character string. Human-readable label for the domain variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Subtitle of the plot. By default: NULL
#' @param x_lab Label for x-axis. By default: NULL (auto-generated based on show_percentage and flip_coordinates)
#' @param y_lab Label for y-axis. By default: NULL (auto-generated based on flip_coordinates)
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param weighted Logical. If TRUE, applies survey weights to produce weighted results. By default: FALSE.
#' @param weights_col Character string. Name of the column containing survey weights. Required when weighted = TRUE.
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#'   domain_list <- list(
#'     accessibility = list(
#'       responses = c("barrier.distance", "barrier.costtransport"),
#'       label = "Accessibility Barriers",
#'       response_labels = c("barrier.distance" = "Too Far",
#'                          "barrier.costtransport" = "Transport Cost")
#'     )
#'   )
#'   # Horizontal bars (default)
#'   plot_domain_distribution(survey_design = data, domain_list = domain_list)
#'
#'   # Vertical bars
#'   plot_domain_distribution(survey_design = data, domain_list = domain_list,
#'                               flip_coordinates = FALSE,
#'                               legend_position = "bottom")
#'
#'   # Show percentages
#'   plot_domain_distribution(survey_design = data, domain_list = domain_list,
#'                               show_percentage = TRUE)
#' }
#' @importFrom rlang .data

plot_domain_distribution <- function(
  survey_design,
  domain_list,
  show_percentage = FALSE,
  flip_coordinates = TRUE,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "bottom",
  weighted = FALSE,
  weights_col = NULL
) {
  origin <- "plot_domain_distribution"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(
        domain_list,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_list(domain_list, origin = origin, soft = FALSE)
      phrutils::phr_assert(
        length(domain_list) > 0,
        phr_txt("domain_list must contain at least one domain"),
        origin = origin
      )
      phrutils::phr_validate_logical(
        show_percentage,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )

      # Extract domain names
      domain_names <- names(domain_list)
      if (is.null(domain_names) || any(domain_names == "")) {
        phr_error(
          phr_txt("domain_list must be a named list with all elements named"),
          origin = origin
        )
      }

      # Build response-to-domain mapping
      all_responses <- c()
      response_to_domain <- c()
      domain_labels_map <- setNames(domain_names, domain_names) # Default: use domain name as label
      all_response_labels <- list()

      for (domain_name in domain_names) {
        domain_info <- domain_list[[domain_name]]

        # Validate domain structure
        if (!is.list(domain_info)) {
          phr_error(
            phr_txt(paste0("Domain '", domain_name, "' must be a list")),
            origin = origin
          )
        }
        if (is.null(domain_info$responses)) {
          phr_error(
            phr_txt(paste0(
              "Domain '",
              domain_name,
              "' must have a 'responses' element"
            )),
            origin = origin
          )
        }

        # Get responses for this domain
        domain_responses <- domain_info$responses
        all_responses <- c(all_responses, domain_responses)
        response_to_domain <- c(
          response_to_domain,
          rep(domain_name, length(domain_responses))
        )

        # Get domain label if provided
        if (!is.null(domain_info$label)) {
          domain_labels_map[domain_name] <- domain_info$label
        }

        # Get response labels if provided
        if (!is.null(domain_info$response_labels)) {
          all_response_labels <- c(
            all_response_labels,
            as.list(domain_info$response_labels)
          )
        }
      }

      # Validate all response columns exist
      phrutils::phr_validate_columns(
        df,
        all_responses,
        origin = origin,
        hint = phr_txt("Ensure all response columns exist in the dataset"),
        soft = FALSE
      )

      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Convert response columns to numeric
      df[all_responses] <- lapply(df[all_responses], as.numeric)

      # Store total number of respondents for percentage calculation
      if (weighted) {
        total_respondents <- sum(df[[weights_col]], na.rm = TRUE)
      } else {
        total_respondents <- nrow(df)
      }

      # Create a mapping dataframe that links each response to its domain
      response_domain_map <- data.frame(
        response = all_responses,
        domain = response_to_domain,
        stringsAsFactors = FALSE
      )

      # Calculate counts for each response variable
      if (weighted) {
        response_counts <- df |>
          dplyr::select(dplyr::all_of(c(all_responses, weights_col))) |>
          tidyr::pivot_longer(
            cols = dplyr::all_of(all_responses),
            names_to = "response",
            values_to = "value"
          ) |>
          dplyr::filter(!is.na(.data$value) & .data$value != 0) |>
          dplyr::group_by(.data$response) |>
          dplyr::summarise(
            count = sum(!!rlang::sym(weights_col), na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        response_counts <- df |>
          dplyr::select(dplyr::all_of(all_responses)) |>
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "response",
            values_to = "value"
          ) |>
          dplyr::filter(!is.na(.data$value) & .data$value != 0) |>
          dplyr::group_by(.data$response) |>
          dplyr::summarise(count = dplyr::n(), .groups = "drop")
      }

      # Calculate percentage if requested
      if (show_percentage) {
        response_counts <- response_counts |>
          dplyr::mutate(percentage = (.data$count / total_respondents) * 100)
      }

      # Join with domain mapping
      plot_data <- response_counts |>
        dplyr::left_join(response_domain_map, by = "response")

      # Apply domain labels
      plot_data <- plot_data |>
        dplyr::mutate(
          domain_label = dplyr::recode(domain, !!!domain_labels_map)
        )

      # Apply response labels if provided
      if (length(all_response_labels) > 0) {
        plot_data <- plot_data |>
          dplyr::mutate(
            response_label = dplyr::recode(response, !!!all_response_labels)
          )
      } else {
        plot_data <- plot_data |>
          dplyr::mutate(response_label = response)
      }

      # Convert to factors for plotting
      plot_data <- plot_data |>
        dplyr::mutate(
          domain_label = as.factor(domain_label),
          response_label = as.factor(response_label)
        )

      # Create subtitle with n and note about percentages
      total_count <- sum(plot_data$count)
      if (show_percentage) {
        total_pct <- sum(plot_data$percentage)
        auto_subtitle <- sprintf(
          "n = %d respondents (total %% = %.1f%%)",
          total_respondents,
          total_pct
        )
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      } else {
        auto_subtitle <- sprintf("n = %d responses", total_count)
        if (weighted) auto_subtitle <- paste0(auto_subtitle, " (weighted)")
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Get colors based on number of unique domains
      unique_domains <- unique(plot_data$domain)
      n_domains <- length(unique_domains)
      colors <- phrutils::get_color_palette(type = color_palette, n = n_domains)

      # Create named color vector for consistent mapping (use original domain names)
      domain_colors <- setNames(colors, unique_domains)

      # Map colors to domain labels for the plot
      plot_colors <- setNames(
        colors[match(unique(plot_data$domain), unique_domains)],
        unique(plot_data$domain_label)
      )

      # Set default axis labels if not provided
      # Logic depends on flip_coordinates
      if (flip_coordinates) {
        # Horizontal bars: responses on y-axis, values on x-axis
        if (is.null(x_lab)) {
          x_lab <- if (show_percentage) "Percentage (%)" else "Count"
        }
        if (is.null(y_lab)) {
          y_lab <- "Response"
        }
      } else {
        # Vertical bars: responses on x-axis, values on y-axis
        if (is.null(x_lab)) {
          x_lab <- "Response"
        }
        if (is.null(y_lab)) {
          y_lab <- if (show_percentage) "Percentage (%)" else "Count"
        }
      }

      # Determine which value to plot
      plot_value <- if (show_percentage) {
        plot_data$percentage
      } else {
        plot_data$count
      }

      # Create base plot
      if (flip_coordinates) {
        # Horizontal bars (flipped)
        g <- ggplot2::ggplot(
          data = plot_data,
          mapping = ggplot2::aes(
            x = stats::reorder(
              .data$response_label,
              if (show_percentage) .data$percentage else .data$count
            ),
            y = plot_value,
            fill = .data$domain_label
          )
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::coord_flip()
      } else {
        # Vertical bars (not flipped)
        g <- ggplot2::ggplot(
          data = plot_data,
          mapping = ggplot2::aes(
            x = stats::reorder(
              .data$response_label,
              if (show_percentage) .data$percentage else .data$count
            ),
            y = plot_value,
            fill = .data$domain_label
          )
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
      }

      # Add common elements
      g <- g +
        ggplot2::scale_fill_manual(
          values = plot_colors,
          name = "Domain"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = legend_position) +
        ggplot2::labs(
          x = x_lab,
          y = y_lab,
          subtitle = final_subtitle
        )

      # Apply angle to x-axis text if not flipped
      if (!flip_coordinates) {
        g <- g +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
      }

      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        g <- g +
          ggplot2::ggtitle(paste(phr_txt("Distribution of"), variable_label))
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot 100\% Stacked Bar Chart
#'
#' Creates a 100\% stacked bar chart for categorical or factor variables, either overall or grouped.
#' The plot shows the relative proportions of each category as a percentage of the total.
#' Supports survey weights for proper representation of weighted surveys.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param category_var Inputs a character value specifying the column name for the categorical variable to plot.
#' @param grouping Inputs an optional character value specifying the column name for grouping the data.
#'   If NULL, creates an overall plot. If provided, creates separate stacked bars for each group.
#' @param fill_var Inputs an optional character value specifying the column name for the fill/stacking variable.
#'   If NULL, uses category_var as the fill variable.
#' @param weighted Logical indicating whether to apply survey weights. Default: FALSE.
#' @param weights_col Character value specifying the column name containing survey weights.
#'   Required if weighted = TRUE. Default: NULL.
#' @param show_overall Logical. When grouping is provided, if TRUE, adds an "Overall" bar (leftmost)
#'   showing the overall distribution across all groups. Default: TRUE. Only applies when grouping is not NULL.
#' @param overall_label Character. Label for the overall bar when show_overall = TRUE. Default: "Overall".
#' @param flip_coordinates Logical. If TRUE, creates horizontal bars (flipped coordinates).
#'   By default: FALSE (vertical bars).
#' @param color_palette Inputs an optional character value specifying the color palette to use.
#'   Options: "reach1", "reach2", "reach3", "reach4", "traffic_light", "default". Default is "reach1".
#' @param title_name Inputs an optional character value for the title of the plot.
#' @param variable_label Character. Optional human-readable label for the category variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Character. Optional human-readable label for the grouping variable, appended
#'   to the auto-generated title as ", by <grouping_label>". Defaults to the column name when NULL.
#' @param subtitle Inputs an optional character value for the subtitle of the plot.
#'   If NULL, automatically displays n (number of records). Custom subtitle will be appended to n display.
#' @param x_label Inputs an optional character value for the x-axis label.
#' @param y_label Inputs an optional character value for the y-axis label (default: "Percentage").
#' @param legend_label Inputs an optional character value for the legend title.
#'   If NULL, uses fill_var as the legend title.
#' @param show_labels Inputs a logical value indicating whether to show percentage labels on bars (default: FALSE).
#' @param show_NA Logical. If TRUE, missing values in the fill variable are shown as an explicit `"NA"` category.
#'   If FALSE (default), rows with missing fill values are excluded from the plotted distribution.
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#'
#' @return Returns a ggplot2 object showing the 100\% stacked bar chart.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Unweighted, vertical bars
#'   plot_stacked_bar(df, category_var = "response_type", grouping = "district",
#'                    title_name = "Response Distribution by District")
#'
#'   # With overall bar for comparison
#'   plot_stacked_bar(df, category_var = "response_type", grouping = "district",
#'                    show_overall = TRUE, title_name = "Response Distribution by District")
#'
#'   # Weighted, horizontal bars
#'   plot_stacked_bar(df, category_var = "response_type", grouping = "district",
#'                    weighted = TRUE, weights_col = "survey_weight",
#'                    flip_coordinates = TRUE, legend_label = "Response Type")
#'
#'   # Show NA as an explicit category
#'   plot_stacked_bar(df, category_var = "response_type",
#'                    show_NA = TRUE, title_name = "Response Distribution (including NA)")
#' }
plot_stacked_bar <- function(
  survey_design,
  category_var,
  grouping = NULL,
  fill_var = NULL,
  weighted = FALSE,
  weights_col = NULL,
  show_overall = TRUE,
  overall_label = "Overall",
  flip_coordinates = FALSE,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = "Percentage",
  legend_label = NULL,
  show_labels = FALSE,
  show_NA = FALSE,
  legend_position = "bottom"
) {
  origin <- "plot_stacked_bar"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_not_null(
        category_var,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(show_NA, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )

      phrutils::phr_validate_columns(
        df,
        category_var,
        origin = origin,
        hint = phr_txt(
          "Ensure the category column '{category_var}' exists in the dataset"
        ),
        soft = FALSE
      )

      # Validate weights if weighted = TRUE
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(
            "Ensure the weights column '{weights_col}' exists in the dataset"
          ),
          soft = FALSE
        )

        # Validate weights column is numeric
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )

        # Convert weights to numeric
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Set fill variable
      if (is.null(fill_var)) {
        fill_var <- category_var
      } else {
        phrutils::phr_validate_columns(
          df,
          fill_var,
          origin = origin,
          hint = phr_txt(
            "Ensure the fill column '{fill_var}' exists in the dataset"
          ),
          soft = FALSE
        )
      }

      # If requested, show NA as an explicit category in the fill var
      if (show_NA) {
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(fill_var) := dplyr::if_else(
              is.na(!!rlang::sym(fill_var)),
              "NA",
              as.character(!!rlang::sym(fill_var))
            )
          )
      }

      # Set legend label
      if (is.null(legend_label)) {
        legend_label <- fill_var
      }

      # Prepare data using phr_calc_survey_categorical_single for consistent
      # survey-weighted proportions that match run_analysis outputs.
      ind_label <- variable_label %||% fill_var
      cat_prefix <- paste0(ind_label, " - ")

      if (is.null(grouping)) {
        # Overall plot — use phr_calc_survey_categorical_single
        calc_df <- phr_calc_survey_categorical_single(
          design = survey_design,
          var_name = fill_var,
          indicator_name = ind_label,
          multiplier = 100
        )

        df_plot <- calc_df |>
          dplyr::mutate(
            !!rlang::sym(fill_var) := gsub(
              cat_prefix,
              "",
              .data$indicator_name,
              fixed = TRUE
            ),
            percentage = .data$point.estimate,
            group = "Overall",
            n = as.integer(.data$n_unweighted),
            label = sprintf("%.1f%%", .data$point.estimate)
          ) |>
          dplyr::filter(show_NA | !is.na(!!rlang::sym(fill_var)))

        total_n <- sum(df_plot$n, na.rm = TRUE)
        total_weighted_n <- sum(calc_df$n_weighted, na.rm = TRUE)
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          total_weighted_n
        )

        # Determine number of colors needed
        if (is.factor(df[[fill_var]])) {
          all_levels <- levels(df[[fill_var]])
          all_colors <- phrutils::get_color_palette(
            type = color_palette,
            n = length(all_levels)
          )
          names(all_colors) <- all_levels
          colors <- all_colors
        } else {
          n_colors <- length(unique(df_plot[[fill_var]]))
          colors <- phrutils::get_color_palette(
            type = color_palette,
            n = n_colors
          )
        }

        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        # Create base plot
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(
            x = .data$group,
            y = percentage,
            fill = as.factor(!!rlang::sym(fill_var))
          )
        ) +
          ggplot2::geom_bar(stat = "identity", position = "fill") +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::scale_fill_manual(values = colors) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(fill = legend_label, subtitle = final_subtitle)

        # Apply coordinate flip if requested
        if (flip_coordinates) {
          g <- g +
            ggplot2::coord_flip() +
            ggplot2::labs(x = x_label %||% "", y = y_label)
        } else {
          g <- g +
            ggplot2::labs(x = x_label %||% "", y = y_label)
        }

        # Add labels if requested
        if (show_labels) {
          g <- g +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              position = ggplot2::position_fill(vjust = 0.5),
              size = 3
            )
        }
      } else {
        # Grouped plot
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(
            "Ensure the grouping column '{grouping}' exists in the dataset"
          ),
          soft = FALSE
        )

        # Ensure overall_label is a non-empty string when show_overall is TRUE
        if (show_overall && !is.character(overall_label)) {
          overall_label <- "Overall"
        }

        group_vals <- unique(df[[grouping]])
        group_vals <- group_vals[!is.na(group_vals)]

        calc_list <- lapply(group_vals, function(gv) {
          dsn_g <- dplyr::filter(survey_design, !!rlang::sym(grouping) == gv)
          r <- phr_calc_survey_categorical_single(
            design = dsn_g,
            var_name = fill_var,
            indicator_name = ind_label,
            multiplier = 100
          )
          r[[grouping]] <- as.character(gv)
          r
        })

        if (show_overall) {
          r_overall <- phr_calc_survey_categorical_single(
            design = survey_design,
            var_name = fill_var,
            indicator_name = ind_label,
            multiplier = 100
          )
          r_overall[[grouping]] <- overall_label
          calc_list <- c(list(r_overall), calc_list)
        }

        calc_df_all <- dplyr::bind_rows(calc_list)

        df_plot <- calc_df_all |>
          dplyr::mutate(
            !!rlang::sym(fill_var) := gsub(
              cat_prefix,
              "",
              .data$indicator_name,
              fixed = TRUE
            ),
            percentage = .data$point.estimate,
            n = as.integer(.data$n_unweighted),
            label = sprintf("%.1f%%", .data$point.estimate),
            !!rlang::sym(grouping) := as.character(.data[[grouping]])
          ) |>
          dplyr::filter(show_NA | !is.na(!!rlang::sym(fill_var)))

        # Build subtitle from group-level sample sizes
        n_by_group <- df_plot |>
          dplyr::filter(!show_overall | .data[[grouping]] != overall_label) |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(
            total_n = sum(.data$n, na.rm = TRUE),
            total_weighted_n = sum(
              calc_df_all$n_weighted[
                calc_df_all[[grouping]] == dplyr::cur_group()[[grouping]]
              ],
              na.rm = TRUE
            ),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            group_label = sprintf(
              "%s (n=%d, wn=%.0f)",
              .data[[grouping]],
              total_n,
              total_weighted_n
            )
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")

        # Determine number of colors needed
        if (is.factor(df[[fill_var]])) {
          all_levels <- levels(df[[fill_var]])
          all_colors <- phrutils::get_color_palette(
            type = color_palette,
            n = length(all_levels)
          )
          names(all_colors) <- all_levels
          colors <- all_colors
        } else {
          n_colors <- length(unique(df_plot[[fill_var]]))
          colors <- phrutils::get_color_palette(
            type = color_palette,
            n = n_colors
          )
        }

        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        # Order x-axis so overall bar appears leftmost
        if (show_overall) {
          other_groups <- setdiff(
            unique(as.character(df_plot[[grouping]])),
            overall_label
          )
          x_levels <- c(overall_label, sort(other_groups))
          df_plot[[grouping]] <- factor(df_plot[[grouping]], levels = x_levels)
        }

        # Create base plot
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(
            x = as.factor(!!rlang::sym(grouping)),
            y = percentage,
            fill = as.factor(!!rlang::sym(fill_var))
          )
        ) +
          ggplot2::geom_bar(stat = "identity", position = "fill") +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::scale_fill_manual(values = colors) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(fill = legend_label, subtitle = final_subtitle)

        # Apply coordinate flip and axis styling
        if (flip_coordinates) {
          g <- g +
            ggplot2::coord_flip() +
            ggplot2::labs(x = x_label %||% grouping, y = y_label)
        } else {
          g <- g +
            ggplot2::labs(x = x_label %||% grouping, y = y_label) +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        }

        # Add labels if requested
        if (show_labels) {
          g <- g +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              position = ggplot2::position_fill(vjust = 0.5),
              size = 3
            )
        }
      }

      # Add title if provided, or auto-generate from variable_label
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Distribution of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot 100\% Stacked Bar Chart for Multiple Variables
#'
#' Creates a 100\% stacked bar chart showing multiple categorical variables in the same plot.
#' Each variable is represented as a separate bar, optionally grouped together.
#' The plot shows the relative proportions of each category as a percentage of the total.
#' Supports survey weights for proper representation of weighted surveys.
#' Allows different color palettes and separate legends for each variable.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param category_vars Inputs a character vector specifying the column names for the categorical variables to plot (required, minimum 2).
#' @param category_labels Inputs an optional character vector of custom labels for each category variable.
#'   If NULL, uses category_vars as labels. Must be same length as category_vars if provided.
#' @param grouping Inputs an optional character value specifying the column name for grouping variables within each bar.
#'   If NULL, each variable shows overall distribution. If provided, shows grouped bars within each variable.
#' @param show_overall Logical. When grouping is provided, if TRUE, adds an "Overall" bar (leftmost) for each variable
#'   showing the overall distribution across all groups. Default: FALSE. Only applies when grouping is not NULL.
#' @param overall_label Character. Label for the overall bar when show_overall = TRUE. Default: "Overall".
#' @param weighted Logical indicating whether to apply survey weights. Default: FALSE.
#' @param weights_col Character value specifying the column name containing survey weights.
#'   Required if weighted = TRUE. Default: NULL.
#' @param flip_coordinates Logical. If TRUE, creates horizontal bars (flipped coordinates).
#'   By default: FALSE (vertical bars).
#' @param color_palette Inputs a character value or vector specifying the color palette(s) to use.
#'   Can be a single palette applied to all variables, or a vector of palettes (one per variable).
#'   Options: "reach1", "reach2", "reach3", "reach4", "traffic_light", "default". Default is "reach1".
#' @param separate_legends Logical. If TRUE, creates truly separate legend for each variable using ggnewscale.
#'   If FALSE, uses single combined legend. Default: FALSE. Requires ggnewscale package when TRUE.
#' @param title_name Inputs an optional character value for the title of the plot.
#' @param variable_label Optional character string. Human-readable label for the variables, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Inputs an optional character value for the subtitle of the plot.
#'   If NULL, automatically displays n (number of records). Custom subtitle will be appended to n display.
#' @param x_label Inputs an optional character value for the x-axis label.
#' @param y_label Inputs an optional character value for the y-axis label (default: "Percentage").
#' @param legend_label Inputs an optional character value or vector for the legend title(s).
#'   If NULL, uses variable names. Can be a vector (one per variable) if separate_legends = TRUE.
#' @param show_labels Inputs a logical value indicating whether to show percentage labels on bars (default: FALSE).
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @param bar_spacing Numeric value for spacing between variable groups (default: 0.5).
#'   Higher values create more space between different variables.
#' @param group_spacing Numeric value for spacing between bars within a variable when using grouping (default: 0.1).
#'   Creates visual separation between different variables' grouped bars.
#'
#' @return Returns a ggplot2 object showing the 100\% stacked bar chart with multiple variables.
#' @export

plot_stacked_bar_multiple_vars <- function(
  survey_design,
  category_vars,
  category_labels = NULL,
  grouping = NULL,
  show_overall = FALSE,
  overall_label = "Overall",
  weighted = FALSE,
  weights_col = NULL,
  flip_coordinates = FALSE,
  color_palette = "reach1",
  separate_legends = FALSE,
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = "Percentage",
  legend_label = NULL,
  show_labels = FALSE,
  legend_position = "bottom",
  bar_spacing = 0.5,
  group_spacing = 0.1
) {
  origin <- "plot_stacked_bar_multiple_vars"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Apply ensure_value with non-NULL defaults
      legend_position <- ensure_value(legend_position, "bottom")
      weighted <- ensure_value(weighted, FALSE)
      show_labels <- ensure_value(show_labels, FALSE)
      flip_coordinates <- ensure_value(flip_coordinates, FALSE)
      separate_legends <- ensure_value(separate_legends, FALSE)
      show_overall <- ensure_value(show_overall, FALSE)
      overall_label <- ensure_value(overall_label, "Overall")
      y_label <- ensure_value(y_label, "Percentage")
      bar_spacing <- ensure_value(bar_spacing, 0.5)
      group_spacing <- ensure_value(group_spacing, 0.1)

      # For optional text fields
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      x_label <- ensure_value(x_label, "")

      # For optional column names
      weights_col <- ensure_value(weights_col, NA_character_)
      grouping <- ensure_value(grouping, NA_character_)
      category_labels <- ensure_value(category_labels, NULL)
      legend_label <- ensure_value(legend_label, NULL)

      # Validate inputs
      phrutils::phr_validate_not_null(
        category_vars,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(show_labels, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        separate_legends,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        overall_label,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        bar_spacing,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        group_spacing,
        origin = origin,
        soft = FALSE
      )

      # Check for ggnewscale if separate_legends = TRUE
      if (separate_legends && !requireNamespace("ggnewscale", quietly = TRUE)) {
        phrutils::phr_warning(
          phr_txt(
            "separate_legends=TRUE requires ggnewscale package. Install with install.packages('ggnewscale'). Falling back to combined legend."
          ),
          origin = origin
        )
        separate_legends <- FALSE
      }

      # Convert back to NULL
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (is.na(grouping)) {
        grouping <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (x_label == "") {
        x_label <- NULL
      }

      # Validate category_vars
      phrutils::phr_validate_vector_length(
        category_vars,
        min_length = 2,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        category_vars,
        origin = origin,
        hint = phr_txt(
          "Ensure all category variable columns exist in the dataset"
        ),
        soft = FALSE
      )

      # Validate and expand color_palette
      if (length(color_palette) == 1) {
        color_palettes <- rep(color_palette, length(category_vars))
      } else if (length(color_palette) == length(category_vars)) {
        color_palettes <- color_palette
      } else {
        phr_error(
          phr_txt(paste0(
            "color_palette must be length 1 or same length as category_vars (",
            length(category_vars),
            "). Got ",
            length(color_palette)
          )),
          origin = origin
        )
      }

      # Validate category_labels if provided
      if (!is.null(category_labels)) {
        phrutils::phr_validate_character(
          category_labels,
          origin = origin,
          soft = FALSE
        )
        if (length(category_labels) != length(category_vars)) {
          phr_error(
            phr_txt(paste0(
              "category_labels must be same length as category_vars. Expected ",
              length(category_vars),
              " labels but got ",
              length(category_labels)
            )),
            origin = origin
          )
        }
        final_category_labels <- category_labels
      } else {
        final_category_labels <- category_vars
      }

      # Validate and expand legend_label
      if (!is.null(legend_label)) {
        if (separate_legends) {
          if (length(legend_label) == 1) {
            legend_labels <- rep(legend_label, length(category_vars))
          } else if (length(legend_label) == length(category_vars)) {
            legend_labels <- legend_label
          } else {
            phr_error(
              phr_txt(paste0(
                "When separate_legends=TRUE, legend_label must be length 1 or same length as category_vars (",
                length(category_vars),
                "). Got ",
                length(legend_label)
              )),
              origin = origin
            )
          }
        } else {
          legend_labels <- rep(legend_label[1], length(category_vars))
        }
      } else {
        if (separate_legends) {
          legend_labels <- final_category_labels
        } else {
          legend_labels <- rep("Category", length(category_vars))
        }
      }

      # Validate weights if weighted = TRUE
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Validate grouping if provided
      has_grouping <- !is.null(grouping)
      if (has_grouping) {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0("Grouping column '", grouping, "' must exist")),
          soft = FALSE
        )
      }

      # If show_overall is TRUE but no grouping, warn and set to FALSE
      if (show_overall && !has_grouping) {
        phrutils::phr_warning(
          phr_txt(
            "show_overall=TRUE only applies when grouping is provided. Ignoring show_overall."
          ),
          origin = origin
        )
        show_overall <- FALSE
      }

      # Reshape data: convert multiple variables into long format
      df_long_list <- list()

      for (i in seq_along(category_vars)) {
        var_name <- category_vars[i]
        var_label <- final_category_labels[i]
        var_palette <- color_palettes[i]

        if (has_grouping) {
          grouping_sym <- rlang::sym(grouping)

          # Calculate grouped data
          if (weighted) {
            weights_sym <- rlang::sym(weights_col)

            df_var_grouped <- df |>
              dplyr::filter(
                !is.na(!!rlang::sym(var_name)) &
                  !is.na(!!grouping_sym) &
                  !is.na(!!weights_sym)
              ) |>
              dplyr::group_by(!!grouping_sym, !!rlang::sym(var_name)) |>
              dplyr::summarise(
                weighted_n = sum(!!weights_sym),
                n = dplyr::n(),
                .groups = "drop_last"
              ) |>
              dplyr::mutate(
                percentage = weighted_n / sum(weighted_n) * 100,
                variable = var_label,
                variable_index = i,
                category = as.character(!!rlang::sym(var_name)),
                group_var = as.character(!!grouping_sym)
              ) |>
              dplyr::ungroup() |>
              dplyr::select(
                variable,
                variable_index,
                group_var,
                category,
                percentage,
                n,
                weighted_n
              )

            # Calculate overall if requested
            if (show_overall) {
              df_var_overall <- df |>
                dplyr::filter(
                  !is.na(!!rlang::sym(var_name)) & !is.na(!!weights_sym)
                ) |>
                dplyr::group_by(!!rlang::sym(var_name)) |>
                dplyr::summarise(
                  weighted_n = sum(!!weights_sym),
                  n = dplyr::n(),
                  .groups = "drop"
                ) |>
                dplyr::mutate(
                  percentage = weighted_n / sum(weighted_n) * 100,
                  variable = var_label,
                  variable_index = i,
                  category = as.character(!!rlang::sym(var_name)),
                  group_var = overall_label
                ) |>
                dplyr::select(
                  variable,
                  variable_index,
                  group_var,
                  category,
                  percentage,
                  n,
                  weighted_n
                )

              df_var <- dplyr::bind_rows(df_var_overall, df_var_grouped)
            } else {
              df_var <- df_var_grouped
            }
          } else {
            df_var_grouped <- df |>
              dplyr::filter(
                !is.na(!!rlang::sym(var_name)) & !is.na(!!grouping_sym)
              ) |>
              dplyr::group_by(!!grouping_sym, !!rlang::sym(var_name)) |>
              dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
              dplyr::mutate(
                percentage = n / sum(n) * 100,
                variable = var_label,
                variable_index = i,
                category = as.character(!!rlang::sym(var_name)),
                group_var = as.character(!!grouping_sym)
              ) |>
              dplyr::ungroup() |>
              dplyr::select(
                variable,
                variable_index,
                group_var,
                category,
                percentage,
                n
              )

            # Calculate overall if requested
            if (show_overall) {
              df_var_overall <- df |>
                dplyr::filter(!is.na(!!rlang::sym(var_name))) |>
                dplyr::count(!!rlang::sym(var_name), name = "n") |>
                dplyr::mutate(
                  percentage = n / sum(n) * 100,
                  variable = var_label,
                  variable_index = i,
                  category = as.character(!!rlang::sym(var_name)),
                  group_var = overall_label
                ) |>
                dplyr::select(
                  variable,
                  variable_index,
                  group_var,
                  category,
                  percentage,
                  n
                )

              df_var <- dplyr::bind_rows(df_var_overall, df_var_grouped)
            } else {
              df_var <- df_var_grouped
            }
          }
        } else {
          # No grouping
          if (weighted) {
            weights_sym <- rlang::sym(weights_col)

            df_var <- df |>
              dplyr::filter(
                !is.na(!!rlang::sym(var_name)) & !is.na(!!weights_sym)
              ) |>
              dplyr::group_by(!!rlang::sym(var_name)) |>
              dplyr::summarise(
                weighted_n = sum(!!weights_sym),
                n = dplyr::n(),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                percentage = weighted_n / sum(weighted_n) * 100,
                variable = var_label,
                variable_index = i,
                category = as.character(!!rlang::sym(var_name)),
                group_var = var_label
              ) |>
              dplyr::select(
                variable,
                variable_index,
                group_var,
                category,
                percentage,
                n,
                weighted_n
              )
          } else {
            df_var <- df |>
              dplyr::filter(!is.na(!!rlang::sym(var_name))) |>
              dplyr::count(!!rlang::sym(var_name), name = "n") |>
              dplyr::mutate(
                percentage = n / sum(n) * 100,
                variable = var_label,
                variable_index = i,
                category = as.character(!!rlang::sym(var_name)),
                group_var = var_label
              ) |>
              dplyr::select(
                variable,
                variable_index,
                group_var,
                category,
                percentage,
                n
              )
          }
        }

        df_long_list[[i]] <- df_var
      }

      # Combine all variables
      df_plot <- dplyr::bind_rows(df_long_list)

      # Create labels
      df_plot <- df_plot |>
        dplyr::mutate(label = sprintf("%.1f%%", percentage))

      # Determine per-variable factor levels (original order from source data)
      var_levels_list <- lapply(seq_along(category_vars), function(i) {
        var_name <- category_vars[i]
        var_label_i <- final_category_labels[i]
        if (is.factor(df[[var_name]])) {
          levels(df[[var_name]])
        } else {
          df_plot |>
            dplyr::filter(variable == var_label_i, !is.na(category)) |>
            dplyr::pull(category) |>
            unique() |>
            sort()
        }
      })

      # Convert category to an ordered factor with reversed per-variable levels so that
      # the 1st factor level (most severe) appears at the TOP of the stacked bar.
      # ggplot2 stacks bottom-to-top in factor level order, so reversing puts level 1 at top.
      # "Spacer" is intentionally excluded from the factor levels so it never appears in legends.
      all_reversed_levels <- unique(unlist(lapply(var_levels_list, rev)))
      df_plot$category <- factor(df_plot$category, levels = all_reversed_levels)

      # Reorder group_var to ensure Overall comes first if present
      if (show_overall && has_grouping) {
        all_groups <- unique(df_plot$group_var)
        other_groups <- setdiff(all_groups, overall_label)
        df_plot$group_var <- factor(
          df_plot$group_var,
          levels = c(overall_label, sort(other_groups))
        )
      }

      # Create x-axis variable - use discrete factor for proper stacking
      if (has_grouping) {
        # For the grouped case the x-axis shows group names; indicator names are rendered
        # as facet strip labels at the bottom (two-level x-axis via facet_grid switch="x").
        df_plot <- df_plot |>
          dplyr::mutate(x_var = as.character(group_var))

        # Order x_var: Overall first if present, then sorted groups
        if (show_overall) {
          other_x <- setdiff(unique(df_plot$x_var), overall_label)
          x_levels <- c(overall_label, sort(other_x))
        } else {
          x_levels <- sort(unique(df_plot$x_var))
        }
        df_plot$x_var <- factor(df_plot$x_var, levels = x_levels)
      } else {
        df_plot <- df_plot |>
          dplyr::mutate(
            x_var = variable
          )

        # Create ordered factor for x_var to maintain grouping
        df_plot$x_var <- factor(df_plot$x_var, levels = unique(df_plot$x_var))
      }

      # Auto-subtitle with n
      total_n <- nrow(df)
      if (weighted) {
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          sum(df[[weights_col]], na.rm = TRUE)
        )
      } else {
        auto_subtitle <- sprintf("n = %d", total_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # SEPARATE LEGENDS APPROACH WITH GGNEWSCALE

      if (separate_legends && length(category_vars) > 1) {
        # Create base plot
        g <- ggplot2::ggplot() +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(x * 100, "%"),
            expand = c(0, 0)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(subtitle = final_subtitle)

        # Add layers for each variable with separate fill scales
        for (i in seq_along(category_vars)) {
          var_label <- final_category_labels[i]
          var_palette <- color_palettes[i]

          # Filter data for this variable only
          df_var <- df_plot |>
            dplyr::filter(variable == var_label)

          # Get colors for this variable
          # Check the original df for factor levels (not df_var which has category as character)
          # so all levels (including unused ones) are used for consistent color-level mapping.
          var_name <- category_vars[i]
          if (is.factor(df[[var_name]])) {
            all_levels <- levels(df[[var_name]])
          } else {
            all_levels <- unique(df_var$category)
          }
          n_colors <- length(all_levels)
          colors <- phrutils::get_color_palette(
            type = var_palette,
            n = n_colors
          )
          color_map <- setNames(colors, all_levels)

          # Add this variable's bars
          g <- g +
            ggplot2::geom_bar(
              data = df_var,
              ggplot2::aes(x = x_var, y = percentage, fill = category),
              stat = "identity",
              position = "fill",
              width = 1 - bar_spacing
            ) +
            ggplot2::scale_fill_manual(
              values = color_map,
              name = legend_labels[i],
              guide = ggplot2::guide_legend(reverse = TRUE),
              drop = FALSE
            )

          # Add new fill scale for next variable (except on last iteration)
          if (i < length(category_vars)) {
            g <- g + ggnewscale::new_scale_fill()
          }
        }

        # Add legend positioning
        g <- g +
          ggplot2::theme(
            legend.position = legend_position,
            legend.box = "vertical",
            legend.spacing.y = ggplot2::unit(0.5, "cm")
          )
      } else {
        # COMBINED LEGEND APPROACH (ORIGINAL)

        # Build per-variable color maps using each variable's own palette and original
        # factor levels, then combine them. First occurrence wins for duplicate level names.
        combined_colors <- c()
        for (i in seq_along(category_vars)) {
          var_levels_i <- var_levels_list[[i]]
          n_colors_i <- length(var_levels_i)
          colors_i <- phrutils::get_color_palette(
            type = color_palettes[i],
            n = n_colors_i
          )
          new_entries <- setNames(colors_i, var_levels_i)
          new_entries <- new_entries[
            !names(new_entries) %in% names(combined_colors)
          ]
          combined_colors <- c(combined_colors, new_entries)
        }

        # Create base plot with combined legend
        g <- ggplot2::ggplot() +
          ggplot2::geom_bar(
            data = df_plot,
            ggplot2::aes(x = x_var, y = percentage, fill = category),
            stat = "identity",
            position = "fill",
            width = 1 - bar_spacing
          ) +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(x * 100, "%"),
            expand = c(0, 0)
          ) +
          ggplot2::scale_fill_manual(
            values = combined_colors,
            name = legend_labels[1],
            guide = ggplot2::guide_legend(reverse = TRUE),
            drop = FALSE
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(subtitle = final_subtitle) +
          ggplot2::theme(legend.position = legend_position)
      }

      # When grouping is provided, use facet_grid with strip labels at the bottom to
      # produce a two-level x-axis: group names as primary labels, indicator names as
      # secondary labels spanning all bars for that indicator.
      if (has_grouping) {
        # Order the variable facet panels by variable_index
        df_plot$variable <- factor(
          df_plot$variable,
          levels = final_category_labels
        )
        g <- g +
          ggplot2::facet_grid(
            . ~ variable,
            scales = "free_x",
            space = "free",
            switch = "x"
          ) +
          ggplot2::theme(
            strip.placement = "outside",
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_text(face = "bold")
          )
      }

      # Apply coordinate flip and axis styling
      if (flip_coordinates) {
        g <- g +
          ggplot2::coord_flip() +
          ggplot2::labs(x = x_label %||% "", y = y_label)
      } else {
        g <- g +
          ggplot2::labs(x = x_label %||% "", y = y_label) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            )
          )
      }

      # Add labels if requested
      if (show_labels) {
        df_plot_labels <- df_plot |>
          dplyr::filter(!is.na(category))

        # Use position_fill(vjust = 0.5) so ggplot2's own stacking logic places every
        # label at the exact midpoint of its bar segment, guaranteed to match the bars.
        g <- g +
          ggplot2::geom_text(
            data = df_plot_labels,
            ggplot2::aes(
              x = x_var,
              y = percentage,
              group = category,
              label = label
            ),
            position = ggplot2::position_fill(vjust = 0.5),
            size = 3,
            color = "black"
          )
      }

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Distribution of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Grouped Bar Chart for Select Multiple Responses
#'
#' Creates grouped bar charts for select-multiple (multi-select) survey columns where
#' each cell contains space-separated response tokens (e.g.
#' \code{"barrier_cost barrier_distance"}).  Survey-weighted proportions for each
#' unique response token are computed via
#' \code{phr_calc_multiple_choice_cat}, so the weights embedded in
#' \code{survey_design} are applied automatically.  Bars do not add up to 100\%
#' because respondents may select more than one option.
#'
#' @param survey_design A \code{srvyr} or \code{survey} design object (e.g.
#'   created with \code{srvyr::as_survey_design()}).
#' @param var_name Character scalar; name of the column in \code{survey_design}
#'   that contains the space-separated select-multiple responses.
#' @param response_labels Optional named or positional character vector for
#'   mapping response tokens to display labels.
#'   \itemize{
#'     \item If \code{NULL} (default), the raw token strings are used as labels.
#'     \item If a \strong{named} vector, names must match the unique response
#'       tokens; each matching token is replaced by its label.
#'     \item If an \strong{unnamed} positional vector, it must have the same
#'       length as the number of unique tokens (tokens are sorted alphabetically
#'       by \code{phr_calc_multiple_choice_cat}) and is applied in that order.
#'   }
#' @param grouping Optional character scalar; column name to use for
#'   disaggregation.  When supplied, separate bars per group are shown side by
#'   side for each response token.  If \code{NULL}, an overall plot is produced.
#' @param calc_percentage Logical; if \code{TRUE} (default) bars show the
#'   survey-weighted percentage of respondents who selected each token.  If
#'   \code{FALSE}, bars show the unweighted count.
#' @param flip_coordinates Logical; if \code{TRUE} (default) creates horizontal
#'   bars (easier to read long response labels).
#' @param color_palette Character; color palette to use.  Options:
#'   \code{"reach1"}, \code{"reach2"}, \code{"reach3"}, \code{"reach4"},
#'   \code{"traffic_light"}, \code{"default"}.  Default is \code{"reach2"}.
#' @param title_name Optional character scalar for the plot title.
#' @param variable_label Optional character scalar used to auto-generate the
#'   title when \code{title_name} is \code{NULL}.
#' @param grouping_label Optional character scalar; human-readable label for the
#'   grouping variable, appended to the auto-generated title.
#' @param subtitle Optional character scalar appended to the auto-generated
#'   subtitle (which shows \emph{n}).
#' @param x_label Character scalar for the x-axis label (default:
#'   \code{"Response Options"}).
#' @param y_label Optional character scalar for the y-axis label.  Defaults to
#'   \code{"Percentage"} or \code{"Count"} based on \code{calc_percentage}.
#' @param legend_label Optional character scalar for the legend title.  Defaults
#'   to the grouping column name.
#' @param show_labels Logical; if \code{TRUE}, percentage or count labels are
#'   shown on the bars (default: \code{FALSE}).
#' @param legend_position Position of the legend.  Default \code{"bottom"}.
#'   Options: \code{"bottom"}, \code{"top"}, \code{"left"}, \code{"right"},
#'   \code{"none"}.
#'
#' @return A \code{ggplot2} object showing the grouped bar chart.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Column with space-separated responses, e.g. "barrier_cost barrier_distance"
#'   plot_grouped_bar_multiple(survey_design, var_name = "barriers",
#'                             title_name = "Health Barriers")
#'
#'   # With disaggregation by district
#'   plot_grouped_bar_multiple(survey_design, var_name = "barriers",
#'                             grouping = "district",
#'                             title_name = "Health Barriers by District")
#' }

plot_grouped_bar_multiple <- function(
  survey_design,
  var_name,
  response_labels = NULL,
  grouping = NULL,
  calc_percentage = TRUE,
  flip_coordinates = TRUE,
  color_palette = "reach2",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_label = "Response Options",
  y_label = NULL,
  legend_label = NULL,
  show_labels = FALSE,
  legend_position = "bottom"
) {
  origin <- "plot_grouped_bar_multiple"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)

      # --- Input validation ---
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(var_name, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(var_name, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )

      phrutils::phr_validate_columns(
        df,
        var_name,
        origin = origin,
        hint = phr_txt(paste0(
          "Ensure column '",
          var_name,
          "' exists in the dataset"
        )),
        soft = FALSE
      )

      if (!is.null(grouping)) {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0(
            "Ensure the grouping column '",
            grouping,
            "' exists in the dataset"
          )),
          soft = FALSE
        )
      }

      # --- Default y-axis label ---
      if (is.null(y_label)) {
        y_label <- if (calc_percentage) "Percentage" else "Count"
      }

      # --- Run phr_calc_multiple_choice_cat, with optional per-group disaggregation ---
      multiplier_val <- if (calc_percentage) 100 else 1

      if (is.null(grouping)) {
        # Overall calculation
        calc_res <- phr_calc_multiple_choice_cat(
          design = survey_design,
          var_name = var_name,
          indicator_name = var_name,
          multiplier = multiplier_val,
          group_name_label = "Overall"
        )

        if (
          is.null(calc_res) ||
            nrow(calc_res) == 0 ||
            all(is.na(calc_res$point.estimate))
        ) {
          phrutils::phr_warning(
            origin,
            paste0("No valid results returned for variable '", var_name, "'.")
          )
          return(invisible(NULL))
        }

        # Extract the response token from indicator_name (strip "{var_name} - " prefix)
        prefix <- paste0(var_name, " - ")
        calc_res$response <- sub(
          paste0("^", prefix),
          "",
          calc_res$indicator_name
        )

        # Apply response_labels
        calc_res$response <- .apply_response_labels(
          calc_res$response,
          response_labels
        )

        # Select value column
        if (calc_percentage) {
          df_plot <- calc_res |>
            dplyr::mutate(
              value = point.estimate,
              label = sprintf("%.1f%%", point.estimate)
            )
        } else {
          df_plot <- calc_res |>
            dplyr::mutate(
              value = n_unweighted,
              label = as.character(round(n_unweighted))
            )
        }

        # Build subtitle (denominator = answered records)
        denom_n <- if (
          !is.null(calc_res$denom_unweighted) &&
            any(!is.na(calc_res$denom_unweighted))
        ) {
          max(calc_res$denom_unweighted, na.rm = TRUE)
        } else {
          sum(
            !is.na(df[[var_name]]) &
              nzchar(trimws(as.character(df[[var_name]])))
          )
        }
        auto_subtitle <- sprintf("n = %d", as.integer(denom_n))
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        # Single-color palette
        colors <- phrutils::get_color_palette(type = color_palette, n = 1)

        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(x = stats::reorder(.data$response, value), y = value)
        ) +
          ggplot2::geom_bar(stat = "identity", fill = colors[1]) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = x_label, y = y_label, subtitle = final_subtitle)

        if (flip_coordinates) {
          g <- g + ggplot2::coord_flip()
          if (show_labels) {
            g <- g +
              ggplot2::geom_text(
                ggplot2::aes(label = label),
                hjust = -0.1,
                size = 3
              )
          }
        } else {
          if (show_labels) {
            g <- g +
              ggplot2::geom_text(
                ggplot2::aes(label = label),
                vjust = -0.5,
                size = 3
              )
          }
          g <- g +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        }
      } else {
        # Grouped calculation: call phr_calc_multiple_choice_cat per group subset
        group_levels <- sort(unique(stats::na.omit(df[[grouping]])))

        calc_res_list <- purrr::map(group_levels, function(g_val) {
          subset_design <- tryCatch(
            subset(survey_design, survey_design$variables[[grouping]] == g_val),
            error = function(e) {
              phrutils::phr_warning(
                origin,
                paste("Subset failed for", grouping, "=", g_val)
              )
              NULL
            }
          )
          if (is.null(subset_design)) {
            return(tibble::tibble())
          }

          res <- phr_calc_multiple_choice_cat(
            design = subset_design,
            var_name = var_name,
            indicator_name = var_name,
            multiplier = multiplier_val,
            group_name_label = as.character(g_val)
          )
          res[[grouping]] <- as.character(g_val)
          res
        })

        calc_res <- dplyr::bind_rows(calc_res_list)

        if (
          is.null(calc_res) ||
            nrow(calc_res) == 0 ||
            all(is.na(calc_res$point.estimate))
        ) {
          phrutils::phr_warning(
            origin,
            paste0(
              "No valid grouped results returned for variable '",
              var_name,
              "'."
            )
          )
          return(invisible(NULL))
        }

        # Extract response token
        prefix <- paste0(var_name, " - ")
        calc_res$response <- sub(
          paste0("^", prefix),
          "",
          calc_res$indicator_name
        )
        calc_res$response <- .apply_response_labels(
          calc_res$response,
          response_labels
        )

        if (calc_percentage) {
          df_plot <- calc_res |>
            dplyr::mutate(
              value = point.estimate,
              label = sprintf("%.1f%%", point.estimate)
            )
        } else {
          df_plot <- calc_res |>
            dplyr::mutate(
              value = n_unweighted,
              label = as.character(round(n_unweighted))
            )
        }

        # Build subtitle: n per group
        n_by_group <- df |>
          dplyr::filter(!is.na(!!rlang::sym(grouping))) |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(
            n = sum(
              !is.na(!!rlang::sym(var_name)) &
                nzchar(trimws(as.character(!!rlang::sym(var_name))))
            ),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), n)
          )

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        n_colors <- length(group_levels)
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )

        if (is.null(legend_label)) {
          legend_label <- grouping
        }

        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(
            x = stats::reorder(.data$response, value),
            y = value,
            fill = as.factor(!!rlang::sym(grouping))
          )
        ) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::scale_fill_manual(values = colors, name = legend_label) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = x_label, y = y_label, subtitle = final_subtitle)

        if (flip_coordinates) {
          g <- g + ggplot2::coord_flip()
          if (show_labels) {
            g <- g +
              ggplot2::geom_text(
                ggplot2::aes(label = label),
                position = ggplot2::position_dodge(width = 0.9),
                hjust = -0.1,
                size = 3
              )
          }
        } else {
          if (show_labels) {
            g <- g +
              ggplot2::geom_text(
                ggplot2::aes(label = label),
                position = ggplot2::position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3
              )
          }
          g <- g +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        }
      }

      # Add title
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(
          phr_txt("Response Distribution"),
          "-",
          variable_label
        )
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Apply response labels to a vector of token strings
#'
#' @param tokens Character vector of response tokens.
#' @param labels Optional named or positional character vector of labels.
#'   If \code{NULL}, \code{tokens} is returned unchanged.
#'   If named, matching token names are replaced by their corresponding label.
#'   If positional, must be the same length as \code{tokens}; otherwise a
#'   warning is issued and \code{tokens} is returned unchanged.
#' @return Character vector of (possibly relabelled) tokens.
#' @noRd
.apply_response_labels <- function(tokens, labels) {
  if (is.null(labels)) {
    return(tokens)
  }
  if (!is.null(names(labels))) {
    # Named vector: replace matching tokens
    idx <- match(tokens, names(labels))
    tokens[!is.na(idx)] <- labels[idx[!is.na(idx)]]
  } else {
    # Positional vector: must be same length
    if (length(labels) == length(tokens)) {
      tokens <- labels
    } else {
      warning(paste0(
        ".apply_response_labels: positional response_labels length (",
        length(labels),
        ") does not match the number of unique tokens (",
        length(tokens),
        "); labels were ignored."
      ))
    }
  }
  tokens
}


#' Plot Box and Whisker Plot
#'
#' Creates box and whisker plots for showing the distribution of numeric variables,
#' either overall or grouped by a categorical variable. Supports survey weights.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param numeric_var Inputs a character value specifying the column name for the numeric variable to plot.
#' @param grouping Inputs an optional character value specifying the column name for grouping the data.
#'   If NULL, creates a single overall box plot. If provided, creates separate box plots for each group.
#' @param weighted Logical. If TRUE, applies survey weights. By default: FALSE.
#'   Note: Weighted box plots show weighted quantiles and may differ from unweighted distributions.
#' @param weights_col Character. Column name containing survey weights. Required if weighted = TRUE.
#' @param show_outliers Inputs a logical value indicating whether to show outlier points (default: TRUE).
#' @param show_mean Inputs a logical value indicating whether to show mean as a point (default: FALSE).
#' @param flip_coordinates Logical. If TRUE, creates horizontal box plots. By default: FALSE.
#' @param color_palette Inputs an optional character value specifying the color palette to use.
#'   Options: "reach1", "reach2", "reach3", "reach4", "traffic_light", "default". Default is "reach3".
#'   Only used when fill_color is NULL.
#' @param show_overall Logical. If TRUE and a grouping column is provided, adds an 'Overall' box showing
#'   the distribution across all data. Default: TRUE. Only applies when grouping is not NULL.
#' @param overall_label Character. Label for the overall group when show_overall = TRUE. Default: "Overall".
#' @param title_name Inputs an optional character value for the title of the plot.
#' @param variable_label Character. Optional human-readable label for the numeric variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Character. Optional human-readable label for the grouping variable, appended
#'   to the auto-generated title as ", by <grouping_label>". Defaults to the column name when NULL.
#' @param subtitle Inputs an optional character value for the subtitle of the plot.
#'   If NULL, automatically displays n (number of records). Custom subtitle will be appended to n display.
#' @param x_label Inputs an optional character value for the x-axis label.
#' @param y_label Inputs an optional character value for the y-axis label.
#' @param legend_label Inputs an optional character value for the legend title. If NULL, uses grouping variable name.
#' @param fill_color Inputs an optional character value or vector for box fill colors.
#'   If provided, overrides color_palette.
#' @param show_labels Inputs a logical value indicating whether to show value labels (default: FALSE).
#'
#' @param legend_position Position of the legend. By default: "bottom". Options: "bottom", "top", "left", "right", "none".
#' @return Returns a ggplot2 object showing the box and whisker plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Unweighted with auto-generated title
#'   plot_boxplot(df, numeric_var = "muac", grouping = "district",
#'                variable_label = "MUAC (cm)", grouping_label = "District")
#'
#'   # Weighted
#'   plot_boxplot(df, numeric_var = "muac", grouping = "district",
#'                weighted = TRUE, weights_col = "survey_weight",
#'                title_name = "MUAC Distribution by District (Weighted)")
#' }

plot_boxplot <- function(
  survey_design,
  numeric_var,
  grouping = NULL,
  weighted = FALSE,
  weights_col = NULL,
  show_outliers = TRUE,
  show_mean = FALSE,
  show_overall = TRUE,
  overall_label = "Overall",
  flip_coordinates = FALSE,
  color_palette = "reach3",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  legend_label = NULL,
  fill_color = NULL,
  show_labels = FALSE,
  legend_position = "bottom"
) {
  origin <- "plot_boxplot"

  phrutils::phr_try(
    {
      df <- phrutils::phr_get_data_from_design(survey_design)
      # Validate inputs
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_not_null(
        numeric_var,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )

      phrutils::phr_validate_columns(
        df,
        numeric_var,
        origin = origin,
        hint = phr_txt(paste0(
          "Ensure the numeric column '",
          numeric_var,
          "' exists in the dataset"
        )),
        soft = FALSE
      )

      # Check if numeric_var is actually numeric
      phrutils::phr_validate_all_numeric(
        df[[numeric_var]],
        origin = origin,
        soft = FALSE
      )

      # Validate weights if requested
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )

        # Validate weights column is numeric
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )

        # Coerce to numeric
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Set default y-axis label based on orientation
      if (is.null(y_label)) {
        y_label <- if (flip_coordinates) numeric_var else numeric_var
      }

      # Set outlier display
      outlier_shape <- if (show_outliers) 16 else NA

      # For weighted boxplots, we need to calculate weighted quantiles manually
      if (weighted) {
        # Calculate weighted quantiles
        if (is.null(grouping)) {
          df_clean <- df |>
            dplyr::filter(
              !is.na(!!rlang::sym(numeric_var)) &
                !is.na(!!rlang::sym(weights_col))
            )

          weighted_stats <- data.frame(
            group = "Overall",
            ymin = wtd_quantile(
              df_clean[[numeric_var]],
              weights = df_clean[[weights_col]],
              probs = 0
            ),
            lower = wtd_quantile(
              df_clean[[numeric_var]],
              weights = df_clean[[weights_col]],
              probs = 0.25
            ),
            middle = wtd_quantile(
              df_clean[[numeric_var]],
              weights = df_clean[[weights_col]],
              probs = 0.50
            ),
            upper = wtd_quantile(
              df_clean[[numeric_var]],
              weights = df_clean[[weights_col]],
              probs = 0.75
            ),
            ymax = wtd_quantile(
              df_clean[[numeric_var]],
              weights = df_clean[[weights_col]],
              probs = 1
            )
          )
        } else {
          df_clean <- df |>
            dplyr::filter(
              !is.na(!!rlang::sym(numeric_var)) &
                !is.na(!!rlang::sym(weights_col))
            )

          weighted_stats <- df_clean |>
            dplyr::group_by(!!rlang::sym(grouping)) |>
            dplyr::summarise(
              ymin = wtd_quantile(
                !!rlang::sym(numeric_var),
                weights = !!rlang::sym(weights_col),
                probs = 0
              ),
              lower = wtd_quantile(
                !!rlang::sym(numeric_var),
                weights = !!rlang::sym(weights_col),
                probs = 0.25
              ),
              middle = wtd_quantile(
                !!rlang::sym(numeric_var),
                weights = !!rlang::sym(weights_col),
                probs = 0.50
              ),
              upper = wtd_quantile(
                !!rlang::sym(numeric_var),
                weights = !!rlang::sym(weights_col),
                probs = 0.75
              ),
              ymax = wtd_quantile(
                !!rlang::sym(numeric_var),
                weights = !!rlang::sym(weights_col),
                probs = 1
              ),
              .groups = "drop"
            )
          weighted_stats <- weighted_stats |>
            dplyr::rename(group = !!rlang::sym(grouping)) |>
            dplyr::mutate(group = as.character(group))

          # Prepend an overall row if show_overall is TRUE
          if (show_overall) {
            if (!is.character(overall_label)) {
              overall_label <- "Overall"
            }
            overall_ws_row <- data.frame(
              group = overall_label,
              ymin = wtd_quantile(
                df_clean[[numeric_var]],
                weights = df_clean[[weights_col]],
                probs = 0
              ),
              lower = wtd_quantile(
                df_clean[[numeric_var]],
                weights = df_clean[[weights_col]],
                probs = 0.25
              ),
              middle = wtd_quantile(
                df_clean[[numeric_var]],
                weights = df_clean[[weights_col]],
                probs = 0.50
              ),
              upper = wtd_quantile(
                df_clean[[numeric_var]],
                weights = df_clean[[weights_col]],
                probs = 0.75
              ),
              ymax = wtd_quantile(
                df_clean[[numeric_var]],
                weights = df_clean[[weights_col]],
                probs = 1
              )
            )
            weighted_stats <- dplyr::bind_rows(overall_ws_row, weighted_stats)
            other_ws_groups <- setdiff(
              as.character(weighted_stats$group),
              overall_label
            )
            weighted_stats$group <- factor(
              weighted_stats$group,
              levels = c(overall_label, sort(other_ws_groups))
            )
          }
        }
      }

      # Prepare data and create plot
      if (is.null(grouping)) {
        # Overall plot
        df_plot <- df |>
          dplyr::filter(!is.na(!!rlang::sym(numeric_var))) |>
          dplyr::mutate(group = "Overall")

        # Create subtitle with n
        total_n <- nrow(df_plot)
        if (weighted) {
          auto_subtitle <- sprintf("n = %d (weighted)", total_n)
        } else {
          auto_subtitle <- sprintf("n = %d", total_n)
        }
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        if (weighted) {
          # Use weighted stats for boxplot
          g <- ggplot2::ggplot(weighted_stats, ggplot2::aes(x = .data$group))

          if (!is.null(fill_color)) {
            g <- g +
              ggplot2::geom_boxplot(
                stat = "identity",
                ggplot2::aes(
                  ymin = ymin,
                  lower = .data$lower,
                  middle = .data$middle,
                  upper = .data$upper,
                  ymax = ymax
                ),
                fill = fill_color[1]
              )
          } else {
            colors <- phrutils::get_color_palette(type = color_palette, n = 1)
            g <- g +
              ggplot2::geom_boxplot(
                stat = "identity",
                ggplot2::aes(
                  ymin = ymin,
                  lower = .data$lower,
                  middle = .data$middle,
                  upper = .data$upper,
                  ymax = ymax
                ),
                fill = colors[1]
              )
          }
        } else {
          # Unweighted boxplot
          g <- ggplot2::ggplot(
            df_plot,
            ggplot2::aes(x = .data$group, y = !!rlang::sym(numeric_var))
          )

          if (!is.null(fill_color)) {
            g <- g +
              ggplot2::geom_boxplot(
                outlier.shape = outlier_shape,
                fill = fill_color[1]
              )
          } else {
            colors <- phrutils::get_color_palette(type = color_palette, n = 1)
            g <- g +
              ggplot2::geom_boxplot(
                outlier.shape = outlier_shape,
                fill = colors[1]
              )
          }
        }

        g <- g +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(
            x = x_label %||% "",
            y = y_label,
            subtitle = final_subtitle
          )
      } else {
        # Grouped plot
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0(
            "Ensure the grouping column '",
            grouping,
            "' exists in the dataset"
          )),
          soft = FALSE
        )

        # Ensure overall_label is a valid string
        if (show_overall && !is.character(overall_label)) {
          overall_label <- "Overall"
        }

        df_plot <- df |>
          dplyr::filter(!is.na(!!rlang::sym(numeric_var))) |>
          dplyr::mutate(
            !!rlang::sym(grouping) := as.character(!!rlang::sym(grouping))
          )

        # Create subtitle with n by group (from original grouped data, before adding overall)
        n_by_group <- df_plot |>
          dplyr::group_by(!!rlang::sym(grouping)) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop")

        if (weighted) {
          n_by_group <- n_by_group |>
            dplyr::mutate(
              group_label = sprintf(
                "%s (n=%d, weighted)",
                !!rlang::sym(grouping),
                n
              )
            )
        } else {
          n_by_group <- n_by_group |>
            dplyr::mutate(
              group_label = sprintf("%s (n=%d)", !!rlang::sym(grouping), n)
            )
        }

        auto_subtitle <- paste(n_by_group$group_label, collapse = "; ")
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        # Add overall group rows to df_plot (used by unweighted boxplot and show_mean)
        if (show_overall) {
          df_overall_rows <- df |>
            dplyr::filter(!is.na(!!rlang::sym(numeric_var))) |>
            dplyr::mutate(!!rlang::sym(grouping) := overall_label)
          df_plot <- dplyr::bind_rows(df_overall_rows, df_plot)
          other_groups <- setdiff(
            unique(as.character(df[[grouping]])),
            overall_label
          )
          df_plot[[grouping]] <- factor(
            df_plot[[grouping]],
            levels = c(overall_label, sort(other_groups))
          )
        }

        # Set legend label
        if (is.null(legend_label)) {
          legend_label <- grouping
        }

        if (weighted) {
          # Use weighted stats for boxplot (overall row already prepended above if show_overall)
          g <- ggplot2::ggplot(
            weighted_stats,
            ggplot2::aes(x = as.factor(.data$group))
          )

          if (!is.null(fill_color)) {
            g <- g +
              ggplot2::geom_boxplot(
                stat = "identity",
                ggplot2::aes(
                  ymin = ymin,
                  lower = .data$lower,
                  middle = .data$middle,
                  upper = .data$upper,
                  ymax = ymax,
                  fill = as.factor(.data$group)
                )
              ) +
              ggplot2::scale_fill_manual(
                values = fill_color,
                name = legend_label
              )
          } else {
            n_colors <- nrow(weighted_stats)
            colors <- phrutils::get_color_palette(
              type = color_palette,
              n = n_colors
            )
            g <- g +
              ggplot2::geom_boxplot(
                stat = "identity",
                ggplot2::aes(
                  ymin = ymin,
                  lower = .data$lower,
                  middle = .data$middle,
                  upper = .data$upper,
                  ymax = ymax,
                  fill = as.factor(.data$group)
                )
              ) +
              ggplot2::scale_fill_manual(values = colors, name = legend_label)
          }
        } else {
          # Unweighted boxplot (df_plot already contains overall rows when show_overall is TRUE)
          g <- ggplot2::ggplot(
            df_plot,
            ggplot2::aes(
              x = as.factor(!!rlang::sym(grouping)),
              y = !!rlang::sym(numeric_var)
            )
          )

          if (!is.null(fill_color)) {
            g <- g +
              ggplot2::geom_boxplot(
                outlier.shape = outlier_shape,
                ggplot2::aes(fill = as.factor(!!rlang::sym(grouping)))
              ) +
              ggplot2::scale_fill_manual(
                values = fill_color,
                name = legend_label
              )
          } else {
            n_colors <- length(unique(df_plot[[grouping]]))
            colors <- phrutils::get_color_palette(
              type = color_palette,
              n = n_colors
            )
            g <- g +
              ggplot2::geom_boxplot(
                outlier.shape = outlier_shape,
                ggplot2::aes(fill = as.factor(!!rlang::sym(grouping)))
              ) +
              ggplot2::scale_fill_manual(values = colors, name = legend_label)
          }
        }

        g <- g +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(
            x = x_label %||% grouping,
            y = y_label,
            subtitle = final_subtitle
          ) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
      }

      # Apply coordinate flip if requested
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      # Add mean points if requested
      if (show_mean) {
        if (is.null(grouping)) {
          if (weighted) {
            mean_val <- stats::weighted.mean(
              df_plot[[numeric_var]],
              w = df_plot[[weights_col]],
              na.rm = TRUE
            )
            mean_df <- data.frame(group = "Overall", mean_val = mean_val)
          } else {
            mean_df <- df_plot |>
              dplyr::summarise(
                mean_val = mean(!!rlang::sym(numeric_var), na.rm = TRUE),
                group = "Overall"
              )
          }
          g <- g +
            ggplot2::geom_point(
              data = mean_df,
              ggplot2::aes(x = .data$group, y = .data$mean_val),
              color = "red",
              size = 3,
              shape = 18
            )
        } else {
          # df_plot includes overall rows (when show_overall = TRUE) so group_by covers all groups
          if (weighted) {
            mean_df <- df_plot |>
              dplyr::group_by(!!rlang::sym(grouping)) |>
              dplyr::summarise(
                mean_val = stats::weighted.mean(
                  !!rlang::sym(numeric_var),
                  w = !!rlang::sym(weights_col),
                  na.rm = TRUE
                ),
                .groups = "drop"
              )
          } else {
            mean_df <- df_plot |>
              dplyr::group_by(!!rlang::sym(grouping)) |>
              dplyr::summarise(
                mean_val = mean(!!rlang::sym(numeric_var), na.rm = TRUE),
                .groups = "drop"
              )
          }
          g <- g +
            ggplot2::geom_point(
              data = mean_df,
              ggplot2::aes(
                x = as.factor(!!rlang::sym(grouping)),
                y = .data$mean_val
              ),
              color = "red",
              size = 3,
              shape = 18
            )
        }
      }

      # Add title if provided, or auto-generate from variable_label
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Distribution of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Treemap
#'
#' Creates a treemap visualization showing hierarchical proportions.
#' Supports single-level or two-level hierarchical treemaps.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param category_var Character, column name for the main category (required)
#' @param subcategory_var Character, optional column name for subcategory within main category (default: NULL)
#'   When provided, creates a hierarchical treemap showing subcategories nested within categories.
#' @param size_var Character, column name for numeric size values or NULL to count rows (default: NULL)
#' @param weighted Logical, whether to use weighted calculations (default: FALSE)
#' @param weights_col Character, name of the weights column (required if weighted = TRUE)
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Optional character string. Human-readable label for the category variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param label_size Numeric, label size in the treemap (default: 1)
#' @param legend_position Character, position of legend (default: "bottom")
#' @param legend_label Character, custom legend title (default: NULL, uses category_var name)
#'
#' @return A ggplot2 treemap object
#' @export
#'
#' @examples
#' \dontrun{
#'   # Single level treemap
#'   plot_treemap(df, category_var = "livelihood")
#'
#'   # Hierarchical treemap with subcategories
#'   plot_treemap(df, category_var = "sector", subcategory_var = "assistance_type")
#' }
plot_treemap <- function(
  survey_design,
  category_var,
  subcategory_var = NULL,
  size_var = NULL,
  weighted = FALSE,
  weights_col = NULL,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  label_size = 1,
  legend_position = "bottom",
  legend_label = NULL
) {
  origin <- "plot_treemap"

  phrutils::phr_try(
    {
      if (!requireNamespace("treemapify", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'treemapify' is required for plot_treemap(). Install it with: install.packages('treemapify')"
          ),
          origin = origin
        )
      }

      df <- phr_get_data_from_design(survey_design)
      # Apply ensure_value to all potentially NULL arguments early with appropriate non-NULL defaults
      color_palette <- ensure_value(color_palette, "reach1")
      label_size <- ensure_value(label_size, 1)
      legend_position <- ensure_value(legend_position, "bottom")
      weighted <- ensure_value(weighted, FALSE)

      # For optional text fields, use empty string as default
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      legend_label <- ensure_value(legend_label, "")

      # For optional column names that can be NULL, use special marker
      subcategory_var <- ensure_value(subcategory_var, NA_character_)
      size_var <- ensure_value(size_var, NA_character_)
      weights_col <- ensure_value(weights_col, NA_character_)

      # Standard validation block
      phrutils::phr_validate_not_null(
        category_var,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        label_size,
        origin = origin,
        soft = FALSE
      )

      phrutils::phr_validate_columns(
        df,
        category_var,
        origin = origin,
        hint = phr_txt(paste0(
          "Category column '",
          category_var,
          "' must exist in the dataset"
        )),
        soft = FALSE
      )

      # Convert NA_character_ back to NULL for logical checks
      if (is.na(subcategory_var)) {
        subcategory_var <- NULL
      }
      if (is.na(size_var)) {
        size_var <- NULL
      }
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (legend_label == "") {
        legend_label <- NULL
      }

      # Validate subcategory if provided
      if (!is.null(subcategory_var)) {
        phrutils::phr_validate_columns(
          df,
          subcategory_var,
          origin = origin,
          hint = phr_txt(paste0(
            "Subcategory column '",
            subcategory_var,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
      }

      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Auto-subtitle with n
      total_n <- nrow(df)
      if (weighted) {
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          sum(df[[weights_col]], na.rm = TRUE)
        )
      } else {
        auto_subtitle <- sprintf("n = %d", total_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Check for treemapify package
      if (!requireNamespace("treemapify", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'treemapify' is required for plot_treemap. Please install it: install.packages('treemapify')"
          ),
          origin = origin
        )
      }

      # Create category symbol once
      category_sym <- rlang::sym(category_var)

      # Determine if we have hierarchical structure
      has_subcategory <- !is.null(subcategory_var)

      # Prepare data based on whether we have subcategories
      if (has_subcategory) {
        # Hierarchical treemap
        subcategory_sym <- rlang::sym(subcategory_var)

        if (!is.null(size_var)) {
          phrutils::phr_validate_columns(
            df,
            size_var,
            origin = origin,
            hint = phr_txt(paste0("Size column '", size_var, "' must exist")),
            soft = FALSE
          )
          phrutils::phr_validate_all_numeric(
            df[[size_var]],
            origin = origin,
            soft = FALSE
          )

          size_sym <- rlang::sym(size_var)

          if (weighted) {
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(
                !is.na(!!category_sym) & !is.na(!!subcategory_sym)
              ) |>
              dplyr::group_by(!!category_sym, !!subcategory_sym) |>
              dplyr::summarise(
                value = sum(
                  !!weights_sym * as.numeric(!!size_sym),
                  na.rm = TRUE
                ),
                .groups = "drop"
              )
          } else {
            df_plot <- df |>
              dplyr::filter(
                !is.na(!!category_sym) & !is.na(!!subcategory_sym)
              ) |>
              dplyr::group_by(!!category_sym, !!subcategory_sym) |>
              dplyr::summarise(
                value = sum(as.numeric(!!size_sym), na.rm = TRUE),
                .groups = "drop"
              )
          }
        } else {
          # Count rows
          if (weighted) {
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(
                !is.na(!!category_sym) & !is.na(!!subcategory_sym)
              ) |>
              dplyr::group_by(!!category_sym, !!subcategory_sym) |>
              dplyr::summarise(
                value = sum(!!weights_sym, na.rm = TRUE),
                .groups = "drop"
              )
          } else {
            df_plot <- df |>
              dplyr::filter(
                !is.na(!!category_sym) & !is.na(!!subcategory_sym)
              ) |>
              dplyr::group_by(!!category_sym, !!subcategory_sym) |>
              dplyr::summarise(
                value = dplyr::n(),
                .groups = "drop"
              )
          }
        }

        # Rename columns to standard names
        df_plot <- df_plot |>
          dplyr::rename(
            category = !!category_sym,
            subcategory = !!subcategory_sym
          )

        # Add percentage (within each category)
        df_plot <- df_plot |>
          dplyr::group_by(category) |>
          dplyr::mutate(
            pct_within_category = value / sum(value) * 100,
            pct_overall = value / sum(df_plot$value) * 100
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            label = paste0(subcategory, "\n", sprintf("%.1f%%", pct_overall))
          )
      } else {
        # Single-level treemap (original logic)
        if (!is.null(size_var)) {
          phrutils::phr_validate_columns(
            df,
            size_var,
            origin = origin,
            hint = phr_txt(paste0("Size column '", size_var, "' must exist")),
            soft = FALSE
          )
          phrutils::phr_validate_all_numeric(
            df[[size_var]],
            origin = origin,
            soft = FALSE
          )

          size_sym <- rlang::sym(size_var)

          if (weighted) {
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(!is.na(!!category_sym)) |>
              dplyr::group_by(!!category_sym) |>
              dplyr::summarise(
                value = sum(
                  !!weights_sym * as.numeric(!!size_sym),
                  na.rm = TRUE
                ),
                .groups = "drop"
              )
          } else {
            df_plot <- df |>
              dplyr::filter(!is.na(!!category_sym)) |>
              dplyr::group_by(!!category_sym) |>
              dplyr::summarise(
                value = sum(as.numeric(!!size_sym), na.rm = TRUE),
                .groups = "drop"
              )
          }
        } else {
          # Count rows
          if (weighted) {
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(!is.na(!!category_sym)) |>
              dplyr::group_by(!!category_sym) |>
              dplyr::summarise(
                value = sum(!!weights_sym, na.rm = TRUE),
                .groups = "drop"
              )
          } else {
            df_plot <- df |>
              dplyr::filter(!is.na(!!category_sym)) |>
              dplyr::group_by(!!category_sym) |>
              dplyr::summarise(
                value = dplyr::n(),
                .groups = "drop"
              )
          }
        }

        # Rename the category column to a standard name
        df_plot <- df_plot |>
          dplyr::rename(category = !!category_sym)

        # Add percentage
        df_plot <- df_plot |>
          dplyr::mutate(
            pct = value / sum(value) * 100,
            label = paste0(category, "\n", sprintf("%.1f%%", pct))
          )
      }

      # Get colors
      if (has_subcategory) {
        # Color by main category
        n_cats <- length(unique(df_plot$category))
        colors <- phrutils::get_color_palette(type = color_palette, n = n_cats)
        # Create named color vector
        color_map <- setNames(colors, unique(df_plot$category))
      } else {
        n_cats <- nrow(df_plot)
        colors <- phrutils::get_color_palette(type = color_palette, n = n_cats)
      }

      # Sort by value descending for layout
      df_plot <- df_plot |> dplyr::arrange(dplyr::desc(value))

      # Set legend label
      final_legend_label <- if (is.null(legend_label)) {
        category_var
      } else {
        legend_label
      }

      # Create treemap using treemapify
      if (has_subcategory) {
        # Hierarchical treemap
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(
            area = value,
            fill = category,
            subgroup = category,
            label = label
          )
        ) +
          treemapify::geom_treemap() +
          treemapify::geom_treemap_subgroup_border(colour = "white", size = 3) +
          treemapify::geom_treemap_subgroup_text(
            place = "centre",
            grow = TRUE,
            alpha = 0.5,
            colour = "white",
            fontface = "bold",
            min.size = 0
          ) +
          treemapify::geom_treemap_text(
            colour = "white",
            place = "centre",
            size = label_size * 10,
            reflow = TRUE
          ) +
          ggplot2::scale_fill_manual(
            values = color_map,
            name = final_legend_label
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(subtitle = final_subtitle)
      } else {
        # Single-level treemap
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(area = value, fill = category, label = label)
        ) +
          treemapify::geom_treemap() +
          treemapify::geom_treemap_text(
            colour = "white",
            place = "centre",
            size = label_size * 12,
            reflow = TRUE
          ) +
          ggplot2::scale_fill_manual(
            values = colors,
            name = final_legend_label
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(subtitle = final_subtitle)
      }

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        g <- g + ggplot2::ggtitle(paste(phr_txt("Treemap of"), variable_label))
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Sankey Diagram
#'
#' Creates a Sankey/alluvial diagram showing flows between categories
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param axis_vars Character vector of 2+ column names representing flow axes (required)
#' @param axis_labels Character vector of custom labels for axes (optional, default: NULL uses axis_vars)
#'   Must be same length as axis_vars if provided
#' @param weighted Logical, whether to use weighted calculations (default: FALSE)
#' @param weights_col Character, name of the weights column (required if weighted = TRUE)
#' @param show_percentage Logical, whether to display as percentages instead of counts (default: FALSE)
#' @param show_stratum_labels Logical, whether to show category names on strata (default: TRUE)
#' @param show_stratum_stats Logical, whether to show XX.X\% (n) on strata (default: FALSE)
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Optional character string. Human-readable label for the flow variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param x_lab Character, x-axis label (default: NULL)
#' @param y_lab Character, y-axis label (default: NULL)
#' @param legend_position Character, position of legend (default: "bottom")
#' @param flip_coordinates Logical, whether to flip coordinates (default: FALSE)
#'
#' @return A ggplot2 alluvial/sankey diagram
#' @export
plot_sankey <- function(
  survey_design,
  axis_vars,
  axis_labels = NULL,
  weighted = FALSE,
  weights_col = NULL,
  show_percentage = FALSE,
  show_stratum_labels = TRUE,
  show_stratum_stats = FALSE,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE
) {
  origin <- "plot_sankey"

  phrutils::phr_try(
    {
      if (!requireNamespace("ggalluvial", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'ggalluvial' is required for plot_sankey(). Install it with: install.packages('ggalluvial')"
          ),
          origin = origin
        )
      }

      df <- phr_get_data_from_design(survey_design)
      # Apply ensure_value with non-NULL defaults
      color_palette <- ensure_value(color_palette, "reach1")
      legend_position <- ensure_value(legend_position, "bottom")
      weighted <- ensure_value(weighted, FALSE)
      show_percentage <- ensure_value(show_percentage, FALSE)
      show_stratum_labels <- ensure_value(show_stratum_labels, TRUE)
      show_stratum_stats <- ensure_value(show_stratum_stats, FALSE)
      flip_coordinates <- ensure_value(flip_coordinates, FALSE)

      # For optional text fields
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      x_lab <- ensure_value(x_lab, "")
      y_lab <- ensure_value(y_lab, "")

      # For optional column names and vectors
      weights_col <- ensure_value(weights_col, NA_character_)
      axis_labels <- ensure_value(axis_labels, NULL)

      # Standard validation block
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        show_percentage,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_stratum_labels,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_stratum_stats,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )

      # Convert back to NULL for logical checks
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (x_lab == "") {
        x_lab <- NULL
      }
      if (y_lab == "") {
        y_lab <- NULL
      }

      # Validate axis_vars
      phrutils::phr_validate_not_null(axis_vars, origin = origin, soft = FALSE)
      phrutils::phr_validate_vector_length(
        axis_vars,
        min_length = 2,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        axis_vars,
        origin = origin,
        hint = phr_txt("Ensure all axis columns exist in the dataset"),
        soft = FALSE
      )

      # Validate axis_labels if provided
      if (!is.null(axis_labels)) {
        phrutils::phr_validate_character(
          axis_labels,
          origin = origin,
          soft = FALSE
        )
        if (length(axis_labels) != length(axis_vars)) {
          phr_error(
            phr_txt(paste0(
              "axis_labels must be same length as axis_vars. Expected ",
              length(axis_vars),
              " labels but got ",
              length(axis_labels)
            )),
            origin = origin
          )
        }
        # Use custom labels
        final_axis_labels <- axis_labels
      } else {
        # Use axis_vars as labels
        final_axis_labels <- axis_vars
      }

      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Check for ggalluvial package
      if (!requireNamespace("ggalluvial", quietly = TRUE)) {
        phr_error(
          phr_txt(
            "Package 'ggalluvial' is required for plot_sankey. Please install it: install.packages('ggalluvial')"
          ),
          origin = origin
        )
      }

      # Filter out rows with NA in any axis variable
      df_plot <- df |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(axis_vars), ~ !is.na(.)))

      # ALWAYS aggregate data first
      if (weighted) {
        df_agg <- df_plot |>
          dplyr::group_by(dplyr::across(dplyr::all_of(axis_vars))) |>
          dplyr::summarise(
            value = sum(!!rlang::sym(weights_col), na.rm = TRUE),
            .groups = "drop"
          )
        total_value <- sum(df_agg$value, na.rm = TRUE)
      } else {
        df_agg <- df_plot |>
          dplyr::count(dplyr::across(dplyr::all_of(axis_vars)), name = "value")
        total_value <- sum(df_agg$value)
      }

      # Convert to percentage if requested
      if (show_percentage) {
        df_agg <- df_agg |>
          dplyr::mutate(plot_value = (value / total_value) * 100)
      } else {
        df_agg <- df_agg |>
          dplyr::mutate(plot_value = value)
      }

      # Calculate stratum-level statistics for labels
      stratum_stats_list <- list()
      for (i in seq_along(axis_vars)) {
        axis_col <- axis_vars[i]

        stratum_data <- df_agg |>
          dplyr::group_by(!!rlang::sym(axis_col)) |>
          dplyr::summarise(
            total_value = sum(value),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            percentage = (total_value / sum(total_value)) * 100,
            stratum = as.character(!!rlang::sym(axis_col))
          )

        if (weighted) {
          stratum_data <- stratum_data |>
            dplyr::mutate(
              label_stats = sprintf("%.1f%% (%.0f)", percentage, total_value),
              label_full = paste0(stratum, "\n", label_stats)
            )
        } else {
          stratum_data <- stratum_data |>
            dplyr::mutate(
              label_stats = sprintf(
                "%.1f%% (%d)",
                percentage,
                as.integer(total_value)
              ),
              label_full = paste0(stratum, "\n", label_stats)
            )
        }

        stratum_stats_list[[i]] <- stratum_data
      }

      # Combine all stratum stats
      all_stratum_stats <- dplyr::bind_rows(stratum_stats_list)

      # Create a lookup for labels
      label_lookup <- setNames(
        all_stratum_stats$label_full,
        all_stratum_stats$stratum
      )
      label_stats_lookup <- setNames(
        all_stratum_stats$label_stats,
        all_stratum_stats$stratum
      )

      # Auto-subtitle with n
      total_n <- nrow(df_plot)
      if (weighted) {
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          total_value
        )
      } else {
        auto_subtitle <- sprintf("n = %d", total_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Get colors based on first axis
      first_axis_col <- axis_vars[1]
      first_axis_vals <- unique(df_agg[[first_axis_col]])
      n_colors <- length(first_axis_vals)
      colors <- phrutils::get_color_palette(type = color_palette, n = n_colors)
      color_map <- setNames(colors, first_axis_vals)

      # Set default labels
      if (is.null(y_lab)) {
        y_lab <- if (show_percentage) {
          "Percentage (%)"
        } else if (weighted) {
          "Weighted Count"
        } else {
          "Count"
        }
      }

      if (is.null(x_lab)) {
        x_lab <- ""
      }

      # Create plot based on number of axes
      n_axes <- length(axis_vars)

      # Build base plot with aggregated data
      if (n_axes == 2) {
        g <- ggplot2::ggplot(
          data = df_agg,
          ggplot2::aes(
            y = .data$plot_value,
            axis1 = !!rlang::sym(axis_vars[1]),
            axis2 = !!rlang::sym(axis_vars[2])
          )
        )
      } else if (n_axes == 3) {
        g <- ggplot2::ggplot(
          data = df_agg,
          ggplot2::aes(
            y = .data$plot_value,
            axis1 = !!rlang::sym(axis_vars[1]),
            axis2 = !!rlang::sym(axis_vars[2]),
            axis3 = !!rlang::sym(axis_vars[3])
          )
        )
      } else if (n_axes == 4) {
        g <- ggplot2::ggplot(
          data = df_agg,
          ggplot2::aes(
            y = .data$plot_value,
            axis1 = !!rlang::sym(axis_vars[1]),
            axis2 = !!rlang::sym(axis_vars[2]),
            axis3 = !!rlang::sym(axis_vars[3]),
            axis4 = !!rlang::sym(axis_vars[4])
          )
        )
      } else {
        phr_error(
          phr_txt("plot_sankey currently supports 2-4 axes only"),
          origin = origin
        )
      }

      # Add layers
      g <- g +
        ggalluvial::geom_alluvium(ggplot2::aes(
          fill = !!rlang::sym(first_axis_col)
        )) +
        ggalluvial::geom_stratum()

      # Add labels based on options
      if (show_stratum_labels && show_stratum_stats) {
        # Show both category name and stats
        g <- g +
          ggplot2::geom_text(
            stat = "stratum",
            ggplot2::aes(label = ggplot2::after_stat(stratum)),
            size = 3,
            fontface = "bold"
          ) +
          ggplot2::geom_text(
            stat = "stratum",
            ggplot2::aes(
              label = ggplot2::after_stat({
                label_stats_lookup[as.character(stratum)]
              })
            ),
            size = 2.5,
            vjust = 2
          )
      } else if (show_stratum_stats) {
        # Show only stats (no category name)
        g <- g +
          ggplot2::geom_text(
            stat = "stratum",
            ggplot2::aes(
              label = ggplot2::after_stat({
                label_stats_lookup[as.character(stratum)]
              })
            ),
            size = 2.5
          )
      } else if (show_stratum_labels) {
        # Show only category names
        g <- g +
          ggplot2::geom_text(
            stat = "stratum",
            ggplot2::aes(label = ggplot2::after_stat(stratum)),
            size = 3
          )
      }

      # Use custom axis labels if provided
      g <- g +
        ggplot2::scale_fill_manual(
          values = color_map,
          name = final_axis_labels[1]
        ) +
        ggplot2::scale_x_discrete(
          limits = final_axis_labels,
          expand = c(0.15, 0.05)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = legend_position,
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          x = x_lab,
          y = y_lab,
          subtitle = final_subtitle
        )

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        g <- g +
          ggplot2::ggtitle(paste(phr_txt("Flow Diagram"), "-", variable_label))
      }

      # Apply coordinate flip if requested
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Bar Chart with Confidence Intervals for Percentages
#'
#' Creates a bar chart showing percentages with confidence intervals.
#' When a survey design object (class \code{tbl_svy} / \code{survey.design} /
#' \code{svyrep.design}) is supplied as \code{df}, the function uses
#' \code{phr_calc_survey_categorical_single} to compute survey-design-
#' corrected confidence intervals that account for clustering, stratification
#' and probability weights. When a plain data frame is supplied the original
#' binomial (or weighted) computation is used.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param category_var Character, column containing categories (required)
#' @param grouping Character, optional grouping variable (default: NULL)
#' @param weighted Logical, whether to use weighted calculations when \code{df}
#'   is a plain data frame (default: FALSE). Ignored when \code{df} is a
#'   survey design object.
#' @param weights_col Character, name of the weights column (required if
#'   \code{weighted = TRUE} and \code{df} is a data frame)
#' @param conf_level Numeric, confidence level used for plain data frame CI
#'   calculations (default: 0.95). Ignored for survey design objects.
#' @param show_labels Logical, show value labels on bars (default: FALSE)
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Character, variable label used in auto-title (default: NULL)
#' @param grouping_label Character, grouping label used in auto-title (default: NULL)
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param x_lab Character, x-axis label (default: NULL)
#' @param y_lab Character, y-axis label (default: "Percentage (\%)")
#' @param legend_label Character, legend title (default: NULL)
#' @param legend_position Character, position of legend (default: "bottom")
#' @param flip_coordinates Logical, whether to flip coordinates (default: FALSE)
#'
#' @return A ggplot2 bar chart with confidence intervals
#' @export
plot_ci_bar_percentage <- function(
  survey_design,
  category_var,
  grouping = NULL,
  weighted = FALSE,
  weights_col = NULL,
  conf_level = 0.95,
  show_labels = FALSE,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = "Percentage (%)",
  legend_label = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE
) {
  origin <- "plot_ci_bar_percentage"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      is_survey <- inherits(
        survey_design,
        c("tbl_svy", "survey.design", "survey.design2", "svyrep.design")
      )
      # Apply ensure_value with non-NULL defaults
      color_palette <- ensure_value(color_palette, "reach1")
      legend_position <- ensure_value(legend_position, "bottom")
      weighted <- ensure_value(weighted, FALSE)
      show_labels <- ensure_value(show_labels, FALSE)
      flip_coordinates <- ensure_value(flip_coordinates, FALSE)
      conf_level <- ensure_value(conf_level, 0.95)
      y_lab <- ensure_value(y_lab, "Percentage (%)")

      # For optional text fields, use empty string as default
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      x_lab <- ensure_value(x_lab, "")
      legend_label <- ensure_value(legend_label, "")

      # For optional column names that can be NULL, use special marker
      weights_col <- ensure_value(weights_col, NA_character_)
      grouping <- ensure_value(grouping, NA_character_)

      # Standard validation block — skip for survey design objects
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(show_labels, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        conf_level,
        origin = origin,
        soft = FALSE
      )

      # Convert NA_character_ back to NULL for logical checks
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (is.na(grouping)) {
        grouping <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (x_lab == "") {
        x_lab <- NULL
      }
      if (legend_label == "") {
        legend_label <- NULL
      }

      # Validate category_var
      phrutils::phr_validate_not_null(
        category_var,
        origin = origin,
        soft = FALSE
      )
      working_df <- df
      phrutils::phr_validate_columns(
        working_df,
        category_var,
        origin = origin,
        hint = phr_txt(paste0(
          "Category column '",
          category_var,
          "' must exist"
        )),
        soft = FALSE
      )

      if (!is.null(grouping)) {
        phrutils::phr_validate_columns(
          working_df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0("Grouping column '", grouping, "' must exist")),
          soft = FALSE
        )
      }

      # Determine if we have grouping
      has_grouping <- !is.null(grouping)

      # Validate that weights_col is provided when weighted = TRUE (applies to all paths)
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
      }

      # SURVEY DESIGN PATH: use phr_calc_survey_categorical_single

      if (is_survey) {
        ind_label <- variable_label %||% category_var

        if (!has_grouping) {
          # Single overall call
          calc_df <- phr_calc_survey_categorical_single(
            design = survey_design,
            var_name = category_var,
            indicator_name = ind_label,
            group_name_label = "Overall"
          )

          df_plot <- calc_df |>
            dplyr::mutate(
              x_var = gsub(
                paste0("^", ind_label, " - "),
                "",
                .data$indicator_name
              ),
              pct = .data$point.estimate,
              ci_lower = .data$lower_ci,
              ci_upper = .data$upper_ci,
              label = sprintf("%.1f%%", pct)
            )
        } else {
          # Per-group: filter design and call for each group value
          group_vals <- unique(df[[grouping]])
          group_vals <- group_vals[!is.na(group_vals)]

          calc_list <- lapply(group_vals, function(gv) {
            dsn_g <- dplyr::filter(survey_design, !!rlang::sym(grouping) == gv)
            r <- phr_calc_survey_categorical_single(
              design = dsn_g,
              var_name = category_var,
              indicator_name = ind_label,
              group_name_label = as.character(gv)
            )
            r$x_var <- as.character(gv)
            r$fill_var <- gsub(
              paste0("^", ind_label, " - "),
              "",
              r$indicator_name
            )
            r
          })

          df_plot <- dplyr::bind_rows(calc_list) |>
            dplyr::mutate(
              pct = .data$point.estimate,
              ci_lower = .data$lower_ci,
              ci_upper = .data$upper_ci,
              label = sprintf("%.1f%%", pct)
            )
        }

        # subtitle: use weighted n from results
        total_n <- if ("n_unweighted" %in% names(df_plot)) {
          sum(df_plot$n_unweighted, na.rm = TRUE)
        } else {
          nrow(df)
        }
        auto_subtitle <- sprintf("n = %d (survey design)", as.integer(total_n))
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }
      } else {
        # DATA FRAME PATH (original logic)

        if (weighted) {
          phrutils::phr_validate_not_null(
            weights_col,
            origin = origin,
            soft = FALSE
          )
          phrutils::phr_validate_columns(
            df,
            weights_col,
            origin = origin,
            hint = phr_txt(paste0(
              "Weights column '",
              weights_col,
              "' must exist in the dataset"
            )),
            soft = FALSE
          )
          phrutils::phr_validate_all_numeric(
            df[[weights_col]],
            origin = origin,
            soft = FALSE
          )
          df <- df |>
            dplyr::mutate(
              !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
            )
        }

        # Convert category_var to character
        df <- df |>
          dplyr::mutate(.cat_val = as.character(!!rlang::sym(category_var)))

        # Calculate statistics - SPLIT INTO SEPARATE BLOCKS to avoid symbol conversion issues
        if (!has_grouping) {
          if (weighted) {
            # Weighted, no grouping
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(!is.na(.cat_val)) |>
              dplyr::group_by(.cat_val) |>
              dplyr::summarise(
                n_total = dplyr::n(),
                n_cat = sum(!!weights_sym, na.rm = TRUE),
                .groups = "drop"
              )
          } else {
            # Unweighted, no grouping
            df_plot <- df |>
              dplyr::filter(!is.na(.cat_val)) |>
              dplyr::group_by(.cat_val) |>
              dplyr::summarise(
                n_total = dplyr::n(),
                n_cat = dplyr::n(),
                .groups = "drop"
              )
          }

          df_plot <- df_plot |>
            dplyr::mutate(
              total_weight = sum(n_cat),
              pct = n_cat / total_weight * 100,
              n_eff = sum(n_total),
              p = pct / 100,
              se = sqrt(p * (1 - p) / n_eff),
              z = qnorm(1 - (1 - conf_level) / 2),
              ci_lower = pmax(0, (p - z * se) * 100),
              ci_upper = pmin(100, (p + z * se) * 100),
              label = sprintf("%.1f%%", pct)
            ) |>
            dplyr::rename(x_var = .cat_val)
        } else {
          # Has grouping
          grouping_sym <- rlang::sym(grouping)

          if (weighted) {
            # Weighted with grouping
            weights_sym <- rlang::sym(weights_col)
            df_plot <- df |>
              dplyr::filter(!is.na(.cat_val) & !is.na(!!grouping_sym)) |>
              dplyr::group_by(!!grouping_sym, .cat_val) |>
              dplyr::summarise(
                n_total = dplyr::n(),
                n_cat = sum(!!weights_sym, na.rm = TRUE),
                .groups = "drop"
              )
          } else {
            # Unweighted with grouping
            df_plot <- df |>
              dplyr::filter(!is.na(.cat_val) & !is.na(!!grouping_sym)) |>
              dplyr::group_by(!!grouping_sym, .cat_val) |>
              dplyr::summarise(
                n_total = dplyr::n(),
                n_cat = dplyr::n(),
                .groups = "drop"
              )
          }

          df_plot <- df_plot |>
            dplyr::group_by(!!grouping_sym) |>
            dplyr::mutate(
              total_weight = sum(n_cat),
              pct = n_cat / total_weight * 100,
              n_eff = sum(n_total),
              p = pct / 100,
              se = sqrt(p * (1 - p) / n_eff),
              z = qnorm(1 - (1 - conf_level) / 2),
              ci_lower = pmax(0, (p - z * se) * 100),
              ci_upper = pmin(100, (p + z * se) * 100),
              label = sprintf("%.1f%%", pct)
            ) |>
            dplyr::ungroup() |>
            dplyr::rename(x_var = !!grouping_sym, fill_var = .cat_val)
        }

        # Auto-subtitle with n
        total_n <- nrow(df)
        if (weighted) {
          auto_subtitle <- sprintf(
            "n = %d (weighted n = %.0f)",
            total_n,
            sum(df[[weights_col]], na.rm = TRUE)
          )
        } else {
          auto_subtitle <- sprintf("n = %d", total_n)
        }
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }
      }

      # PLOTTING (shared for both paths)

      # Get colors
      n_cats <- if (has_grouping) {
        length(unique(df_plot$fill_var))
      } else {
        length(unique(df_plot$x_var))
      }
      colors <- phrutils::get_color_palette(type = color_palette, n = n_cats)

      # Set final labels with defaults
      final_x_lab <- if (is.null(x_lab)) {
        if (!has_grouping) category_var else grouping
      } else {
        x_lab
      }

      final_legend_label <- if (is.null(legend_label)) {
        category_var
      } else {
        legend_label
      }

      # Create plot
      if (!has_grouping) {
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(x = x_var, y = pct, fill = x_var)
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
            width = 0.2
          ) +
          ggplot2::scale_fill_manual(values = colors, guide = "none") +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            limits = c(0, NA)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = final_x_lab, y = y_lab, subtitle = final_subtitle)
      } else {
        g <- ggplot2::ggplot(
          df_plot,
          ggplot2::aes(x = x_var, y = pct, fill = .data$fill_var)
        ) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              ymin = .data$ci_lower,
              ymax = .data$ci_upper,
              group = .data$fill_var
            ),
            position = ggplot2::position_dodge(0.9),
            width = 0.2
          ) +
          ggplot2::scale_fill_manual(
            values = colors,
            name = final_legend_label
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            limits = c(0, NA)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = final_x_lab, y = y_lab, subtitle = final_subtitle)
      }

      # Add labels if requested
      if (show_labels) {
        if (!has_grouping) {
          g <- g +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              vjust = -0.5,
              size = 3
            )
        } else {
          g <- g +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              position = ggplot2::position_dodge(0.9),
              vjust = -0.5,
              size = 3
            )
        }
      }

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Prevalence of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      # Apply coordinate flip if requested
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Point Chart with Confidence Intervals for Means or Ratios
#'
#' Creates a point chart showing means or ratios with confidence intervals.
#' When a survey design object (class \code{tbl_svy} / \code{survey.design} /
#' \code{svyrep.design}) is supplied as \code{df}, the function uses
#' \code{phr_calc_survey_mean_single} (or
#' \code{phr_calc_survey_ratio_single} when \code{numeric_var2} is
#' provided) to compute survey-design-corrected confidence intervals that
#' account for clustering, stratification and probability weights. When a plain
#' data frame is supplied the original Wald CI computation is used.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param numeric_var Character, column for numeric values (required)
#' @param grouping Character, optional grouping variable (default: NULL)
#' @param numeric_var2 Character, optional second numeric variable for ratio
#'   (\code{numeric_var / numeric_var2}). When set, uses
#'   \code{phr_calc_survey_ratio_single} for survey design objects.
#'   (default: NULL)
#' @param weighted Logical, whether to use weighted calculations when \code{df}
#'   is a plain data frame (default: FALSE). Ignored for survey design objects.
#' @param weights_col Character, name of the weights column (required if
#'   \code{weighted = TRUE} and \code{df} is a data frame)
#' @param conf_level Numeric, confidence level for plain data frame CI
#'   calculations (default: 0.95). Ignored for survey design objects.
#' @param show_labels Logical, show value labels on points (default: FALSE)
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Character, variable label used in auto-title (default: NULL)
#' @param grouping_label Character, grouping label used in auto-title (default: NULL)
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param x_lab Character, x-axis label (default: NULL)
#' @param y_lab Character, y-axis label (default: NULL)
#' @param legend_label Character, legend title (default: NULL)
#' @param legend_position Character, position of legend (default: "bottom")
#' @param flip_coordinates Logical, whether to flip coordinates (default: FALSE)
#' @param point_size Numeric, size of points (default: 3)
#' @param ylim Numeric vector of length 2, y-axis limits (default: NULL, auto-calculated)
#'
#' @return A ggplot2 point chart with confidence intervals
#' @export
plot_ci_point_mean <- function(
  survey_design,
  numeric_var,
  grouping = NULL,
  numeric_var2 = NULL,
  weighted = FALSE,
  weights_col = NULL,
  conf_level = 0.95,
  show_labels = FALSE,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_label = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  point_size = 5,
  ylim = NULL
) {
  origin <- "plot_ci_point_mean"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      is_survey <- inherits(
        survey_design,
        c("tbl_svy", "survey.design", "survey.design2", "svyrep.design")
      )
      # Apply ensure_value with non-NULL defaults
      color_palette <- ensure_value(color_palette, "reach1")
      legend_position <- ensure_value(legend_position, "bottom")
      weighted <- ensure_value(weighted, FALSE)
      show_labels <- ensure_value(show_labels, FALSE)
      flip_coordinates <- ensure_value(flip_coordinates, FALSE)
      conf_level <- ensure_value(conf_level, 0.95)
      point_size <- ensure_value(point_size, 3)

      # For optional text fields
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      x_lab <- ensure_value(x_lab, "")
      y_lab <- ensure_value(y_lab, "")
      legend_label <- ensure_value(legend_label, "")

      # For optional column names
      weights_col <- ensure_value(weights_col, NA_character_)
      grouping <- ensure_value(grouping, NA_character_)
      numeric_var2 <- ensure_value(numeric_var2, NA_character_)

      # ylim stays as NULL if not provided (we'll handle it later)

      # Standard validation block — skip for survey design objects
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(show_labels, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        conf_level,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        point_size,
        origin = origin,
        soft = FALSE
      )

      # Validate ylim if provided
      if (!is.null(ylim)) {
        phrutils::phr_validate_all_numeric(ylim, origin = origin, soft = FALSE)
        if (length(ylim) != 2) {
          phr_error(
            phr_txt("ylim must be a numeric vector of length 2 (c(min, max))"),
            origin = origin
          )
        }
        if (ylim[1] >= ylim[2]) {
          phr_error(
            phr_txt("ylim[1] must be less than ylim[2]"),
            origin = origin
          )
        }
      }

      # Convert back to NULL
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (is.na(grouping)) {
        grouping <- NULL
      }
      if (is.na(numeric_var2)) {
        numeric_var2 <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (x_lab == "") {
        x_lab <- NULL
      }
      if (y_lab == "") {
        y_lab <- NULL
      }
      if (legend_label == "") {
        legend_label <- NULL
      }

      working_df <- df

      # Validate numeric_var
      phrutils::phr_validate_not_null(
        numeric_var,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        working_df,
        numeric_var,
        origin = origin,
        hint = phr_txt(paste0("Numeric column '", numeric_var, "' must exist")),
        soft = FALSE
      )

      if (!is_survey) {
        phrutils::phr_validate_all_numeric(
          df[[numeric_var]],
          origin = origin,
          soft = FALSE
        )
      }

      if (!is_survey && weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      if (!is.null(numeric_var2)) {
        phrutils::phr_validate_columns(
          working_df,
          numeric_var2,
          origin = origin,
          hint = phr_txt(paste0(
            "Second numeric column '",
            numeric_var2,
            "' must exist"
          )),
          soft = FALSE
        )
        if (!is_survey) {
          phrutils::phr_validate_all_numeric(
            df[[numeric_var2]],
            origin = origin,
            soft = FALSE
          )
        }
      }

      if (!is.null(grouping)) {
        phrutils::phr_validate_columns(
          working_df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0("Grouping column '", grouping, "' must exist")),
          soft = FALSE
        )
      }

      # Determine mode
      is_ratio <- !is.null(numeric_var2)
      has_grouping <- !is.null(grouping)

      # Set y-axis default label
      if (is.null(y_lab)) {
        y_lab <- if (is_ratio) {
          paste0("Ratio: ", numeric_var, " / ", numeric_var2)
        } else {
          paste0("Mean of ", numeric_var)
        }
      }

      # SURVEY DESIGN PATH: use phr_calc_survey_*_single

      if (is_survey) {
        ind_label <- variable_label %||% numeric_var

        .calc_one <- function(dsn, group_lbl) {
          if (is_ratio) {
            phr_calc_survey_ratio_single(
              design = dsn,
              numerator_var = numeric_var,
              denominator_var = numeric_var2,
              indicator_name = ind_label,
              group_name_label = group_lbl
            )
          } else {
            phr_calc_survey_mean_single(
              design = dsn,
              var_name = numeric_var,
              indicator_name = ind_label,
              group_name_label = group_lbl
            )
          }
        }

        if (!has_grouping) {
          calc_df <- .calc_one(survey_design, "Overall")
          stats_df <- data.frame(
            x_var = "Overall",
            est = calc_df$point.estimate,
            ci_lower = calc_df$lower_ci,
            ci_upper = calc_df$upper_ci,
            label = sprintf("%.2f", calc_df$point.estimate),
            stringsAsFactors = FALSE
          )
        } else {
          group_vals <- unique(df[[grouping]])
          group_vals <- group_vals[!is.na(group_vals)]

          stats_list <- lapply(group_vals, function(gv) {
            dsn_g <- dplyr::filter(survey_design, !!rlang::sym(grouping) == gv)
            r <- .calc_one(dsn_g, as.character(gv))
            data.frame(
              x_var = as.character(gv),
              est = r$point.estimate,
              ci_lower = r$lower_ci,
              ci_upper = r$upper_ci,
              label = sprintf("%.2f", r$point.estimate),
              stringsAsFactors = FALSE
            )
          })
          stats_df <- dplyr::bind_rows(stats_list)
        }

        total_n <- sum(!is.na(working_df[[numeric_var]]), na.rm = TRUE)
        auto_subtitle <- sprintf("n = %d (survey design)", as.integer(total_n))
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }
      } else {
        # DATA FRAME PATH (original logic)

        # Auto-subtitle with n
        total_n <- nrow(df)
        if (weighted) {
          auto_subtitle <- sprintf(
            "n = %d (weighted n = %.0f)",
            total_n,
            sum(df[[weights_col]], na.rm = TRUE)
          )
        } else {
          auto_subtitle <- sprintf("n = %d", total_n)
        }
        final_subtitle <- if (!is.null(subtitle)) {
          paste0(auto_subtitle, "; ", subtitle)
        } else {
          auto_subtitle
        }

        # Compute mean/ratio with CI
        z_val <- qnorm(1 - (1 - conf_level) / 2)

        compute_stats <- function(x, w = NULL, x2 = NULL) {
          x <- as.numeric(x[!is.na(x)])
          if (is_ratio && !is.null(x2)) {
            x2 <- as.numeric(x2[!is.na(x2)])
            n <- min(length(x), length(x2))
            x <- x[seq_len(n)]
            x2 <- x2[seq_len(n)]
            if (!is.null(w)) {
              w <- as.numeric(w[seq_len(n)])
              mu <- sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
              mu2 <- sum(x2 * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
              est <- mu / mu2
              se <- sd(x / x2, na.rm = TRUE) / sqrt(n)
            } else {
              est <- mean(x, na.rm = TRUE) / mean(x2, na.rm = TRUE)
              se <- sd(x / x2, na.rm = TRUE) / sqrt(n)
            }
          } else {
            n <- length(x)
            if (!is.null(w)) {
              w <- as.numeric(w[!is.na(w)])
              est <- sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
              se <- sqrt(
                sum(w * (x - est)^2, na.rm = TRUE) / sum(w, na.rm = TRUE)
              ) /
                sqrt(n)
            } else {
              est <- mean(x, na.rm = TRUE)
              se <- sd(x, na.rm = TRUE) / sqrt(n)
            }
          }
          data.frame(
            est = est,
            ci_lower = est - z_val * se,
            ci_upper = est + z_val * se
          )
        }

        if (!has_grouping) {
          w_vals <- if (weighted) df[[weights_col]] else NULL
          x2_vals <- if (is_ratio) df[[numeric_var2]] else NULL
          stats_df <- compute_stats(df[[numeric_var]], w = w_vals, x2 = x2_vals)
          stats_df$label <- sprintf("%.2f", stats_df$est)
        } else {
          grouping_sym <- rlang::sym(grouping)
          groups <- unique(df[[grouping]][!is.na(df[[grouping]])])

          stats_list <- lapply(groups, function(g_val) {
            df_g <- df |> dplyr::filter(!!grouping_sym == g_val)
            w_vals <- if (weighted) df_g[[weights_col]] else NULL
            x2_vals <- if (is_ratio) df_g[[numeric_var2]] else NULL
            s <- compute_stats(df_g[[numeric_var]], w = w_vals, x2 = x2_vals)
            s$x_var <- as.character(g_val)
            s
          })

          stats_df <- dplyr::bind_rows(stats_list)
          stats_df$label <- sprintf("%.2f", stats_df$est)
        }
      }

      # PLOTTING (shared for both paths)

      if (!has_grouping) {
        if (!"x_var" %in% names(stats_df)) {
          stats_df$x_var <- "Overall"
        }

        n_colors <- 1
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )
        final_x_lab <- if (is.null(x_lab)) "" else x_lab

        g <- ggplot2::ggplot(stats_df, ggplot2::aes(x = x_var, y = .data$est)) +
          ggplot2::geom_point(size = point_size, color = colors[1]) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
            width = 0.2,
            color = colors[1]
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = final_x_lab, y = y_lab, subtitle = final_subtitle)
      } else {
        n_colors <- length(unique(stats_df$x_var))
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )
        final_x_lab <- if (is.null(x_lab)) grouping else x_lab
        final_legend_label <- if (is.null(legend_label)) {
          grouping
        } else {
          legend_label
        }

        g <- ggplot2::ggplot(
          stats_df,
          ggplot2::aes(x = x_var, y = .data$est, color = x_var)
        ) +
          ggplot2::geom_point(size = point_size) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
            width = 0.2
          ) +
          ggplot2::scale_color_manual(
            values = colors,
            name = final_legend_label
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = final_x_lab, y = y_lab, subtitle = final_subtitle)
      }

      # Calculate default ylim if not provided
      if (is.null(ylim)) {
        max_ci <- max(stats_df$ci_upper, na.rm = TRUE)
        min_ci <- min(stats_df$ci_lower, na.rm = TRUE)

        # Default: lower limit is 0, upper limit is 10% above highest CI
        ylim_lower <- 0
        ylim_upper <- max_ci * 1.10

        # If all values are negative, adjust lower limit
        if (max_ci < 0) {
          ylim_lower <- min_ci * 1.10
          ylim_upper <- 0
        }

        # If data spans negative and positive, include both
        if (min_ci < 0 && max_ci > 0) {
          ylim_lower <- min_ci * 1.10
          ylim_upper <- max_ci * 1.10
        }

        final_ylim <- c(ylim_lower, ylim_upper)
      } else {
        final_ylim <- ylim
      }

      # Apply y-axis limits
      g <- g + ggplot2::ylim(final_ylim)

      if (show_labels) {
        g <- g +
          ggplot2::geom_text(ggplot2::aes(label = label), vjust = -1, size = 3)
      }

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- paste(phr_txt("Mean of"), variable_label)
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      # Apply coordinate flip if requested
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Plot Scatter Plot
#'
#' Creates a scatter plot between two numeric variables with optional grouping
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param x_var Character, column for x-axis (required)
#' @param y_var Character, column for y-axis (required)
#' @param grouping Character, optional column for coloring points (default: NULL)
#' @param weighted Logical, whether to use weighted calculations (default: FALSE)
#' @param weights_col Character, name of the weights column (when weighted=TRUE, point size is proportional to weight)
#' @param add_smooth Logical, add a smooth trend line (default: TRUE)
#' @param smooth_method Character, smooth method ("lm", "loess") (default: "lm")
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Optional character string. Human-readable label for the x variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param grouping_label Optional character string. Human-readable label for the grouping variable, appended
#'   to the auto-generated title. Defaults to the column name when NULL.
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param x_lab Character, x-axis label (default: NULL)
#' @param y_lab Character, y-axis label (default: NULL)
#' @param legend_label Character, legend title (default: NULL)
#' @param legend_position Character, position of legend (default: "bottom")
#' @param flip_coordinates Logical, whether to flip coordinates (default: FALSE)
#' @param point_alpha Numeric, point transparency (default: 0.6)
#'
#' @return A ggplot2 scatter plot
#' @export
plot_scatter <- function(
  survey_design,
  x_var,
  y_var,
  grouping = NULL,
  weighted = FALSE,
  weights_col = NULL,
  add_smooth = TRUE,
  smooth_method = "lm",
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  grouping_label = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_label = NULL,
  legend_position = "bottom",
  flip_coordinates = FALSE,
  point_alpha = 0.6
) {
  origin <- "plot_scatter"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Standard validation block
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        flip_coordinates,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )

      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Auto-subtitle with n
      total_n <- nrow(df)
      auto_subtitle <- if (weighted) {
        sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          sum(df[[weights_col]], na.rm = TRUE)
        )
      } else {
        sprintf("n = %d", total_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      phrutils::phr_validate_not_null(x_var, origin = origin, soft = FALSE)
      phrutils::phr_validate_not_null(y_var, origin = origin, soft = FALSE)
      phrutils::phr_validate_columns(
        df,
        c(x_var, y_var),
        origin = origin,
        hint = phr_txt("Ensure both x_var and y_var columns exist"),
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        df[[x_var]],
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        df[[y_var]],
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(add_smooth, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        smooth_method,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        point_alpha,
        origin = origin,
        soft = FALSE
      )

      if (!is.null(grouping)) {
        phrutils::phr_validate_columns(
          df,
          grouping,
          origin = origin,
          hint = phr_txt(paste0("Grouping column '", grouping, "' must exist")),
          soft = FALSE
        )
      }

      df <- df |>
        dplyr::filter(!is.na(!!rlang::sym(x_var)) & !is.na(!!rlang::sym(y_var)))

      if (is.null(grouping)) {
        n_colors <- 1
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )

        if (weighted) {
          g <- ggplot2::ggplot(
            df,
            ggplot2::aes(
              x = !!rlang::sym(x_var),
              y = !!rlang::sym(y_var),
              size = !!rlang::sym(weights_col)
            )
          ) +
            ggplot2::geom_point(color = colors[1], alpha = point_alpha) +
            ggplot2::scale_size_continuous(
              name = ensure_value(legend_label, weights_col)
            )
        } else {
          g <- ggplot2::ggplot(
            df,
            ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))
          ) +
            ggplot2::geom_point(color = colors[1], alpha = point_alpha)
        }
      } else {
        n_colors <- length(unique(df[[grouping]]))
        colors <- phrutils::get_color_palette(
          type = color_palette,
          n = n_colors
        )

        if (weighted) {
          g <- ggplot2::ggplot(
            df,
            ggplot2::aes(
              x = !!rlang::sym(x_var),
              y = !!rlang::sym(y_var),
              color = as.factor(!!rlang::sym(grouping)),
              size = !!rlang::sym(weights_col)
            )
          ) +
            ggplot2::geom_point(alpha = point_alpha) +
            ggplot2::scale_color_manual(
              values = colors,
              name = ensure_value(legend_label, grouping)
            ) +
            ggplot2::scale_size_continuous(name = weights_col)
        } else {
          g <- ggplot2::ggplot(
            df,
            ggplot2::aes(
              x = !!rlang::sym(x_var),
              y = !!rlang::sym(y_var),
              color = as.factor(!!rlang::sym(grouping))
            )
          ) +
            ggplot2::geom_point(alpha = point_alpha) +
            ggplot2::scale_color_manual(
              values = colors,
              name = ensure_value(legend_label, grouping)
            )
        }
      }

      if (add_smooth) {
        if (is.null(grouping)) {
          g <- g +
            ggplot2::geom_smooth(
              method = smooth_method,
              se = TRUE,
              color = "darkred",
              linewidth = 0.8
            )
        } else {
          g <- g +
            ggplot2::geom_smooth(
              method = smooth_method,
              se = FALSE,
              linewidth = 0.8
            )
        }
      }

      g <- g +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = legend_position) +
        ggplot2::labs(
          x = ensure_value(x_lab, x_var),
          y = ensure_value(y_lab, y_var),
          subtitle = final_subtitle
        )

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        auto_title <- variable_label
        if (!is.null(grouping)) {
          group_lbl <- grouping_label %||% grouping
          auto_title <- paste0(auto_title, phr_txt(", by"), " ", group_lbl)
        }
        g <- g + ggplot2::ggtitle(auto_title)
      }

      # Apply coordinate flip if requested
      if (flip_coordinates) {
        g <- g + ggplot2::coord_flip()
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

#' Plot Donut Chart
#'
#' Creates a donut chart showing categorical proportions
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param category_var Character, column containing categories (required)
#' @param value_var Character, optional column with numeric values to sum (default: NULL)
#'   When provided, the donut shows sum of values by category instead of count of observations
#' @param weighted Logical, whether to use weighted calculations (default: FALSE)
#' @param weights_col Character, name of the weights column (required if weighted = TRUE)
#' @param show_labels Logical, show value labels on segments (default: TRUE)
#' @param label_type Character, type of labels: "percentage", "count", "both" (default: "percentage")
#'   Note: "count" shows sum when value_var is used, otherwise shows observation count
#' @param label_color Character, color of text labels (default: "white")
#' @param hole_size Numeric, size of center hole (0-1, default: 0.4)
#' @param color_palette Character, color palette type (default: "reach1")
#' @param title_name Character, plot title (default: NULL)
#' @param variable_label Optional character string. Human-readable label for the category variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param subtitle Character, additional subtitle text (default: NULL)
#' @param legend_label Character, legend title (default: NULL)
#' @param legend_position Character, position of legend (default: "right")
#'
#' @return A ggplot2 donut chart
#' @export
plot_donut <- function(
  survey_design,
  category_var,
  value_var = NULL,
  weighted = FALSE,
  weights_col = NULL,
  show_labels = TRUE,
  label_type = "percentage",
  label_color = "white",
  hole_size = 0.4,
  color_palette = "reach1",
  title_name = NULL,
  variable_label = NULL,
  subtitle = NULL,
  legend_label = NULL,
  legend_position = "right"
) {
  origin <- "plot_donut"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Apply ensure_value with non-NULL defaults
      color_palette <- ensure_value(color_palette, "reach1")
      legend_position <- ensure_value(legend_position, "right")
      weighted <- ensure_value(weighted, FALSE)
      show_labels <- ensure_value(show_labels, TRUE)
      label_type <- ensure_value(label_type, "percentage")
      label_color <- ensure_value(label_color, "white")
      hole_size <- ensure_value(hole_size, 0.4)

      # For optional text fields
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      legend_label <- ensure_value(legend_label, "")

      # For optional column names
      weights_col <- ensure_value(weights_col, NA_character_)
      value_var <- ensure_value(value_var, NA_character_)

      # Standard validation block
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(show_labels, origin = origin, soft = FALSE)
      phrutils::phr_validate_character(
        legend_position,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        label_color,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        hole_size,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        label_type,
        choices = c("percentage", "count", "both"),
        origin = origin,
        soft = FALSE
      )

      # Convert back to NULL for logical checks
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (is.na(value_var)) {
        value_var <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (legend_label == "") {
        legend_label <- NULL
      }

      # Validate category_var
      phrutils::phr_validate_columns(
        df,
        category_var,
        origin = origin,
        hint = phr_txt(paste0(
          "Category column '",
          category_var,
          "' must exist"
        )),
        soft = FALSE
      )

      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist in the dataset"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      if (!is.null(value_var)) {
        phrutils::phr_validate_columns(
          df,
          value_var,
          origin = origin,
          hint = phr_txt(paste0("Value column '", value_var, "' must exist")),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[value_var]],
          origin = origin,
          soft = FALSE
        )
      }

      # Auto-subtitle with n
      total_n <- nrow(df)
      total_weighted_n <- tryCatch(
        sum(survey_design$prob^(-1), na.rm = TRUE),
        error = function(e) NA_real_
      )
      if (!is.na(total_weighted_n) && total_weighted_n != total_n) {
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          total_weighted_n
        )
      } else {
        auto_subtitle <- sprintf("n = %d", total_n)
      }
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Calculate proportions using phr_calc_survey_categorical_single for
      # consistency with run_analysis outputs (unless value_var is specified,
      # which aggregates a numeric value rather than counting observations).
      category_sym <- rlang::sym(category_var)
      ind_label <- variable_label %||% category_var
      cat_prefix <- paste0(ind_label, " - ")

      has_value_var <- !is.null(value_var)

      if (!has_value_var) {
        # Use phr_calc for consistent survey-weighted proportions
        calc_df <- phr_calc_survey_categorical_single(
          design = survey_design,
          var_name = category_var,
          indicator_name = ind_label,
          multiplier = 100
        )

        df_plot <- calc_df |>
          dplyr::mutate(
            !!category_sym := factor(
              gsub(cat_prefix, "", .data$indicator_name, fixed = TRUE)
            ),
            n = .data$n_unweighted,
            count = as.integer(.data$n_unweighted),
            pct = .data$point.estimate
          ) |>
          dplyr::filter(!is.na(!!category_sym)) |>
          dplyr::arrange(dplyr::desc(pct)) |>
          dplyr::mutate(
            ymax = cumsum(pct),
            ymin = dplyr::lag(ymax, default = 0),
            label_pos = (ymax + ymin) / 2
          )
      } else {
        # When value_var is provided, fall back to direct aggregation on extracted df
        phrutils::phr_validate_columns(
          df,
          value_var,
          origin = origin,
          hint = phr_txt(paste0("Value column '", value_var, "' must exist")),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[value_var]],
          origin = origin,
          soft = FALSE
        )

        value_sym <- rlang::sym(value_var)
        df_plot <- df |>
          dplyr::filter(!is.na(!!category_sym)) |>
          dplyr::group_by(!!category_sym) |>
          dplyr::summarise(
            n = sum(!!value_sym, na.rm = TRUE),
            count = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(n)) |>
          dplyr::mutate(
            pct = n / sum(n) * 100,
            ymax = cumsum(pct),
            ymin = dplyr::lag(ymax, default = 0),
            label_pos = (ymax + ymin) / 2
          )
      }

      # Create labels based on what we're displaying
      if (has_value_var) {
        # When value_var is used, "count" means the sum of values, not observations
        df_plot <- df_plot |>
          dplyr::mutate(
            label = dplyr::case_when(
              label_type == "percentage" ~ sprintf("%.1f%%", pct),
              label_type == "count" ~ sprintf("%.0f", n), # This is the sum of values
              label_type == "both" ~ sprintf("%.1f%%\n(%.0f)", pct, n),
              TRUE ~ sprintf("%.1f%%", pct)
            )
          )
      } else {
        # Standard: count means number of observations
        df_plot <- df_plot |>
          dplyr::mutate(
            label = dplyr::case_when(
              label_type == "percentage" ~ sprintf("%.1f%%", pct),
              label_type == "count" ~ sprintf("%.0f", n),
              label_type == "both" ~ sprintf("%.1f%%\n(%.0f)", pct, n),
              TRUE ~ sprintf("%.1f%%", pct)
            )
          )
      }

      # Get colors
      n_cats <- nrow(df_plot)
      colors <- phrutils::get_color_palette(type = color_palette, n = n_cats)

      # Set legend label
      final_legend_label <- if (is.null(legend_label)) {
        category_var
      } else {
        legend_label
      }

      # Calculate label x position to center in the band
      label_x_pos <- 4 - hole_size * 2

      g <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          ymax = ymax,
          ymin = ymin,
          xmax = 4,
          xmin = 4 - hole_size * 4,
          fill = !!category_sym
        )
      ) +
        ggplot2::geom_rect() +
        ggplot2::scale_fill_manual(values = colors, name = final_legend_label) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(c(0, 4)) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = legend_position)

      if (show_labels) {
        g <- g +
          ggplot2::geom_text(
            ggplot2::aes(x = label_x_pos, y = label_pos, label = label),
            size = 3,
            color = label_color,
            fontface = "bold"
          )
      }

      g <- g + ggplot2::labs(subtitle = final_subtitle)

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        g <- g +
          ggplot2::ggtitle(paste(phr_txt("Distribution of"), variable_label))
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}

# Internal helper: expand a crosstab data frame to include all factor-level
# combinations, filling missing cells with 0 for the specified fill columns.
.expand_crosstab_factor_levels <- function(
  df_crosstab,
  row_var,
  col_var,
  row_factor_levels,
  col_factor_levels,
  fill_cols
) {
  expand_row <- if (!is.null(row_factor_levels)) {
    row_factor_levels
  } else {
    unique(df_crosstab[[row_var]])
  }
  expand_col <- if (!is.null(col_factor_levels)) {
    col_factor_levels
  } else {
    unique(df_crosstab[[col_var]])
  }
  full_grid <- expand.grid(
    setNames(list(expand_row, expand_col), c(row_var, col_var)),
    stringsAsFactors = FALSE
  )
  df_crosstab[[row_var]] <- as.character(df_crosstab[[row_var]])
  df_crosstab[[col_var]] <- as.character(df_crosstab[[col_var]])
  df_crosstab <- dplyr::left_join(
    full_grid,
    df_crosstab,
    by = c(row_var, col_var)
  )
  for (col in fill_cols) {
    df_crosstab[[col]][is.na(df_crosstab[[col]])] <- 0
  }
  df_crosstab
}

#' Plot Crosstab Heatmap
#'
#' Creates a heatmap visualization of a contingency table (crosstab) between two categorical variables.
#' Cells show percentages with counts, and are shaded on a gradient from lowest to highest values.
#' Supports survey weights, marginal totals, and optional cell highlighting for significant findings.
#'
#' @param survey_design A srvyr survey design object (e.g., created with \code{srvyr::as_survey_design()})
#' @param row_var Inputs a character value specifying the column name for the row variable.
#' @param col_var Inputs a character value specifying the column name for the column variable.
#' @param weighted Logical indicating whether to apply survey weights. Default: FALSE.
#' @param weights_col Character value specifying the column name containing survey weights.
#'   Required if weighted = TRUE. Default: NULL.
#' @param percentage_by Character specifying how to calculate percentages. Options:
#'   "total" (cell % of grand total), "row" (cell % of row total), "column" (cell % of column total).
#'   Default: "total".
#' @param gradient_by Character specifying how to apply color gradient. Options:
#'   "all" (gradient across all cells), "row" (gradient within each row), "column" (gradient within each column).
#'   Default: "all".
#' @param show_margins Logical indicating whether to add marginal totals. Default: FALSE.
#' @param margins_label Character specifying label for margins row/column. Default: "Total".
#' @param color_low Character specifying color for lowest values. Default: "#FFFFFF" (white).
#' @param color_high Character specifying color for highest values. Default: "#0067A0" (REACH blue).
#' @param color_mid Character specifying optional midpoint color for diverging scales. Default: NULL.
#' @param show_counts Logical indicating whether to show counts in cells. Default: TRUE.
#' @param show_percentages Logical indicating whether to show percentages in cells. Default: TRUE.
#' @param highlight_cells_row_val1 Optional character value specifying the row value of the first cell to highlight. Default: NULL.
#' @param highlight_cells_col_val1 Optional character value specifying the column value of the first cell to highlight. Default: NULL.
#' @param highlight_cells_row_val2 Optional character value specifying the row value of a second cell to highlight. Default: NULL.
#' @param highlight_cells_col_val2 Optional character value specifying the column value of a second cell to highlight. Default: NULL.
#' @param highlight_color Character specifying default color for cell highlighting. Default: "red".
#' @param highlight_size Numeric specifying default line width for cell highlighting. Default: 2.
#' @param title_name Inputs an optional character value for the title of the plot.
#' @param variable_label Optional character string. Human-readable label for the row variable, used to
#'   auto-generate the plot title when title_name is NULL. Default: NULL.
#' @param variable_label2 Optional character string. Human-readable label for the column variable, used
#'   alongside variable_label to auto-generate the title as "variable_label vs. variable_label2".
#'   Defaults to the col_var column name when NULL. Default: NULL.
#' @param subtitle Inputs an optional character value for the subtitle of the plot.
#'   If NULL, automatically displays n (number of records). Custom subtitle will be appended to n display.
#' @param x_label Inputs an optional character value for the x-axis label.
#' @param y_label Inputs an optional character value for the y-axis label.
#' @param legend_label Inputs an optional character value for the legend title. Default: "Percentage".
#' @param text_size Numeric specifying the size of text in cells. Default: 3.5.
#' @param text_color Character specifying the color of text in cells. Default: "black".
#'
#' @return Returns a ggplot2 object showing the crosstab heatmap.
#' @export

plot_crosstab <- function(
  survey_design,
  row_var,
  col_var,
  weighted = FALSE,
  weights_col = NULL,
  percentage_by = "total",
  gradient_by = "all",
  show_margins = FALSE,
  margins_label = "Total",
  color_low = "#FFFFFF",
  color_high = "#0067A0",
  color_mid = NULL,
  show_counts = TRUE,
  show_percentages = TRUE,
  highlight_cells_row_val1 = NULL,
  highlight_cells_col_val1 = NULL,
  highlight_cells_row_val2 = NULL,
  highlight_cells_col_val2 = NULL,
  highlight_color = "red",
  highlight_size = 2,
  title_name = NULL,
  variable_label = NULL,
  variable_label2 = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  legend_label = "Percentage",
  text_size = 3.5,
  text_color = "black"
) {
  origin <- "plot_crosstab"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)
      # Apply ensure_value with non-NULL defaults
      weighted <- ensure_value(weighted, FALSE)
      percentage_by <- ensure_value(percentage_by, "total")
      gradient_by <- ensure_value(gradient_by, "all")
      show_margins <- ensure_value(show_margins, FALSE)
      margins_label <- ensure_value(margins_label, "Total")
      color_low <- ensure_value(color_low, "#FFFFFF")
      color_high <- ensure_value(color_high, "#0067A0")
      show_counts <- ensure_value(show_counts, TRUE)
      show_percentages <- ensure_value(show_percentages, TRUE)
      highlight_color <- ensure_value(highlight_color, "red")
      highlight_size <- ensure_value(highlight_size, 2)
      legend_label <- ensure_value(legend_label, "Percentage")
      text_size <- ensure_value(text_size, 3.5)
      text_color <- ensure_value(text_color, "black")

      # For optional text fields
      title_name <- ensure_value(title_name, "")
      subtitle <- ensure_value(subtitle, "")
      x_label <- ensure_value(x_label, "")
      y_label <- ensure_value(y_label, "")

      # For optional column names
      weights_col <- ensure_value(weights_col, NA_character_)
      color_mid <- ensure_value(color_mid, NA_character_)
      highlight_cells_row_val1 <- ensure_value(highlight_cells_row_val1, NULL)
      highlight_cells_col_val1 <- ensure_value(highlight_cells_col_val1, NULL)
      highlight_cells_row_val2 <- ensure_value(highlight_cells_row_val2, NULL)
      highlight_cells_col_val2 <- ensure_value(highlight_cells_col_val2, NULL)

      # Validate inputs
      phrutils::phr_validate_not_null(row_var, origin = origin, soft = FALSE)
      phrutils::phr_validate_not_null(col_var, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(weighted, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(show_counts, origin = origin, soft = FALSE)
      phrutils::phr_validate_logical(
        show_percentages,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_logical(
        show_margins,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        percentage_by,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        gradient_by,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_character(
        margins_label,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        text_size,
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_all_numeric(
        highlight_size,
        origin = origin,
        soft = FALSE
      )

      # Convert back to NULL
      if (is.na(weights_col)) {
        weights_col <- NULL
      }
      if (is.na(color_mid)) {
        color_mid <- NULL
      }
      if (title_name == "") {
        title_name <- NULL
      }
      if (subtitle == "") {
        subtitle <- NULL
      }
      if (x_label == "") {
        x_label <- NULL
      }
      if (y_label == "") {
        y_label <- NULL
      }

      # Validate percentage_by
      if (!percentage_by %in% c("total", "row", "column")) {
        phr_error(
          phr_txt("percentage_by must be 'total', 'row', or 'column'"),
          origin = origin
        )
      }

      # Validate gradient_by
      if (!gradient_by %in% c("all", "row", "column")) {
        phr_error(
          phr_txt("gradient_by must be 'all', 'row', or 'column'"),
          origin = origin
        )
      }

      # Validate columns exist
      phrutils::phr_validate_columns(
        df,
        row_var,
        origin = origin,
        hint = phr_txt(paste0("Row variable '", row_var, "' must exist")),
        soft = FALSE
      )
      phrutils::phr_validate_columns(
        df,
        col_var,
        origin = origin,
        hint = phr_txt(paste0("Column variable '", col_var, "' must exist")),
        soft = FALSE
      )

      # Validate weights if weighted = TRUE
      if (weighted) {
        phrutils::phr_validate_not_null(
          weights_col,
          origin = origin,
          soft = FALSE
        )
        phrutils::phr_validate_columns(
          df,
          weights_col,
          origin = origin,
          hint = phr_txt(paste0(
            "Weights column '",
            weights_col,
            "' must exist"
          )),
          soft = FALSE
        )
        phrutils::phr_validate_all_numeric(
          df[[weights_col]],
          origin = origin,
          soft = FALSE
        )
        df <- df |>
          dplyr::mutate(
            !!rlang::sym(weights_col) := as.numeric(!!rlang::sym(weights_col))
          )
      }

      # Create crosstab data
      row_sym <- rlang::sym(row_var)
      col_sym <- rlang::sym(col_var)

      # Capture original factor levels before any filtering
      row_factor_levels <- if (is.factor(df[[row_var]])) {
        levels(df[[row_var]])
      } else {
        NULL
      }
      col_factor_levels <- if (is.factor(df[[col_var]])) {
        levels(df[[col_var]])
      } else {
        NULL
      }

      if (weighted) {
        weights_sym <- rlang::sym(weights_col)

        # Calculate weighted crosstab
        df_crosstab <- df |>
          dplyr::filter(
            !is.na(!!row_sym) & !is.na(!!col_sym) & !is.na(!!weights_sym)
          ) |>
          dplyr::group_by(!!row_sym, !!col_sym) |>
          dplyr::summarise(
            weighted_n = sum(!!weights_sym),
            n = dplyr::n(),
            .groups = "drop"
          )

        # Expand to all factor level combinations, filling missing cells with 0
        if (!is.null(row_factor_levels) || !is.null(col_factor_levels)) {
          df_crosstab <- .expand_crosstab_factor_levels(
            df_crosstab,
            row_var,
            col_var,
            row_factor_levels,
            col_factor_levels,
            fill_cols = c("n", "weighted_n")
          )
        }

        # Add marginal totals if requested
        if (show_margins) {
          # Row margins
          row_margins <- df_crosstab |>
            dplyr::group_by(!!row_sym) |>
            dplyr::summarise(
              weighted_n = sum(weighted_n),
              n = sum(n),
              .groups = "drop"
            ) |>
            dplyr::mutate(!!col_sym := margins_label)

          # Column margins
          col_margins <- df_crosstab |>
            dplyr::group_by(!!col_sym) |>
            dplyr::summarise(
              weighted_n = sum(weighted_n),
              n = sum(n),
              .groups = "drop"
            ) |>
            dplyr::mutate(!!row_sym := margins_label)

          # Grand total
          grand_total <- data.frame(
            weighted_n = sum(df_crosstab$weighted_n),
            n = sum(df_crosstab$n)
          )
          grand_total[[row_var]] <- margins_label
          grand_total[[col_var]] <- margins_label

          # Combine
          df_crosstab <- dplyr::bind_rows(
            df_crosstab,
            row_margins,
            col_margins,
            grand_total
          )
        }

        # Calculate percentages based on percentage_by
        if (percentage_by == "total") {
          total_n <- sum(df_crosstab$weighted_n[
            df_crosstab[[row_var]] != margins_label &
              df_crosstab[[col_var]] != margins_label
          ])
          df_crosstab <- df_crosstab |>
            dplyr::mutate(percentage = weighted_n / total_n * 100)
        } else if (percentage_by == "row") {
          df_crosstab <- df_crosstab |>
            dplyr::group_by(!!row_sym) |>
            dplyr::mutate(
              percentage = weighted_n /
                sum(weighted_n[!!col_sym != margins_label]) *
                100
            ) |>
            dplyr::ungroup()
        } else if (percentage_by == "column") {
          df_crosstab <- df_crosstab |>
            dplyr::group_by(!!col_sym) |>
            dplyr::mutate(
              percentage = weighted_n /
                sum(weighted_n[!!row_sym != margins_label]) *
                100
            ) |>
            dplyr::ungroup()
        }

        total_n <- sum(df_crosstab$n[
          df_crosstab[[row_var]] != margins_label &
            df_crosstab[[col_var]] != margins_label
        ])
        total_weighted_n <- sum(df_crosstab$weighted_n[
          df_crosstab[[row_var]] != margins_label &
            df_crosstab[[col_var]] != margins_label
        ])
        auto_subtitle <- sprintf(
          "n = %d (weighted n = %.0f)",
          total_n,
          total_weighted_n
        )
      } else {
        # Calculate unweighted crosstab
        df_crosstab <- df |>
          dplyr::filter(!is.na(!!row_sym) & !is.na(!!col_sym)) |>
          dplyr::group_by(!!row_sym, !!col_sym) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop")

        # Expand to all factor level combinations, filling missing cells with 0
        if (!is.null(row_factor_levels) || !is.null(col_factor_levels)) {
          df_crosstab <- .expand_crosstab_factor_levels(
            df_crosstab,
            row_var,
            col_var,
            row_factor_levels,
            col_factor_levels,
            fill_cols = "n"
          )
        }

        # Add marginal totals if requested
        if (show_margins) {
          # Row margins
          row_margins <- df_crosstab |>
            dplyr::group_by(!!row_sym) |>
            dplyr::summarise(n = sum(n), .groups = "drop") |>
            dplyr::mutate(!!col_sym := margins_label)

          # Column margins
          col_margins <- df_crosstab |>
            dplyr::group_by(!!col_sym) |>
            dplyr::summarise(n = sum(n), .groups = "drop") |>
            dplyr::mutate(!!row_sym := margins_label)

          # Grand total
          grand_total <- data.frame(n = sum(df_crosstab$n))
          grand_total[[row_var]] <- margins_label
          grand_total[[col_var]] <- margins_label

          # Combine
          df_crosstab <- dplyr::bind_rows(
            df_crosstab,
            row_margins,
            col_margins,
            grand_total
          )
        }

        # Calculate percentages based on percentage_by
        if (percentage_by == "total") {
          total_n <- sum(df_crosstab$n[
            df_crosstab[[row_var]] != margins_label &
              df_crosstab[[col_var]] != margins_label
          ])
          df_crosstab <- df_crosstab |>
            dplyr::mutate(percentage = .data$n / total_n * 100)
        } else if (percentage_by == "row") {
          df_crosstab <- df_crosstab |>
            dplyr::group_by(!!row_sym) |>
            dplyr::mutate(
              percentage = .data$n /
                sum(.data$n[!!col_sym != margins_label]) *
                100
            ) |>
            dplyr::ungroup()
        } else if (percentage_by == "column") {
          df_crosstab <- df_crosstab |>
            dplyr::group_by(!!col_sym) |>
            dplyr::mutate(
              percentage = .data$n /
                sum(.data$n[!!row_sym != margins_label]) *
                100
            ) |>
            dplyr::ungroup()
        }

        total_n <- sum(df_crosstab$n[
          df_crosstab[[row_var]] != margins_label &
            df_crosstab[[col_var]] != margins_label
        ])
        auto_subtitle <- sprintf("n = %d", total_n)
      }

      # Create gradient values based on gradient_by (excluding margins)
      df_crosstab_no_margins <- df_crosstab |>
        dplyr::filter(!!row_sym != margins_label, !!col_sym != margins_label)

      if (gradient_by == "all") {
        df_crosstab_no_margins <- df_crosstab_no_margins |>
          dplyr::mutate(gradient_value = percentage)
      } else if (gradient_by == "row") {
        df_crosstab_no_margins <- df_crosstab_no_margins |>
          dplyr::group_by(!!row_sym) |>
          dplyr::mutate(
            gradient_value = (percentage - min(percentage)) /
              (max(percentage) - min(percentage) + 0.001) *
              100
          ) |>
          dplyr::ungroup()
      } else if (gradient_by == "column") {
        df_crosstab_no_margins <- df_crosstab_no_margins |>
          dplyr::group_by(!!col_sym) |>
          dplyr::mutate(
            gradient_value = (percentage - min(percentage)) /
              (max(percentage) - min(percentage) + 0.001) *
              100
          ) |>
          dplyr::ungroup()
      }

      # Merge gradient values back and set margins to NA
      df_crosstab <- df_crosstab |>
        dplyr::left_join(
          df_crosstab_no_margins |>
            dplyr::select(!!row_sym, !!col_sym, gradient_value),
          by = c(row_var, col_var)
        )

      # Create cell labels
      if (show_percentages && show_counts) {
        df_crosstab <- df_crosstab |>
          dplyr::mutate(label = sprintf("%.1f%%\n(n=%d)", percentage, n))
      } else if (show_percentages) {
        df_crosstab <- df_crosstab |>
          dplyr::mutate(label = sprintf("%.1f%%", percentage))
      } else if (show_counts) {
        df_crosstab <- df_crosstab |>
          dplyr::mutate(label = sprintf("n=%d", n))
      } else {
        df_crosstab <- df_crosstab |>
          dplyr::mutate(label = "")
      }

      # Prepare subtitle
      final_subtitle <- if (!is.null(subtitle)) {
        paste0(auto_subtitle, "; ", subtitle)
      } else {
        auto_subtitle
      }

      # Prepare axis labels
      final_x_label <- if (!is.null(x_label)) x_label else col_var
      final_y_label <- if (!is.null(y_label)) y_label else row_var

      # Order factors to put margins at the end; respect original factor levels if applicable
      if (show_margins) {
        base_row_levels <- if (!is.null(row_factor_levels)) {
          row_factor_levels
        } else {
          setdiff(unique(df_crosstab[[row_var]]), margins_label)
        }
        base_col_levels <- if (!is.null(col_factor_levels)) {
          col_factor_levels
        } else {
          setdiff(unique(df_crosstab[[col_var]]), margins_label)
        }
        row_levels <- c(base_row_levels, margins_label)
        col_levels <- c(base_col_levels, margins_label)
        df_crosstab[[row_var]] <- factor(
          df_crosstab[[row_var]],
          levels = row_levels
        )
        df_crosstab[[col_var]] <- factor(
          df_crosstab[[col_var]],
          levels = col_levels
        )
      } else if (!is.null(row_factor_levels) || !is.null(col_factor_levels)) {
        if (!is.null(row_factor_levels)) {
          df_crosstab[[row_var]] <- factor(
            df_crosstab[[row_var]],
            levels = row_factor_levels
          )
        }
        if (!is.null(col_factor_levels)) {
          df_crosstab[[col_var]] <- factor(
            df_crosstab[[col_var]],
            levels = col_factor_levels
          )
        }
      }

      # Create base plot
      g <- ggplot2::ggplot(
        df_crosstab,
        ggplot2::aes(x = !!col_sym, y = !!row_sym)
      ) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = gradient_value),
          color = "grey70",
          size = 0.5
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = label),
          size = text_size,
          color = text_color
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          x = final_x_label,
          y = final_y_label,
          fill = legend_label,
          subtitle = final_subtitle
        )

      # Add color gradient
      if (!is.null(color_mid)) {
        g <- g +
          ggplot2::scale_fill_gradient2(
            low = color_low,
            mid = color_mid,
            high = color_high,
            midpoint = 50,
            na.value = "grey90",
            labels = function(x) paste0(round(x, 1), "%")
          )
      } else {
        g <- g +
          ggplot2::scale_fill_gradient(
            low = color_low,
            high = color_high,
            na.value = "grey90",
            labels = function(x) paste0(round(x, 1), "%")
          )
      }

      # Build internal highlight_cells list from individual cell parameters
      highlight_cells <- NULL
      if (
        !is.null(highlight_cells_row_val1) && !is.null(highlight_cells_col_val1)
      ) {
        highlight_cells <- list(list(
          row = highlight_cells_row_val1,
          col = highlight_cells_col_val1
        ))
        if (
          !is.null(highlight_cells_row_val2) &&
            !is.null(highlight_cells_col_val2)
        ) {
          highlight_cells[[2]] <- list(
            row = highlight_cells_row_val2,
            col = highlight_cells_col_val2
          )
        }
      }

      # Add cell highlighting with smart boundary merging
      if (!is.null(highlight_cells)) {
        # Group highlights by color
        highlights_by_color <- list()

        for (cell in highlight_cells) {
          # Validate cell specification
          if (!"row" %in% names(cell) || !"col" %in% names(cell)) {
            phrutils::phr_warning(
              phr_txt(
                "highlight_cells must include 'row' and 'col' values. Skipping invalid cell."
              ),
              origin = origin
            )
            next
          }

          # Get highlight parameters
          cell_color <- if ("color" %in% names(cell)) {
            cell$color
          } else {
            highlight_color
          }
          cell_size <- if ("size" %in% names(cell)) {
            cell$size
          } else {
            highlight_size
          }

          # Group by color and size
          group_key <- paste0(cell_color, "_", cell_size)
          if (!group_key %in% names(highlights_by_color)) {
            highlights_by_color[[group_key]] <- list(
              color = cell_color,
              size = cell_size,
              cells = list()
            )
          }

          highlights_by_color[[group_key]]$cells[[
            length(highlights_by_color[[group_key]]$cells) + 1
          ]] <-
            list(row = cell$row, col = cell$col)
        }

        # For each color group, find connected regions and draw boundaries
        for (group_key in names(highlights_by_color)) {
          group_info <- highlights_by_color[[group_key]]
          cells_list <- group_info$cells
          cell_color <- group_info$color
          cell_size <- group_info$size

          # Get all unique row and column values from the crosstab data
          if (show_margins) {
            all_rows <- levels(df_crosstab[[row_var]])
            all_cols <- levels(df_crosstab[[col_var]])
          } else {
            all_rows <- as.character(unique(df_crosstab[[row_var]]))
            all_cols <- as.character(unique(df_crosstab[[col_var]]))
          }

          # Validate that highlighted cells exist in the data
          valid_cells <- list()
          for (cell in cells_list) {
            cell_row <- as.character(cell$row)
            cell_col <- as.character(cell$col)

            # Silently skip cells where row or col is an unresolved schema reference
            # (e.g. @value_map$... or @variable_map$... that had no matching key)
            if (startsWith(cell_row, "@") || startsWith(cell_col, "@")) {
              next
            }

            if (cell_row %in% all_rows && cell_col %in% all_cols) {
              valid_cells[[length(valid_cells) + 1]] <- list(
                row = cell_row,
                col = cell_col
              )
            } else {
              phrutils::phr_warning(
                phr_txt(paste0(
                  "highlight_cells references non-existent cell: row='",
                  cell_row,
                  "', col='",
                  cell_col,
                  "'. Skipping."
                )),
                origin = origin
              )
            }
          }

          # Skip if no valid cells
          if (length(valid_cells) == 0) {
            next
          }

          # Create matrix of highlighted cells
          highlight_matrix <- matrix(
            FALSE,
            nrow = length(all_rows),
            ncol = length(all_cols)
          )
          rownames(highlight_matrix) <- all_rows
          colnames(highlight_matrix) <- all_cols

          for (cell in valid_cells) {
            highlight_matrix[cell$row, cell$col] <- TRUE
          }

          # Find connected components (regions of adjacent cells)
          visited <- matrix(
            FALSE,
            nrow = nrow(highlight_matrix),
            ncol = ncol(highlight_matrix)
          )
          regions <- list()

          # Helper function to flood fill and find connected region
          find_region <- function(r, c) {
            if (
              r < 1 ||
                r > nrow(highlight_matrix) ||
                c < 1 ||
                c > ncol(highlight_matrix) ||
                visited[r, c] ||
                !highlight_matrix[r, c]
            ) {
              return(list())
            }

            visited[r, c] <<- TRUE
            cells <- list(list(row = r, col = c))

            # Check 4 adjacent cells (up, down, left, right)
            cells <- c(cells, find_region(r - 1, c))
            cells <- c(cells, find_region(r + 1, c))
            cells <- c(cells, find_region(r, c - 1))
            cells <- c(cells, find_region(r, c + 1))

            return(cells)
          }

          # Find all connected regions
          for (r in 1:nrow(highlight_matrix)) {
            for (c in 1:ncol(highlight_matrix)) {
              if (highlight_matrix[r, c] && !visited[r, c]) {
                region <- find_region(r, c)
                if (length(region) > 0) {
                  regions[[length(regions) + 1]] <- region
                }
              }
            }
          }

          # Draw boundary for each region
          for (region in regions) {
            # Convert indices to actual values
            region_cells <- do.call(
              rbind,
              lapply(region, function(cell) {
                data.frame(
                  row_val = all_rows[cell$row],
                  col_val = all_cols[cell$col],
                  stringsAsFactors = FALSE
                )
              })
            )

            # Ensure proper factor levels if using factors
            if (show_margins) {
              region_cells$row_val <- factor(
                region_cells$row_val,
                levels = all_rows
              )
              region_cells$col_val <- factor(
                region_cells$col_val,
                levels = all_cols
              )
            }

            # Draw rectangle for each cell in region
            g <- g +
              ggplot2::geom_tile(
                data = region_cells,
                ggplot2::aes(x = .data$col_val, y = .data$row_val),
                fill = NA,
                color = cell_color,
                size = cell_size,
                inherit.aes = FALSE
              )
          }
        }
      }

      # Add title if provided
      if (!is.null(title_name)) {
        g <- g + ggplot2::ggtitle(title_name)
      } else if (!is.null(variable_label)) {
        row_lbl <- variable_label
        col_lbl <- variable_label2 %||% col_var
        auto_title <- paste0(row_lbl, " vs. ", col_lbl)
        g <- g + ggplot2::ggtitle(auto_title)
      }

      return(g)
    },
    on_error = "warn",
    origin = origin
  )
}


table_frequency <- function(
  survey_design,
  variable,
  stat_type = "percentage",
  weighted_result = TRUE,
  weights_col = NULL,
  disaggregation = NULL,
  disaggregation_wide = FALSE,
  show_overall = FALSE,
  ratio_denominator = NULL,
  show_ci = FALSE,
  title_name = NULL,
  variable_label = NULL,
  disaggregation_label = NULL,
  show_n = TRUE,
  show_unit = TRUE,
  digits = 1,
  table_width = NULL,
  table_height = NULL,
  color_palette = "reach1"
) {
  origin <- "table_frequency"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)

      is_srvyr <- inherits(survey_design, "tbl_svy")

      # Validate inputs
      valid_stat_types <- c("percentage", "mean", "median", "ratio")

      working_df <- df

      phrutils::phr_validate_not_null(variable, origin = origin, soft = FALSE)

      # Handle multiple variables
      n_vars <- length(variable)

      # Expand scalar arguments to match number of variables
      if (length(stat_type) == 1) {
        stat_type <- rep(stat_type, n_vars)
      }

      # Handle disaggregation - keep NULL as NULL, otherwise expand
      if (is.null(disaggregation)) {
        disaggregation <- rep(list(NULL), n_vars)
      } else if (length(disaggregation) == 1 && !is.list(disaggregation)) {
        disaggregation <- rep(list(disaggregation), n_vars)
      } else if (!is.list(disaggregation)) {
        disaggregation <- as.list(disaggregation)
      }

      # Handle ratio_denominator - keep NULL as NULL, otherwise expand
      if (is.null(ratio_denominator)) {
        ratio_denominator <- rep(list(NULL), n_vars)
      } else if (
        length(ratio_denominator) == 1 && !is.list(ratio_denominator)
      ) {
        if (is.na(ratio_denominator)) {
          ratio_denominator <- rep(list(NULL), n_vars)
        } else {
          ratio_denominator <- rep(list(ratio_denominator), n_vars)
        }
      } else if (!is.list(ratio_denominator)) {
        ratio_denominator <- lapply(ratio_denominator, function(x) {
          if (is.na(x)) NULL else x
        })
      }

      # Handle variable_label
      if (is.null(variable_label)) {
        variable_label <- variable
      } else if (length(variable_label) == 1) {
        variable_label <- rep(variable_label, n_vars)
      }

      # Validate lengths match
      if (length(stat_type) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "stat_type must be length 1 or same length as variable"
          )
        )
      }
      if (length(disaggregation) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "disaggregation must be length 1 or same length as variable"
          )
        )
      }
      if (length(ratio_denominator) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "ratio_denominator must be length 1 or same length as variable"
          )
        )
      }
      if (length(variable_label) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "variable_label must be length 1 or same length as variable"
          )
        )
      }

      # Validate each stat_type
      for (st in stat_type) {
        if (!st %in% valid_stat_types) {
          phr_error(
            origin = origin,
            message = phr_txt(glue::glue(
              "stat_type must be one of: {paste(valid_stat_types, collapse=', ')}. Got '{st}'."
            ))
          )
        }
      }

      # Validate show_overall
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )

      # Validate columns exist
      phrutils::phr_validate_columns(
        working_df,
        variable,
        origin = origin,
        soft = FALSE
      )

      for (i in seq_along(variable)) {
        if (!is.null(disaggregation[[i]])) {
          phrutils::phr_validate_columns(
            working_df,
            disaggregation[[i]],
            origin = origin,
            soft = FALSE
          )
        }
        if (stat_type[i] == "ratio" && is.null(ratio_denominator[[i]])) {
          phr_error(
            origin = origin,
            message = phr_txt(glue::glue(
              "ratio_denominator must be specified for variable '{variable[i]}' when stat_type = 'ratio'."
            ))
          )
        }
        if (!is.null(ratio_denominator[[i]])) {
          phrutils::phr_validate_columns(
            working_df,
            ratio_denominator[[i]],
            origin = origin,
            soft = FALSE
          )
        }
      }

      # Calculate overall N for each variable (for appending to labels)
      overall_n_by_var <- sapply(variable, function(var) {
        sum(!is.na(working_df[[var]]))
      })

      # Append N to variable labels
      variable_label_with_n <- sapply(seq_along(variable), function(i) {
        sprintf("%s (N = %d)", variable_label[i], overall_n_by_var[i])
      })

      # Process each variable
      all_results <- list()

      for (i in seq_along(variable)) {
        var <- variable[i]
        st <- stat_type[i]
        disagg <- disaggregation[[i]]
        ratio_denom <- ratio_denominator[[i]]
        var_label <- variable_label_with_n[i]

        # Determine unit label for this variable (if show_unit = TRUE)
        if (show_unit) {
          unit_label <- switch(
            st,
            percentage = "%",
            mean = "Mean",
            median = "Median",
            ratio = "Ratio"
          )
        }

        # Compute statistics for this variable
        if (is_srvyr) {
          # --- Weighted via srvyr design ---
          dsn <- survey_design

          if (!is.null(disagg)) {
            dsn <- dsn |> srvyr::group_by(!!rlang::sym(disagg))
          }

          var_sym <- rlang::sym(var)
          ci_vartype <- if (show_ci) "ci" else NULL

          if (st == "percentage") {
            results_df <- dsn |>
              srvyr::group_by(!!var_sym, .add = TRUE) |>
              srvyr::summarise(
                n = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
                estimate = srvyr::survey_prop(
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            results_df <- results_df |>
              dplyr::filter(!is.na(!!var_sym))

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(
                  Value = as.character(!!var_sym),
                  .after = !!rlang::sym(disagg)
                ) |>
                dplyr::select(-!!var_sym)
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = as.character(!!var_sym), .before = 1) |>
                dplyr::select(-!!var_sym)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_pct = round(estimate * 100, digits),
                  lower_ci = round(estimate_low * 100, digits),
                  upper_ci = round(estimate_upp * 100, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_pct,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_pct,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate * 100, digits))
            }
          } else if (st == "mean") {
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_mean(
                  !!var_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          } else if (st == "median") {
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_median(
                  !!var_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          } else if (st == "ratio") {
            denom_sym <- rlang::sym(ratio_denom)
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_ratio(
                  !!var_sym,
                  !!denom_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          }
        } else if (weighted_result && !is.null(weights_col)) {
          # --- Weighted via weights column ---
          phrutils::phr_validate_columns(
            working_df,
            weights_col,
            origin = origin,
            soft = FALSE
          )
          dsn <- srvyr::as_survey_design(
            working_df,
            weights = !!rlang::sym(weights_col)
          )

          if (!is.null(disagg)) {
            dsn <- dsn |> srvyr::group_by(!!rlang::sym(disagg))
          }

          var_sym <- rlang::sym(var)
          ci_vartype <- if (show_ci) "ci" else NULL

          if (st == "percentage") {
            results_df <- dsn |>
              srvyr::group_by(!!var_sym, .add = TRUE) |>
              srvyr::summarise(
                n = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
                estimate = srvyr::survey_prop(
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            results_df <- results_df |>
              dplyr::filter(!is.na(!!var_sym))

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(
                  Value = as.character(!!var_sym),
                  .after = !!rlang::sym(disagg)
                ) |>
                dplyr::select(-!!var_sym)
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = as.character(!!var_sym), .before = 1) |>
                dplyr::select(-!!var_sym)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_pct = round(estimate * 100, digits),
                  lower_ci = round(estimate_low * 100, digits),
                  upper_ci = round(estimate_upp * 100, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_pct,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_pct,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate * 100, digits))
            }
          } else if (st == "mean") {
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_mean(
                  !!var_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          } else if (st == "median") {
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_median(
                  !!var_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          } else if (st == "ratio") {
            denom_sym <- rlang::sym(ratio_denom)
            results_df <- dsn |>
              srvyr::summarise(
                n = srvyr::survey_total(
                  !is.na(!!var_sym),
                  vartype = NULL,
                  na.rm = TRUE
                ),
                estimate = srvyr::survey_ratio(
                  !!var_sym,
                  !!denom_sym,
                  vartype = ci_vartype,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )

            if (!is.null(disagg)) {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- results_df |>
                dplyr::mutate(Value = "Overall", .before = 1)
            }

            if (show_ci && "estimate_low" %in% names(results_df)) {
              results_df <- results_df |>
                dplyr::mutate(
                  estimate_val = round(estimate, digits),
                  lower_ci = round(estimate_low, digits),
                  upper_ci = round(estimate_upp, digits),
                  estimate = sprintf(
                    "%.*f [%.*f - %.*f]",
                    digits,
                    estimate_val,
                    digits,
                    lower_ci,
                    digits,
                    upper_ci
                  )
                ) |>
                dplyr::select(
                  -estimate_low,
                  -estimate_upp,
                  -estimate_val,
                  -lower_ci,
                  -upper_ci
                )
            } else {
              results_df <- results_df |>
                dplyr::mutate(estimate = round(estimate, digits))
            }
          }
        } else {
          # --- Unweighted ---
          var_sym <- rlang::sym(var)

          if (st == "percentage") {
            if (!is.null(disagg)) {
              results_df <- working_df |>
                dplyr::filter(!is.na(!!var_sym)) |>
                dplyr::group_by(!!rlang::sym(disagg), !!var_sym) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop")

              total_by_group <- results_df |>
                dplyr::group_by(!!rlang::sym(disagg)) |>
                dplyr::summarise(total_n = sum(n), .groups = "drop")

              results_df <- results_df |>
                dplyr::left_join(total_by_group, by = disagg) |>
                dplyr::mutate(estimate = round(n / total_n * 100, digits)) |>
                dplyr::select(-total_n)

              results_df <- results_df |>
                dplyr::mutate(
                  Value = as.character(!!var_sym),
                  .after = !!rlang::sym(disagg)
                ) |>
                dplyr::select(-!!var_sym)
            } else {
              results_df <- working_df |>
                dplyr::filter(!is.na(!!var_sym)) |>
                dplyr::group_by(!!var_sym) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::mutate(estimate = round(n / sum(n) * 100, digits))

              results_df <- results_df |>
                dplyr::mutate(Value = as.character(!!var_sym), .before = 1) |>
                dplyr::select(-!!var_sym)
            }
          } else if (st == "mean") {
            if (!is.null(disagg)) {
              results_df <- working_df |>
                dplyr::group_by(!!rlang::sym(disagg)) |>
                dplyr::summarise(
                  n = sum(!is.na(!!var_sym)),
                  estimate = round(mean(!!var_sym, na.rm = TRUE), digits),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(mean(working_df[[var]], na.rm = TRUE), digits)
              )
            }
          } else if (st == "median") {
            if (!is.null(disagg)) {
              results_df <- working_df |>
                dplyr::group_by(!!rlang::sym(disagg)) |>
                dplyr::summarise(
                  n = sum(!is.na(!!var_sym)),
                  estimate = round(
                    stats::median(!!var_sym, na.rm = TRUE),
                    digits
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(
                  stats::median(working_df[[var]], na.rm = TRUE),
                  digits
                )
              )
            }
          } else if (st == "ratio") {
            denom_sym <- rlang::sym(ratio_denom)
            if (!is.null(disagg)) {
              results_df <- working_df |>
                dplyr::group_by(!!rlang::sym(disagg)) |>
                dplyr::summarise(
                  n = sum(!is.na(!!var_sym)),
                  estimate = round(
                    sum(!!var_sym, na.rm = TRUE) /
                      sum(!!denom_sym, na.rm = TRUE),
                    digits
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall", .after = !!rlang::sym(disagg))
            } else {
              results_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(
                  sum(working_df[[var]], na.rm = TRUE) /
                    sum(working_df[[ratio_denom]], na.rm = TRUE),
                  digits
                )
              )
            }
          }
        }

        # Compute and prepend Overall rows when show_overall = TRUE
        if (show_overall && !is.null(disagg)) {
          overall_df <- NULL

          if (is_srvyr) {
            dsn_overall <- survey_design
            var_sym_o <- rlang::sym(var)
            ci_vartype_o <- if (show_ci) "ci" else NULL

            if (st == "percentage") {
              overall_df <- dsn_overall |>
                srvyr::group_by(!!var_sym_o) |>
                srvyr::summarise(
                  n = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
                  estimate = srvyr::survey_prop(
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::filter(!is.na(!!var_sym_o)) |>
                dplyr::mutate(Value = as.character(!!var_sym_o)) |>
                dplyr::select(-!!var_sym_o)

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_pct = round(estimate * 100, digits),
                    lower_ci = round(estimate_low * 100, digits),
                    upper_ci = round(estimate_upp * 100, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_pct,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_pct,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate * 100, digits))
              }
            } else if (st == "mean") {
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_mean(
                    !!var_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            } else if (st == "median") {
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_median(
                    !!var_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            } else if (st == "ratio") {
              denom_sym_o <- rlang::sym(ratio_denom)
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_ratio(
                    !!var_sym_o,
                    !!denom_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            }
          } else if (weighted_result && !is.null(weights_col)) {
            dsn_overall <- srvyr::as_survey_design(
              working_df,
              weights = !!rlang::sym(weights_col)
            )
            var_sym_o <- rlang::sym(var)
            ci_vartype_o <- if (show_ci) "ci" else NULL

            if (st == "percentage") {
              overall_df <- dsn_overall |>
                srvyr::group_by(!!var_sym_o) |>
                srvyr::summarise(
                  n = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
                  estimate = srvyr::survey_prop(
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::filter(!is.na(!!var_sym_o)) |>
                dplyr::mutate(Value = as.character(!!var_sym_o)) |>
                dplyr::select(-!!var_sym_o)

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_pct = round(estimate * 100, digits),
                    lower_ci = round(estimate_low * 100, digits),
                    upper_ci = round(estimate_upp * 100, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_pct,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_pct,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate * 100, digits))
              }
            } else if (st == "mean") {
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_mean(
                    !!var_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            } else if (st == "median") {
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_median(
                    !!var_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            } else if (st == "ratio") {
              denom_sym_o <- rlang::sym(ratio_denom)
              overall_df <- dsn_overall |>
                srvyr::summarise(
                  n = srvyr::survey_total(
                    !is.na(!!var_sym_o),
                    vartype = NULL,
                    na.rm = TRUE
                  ),
                  estimate = srvyr::survey_ratio(
                    !!var_sym_o,
                    !!denom_sym_o,
                    vartype = ci_vartype_o,
                    na.rm = TRUE
                  ),
                  .groups = "drop"
                ) |>
                dplyr::mutate(Value = "Overall")

              if (show_ci && "estimate_low" %in% names(overall_df)) {
                overall_df <- overall_df |>
                  dplyr::mutate(
                    estimate_val = round(estimate, digits),
                    lower_ci = round(estimate_low, digits),
                    upper_ci = round(estimate_upp, digits),
                    estimate = sprintf(
                      "%.*f [%.*f - %.*f]",
                      digits,
                      estimate_val,
                      digits,
                      lower_ci,
                      digits,
                      upper_ci
                    )
                  ) |>
                  dplyr::select(
                    -estimate_low,
                    -estimate_upp,
                    -estimate_val,
                    -lower_ci,
                    -upper_ci
                  )
              } else {
                overall_df <- overall_df |>
                  dplyr::mutate(estimate = round(estimate, digits))
              }
            }
          } else {
            # Unweighted overall
            var_sym_o <- rlang::sym(var)

            if (st == "percentage") {
              overall_df <- working_df |>
                dplyr::filter(!is.na(!!var_sym_o)) |>
                dplyr::group_by(!!var_sym_o) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::mutate(estimate = round(n / sum(n) * 100, digits)) |>
                dplyr::mutate(Value = as.character(!!var_sym_o)) |>
                dplyr::select(-!!var_sym_o)
            } else if (st == "mean") {
              overall_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(mean(working_df[[var]], na.rm = TRUE), digits)
              )
            } else if (st == "median") {
              overall_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(
                  stats::median(working_df[[var]], na.rm = TRUE),
                  digits
                )
              )
            } else if (st == "ratio") {
              overall_df <- tibble::tibble(
                Value = "Overall",
                n = sum(!is.na(working_df[[var]])),
                estimate = round(
                  sum(working_df[[var]], na.rm = TRUE) /
                    sum(working_df[[ratio_denom]], na.rm = TRUE),
                  digits
                )
              )
            }
          }

          if (!is.null(overall_df)) {
            if ("n" %in% names(overall_df)) {
              overall_df$n <- as.integer(round(overall_df$n))
            }
            # Tag with the disaggregation column value "Overall"
            overall_df <- overall_df |>
              dplyr::mutate(!!rlang::sym(disagg) := "Overall", .before = 1)

            # Reorder to match results_df column order
            common_cols <- intersect(names(results_df), names(overall_df))
            overall_df <- overall_df |>
              dplyr::select(dplyr::all_of(common_cols))
            results_df <- results_df |>
              dplyr::select(dplyr::all_of(common_cols))

            results_df <- dplyr::bind_rows(results_df, overall_df)
          }
        }

        if ("n" %in% names(results_df)) {
          results_df$n <- as.integer(round(results_df$n))
        }

        # Add Unit column after n (if show_unit = TRUE)
        if (show_unit) {
          results_df <- results_df |>
            dplyr::mutate(Unit = unit_label, .after = "n")
        }

        if (n_vars > 1) {
          if (!is.null(disagg) && disagg %in% names(results_df)) {
            results_df <- results_df |>
              dplyr::mutate(Variable = var_label, .after = !!rlang::sym(disagg))
          } else {
            results_df <- results_df |>
              dplyr::mutate(Variable = var_label, .before = 1)
          }
        }

        all_results[[i]] <- results_df
      }

      # Coerce disaggregation columns, the Value column, and any factor columns to
      # character so dplyr::bind_rows() can combine them without type errors
      # (e.g. "Can't combine `..1$enumerator`" when the same column is character in one
      # result and integer/factor in another).
      disagg_cols <- unique(unlist(disaggregation))
      char_cols <- c(disagg_cols, "Value")
      all_results <- lapply(all_results, function(df) {
        df |>
          dplyr::mutate(
            dplyr::across(dplyr::any_of(char_cols), as.character),
            dplyr::across(where(is.factor), as.character)
          )
      })

      # Combine all results
      results_df <- dplyr::bind_rows(all_results)

      first_disagg <- disaggregation[[1]]

      # Handle wide format for disaggregation
      if (
        disaggregation_wide &&
          !is.null(first_disagg) &&
          first_disagg %in% names(results_df)
      ) {
        # Ensure proper column ordering before pivot.
        # Unit is treated as an id column (shown once after Value) rather than
        # a per-group value column, so it goes before the disaggregation column.
        col_order <- c()
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          col_order <- c(col_order, "Variable")
        }
        if ("Value" %in% names(results_df)) {
          col_order <- c(col_order, "Value")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          col_order <- c(col_order, "Unit")
        }
        col_order <- c(col_order, first_disagg)
        if ("n" %in% names(results_df)) {
          col_order <- c(col_order, "n")
        }
        if ("estimate" %in% names(results_df)) {
          col_order <- c(col_order, "estimate")
        }

        # Reorder columns
        results_df <- results_df |>
          dplyr::select(dplyr::all_of(col_order))

        # ID columns for pivot — Unit is included here so it appears once
        id_cols <- c()
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          id_cols <- c(id_cols, "Variable")
        }
        if ("Value" %in% names(results_df)) {
          id_cols <- c(id_cols, "Value")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          id_cols <- c(id_cols, "Unit")
        }

        # Value columns to spread (n and estimate only — not Unit)
        value_cols <- c()
        if ("n" %in% names(results_df)) {
          value_cols <- c(value_cols, "n")
        }
        if ("estimate" %in% names(results_df)) {
          value_cols <- c(value_cols, "estimate")
        }

        # Pivot to wide format
        results_df <- results_df |>
          tidyr::pivot_wider(
            id_cols = dplyr::all_of(id_cols),
            names_from = dplyr::all_of(first_disagg),
            values_from = dplyr::all_of(value_cols),
            names_sep = "_"
          )

        # pivot_wider groups columns by the values_from names (e.g., all n_* first, then
        # all estimate_*). For the two-level header merge to work, columns belonging to
        # the same disaggregation group must be consecutive. Reorder accordingly so the
        # layout is: id_cols | n_G1, estimate_G1 | n_G2, estimate_G2 | ...
        {
          pivot_id_present <- id_cols[id_cols %in% names(results_df)]
          data_col_names <- setdiff(names(results_df), pivot_id_present)

          # Extract group names (in their current column order) from the data columns
          # Use startsWith to safely extract group names without regex complications
          grp_names_raw <- character(length(data_col_names))
          for (j_col in seq_along(data_col_names)) {
            data_col <- data_col_names[[j_col]]
            for (vc in value_cols) {
              prefix <- paste0(vc, "_")
              if (startsWith(data_col, prefix)) {
                grp_names_raw[[j_col]] <- substr(
                  data_col,
                  nchar(prefix) + 1L,
                  nchar(data_col)
                )
                break
              }
            }
          }
          grp_names <- unique(grp_names_raw[nzchar(grp_names_raw)])

          reordered_cols <- vector(
            "list",
            length(grp_names) * length(value_cols)
          )
          k_col <- 0L
          for (grp in grp_names) {
            for (vc in value_cols) {
              col <- paste0(vc, "_", grp)
              if (col %in% names(results_df)) {
                k_col <- k_col + 1L
                reordered_cols[[k_col]] <- col
              }
            }
          }
          reordered_cols <- unlist(reordered_cols[seq_len(k_col)])

          if (length(reordered_cols) > 0) {
            results_df <- results_df |>
              dplyr::select(dplyr::all_of(c(pivot_id_present, reordered_cols)))
          }
        }
      } else {
        # Long format (original behavior)
        if (
          n_vars > 1 &&
            !is.null(first_disagg) &&
            first_disagg %in% names(results_df)
        ) {
          other_cols <- setdiff(
            names(results_df),
            c(first_disagg, "Variable", "Value")
          )
          results_df <- results_df |>
            dplyr::select(
              !!rlang::sym(first_disagg),
              Variable,
              Value,
              dplyr::all_of(other_cols)
            )
        }
      }

      if (!show_n && "n" %in% names(results_df)) {
        # In wide format, remove all n_ columns
        if (disaggregation_wide) {
          n_cols <- grep("^n_", names(results_df), value = TRUE)
          if (length(n_cols) > 0) {
            results_df <- results_df |> dplyr::select(-dplyr::all_of(n_cols))
          }
        } else {
          results_df <- results_df |> dplyr::select(-n)
        }
      }

      # Remove Unit column if show_unit = FALSE (Unit is now a single column in both formats)
      if (!show_unit && "Unit" %in% names(results_df)) {
        results_df <- results_df |> dplyr::select(-Unit)
      }

      # Build flextable
      ft <- flextable::flextable(as.data.frame(results_df))

      # Handle two-level headers for wide format
      if (disaggregation_wide && !is.null(first_disagg)) {
        # Get unique disaggregation values from column names (n_GROUP and estimate_GROUP)
        disagg_groups <- unique(gsub(
          "^(n|estimate)_(.*)$",
          "\\2",
          grep("^(n|estimate)_", names(results_df), value = TRUE)
        ))

        # Build header mapping data frame
        header_df <- data.frame(
          col_keys = names(results_df),
          line1 = character(length(names(results_df))),
          line2 = character(length(names(results_df))),
          stringsAsFactors = FALSE
        )

        # Handle ID columns (Variable, Value, Unit)
        for (col in names(results_df)) {
          if (col == "Variable" && n_vars > 1) {
            header_df[header_df$col_keys == col, "line1"] <- "Variable"
            header_df[header_df$col_keys == col, "line2"] <- "Variable"
          } else if (col == "Value") {
            val_label <- if (n_vars == 1) variable_label_with_n[1] else "Value"
            header_df[header_df$col_keys == col, "line1"] <- val_label
            header_df[header_df$col_keys == col, "line2"] <- val_label
          } else if (col == "Unit" && show_unit) {
            header_df[header_df$col_keys == col, "line1"] <- "Unit"
            header_df[header_df$col_keys == col, "line2"] <- "Unit"
          }
        }

        # Build headers for each disaggregation group
        for (group in disagg_groups) {
          n_col <- paste0("n_", group)
          est_col <- paste0("estimate_", group)

          if (n_col %in% names(results_df)) {
            header_df[header_df$col_keys == n_col, "line1"] <- group
            header_df[header_df$col_keys == n_col, "line2"] <- "n"
          }
          if (est_col %in% names(results_df)) {
            header_df[header_df$col_keys == est_col, "line1"] <- group
            header_df[header_df$col_keys == est_col, "line2"] <- "Estimate"
          }
        }

        # Apply two-level headers using correct parameter name
        ft <- flextable::set_header_df(
          ft,
          mapping = header_df,
          key = "col_keys"
        )

        # Merge cells in top header row for disaggregation groups
        for (group in disagg_groups) {
          # Use endsWith to safely match group names that may contain regex special characters
          group_cols <- names(results_df)[endsWith(
            names(results_df),
            paste0("_", group)
          )]
          if (length(group_cols) > 1) {
            ft <- flextable::merge_at(
              ft,
              i = 1,
              j = group_cols,
              part = "header"
            )
          }
        }

        # Merge ID column headers across both rows
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          ft <- flextable::merge_at(
            ft,
            i = 1:2,
            j = "Variable",
            part = "header"
          )
        }
        if ("Value" %in% names(results_df)) {
          ft <- flextable::merge_at(ft, i = 1:2, j = "Value", part = "header")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          ft <- flextable::merge_at(ft, i = 1:2, j = "Unit", part = "header")
        }
      } else {
        # Single-level headers (original behavior)
        col_labels <- list()

        if (!is.null(first_disagg) && first_disagg %in% names(results_df)) {
          col_labels[[first_disagg]] <- disaggregation_label %||% first_disagg
        }

        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          col_labels[["Variable"]] <- "Variable"
        }

        if ("Value" %in% names(results_df)) {
          col_labels[["Value"]] <- if (n_vars == 1) {
            variable_label_with_n[1]
          } else {
            "Value"
          }
        }

        if (show_n && "n" %in% names(results_df)) {
          col_labels[["n"]] <- "n"
        }

        if (show_unit && "Unit" %in% names(results_df)) {
          col_labels[["Unit"]] <- "Unit"
        }

        if ("estimate" %in% names(results_df)) {
          col_labels[["estimate"]] <- "Estimate"
        }

        present_labels <- col_labels[names(col_labels) %in% names(results_df)]
        if (length(present_labels) > 0) {
          ft <- flextable::set_header_labels(ft, values = present_labels)
        }
      }

      # Styling — apply standard phr theme
      ft <- apply_phr_flextable_theme(ft, color_palette = color_palette)

      # Right-align numeric columns
      if (disaggregation_wide) {
        right_cols <- grep("^(n|estimate)_", names(results_df), value = TRUE)
      } else {
        right_cols <- intersect(c("estimate", "n"), names(results_df))
      }
      if (length(right_cols) > 0) {
        ft <- flextable::align(
          ft,
          j = right_cols,
          align = "right",
          part = "body"
        )
      }

      # Center-align Unit column (single column in both wide and long formats)
      if (show_unit && "Unit" %in% names(results_df)) {
        ft <- flextable::align(ft, j = "Unit", align = "center", part = "body")
        ft <- flextable::align(
          ft,
          j = "Unit",
          align = "center",
          part = "header"
        )
      }

      # Merge cells for long format multi-variable tables
      if (!disaggregation_wide) {
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          var_values <- results_df[["Variable"]]
          run_starts <- which(!duplicated(var_values))
          run_ends <- c(run_starts[-1] - 1, length(var_values))

          for (k in seq_along(run_starts)) {
            if (run_ends[k] > run_starts[k]) {
              ft <- flextable::merge_at(
                ft,
                i = run_starts[k]:run_ends[k],
                j = "Variable",
                part = "body"
              )
            }
          }
          ft <- flextable::valign(
            ft,
            j = "Variable",
            valign = "top",
            part = "body"
          )
        }

        # Merge disaggregation cells only within Variable groups
        if (!is.null(first_disagg) && first_disagg %in% names(results_df)) {
          if (n_vars > 1 && "Variable" %in% names(results_df)) {
            var_values <- results_df[["Variable"]]
            var_starts <- which(!duplicated(var_values))
            var_ends <- c(var_starts[-1] - 1, nrow(results_df))

            for (v in seq_along(var_starts)) {
              var_row_range <- var_starts[v]:var_ends[v]
              disagg_in_var <- results_df[[first_disagg]][var_row_range]

              disagg_run_starts <- which(!duplicated(disagg_in_var))
              disagg_run_ends <- c(
                disagg_run_starts[-1] - 1,
                length(disagg_in_var)
              )

              for (d in seq_along(disagg_run_starts)) {
                if (disagg_run_ends[d] > disagg_run_starts[d]) {
                  actual_rows <- var_row_range[
                    disagg_run_starts[d]:disagg_run_ends[d]
                  ]
                  ft <- flextable::merge_at(
                    ft,
                    i = actual_rows,
                    j = first_disagg,
                    part = "body"
                  )
                }
              }
            }
          } else {
            disagg_values <- results_df[[first_disagg]]
            run_starts <- which(!duplicated(disagg_values))
            run_ends <- c(run_starts[-1] - 1, length(disagg_values))

            for (k in seq_along(run_starts)) {
              if (run_ends[k] > run_starts[k]) {
                ft <- flextable::merge_at(
                  ft,
                  i = run_starts[k]:run_ends[k],
                  j = first_disagg,
                  part = "body"
                )
              }
            }
          }

          ft <- flextable::valign(
            ft,
            j = first_disagg,
            valign = "top",
            part = "body"
          )
        }
      }

      if (!is.null(title_name)) {
        ft <- flextable::set_caption(ft, caption = title_name)
      } else {
        # Auto-generate caption from variable_label (falls back to column name)
        first_var_label <- variable_label[1]
        auto_caption <- paste(phr_txt("Frequency Table of"), first_var_label)
        # Add disaggregation if any
        first_disagg_col <- disaggregation[[1]]
        if (!is.null(first_disagg_col)) {
          disagg_lbl <- if (!is.null(disaggregation_label)) {
            disaggregation_label
          } else {
            first_disagg_col
          }
          auto_caption <- paste0(auto_caption, phr_txt(", by"), " ", disagg_lbl)
        }
        ft <- flextable::set_caption(ft, caption = auto_caption)
      }

      if (nrow(results_df) > 1) {
        even_rows <- seq(2, nrow(results_df), by = 2)
        ft <- flextable::bg(ft, i = even_rows, bg = "#f5f5f5", part = "body")
      }

      if (!is.null(table_width)) {
        ft <- flextable::width(ft, width = table_width)
      }

      if (!is.null(table_height)) {
        ft <- flextable::height(ft, height = table_height)
      }

      return(ft)
    },
    on_error = "warn",
    origin = origin
  )
}


table_frequency_v2 <- function(
  survey_design,
  variable,
  stat_type = "percentage",
  weighted_result = TRUE,
  weights_col = NULL,
  disaggregation = NULL,
  disaggregation_wide = FALSE,
  show_overall = FALSE,
  ratio_denominator = NULL,
  show_ci = FALSE,
  title_name = NULL,
  variable_label = NULL,
  disaggregation_label = NULL,
  show_n = TRUE,
  show_unit = TRUE,
  digits = 1,
  table_width = NULL,
  table_height = NULL,
  color_palette = "reach1"
) {
  origin <- "table_frequency_v2"

  phrutils::phr_try(
    {
      df <- phr_get_data_from_design(survey_design)

      working_df <- df

      # Validate inputs
      valid_stat_types <- c("percentage", "mean", "median", "ratio")

      phrutils::phr_validate_not_null(variable, origin = origin, soft = FALSE)

      # Handle multiple variables
      n_vars <- length(variable)

      # Expand scalar arguments to match number of variables
      if (length(stat_type) == 1) {
        stat_type <- rep(stat_type, n_vars)
      }

      # Handle disaggregation - keep NULL as NULL, otherwise expand
      if (is.null(disaggregation)) {
        disaggregation <- rep(list(NULL), n_vars)
      } else if (length(disaggregation) == 1 && !is.list(disaggregation)) {
        disaggregation <- rep(list(disaggregation), n_vars)
      } else if (!is.list(disaggregation)) {
        disaggregation <- as.list(disaggregation)
      }

      # Handle ratio_denominator - keep NULL as NULL, otherwise expand
      if (is.null(ratio_denominator)) {
        ratio_denominator <- rep(list(NULL), n_vars)
      } else if (
        length(ratio_denominator) == 1 && !is.list(ratio_denominator)
      ) {
        if (is.na(ratio_denominator)) {
          ratio_denominator <- rep(list(NULL), n_vars)
        } else {
          ratio_denominator <- rep(list(ratio_denominator), n_vars)
        }
      } else if (!is.list(ratio_denominator)) {
        ratio_denominator <- lapply(ratio_denominator, function(x) {
          if (is.na(x)) NULL else x
        })
      }

      # Handle variable_label
      if (is.null(variable_label)) {
        variable_label <- variable
      } else if (length(variable_label) == 1) {
        variable_label <- rep(variable_label, n_vars)
      }

      # Validate lengths match
      if (length(stat_type) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "stat_type must be length 1 or same length as variable"
          )
        )
      }
      if (length(disaggregation) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "disaggregation must be length 1 or same length as variable"
          )
        )
      }
      if (length(ratio_denominator) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "ratio_denominator must be length 1 or same length as variable"
          )
        )
      }
      if (length(variable_label) != n_vars) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "variable_label must be length 1 or same length as variable"
          )
        )
      }

      # Validate each stat_type
      for (st in stat_type) {
        if (!st %in% valid_stat_types) {
          phr_error(
            origin = origin,
            message = phr_txt(glue::glue(
              "stat_type must be one of: {paste(valid_stat_types, collapse=', ')}. Got '{st}'."
            ))
          )
        }
      }

      # Validate show_overall
      phrutils::phr_validate_logical(
        show_overall,
        origin = origin,
        soft = FALSE
      )

      # Validate columns exist
      phrutils::phr_validate_columns(
        working_df,
        variable,
        origin = origin,
        soft = FALSE
      )

      for (i in seq_along(variable)) {
        if (!is.null(disaggregation[[i]])) {
          phrutils::phr_validate_columns(
            working_df,
            disaggregation[[i]],
            origin = origin,
            soft = FALSE
          )
        }
        if (stat_type[i] == "ratio" && is.null(ratio_denominator[[i]])) {
          phr_error(
            origin = origin,
            message = phr_txt(glue::glue(
              "ratio_denominator must be specified for variable '{variable[i]}' when stat_type = 'ratio'."
            ))
          )
        }
        if (!is.null(ratio_denominator[[i]])) {
          phrutils::phr_validate_columns(
            working_df,
            ratio_denominator[[i]],
            origin = origin,
            soft = FALSE
          )
        }
      }

      # Calculate overall N for each variable (for appending to labels)
      overall_n_by_var <- sapply(variable, function(var) {
        sum(!is.na(working_df[[var]]))
      })

      # Append N to variable labels
      variable_label_with_n <- sapply(seq_along(variable), function(i) {
        sprintf("%s (N = %d)", variable_label[i], overall_n_by_var[i])
      })

      # Internal helper: compute stats for one (possibly subsetted) design and
      # return a tidy data frame with columns Value, n, estimate (or estimate with
      # CI string when show_ci = TRUE).
      .calc_one_group <- function(design_sub, var, st, ratio_denom) {
        if (st == "percentage") {
          result <- phr_calc_survey_categorical_single(
            design = design_sub,
            var_name = var,
            indicator_name = var,
            indicator_unit = "%",
            multiplier = 100,
            group_name_label = "Overall"
          )
          prefix <- paste0(var, " - ")
          df <- tibble::tibble(
            Value = sub(prefix, "", result$indicator_name, fixed = TRUE),
            n = as.integer(round(result$n_weighted))
          )
          if (show_ci) {
            df$estimate <- sprintf(
              "%.*f [%.*f - %.*f]",
              digits,
              round(result$point.estimate, digits),
              digits,
              round(result$lower_ci, digits),
              digits,
              round(result$upper_ci, digits)
            )
          } else {
            df$estimate <- round(result$point.estimate, digits)
          }
        } else if (st == "mean") {
          result <- phr_calc_survey_mean_single(
            design = design_sub,
            var_name = var,
            indicator_name = var,
            group_name_label = "Overall"
          )
          df <- tibble::tibble(
            Value = "Overall",
            n = as.integer(round(result$n_weighted))
          )
          if (show_ci) {
            df$estimate <- sprintf(
              "%.*f [%.*f - %.*f]",
              digits,
              round(result$point.estimate, digits),
              digits,
              round(result$lower_ci, digits),
              digits,
              round(result$upper_ci, digits)
            )
          } else {
            df$estimate <- round(result$point.estimate, digits)
          }
        } else if (st == "median") {
          result <- phr_calc_survey_median_single(
            design = design_sub,
            var_name = var,
            indicator_name = var,
            group_name_label = "Overall"
          )
          df <- tibble::tibble(
            Value = "Overall",
            n = as.integer(round(result$n_weighted))
          )
          if (show_ci) {
            df$estimate <- sprintf(
              "%.*f [%.*f - %.*f]",
              digits,
              round(result$point.estimate, digits),
              digits,
              round(result$lower_ci, digits),
              digits,
              round(result$upper_ci, digits)
            )
          } else {
            df$estimate <- round(result$point.estimate, digits)
          }
        } else if (st == "ratio") {
          result <- phr_calc_survey_ratio_single(
            design = design_sub,
            numerator_var = var,
            denominator_var = ratio_denom,
            indicator_name = var,
            group_name_label = "Overall"
          )
          df <- tibble::tibble(
            Value = "Overall",
            n = as.integer(round(result$n_weighted))
          )
          if (show_ci) {
            df$estimate <- sprintf(
              "%.*f [%.*f - %.*f]",
              digits,
              round(result$point.estimate, digits),
              digits,
              round(result$lower_ci, digits),
              digits,
              round(result$upper_ci, digits)
            )
          } else {
            df$estimate <- round(result$point.estimate, digits)
          }
        }

        df
      }

      # Process each variable
      all_results <- list()

      for (i in seq_along(variable)) {
        var <- variable[i]
        st <- stat_type[i]
        disagg <- disaggregation[[i]]
        ratio_denom <- ratio_denominator[[i]]
        var_label <- variable_label_with_n[i]

        # Determine unit label for this variable (if show_unit = TRUE)
        if (show_unit) {
          unit_label <- switch(
            st,
            percentage = "%",
            mean = "Mean",
            median = "Median",
            ratio = "Ratio"
          )
        }

        # --- Compute statistics for this variable ---
        if (!is.null(disagg)) {
          group_levels <- unique(na.omit(working_df[[disagg]]))

          per_group <- lapply(group_levels, function(g) {
            design_sub <- tryCatch(
              subset(survey_design, df[[disagg]] == g),
              error = function(e) {
                phrutils::phr_warning(
                  origin,
                  paste("Subset failed for", disagg, "=", g)
                )
                NULL
              }
            )
            if (is.null(design_sub)) {
              return(tibble::tibble())
            }
            row_df <- .calc_one_group(design_sub, var, st, ratio_denom)
            dplyr::mutate(
              row_df,
              !!rlang::sym(disagg) := as.character(g),
              .before = 1
            )
          })

          results_df <- dplyr::bind_rows(per_group)
        } else {
          results_df <- .calc_one_group(survey_design, var, st, ratio_denom)
        }

        # Compute and append Overall rows when show_overall = TRUE
        if (show_overall && !is.null(disagg)) {
          overall_df <- .calc_one_group(survey_design, var, st, ratio_denom)
          overall_df <- dplyr::mutate(
            overall_df,
            !!rlang::sym(disagg) := "Overall",
            .before = 1
          )

          common_cols <- intersect(names(results_df), names(overall_df))
          overall_df <- overall_df |> dplyr::select(dplyr::all_of(common_cols))
          results_df <- results_df |> dplyr::select(dplyr::all_of(common_cols))
          results_df <- dplyr::bind_rows(results_df, overall_df)
        }

        if ("n" %in% names(results_df)) {
          results_df$n <- as.integer(round(results_df$n))
        }

        # Add Unit column after n (if show_unit = TRUE)
        if (show_unit) {
          results_df <- results_df |>
            dplyr::mutate(Unit = unit_label, .after = "n")
        }

        if (n_vars > 1) {
          if (!is.null(disagg) && disagg %in% names(results_df)) {
            results_df <- results_df |>
              dplyr::mutate(Variable = var_label, .after = !!rlang::sym(disagg))
          } else {
            results_df <- results_df |>
              dplyr::mutate(Variable = var_label, .before = 1)
          }
        }

        all_results[[i]] <- results_df
      }

      # Coerce factor and other non-character columns that are used as disaggregation
      # labels to character so dplyr::bind_rows() can combine them without type errors
      all_results <- lapply(all_results, function(df) {
        df |> dplyr::mutate(dplyr::across(where(is.factor), as.character))
      })

      # Combine all results
      results_df <- dplyr::bind_rows(all_results)

      first_disagg <- disaggregation[[1]]

      # Handle wide format for disaggregation
      if (
        disaggregation_wide &&
          !is.null(first_disagg) &&
          first_disagg %in% names(results_df)
      ) {
        col_order <- c()
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          col_order <- c(col_order, "Variable")
        }
        if ("Value" %in% names(results_df)) {
          col_order <- c(col_order, "Value")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          col_order <- c(col_order, "Unit")
        }
        col_order <- c(col_order, first_disagg)
        if ("n" %in% names(results_df)) {
          col_order <- c(col_order, "n")
        }
        if ("estimate" %in% names(results_df)) {
          col_order <- c(col_order, "estimate")
        }

        results_df <- results_df |>
          dplyr::select(dplyr::all_of(col_order))

        id_cols <- c()
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          id_cols <- c(id_cols, "Variable")
        }
        if ("Value" %in% names(results_df)) {
          id_cols <- c(id_cols, "Value")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          id_cols <- c(id_cols, "Unit")
        }

        value_cols <- c()
        if ("n" %in% names(results_df)) {
          value_cols <- c(value_cols, "n")
        }
        if ("estimate" %in% names(results_df)) {
          value_cols <- c(value_cols, "estimate")
        }

        results_df <- results_df |>
          tidyr::pivot_wider(
            id_cols = dplyr::all_of(id_cols),
            names_from = dplyr::all_of(first_disagg),
            values_from = dplyr::all_of(value_cols),
            names_sep = "_"
          )

        {
          pivot_id_present <- id_cols[id_cols %in% names(results_df)]
          data_col_names <- setdiff(names(results_df), pivot_id_present)

          grp_names_raw <- character(length(data_col_names))
          for (j_col in seq_along(data_col_names)) {
            data_col <- data_col_names[[j_col]]
            for (vc in value_cols) {
              prefix <- paste0(vc, "_")
              if (startsWith(data_col, prefix)) {
                grp_names_raw[[j_col]] <- substr(
                  data_col,
                  nchar(prefix) + 1L,
                  nchar(data_col)
                )
                break
              }
            }
          }
          grp_names <- unique(grp_names_raw[nzchar(grp_names_raw)])

          reordered_cols <- vector(
            "list",
            length(grp_names) * length(value_cols)
          )
          k_col <- 0L
          for (grp in grp_names) {
            for (vc in value_cols) {
              col <- paste0(vc, "_", grp)
              if (col %in% names(results_df)) {
                k_col <- k_col + 1L
                reordered_cols[[k_col]] <- col
              }
            }
          }
          reordered_cols <- unlist(reordered_cols[seq_len(k_col)])

          if (length(reordered_cols) > 0) {
            results_df <- results_df |>
              dplyr::select(dplyr::all_of(c(pivot_id_present, reordered_cols)))
          }
        }
      } else {
        # Long format (original behavior)
        if (
          n_vars > 1 &&
            !is.null(first_disagg) &&
            first_disagg %in% names(results_df)
        ) {
          other_cols <- setdiff(
            names(results_df),
            c(first_disagg, "Variable", "Value")
          )
          results_df <- results_df |>
            dplyr::select(
              !!rlang::sym(first_disagg),
              Variable,
              Value,
              dplyr::all_of(other_cols)
            )
        }
      }

      if (!show_n && "n" %in% names(results_df)) {
        if (disaggregation_wide) {
          n_cols <- grep("^n_", names(results_df), value = TRUE)
          if (length(n_cols) > 0) {
            results_df <- results_df |> dplyr::select(-dplyr::all_of(n_cols))
          }
        } else {
          results_df <- results_df |> dplyr::select(-n)
        }
      }

      if (!show_unit && "Unit" %in% names(results_df)) {
        results_df <- results_df |> dplyr::select(-Unit)
      }

      # Build flextable
      ft <- flextable::flextable(as.data.frame(results_df))

      # Handle two-level headers for wide format
      if (disaggregation_wide && !is.null(first_disagg)) {
        disagg_groups <- unique(gsub(
          "^(n|estimate)_(.*)$",
          "\\2",
          grep("^(n|estimate)_", names(results_df), value = TRUE)
        ))

        header_df <- data.frame(
          col_keys = names(results_df),
          line1 = character(length(names(results_df))),
          line2 = character(length(names(results_df))),
          stringsAsFactors = FALSE
        )

        for (col in names(results_df)) {
          if (col == "Variable" && n_vars > 1) {
            header_df[header_df$col_keys == col, "line1"] <- "Variable"
            header_df[header_df$col_keys == col, "line2"] <- "Variable"
          } else if (col == "Value") {
            val_label <- if (n_vars == 1) variable_label_with_n[1] else "Value"
            header_df[header_df$col_keys == col, "line1"] <- val_label
            header_df[header_df$col_keys == col, "line2"] <- val_label
          } else if (col == "Unit" && show_unit) {
            header_df[header_df$col_keys == col, "line1"] <- "Unit"
            header_df[header_df$col_keys == col, "line2"] <- "Unit"
          }
        }

        for (group in disagg_groups) {
          n_col <- paste0("n_", group)
          est_col <- paste0("estimate_", group)

          if (n_col %in% names(results_df)) {
            header_df[header_df$col_keys == n_col, "line1"] <- group
            header_df[header_df$col_keys == n_col, "line2"] <- "n"
          }
          if (est_col %in% names(results_df)) {
            header_df[header_df$col_keys == est_col, "line1"] <- group
            header_df[header_df$col_keys == est_col, "line2"] <- "Estimate"
          }
        }

        ft <- flextable::set_header_df(
          ft,
          mapping = header_df,
          key = "col_keys"
        )

        for (group in disagg_groups) {
          group_cols <- names(results_df)[endsWith(
            names(results_df),
            paste0("_", group)
          )]
          if (length(group_cols) > 1) {
            ft <- flextable::merge_at(
              ft,
              i = 1,
              j = group_cols,
              part = "header"
            )
          }
        }

        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          ft <- flextable::merge_at(
            ft,
            i = 1:2,
            j = "Variable",
            part = "header"
          )
        }
        if ("Value" %in% names(results_df)) {
          ft <- flextable::merge_at(ft, i = 1:2, j = "Value", part = "header")
        }
        if (show_unit && "Unit" %in% names(results_df)) {
          ft <- flextable::merge_at(ft, i = 1:2, j = "Unit", part = "header")
        }
      } else {
        # Single-level headers (original behavior)
        col_labels <- list()

        if (!is.null(first_disagg) && first_disagg %in% names(results_df)) {
          col_labels[[first_disagg]] <- disaggregation_label %||% first_disagg
        }

        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          col_labels[["Variable"]] <- "Variable"
        }

        if ("Value" %in% names(results_df)) {
          col_labels[["Value"]] <- if (n_vars == 1) {
            variable_label_with_n[1]
          } else {
            "Value"
          }
        }

        if (show_n && "n" %in% names(results_df)) {
          col_labels[["n"]] <- "n"
        }

        if (show_unit && "Unit" %in% names(results_df)) {
          col_labels[["Unit"]] <- "Unit"
        }

        if ("estimate" %in% names(results_df)) {
          col_labels[["estimate"]] <- "Estimate"
        }

        present_labels <- col_labels[names(col_labels) %in% names(results_df)]
        if (length(present_labels) > 0) {
          ft <- flextable::set_header_labels(ft, values = present_labels)
        }
      }

      # Styling — apply standard phr theme
      ft <- apply_phr_flextable_theme(ft, color_palette = color_palette)

      # Right-align numeric columns
      if (disaggregation_wide) {
        right_cols <- grep("^(n|estimate)_", names(results_df), value = TRUE)
      } else {
        right_cols <- intersect(c("estimate", "n"), names(results_df))
      }
      if (length(right_cols) > 0) {
        ft <- flextable::align(
          ft,
          j = right_cols,
          align = "right",
          part = "body"
        )
      }

      # Center-align Unit column
      if (show_unit && "Unit" %in% names(results_df)) {
        ft <- flextable::align(ft, j = "Unit", align = "center", part = "body")
        ft <- flextable::align(
          ft,
          j = "Unit",
          align = "center",
          part = "header"
        )
      }

      # Merge cells for long format multi-variable tables
      if (!disaggregation_wide) {
        if (n_vars > 1 && "Variable" %in% names(results_df)) {
          var_values <- results_df[["Variable"]]
          run_starts <- which(!duplicated(var_values))
          run_ends <- c(run_starts[-1] - 1, length(var_values))

          for (k in seq_along(run_starts)) {
            if (run_ends[k] > run_starts[k]) {
              ft <- flextable::merge_at(
                ft,
                i = run_starts[k]:run_ends[k],
                j = "Variable",
                part = "body"
              )
            }
          }
          ft <- flextable::valign(
            ft,
            j = "Variable",
            valign = "top",
            part = "body"
          )
        }

        if (!is.null(first_disagg) && first_disagg %in% names(results_df)) {
          if (n_vars > 1 && "Variable" %in% names(results_df)) {
            var_values <- results_df[["Variable"]]
            var_starts <- which(!duplicated(var_values))
            var_ends <- c(var_starts[-1] - 1, nrow(results_df))

            for (v in seq_along(var_starts)) {
              var_row_range <- var_starts[v]:var_ends[v]
              disagg_in_var <- results_df[[first_disagg]][var_row_range]

              disagg_run_starts <- which(!duplicated(disagg_in_var))
              disagg_run_ends <- c(
                disagg_run_starts[-1] - 1,
                length(disagg_in_var)
              )

              for (d in seq_along(disagg_run_starts)) {
                if (disagg_run_ends[d] > disagg_run_starts[d]) {
                  actual_rows <- var_row_range[
                    disagg_run_starts[d]:disagg_run_ends[d]
                  ]
                  ft <- flextable::merge_at(
                    ft,
                    i = actual_rows,
                    j = first_disagg,
                    part = "body"
                  )
                }
              }
            }
          } else {
            disagg_values <- results_df[[first_disagg]]
            run_starts <- which(!duplicated(disagg_values))
            run_ends <- c(run_starts[-1] - 1, length(disagg_values))

            for (k in seq_along(run_starts)) {
              if (run_ends[k] > run_starts[k]) {
                ft <- flextable::merge_at(
                  ft,
                  i = run_starts[k]:run_ends[k],
                  j = first_disagg,
                  part = "body"
                )
              }
            }
          }

          ft <- flextable::valign(
            ft,
            j = first_disagg,
            valign = "top",
            part = "body"
          )
        }
      }

      if (!is.null(title_name)) {
        ft <- flextable::set_caption(ft, caption = title_name)
      } else {
        first_var_label <- variable_label[1]
        auto_caption <- paste(phr_txt("Frequency Table of"), first_var_label)
        first_disagg_col <- disaggregation[[1]]
        if (!is.null(first_disagg_col)) {
          disagg_lbl <- if (!is.null(disaggregation_label)) {
            disaggregation_label
          } else {
            first_disagg_col
          }
          auto_caption <- paste0(auto_caption, phr_txt(", by"), " ", disagg_lbl)
        }
        ft <- flextable::set_caption(ft, caption = auto_caption)
      }

      if (nrow(results_df) > 1) {
        even_rows <- seq(2, nrow(results_df), by = 2)
        ft <- flextable::bg(ft, i = even_rows, bg = "#f5f5f5", part = "body")
      }

      if (!is.null(table_width)) {
        ft <- flextable::width(ft, width = table_width)
      }

      if (!is.null(table_height)) {
        ft <- flextable::height(ft, height = table_height)
      }

      return(ft)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Create a Quality Penalty Summary Table (flextable)
#'
#' Takes the results data frame from a \code{DataAnalytics} object's
#' \code{results_to_table()} and produces a formatted flextable summarising
#' penalties by \code{check_group}. Each row shows the statistical test result
#' and penalty for an individual check, and merged cells display the summed
#' penalty per check group.
#'
#' @param results_df A data frame as returned by
#'   \code{DataAnalytics$results_to_table()}. Must contain columns:
#'   \code{check_name}, \code{check_label}, \code{penalty}. Optional columns:
#'   \code{check_group}, \code{test_statistic}, \code{p_value},
#'   \code{max_penalty}.
#' @param title_name Optional title for the table caption. Default: NULL.
#' @param show_max_penalty Logical. Whether to include the \code{max_penalty}
#'   column. Default: TRUE.
#' @param digits Number of decimal places for numeric columns. Default: 3.
#' @param color_palette Character string naming the colour palette for the
#'   table header. Passed to \code{\link{apply_phr_flextable_theme}}. Accepts
#'   any palette name supported by \code{\link{get_color_palette}} (e.g.
#'   \code{"reach1"}, \code{"reach2"}, \code{"group"}). Default: \code{"reach1"}.
#'
#' @return A \code{flextable} object.
#' @export
#'
#' @examples
#' \dontrun{
#'   dq$run_quality_checks()
#'   tbl <- table_quality_penalty_summary(dq$results_to_table())
#'   print(tbl)
#' }
table_quality_penalty_summary <- function(
  results_df,
  title_name = NULL,
  show_max_penalty = TRUE,
  digits = 3,
  color_palette = "reach1"
) {
  origin <- "table_quality_penalty_summary"

  phrutils::phr_try(
    {
      # Validate inputs
      phrutils::phr_validate_dataframe(
        results_df,
        origin = origin,
        soft = FALSE
      )

      required_cols <- c("check_name", "check_label", "penalty")
      phrutils::phr_validate_columns(
        results_df,
        required_cols,
        origin = origin,
        soft = FALSE
      )

      # Track whether check_group was originally provided with real values
      has_check_group <- "check_group" %in%
        names(results_df) &&
        any(!is.na(results_df$check_group) & nzchar(results_df$check_group))

      # Ensure check_group column exists; fill with "(Ungrouped)" if missing
      if (!"check_group" %in% names(results_df)) {
        results_df$check_group <- NA_character_
      }

      # Ensure numeric columns exist with NA defaults
      for (col in c("test_statistic", "p_value", "penalty", "max_penalty")) {
        if (!col %in% names(results_df)) {
          results_df[[col]] <- NA_real_
        }
      }

      # Replace NA check_group with "(Ungrouped)"
      results_df$check_group <- ifelse(
        is.na(results_df$check_group) | !nzchar(results_df$check_group),
        "(Ungrouped)",
        results_df$check_group
      )

      # Compute group penalty sums
      group_sums <- results_df |>
        dplyr::group_by(check_group) |>
        dplyr::summarise(
          group_penalty_sum = sum(penalty, na.rm = TRUE),
          group_max_penalty_sum = sum(max_penalty, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          # Display as "numerator/denominator" string
          group_penalty = paste0(group_penalty_sum, "/", group_max_penalty_sum),
          pct_group_penalty = dplyr::case_when(
            is.na(group_max_penalty_sum) |
              group_max_penalty_sum == 0 ~ NA_real_,
            TRUE ~ round(
              group_penalty_sum / group_max_penalty_sum * 100,
              digits
            )
          )
        ) |>
        dplyr::select(-group_penalty_sum, -group_max_penalty_sum)

      # Build display table
      results_display <- results_df |>
        dplyr::left_join(
          group_sums[, c("check_group", "group_penalty", "pct_group_penalty")],
          by = "check_group"
        ) |>
        dplyr::arrange(check_group, check_name) |>
        dplyr::mutate(
          test_statistic = round(as.numeric(test_statistic), digits),
          p_value = round(as.numeric(p_value), digits),
          penalty = as.numeric(penalty)
        )

      # Select and order display columns — only include check_group when it has real values
      all_cols <- c("check_label", "test_statistic", "p_value", "penalty")
      if (has_check_group) {
        all_cols <- c("check_group", all_cols)
      }
      if (show_max_penalty) {
        all_cols <- c(all_cols, "max_penalty")
      }
      all_cols <- c(all_cols, "group_penalty", "pct_group_penalty")

      results_display <- results_display |>
        dplyr::select(dplyr::all_of(intersect(
          all_cols,
          names(results_display)
        )))

      # Build flextable
      ft <- flextable::flextable(as.data.frame(results_display))

      # Column headers
      header_map <- list(
        check_group = "Check Group",
        check_label = "Check",
        test_statistic = "Test Statistic",
        p_value = "P-Value",
        penalty = "Penalty",
        max_penalty = "Max Penalty",
        group_penalty = "Group Total Penalty",
        pct_group_penalty = "% Group Penalty"
      )
      ft <- flextable::set_header_labels(
        ft,
        values = header_map[intersect(
          names(header_map),
          names(results_display)
        )]
      )

      # Merge check_group cells (one per group) — only when check_group is shown
      if (has_check_group) {
        ft <- flextable::merge_v(ft, j = "check_group")
      }

      # Merge group_penalty and pct_group_penalty cells (one per group)
      ft <- flextable::merge_v(ft, j = "group_penalty")
      if ("pct_group_penalty" %in% names(results_display)) {
        ft <- flextable::merge_v(ft, j = "pct_group_penalty")
      }

      # Styling — apply standard phr theme
      ft <- apply_phr_flextable_theme(ft, color_palette = color_palette)
      valign_cols <- "group_penalty"
      if (has_check_group) {
        valign_cols <- c("check_group", valign_cols)
      }
      ft <- flextable::valign(
        ft,
        j = valign_cols,
        valign = "top",
        part = "body"
      )
      if ("pct_group_penalty" %in% names(results_display)) {
        ft <- flextable::valign(
          ft,
          j = "pct_group_penalty",
          valign = "top",
          part = "body"
        )
      }
      ft <- flextable::align(
        ft,
        j = intersect(
          c(
            "test_statistic",
            "p_value",
            "penalty",
            "max_penalty",
            "pct_group_penalty"
          ),
          names(results_display)
        ),
        align = "right",
        part = "body"
      )
      # group_penalty is a "sum/max" string — center it
      ft <- flextable::align(
        ft,
        j = "group_penalty",
        align = "center",
        part = "body"
      )

      # Highlight rows where penalty > 0
      penalty_rows <- which(results_display$penalty > 0)
      if (length(penalty_rows) > 0) {
        ft <- flextable::bg(
          ft,
          i = penalty_rows,
          j = "penalty",
          bg = "#FFE0E0",
          part = "body"
        )
      }

      # Add title caption if provided
      if (!is.null(title_name)) {
        ft <- flextable::set_caption(ft, caption = title_name)
      }

      return(ft)
    },
    on_error = "warn",
    origin = origin
  )
}


#' Create a Quality Penalty Summary Table by Group — Wide Format (flextable)
#'
#' Takes a per-group results data frame (e.g. as produced by
#' \code{DataAnalytics$.compute_results_by_group()}) and produces a formatted
#' flextable summarising penalties broken down by a grouping variable such as
#' enumerator ID or stratum.  The output uses a \strong{wide format}: each
#' unique group value becomes a column set rather than a row group.  The table
#' has two-level column headers — the group value at the top level and three
#' metric columns at the second level (\emph{Penalty}, \emph{Max Penalty},
#' \emph{Group Total Penalty}).  Test statistics and p-values are not shown.
#'
#' @param results_df A data frame containing per-group quality check results.
#'   Must contain columns: \code{check_name}, \code{check_label},
#'   \code{penalty}, and the column named by \code{group_col}. Optional
#'   columns: \code{check_group}, \code{max_penalty}.
#' @param group_col Character string naming the column in \code{results_df}
#'   that contains the group values (e.g. \code{"group_value"} for enumerator
#'   ID or stratum breakdowns).
#' @param group_label Optional display label for the grouping variable.
#'   Currently retained for backward compatibility; the top-level column
#'   headers display the actual group values. Default: \code{NULL}.
#' @param title_name Optional title for the table caption. Default: NULL.
#' @param show_max_penalty Logical. Whether to include the \code{max_penalty}
#'   column for each group. Default: TRUE.
#' @param digits Number of decimal places for numeric columns. Default: 3.
#' @param color_palette Character string naming the colour palette for the
#'   table header. Passed to \code{\link{apply_phr_flextable_theme}}. Accepts
#'   any palette name supported by \code{\link{get_color_palette}} (e.g.
#'   \code{"reach1"}, \code{"reach2"}, \code{"group"}). Default: \code{"reach1"}.
#'
#' @return A \code{flextable} object with one set of columns per group value.
#' @export
#'
#' @examples
#' \dontrun{
#'   dq$run_quality_checks()
#'   per_group_df <- dq$.compute_results_by_group("enumerator_col")
#'   tbl <- table_quality_penalty_summary_by_group(per_group_df, group_col = "group_value")
#'   print(tbl)
#' }
table_quality_penalty_summary_by_group <- function(
  results_df,
  group_col,
  group_label = NULL,
  title_name = NULL,
  show_max_penalty = TRUE,
  digits = 3,
  color_palette = "reach1"
) {
  origin <- "table_quality_penalty_summary_by_group"

  phrutils::phr_try(
    {
      # Validate inputs
      phrutils::phr_validate_dataframe(
        results_df,
        origin = origin,
        soft = FALSE
      )

      required_cols <- c("check_name", "check_label", "penalty", group_col)
      phrutils::phr_validate_columns(
        results_df,
        required_cols,
        origin = origin,
        soft = FALSE
      )

      # Track whether check_group was originally provided with real values
      has_check_group <- "check_group" %in%
        names(results_df) &&
        any(!is.na(results_df$check_group) & nzchar(results_df$check_group))

      # Ensure optional columns exist with NA defaults
      if (!"check_group" %in% names(results_df)) {
        results_df$check_group <- NA_character_
      }
      if (!"max_penalty" %in% names(results_df)) {
        results_df$max_penalty <- NA_real_
      }

      # Replace NA check_group with "(Ungrouped)"
      results_df$check_group <- ifelse(
        is.na(results_df$check_group) | !nzchar(results_df$check_group),
        "(Ungrouped)",
        results_df$check_group
      )

      # Ordered unique group values
      group_values <- sort(unique(results_df[[group_col]]))

      # Compute group total penalty per check_group and group value (displayed as "sum/max" string)
      group_totals <- results_df |>
        dplyr::group_by(.data[[group_col]], check_group) |>
        dplyr::summarise(
          group_penalty_sum = sum(penalty, na.rm = TRUE),
          group_max_penalty_sum = sum(max_penalty, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          group_total = paste0(group_penalty_sum, "/", group_max_penalty_sum)
        ) |>
        dplyr::select(dplyr::all_of(c(group_col, "check_group", "group_total")))

      # Helper: create safe R column name suffix for a group value
      make_col_suffix <- function(value) {
        gsub("[^A-Za-z0-9]", "_", as.character(value))
      }

      # Base rows: unique check_group + check_name + check_label (row identity)
      base_df <- results_df |>
        dplyr::select(check_group, check_name, check_label) |>
        dplyr::distinct() |>
        dplyr::arrange(check_group, check_name)

      # Build wide data frame: join per-group penalty columns onto base rows
      wide_df <- base_df
      for (gv in group_values) {
        sfx <- make_col_suffix(gv)
        gv_data <- results_df |>
          dplyr::filter(.data[[group_col]] == gv) |>
          dplyr::select(check_name, penalty, max_penalty) |>
          dplyr::mutate(
            penalty = as.numeric(penalty),
            max_penalty = as.numeric(max_penalty)
          )

        pen_col <- paste0("penalty__", sfx)
        maxp_col <- paste0("max_penalty__", sfx)
        total_col <- paste0("group_total__", sfx)

        names(gv_data)[names(gv_data) == "penalty"] <- pen_col
        names(gv_data)[names(gv_data) == "max_penalty"] <- maxp_col

        gv_total_map <- group_totals |>
          dplyr::filter(.data[[group_col]] == gv) |>
          dplyr::select(check_group, group_total)
        names(gv_total_map)[names(gv_total_map) == "group_total"] <- total_col

        wide_df <- dplyr::left_join(wide_df, gv_data, by = "check_name")
        wide_df <- dplyr::left_join(wide_df, gv_total_map, by = "check_group")
      }

      # Build final column order: check_group (when present), check_label, then per-group columns
      col_order <- if (has_check_group) {
        c("check_group", "check_label")
      } else {
        c("check_label")
      }
      for (gv in group_values) {
        sfx <- make_col_suffix(gv)
        col_order <- c(col_order, paste0("penalty__", sfx))
        if (show_max_penalty) {
          col_order <- c(col_order, paste0("max_penalty__", sfx))
        }
        col_order <- c(col_order, paste0("group_total__", sfx))
      }
      col_order <- intersect(col_order, names(wide_df))

      # Drop check_name (only used for joining)
      wide_df <- wide_df |>
        dplyr::select(dplyr::all_of(col_order))

      # Build flextable
      ft <- flextable::flextable(as.data.frame(wide_df))

      # Second-level (bottom) column header labels
      second_labels <- if (has_check_group) {
        c(check_group = "Check Group", check_label = "Check")
      } else {
        c(check_label = "Check")
      }
      for (gv in group_values) {
        sfx <- make_col_suffix(gv)
        second_labels[[paste0("penalty__", sfx)]] <- "Penalty"
        if (show_max_penalty) {
          second_labels[[paste0("max_penalty__", sfx)]] <- "Max Penalty"
        }
        second_labels[[paste0("group_total__", sfx)]] <- "Group Total Penalty"
      }
      ft <- flextable::set_header_labels(ft, values = as.list(second_labels))

      # First-level (top) header row: blank for identifier cols, group value per group
      n_per_group <- if (show_max_penalty) 3L else 2L
      n_identifier_cols <- if (has_check_group) 2L else 1L
      top_values <- c(rep("", n_identifier_cols), as.character(group_values))
      top_widths <- c(
        rep(1L, n_identifier_cols),
        rep(n_per_group, length(group_values))
      )
      ft <- flextable::add_header_row(
        ft,
        values = top_values,
        colwidths = top_widths,
        top = TRUE
      )

      # Apply standard phr theme
      ft <- apply_phr_flextable_theme(ft, color_palette = color_palette)

      # Align numeric (penalty / max_penalty) columns right; group_total centered
      penalty_cols <- col_order[grepl("^penalty__", col_order)]
      maxp_cols <- col_order[grepl("^max_penalty__", col_order)]
      total_cols <- col_order[grepl("^group_total__", col_order)]

      if (length(c(penalty_cols, maxp_cols)) > 0) {
        ft <- flextable::align(
          ft,
          j = c(penalty_cols, maxp_cols),
          align = "right",
          part = "body"
        )
      }
      if (length(total_cols) > 0) {
        ft <- flextable::align(
          ft,
          j = total_cols,
          align = "center",
          part = "body"
        )
      }

      # Merge check_group cells vertically — only when check_group is shown
      if (has_check_group) {
        ft <- flextable::merge_v(ft, j = "check_group", part = "body")
        ft <- flextable::valign(
          ft,
          j = "check_group",
          valign = "top",
          part = "body"
        )
      }

      # Merge group_total cells vertically within each check_group block
      if (length(total_cols) > 0) {
        for (tc in total_cols) {
          ft <- flextable::merge_v(ft, j = tc, part = "body")
        }
        ft <- flextable::valign(
          ft,
          j = total_cols,
          valign = "top",
          part = "body"
        )
      }

      # Highlight cells where penalty > 0
      for (gv in group_values) {
        pen_col <- paste0("penalty__", make_col_suffix(gv))
        if (pen_col %in% names(wide_df)) {
          rows_to_highlight <- which(
            !is.na(wide_df[[pen_col]]) & wide_df[[pen_col]] > 0
          )
          if (length(rows_to_highlight) > 0) {
            ft <- flextable::bg(
              ft,
              i = rows_to_highlight,
              j = pen_col,
              bg = "#FFE0E0",
              part = "body"
            )
          }
        }
      }

      # Add title caption if provided
      if (!is.null(title_name)) {
        ft <- flextable::set_caption(ft, caption = title_name)
      }

      return(ft)
    },
    on_error = "warn",
    origin = origin
  )
}
