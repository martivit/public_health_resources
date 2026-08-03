#' Quality Schema Utility Functions
#'
#' This file contains utility functions for working with quality check schemas,
#' including conversion between nested list and table formats, and validation.
#'
#' @name quality_schema_utils
NULL

#' Convert quality check schema to table form
#'
#' Converts a nested list quality schema to a flat table format where each
#' check can have multiple rows (one per threshold).
#'
#' @param qc_schema A nested list quality schema
#' @return A data frame with one row per threshold
#' @export
quality_schema_to_table <- function(qc_schema) {

  quality_validate_schema_to_table(
    qc_schema,
    origin = "quality_schema_to_table"
  )

  rows <- list()

  for (nm in names(qc_schema)) {

    x <- qc_schema[[nm]]

    check_name <- x$check_name %||% nm
    check_label <- x$check_label %||% NA_character_
    check_group <- x$check_group %||% NA_character_

    variables <- if (!is.null(x$variables)) {
      paste(x$variables, collapse = ",")
    } else NA_character_

    statistical_test <- x$statistical_test %||% NA_character_

    test_params <- if (!is.null(x$test_params) && length(x$test_params) > 0) {
      paste(names(x$test_params), x$test_params, sep = "=", collapse = ",")
    } else NA_character_


    # Thresholds → rows (paired, lossless)

    if (!is.null(x$thresholds) && length(x$thresholds) > 0) {

      for (thr in x$thresholds) {
        rows <- append(rows, list(tibble::tibble(
          check_group = check_group,
          check_name = check_name,
          check_label = check_label,
          variables = variables,
          statistical_test = statistical_test,
          threshold_expression = thr$threshold_expression %||% NA_character_,
          penalty_score = thr$penalty_score %||% 0,
          test_params = test_params
        )))
      }

    } else {

      # No thresholds → single NA row
      rows <- append(rows, list(tibble::tibble(
        check_group = check_group,
        check_name = check_name,
        check_label = check_label,
        variables = variables,
        statistical_test = statistical_test,
        threshold_expression = NA_character_,
        penalty_score = 0,
        test_params = test_params
      )))
    }
  }

  dplyr::bind_rows(rows)
}


#' Convert a QC schema table back into canonical nested list
#'
#' Converts a flat table format (where each check can have multiple rows)
#' back into a nested list structure.
#'
#' @param df A data frame with quality check definitions
#' @return A nested list quality schema
#' @export
quality_table_to_schema <- function(df) {

  phr_validate_dataframe(df, origin = "quality_table_to_schema", soft = FALSE)
  quality_validate_table_to_schema(df)

  phr_validate_columns(
    df,
    required_cols = c(
      "check_name",
      "check_label",
      "variables",
      "statistical_test",
      "threshold_expression",
      "penalty_score",
      "test_params"
    ),
    origin = "quality_table_to_schema",
    hint = phr_txt("Quality table must include all required schema fields."),
    soft = FALSE
  )

  checks <- list()

  for (check_name in unique(df$check_name)) {

    check_rows <- df[df$check_name == check_name, , drop = FALSE]
    first_row <- check_rows[1, ]

    # check_group is optional
    check_group <- if ("check_group" %in% names(first_row) &&
                       !is.na(first_row$check_group) &&
                       nzchar(first_row$check_group)) {
      first_row$check_group
    } else NULL

    variables <- if (!is.na(first_row$variables) && nzchar(first_row$variables)) {
      strsplit(first_row$variables, ",")[[1]] |> trimws()
    } else NULL

    test_params <- if (!is.na(first_row$test_params) &&
                       nzchar(first_row$test_params)) {

      pairs <- .parse_indicator_arguments(first_row$test_params)
      params <- list()

      for (pair in pairs) {
        pair <- trimws(pair)
        eq_pos <- regexpr("=", pair)[[1]]
        if (eq_pos > 1) {
          key <- trimws(substr(pair, 1, eq_pos - 1))
          value_str <- trimws(substr(pair, eq_pos + 1, nchar(pair)))
          value <- suppressWarnings(as.numeric(value_str))
          if (is.na(value)) {
            if (toupper(value_str) %in% c("TRUE", "FALSE")) {
              value <- as.logical(value_str)
            } else {
              value <- value_str
            }
          }
          params[[key]] <- value
        }
      }
      params
    } else NULL


    # Build paired thresholds list

    thresholds <- list()

    for (i in seq_len(nrow(check_rows))) {
      row <- check_rows[i, ]

      if (!is.na(row$threshold_expression) && nzchar(row$threshold_expression)) {
        thresholds[[length(thresholds) + 1]] <- list(
          threshold_expression = row$threshold_expression,
          penalty_score        = as.numeric(row$penalty_score %||% 0)
        )
      }
    }

    checks[[check_name]] <- list(
      check_name       = check_name,
      check_label      = first_row$check_label,
      check_group      = check_group,
      variables        = variables,
      statistical_test = first_row$statistical_test,
      thresholds       = thresholds,
      test_params      = test_params
    )
  }

  list(
    checks   = checks
  )
}


#' Validate whether QC schema table is safe before schema update
#'
#' @param df Data frame with quality check definitions
#' @param data_obj Optional data object to check variable existence
#' @return TRUE if valid, otherwise throws error
#' @export
quality_validate_table_to_schema <- function(df, data_obj = NULL) {

  phr_try({

    phr_validate_dataframe(
      df,
      origin = "quality_validate_table_to_schema",
      soft = FALSE
    )

    required_cols <- c(
      "check_name",
      "check_label",
      "variables",
      "statistical_test",
      "threshold_expression",
      "penalty_score",
      "test_params"
    )


    # Required column check (delegated)

    phr_validate_columns(
      df,
      required_cols = required_cols,
      origin = "quality_validate_table_to_schema",
      hint = phr_txt("QC schema table must include all required fields."),
      soft = FALSE
    )


    # Required NA check (delegated)

    phr_validate_required_na(
      df,
      required_cols = required_cols[required_cols != "test_params"],
      origin = "quality_validate_table_to_schema",
      hint = phr_txt("QC schema table cannot contain missing values in required fields."),
      soft = FALSE
    )


    # Duplicate identical row check

    phr_validate_no_duplicates(
      df,
      origin = "quality_validate_table_to_schema",
      hint   = phr_txt("Each QC rule row must be unique."),
      soft   = FALSE
    )


    # Optional variable existence check (unchanged)

    if (!is.null(data_obj)) {

      vars <- unique(unlist(strsplit(df$variables, ",")))
      vars <- vars[!is.na(vars)]

      missing_vars <- setdiff(vars, names(data_obj$raw_data))
      if (length(missing_vars) > 0) {
        phr_warning(
          "QC Schema",
          phr_txt(
            "QC schema references missing variables: {paste(missing_vars, collapse=', ')}"
          )
        )
      }
    }

    TRUE

  }, on_error = "abort", origin = "quality_validate_table_to_schema")
}


#' Validate Quality Check Schema Structure
#'
#' This function validates the structure and internal consistency
#' of a quality check schema (list-of-lists). It does NOT run the
#' quality checks themselves — it only verifies that the QC schema
#' is well-formed and safe to use.
#'
#' The function enforces that the list name used in the checks list
#' (e.g., `qc_schema$checks$check_name`) must be identical to the
#' `check_name` field value within that check. This ensures consistency
#' during schema-to-table and table-to-schema conversions.
#'
#' @param qc_schema A list containing `checks` (canonical QC schema)
#' @param origin Origin string for error messages
#' @return Invisibly returns TRUE if valid, otherwise throws error
#' @export
quality_validate_schema_to_table <- function(qc_schema, origin = "quality_validate_schema_to_table") {

  phr_try({

    # 1. Top-level structure check

    if (is.null(qc_schema) || !is.list(qc_schema)) {
      phr_error(
        origin  = origin,
        message = phr_txt("Quality schema must be a list object."),
        hint    = phr_txt("Ensure qc_schema is a named list with a `checks` element.")
      )
    }

    if (is.null(qc_schema) || !is.list(qc_schema)) {
      phr_error(
        origin  = origin,
        message = phr_txt("Quality schema must contain a `checks` list."),
        hint    = phr_txt("Structure should be qc_schema$checks = list(check1 = list(...), ...).")
      )
    }

    # 2. Validate each check entry

    purrr::iwalk(qc_schema, function(chk, name) {

      if (!is.list(chk)) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("Check '{name}' must be a list."))
        )
      }


      # REQUIRED FIELDS (updated)

      required_fields <- c(
        "check_name",
        "check_label",
        "variables",
        "statistical_test",
        "thresholds",
        "test_params"
      )

      missing <- setdiff(required_fields, names(chk))
      if (length(missing) > 0) {
        phr_error(
          origin  = origin,
          message = phr_txt(
            glue::glue(
              "Check '{name}' is missing required fields: {paste(missing, collapse = ', ')}."
            )
          ),
          hint = phr_txt(
            glue::glue(
              "Ensure each check defines: {paste(required_fields, collapse = ', ')}."
            )
          )
        )
      }

      # check_name
      if (!is.character(chk$check_name) || length(chk$check_name) != 1) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`check_name` in '{name}' must be a single character string."))
        )
      }

      if (chk$check_name != name) {
        phr_error(
          origin  = origin,
          message = phr_txt(
            glue::glue(
              "Check list name '{name}' does not match check_name field value '{chk$check_name}'."
            )
          ),
          hint = phr_txt(
            "The name used in the checks list (qc_schema$checks$<name>) must be identical to the check_name field value within that check."
          )
        )
      }

      # check_label
      if (!is.character(chk$check_label) || length(chk$check_label) != 1) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`check_label` in '{name}' must be a single character string."))
        )
      }

      # variables (optional)
      if (!is.null(chk$variables) &&
          !(is.character(chk$variables) && is.atomic(chk$variables))) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`variables` in '{name}' must be a character vector."))
        )
      }

      # statistical_test
      if (!is.character(chk$statistical_test) || length(chk$statistical_test) != 1) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`statistical_test` in '{name}' must be a single character string."))
        )
      }


      # NEW: thresholds validation (paired structure)

      if (!is.list(chk$thresholds)) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`thresholds` in '{name}' must be a list."))
        )
      }

      purrr::iwalk(chk$thresholds, function(thr, i) {

        if (!is.list(thr)) {
          phr_error(
            origin  = origin,
            message = phr_txt(glue::glue("Each threshold in '{name}' must be a list."))
          )
        }

        required_thr_fields <- c("threshold_expression", "penalty_score")
        missing_thr <- setdiff(required_thr_fields, names(thr))

        if (length(missing_thr) > 0) {
          phr_error(
            origin  = origin,
            message = phr_txt(
              glue::glue(
                "Threshold {i} in '{name}' is missing fields: {paste(missing_thr, collapse = ', ')}."
              )
            )
          )
        }

        # threshold_expression
        if (!is.character(thr$threshold_expression) ||
            length(thr$threshold_expression) != 1) {
          phr_error(
            origin  = origin,
            message = phr_txt(
              "threshold_expression in threshold {i} of '{name}' must be a single character string."
            )
          )
        }

        if (!is_logical_expression(thr$threshold_expression)) {
          phr_error(
            origin  = origin,
            message = phr_txt(
              "threshold_expression in threshold {i} of '{name}' must be a valid logical expression."
            ),
            hint = phr_txt(
              "Use logical comparisons or functions like `is.na()` or `grepl()`."
            )
          )
        }

        # penalty_score
        if (!is.numeric(thr$penalty_score) || length(thr$penalty_score) != 1) {
          phr_error(
            origin  = origin,
            message = phr_txt(
              "penalty_score in threshold {i} of '{name}' must be a single numeric value."
            )
          )
        }
      })

      # test_params
      if (!is.null(chk$test_params) && !is.list(chk$test_params)) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`test_params` in '{name}' must be a list."))
        )
      }

      # check_group (optional)
      if (!is.null(chk$check_group) &&
          !(is.character(chk$check_group) && length(chk$check_group) == 1)) {
        phr_error(
          origin  = origin,
          message = phr_txt(glue::glue("`check_group` in '{name}' must be a single character string or NULL."))
        )
      }
    })

    invisible(TRUE)

  }, on_error = "abort", origin = origin)
}
