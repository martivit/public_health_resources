#' Convert a canonical schema list into a flat table format
#' @param schema_list A named list representing the canonical data schema, as produced by
#'   \code{data_table_to_schema()} or constructed manually.
#' @export
data_schema_to_table <- function(schema_list) {
  # 0. VALIDATE SCHEMA FIRST (fixes failing tests)
  data_validate_schema_to_table(
    schema_list = schema_list,
    origin      = "data_schema_to_table"
  )

  # 1. FIXED COLUMN SET FOR TABLE EXPORT
  # Define the fixed column set for exporting the schema list to a table.
  # Place question_type, is_other and other_column_link immediately after type.
  # Include "value" column after "variable" for canonical value mapping.
  # Include an optional `action` column for dependency rules after dep_group.
  # Note: The following exported column names have been removed from variable schema
  # and moved to dependency_schema: pattern (from patterns), range (from ranges),
  # precision_limits, mutex_group (from mutually_exclusive), not_future (from date_validity).
  fixed_cols <- c(
    "rule_type", "variable", "value", "required", "type",
    "question_type", "is_other", "other_column_link",
    "allowed", "col_names",
    "unique",
    "variable_label_en", "variable_label_fr", "variable_label_ar",
    "value_label_en",    "value_label_fr",    "value_label_ar",
    "label", "comment"
  )

  # Helper to return a fully NA row (Parse-safe)
  empty_row <- function() {
    x <- setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
    x$required <- NA
    x
  }

  out <- list()

  # 2. VARIABLE ROWS
  # Collect all unique variables from various schema components
  # Note: Variables with value_map will generate multiple rows (one per canonical value)
  # This is intentional to support the new value mapping feature where each canonical
  # value has its own set of allowed dataset values
  # Note: patterns, ranges, precision_limits have been moved to dependency_schema
  vars <- unique(c(
    names(schema_list$types %||% list()),
    names(schema_list$allowed_values %||% list()),
    names(schema_list$value_map %||% list()),
    names(schema_list$question_types %||% list()),
    names(schema_list$is_other %||% list()),
    names(schema_list$other_column_link %||% list()),
    schema_list$required %||% character(0)
  ))

  for (v in vars) {
    # Check if this variable has value mappings
    has_value_map <- !is.null(schema_list$value_map[[v]])

    if (has_value_map) {
      # Generate one row per canonical value
      value_mappings <- schema_list$value_map[[v]]

      for (canonical_value in names(value_mappings)) {
        row <- empty_row()
        row$rule_type <- "variable"
        row$variable  <- v
        row$value     <- canonical_value

        # required
        row$required <- v %in% (schema_list$required %||% character(0))

        # type
        if (!is.null(schema_list$types[[v]])) {
          row[["type"]] <- schema_list$types[[v]]
        } else {
          row[["type"]] <- NA_character_
        }

        # allowed - use the mapped values for this canonical value
        dataset_values <- value_mappings[[canonical_value]]
        if (!is.null(dataset_values) && length(dataset_values) > 0) {
          row[["allowed"]] <- paste(dataset_values, collapse = ",")
        } else {
          row[["allowed"]] <- NA_character_
        }

        # col_names (possible column name alternatives for auto-mapping)
        if (!is.null(schema_list$col_names[[v]])) {
          row[["col_names"]] <- paste(schema_list$col_names[[v]], collapse = ",")
        } else {
          row[["col_names"]] <- NA_character_
        }

        # unique constraint
        if (!is.null(schema_list$unique) && v %in% schema_list$unique) {
          row$unique <- "TRUE"
        } else {
          row$unique <- NA_character_
        }

        # question_type (xlsform/odk question type)
        if (!is.null(schema_list$question_types[[v]])) {
          row$question_type <- schema_list$question_types[[v]]
        } else {
          row$question_type <- NA_character_
        }

        # is_other (flag for "other" open-ended text columns)
        if (!is.null(schema_list$is_other[[v]])) {
          row$is_other <- ifelse(isTRUE(schema_list$is_other[[v]]), "TRUE", "FALSE")
        } else {
          row$is_other <- NA_character_
        }

        # other_column_link (corresponding main column for "other" columns)
        if (!is.null(schema_list$other_column_link[[v]])) {
          if (is.character(schema_list$other_column_link[[v]])) {
            row$other_column_link <- paste(schema_list$other_column_link[[v]], collapse = ",")
          } else {
            row$other_column_link <- as.character(schema_list$other_column_link[[v]])
          }
        } else {
          row$other_column_link <- NA_character_
        }

        # variable_label columns (same for all value rows of this variable)
        for (lang in c("en", "fr", "ar")) {
          col <- paste0("variable_label_", lang)
          lbl <- (schema_list$variable_labels %||% list())[[lang]][[v]]
          row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
        }

        # value_label columns (specific to this canonical value)
        for (lang in c("en", "fr", "ar")) {
          col <- paste0("value_label_", lang)
          lbl <- (schema_list$value_labels %||% list())[[lang]][[v]][[canonical_value]]
          row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
        }

        out <- append(out, list(row))
      }
    } else {
      # No value map - generate single row with value = NA
      row <- empty_row()
      row$rule_type <- "variable"
      row$variable  <- v
      row$value     <- NA_character_

      # required
      row$required <- v %in% (schema_list$required %||% character(0))

      # type
      if (!is.null(schema_list$types[[v]])) {
        row[["type"]] <- schema_list$types[[v]]
      } else {
        row[["type"]] <- NA_character_
      }

      # allowed - use old allowed_values if present
      if (!is.null(schema_list$allowed_values[[v]])) {
        row[["allowed"]] <- paste(schema_list$allowed_values[[v]], collapse = ",")
      } else {
        row[["allowed"]] <- NA_character_
      }

      # col_names (possible column name alternatives for auto-mapping)
      if (!is.null(schema_list$col_names[[v]])) {
        row[["col_names"]] <- paste(schema_list$col_names[[v]], collapse = ",")
      } else {
        row[["col_names"]] <- NA_character_
      }

      # unique constraint
      if (!is.null(schema_list$unique) && v %in% schema_list$unique) {
        row$unique <- "TRUE"
      } else {
        row$unique <- NA_character_
      }

      # question_type (xlsform/odk question type)
      if (!is.null(schema_list$question_types[[v]])) {
        row$question_type <- schema_list$question_types[[v]]
      } else {
        row$question_type <- NA_character_
      }

      # is_other (flag for "other" open-ended text columns)
      if (!is.null(schema_list$is_other[[v]])) {
        row$is_other <- ifelse(isTRUE(schema_list$is_other[[v]]), "TRUE", "FALSE")
      } else {
        row$is_other <- NA_character_
      }

      # other_column_link (corresponding main column for "other" columns)
      if (!is.null(schema_list$other_column_link[[v]])) {
        if (is.character(schema_list$other_column_link[[v]])) {
          row$other_column_link <- paste(schema_list$other_column_link[[v]], collapse = ",")
        } else {
          row$other_column_link <- as.character(schema_list$other_column_link[[v]])
        }
      } else {
        row$other_column_link <- NA_character_
      }

      # variable_label columns
      for (lang in c("en", "fr", "ar")) {
        col <- paste0("variable_label_", lang)
        lbl <- (schema_list$variable_labels %||% list())[[lang]][[v]]
        row[[col]] <- if (!is.null(lbl)) as.character(lbl) else NA_character_
      }

      # value_label columns (NA for rows without a canonical value)
      for (lang in c("en", "fr", "ar")) {
        row[[paste0("value_label_", lang)]] <- NA_character_
      }

      out <- append(out, list(row))
    }
  }

  # 3. DEPENDENCY ROWS (parse-safe)
  deps <- schema_list$dependencies %||% list()
  if (length(deps) > 0) {
    # Iterate over named list
    for (dep_name in names(deps)) {
      d <- deps[[dep_name]]
      row <- empty_row()
      row[["rule_type"]] <- "dependency"
      row[["variable"]]  <- NA_character_
      row[["variables"]] <- paste(d[["variables"]], collapse = ",")

      row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
      row[["then"]]        <- d[["then"]]         %||% NA_character_

      # Extract dep_group from the name (remove flag_ prefix if present)
      if (grepl("^flag_", dep_name)) {
        row[["dep_group"]] <- sub("^flag_", "", dep_name)
      } else if (grepl("^dq_dep_", dep_name)) {
        # For unnamed deps, leave dep_group as NA
        row[["dep_group"]] <- NA_character_
      } else {
        row[["dep_group"]] <- dep_name
      }

      # action is optional; store any provided action value
      row[["action"]]      <- d[["action"]]       %||% NA_character_

      out <- append(out, list(row))
    }
  }

  # 4. SOFT_DEPENDENCY ROWS (parse-safe)
  soft <- schema_list$soft_dependencies %||% list()
  if (length(soft) > 0) {
    # Iterate over named list
    for (dep_name in names(soft)) {
      d <- soft[[dep_name]]
      row <- empty_row()
      row[["rule_type"]] <- "soft_dependency"
      row[["variable"]]  <- ""
      row[["variables"]] <- paste(d[["variables"]], collapse = ",")

      row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
      row[["then"]]        <- d[["then"]]         %||% NA_character_

      # Extract dep_group from the name (remove flag_ prefix if present)
      if (grepl("^flag_", dep_name)) {
        row[["dep_group"]] <- sub("^flag_", "", dep_name)
      } else if (grepl("^dq_soft_dep_", dep_name)) {
        # For unnamed soft deps, leave dep_group as NA
        row[["dep_group"]] <- NA_character_
      } else {
        row[["dep_group"]] <- dep_name
      }

      # action is optional; store any provided action value
      row[["action"]]      <- d[["action"]]       %||% NA_character_

      out <- append(out, list(row))
    }
  }

  # FINAL OUTPUT
  tibble::as_tibble(do.call(
    rbind,
    lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  ))
}


#' Helper function to safely convert boolean values from Excel/CSV
#'
#' Handles boolean values that may come in different formats when reading from
#' external sources like Excel files. Excel and readxl may convert TRUE/FALSE
#' to logical, string, or numeric types depending on the cell format.
#'
#' @param val A value to convert to boolean. Can be:
#'   - logical: TRUE/FALSE
#'   - character: "TRUE", "true", "True", "T", "1" (treated as TRUE)
#'   - numeric: 1 (TRUE), 0 (FALSE)
#'   - NA (treated as FALSE)
#'
#' @return Logical TRUE or FALSE
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' .safe_bool(TRUE)        # TRUE
#' .safe_bool("TRUE")      # TRUE
#' .safe_bool("true")      # TRUE
#' .safe_bool(1)           # TRUE
#' .safe_bool(FALSE)       # FALSE
#' .safe_bool("FALSE")     # FALSE
#' .safe_bool(0)           # FALSE
#' .safe_bool(NA)          # FALSE
#' .safe_bool("")          # FALSE
#' }
.safe_bool <- function(val) {
  # Check for NA first (before any other operations)
  if (is.na(val)) {
    return(FALSE)
  }
  # Check for empty string or "NA" string
  if (is.character(val) && (val == "" || val == "NA")) {
    return(FALSE)
  }
  # Handle logical values directly
  if (is.logical(val)) {
    return(isTRUE(val))
  }
  # Handle string values
  if (is.character(val)) {
    return(val == "TRUE" || val == "true" || val == "True" || val == "T" || val == "1")
  }
  # Handle numeric values (1 = TRUE, 0 or other values = FALSE)
  # Note: Only 1 is treated as TRUE to match Excel's boolean cell conversion (1=TRUE, 0=FALSE)
  # Other numeric values are explicitly treated as FALSE for safety
  if (is.numeric(val)) {
    return(val == 1)
  }
  return(FALSE)
}

#' Convert a schema table into a canonical nested schema list
#' @param df A data frame or tibble in the flat schema table format (as produced by
#'   \code{data_schema_to_table()}).
#' @export
data_table_to_schema <- function(df) {

  phrutils::phr_validate_dataframe(df, origin = "data_table_to_schema", soft = FALSE)

  # Validate the table format
  data_validate_table_to_schema(df)

  # Canonical schema object
  schema <- list(
    required          = character(0),
    types             = list(),
    allowed_values    = list(),
    value_map         = list(),
    col_names         = list(),
    unique            = character(0),
    question_types    = list(),
    is_other          = list(),
    other_column_link = list(),
    variable_labels   = list(en = list(), fr = list(), ar = list()),
    value_labels      = list(en = list(), fr = list(), ar = list())
  )

  # ------------------------------------------
  # VARIABLE ROWS
  # ------------------------------------------

  vars_df <- df[df$rule_type == "variable", ]

  for (i in seq_len(nrow(vars_df))) {
    row <- vars_df[i, ]

    v <- row$variable
    if (v == "" || is.na(v)) next

    # Check if this row has a value (canonical value mapping)
    has_value <- "value" %in% names(row) && !is.na(row$value) && row$value != ""

    # required (only set once per variable, not per value)
    if (isTRUE(row$required)) {
      schema$required <- unique(c(schema$required, v))
    }

    # type (only set once per variable, not per value)
    if (!is.na(row$type) && row$type != "") {
      schema$types[[v]] <- row$type
    }

    if (has_value) {
      # This row maps a canonical value to dataset values
      canonical_val <- row$value

      # Parse allowed values for this canonical value
      if (!is.na(row$allowed) && row$allowed != "") {
        dataset_vals <- trimws(strsplit(row$allowed, ",")[[1]])

        # Initialize value_map for this variable if needed
        if (is.null(schema$value_map[[v]])) {
          schema$value_map[[v]] <- list()
        }

        # Store the mapping: canonical_value -> dataset_values
        schema$value_map[[v]][[canonical_val]] <- dataset_vals
      }
    } else {
      # No value column or value is NA - use old allowed_values approach
      if (!is.na(row$allowed) && row$allowed != "") {
        schema$allowed_values[[v]] <- trimws(strsplit(row$allowed, ",")[[1]])
      }
    }

    # col_names (possible column name alternatives for auto-mapping)
    # Only set once per variable
    if ("col_names" %in% names(row) && !is.na(row$col_names) && row$col_names != "") {
      schema$col_names[[v]] <- trimws(strsplit(row$col_names, ",")[[1]])
    }

    # unique (only set once per variable)
    if (.safe_bool(row$unique)) {
      schema$unique <- unique(c(schema$unique, v))
    }

    # metadata (only set once per variable)
    if (is.null(schema$variables[[v]])) {
      schema$variables[[v]] <- list(
        required = isTRUE(row$required),
        type     = row$type,
        label    = row$label %||% "",
        comment  = row$comment %||% ""
      )
    }

    # question_type (xlsform/odk question type) (only set once per variable)
    if ("question_type" %in% names(row) && !is.na(row$question_type) && row$question_type != "") {
      schema$question_types[[v]] <- row$question_type
    }

    # is_other (flag for "other" columns) (only set once per variable)
    if ("is_other" %in% names(row)) {
      schema$is_other[[v]] <- .safe_bool(row$is_other)
    }

    # other_column_link (corresponding main column for "other" columns) (only set once per variable)
    if ("other_column_link" %in% names(row) && !is.na(row$other_column_link) && row$other_column_link != "") {
      schema$other_column_link[[v]] <- trimws(strsplit(row$other_column_link, ",")[[1]])
    }

    # variable_label columns (store once per variable; first non-NA occurrence wins)
    for (lang in c("en", "fr", "ar")) {
      col <- paste0("variable_label_", lang)
      if (col %in% names(row) && !is.null(row[[col]]) && !is.na(row[[col]]) && row[[col]] != "") {
        if (is.null(schema$variable_labels[[lang]][[v]])) {
          schema$variable_labels[[lang]][[v]] <- as.character(row[[col]])
        }
      }
    }

    # value_label columns (store per canonical value; only when has_value is TRUE)
    if (has_value) {
      canonical_val <- row$value
      for (lang in c("en", "fr", "ar")) {
        col <- paste0("value_label_", lang)
        if (col %in% names(row) && !is.null(row[[col]]) && !is.na(row[[col]]) && row[[col]] != "") {
          if (is.null(schema$value_labels[[lang]][[v]])) {
            schema$value_labels[[lang]][[v]] <- character(0)
          }
          schema$value_labels[[lang]][[v]][[canonical_val]] <- as.character(row[[col]])
        }
      }
    }
  }

  schema
}

#' Validate whether a schema table is safe to convert
#' @param df A data frame or tibble in the flat schema table format to validate.
#' @param data_obj Optional data object used for additional contextual validation. Default: NULL.
#' @export
data_validate_table_to_schema <- function(df, data_obj = NULL) {

  phrutils::phr_try({

    # Validate dataframe
    phrutils::phr_validate_dataframe(df, origin = "data_validate_table_to_schema", soft = FALSE)

    # Fixed required columns - now includes "value" column
    required_cols <- c(
      "rule_type","variable","value","required","type","allowed",
      "col_names","unique",
      "label","comment",
      "question_type","is_other","other_column_link"
    )

    phrutils::phr_validate_columns(
      df,
      required_cols = required_cols,
      origin = "data_validate_table_to_schema",
      hint   = "Schema table missing required columns.",
      soft   = FALSE
    )

    # rule_type check
    valid_rules <- c("variable","dependency","soft_dependency")
    bad <- setdiff(unique(df$rule_type), valid_rules)
    if (length(bad) > 0) {
      phr_error("data_validate_table_to_schema",
                  phr_txt("Invalid rule_type values: {paste(bad, collapse=', ')}."))
    }

    # duplicate (variable, value) pairs should warn
    # Note: duplicate variables are now OK if they have different values
    vars_only <- df[df$rule_type == "variable", ]

    # Create a key combining variable and value
    # Handle cases where value might be NA (for variables without value mappings)
    # Use a separator unlikely to appear in variable names or values
    var_value_key <- paste0(
      vars_only$variable,
      "|||",  # Triple pipe as separator to avoid conflicts
      ifelse(is.na(vars_only$value) | vars_only$value == "", "NA", vars_only$value)
    )

    dups <- var_value_key[duplicated(var_value_key)]
    if (length(dups) > 0) {
      phrutils::phr_warning(
        origin  = "data_validate_table_to_schema",
        message = phr_txt("Duplicate (variable, value) rows detected: {paste(unique(dups), collapse=', ')}."),
        hint    = "Only the last row per (variable, value) pair will be retained."
      )
    }

    TRUE
  },
  on_error = "abort",
  origin   = "data_validate_table_to_schema")
}

#' Validate schema_list structure for data_schema_to_table()
#' @keywords internal
data_validate_schema_to_table <- function(schema_list, origin = "schema") {

  # ------------------------------------------
  # 0. BASIC STRUCTURE
  # ------------------------------------------
  if (is.null(schema_list) || !is.list(schema_list)) {
    phr_error(origin, "Schema must be a list object.")
  }

  # required
  required <- schema_list$required %||% character(0)
  if (!is.character(required)) {
    phr_error(origin, "`required` must be a character vector.")
  }

  # types
  types <- schema_list$types %||% list()
  if (!is.list(types)) {
    phr_error(origin, "`types` must be a list.")
  }

  # must be a *named* list
  if (length(types) > 0 && (is.null(names(types)) || any(names(types) == ""))) {
    phr_error(origin, "`types` must be a named list.")
  }

  # allowed
  allowed <- schema_list$allowed_values %||% list()
  if (!is.list(allowed)) {
    phr_error(origin, "`allowed_values` must be a list.")
  }

  # col_names (possible column name alternatives for auto-mapping)
  col_names <- schema_list$col_names %||% list()
  if (!is.list(col_names)) {
    phr_error(origin, "`col_names` must be a list.")
  }

  # patterns
  patterns <- schema_list$patterns %||% list()
  if (!is.list(patterns)) {
    phr_error(origin, "`patterns` must be a list.")
  }

  # must be a *named* list
  if (length(patterns) > 0 && (is.null(names(patterns)) || any(names(patterns) == ""))) {
    phr_error(origin, "`patterns` must be a named list.")
  }

  # ranges
  ranges <- schema_list$ranges %||% list()
  if (!is.list(ranges)) {
    phr_error(origin, "`ranges` must be a list.")
  }

  # precision limits
  precision <- schema_list$precision_limits %||% list()
  if (!is.list(precision)) {
    phr_error(origin, "`precision_limits` must be a list.")
  }

  # ------------------------------------------
  # 1. VALIDATE TYPES (restored purrr loop)
  # ------------------------------------------

  valid_type_strings <- c(
    "numeric", "integer", "double",
    "character",
    "logical",
    "factor",
    "date", "Date", "POSIXct", "POSIXlt", "datetime"
  )

  purrr::iwalk(types, function(val, varname) {

    if (!is.atomic(val) ||
        !is.character(val) ||
        length(val) != 1 ||
        !(val %in% valid_type_strings)) {

      bad_val <- paste0(capture.output(str(val)), collapse = " ")

      phr_error(
        origin  = origin,
        message = phr_txt("Invalid type entry for variable '{varname}'."),
        hint    = phr_txt(
          "Expected one of: {paste(valid_type_strings, collapse=', ')}. Received: {bad_val}"
        )
      )
    }
  })


  # ------------------------------------------
  # 2. VALIDATE ALLOWED VALUES
  # ------------------------------------------

  purrr::iwalk(allowed, function(val, varname) {
    if (!is.atomic(val)) {
      bad_val <- paste0(capture.output(str(val)), collapse = " ")
      phr_error(
        origin  = origin,
        message = phr_txt("Allowed values for '{varname}' must be atomic."),
        hint    = phr_txt("Received: {bad_val}")
      )
    }
  })


  # ------------------------------------------
  # 2.4 VALIDATE VALUE_MAP (nested list structure)
  # ------------------------------------------

  value_map <- schema_list$value_map %||% list()
  if (!is.list(value_map)) {
    phr_error(origin, "`value_map` must be a list.")
  }

  # value_map must be a named list where each entry is also a named list
  if (length(value_map) > 0) {
    if (is.null(names(value_map)) || any(names(value_map) == "")) {
      phr_error(origin, "`value_map` must be a named list (variable names).")
    }

    # Each variable's value_map must be a named list
    purrr::iwalk(value_map, function(val_mappings, varname) {
      if (!is.list(val_mappings)) {
        phr_error(
          origin  = origin,
          message = phr_txt("value_map for '{varname}' must be a list."),
          hint    = phr_txt("Expected: list(canonical_val = c(dataset_vals, ...))")
        )
      }

      if (is.null(names(val_mappings)) || any(names(val_mappings) == "")) {
        phr_error(
          origin  = origin,
          message = phr_txt("value_map for '{varname}' must be a named list (canonical values)."),
          hint    = phr_txt("Expected: list(canonical_val = c(dataset_vals, ...))")
        )
      }

      # Each canonical value must map to an atomic vector
      purrr::iwalk(val_mappings, function(dataset_vals, canonical_val) {
        if (!is.atomic(dataset_vals)) {
          bad_val <- paste0(capture.output(str(dataset_vals)), collapse = " ")
          phr_error(
            origin  = origin,
            message = phr_txt("value_map['{varname}']['{canonical_val}'] must be atomic."),
            hint    = phr_txt("Received: {bad_val}")
          )
        }
      })
    })
  }



  # 2.5 VALIDATE COL_NAMES (must be character vector)


  purrr::iwalk(col_names, function(val, varname) {
    if (!is.atomic(val) || !is.character(val)) {
      bad_val <- paste0(capture.output(str(val)), collapse = " ")
      phr_error(
        origin  = origin,
        message = phr_txt("col_names for '{varname}' must be a character vector."),
        hint    = phr_txt("Received: {bad_val}")
      )
    }
  })



  # 3. VALIDATE PATTERNS - now optional (moved to dependency_schema)
  # Keep validation only if patterns are present for backward compatibility

  patterns <- schema_list$patterns %||% list()
  if (length(patterns) > 0) {
    purrr::iwalk(patterns, function(val, varname) {

      if (!is.atomic(val) || !is.character(val) || length(val) != 1) {

        bad_val <- paste0(capture.output(str(val)), collapse = " ")

        phrutils::phr_warning(
          origin  = origin,
          message = phr_txt("Pattern for '{varname}' should be a single regex string. Note: patterns have moved to dependency_schema."),
          hint    = phr_txt("Received: {bad_val}")
        )
      }
    })
  }



  # 4. VALIDATE RANGES (must be length 2 numeric)


  purrr::iwalk(ranges, function(val, varname) {

    if (!is.atomic(val) ||
        length(val) != 2 ||
        !all(suppressWarnings(!is.na(as.numeric(val))))) {

      bad_val <- paste0(capture.output(str(val)), collapse = " ")

      phr_error(
        origin  = origin,
        message = phr_txt("Range for '{varname}' must be numeric of length 2 (min, max)."),
        hint    = phr_txt("Received: {bad_val}")
      )
    }
  })



  # 5. VALIDATE PRECISION LIMITS (must be numeric of length 1)


  purrr::iwalk(precision, function(val, varname) {

    if (!is.atomic(val) ||
        length(val) != 1 ||
        suppressWarnings(is.na(as.numeric(val)))) {

      bad_val <- paste0(capture.output(str(val)), collapse = " ")

      phr_error(
        origin  = origin,
        message = phr_txt("Precision limit for '{varname}' must be a numeric value."),
        hint    = phr_txt("Received: {bad_val}")
      )
    }
  })


  # 6. VALIDATE VARIABLE_LABELS (optional; must be a named list of named char lists per language)

  variable_labels <- schema_list$variable_labels %||% list()
  if (length(variable_labels) > 0) {
    if (!is.list(variable_labels)) {
      phr_error(origin, "`variable_labels` must be a named list keyed by language code (en/fr/ar).")
    }
    valid_langs <- c("en", "fr", "ar")
    bad_langs <- setdiff(names(variable_labels), valid_langs)
    if (length(bad_langs) > 0) {
      phrutils::phr_warning(
        origin  = origin,
        message = phr_txt("Unexpected language keys in `variable_labels`: {paste(bad_langs, collapse=', ')}."),
        hint    = phr_txt("Expected keys: {paste(valid_langs, collapse=', ')}.")
      )
    }
    purrr::iwalk(variable_labels, function(lang_labels, lang) {
      if (!is.list(lang_labels)) {
        phr_error(origin, phr_txt("`variable_labels${lang}` must be a named list."))
      }
      purrr::iwalk(lang_labels, function(lbl, varname) {
        if (!is.character(lbl) || length(lbl) != 1) {
          phr_error(
            origin  = origin,
            message = phr_txt("variable_labels${lang}${varname} must be a single character string.")
          )
        }
      })
    })
  }


  # 7. VALIDATE VALUE_LABELS (optional; must be a named list of named char vectors per language)

  schema_value_labels <- schema_list$value_labels %||% list()
  if (length(schema_value_labels) > 0) {
    if (!is.list(schema_value_labels)) {
      phr_error(origin, "`value_labels` in schema must be a named list keyed by language code (en/fr/ar).")
    }
    purrr::iwalk(schema_value_labels, function(lang_labels, lang) {
      if (!is.list(lang_labels)) {
        phr_error(origin, phr_txt("`value_labels${lang}` must be a named list."))
      }
      purrr::iwalk(lang_labels, function(val_labels, varname) {
        if (!is.character(val_labels)) {
          phr_error(
            origin  = origin,
            message = phr_txt("value_labels${lang}${varname} must be a named character vector.")
          )
        }
      })
    })
  }

  invisible(TRUE)
}


#' Expand select_multiple column into dummy variables
#'
#' @description
#' Takes a select_multiple column (with space-separated values) and expands it
#' into multiple dummy columns, one for each unique response option found.
#'
#' @param column A character vector containing space-separated values
#' @param column_name The name of the original column (used for naming dummies)
#' @param separator The separator used in the select_multiple values (default: " ")
#'
#' @return A data frame with dummy columns named \{column_name\}_\{value\} containing 1
#'   if value present, 0 if not. Returns empty data frame (0 rows, 0 columns) if no
#'   valid values found.
#'
#' @keywords internal
expand_select_multiple <- function(column, column_name, separator = " ") {

  # Handle NA and empty values
  column <- ifelse(is.na(column) | column == "", NA_character_, column)

  # Split each cell by separator and get unique values across all rows
  all_values <- unlist(strsplit(column[!is.na(column)], separator, fixed = TRUE))
  all_values <- trimws(all_values)
  all_values <- all_values[all_values != ""]
  unique_values <- sort(unique(all_values))

  # If no unique values found, return empty data frame
  if (length(unique_values) == 0) {
    return(data.frame())
  }

  # Create dummy columns
  dummy_df <- data.frame(matrix(0, nrow = length(column), ncol = length(unique_values)))
  colnames(dummy_df) <- paste0(column_name, ".", unique_values)

  # Fill in 1s where values are present
  for (i in seq_along(column)) {
    if (!is.na(column[i]) && column[i] != "") {
      values_in_row <- trimws(strsplit(column[i], separator, fixed = TRUE)[[1]])
      values_in_row <- values_in_row[values_in_row != ""]

      for (val in values_in_row) {
        if (val %in% unique_values) {
          col_idx <- which(unique_values == val)
          dummy_df[i, col_idx] <- 1
        }
      }
    }
  }

  return(dummy_df)
}


#' Identify and expand all select_multiple columns in a dataset
#'
#' @description
#' Helper function that processes a dataset based on schema question_types,
#' identifying select_multiple columns and expanding them into dummy variables.
#'
#' @param data A data frame to process
#' @param schema A schema list with question_types field (can be NULL or without
#'   question_types, in which case the function returns the original data unchanged)
#'
#' @return A list with 'data' (modified data frame), 'expanded_columns' (character
#'   vector of new column names), and 'other_related_columns' (list tracking columns
#'   related to "other" responses in select_multiple questions)
#'
#' @keywords internal
process_select_multiple_columns <- function(data, schema) {

  result <- list(
    data = data,
    expanded_columns = character(0),
    other_related_columns = list()
  )

  # Check if schema has question_types
  if (is.null(schema) || is.null(schema$question_types) || length(schema$question_types) == 0) {
    return(result)
  }

  # Find select_multiple columns
  question_types <- schema$question_types
  select_mult_vars <- names(question_types)[question_types == "select_multiple"]

  if (length(select_mult_vars) == 0) {
    return(result)
  }

  # Process each select_multiple column
  for (var in select_mult_vars) {
    if (var %in% names(data)) {

      # Expand the column
      dummy_df <- expand_select_multiple(data[[var]], var)

      if (ncol(dummy_df) > 0) {
        # Add dummy columns to the data
        result$data <- cbind(result$data, dummy_df)
        result$expanded_columns <- c(result$expanded_columns, colnames(dummy_df))

        # Check if "other" is one of the values in this select_multiple
        other_dummy_col <- paste0(var, ".other")
        if (other_dummy_col %in% colnames(dummy_df)) {

          # Look for a corresponding open text "other" column
          # Common naming patterns: var_other_text, var_other_specify, var_other_value
          potential_other_text_cols <- c(
            paste0(var, "_other_text"),
            paste0(var, "_other_specify"),
            paste0(var, "_other"),
            paste0(var, "_autre"),
            paste0(var, "_text")
          )

          # Find which one exists in the data
          other_text_col <- NULL
          for (potential_col in potential_other_text_cols) {
            if (potential_col %in% names(result$data)) {
              other_text_col <- potential_col
              break
            }
          }

          # Track all three columns related to "other" response
          result$other_related_columns[[var]] <- list(
            original_column = var,
            dummy_other_column = other_dummy_col,
            text_other_column = other_text_col  # May be NULL if not found
          )
        }
      }
    }
  }

  return(result)
}


# INDICATOR SCHEMA FUNCTIONS ####

#' Convert an indicator schema list into a flat table format
#' @param indicator_schema_list A named list representing the canonical indicator schema, as produced by
#'   \code{indicator_table_to_schema()} or constructed manually.
#' @export
indicator_schema_to_table <- function(indicator_schema_list) {

  # Validate input
  if (is.null(indicator_schema_list) || !is.list(indicator_schema_list)) {
    phr_error("indicator_schema_to_table", "Indicator schema must be a list object.")
  }

  # Fixed column set for indicator schema export
  fixed_cols <- c(
    "indicator_name",
    "function_name",
    "variables",
    "arguments",
    "label",
    "comment"
  )

  # Helper to return a fully NA row
  empty_row <- function() {
    setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
  }

  out <- list()

  # Process each indicator
  if (length(indicator_schema_list) > 0) {
    for (ind_name in names(indicator_schema_list)) {
      ind <- indicator_schema_list[[ind_name]]
      row <- empty_row()

      row[["indicator_name"]] <- ind[["indicator_name"]] %||% ind_name
      row[["function_name"]]  <- ind[["function_name"]] %||% NA_character_
      row[["variables"]]      <- paste(ind[["variables"]] %||% character(0), collapse = ",")
      row[["label"]]          <- ind[["label"]] %||% NA_character_
      row[["comment"]]        <- ind[["comment"]] %||% NA_character_

      # Convert arguments list to string format "arg1=value1,arg2=value2"
      args <- ind[["arguments"]] %||% list()
      if (length(args) > 0) {
        arg_strings <- mapply(
          function(name, value) paste0(name, "=", value),
          names(args),
          args,
          SIMPLIFY = TRUE
        )
        row[["arguments"]] <- paste(arg_strings, collapse = ",")
      } else {
        row[["arguments"]] <- NA_character_
      }

      out <- append(out, list(row))
    }
  }

  # Return as tibble
  if (length(out) == 0) {
    # Return empty tibble with correct structure
    return(tibble::tibble(
      indicator_name = character(0),
      function_name = character(0),
      variables = character(0),
      arguments = character(0),
      label = character(0),
      comment = character(0)
    ))
  }

  tibble::as_tibble(do.call(
    rbind,
    lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  ))
}


#' Parse indicator arguments string respecting parentheses and quoted strings
#' 
#' Internal helper to split argument strings like "arg1=val1,arg2=c(a,b,c),arg3=val3"
#' properly by respecting parentheses and quoted strings so that c(...) vectors
#' and quoted values containing commas are not split incorrectly.
#' 
#' This function is intentionally called twice during indicator processing:
#' 1. First call: Parse the full argument string into individual argument pairs
#' 2. Second call: Parse the content inside c(...) vectors into individual elements
#' This two-stage parsing is necessary because arguments and vector elements have
#' different semantic meanings and resolution rules.
#' 
#' @param arg_string Character string containing arguments
#' @return Character vector of argument pairs
#' @keywords internal
.parse_indicator_arguments <- function(arg_string) {
  if (is.null(arg_string) || is.na(arg_string) || arg_string == "") {
    return(character(0))
  }
  
  result      <- character(0)
  current_arg <- ""
  paren_depth <- 0L
  in_quote    <- FALSE
  quote_char  <- ""
  chars       <- strsplit(arg_string, "")[[1]]
  
  for (char in chars) {
    if (in_quote) {
      current_arg <- paste0(current_arg, char)
      if (char == quote_char) {
        in_quote   <- FALSE
        quote_char <- ""
      }
    } else if (char %in% c('"', "'")) {
      in_quote   <- TRUE
      quote_char <- char
      current_arg <- paste0(current_arg, char)
    } else if (char == "(") {
      paren_depth <- paren_depth + 1L
      current_arg <- paste0(current_arg, char)
    } else if (char == ")") {
      paren_depth <- paren_depth - 1L
      current_arg <- paste0(current_arg, char)
    } else if (char == "," && paren_depth == 0L) {
      # This is a separator between arguments, not within a vector or quoted string
      if (nchar(current_arg) > 0) {
        result <- c(result, trimws(current_arg))
        current_arg <- ""
      }
    } else {
      current_arg <- paste0(current_arg, char)
    }
  }
  
  # Add the last argument
  if (nchar(current_arg) > 0) {
    result <- c(result, trimws(current_arg))
  }
  
  result
}


#' Convert an indicator schema table into a canonical nested list
#' @param df A data frame or tibble in the flat indicator schema table format (as produced by
#'   \code{indicator_schema_to_table()}).
#' @export
indicator_table_to_schema <- function(df) {

  phrutils::phr_validate_dataframe(df, origin = "indicator_table_to_schema", soft = FALSE)

  # Validate the table format
  indicator_validate_table_to_schema(df)

  # Canonical indicator schema object
  indicator_schema <- list()

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    # Get the indicator name
    indicator_name <- row[["indicator_name"]]
    if (is.na(indicator_name) || indicator_name == "" || indicator_name == "NA") {
      # Skip indicators without a name
      next
    }

    # Get function name
    function_name <- row[["function_name"]] %||% ""

    # Parse variables (comma-separated list)
    variables <- character(0)
    if (!is.na(row[["variables"]]) && row[["variables"]] != "") {
      variables <- trimws(strsplit(row[["variables"]], ",")[[1]])
    }

    # Parse arguments (stored as "arg1=value1,arg2=value2" format)
    # Handle vectors like c(val1,val2,val3) by respecting parentheses
    arguments <- list()
    if (!is.na(row[["arguments"]]) && row[["arguments"]] != "") {
      arg_string <- row[["arguments"]]
      arg_pairs <- .parse_indicator_arguments(arg_string)

      for (pair in arg_pairs) {
        if (grepl("=", pair)) {
          parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
          if (length(parts) == 2) {
            arg_name <- trimws(parts[1])
            arg_value <- trimws(parts[2])
            arguments[[arg_name]] <- arg_value
          }
        }
      }
    }

    # Get label and comment
    label <- row[["label"]]
    if (is.na(label) || label == "NA") label <- NULL

    comment <- row[["comment"]]
    if (is.na(comment) || comment == "NA") comment <- NULL

    # Create indicator object
    ind_obj <- list(
      indicator_name = indicator_name,
      function_name  = function_name,
      variables      = variables,
      arguments      = arguments
    )

    if (!is.null(label)) ind_obj$label <- label
    if (!is.null(comment)) ind_obj$comment <- comment

    # Use indicator_name as the key
    indicator_schema[[indicator_name]] <- ind_obj
  }

  indicator_schema
}


#' Validate whether an indicator schema table is safe to convert
#' @param df A data frame or tibble in the flat indicator schema table format to validate.
#' @export
indicator_validate_table_to_schema <- function(df) {

  phrutils::phr_try({

    # Validate dataframe
    phrutils::phr_validate_dataframe(df, origin = "indicator_validate_table_to_schema", soft = FALSE)

    # Required columns
    required_cols <- c(
      "indicator_name",
      "function_name",
      "variables",
      "arguments",
      "label",
      "comment"
    )

    phrutils::phr_validate_columns(
      df,
      required_cols = required_cols,
      origin = "indicator_validate_table_to_schema",
      hint   = "Indicator schema table missing required columns.",
      soft   = FALSE
    )

    # Check for duplicate indicator names
    ind_names <- df$indicator_name[!is.na(df$indicator_name) & df$indicator_name != ""]
    dups <- ind_names[duplicated(ind_names)]
    if (length(dups) > 0) {
      phrutils::phr_warning(
        origin  = "indicator_validate_table_to_schema",
        message = phr_txt("Duplicate indicator names detected: {paste(unique(dups), collapse=', ')}."),
        hint    = "Only the last row per indicator name will be retained."
      )
    }

    TRUE
  },
  on_error = "abort",
  origin   = "indicator_validate_table_to_schema")
}


# DEPENDENCY SCHEMA FUNCTIONS ####

#' Convert a dependency schema list into a flat table format
#' @param dependency_schema_list A named list representing the canonical dependency schema, as produced by
#'   \code{dependency_table_to_schema()} or constructed manually.
#' @export
dependency_schema_to_table <- function(dependency_schema_list) {

  # 0. VALIDATE SCHEMA FIRST
  dependency_validate_schema_to_table(
    dependency_schema_list = dependency_schema_list,
    origin = "dependency_schema_to_table"
  )

  # Fixed column set for dependency schema export
  # Aligns with existing dependency rows from data_schema_to_table()
  fixed_cols <- c(
    "rule_type",
    "dep_name",
    "variables",
    "condition_if",
    "then",
    "action",
    "label",
    "comment"
  )

  # Helper to return a fully NA row
  empty_row <- function() {
    setNames(as.list(rep(NA_character_, length(fixed_cols))), fixed_cols)
  }

  out <- list()

  # Process dependencies (hard dependencies)
  deps <- dependency_schema_list$dependencies %||% list()
  if (length(deps) > 0) {
    for (dep_name in names(deps)) {
      d <- deps[[dep_name]]
      row <- empty_row()

      row[["rule_type"]]    <- "dependency"
      row[["dep_name"]]     <- dep_name
      row[["variables"]]    <- paste(d[["variables"]] %||% character(0), collapse = ",")
      row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
      row[["then"]]         <- d[["then"]] %||% NA_character_
      row[["action"]]       <- d[["action"]] %||% NA_character_
      row[["label"]]        <- d[["label"]] %||% NA_character_
      row[["comment"]]      <- d[["comment"]] %||% NA_character_

      out <- append(out, list(row))
    }
  }

  # Process soft_dependencies
  soft <- dependency_schema_list$soft_dependencies %||% list()
  if (length(soft) > 0) {
    for (dep_name in names(soft)) {
      d <- soft[[dep_name]]
      row <- empty_row()

      row[["rule_type"]]    <- "soft_dependency"
      row[["dep_name"]]     <- dep_name
      row[["variables"]]    <- paste(d[["variables"]] %||% character(0), collapse = ",")
      row[["condition_if"]] <- d[["condition_if"]] %||% NA_character_
      row[["then"]]         <- d[["then"]] %||% NA_character_
      row[["action"]]       <- d[["action"]] %||% NA_character_
      row[["label"]]        <- d[["label"]] %||% NA_character_
      row[["comment"]]      <- d[["comment"]] %||% NA_character_

      out <- append(out, list(row))
    }
  }

  # Return as tibble
  if (length(out) == 0) {
    # Return empty tibble with correct structure
    return(tibble::tibble(
      rule_type = character(0),
      dep_name = character(0),
      variables = character(0),
      condition_if = character(0),
      then = character(0),
      action = character(0),
      label = character(0),
      comment = character(0)
    ))
  }

  tibble::as_tibble(do.call(
    rbind,
    lapply(out, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  ))
}


#' Convert a dependency schema table into a canonical nested list
#' @param df A data frame or tibble in the flat dependency schema table format (as produced by
#'   \code{dependency_schema_to_table()}).
#' @export
dependency_table_to_schema <- function(df) {

  phrutils::phr_validate_dataframe(df, origin = "dependency_table_to_schema", soft = FALSE)

  # Validate the table format
  dependency_validate_table_to_schema(df)

  # Canonical dependency schema object
  dependency_schema <- list(
    dependencies = list(),
    soft_dependencies = list()
  )

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    rule_type <- row[["rule_type"]]
    dep_name <- row[["dep_name"]]

    # Skip rows without a name
    if (is.na(dep_name) || dep_name == "" || dep_name == "NA") {
      next
    }

    # Parse variables (comma-separated list)
    variables <- character(0)
    if (!is.na(row[["variables"]]) && row[["variables"]] != "") {
      variables <- trimws(strsplit(row[["variables"]], ",")[[1]])
    }

    # Get condition_if and then
    condition_if <- row[["condition_if"]]
    if (is.na(condition_if) || condition_if == "NA") condition_if <- ""

    then_cond <- row[["then"]]
    if (is.na(then_cond) || then_cond == "NA") then_cond <- ""

    # Get action
    action <- row[["action"]]
    if (is.na(action) || action == "NA") action <- ""

    # Get label and comment
    label <- row[["label"]]
    if (is.na(label) || label == "NA") label <- NULL

    comment <- row[["comment"]]
    if (is.na(comment) || comment == "NA") comment <- NULL

    # Create dependency object
    dep_obj <- list(
      variables = variables,
      condition_if = condition_if,
      then = then_cond,
      action = action
    )

    if (!is.null(label)) dep_obj$label <- label
    if (!is.null(comment)) dep_obj$comment <- comment

    # Add to appropriate list based on rule_type
    if (rule_type == "dependency") {
      dependency_schema$dependencies[[dep_name]] <- dep_obj
    } else if (rule_type == "soft_dependency") {
      dependency_schema$soft_dependencies[[dep_name]] <- dep_obj
    }
  }

  dependency_schema
}


#' Validate whether a dependency schema table is safe to convert
#' @param df A data frame or tibble in the flat dependency schema table format to validate.
#' @export
dependency_validate_table_to_schema <- function(df) {

  phrutils::phr_try({

    # Validate dataframe
    phrutils::phr_validate_dataframe(df, origin = "dependency_validate_table_to_schema", soft = FALSE)

    # Required columns
    required_cols <- c(
      "rule_type",
      "dep_name",
      "variables",
      "condition_if",
      "then",
      "action",
      "label",
      "comment"
    )

    phrutils::phr_validate_columns(
      df,
      required_cols = required_cols,
      origin = "dependency_validate_table_to_schema",
      hint   = "Dependency schema table missing required columns.",
      soft   = FALSE
    )

    # rule_type check
    valid_rules <- c("dependency", "soft_dependency")
    bad <- setdiff(unique(df$rule_type), valid_rules)
    if (length(bad) > 0) {
      phr_error("dependency_validate_table_to_schema",
                  phr_txt("Invalid rule_type values: {paste(bad, collapse=', ')}. Expected: dependency or soft_dependency."))
    }

    # Check for duplicate dependency names within each rule type
    for (rule_type in c("dependency", "soft_dependency")) {
      rule_rows <- df[df$rule_type == rule_type, ]
      if (nrow(rule_rows) > 0) {
        dep_names <- rule_rows$dep_name[!is.na(rule_rows$dep_name) & rule_rows$dep_name != ""]
        dups <- dep_names[duplicated(dep_names)]
        if (length(dups) > 0) {
          phrutils::phr_warning(
            origin  = "dependency_validate_table_to_schema",
            message = phr_txt("Duplicate {rule_type} names detected: {paste(unique(dups), collapse=', ')}."),
            hint    = "Only the last row per dependency name will be retained."
          )
        }
      }
    }

    TRUE
  },
  on_error = "abort",
  origin   = "dependency_validate_table_to_schema")
}


#' Validate dependency_schema_list structure for dependency_schema_to_table()
#' @keywords internal
dependency_validate_schema_to_table <- function(dependency_schema_list, origin = "dependency_schema") {

  # ------------------------------------------
  # 0. BASIC STRUCTURE
  # ------------------------------------------
  if (is.null(dependency_schema_list) || !is.list(dependency_schema_list)) {
    phr_error(origin, "Dependency schema must be a list object.")
  }

  # dependencies
  deps <- dependency_schema_list$dependencies %||% list()
  if (!is.list(deps)) {
    phr_error(origin, "`dependencies` must be a list.")
  }

  # must be a *named* list if not empty
  if (length(deps) > 0 && (is.null(names(deps)) || any(names(deps) == ""))) {
    phr_error(origin, "`dependencies` must be a named list.")
  }

  # soft_dependencies
  soft <- dependency_schema_list$soft_dependencies %||% list()
  if (!is.list(soft)) {
    phr_error(origin, "`soft_dependencies` must be a list.")
  }

  # must be a *named* list if not empty
  if (length(soft) > 0 && (is.null(names(soft)) || any(names(soft) == ""))) {
    phr_error(origin, "`soft_dependencies` must be a named list.")
  }


  # ------------------------------------------
  # 1. VALIDATE DEPENDENCIES
  # ------------------------------------------

  validate_dependency <- function(d, dep_name) {

    if (!is.list(d)) {
      phr_error(origin, "Each dependency must be a list.")
    }

    # variables
    if (is.null(d[["variables"]]) || !is.character(d[["variables"]])) {
      phr_error(origin, phr_txt("Dependency '{dep_name}' must include character `variables`."))
    }

    # condition_if condition
    if (!is.null(d[["condition_if"]]) && !is.character(d[["condition_if"]])) {
      bad_val <- paste0(capture.output(str(d[["condition_if"]])), collapse = " ")
      phr_error(
        origin,
        phr_txt("Dependency '{dep_name}' `condition_if` must be a character string."),
        hint = phr_txt("Received: {bad_val}")
      )
    }

    # then condition
    if (!is.null(d[["then"]]) && !is.character(d[["then"]])) {
      bad_val <- paste0(capture.output(str(d[["then"]])), collapse = " ")
      phr_error(
        origin,
        phr_txt("Dependency '{dep_name}' `then` must be a character string."),
        hint = phr_txt("Received: {bad_val}")
      )
    }

    # action (optional)
    if (!is.null(d[["action"]]) && !is.character(d[["action"]])) {
      bad_val <- paste0(capture.output(str(d[["action"]])), collapse = " ")
      phr_error(
        origin,
        phr_txt("Dependency '{dep_name}' `action` must be a character string if provided."),
        hint = phr_txt("Received: {bad_val}")
      )
    }
  }

  # Validate all dependencies
  for (dep_name in names(deps)) {
    validate_dependency(deps[[dep_name]], dep_name)
  }

  # Validate all soft_dependencies
  for (dep_name in names(soft)) {
    validate_dependency(soft[[dep_name]], dep_name)
  }


  invisible(TRUE)
}


