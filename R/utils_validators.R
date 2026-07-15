#' Ensure a Value with a Default Fallback
#'
#' The `ensure_value` function ensures that a variable has a valid value.
#' If the input variable is `NULL` or an empty vector (e.g., `character(0)`),
#' it returns the specified default value. Otherwise, it returns the input value unchanged.
#'
#' @param value Any variable or expression to be checked.
#' @param default A default value to return if `value` is `NULL` or empty (`length(value) == 0`).
#'
#' @return Either the original `value` (if it's not `NULL` and not empty)
#' or the `default` value if `value` is `NULL` or empty.
#'
#' @examples
#' # Default value is returned when input is NULL
#' ensure_value(NULL, "default_value")  # Returns: "default_value"
#'
#' # Default value is returned when input is an empty vector
#' ensure_value(character(0), "default_value")  # Returns: "default_value"
#'
#' # Input value is returned when it is valid
#' ensure_value("valid_input", "default_value")  # Returns: "valid_input"
#'
#' # Usage inside a pipeline or similar defensive checks
#' some_var <- NULL
#' some_var <- ensure_value(some_var, "fallback_value")
#'
#' @export
ensure_value <- function(value, default) {
  if (is.null(value) || length(value) == 0) {
    default
  } else {
    value
  }
}

#' @title Check if a Select Multiple Input Contains Only Allowable Values
#'
#' @description This function checks if a `select_multiple` input (character or factor vector) contains only values included in a set of allowable values. If all values are valid, it returns `TRUE`; otherwise, it returns `FALSE`.
#'
#' @param variable A character or factor vector (representing the `select_multiple` input) to be checked.
#' @param allowable_values A character vector specifying the allowable values.
#'
#' @return A logical value: `TRUE` if all values are valid, `FALSE` if there are any invalid values.
#'
#' @details The function splits elements in the `variable` vector by whitespace and checks if each value belongs to the `allowable_values`. `NA` is considered valid and excluded from checks.
#'
#' @examples
#' variable <- c("option_a option_b", "option_c", "option_d option_e", NA)
#' allowable_values <- c("option_a", "option_b", "option_c")
#' .is_select_multiple_allowed(variable, allowable_values)
#' # Output: FALSE (because "option_d" and "option_e" are invalid)
#'
#' @noRd
.is_select_multiple_allowed <- function(variable, allowable_values) {
  origin <- "is_select_multiple_allowed"

  phr_try({
    # Validate `variable`
    phr_assert(
      is.vector(variable) && (is.character(variable) || is.factor(variable)),
      origin = origin,
      hint = phr_txt("`variable` must be a character or factor vector.")
    )

    # Validate `allowable_values`
    phr_assert(
      is.vector(allowable_values) && is.character(allowable_values),
      origin = origin,
      hint = phr_txt("`allowable_values` must be a character vector.")
    )

    # Convert `variable` to character if it's a factor
    variable <- as.character(variable)

    # Remove NA values from `allowable_values` for safe comparison
    allowable_values <- allowable_values[!is.na(allowable_values)]

    # Validate that `allowable_values` is not empty after removing NAs
    phr_assert(
      length(allowable_values) > 0,
      origin = origin,
      hint = phr_txt("`allowable_values` must contain at least one valid value.")
    )

    # Helper function to check if all values in a cell are valid
    check_element <- function(x) {
      if (is.na(x)) return(TRUE)  # NA is valid
      values <- unlist(strsplit(x, "\\s+"))  # Split by spaces
      all(values %in% allowable_values)  # Check if all values are valid
    }

    # Apply the helper function to the `variable`
    result <- all(sapply(variable, check_element))

    return(result)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure `variable` contains valid select multiple responses, and `allowable_values` defines the allowed options."))
}

#' @title Check if a Select Multiple Input has More Than One Selection
#'
#' @description This helper function checks if rows in a `select_multiple` column or a vector have more than one selection (response) by splitting responses based on whitespace delimiters.
#'
#' @param select_multiple_input A data frame, tibble, or a character vector containing the relevant column or values.
#' @param select_multiple_col A character string specifying the name of the `select_multiple` column to be checked (if a data frame is provided).
#'
#' @return A logical vector where `TRUE` indicates that a row or vector element has more than one selection and `FALSE` otherwise.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   select_multiple_column = c(
#'     "option_a option_b",
#'     "option_c",
#'     "option_d option_e option_f option_g",
#'     ""
#'   )
#' )
#'
#' # Check with a data frame
#' .is_greater_than_one_selection(df, "select_multiple_column")
#' # Output: TRUE, FALSE, TRUE, FALSE
#'
#' # Check with a vector
#' vec <- c("option_a option_b", "option_c", "option_d option_e option_f option_g", "")
#' .is_greater_than_one_selection(vec)
#' # Output: TRUE, FALSE, TRUE, FALSE
#'
#' @noRd
.is_greater_than_one_selection <- function(select_multiple_input, select_multiple_col = NULL) {
  origin <- "is_greater_than_one_selection"

  phr_try({
    # Handle input validation
    if (is.data.frame(select_multiple_input)) {
      # Validate column
      phr_validate_columns(
        select_multiple_input,
        select_multiple_col,
        origin = origin,
        hint = phr_txt("Ensure the column for testing multiple selections exists in the dataset."),
        soft = FALSE
      )

      select_multiple_input <- select_multiple_input[[select_multiple_col]]
    }

    # Ensure the input is a character or factor vector
    phr_assert(
      is.character(select_multiple_input) || is.factor(select_multiple_input),
      origin = origin,
      phr_txt("The input must be a character or factor vector.")
    )

    # Check condition: more than one selection
    result <- sapply(
      as.character(select_multiple_input),
      function(row) {
        length(strsplit(row, split = "\\s+")[[1]]) > 1
      }
    )

    return(result)
  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure the input contains valid select multiple responses delimited by spaces."))
}

#' @title Check if a Select Multiple Input has More Than Three Selections
#'
#' @description This helper function checks if rows in a `select_multiple` column or a vector have more than three selections (responses) by splitting responses based on whitespace delimiters.
#'
#' @param select_multiple_input A data frame, tibble, or a character vector containing the relevant column or values.
#' @param select_multiple_col A character string specifying the name of the `select_multiple` column to be checked (if a data frame is provided).
#'
#' @return A logical vector where `TRUE` indicates that a row or vector element has more than three selections and `FALSE` otherwise.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   select_multiple_column = c(
#'     "option_a option_b",
#'     "option_c",
#'     "option_d option_e option_f option_g",
#'     ""
#'   )
#' )
#'
#' # Check with a data frame
#' .is_greater_than_three_selection(df, "select_multiple_column")
#' # Output: FALSE, FALSE, TRUE, FALSE
#'
#' # Check with a vector
#' vec <- c("option_a option_b", "option_c", "option_d option_e option_f option_g", "")
#' .is_greater_than_three_selection(vec)
#' # Output: FALSE, FALSE, TRUE, FALSE
#'
#' @noRd
.is_greater_than_three_selection <- function(select_multiple_input, select_multiple_col = NULL) {
  origin <- "is_greater_than_three_selection"

  phr_try({
    # Handle input validation
    if (is.data.frame(select_multiple_input)) {
      # Validate column
      phr_validate_columns(
        select_multiple_input,
        select_multiple_col,
        origin = origin,
        hint = phr_txt("Ensure the column for testing multiple selections exists in the dataset."),
        soft = FALSE
      )

      select_multiple_input <- select_multiple_input[[select_multiple_col]]
    }

    # Ensure the input is a character or factor vector
    phr_assert(
      is.character(select_multiple_input) || is.factor(select_multiple_input),
      origin = origin,
      phr_txt("The input must be a character or factor vector.")
    )

    # Check condition: more than three selections
    result <- sapply(
      as.character(select_multiple_input),
      function(row) {
        length(strsplit(row, split = "\\s+")[[1]]) > 3
      }
    )

    return(result)
  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure the input contains valid select multiple responses delimited by spaces."))
}


#' Check whether an expression is a valid logical expression (static analysis)
#'
#' @description
#' `.is_logical_expression()` performs a **purely static (non-evaluating)**
#' inspection of an R expression to determine whether it represents a
#' syntactically valid logical statement.
#'
#' This function is intended for **schema- or configuration-time validation**
#' (e.g. validating user-supplied logical expressions that will later be
#' evaluated against a dataset). It does **not** evaluate the expression and
#' does not require referenced objects to exist.
#'
#' Supported constructs include:
#' \itemize{
#'   \item Logical operators: `&`, `|`, `!`, `&&`, `||`
#'   \item Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`, `%in%`
#'   \item Logical-returning functions: `is.na()`, `grepl()`, `.is_safely_coercible()`
#'   \item Base type check functions: `is.numeric()`, `is.character()`, `is.logical()`,
#'         `is.integer()`, `is.double()`, `is.factor()`, `is.list()`, `is.vector()`,
#'         `is.data.frame()`, `is.matrix()`, `is.null()`, `is.atomic()`, `is.recursive()`
#'   \item Symbols (e.g. dataset column names)
#'   \item Literal `TRUE` / `FALSE`
#' }
#'
#' Unsupported constructs (and therefore rejected) include:
#' \itemize{
#'   \item Character or numeric literals as standalone expressions
#'   \item Non-logical functions (e.g. `mean()`, `paste()`)
#'   \item Arbitrary function calls or side-effect expressions
#' }
#'
#' @param expr_chr A single character string containing a logical expression
#'   (typically the text of an expression to be parsed and evaluated).
#'
#' @return Logical scalar:
#' \itemize{
#'   \item `TRUE` if the expression represents a valid logical statement
#'   \item `FALSE` otherwise
#' }
#'
#' @examples
#' \dontrun{
#' .is_logical_expression("age > 5 & !is.na(sex)")
#'
#' .is_logical_expression("grepl('a', name)")
#'
#' .is_logical_expression("x %in% c('a', 'b', 'c')")
#'
#' .is_logical_expression("is.numeric(age)")
#'
#' .is_logical_expression("'my name is Jack'")
#' }
#'
#' @keywords internal
.is_logical_expression <- function(expr_chr) {

  # Must be a single character string
  if (!is.character(expr_chr) || length(expr_chr) != 1L || is.na(expr_chr)) {
    return(FALSE)
  }

  # Must parse
  parsed <- try(parse(text = expr_chr), silent = TRUE)
  if (inherits(parsed, "try-error") || length(parsed) != 1L) {
    return(FALSE)
  }

  expr <- parsed[[1]]

  # Allowed logical operators and functions
  logical_ops <- c("&", "|", "!", "&&", "||", "==", "!=", "<", "<=", ">", ">=", "%in%")
  logical_fns <- c(
    "is.na", "grepl",
    ".is_safely_coercible",
    # Base R type checking functions
    "is.numeric", "is.character", "is.logical", "is.integer", "is.double",
    "is.factor", "is.list", "is.vector", "is.data.frame", "is.matrix",
    "is.null", "is.atomic", "is.recursive", ".is_greater_than_one_selection", ".is_greater_than_three_selection"
  )

  # Case 1: direct logical literal
  if (is.logical(expr)) {
    return(TRUE)
  }

  # Case 2: expression contains logical operator
  if (is.call(expr) && as.character(expr[[1]]) %in% logical_ops) {
    return(TRUE)
  }

  # Case 3: expression contains logical-returning function
  if (is.call(expr) && as.character(expr[[1]]) %in% logical_fns) {
    return(TRUE)
  }

  FALSE
}



#' @title Validate That an Object Is Not NULL or NA
#' @description
#' Checks that an object exists and is not missing. Handles all object types safely.
#'
#' @param x Object to check.
#' @param origin Optional name of the calling function for context.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly TRUE if valid; otherwise throws [phr_error()] or [phr_warning()].
#' @export
phr_validate_not_null <- function(x, origin = NULL, soft) {
  if (is.null(x)) {
    msg <- "Received NULL input, expected a valid object."
    hint <- "Ensure the object exists before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint)
  }

  # Only check for NA if x is atomic (vector, not list/data.frame)
  if (is.atomic(x) && length(x) == 1 && is.na(x)) {
    msg <- "Received NA input, expected a non-missing value."
    hint <- "Ensure missing values are handled before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint)
  }

  invisible(TRUE)
}


#' @title Validate Numeric Input (Strict)
#' @description
#' Checks that `x` is strictly numeric (not logical, character, or other type).
#' Accepts vectors of numeric values.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_numeric <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.numeric(x) || is.logical(x)) {
    msg <- "Expected a strictly numeric value."
    hint_txt <- hint %||% "Ensure input is of type 'numeric'. Logical or character values are not accepted."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Character Input (Strict)
#' @description
#' Ensures that `x` is a character vector (not factor, numeric, or logical).
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_character <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.character(x)) {
    msg <- "Expected a character input (string)."
    hint_txt <- hint %||% "Ensure the input variable is of type 'character'. Factors or numerics are not allowed."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Logical Input (Strict)
#' @description
#' Checks that `x` is strictly logical. Does not allow numeric 0/1 substitutes.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_logical <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.logical(x)) {
    msg <- "Expected a logical (TRUE/FALSE) value."
    hint_txt <- hint %||% "Convert numeric indicators (0/1) to TRUE/FALSE explicitly."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Date or Date-Time Input (Strict Validation Only)
#' @description
#' Checks whether `x` is a valid date or datetime representation that can be safely
#' converted to a standard `Date` ("YYYY-MM-DD") format.
#'
#' The function performs format recognition without returning the converted value.
#' Supported inputs include:
#' - `Date` objects
#' - `POSIXct` or `POSIXlt` (considered valid)
#' - Character strings that can be parsed into valid dates using common formats
#'   such as `"2025-10-16"`, `"16/10/2025"`, `"2025-10-16T14:32:00Z"`, etc.
#'
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint to display on failure.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid. Triggers `phr_error()` or `phr_warning()` if invalid.
#' @export
phr_validate_date <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Accept existing Date or POSIX objects outright
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(invisible(TRUE))
  }

  # Attempt parsing if character vector
  if (is.character(x)) {
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
      "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ"
    )

    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(x, format = fmt))
      if (!any(is.na(parsed))) {
        return(invisible(TRUE))
      }
    }
  }

  # If nothing matched, raise structured error or warning
  msg <- "Expected a valid date or datetime convertible to 'YYYY-MM-DD'."
  hint_txt <- hint %||% "Ensure input is a Date, POSIX, or correctly formatted string (e.g. '2025-10-16')."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}

# Shared datetime format strings used by phr_validate_datetime(),
# .is_safely_coercible(), and phr_convert_datetime().
.phr_datetime_formats <- c(
  "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
  "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
  "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %I:%M:%S %p",
  "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
  "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M"
)

# ---- Validate Datetime ----------------------------------------------

#' @title Validate Date-Time Input
#' @description
#' Checks whether `x` is a valid datetime object (`POSIXct` or `POSIXlt`),
#' or a character string that can be parsed as a datetime with both date
#' and time components.
#'
#' Unlike `phr_validate_date`, this function rejects bare `Date` objects
#' and date-only strings without a time component.
#'
#' Supported inputs include:
#' - `POSIXct` or `POSIXlt` objects
#' - Character strings with time components such as `"2025-10-16 14:32:00"`,
#'   `"2025-10-16T14:32:00Z"`, `"16/10/2025 14:32"`, etc.
#'
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint to display on failure.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid. Triggers `phr_error()` or `phr_warning()` if invalid.
#' @export
phr_validate_datetime <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # POSIXct or POSIXlt are datetime objects \u2014 accept directly
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(invisible(TRUE))
  }

  # Check character strings that include a time component
  if (is.character(x)) {
    for (fmt in .phr_datetime_formats) {
      parsed <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
      if (!any(is.na(parsed))) {
        return(invisible(TRUE))
      }
    }
  }

  # Nothing matched \u2014 raise structured error or warning
  msg <- "Expected a datetime object (POSIXct or POSIXlt) or a string with date and time components."
  hint_txt <- hint %||% "Ensure input is POSIXct, POSIXlt, or a datetime string like '2025-10-16 14:32:00'."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}

# ---- Validate Factor ------------------------------------------------

#' @title Validate Factor Input
#' @description
#' Ensures that `x` is a factor variable.
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_factor <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (!is.factor(x)) {
    msg <- "Expected a factor variable."
    hint_txt <- hint %||% "Use factor() to convert character or numeric inputs before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}

# ---- Validate Columns (new) -----------------------------------------

#' @title Validate Presence of Required Columns
#' @description
#' Checks whether specified column names exist within a data frame.
#' @param df The data frame to check.
#' @param required_cols Character vector of required column names.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if all columns are present; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_columns <- function(df, required_cols, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(df, origin, soft)
  phr_validate_not_null(required_cols, origin, soft)
  if (!is.data.frame(df)) {
    msg <- "Input must be a data frame for column validation."
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  }
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    msg <- paste0("Missing required columns: ", paste(missing, collapse = ", "))
    hint_txt <- hint %||% "Ensure the data frame includes all required fields."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}

#' @title Validate Data Frame Input
#' @description
#' Ensures the object is a data.frame/tibble **and every column is atomic**,
#' not a list, not nested, not a data.frame inside a column.
#'
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_dataframe <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Must be a data.frame
  if (!is.data.frame(x)) {
    msg <- "Expected a data frame (or tibble)."
    hint_txt <- hint %||% "Ensure the object is created with data.frame(), tibble(), or similar."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # --- NEW: Validate all columns are atomic (no list columns) ---
  bad_cols <- names(x)[vapply(x, function(col) !is.atomic(col) || is.list(col), logical(1))]
  if (length(bad_cols) > 0) {
    msg <- paste0(
      "The following columns are non-atomic or contain list-like data: ",
      paste(bad_cols, collapse = ", ")
    )
    hint_txt <- "Flatten, unnest, or otherwise convert these columns to atomic vectors before use."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}

# ---- Get Data From Survey Design ------------------------------------

#' Extract Data Frame from Survey Design Object
#'
#' Validates that the input is a srvyr or survey design object and extracts
#' the underlying data frame.
#'
#' @param survey_design A srvyr or survey design object (e.g., created with
#'   \code{srvyr::as_survey_design()}).
#' @param origin Optional character string indicating where this function was called from.
#'
#' @return The underlying data frame extracted from the survey design object.
#' @export
phr_get_data_from_design <- function(survey_design, origin = NULL) {
  valid_classes <- c("tbl_svy", "survey.design", "survey.design2", "svyrep.design")

  if (!inherits(survey_design, valid_classes)) {
    phr_error(
      message = phr_txt(
        "survey_design must be a survey design object (e.g. created with srvyr::as_survey_design())."
      ),
      origin = origin,
      hint = "Use srvyr::as_survey_design() to create a survey design from your data frame."
    )
  }

  df <- survey_design$variables

  if (!is.data.frame(df)) {
    phr_error(
      message = phr_txt("Could not extract a valid data frame from the survey design object."),
      origin = origin
    )
  }

  df
}

# ---- Validate List --------------------------------------------------

#' @title Validate That an Object Is a True List (Not a Data Frame)
#' @description
#' Ensures the input object is a plain list. Rejects data frames, tibbles,
#' and other list-like objects used to represent tabular data.
#'
#' @param x Object to validate.
#' @param origin Optional character string indicating where validation was called.
#' @param hint Optional hint to include in the error message for user guidance.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if the object is a list (and not a data frame).
#' Otherwise, triggers [phr_error()] or [phr_warning()].
#' @export
phr_validate_list <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Reject anything that isn't a list, or that is a data.frame
  if (!is.list(x) || is.data.frame(x)) {
    msg <- "Expected a list (not a data frame or tibble)."
    hint_txt <- hint %||% "Ensure the input is a plain list structure (not a data.frame)."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}


# ---- Validate Vector Length -----------------------------------------

#' @title Validate Vector Length
#' @description
#' Ensures a vector has a specified minimum or exact length.
#' @param x The vector to test.
#' @param min_length Minimum allowed length (default = 1).
#' @param exact_length Optional integer specifying exact expected length.
#' @param origin Optional name of the originating function.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_vector_length <- function(x, min_length = 1, exact_length = NULL, origin = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  len <- length(x)
  if (!is.null(exact_length) && len != exact_length) {
    msg <- paste0("Expected vector of length ", exact_length, ", got length ", len, ".")
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  } else if (len < min_length) {
    msg <- paste0("Expected vector of length \u2265 ", min_length, ", got ", len, ".")
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin)
  }
  invisible(TRUE)
}

# ---- Validate Choice Membership -------------------------------------

#' @title Validate Choice Membership
#' @description
#' Ensures that the input value(s) belong to a defined set of allowed options.
#' @param x Value or vector to test.
#' @param choices Character vector of allowed values.
#' @param origin Optional name of the originating function.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_choice <- function(x, choices, origin = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  phr_validate_not_null(choices, origin, soft)
  bad <- setdiff(x, choices)
  if (length(bad) > 0) {
    msg <- paste0("Invalid choice(s): ", paste(bad, collapse = ", "))
    hint_txt <- paste0("Allowed values are: ", paste(choices, collapse = ", "))
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_warning(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}

# ---- Utility: Internal coercion check --------------------------------

.is_safely_coercible <- function(x, to_type) {
  # returns TRUE if all elements of x can be safely coerced to the target type
  suppressWarnings({
    if (to_type == "numeric") {
      coerced <- suppressWarnings(as.numeric(x))
      return(!any(is.na(coerced) & !is.na(x)))
    } else if (to_type == "character") {
      coerced <- suppressWarnings(as.character(x))
      return(TRUE)  # as.character() never fails
    } else if (to_type == "logical") {
      valid_vals <- c("TRUE", "FALSE", "T", "F", "1", "0")
      return(all(toupper(as.character(x)) %in% valid_vals | is.na(x)))
    } else if (to_type == "Date" | to_type == "date") {
      formats <- c(
        "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d%m%Y", "%m%d%Y",
        "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %I:%M:%S %p",
        "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
        "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M",
        "%Y-%m", "%m/%Y"
      )

      # Check if all elements can be parsed by at least one format
      is_convertible <- vapply(x, function(val) {
        if (is.na(val)) {
          TRUE  # Treat NA as safely coercible
        } else {
          any(!is.na(sapply(formats, function(fmt) {
            suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
          })))
        }
      }, logical(1))

      return(all(is_convertible))

    } else if (to_type == "factor") {
      return(is.character(x) || is.factor(x))
    } else if (to_type == "datetime" || to_type == "POSIXct" || to_type == "POSIXlt") {
      if (inherits(x, c("POSIXct", "POSIXlt"))) return(TRUE)
      is_convertible <- vapply(x, function(val) {
        if (is.na(val)) {
          TRUE
        } else {
          any(!is.na(sapply(.phr_datetime_formats, function(fmt) {
            suppressWarnings(as.POSIXct(as.character(val), format = fmt, tz = "UTC"))
          })))
        }
      }, logical(1))
      return(all(is_convertible))
    } else {
      return(FALSE)
    }
  })
}

.which_bad_coercible <- function(x, to_type) {
  suppressWarnings({
    n <- length(x)

    if (to_type == "numeric") {
      coerced <- suppressWarnings(as.numeric(x))
      return(is.na(coerced) & !is.na(x))
    }

    if (to_type == "character") {
      return(rep(FALSE, n))  # always safe
    }

    if (to_type == "logical") {
      valid_vals <- c("TRUE", "FALSE", "T", "F", "1", "0")
      return(!(toupper(as.character(x)) %in% valid_vals | is.na(x)))
    }

    if (to_type == "Date") {
      formats <- c(
        "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d%m%Y", "%m%d%Y",
        "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p",
        "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
        "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
        "%d/%m/%Y %H:%M", "%Y-%m", "%m/%Y"
      )

      ok_row <- vapply(x, function(val) {
        any(!is.na(sapply(formats, function(fmt) {
          suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
        })))
      }, logical(1))

      return(!ok_row & !is.na(x))
    }

    if (to_type == "factor") {
      return(!(is.character(x) | is.factor(x)))
    }

    # fallback
    rep(TRUE, n)
  })
}

# ---- All Numeric ----------------------------------------------------

#' @title Validate Entire Vector is Numeric or Coercible to Numeric
#' @description
#' Checks that all values in a vector (or column) are numeric or can be safely
#' converted to numeric without introducing NAs.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()`.
#' @export
phr_validate_all_numeric <- function(x,
                                       origin = NULL,
                                       hint = NULL,
                                       soft) {

  phr_validate_not_null(x, origin, soft)

  # Already numeric \u2192 valid
  if (is.numeric(x)) return(invisible(TRUE))

  # Safely coercible \u2192 valid
  if (.is_safely_coercible(x, "numeric")) return(invisible(TRUE))

  # ---- Build translated message + hint ----
  msg_txt  <- phr_txt("Expected all values to be numeric or safely coercible to numeric.")
  hint_txt <- hint %||% phr_txt("Ensure values contain only digits and valid numeric strings.")

  # ---- SOFT MODE: warning only ----
  if (soft) {
    phr_warning(message = msg_txt, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }

  # ---- HARD MODE: throw error ----
  phr_error(
    msg_txt,
    origin = origin,
    hint = hint_txt
  )
}

# ---- All Character ----------------------------------------------------

#' @title Validate Entire Vector is Character or Coercible to Character
#' @description
#' Checks that all values in a vector (or column) are character strings or can be
#' represented as character safely. Optionally enforces that all values belong to a
#' specified set of allowable options.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_values Optional character vector of allowed values. If provided,
#'   validation will fail if any element of `x` is not found within this set.
#' @param origin Optional name of the originating function (for tracing errors).
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_character <- function(x, allowed_values = NULL, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)

  # Type or coercion check
  if (!is.character(x) && !.is_safely_coercible(x, "character")) {
    msg <- "Expected all values to be character or coercible to character."
    hint_txt <- hint %||% "Use as.character() or convert factors/integers before passing."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # Allowed values check
  if (!is.null(allowed_values)) {
    # Coerce to character for comparison safety
    x_chr <- as.character(x)
    invalid <- setdiff(unique(x_chr), allowed_values)
    if (length(invalid) > 0) {
      msg <- paste0(
        "Invalid values detected: ", paste(invalid, collapse = ", "),
        ". Expected values: ", paste(allowed_values, collapse = ", ")
      )
      hint_txt <- hint %||% "Ensure all character entries match allowed options."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  invisible(TRUE)
}


# ---- All Logical ----------------------------------------------------

#' @title Validate Entire Vector is Logical or Coercible to Logical
#' @description
#' Checks that all values in a vector are logical (TRUE/FALSE) or coercible from
#' standard encodings such as "TRUE"/"FALSE", "T"/"F", or 0/1.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_logical <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (is.logical(x)) return(invisible(TRUE))
  if (.is_safely_coercible(x, "logical")) return(invisible(TRUE))
  msg <- "Expected all values to be logical (TRUE/FALSE) or safely coercible."
  hint_txt <- hint %||% "Ensure inputs are TRUE/FALSE, 1/0, or equivalent character codes."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}


# ---- All Date -------------------------------------------------------

#' @title Validate Entire Vector is Date or Coercible to Date
#' @description
#' Checks that all values in a vector (or column) are Date, POSIX, or can be safely
#' converted to a Date representation using common formats.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_all_date <- function(x, origin = NULL, hint = NULL, soft) {
  phr_validate_not_null(x, origin, soft)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(invisible(TRUE))
  if (.is_safely_coercible(x, "Date")) return(invisible(TRUE))
  msg <- "Expected all values to be Date, POSIX, or coercible to Date format."
  hint_txt <- hint %||% "Ensure date strings follow standard formats like 'YYYY-MM-DD' or 'DD/MM/YYYY'."
  if (soft) {
    phr_warning(message = msg, origin = origin, hint = hint_txt)
    return(invisible(FALSE))
  }
  phr_error(msg, origin = origin, hint = hint_txt)
}

# ---- All Factor -----------------------------------------------------

#' @title Validate Entire Vector is Factor or Coercible to Factor
#' @description
#' Checks that all values are factors or coercible (e.g., from character strings).
#' Optionally enforces that all factor levels belong to a specified set of
#' allowed levels, and optionally verifies that the factor order matches an
#' expected ordering.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_levels Optional character vector specifying the allowed factor levels.
#'   Validation fails if any observed level is not in this set.
#' @param expected_order Optional character vector specifying the correct ordering
#'   of factor levels. If provided, validation fails if the factor's level order
#'   does not exactly match.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()`.
#' @export
#' @title Validate That All Values Are Factors
#' @description
#' Validates that all values in a vector are factor type.
#' Optionally checks for allowed levels and correct ordering.
#' Strict: will not coerce from character or numeric values.
#'
#' @param x Vector to validate.
#' @param allowed_levels Optional character vector of valid factor levels.
#' @param expected_order Optional character vector specifying expected order of levels.
#' @param origin Optional name of the function or module calling this validator.
#' @param hint Optional suggestion or guidance for fixing detected issues.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return Invisibly returns TRUE if valid; otherwise throws an [phr_error()] or [phr_warning()].
#' @export
phr_validate_all_factor <- function(
    x,
    allowed_levels = NULL,
    expected_order = NULL,
    origin = NULL,
    hint = NULL,
    soft
) {
  phr_validate_not_null(x, origin, soft)

  # ---- Type check ----
  if (!is.factor(x)) {
    msg <- "Expected a factor vector, but received a non-factor type."
    hint_txt <- hint %||% "Convert variable to factor before validation."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }

  # ---- Check allowed levels ----
  if (!is.null(allowed_levels)) {
    levels_present <- levels(x)
    invalid <- setdiff(levels_present, allowed_levels)
    if (length(invalid) > 0) {
      msg <- paste0(
        "Invalid factor levels detected: ", paste(invalid, collapse = ", "),
        ". Allowed levels are: ", paste(allowed_levels, collapse = ", ")
      )
      hint_txt <- hint %||% "Check factor level definitions or category spelling."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  # ---- Check expected order ----
  if (!is.null(expected_order)) {
    actual_order <- levels(x)
    if (!identical(actual_order, expected_order)) {
      msg <- paste0(
        "Incorrect factor level order. Actual: ",
        paste(actual_order, collapse = " > "),
        ". Expected: ",
        paste(expected_order, collapse = " > ")
      )
      hint_txt <- hint %||% "Ensure factor levels are defined in the correct order."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }

  invisible(TRUE)
}


#' @title Validate Column Types
#' @description
#' Ensures specified columns in a data frame match expected R classes.
#' @param df Data frame to validate.
#' @param expected_types Named list of expected types, e.g. `list(age = "numeric", name = "character")`.
#' @param origin Optional name of the calling function.
#' @param hint Optional hint for correction.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly returns TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_column_types <- function(df, expected_types, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  missing <- setdiff(names(expected_types), names(df))
  if (length(missing) > 0) {
    msg <- paste("Missing expected columns:", paste(missing, collapse = ", "))
    hint_txt <- hint %||% "Ensure all required columns are present."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  for (col in names(expected_types)) {
    actual <- class(df[[col]])[1]
    expected <- expected_types[[col]]
    if (actual != expected) {
      msg <- paste0("Column '", col, "' is of type '", actual, "' but expected '", expected, "'.")
      hint_txt <- hint %||% "Check data import or preprocessing steps."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}


#' @title Validate No Missing Values
#' @description
#' Checks that specified columns contain no missing (NA) values.
#' @param df Data frame to test.
#' @param cols Character vector of column names to check.
#' @param origin Optional function name.
#' @param hint Optional correction hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid, otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_missing <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in cols) {
    if (any(is.na(df[[col]]))) {
      msg <- paste0("Column '", col, "' contains missing (NA) values.")
      hint_txt <- hint %||% "Fill or remove missing data before proceeding."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}


#' @title Validate Column Uniqueness
#' @description
#' Ensures one or more columns (or their combination) form a unique identifier.
#' @param df Data frame to validate.
#' @param cols Character vector of columns whose combination must be unique.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_unique <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  dupes <- duplicated(df[cols])
  if (any(dupes)) {
    msg <- paste("Duplicate entries detected for key columns:", paste(cols, collapse = ", "))
    hint_txt <- hint %||% "Ensure unique identifiers or combination keys."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Non-Negative Values
#' @description
#' Ensures numeric columns have no negative values.
#' @param df Data frame to validate.
#' @param cols Character vector of numeric columns to check.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_non_negative <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in cols) {
    if (any(df[[col]] < 0, na.rm = TRUE)) {
      msg <- paste0("Column '", col, "' contains negative values.")
      hint_txt <- hint %||% "Ensure counts or quantities are zero or positive."
      if (soft) {
        phr_warning(message = msg, origin = origin, hint = hint_txt)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin, hint = hint_txt)
    }
  }
  invisible(TRUE)
}


#' @title Validate Range of Numeric Values
#' @description
#' Checks that values in a numeric column fall within a specified inclusive range.
#' @param df Data frame to validate.
#' @param col Column name to check.
#' @param min Minimum acceptable value.
#' @param max Maximum acceptable value.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()`.
#' @export
phr_validate_range <- function(df,
                                 col,
                                 min,
                                 max,
                                 origin = NULL,
                                 hint = NULL,
                                 soft) {

  phr_validate_dataframe(df, origin, soft)

  vals <- df[[col]]
  out_of_range <- vals < min | vals > max

  if (any(out_of_range, na.rm = TRUE)) {

    msg <- paste0(
      "Values in '", col, "' are out of range [", min, ", ", max, "]."
    )

    if (isTRUE(soft)) {
      # -----------------------------------------
      # NEW: Soft mode (warning only)
      # -----------------------------------------
      phr_warning(
        message = phr_txt(msg),
        origin = origin,
        hint = hint %||% phr_txt("Check data entry or range filters.")
      )
      return(invisible(FALSE))  # return FALSE for caller logic
    }

    # -----------------------------------------
    # Original behavior: hard error
    # -----------------------------------------
    phr_error(
      message = msg,
      origin = origin,
      hint = hint %||% phr_txt("Check data entry or range filters.")
    )
  }

  invisible(TRUE)
}


#' @title Validate Pattern Match for Character Columns
#' @description
#' Ensures all non-missing values in a character column match a regular expression.
#' @param df Data frame to validate.
#' @param col Character column name.
#' @param pattern Regular expression pattern (e.g., `"^[0-9]\{10\}$"`).
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_pattern <- function(df, col, pattern, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  vals <- df[[col]]
  bad <- !grepl(pattern, vals[!is.na(vals)])
  if (any(bad)) {
    msg <- paste0("Some values in '", col, "' do not match the required pattern: ", pattern)
    hint_txt <- hint %||% "Verify field formatting and encoding."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Date Order
#' @description
#' Ensures that all rows have start_date <= end_date.
#' @param df Data frame containing the two date columns.
#' @param start_col Name of the start date column.
#' @param end_col Name of the end date column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_date_order <- function(df, start_col, end_col, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  if (any(df[[start_col]] > df[[end_col]], na.rm = TRUE)) {
    msg <- paste0("Start date in column '", start_col, "' exceeds end date in '", end_col, "'.")
    hint_txt <- hint %||% "Ensure start and end dates are in logical order."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Column Dependency
#' @description
#' Ensures if one column has a non-missing value, another dependent column is also filled.
#' @param df Data frame to test.
#' @param col_a Independent column (trigger).
#' @param col_b Dependent column (must be filled if A is non-missing).
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_dependency <- function(df, col_a, col_b, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  missing_dep <- !is.na(df[[col_a]]) & is.na(df[[col_b]])
  if (any(missing_dep)) {
    msg <- paste0("Rows where '", col_a, "' is filled but '", col_b, "' is missing.")
    hint_txt <- hint %||% paste0("Ensure '", col_b, "' is provided when '", col_a, "' is non-missing.")
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Mutually Exclusive Columns
#' @description
#' Ensures only one of several related indicator columns is TRUE (or 1) per row.
#' @param df Data frame to test.
#' @param cols Character vector of logical or numeric indicator columns.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_mutually_exclusive <- function(df, cols, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  count_true <- rowSums(df[cols] == TRUE | df[cols] == 1, na.rm = TRUE)
  if (any(count_true > 1)) {
    msg <- paste0("Multiple TRUE/1 values found across mutually exclusive columns: ", paste(cols, collapse = ", "))
    hint_txt <- hint %||% "Ensure only one column is TRUE per record."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate No Duplicate Rows
#' @description
#' Ensures there are no fully duplicated rows in the data frame.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_duplicates <- function(df, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  if (any(duplicated(df))) {
    msg <- "Duplicate rows detected in the dataset."
    hint_txt <- hint %||% "Use distinct() or unique() to remove duplicates."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate No Constant Columns
#' @description
#' Ensures no column in a data frame has the same value for all rows.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_no_constant_columns <- function(df, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  const_cols <- names(df)[sapply(df, function(x) length(unique(na.omit(x))) <= 1)]
  if (length(const_cols) > 0) {
    msg <- paste0("Constant (uninformative) columns detected: ", paste(const_cols, collapse = ", "))
    hint_txt <- hint %||% "Consider removing or reviewing these columns."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Reference Values
#' @description
#' Ensures all entries in a column exist within a reference vector or table.
#' @param df Data frame to check.
#' @param col Column name.
#' @param ref_values Vector of reference or lookup values.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_reference_values <- function(df, col, ref_values, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  invalid <- setdiff(unique(df[[col]]), ref_values)
  if (length(invalid) > 0) {
    msg <- paste0("Values in '", col, "' not found in reference list: ", paste(invalid, collapse = ", "))
    hint_txt <- hint %||% "Check code lists or join keys for consistency."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Join Keys
#' @description
#' Ensures all key values from one data frame exist in a reference data frame before joining.
#' @param df Data frame containing key column to check.
#' @param ref_df Reference data frame containing valid key column.
#' @param key_col Name of the key column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_join_keys <- function(df, ref_df, key_col, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  phr_validate_dataframe(ref_df, origin, soft)
  missing_keys <- setdiff(unique(df[[key_col]]), unique(ref_df[[key_col]]))
  if (length(missing_keys) > 0) {
    msg <- paste0("Join key values in '", key_col, "' not found in reference data: ", paste(missing_keys, collapse = ", "))
    hint_txt <- hint %||% "Check for mismatched or missing join identifiers."
    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }
    phr_error(msg, origin = origin, hint = hint_txt)
  }
  invisible(TRUE)
}


#' @title Validate Full Data Frame Schema
#' @description
#' Validates a data frame against a declarative schema specification.
#' Each element of the schema list can include:
#'   - `type`: Expected R class (e.g. `"numeric"`, `"character"`, `"Date"`)
#'   - `allowed_values`: Optional allowed set
#'   - `range`: Optional numeric vector of length 2 for min/max
#'
#' @param df Data frame to validate.
#' @param schema Named list specifying expected structure.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#' @return Invisibly TRUE if valid; otherwise triggers `phr_error()` or `phr_warning()`.
#' @export
phr_validate_schema <- function(df, schema, origin = NULL, hint = NULL, soft) {
  phr_validate_dataframe(df, origin, soft)
  for (col in names(schema)) {
    if (!col %in% names(df)) {
      msg <- paste("Missing expected column:", col)
      if (soft) {
        phr_warning(message = msg, origin = origin)
        return(invisible(FALSE))
      }
      phr_error(msg, origin = origin)
    }
    spec <- schema[[col]]
    if (!is.null(spec$type)) {
      phr_validate_column_types(df[, col, drop = FALSE], setNames(list(spec$type), col), origin = origin, soft = soft)
    }
    if (!is.null(spec$allowed_values)) {
      phr_validate_all_character(df[[col]], allowed_values = spec$allowed_values, origin = origin, soft = soft)
    }
    if (!is.null(spec$range) && length(spec$range) == 2 && is.numeric(df[[col]])) {
      phr_validate_range(df, col, spec$range[1], spec$range[2], origin = origin, soft = soft)
    }
  }
  invisible(TRUE)
}

#' Infer the likely data type of a column
#'
#' Inspects a vector and attempts to infer whether it represents
#' numeric, date, logical, or character data based on its content.
#' Non-destructive: does not coerce, only classifies.
#'
#' @param x A vector (typically one column from a data frame).
#' @param name Optional column name (for diagnostic messages).
#' @param return_details Logical; if TRUE, returns additional metadata
#'   (percentages of matching patterns per type).
#'
#' @return A character string ("numeric", "date", "logical", "character", or "unknown"),
#'   or a list with detailed info if `return_details = TRUE`.
#'
#' @export
#'
#' @examples
#' phr_infer_column_type(c("12", "5", "9"))
#' phr_infer_column_type(c("TRUE", "FALSE", "TRUE"))
#' phr_infer_column_type(c("2024-05-03", "2024-05-04"))
#' phr_infer_column_type(c("North", "South"))

phr_infer_column_type <- function(x, name = NULL, return_details = FALSE) {
  # --- Prepare input ---
  x_clean <- trimws(as.character(x))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) {
    if (return_details) {
      return(list(type = "unknown", valid = TRUE, reason = "All values empty or NA"))
    } else {
      return("unknown")
    }
  }

  n <- length(x_clean)

  # --- Numeric detection ---
  numeric_pattern <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", x_clean)
  pct_numeric <- mean(numeric_pattern)

  # --- Logical detection ---
  logical_pattern <- grepl("^(TRUE|FALSE|T|F|Yes|No|Y|N|1|0)$", x_clean, ignore.case = TRUE)
  pct_logical <- mean(logical_pattern)

  # --- Date detection ---
  date_formats <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ"
  )
  pct_date <- 0
  for (fmt in date_formats) {
    suppressWarnings({
      converted <- as.POSIXct(x_clean, format = fmt, tz = "UTC")
    })
    if (all(!is.na(converted))) {
      pct_date <- 1
      break
    } else {
      pct_date <- max(pct_date, mean(!is.na(converted)))
    }
  }

  # --- Choose dominant type ---
  scores <- c(numeric = pct_numeric, date = pct_date, logical = pct_logical)
  best_type <- names(scores)[which.max(scores)]
  confidence <- max(scores)

  # --- Threshold logic ---
  inferred_type <- if (confidence >= 0.9) best_type else "character"

  # --- Construct result ---
  if (return_details) {
    return(list(
      type = inferred_type,
      confidence = confidence,
      pct_numeric = pct_numeric,
      pct_logical = pct_logical,
      pct_date = pct_date,
      n_values = n
    ))
  } else {
    return(inferred_type)
  }
}

#' Validate chronological order of date vectors
#'
#' Ensures that for each record, the start date is before or equal to the end date.
#'
#' @param start A vector of start dates (character, Date, or POSIXct).
#' @param end A vector of end dates (character, Date, or POSIXct).
#' @param origin Optional string indicating the calling module or object.
#' @param soft Logical; if TRUE, issues a warning on failure; if FALSE, throws an error.
#'
#' @return TRUE (invisibly) if validation passes; otherwise issues an IPHRA warning or error.
#'
#' @export
#'
#' @examples
#' phr_validate_date_order_vectors(
#'   as.Date(c("2024-01-01", "2024-02-01")),
#'   as.Date(c("2024-03-01", "2024-01-15")),
#'   origin = "MortalityHouseholdData",
#'   soft = TRUE
#' )
#' # Issues a warning for the second pair.

phr_validate_date_order_vectors <- function(start, end, origin = NULL, soft) {
  # --- Defensive checks ---
  if (length(start) != length(end)) {
    msg <- phr_txt(glue::glue("Start and end date vectors differ in length. Comparing first {min(length(start), length(end))} pairs."))
    if (soft) {
      phr_warning(message = msg, origin = origin)
    } else {
      phr_error(msg, origin = origin)
    }
    n <- min(length(start), length(end))
    start <- start[seq_len(n)]
    end <- end[seq_len(n)]
  }

  # Coerce to Date if necessary
  suppressWarnings({
    start_date <- as.Date(start)
    end_date <- as.Date(end)
  })

  # Identify invalid coercions
  invalid_pairs <- which(is.na(start_date) | is.na(end_date))
  if (length(invalid_pairs) > 0) {
    msg <- phr_txt(glue::glue("Found {length(invalid_pairs)} records with invalid or missing start/end dates."))
    if (soft) {
      phr_warning(message = msg, origin = origin)
    } else {
      phr_error(msg, origin = origin)
    }
  }

  # Identify reversed (chronologically invalid) pairs
  reversed <- which(start_date > end_date)
  if (length(reversed) > 0) {
    msg <- phr_txt(glue::glue("Found {length(reversed)} records where recall_start occurs after recall_end."))
    if (soft) {
      phr_warning(message = msg, origin = origin)
      return(invisible(FALSE))
    } else {
      phr_error(msg, origin = origin)
    }
  }

  phr_message(phr_txt(glue::glue("Date order validation passed for {length(start_date)} records.")))
  return(invisible(TRUE))
}

#' Convert character, numeric, or POSIX dates to standard Date (YYYY-MM-DD)
#'
#' @param x Character, Date, numeric, or POSIX vector.
#' @param origin Character string specifying the origin date for numeric input conversion.
#'   Use `"excel"` for Excel serial dates (treated as `"1899-12-30"`), or any date string
#'   in `"YYYY-MM-DD"` format (default: `"1970-01-01"` for Unix epoch).
#' @return A Date vector in YYYY-MM-DD format.
#' @export
phr_convert_date <- function(x, origin = "1970-01-01") {

  # Excel keyword
  if (identical(origin, "excel")) {
    origin <- "1899-12-30"
  }

  today <- Sys.Date()
  future_limit <- today + 365 * 5   # numeric\u2192date more than 5 years in the future is suspicious

  # Already Date
  if (inherits(x, "Date")) return(x)

  # POSIX \u2192 Date
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  # -------- NUMERIC INPUT --------
  if (is.numeric(x)) {

    # ORIGINAL LOGIC (minimal revision)
    origin_num <- if (all(x > 30000 & x < 60000, na.rm = TRUE)) {
      "1899-12-30"     # Excel
    } else {
      "1970-01-01"     # Unix
    }

    return(as.Date(x, origin = origin_num))
  }

  # -------- CHARACTER NUMERIC-LIKE --------
  if (is.character(x) && all(grepl("^[0-9]+$", x[!is.na(x)]))) {

    x_num <- as.numeric(x)

    # ORIGINAL LOGIC (minimal revision)
    origin_num <- if (all(x_num > 20000 & x_num < 60000, na.rm = TRUE)) {
      "1899-12-30"     # Excel
    } else {
      "1970-01-01"     # Unix
    }

    return(as.Date(x_num, origin = origin_num))
  }

  # -------- GENERAL CHARACTER DATES --------
  x_chr <- as.character(x)
  is_na <- is.na(x_chr)
  to_parse <- trimws(x_chr[!is_na])

  # Remove timezones
  to_parse <- sub("\\s*(UTC|GMT|CEST|CET|EST|PST|[+-]\\d{2}:\\d{2})$",
                  "", to_parse, ignore.case = TRUE)

  # Strip ISO timestamps
  to_parse <- sub("T.*$", "", to_parse)

  # Warn if time-of-day present
  if (any(grepl("\\d{2}:\\d{2}:\\d{2}", x_chr[!is_na]))) {
    warning("Time components detected and removed by phr_convert_date().")
  }

  # Try to parse human-readable dates
  parsed <- suppressWarnings(lubridate::parse_date_time(
    to_parse,
    orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS"),
    exact = FALSE
  ))

  parsed <- as.Date(parsed)

  # Fail on parsing errors
  if (any(is.na(parsed))) {
    invalid_vals <- unique(to_parse[is.na(parsed)])
    stop(
      "Could not convert values to Date: ",
      paste0("'", invalid_vals, "'", collapse = ", "),
      ". Expected formats: ymd, dmy, mdy."
    )
  }

  # Reinsert NA
  out <- rep(NA, length(x_chr))
  out[!is_na] <- parsed
  class(out) <- "Date"

  return(out)
}

#' Convert character, numeric, Date, or POSIX values to POSIXct (datetime)
#'
#' Converts various input types to a `POSIXct` vector, preserving time
#' information. Unlike `phr_convert_date()`, this function does not strip the
#' time component.
#'
#' @param x Character, Date, numeric (Unix timestamp), or POSIX vector.
#' @param tz Time zone to use for the output POSIXct vector. Defaults to `"UTC"`.
#' @return A `POSIXct` vector.
#' @export
phr_convert_datetime <- function(x, tz = "UTC") {

  # Already POSIXct \u2014 return as-is (re-stamp tz to be safe)
  if (inherits(x, "POSIXct")) return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = tz))

  # POSIXlt \u2192 POSIXct
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x, tz = tz))

  # Date \u2192 POSIXct (midnight)
  if (inherits(x, "Date")) return(as.POSIXct(as.character(x), format = "%Y-%m-%d", tz = tz))

  # Numeric \u2014 treat as Unix timestamp
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = tz))

  # Character \u2014 try known datetime formats
  x_chr <- as.character(x)
  is_na <- is.na(x_chr)
  to_parse <- trimws(x_chr[!is_na])

  parsed <- NULL
  for (fmt in .phr_datetime_formats) {
    converted <- suppressWarnings(as.POSIXct(to_parse, format = fmt, tz = tz))
    if (all(!is.na(converted))) {
      parsed <- converted
      break
    }
  }

  if (is.null(parsed) || any(is.na(parsed))) {
    invalid_vals <- if (is.null(parsed)) unique(to_parse) else unique(to_parse[is.na(parsed)])
    stop(
      "Could not convert values to datetime (POSIXct): ",
      paste0("'", invalid_vals, "'", collapse = ", "),
      ". Expected formats like '2025-10-16 14:32:00' or ISO 8601."
    )
  }

  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = tz)
  out[!is_na] <- parsed
  return(out)
}

#' Parse an \code{"HH:MM"} time-of-day string to minutes since midnight
#'
#' Converts a character string in 24-hour \code{"HH:MM"} format (e.g.\
#' \code{"08:30"}, \code{"18:00"}) to a single numeric value representing
#' the number of minutes elapsed since midnight.  Numeric inputs are returned
#' unchanged (they are assumed to already be minutes since midnight).
#'
#' @param x A character string in \code{"HH:MM"} format, or a numeric value
#'   (minutes since midnight).
#' @param origin Optional name of the calling function, used in error messages.
#' @return A single numeric value: minutes since midnight.
#' @export
phr_parse_hhmm <- function(x, origin = NULL) {

  if (is.numeric(x)) return(x)

  x_chr <- trimws(as.character(x))

  if (!grepl("^[0-9]{1,2}:[0-9]{2}$", x_chr)) {
    phr_error(
      message = phr_txt(
        "Cannot parse time-of-day value: '{x_chr}'. Expected 'HH:MM' format (e.g. '08:30', '18:00')."
      ),
      origin = origin %||% "phr_parse_hhmm",
      hint   = "Provide a 24-hour time string such as '08:00' or '18:30'."
    )
  }

  parts <- as.integer(strsplit(x_chr, ":", fixed = TRUE)[[1L]])
  h <- parts[1L]
  m <- parts[2L]

  if (h < 0L || h > 23L || m < 0L || m > 59L) {
    phr_error(
      message = phr_txt(
        "Invalid time-of-day value: '{x_chr}'. Hours must be 0-23 and minutes 0-59."
      ),
      origin = origin %||% "phr_parse_hhmm"
    )
  }

  h * 60L + m
}

phr_validate_required_na <- function(df, required_cols, origin = NULL, hint = NULL, soft) {

  phr_validate_dataframe(df, origin = origin, soft = soft)

  na_rows <- which(
    apply(df[required_cols], 1, function(x) any(is.na(x)))
  )

  if (length(na_rows) > 0) {
    msg <- paste0(
      "Missing values (NA) detected in required fields at rows: ",
      paste(na_rows, collapse = ", ")
    )
    hint_txt <- hint %||% "Ensure all required fields are populated."

    if (soft) {
      phr_warning(message = msg, origin = origin, hint = hint_txt)
      return(invisible(FALSE))
    }

    phr_error(message = msg, origin = origin, hint = hint_txt)
  }

  invisible(TRUE)
}
