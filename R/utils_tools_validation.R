# ---------------------------------------------------------------
# XLSForm Tool Validation Helpers
# ---------------------------------------------------------------
# Helper functions for validating XLSForm tool structure and coding.
# These utilities operate on XLSForm survey sheets represented as
# data frames where columns such as 'name', 'type', and 'relevant'
# follow standard XLSForm conventions.
#
# Architecture
# ------------
# xlsform_extract_variables() and xlsform_collect_variables() are
# general-purpose extract helpers.  All other functions in this
# file build on them where applicable and return a standardised
# list(valid = <bool>, issues = list(list(row = <int>, message = <chr>))).
# For functions that check a single cell/string (not a data frame),
# row is NA_integer_.
# ---------------------------------------------------------------


# ---- 1. Extract values from a cell (general extract helper) ----

#' @title Extract Matched Values from a Single Cell String
#'
#' @description
#' A general-purpose extraction helper that applies a PCRE regex
#' `pattern` to a single character string and returns the content
#' of the **first capture group** for every match.  When the pattern
#' contains no capture group the full match is returned instead.
#'
#' By default the pattern targets XLSForm `${}` variable references
#' (e.g. `${my_var}`), so the function extracts the variable name
#' inside the braces.  Pass a different `pattern` to extract other
#' structured tokens.
#'
#' This helper is called internally by `xlsform_collect_variables()`
#' and `xlsform_check_undefined_references()`.
#'
#' @param cell    A single character string (one XLSForm cell value).
#' @param pattern A PCRE-compatible regular expression.  The default
#'   `"\\$\\{([^}]+)\\}"` captures XLSForm variable names.
#'
#' @return A character vector of extracted values (one per match).
#'   Returns `character(0)` when no matches are found or when `cell`
#'   is `NA`, `NULL`, or not a single character string.
#'
#' @examples
#' xlsform_extract_variables("${age} > 18 and ${consent} = 'yes'")
#' # Returns: c("age", "consent")
#'
#' xlsform_extract_variables("no variables here")
#' # Returns: character(0)
#'
#' @export
xlsform_extract_variables <- function(cell, pattern = "\\$\\{([^}]+)\\}") {

  if (!is.character(cell) || length(cell) != 1L || is.na(cell)) {
    return(character(0))
  }

  pos <- gregexpr(pattern, cell, perl = TRUE)[[1L]]
  if (pos[1L] == -1L) return(character(0))

  cap_start  <- attr(pos, "capture.start")
  cap_length <- attr(pos, "capture.length")

  if (!is.null(cap_start) && ncol(cap_start) >= 1L && nrow(cap_start) > 0L && all(cap_start[, 1L] != -1L)) {
    # Capture group 1 content
    substring(cell, cap_start[, 1L], cap_start[, 1L] + cap_length[, 1L] - 1L)
  } else {
    # No capture groups: return full matches
    substring(cell, as.integer(pos), as.integer(pos) + attr(pos, "match.length") - 1L)
  }
}


# ---- 2. Collect matched values across a column (general extract helper) ----

#' @title Collect Matched Values Across a Data Frame Column
#'
#' @description
#' Iterates every row of a character column in a data frame, calling
#' `xlsform_extract_variables()` on each cell, and returns a single
#' character vector of all extracted values.
#'
#' By default the function extracts XLSForm `${}` variable names.
#' Pass a custom `pattern` to extract other structured tokens.
#'
#' This helper is called internally by `xlsform_check_undefined_references()`.
#'
#' @param df          A data frame containing XLSForm data.
#' @param col         A character string naming the column to scan.
#' @param only_unique Logical; if `TRUE` (default) return only unique
#'   values.  Set to `FALSE` to return all occurrences.
#' @param pattern     PCRE regex passed to `xlsform_extract_variables()`.
#'   Defaults to the XLSForm `${}` variable reference pattern.
#'
#' @return A character vector of extracted values found across the
#'   column.  Returns `character(0)` when no matches exist.
#'
#' @examples
#' survey <- data.frame(
#'   relevant = c(
#'     "${age} > 5",
#'     "${consent} = 'yes' and ${age} > 0",
#'     NA_character_
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_collect_variables(survey, "relevant")
#' # Returns: c("age", "consent")
#'
#' @export
xlsform_collect_variables <- function(df, col, only_unique = TRUE,
                                       pattern = "\\$\\{([^}]+)\\}") {
  origin <- "xlsform_collect_variables"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(col) && length(col) == 1L && col %in% names(df),
    paste0("Column '", col, "' not found in `df`."),
    origin = origin,
    hint = "Check column name spelling and that the data frame has been loaded correctly."
  )

  column_values <- df[[col]]

  all_vars <- unlist(
    lapply(column_values, xlsform_extract_variables, pattern = pattern),
    use.names = FALSE
  )

  if (is.null(all_vars) || length(all_vars) == 0L) {
    return(character(0))
  }

  if (only_unique) {
    return(unique(all_vars))
  }

  all_vars
}


# ---- 3. Validate a variable name -------------------------------

#' @title Check Whether an XLSForm Variable Name Is Valid
#'
#' @description
#' A valid XLSForm variable name must:
#' \itemize{
#'   \item Contain only letters (ASCII), digits, and underscores (`_`).
#'   \item Start with a letter or underscore (not a digit).
#'   \item Not contain spaces, apostrophes, hyphens, or any other characters
#'         that would make it non-machine-readable.
#' }
#'
#' @param varname A single character string to validate.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if the name is valid; `FALSE` otherwise.}
#'   \item{`issues`}{A list of issue items (each with `row` and `message`).
#'     Empty when `valid` is `TRUE`.  `row` is `NA_integer_` because this
#'     function operates on a single value, not a data frame row.}
#' }
#'
#' @examples
#' xlsform_is_valid_varname("my_variable")$valid   # TRUE
#' xlsform_is_valid_varname("my variable")$valid   # FALSE – space
#' xlsform_is_valid_varname("123start")$valid      # FALSE – starts with digit
#'
#' @export
xlsform_is_valid_varname <- function(varname) {

  if (!is.character(varname) || length(varname) != 1L ||
      is.na(varname) || nchar(varname) == 0L) {
    msg <- "Variable name is missing, empty, or not a character string."
    return(list(
      valid  = FALSE,
      issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
    ))
  }

  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", varname, perl = TRUE)) {
    return(list(valid = TRUE, issues = list()))
  }

  msg <- paste0(
    "Variable name '", varname,
    "' is invalid: use only letters, digits, and underscores, ",
    "starting with a letter or underscore."
  )
  list(
    valid  = FALSE,
    issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
  )
}


# ---- 4. Check whether a variable name appears in a name vector -

#' @title Check Whether an XLSForm Variable Name Is Defined in the Survey
#'
#' @description
#' Looks up a variable name in a reference vector of names — typically the
#' `name` column of the XLSForm survey sheet — to confirm the variable has
#' been declared.
#'
#' @param varname     A single character string: the variable name to look up.
#' @param name_vector A character vector of declared variable names to search
#'   within (e.g. `survey$name`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if `varname` is found in `name_vector`.}
#'   \item{`issues`}{A list of issue items (each with `row` and `message`).
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' declared <- c("age", "sex", "consent", "hh_size")
#' xlsform_varname_in_survey("age", declared)$valid     # TRUE
#' xlsform_varname_in_survey("weight", declared)$valid  # FALSE
#'
#' @export
xlsform_varname_in_survey <- function(varname, name_vector) {
  origin <- "xlsform_varname_in_survey"

  if (!is.character(varname) || length(varname) != 1L || is.na(varname)) {
    msg <- "Argument `varname` must be a single non-NA character string."
    phr_warning(msg, origin = origin)
    return(list(
      valid  = FALSE,
      issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
    ))
  }

  phr_assert(
    is.character(name_vector),
    "Argument `name_vector` must be a character vector.",
    origin = origin,
    hint = "Typically pass the 'name' column of the XLSForm survey sheet."
  )

  if (varname %in% name_vector) {
    return(list(valid = TRUE, issues = list()))
  }

  msg <- paste0("Variable '", varname, "' is not declared in the survey name column.")
  list(
    valid  = FALSE,
    issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
  )
}


# ---- 5. Detect unmatched parentheses or square brackets --------

#' @title Check a Cell for Unmatched Parentheses or Square Brackets
#'
#' @description
#' Scans a single character string for bracket balance issues:
#' unmatched opening or closing parentheses `()` and unmatched opening
#' or closing square brackets `[]`.
#'
#' @param cell A single character string (one XLSForm cell value).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` when both parentheses and square brackets are
#'     balanced.}
#'   \item{`issues`}{A list of issue items (each with `row` and `message`),
#'     one for each imbalanced bracket type.  Empty when `valid` is `TRUE`.}
#' }
#' `valid` is `TRUE` and `issues` is empty when `cell` is `NA`, `NULL`, or
#' an empty string.
#'
#' @examples
#' xlsform_check_brackets("(a + b) * (c - d)")$valid
#' # TRUE
#'
#' xlsform_check_brackets("if(a > 1, 'yes'")$valid
#' # FALSE – unmatched opening parenthesis
#'
#' @export
xlsform_check_brackets <- function(cell) {

  if (!is.character(cell) || length(cell) != 1L || is.na(cell) || nchar(cell) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  chars <- strsplit(cell, "", fixed = TRUE)[[1]]

  paren_depth   <- 0L
  bracket_depth <- 0L
  paren_ok      <- TRUE
  bracket_ok    <- TRUE

  for (ch in chars) {
    if (ch == "(") {
      paren_depth <- paren_depth + 1L
    } else if (ch == ")") {
      paren_depth <- paren_depth - 1L
      if (paren_depth < 0L) {
        paren_ok <- FALSE
        break
      }
    } else if (ch == "[") {
      bracket_depth <- bracket_depth + 1L
    } else if (ch == "]") {
      bracket_depth <- bracket_depth - 1L
      if (bracket_depth < 0L) {
        bracket_ok <- FALSE
        break
      }
    }
  }

  paren_ok   <- paren_ok   && paren_depth == 0L
  bracket_ok <- bracket_ok && bracket_depth == 0L

  issues <- list()

  if (!paren_ok) {
    msg <- "Parentheses are not balanced."
    issues <- c(issues, list(list(row = NA_integer_, message = phr_txt(msg, default = msg))))
  }

  if (!bracket_ok) {
    msg <- "Square brackets are not balanced."
    issues <- c(issues, list(list(row = NA_integer_, message = phr_txt(msg, default = msg))))
  }

  list(valid = length(issues) == 0L, issues = issues)
}


# ---- 6. Detect orphaned square brackets (no preceding $) -------

#' @title Detect Square Brackets Not Preceded by a Dollar Sign
#'
#' @description
#' In XLSForm logic, square brackets are only valid inside the `${}` variable
#' reference syntax. A `[` that appears on its own (not immediately after `$`)
#' is invalid coding and will cause evaluation errors.
#'
#' This helper strips all `${}` variable blocks (using the extract helper
#' pattern) from the cell and then checks for any remaining `[` characters.
#'
#' @param cell A single character string (one XLSForm cell value).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if no orphaned square brackets are found.}
#'   \item{`issues`}{A list of issue items (each with `row` and `message`).
#'     Empty when `valid` is `TRUE`.}
#' }
#' `valid` is `TRUE` and `issues` is empty when `cell` is `NA`, `NULL`, or
#' an empty string.
#'
#' @examples
#' xlsform_orphan_square_brackets("${var}")$valid   # TRUE
#' xlsform_orphan_square_brackets("[1]")$valid      # FALSE
#'
#' @export
xlsform_orphan_square_brackets <- function(cell) {

  if (!is.character(cell) || length(cell) != 1L || is.na(cell) || nchar(cell) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  # Strip all ${...} variable reference blocks so that brackets inside them
  # are not flagged, then check for any remaining [.
  stripped <- gsub("\\$\\{[^}]*\\}", "VARREF", cell, perl = TRUE)

  if (!grepl("\\[", stripped, fixed = TRUE)) {
    return(list(valid = TRUE, issues = list()))
  }

  msg <- "Cell contains a '[' outside of a '${...}' variable reference."
  list(
    valid  = FALSE,
    issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
  )
}


# ---- 7. Detect unclosed begin_group / begin_repeat blocks ------

#' @title Check XLSForm Survey for Unclosed Group or Repeat Blocks
#'
#' @description
#' In an XLSForm survey each `begin_group` or `begin_repeat` row in the
#' `type` column must be matched by a subsequent `end_group` or `end_repeat`
#' row respectively.  This helper analyses a data frame representing the survey
#' sheet and reports any blocks that are opened but never closed, or closed
#' without having been opened first.
#'
#' @param df       A data frame representing the XLSForm survey sheet. Must
#'   contain a `type` column.
#' @param type_col A character string naming the column that holds question
#'   types (default `"type"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all groups and repeats are properly closed.}
#'   \item{`issues`}{A list of issue items, each a list with `row` (integer
#'     row index) and `message` (character description from `phr_txt`).
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type = c("text", "begin_group", "text", "end_group"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_group_repeats(survey)$valid
#' # TRUE
#'
#' @export
xlsform_check_group_repeats <- function(df, type_col = "type") {
  origin <- "xlsform_check_group_repeats"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(type_col) && length(type_col) == 1L && type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin,
    hint = "Ensure the data frame contains a 'type' column or pass the correct column name."
  )

  types  <- df[[type_col]]
  issues <- list()

  # Separate stacks for groups and repeats
  group_stack  <- integer(0)   # row indices of unmatched begin_group
  repeat_stack <- integer(0)   # row indices of unmatched begin_repeat

  for (i in seq_along(types)) {
    raw_type  <- trimws(as.character(types[[i]]))
    norm_type <- tolower(gsub("[[:space:]]+", "_", raw_type))

    if (norm_type == "begin_group") {
      group_stack <- c(group_stack, i)

    } else if (norm_type == "end_group") {
      if (length(group_stack) == 0L) {
        msg <- paste0("'end_group' at row ", i, " has no matching 'begin_group'.")
        issues <- c(issues, list(list(row = as.integer(i), message = phr_txt(msg, default = msg))))
      } else {
        group_stack <- group_stack[-length(group_stack)]
      }

    } else if (norm_type == "begin_repeat") {
      repeat_stack <- c(repeat_stack, i)

    } else if (norm_type == "end_repeat") {
      if (length(repeat_stack) == 0L) {
        msg <- paste0("'end_repeat' at row ", i, " has no matching 'begin_repeat'.")
        issues <- c(issues, list(list(row = as.integer(i), message = phr_txt(msg, default = msg))))
      } else {
        repeat_stack <- repeat_stack[-length(repeat_stack)]
      }
    }
  }

  for (row_i in group_stack) {
    msg <- paste0("'begin_group' at row ", row_i, " has no matching 'end_group'.")
    issues <- c(issues, list(list(row = as.integer(row_i), message = phr_txt(msg, default = msg))))
  }

  for (row_i in repeat_stack) {
    msg <- paste0("'begin_repeat' at row ", row_i, " has no matching 'end_repeat'.")
    issues <- c(issues, list(list(row = as.integer(row_i), message = phr_txt(msg, default = msg))))
  }

  list(valid = length(issues) == 0L, issues = issues)
}


# ---- 8. Validate required columns in a sheet -------------------

#' @title Check That an XLSForm Sheet Contains Its Required Columns
#'
#' @description
#' Every XLSForm sheet has a set of mandatory columns.  By convention:
#' \itemize{
#'   \item The **survey** sheet requires `type`, `name`, and `label`.
#'   \item The **choices** sheet requires `list_name`, `name`, and `label`.
#'   \item The **settings** sheet requires `form_title` and `form_id`.
#' }
#' You can also supply a fully custom set of required column names via the
#' `required_cols` argument, which takes precedence over the named-sheet
#' defaults.
#'
#' @param df            A data frame representing the XLSForm sheet.
#' @param sheet         One of `"survey"`, `"choices"`, or `"settings"`.
#'   Ignored when `required_cols` is supplied.
#' @param required_cols An optional character vector of column names that must
#'   be present.  When provided, `sheet` is ignored.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all required columns are present.}
#'   \item{`issues`}{A list of issue items, each a list with `row`
#'     (`NA_integer_`, as this is a sheet-level check) and `message`.
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(type = "text", name = "q1", label = "Question 1",
#'                      stringsAsFactors = FALSE)
#' xlsform_check_required_sheet_cols(survey, sheet = "survey")$valid
#' # TRUE
#'
#' @export
xlsform_check_required_sheet_cols <- function(df,
                                               sheet = c("survey", "choices", "settings"),
                                               required_cols = NULL) {
  origin <- "xlsform_check_required_sheet_cols"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)

  if (!is.null(required_cols)) {
    phr_assert(
      is.character(required_cols) && length(required_cols) >= 1L,
      "Argument `required_cols` must be a non-empty character vector.",
      origin = origin
    )
    cols_needed <- required_cols
  } else {
    sheet <- match.arg(sheet)
    cols_needed <- switch(
      sheet,
      survey   = c("type", "name", "label"),
      choices  = c("list_name", "name", "label"),
      settings = c("form_title", "form_id")
    )
  }

  missing_cols <- setdiff(cols_needed, names(df))

  if (length(missing_cols) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  issues <- lapply(missing_cols, function(col) {
    msg <- paste0("Required column '", col, "' is missing from the sheet.")
    list(row = NA_integer_, message = phr_txt(msg, default = msg))
  })

  list(valid = FALSE, issues = issues)
}


# ---- 9. Detect duplicate variable names ------------------------

#' @title Check for Duplicate Variable Names in an XLSForm Survey
#'
#' @description
#' Each question row in an XLSForm must have a unique value in its `name`
#' column.  Duplicate names cause conversion errors and ambiguous references
#' in logic expressions.
#'
#' Structural rows such as `begin_group`, `end_group`, `begin_repeat`, and
#' `end_repeat` are excluded from the check by default.
#'
#' @param df       A data frame representing the XLSForm survey sheet.
#' @param name_col A character string naming the column that contains variable
#'   names (default `"name"`).
#' @param type_col A character string naming the column that contains question
#'   types (default `"type"`).  Structural rows are excluded automatically.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all (non-structural) variable names are unique.}
#'   \item{`issues`}{A list of issue items, one per duplicate row occurrence,
#'     each with `row` (integer row index) and `message`.  Empty when `valid`
#'     is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type = c("text", "integer", "text"),
#'   name = c("q1", "q2", "q1"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_duplicate_names(survey)$valid
#' # FALSE
#'
#' @export
xlsform_check_duplicate_names <- function(df, name_col = "name", type_col = "type") {
  origin <- "xlsform_check_duplicate_names"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(name_col) && length(name_col) == 1L && name_col %in% names(df),
    paste0("Column '", name_col, "' not found in `df`."),
    origin = origin,
    hint = "Ensure the data frame contains a 'name' column or pass the correct column name."
  )

  structural_types <- c(
    "begin_group", "end_group", "begin_repeat", "end_repeat",
    "begin group", "end group", "begin repeat", "end repeat"
  )

  if (type_col %in% names(df)) {
    norm_types    <- tolower(trimws(as.character(df[[type_col]])))
    is_structural <- norm_types %in% structural_types
  } else {
    is_structural <- rep(FALSE, nrow(df))
  }

  # Build a character vector of names for non-structural, non-NA rows
  names_vec <- as.character(df[[name_col]])
  non_empty <- !is_structural & !is.na(names_vec) & nchar(trimws(names_vec)) > 0L

  name_counts <- table(names_vec[non_empty])
  dup_names   <- names(name_counts[name_counts > 1L])

  if (length(dup_names) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  # Report every occurrence of each duplicate name (not just the name itself)
  issues <- list()
  for (nm in dup_names) {
    dup_rows <- which(names_vec == nm & non_empty)
    for (r in dup_rows) {
      msg <- paste0("Variable name '", nm, "' is duplicated (row ", r, ").")
      issues <- c(issues, list(list(row = as.integer(r), message = phr_txt(msg, default = msg))))
    }
  }

  list(valid = FALSE, issues = issues)
}


# ---- 10. Validate a question type string -----------------------

#' @title Check Whether a String Is a Recognized XLSForm Question Type
#'
#' @description
#' XLSForm defines a standard set of question types.  This helper checks
#' whether a given type string is a recognized type after normalising
#' underscores and spaces.  `select_one` and `select_multiple` types are
#' recognised regardless of their trailing list name.
#'
#' @param type_str A single character string representing one cell value from
#'   the `type` column of an XLSForm survey sheet.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if the type is recognized; `FALSE` otherwise.}
#'   \item{`issues`}{A list of issue items.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' xlsform_is_valid_type("text")$valid               # TRUE
#' xlsform_is_valid_type("select_one yn")$valid      # TRUE
#' xlsform_is_valid_type("freetext")$valid           # FALSE
#'
#' @export
xlsform_is_valid_type <- function(type_str) {

  if (!is.character(type_str) || length(type_str) != 1L ||
      is.na(type_str) || nchar(trimws(type_str)) == 0L) {
    msg <- "Type value is missing or not a character string."
    return(list(
      valid  = FALSE,
      issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
    ))
  }

  norm <- tolower(trimws(gsub("[[:space:]]+", " ", gsub("_", " ", type_str))))

  # select_one / select_multiple carry a trailing list name
  if (grepl("^select (one|multiple)(\\s+\\S.*|$)", norm, perl = TRUE)) {
    return(list(valid = TRUE, issues = list()))
  }

  valid_types <- c(
    "text", "integer", "decimal", "range",
    "date", "time", "datetime",
    "begin group", "end group", "begin repeat", "end repeat",
    "file", "image", "audio", "video", "barcode",
    "note", "calculate", "acknowledge", "hidden", "xml-external",
    "geopoint", "geotrace", "geoshape"
  )

  if (norm %in% valid_types) {
    return(list(valid = TRUE, issues = list()))
  }

  msg <- paste0("Type '", type_str, "' is not a recognized XLSForm question type.")
  list(
    valid  = FALSE,
    issues = list(list(row = NA_integer_, message = phr_txt(msg, default = msg)))
  )
}


# ---- 11. Verify choice list references -------------------------

#' @title Check That Select Questions Reference Valid Choice Lists
#'
#' @description
#' For every `select_one` and `select_multiple` question in the survey sheet,
#' the type cell contains a list name (e.g. `"select_one yes_no"`).  This
#' helper verifies that each referenced list name is present in the
#' `list_name` column of the choices sheet.
#'
#' @param survey_df    A data frame representing the XLSForm survey sheet.
#' @param choices_df   A data frame representing the XLSForm choices sheet.
#' @param type_col     Character string naming the type column in `survey_df`
#'   (default `"type"`).
#' @param list_name_col Character string naming the list-name column in
#'   `choices_df` (default `"list_name"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every referenced list name is found.}
#'   \item{`issues`}{A list of issue items, one per survey row whose list name
#'     is absent from choices, each with `row` (integer) and `message`.
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey  <- data.frame(type = c("text", "select_one yes_no"),
#'                       stringsAsFactors = FALSE)
#' choices <- data.frame(list_name = "yes_no", stringsAsFactors = FALSE)
#' xlsform_check_choice_references(survey, choices)$valid
#' # TRUE
#'
#' @export
xlsform_check_choice_references <- function(survey_df, choices_df,
                                             type_col      = "type",
                                             list_name_col = "list_name") {
  origin <- "xlsform_check_choice_references"

  phr_assert(is.data.frame(survey_df),  "Argument `survey_df` must be a data frame.",  origin = origin)
  phr_assert(is.data.frame(choices_df), "Argument `choices_df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(survey_df),
    paste0("Column '", type_col, "' not found in `survey_df`."),
    origin = origin,
    hint = "Pass the correct column name via `type_col`."
  )
  phr_assert(
    list_name_col %in% names(choices_df),
    paste0("Column '", list_name_col, "' not found in `choices_df`."),
    origin = origin,
    hint = "Pass the correct column name via `list_name_col`."
  )

  # Normalise: lower-case, collapse spaces, underscore → space
  types <- trimws(tolower(
    gsub("[[:space:]]+", " ", gsub("_", " ", as.character(survey_df[[type_col]])))
  ))

  select_mask <- grepl("^select (one|multiple)\\s+\\S", types, perl = TRUE)

  available_lists <- unique(as.character(choices_df[[list_name_col]]))
  available_lists <- available_lists[!is.na(available_lists)]

  issues <- list()

  for (i in which(select_mask)) {
    tokens  <- strsplit(types[i], "[[:space:]]+")[[1]]
    list_nm <- if (length(tokens) >= 3L) tokens[3L] else NA_character_

    if (!is.na(list_nm) && !list_nm %in% available_lists) {
      msg <- paste0(
        "Row ", i, ": select question references list '", list_nm,
        "' which is not defined in the choices sheet."
      )
      issues <- c(issues, list(list(row = as.integer(i), message = phr_txt(msg, default = msg))))
    }
  }

  list(valid = length(issues) == 0L, issues = issues)
}


# ---- 12. Check that questions have labels ----------------------

#' @title Flag XLSForm Questions Missing a Label
#'
#' @description
#' Every user-facing question in an XLSForm should have a value in the
#' `label` column.  Structural and non-visible row types (`begin_group`,
#' `end_group`, `begin_repeat`, `end_repeat`, `calculate`, `hidden`,
#' `note`) do not require a label and are excluded from the check.
#'
#' @param df        A data frame representing the XLSForm survey sheet.
#' @param type_col  Character string naming the type column (default `"type"`).
#' @param label_col Character string naming the label column (default
#'   `"label"`).  The column may be absent; if so every non-structural row
#'   is flagged.
#' @param name_col  Character string naming the name column (default `"name"`),
#'   used in diagnostic messages.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every user-facing question has a label.}
#'   \item{`issues`}{A list of issue items, one per unlabelled row, each with
#'     `row` (integer) and `message`.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type  = c("text", "integer"),
#'   name  = c("q1", "q2"),
#'   label = c("Your name", NA),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_label_presence(survey)$valid
#' # FALSE
#'
#' @export
xlsform_check_label_presence <- function(df,
                                          type_col  = "type",
                                          label_col = "label",
                                          name_col  = "name") {
  origin <- "xlsform_check_label_presence"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin
  )

  no_label_types <- c(
    "begin_group", "end_group", "begin_repeat", "end_repeat",
    "begin group", "end group", "begin repeat", "end repeat",
    "calculate", "hidden", "note"
  )

  types      <- trimws(tolower(as.character(df[[type_col]])))
  base_types <- vapply(types, function(t) strsplit(t, "[[:space:]]+")[[1]][1], character(1L))
  needs_label <- !base_types %in% no_label_types

  if (!label_col %in% names(df)) {
    unlabelled <- which(needs_label)
  } else {
    labels        <- df[[label_col]]
    label_missing <- is.na(labels) | trimws(as.character(labels)) == ""
    unlabelled    <- which(needs_label & label_missing)
  }

  if (length(unlabelled) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  issues <- lapply(as.integer(unlabelled), function(i) {
    nm  <- if (name_col %in% names(df)) as.character(df[[name_col]][i]) else NA_character_
    msg <- if (!is.na(nm) && nchar(trimws(nm)) > 0L) {
      paste0("Question '", nm, "' at row ", i, " is missing a label.")
    } else {
      paste0("Row ", i, ": question is missing a label.")
    }
    list(row = i, message = phr_txt(msg, default = msg))
  })

  list(valid = FALSE, issues = issues)
}


# ---- 13. Verify calculate rows have an expression --------------

#' @title Check That All 'calculate' Rows Have a Calculation Expression
#'
#' @description
#' In XLSForm, a row of type `calculate` must have a non-empty value in the
#' `calculation` column.  A missing or empty calculation makes the question
#' evaluate to `NULL` and is almost certainly a coding error.
#'
#' @param df              A data frame representing the XLSForm survey sheet.
#' @param type_col        Character string naming the type column
#'   (default `"type"`).
#' @param calculation_col Character string naming the calculation column
#'   (default `"calculation"`).
#' @param name_col        Character string naming the name column
#'   (default `"name"`), used in diagnostic messages.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every `calculate` row has a non-empty expression.}
#'   \item{`issues`}{A list of issue items, one per empty calculate row, each
#'     with `row` (integer) and `message`.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type        = c("text", "calculate"),
#'   name        = c("q1", "bmi"),
#'   calculation = c(NA, NA),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_calculate_expression(survey)$valid
#' # FALSE
#'
#' @export
xlsform_check_calculate_expression <- function(df,
                                                type_col        = "type",
                                                calculation_col = "calculation",
                                                name_col        = "name") {
  origin <- "xlsform_check_calculate_expression"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin
  )

  types     <- trimws(tolower(as.character(df[[type_col]])))
  calc_rows <- which(types == "calculate")

  if (length(calc_rows) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  if (!calculation_col %in% names(df)) {
    empty_rows <- calc_rows
  } else {
    calculations <- df[[calculation_col]]
    is_empty     <- is.na(calculations) | trimws(as.character(calculations)) == ""
    empty_rows   <- calc_rows[is_empty[calc_rows]]
  }

  if (length(empty_rows) == 0L) {
    return(list(valid = TRUE, issues = list()))
  }

  issues <- lapply(as.integer(empty_rows), function(i) {
    nm  <- if (name_col %in% names(df)) as.character(df[[name_col]][i]) else NA_character_
    msg <- if (!is.na(nm) && nchar(trimws(nm)) > 0L) {
      paste0("Calculate variable '", nm, "' at row ", i, " has no calculation expression.")
    } else {
      paste0("Row ", i, ": calculate row has no calculation expression.")
    }
    list(row = i, message = phr_txt(msg, default = msg))
  })

  list(valid = FALSE, issues = issues)
}


# ---- 14. Check for undefined variable references ---------------

#' @title Check That Variable References in Expression Columns Are Declared
#'
#' @description
#' Scans one or more expression columns (e.g. `relevant`, `constraint`,
#' `calculation`) for XLSForm `${}` variable references and verifies that
#' every referenced variable is declared in the `name` column.
#'
#' Internally calls `xlsform_extract_variables()` for each cell to extract
#' variable names, then compares against the declared names.
#'
#' @param df         A data frame representing the XLSForm survey sheet.
#'   Must contain a `name` column.
#' @param name_col   Character string naming the column of declared variable
#'   names (default `"name"`).
#' @param check_cols Character vector of column names to scan for `${}`
#'   references.  Columns absent from `df` are silently skipped.
#'   Default: `c("relevant", "constraint", "calculation")`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every variable reference in the scanned columns
#'     is declared in `name_col`.}
#'   \item{`issues`}{A list of issue items, one per undefined reference found,
#'     each with `row` (integer row index) and `message`.  Empty when `valid`
#'     is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type      = c("text", "text"),
#'   name      = c("age", "weight"),
#'   relevant  = c(NA, "${height} > 0"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_undefined_references(survey)$valid
#' # FALSE – 'height' is not declared
#'
#' @export
xlsform_check_undefined_references <- function(df,
                                                name_col   = "name",
                                                check_cols = c("relevant", "constraint", "calculation")) {
  origin <- "xlsform_check_undefined_references"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(name_col) && length(name_col) == 1L && name_col %in% names(df),
    paste0("Column '", name_col, "' not found in `df`."),
    origin = origin,
    hint = "Pass the correct column name via `name_col`."
  )

  declared_names <- as.character(df[[name_col]])
  declared_names <- declared_names[!is.na(declared_names) & nchar(trimws(declared_names)) > 0L]

  existing_check_cols <- intersect(check_cols, names(df))

  issues <- list()

  for (col in existing_check_cols) {
    for (i in seq_len(nrow(df))) {
      # Use the general extract helper to find all ${...} variable names
      refs      <- xlsform_extract_variables(df[[col]][i])
      undefined <- refs[!refs %in% declared_names]

      for (ref in undefined) {
        msg <- paste0(
          "Row ", i, " ('", col, "' column): variable '${", ref,
          "}' is not declared in the '", name_col, "' column."
        )
        issues <- c(issues, list(list(row = as.integer(i), message = phr_txt(msg, default = msg))))
      }
    }
  }

  list(valid = length(issues) == 0L, issues = issues)
}
