#' Objective Schema Utility Functions
#'
#' @description
#' Functions for loading and validating the objective schema used in Protocol
#' planning.  The default objectives schema is loaded from the bundled
#' \code{reference_objectives.xlsx} resource file, and the indicator bank is
#' loaded from \code{reference_indicator_bank.xlsx}.  Together these files list
#' standard humanitarian objectives and their associated indicators across
#' sectors (General, Health, FSL, WASH, Nutrition, Shelter).
#'
#' @name objective_schema_utils
NULL


# Required columns that every objective schema data frame must contain.
.objective_schema_required_cols <- c(
  "sector",
  "pillar",
  "sub_pillar",
  "short_objective",
  "text_objective",
  "objective_code"
)

# All columns present in the bundled reference_objectives.xlsx file.
.objective_schema_all_cols <- c(
  "objective_code",
  "sector",
  "pillar",
  "sub_pillar",
  "short_objective",
  "text_objective",
  "objective_research_question",
  "objective_threshold_name",
  "objective_threshold_description"
)

# All columns present in the bundled reference_indicator_bank.xlsx file.
.indicator_bank_all_cols <- c(
  "objective_code",
  "data_source",
  "indicator_code",
  "dep_indicator_code",
  "tool",
  "indicator_name",
  "indicator_definition",
  "indicator_numerator",
  "indicator_denominator",
  "research_question",
  "citation",
  "threshold_name",
  "threshold_value",
  "citation_threshold"
)


#' Load the default objective schema from the bundled reference file
#'
#' Reads the \code{reference_objectives.xlsx} resource file that ships with the
#' package and returns its contents as a data frame.  Each row represents a
#' standard humanitarian objective.
#'
#' @return A data frame with objective schema columns:
#'   \code{objective_code}, \code{sector}, \code{pillar}, \code{sub_pillar},
#'   \code{short_objective}, \code{text_objective},
#'   \code{objective_research_question}, \code{objective_threshold_name},
#'   \code{objective_threshold_description}.
#'   Returns an empty data frame if the file cannot be found or read.
#' @export
load_objective_schema <- function() {
  file <- system.file("resources", "reference_objectives.xlsx", package = "phr")

  if (!file.exists(file) || file == "") {
    phrutils::phr_warning(
      origin = "load_objective_schema",
      message = phr_txt(
        "Could not locate reference_objectives.xlsx; returning empty objective schema."
      ),
      hint = phr_txt(
        "Ensure the 'inst/resources/reference_objectives.xlsx' file is present in the package installation."
      )
    )
    return(data.frame())
  }

  schema <- phrutils::phr_try(
    readxl::read_xlsx(file),
    on_error = "warn",
    origin = "load_objective_schema",
    hint = "Check that reference_objectives.xlsx is a valid Excel file."
  )

  if (is.null(schema) || nrow(schema) == 0) {
    return(data.frame())
  }

  # Coerce to plain data frame
  schema <- as.data.frame(schema, stringsAsFactors = FALSE)

  schema
}


#' Load the indicator bank from the bundled reference file
#'
#' Reads the \code{reference_indicator_bank.xlsx} resource file that ships with
#' the package and returns its contents as a data frame.  Each row represents
#' a standard indicator associated with a humanitarian objective.
#'
#' @return A data frame with indicator bank columns:
#'   \code{objective_code}, \code{data_source}, \code{indicator_code},
#'   \code{tool}, \code{indicator_name}, \code{indicator_definition},
#'   \code{indicator_numerator}, \code{indicator_denominator},
#'   \code{research_question}, \code{citation}, \code{threshold_name},
#'   \code{threshold_value}, \code{citation_threshold}.
#'   Returns an empty data frame if the file cannot be found or read.
#' @export
load_indicator_bank <- function() {
  file <- system.file(
    "resources",
    "reference_indicator_bank.xlsx",
    package = "phr"
  )

  if (!file.exists(file) || file == "") {
    phrutils::phr_warning(
      origin = "load_indicator_bank",
      message = phr_txt(
        "Could not locate reference_indicator_bank.xlsx; returning empty indicator bank."
      ),
      hint = phr_txt(
        "Ensure the 'inst/resources/reference_indicator_bank.xlsx' file is present in the package installation."
      )
    )
    return(data.frame())
  }

  bank <- phrutils::phr_try(
    readxl::read_xlsx(file),
    on_error = "warn",
    origin = "load_indicator_bank",
    hint = "Check that reference_indicator_bank.xlsx is a valid Excel file."
  )

  if (is.null(bank) || nrow(bank) == 0) {
    return(data.frame())
  }

  # Coerce to plain data frame
  bank <- as.data.frame(bank, stringsAsFactors = FALSE)

  bank
}


#' Validate an objective schema data frame
#'
#' Checks that a data frame conforms to the expected objective schema
#' structure: required columns are present, key columns are complete (not
#' all-NA), and text columns are character typed.  Duplicate rows are
#' permitted because the schema may contain multiple rows per objective when
#' each row corresponds to a distinct indicator.
#'
#' Columns that must be of character type:
#' \code{sector}, \code{pillar}, \code{sub_pillar}, \code{text_objective}.
#' \code{short_objective} is allowed to be character or numeric/integer (it
#' is used as a code and may be imported as an integer).
#'
#' @param schema A data frame to validate as an objective schema.
#' @param soft Logical.  If \code{TRUE}, issues warnings instead of errors
#'   for recoverable problems; hard structural problems always abort.
#'   Defaults to \code{FALSE}.
#' @return Invisibly returns \code{TRUE} if the schema is valid.  Throws an
#'   error (or warning, when \code{soft = TRUE}) if validation fails.
#' @export
validate_objective_schema <- function(schema, soft = FALSE) {
  origin <- "validate_objective_schema"

  phrutils::phr_try(
    {
      # Must be a non-NULL data frame
      if (is.null(schema) || !is.data.frame(schema)) {
        phr_error(
          origin = origin,
          message = phr_txt("Objective schema must be a data frame."),
          hint = phr_txt(
            "Use load_objective_schema() to obtain the default schema."
          )
        )
      }

      # Must have at least one row
      if (nrow(schema) == 0) {
        msg <- phr_txt("Objective schema is empty (zero rows).")
        if (soft) {
          phrutils::phr_warning(origin = origin, message = msg)
          return(invisible(FALSE))
        }
        phr_error(origin = origin, message = msg)
      }

      # Check required columns are present
      missing_cols <- setdiff(.objective_schema_required_cols, names(schema))
      if (length(missing_cols) > 0) {
        phr_error(
          origin = origin,
          message = phr_txt(
            glue::glue(
              "Objective schema is missing required column(s): {paste(missing_cols, collapse = ', ')}"
            )
          ),
          hint = phr_txt(
            glue::glue(
              "Required columns are: {paste(.objective_schema_required_cols, collapse = ', ')}"
            )
          )
        )
      }

      # Completeness: sector must not be all-NA
      if (all(is.na(schema$sector))) {
        phr_error(
          origin = origin,
          message = phr_txt(
            "All 'sector' values in the objective schema are NA."
          )
        )
      }

      # Completeness: short_objective must not be all-NA
      if (all(is.na(schema$short_objective))) {
        msg <- phr_txt(
          "All 'short_objective' values in the objective schema are NA."
        )
        if (soft) {
          phrutils::phr_warning(origin = origin, message = msg)
        } else {
          phr_error(origin = origin, message = msg)
        }
      }

      # Type checks: text columns must be character type (factor is also accepted
      # as it is coercible to character, but callers should use stringsAsFactors = FALSE)
      char_cols <- c("sector", "pillar", "sub_pillar", "text_objective")
      bad_types <- char_cols[sapply(char_cols, function(col) {
        col %in%
          names(schema) &&
          !is.character(schema[[col]]) &&
          !is.factor(schema[[col]])
      })]
      if (length(bad_types) > 0) {
        msg <- phr_txt(
          glue::glue(
            "The following column(s) should be character (or factor): {paste(bad_types, collapse = ', ')}"
          )
        )
        if (soft) {
          phrutils::phr_warning(origin = origin, message = msg)
        } else {
          phr_error(origin = origin, message = msg)
        }
      }

      invisible(TRUE)
    },
    on_error = "abort",
    origin = origin
  )
}


#' Filter the objective schema by sector
#'
#' Returns the subset of an objective schema data frame that matches one or
#' more sectors.
#'
#' @param schema A data frame produced by \code{load_objective_schema()}.
#' @param sectors Character vector of sector names to keep.
#' @return A filtered data frame.
#' @export
filter_objective_schema_by_sector <- function(schema, sectors) {
  validate_objective_schema(schema, soft = FALSE)
  schema[schema$sector %in% sectors, ]
}


#' Return the required column names for an objective schema
#'
#' Convenience helper that lists the mandatory columns every objective schema
#' data frame must contain.
#'
#' @return Character vector of required column names.
#' @export
objective_schema_required_cols <- function() {
  .objective_schema_required_cols
}
