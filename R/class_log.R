#' Log: Log Base Class
#'
#' @description
#' A foundational R6 class for managing structured log data with validation, schema enforcement,
#' and export capabilities. Used as a base class for specialized logs (CleaningLog, DeletionLog).
#'
#' @details
#' The Log class provides:
#' * Storage and management of structured log entries in a data.frame
#' * Schema definition and validation (column types, allowed values)
#' * Required column enforcement
#' * Metadata tracking (timestamps, validation status)
#' * Export to multiple formats (CSV, RDS, XLSX)
#' * Issue tracking during validation
#' * Hook methods for customization (pre_validate, post_validate)
#' * Safe getter and setter methods for accessing private fields
#'
#' Private fields (accessible via get() and set() methods):
#' * log_df: A tibble containing the log entries
#' * log_name: Character name of the log for display purposes
#' * required_columns: Character vector of columns that must exist
#' * schema: List with 'types' and 'allowed_values' for validation
#' * validated: Logical indicating if log has passed validation
#' * metadata: List of metadata including update timestamps
#'
#' @field autosave Logical for automatic saving (not yet implemented)
#' @field issues List of validation issues found
#'
#' @importFrom R6 R6Class
#' @export
Log <- R6::R6Class(
  classname = "Log",

  public = list(
    # Fields

    autosave = FALSE,

    issues = NULL, # <- must be declared here

    #' Get Field Value
    #'
    #' @description
    #' Safely retrieves a private field value
    #'
    #' @param field Character name of the field to retrieve.
    #'   Must be one of: "log_df", "log_name", "required_columns", "schema", "validated", "metadata"
    #'
    #' @return The value of the requested field
    #'
    #' @examples
    #' \dontrun{
    #' log <- Log$new()
    #' log$get("log_df")
    #' log$get("log_name")
    #' }
    get = function(field) {
      allowed_fields <- self$allowed_get_fields()

      if (!field %in% allowed_fields) {
        phrutils::phr_error(
          "Log",
          phr_txt(glue::glue(
            "Field '{field}' is not accessible. Allowed fields: {paste(allowed_fields, collapse=', ')}"
          ))
        )
      }

      return(private[[field]])
    },
    #' Accessible Fields Hook
    #'
    #' @description
    #' Returns the list of fields accessible via get().
    #' Subclasses can override to expose additional fields.
    #'
    #' @return Character vector of allowed field names
    allowed_get_fields = function() {
      c(
        "log_df",
        "log_name",
        "required_columns",
        "schema",
        "validated",
        "metadata",
        self$additional_get_fields()
      )
    },

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    additional_get_fields = function() {
      character(0)
    },

    #' Settable Fields Hook
    #'
    #' @description
    #' Returns the list of fields that may be modified via `set()`.
    #'
    #' @return Character vector of field names.
    allowed_set_fields = function() {
      c(
        "log_df",
        "log_name",
        "required_columns",
        "schema",
        "validated",
        "metadata",
        self$additional_set_fields()
      )
    },

    #' Additional Settable Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields that may be modified via `set()`.
    #'
    #' @return Character vector of field names.
    additional_set_fields = function() {
      character(0)
    },

    #' Set Field Value
    #'
    #' @description
    #' Safely sets a private field value
    #'
    #' @param field Character name of the field to set.
    #'   Must be one of: "log_df", "log_name", "required_columns", "schema", "validated", "metadata"
    #' @param value The value to assign to the field
    #'
    #' @return Invisible TRUE
    #'
    #' @examples
    #' \dontrun{
    #' log <- Log$new()
    #' log$set("log_name", "My Custom Log")
    #' log$set("validated", TRUE)
    #' }
    set = function(field, value) {
      allowed_fields <- self$allowed_set_fields()

      if (!field %in% allowed_fields) {
        phrutils::phr_error(
          "Log",
          phr_txt(glue::glue(
            "Field '{field}' is not settable. Allowed fields: {paste(allowed_fields, collapse=', ')}"
          ))
        )
      }

      # Validation for specific fields
      if (field == "log_df" && !is.null(value)) {
        phrutils::phr_validate_dataframe(
          value,
          origin = "Log$set",
          soft = FALSE
        )
      }

      private[[field]] <- value
      private$..touch()

      phrutils::phr_message(phr_txt(glue::glue(
        "Field '{field}' updated in {private$log_name}."
      )))

      invisible(TRUE)
    },

    #' @description
    #' Creates a new Log object with optional initial data, required columns, and schema
    #'
    #' @param log_df Optional data.frame of log entries; if NULL, creates empty log
    #' @param log_name Character name for the log (default: "Log")
    #' @param required_columns Character vector of required column names
    #' @param schema List with 'types' (column_name = type) and 'allowed_values' (column_name = vector)
    #'
    #' @return A new Log R6 object
    #'
    #' @details
    #' Initialization process:
    #' 1. Creates empty log template if log_df is NULL
    #' 2. Validates that log_df is a data.frame
    #' 3. Adds missing required columns (filled with NA)
    #' 4. Reorders columns (required columns first)
    #' 5. Initializes metadata with current timestamp
    initialize = function(
      log_df = NULL,
      log_name = "Log",
      required_columns = NULL,
      schema = NULL
    ) {
      private$log_name <- log_name
      private$required_columns <- required_columns %||% character(0)
      private$schema <- schema %||% list()

      # ---- CREATE BLANK LOG IF NONE PROVIDED
      if (is.null(log_df)) {
        log_df <- private$empty_log_template()
      }

      # ---- VALIDATION: must be a data.frame
      phrutils::phr_validate_dataframe(
        log_df,
        origin = paste0(log_name, "$initialize"),
        soft = FALSE
      )

      # ---- Ensure required columns exist (add empty ones if missing)
      for (col in private$required_columns) {
        if (!col %in% names(log_df)) {
          log_df[[col]] <- NA_character_
        }
      }

      # reorder columns (required first)
      log_df <- log_df[,
        unique(c(private$required_columns, names(log_df))),
        drop = FALSE
      ]

      private$log_df <- tibble::as_tibble(log_df)

      # guarantee metadata exists
      timestamp <- Sys.time()
      private$metadata$created_datetime <- timestamp
      private$metadata$modified_datetime <- timestamp

      self$issues <- list()

      phrutils::phr_message(phr_txt(glue::glue(
        "{log_name} initialized with {nrow(log_df)} entries."
      )))
    },

    #' Set Schema
    #'
    #' @description
    #' Assigns a validation schema to the log
    #'
    #' @param schema_list List with 'types' and/or 'allowed_values' elements
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Schema format:
    #' * types: named list of column_name = "character"/"numeric"/"logical"/"date"
    #' * allowed_values: named list of column_name = vector of allowed values
    set_schema = function(schema_list) {
      private$schema <- schema_list
      phrutils::phr_message(phr_txt(glue::glue("Schema attached to {private$log_name}.")))
      invisible(TRUE)
    },

    #' Validate Log
    #'
    #' @description
    #' Validates log structure, required columns, and schema compliance
    #'
    #' @param schema_override Optional schema list to use instead of self$schema
    #'
    #' @return Invisible list of validation issues (empty list if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Checks log_df is a valid data.frame
    #' 2. Verifies all required columns exist
    #' 3. Type checking and safe coercion for schema types
    #' 4. Allowed values checking for categorical columns
    #'
    #' Sets self$validated to TRUE if no issues found, updates self$issues.
    #' Attempts safe type coercion (e.g., "123" to 123) when possible.
    validate = function(schema_override = NULL) {
      phrutils::phr_try(
        {
          schema_to_use <- schema_override %||% private$schema
          issues <- list()

          # 1. BASIC STRUCTURAL VALIDATION

          phrutils::phr_validate_dataframe(
            private$log_df,
            origin = private$log_name,
            soft = TRUE
          )

          # Required columns must exist
          missing_req <- setdiff(private$required_columns, names(private$log_df))
          if (length(missing_req) > 0) {
            issues$missing_required_columns <- missing_req
          }

          # 2. SCHEMA VALIDATION (types + allowed_values)

          if (!is.null(schema_to_use) && length(schema_to_use) > 0) {
            # ---- TYPE CHECKS
            if (!is.null(schema_to_use$types)) {
              for (nm in names(schema_to_use$types)) {
                if (!nm %in% names(private$log_df)) {
                  next
                }

                want <- schema_to_use$types[[nm]]
                got <- class(private$log_df[[nm]])[1]

                # Safe coercion classification
                coercible <- phrutils::is_safely_coercible(private$log_df[[nm]], want)

                # Soft-coercion when safe
                if (!identical(got, want) && coercible) {
                  new_vec <- tryCatch(
                    {
                      if (want == "numeric") {
                        suppressWarnings(as.numeric(private$log_df[[nm]]))
                      } else if (want == "character") {
                        as.character(private$log_df[[nm]])
                      } else if (want == "logical") {
                        suppressWarnings(as.logical(private$log_df[[nm]]))
                      } else if (want == "date" || want == "Date") {
                        phrutils::phr_convert_date(private$log_df[[nm]])
                      } else {
                        private$log_df[[nm]]
                      }
                    },
                    error = function(e) private$log_df[[nm]]
                  )

                  private$log_df[[nm]] <- new_vec

                  phrutils::phr_message(
                    phr_txt(glue::glue(
                      "Coerced column '{nm}' to type '{want}'."
                    ))
                  )
                }

                # Re-check type after possible coercion
                final_type <- class(private$log_df[[nm]])[1]

                if (!identical(final_type, want)) {
                  issues$type_mismatch[[nm]] <- paste0(
                    "want=",
                    want,
                    ", got=",
                    final_type
                  )
                }
              }
            }

            # ---- ALLOWED VALUES CHECK
            if (!is.null(schema_to_use$allowed_values)) {
              for (nm in names(schema_to_use$allowed_values)) {
                if (!nm %in% names(private$log_df)) {
                  next
                }

                allowed <- schema_to_use$allowed_values[[nm]]
                vals <- unique(private$log_df[[nm]])

                bad <- setdiff(vals, allowed)

                if (length(bad) > 0) {
                  issues$disallowed_values[[nm]] <- bad
                }
              }
            }
          }

          # 3. FINALIZE

          if (length(issues) > 0) {
            phrutils::phr_warning(
              private$log_name,
              phr_txt(glue::glue(
                "Log validation completed with issues: {paste(names(issues), collapse=', ')}"
              ))
            )
          }

          private$validated <- length(issues) == 0
          self$issues <- issues %||% list()

          private$..touch()

          invisible(issues)
        },
        on_error = "abort",
        origin = paste0(private$log_name, "$validate")
      )
    },

    #' Append Log Entry
    #'
    #' @description
    #' Adds a new row to the log
    #'
    #' @param row_list Named list with column_name = value pairs
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Validates that all required columns are present in row_list.
    #' Converts list to data.frame row and appends via dplyr::bind_rows().
    #' Updates metadata timestamp.
    append_entry = function(row_list) {
      if (!is.list(row_list)) {
        phrutils::phr_error(
          private$log_name,
          phr_txt("append_entry() requires a named list.")
        )
      }

      missing_cols <- setdiff(private$required_columns, names(row_list))
      if (length(missing_cols) > 0) {
        phrutils::phr_error(
          private$log_name,
          phr_txt(glue::glue(
            "Missing required fields in new log entry: {paste(missing_cols, collapse=', ')}"
          ))
        )
      }

      # align names
      df_row <- as.data.frame(row_list, stringsAsFactors = FALSE)
      private$log_df <- dplyr::bind_rows(private$log_df, df_row)

      private$..touch()

      phrutils::phr_message(phr_txt(glue::glue("Added new entry to {private$log_name}.")))
      invisible(TRUE)
    },

    #' Clear Log
    #'
    #' @description
    #' Removes all entries from the log while preserving structure
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Keeps column structure intact but sets row count to 0.
    #' Updates metadata timestamp.
    clear = function() {
      private$log_df <- private$log_df[0, , drop = FALSE]
      private$..touch()
      phrutils::phr_message(phr_txt(glue::glue("{private$log_name} cleared.")))
      invisible(TRUE)
    },

    #' Export Log
    #'
    #' @description
    #' Exports log data to file in specified format
    #'
    #' @param path Character file path for export
    #' @param format Character; one of "csv", "rds", or "xlsx"
    #'
    #' @return Invisible path to exported file
    #'
    #' @details
    #' Supported formats:
    #' * csv: Comma-separated values via utils::write.csv
    #' * rds: R data structure via saveRDS
    #' * xlsx: Excel workbook via openxlsx (requires openxlsx package)
    export = function(path, format = c("csv", "rds", "xlsx")) {
      format <- match.arg(format)

      phrutils::phr_try(
        {
          if (format == "csv") {
            utils::write.csv(private$log_df, path, row.names = FALSE)
          }
          if (format == "rds") {
            saveRDS(private$log_df, path)
          }
          if (format == "xlsx") {
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              phrutils::phr_error(
                private$log_name,
                phr_txt("Package 'openxlsx' is required for XLSX export.")
              )
            }
            openxlsx::write.xlsx(private$log_df, file = path)
          }

          phrutils::phr_message(phr_txt(glue::glue(
            "Exported {private$log_name} to {path}."
          )))
          invisible(path)
        },
        on_error = "abort",
        origin = paste0(private$log_name, "$export")
      )
    },

    #' Get Hash Fingerprint
    #'
    #' @description
    #' Computes a hash of the log data for integrity checking
    #'
    #' @return Character hash digest of log_df
    #'
    #' @details
    #' Uses digest package to create a unique fingerprint of the log contents.
    #' Useful for detecting changes or verifying data integrity.
    #'
    #' @note Requires digest package
    get_hash = function() {
      if (!requireNamespace("digest", quietly = TRUE)) {
        phrutils::phr_error(
          private$log_name,
          phr_txt("Package 'digest' is required for hashing.")
        )
      }
      digest::digest(private$log_df)
    },

    #' Get Summary
    #'
    #' @description
    #' Returns a summary list with key log information
    #'
    #' @return Named list with:
    #'   * log_name: Name of the log
    #'   * n_entries: Number of rows
    #'   * n_columns: Number of columns
    #'   * validated: Validation status
    #'   * required_columns: Vector of required column names
    #'   * schema_attached: Whether schema is defined
    #'   * last_updated: Timestamp of last modification
    summary = function() {
      list(
        log_name = private$log_name,
        n_entries = nrow(private$log_df),
        n_columns = ncol(private$log_df),
        validated = private$validated,
        required_columns = private$required_columns,
        schema_attached = !is.null(private$schema),
        last_updated = private$metadata$modified_datetime
      )
    },

    #' Pre-Validation Hook
    #'
    #' @description
    #' Hook method called before validation; override in subclasses for custom logic
    #'
    #' @return NULL (default implementation is empty)
    pre_validate = function() {},

    #' Post-Validation Hook
    #'
    #' @description
    #' Hook method called after validation; override in subclasses for custom logic
    #'
    #' @return NULL (default implementation is empty)
    post_validate = function() {}
  ),

  private = list(
    # Private fields
    log_df = NULL,
    log_name = NULL,
    required_columns = NULL,
    schema = NULL,
    validated = FALSE,
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    # Build Empty Log Template
    #
    # Creates an empty tibble with correct column types from schema
    #
    # Returns: Empty tibble with required columns and proper types
    #
    # Uses schema$types to create appropriately typed empty vectors for each
    # required column. Defaults to character type if no schema type specified.
    empty_log_template = function() {
      cols <- private$required_columns %||% character(0)

      # If schema has types, use them for empty vectors
      if (!is.null(private$schema$types)) {
        df <- lapply(cols, function(col) {
          type <- private$schema$types[[col]] %||% "character"

          if (type == "numeric") {
            return(numeric())
          } else if (type == "logical") {
            return(logical())
          } else if (type == "date") {
            return(as.Date(character()))
          } else {
            return(character())
          }
        })
      } else {
        # default all-character template
        df <- lapply(cols, function(col) character())
      }

      names(df) <- cols
      tibble::as_tibble(df)
    },
    # @description Update modified timestamp.
    # @return Invisibly returns NULL.
    # @keywords internal
    ..touch = function() {
      private$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    }
  )
)
