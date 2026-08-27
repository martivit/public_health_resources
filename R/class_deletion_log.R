#' DeletionLog: Deletion Log Class
#'
#' @description
#' A specialized Log class for tracking records marked for deletion from the dataset.
#' Records which observations should be removed and the reasons for deletion.
#'
#' @details
#' The DeletionLog extends the Log base class with specific functionality for:
#' * Recording which survey records should be deleted
#' * Tracking reasons for deletion (data quality issues, duplicates, etc.)
#' * Validating that UUIDs exist in the dataset before deletion
#' * Providing audit trail for removed observations
#'
#' Required columns:
#' * uuid: Unique identifier for the record to be deleted
#' * enum_id: Enumerator/interviewer identifier
#' * device_id: Device identifier where data was collected
#' * issue: Description of why the record should be deleted
#' * feedback: Additional comments or resolution notes
#'
#' @keywords internal
DeletionLog <- R6::R6Class(
  classname = "DeletionLog",
  inherit = Log,

  public = list(


    #' @description
    #' Creates a new DeletionLog with default schema and required columns
    #'
    #' @param log_df Optional data.frame of existing deletion log entries
    #' @param log_name Character name for the log (default: "Deletion Log")
    #' @param required_columns Character vector; defaults to standard deletion log columns
    #' @param schema List; defaults to standard deletion log schema (all character types)
    #'
    #' @return A new DeletionLog R6 object
    #'
    #' @details
    #' Sets up default schema with all character types for required columns.
    #' Calls parent Log$initialize() to complete setup.
    initialize = function(log_df = NULL,
                          log_name = "Deletion Log",
                          required_columns = NULL,
                          schema = NULL) {

      required_columns <- required_columns %||% c(
        "uuid",
        "enum_id",
        "device_id",
        "issue",
        "feedback"
      )

      schema <- schema %||% list(
        types = list(
          uuid      = "character",
          enum_id   = "character",
          device_id = "character",
          issue     = "character",
          feedback  = "character"
        )
      )

      super$initialize(
        log_df = log_df,
        log_name = log_name,
        required_columns = required_columns,
        schema = schema
      )
    },

    #' Add Deletion Entry
    #'
    #' @description
    #' Helper method to add a deletion entry with required fields
    #'
    #' @param uuid Character; unique identifier of record to delete
    #' @param enum_id Character; enumerator identifier (optional, defaults to NA)
    #' @param device_id Character; device identifier (optional, defaults to NA)
    #' @param issue Character; description of why record should be deleted
    #' @param feedback Character; additional comments (optional, defaults to NA)
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Coerces all inputs to character and calls append_entry() to add to log.
    add_deletion = function(uuid,
                            enum_id   = NA_character_,
                            device_id = NA_character_,
                            issue,
                            feedback  = NA_character_) {

      entry <- list(
        uuid      = as.character(uuid),
        enum_id   = as.character(enum_id),
        device_id = as.character(device_id),
        issue     = as.character(issue),
        feedback  = as.character(feedback)
      )

      self$append_entry(entry)
    },



    #' Validate Deletion Log
    #'
    #' @description
    #' Validates deletion log structure and optionally checks alignment with dataset
    #'
    #' @param check_against Optional Data object to validate log entries against
    #' @param stage Character; data stage to validate against ("raw", "standardized", "clean")
    #'
    #' @return Invisible list of validation issues (empty if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Calls parent Log$validate() for basic structure and schema validation
    #' 2. Checks that critical fields (uuid, issue) are non-empty
    #' 3. If check_against provided, validates that:
    #'    - UUIDs in deletion log exist in the dataset
    #'    - Issues are recorded for each deletion
    #'
    #' Sets self$validated and self$issues based on results.
    validate = function(check_against = NULL, stage = "clean") {

      phrutils::phr_try({

        # ---- 1. Run base Log validation first ----
        super_issues <- super$validate()
        if (is.null(super_issues)) super_issues <- list()

        df <- self$log_df


        # ---- 2. Deletion-log completeness rules ----
        required_nonempty <- c("uuid", "issue")

        incomplete_cols <- c()
        for (col in required_nonempty) {
          if (any(is.na(df[[col]]) | trimws(df[[col]]) == "")) {
            incomplete_cols <- c(incomplete_cols, col)
          }
        }

        if (length(incomplete_cols) > 0) {
          super_issues$missing_or_empty <- incomplete_cols

          phrutils::phr_warning(
            self$log_name,
            phr_txt(
              "Deletion log contains missing or empty values in: {paste(incomplete_cols, collapse=', ')}."
            )
          )
        }


        # ---- 3. POST VALIDATE (dataset alignment), optional
        if (!is.null(check_against)) {

          post_issues <- self$post_validate(check_against, stage = stage)

          # integrate issues (post_validate returns list or NULL)
          if (!is.null(post_issues)) {
            for (nm in names(post_issues)) {
              super_issues[[nm]] <- post_issues[[nm]]
            }
          }
        }


        # ---- 4. Store issues + update validated flag
        self$issues <- super_issues
        self$validated <- length(self$issues) == 0

        invisible(self$issues)

      }, on_error = "abort", origin = "DeletionLog$validate")

    },

    #' Post-Validate Against Dataset
    #'
    #' @description
    #' Validates deletion log entries against an actual dataset
    #'
    #' @param data_obj Data object containing the dataset to validate against
    #' @param stage Character; which data stage to use ("raw", "standardized", "clean")
    #'
    #' @return List of validation issues (empty list if no issues)
    #'
    #' @details
    #' Checks for:
    #' * Missing UUIDs: UUIDs in deletion log that don't exist in dataset
    #'
    #' Helps identify if the deletion log references records that are already removed
    #' or never existed. Issues are returned as warnings for review.
    post_validate = function(data_obj, stage = "clean") {

      # If log is empty, nothing to validate
      if (nrow(self$log_df) == 0) {
        return(list())   # no issues
      }

      out <- list()

      phrutils::phr_try({

        df <- self$log_df

        # Get data from the specified stage
        data_df <- data_obj$get_data(stage)

        # ---- Dataset must have UUID column
        if (!data_obj$uuid %in% names(data_df)) {
          phr_error(
            message = paste0("UUID column '", data_obj$uuid, "' not found in dataset."),
            origin = "DeletionLog$post_validate"
          )
        }

        # ---- Check missing UUIDs
        missing_uuid <- setdiff(df$uuid, unique(as.character(data_df[[data_obj$uuid]])))
        if (length(missing_uuid) > 0) {
          out$missing_uuid <- missing_uuid

          phrutils::phr_warning(
            "DeletionLog",
            phr_txt(
              "The following UUIDs in deletion log do not exist in dataset: {paste(missing_uuid, collapse=', ')}"
            )
          )
        }

        invisible(out)

      }, on_error = "abort", origin = "DeletionLog$post_validate")
    }
  )
)
