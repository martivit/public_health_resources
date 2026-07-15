
# R6 Class: Tool (Base Class for XLSForm Management)

#
# This module provides the base Tool R6 class for managing XLSForm data used
# in Kobo, ODK, and other data collection applications.
#
# The Tool class provides the foundation for the tool class hierarchy:
# - Tool: Base class (this file)
# - HouseholdTool: Subclass for household survey tools (class_household_tool.R)
# - KeyInformantTool: Subclass for key informant interview tools (class_key_informant_tool.R)
# - ObservationTool: Subclass for observation/checklist tools (class_observation_tool.R)
#
# Key Features:
# - Load and manage survey and choices sheets from XLSForm
# - Filter questions by selected indicators
# - Validate XLSForm structure according to specification
# - Modify select question options and choices
#
# XLSForm Reference: https://xlsform.org/en/



# Null-coalescing operator (if not already available)
# This is also defined in utils_session.R but we define it here to ensure
# availability regardless of file load order


if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}


# R6 Class: Tool (Base Class)


#' @title Tool R6 Class for XLSForm Management
#' @description
#' Base R6 class for managing XLSForm data collection tools used with
#' Kobo, ODK, and other mobile data collection platforms.
#'
#' An XLSForm consists of at least two sheets:
#' - survey: Contains the form structure (questions, groups, etc.)
#' - choices: Contains choice lists for select questions
#' - settings: Contains form-level settings (title, language, version)
#'
#' This class maintains both master (template) and revised (filtered/modified)
#' versions of the survey, choices, and settings sheets.  The master copies
#' (\code{survey}, \code{choices}, \code{settings}) are loaded from a default
#' XLSForm file on initialisation and are never altered by filter or
#' modification methods.  The revised copies (\code{revised_survey},
#' \code{revised_choices}, \code{revised_settings}) are initialised as copies
#' of the masters and updated by methods such as \code{filter_survey_by_indicator()}.
#'
#' This class provides methods for:
#' - Loading and storing XLSForm data (master and revised)
#' - Filtering the survey and choices by indicator codes (\code{filter_survey_by_indicator()})
#' - Safely updating specific choice lists with new values
#' - Changing the default language in revised_settings (english, french, arabic, spanish)
#' - Validating the revised survey against available revised choices
#' - Validating structure according to XLSForm specification
#'
#' @importFrom readxl read_excel
#' @export
Tool <- R6::R6Class(

"Tool",
cloneable = TRUE,
public = list(

  #' @field survey Data frame containing the survey sheet of the XLSForm.
  survey = NULL,

  #' @field choices Data frame containing the choices sheet of the XLSForm.
  choices = NULL,

  #' @field settings Data frame containing the settings sheet of the XLSForm.
  settings = NULL,

  #' @field revised_survey Working copy of the survey sheet, updated by filter
  #'   or modification methods.  Initialised as a copy of \code{survey} on
  #'   construction.
  revised_survey = NULL,

  #' @field revised_choices Working copy of the choices sheet, updated by
  #'   filter or modification methods.  Initialised as a copy of \code{choices}
  #'   on construction.
  revised_choices = NULL,

  #' @field revised_settings Working copy of the settings sheet, updated by
  #'   modification methods.  Initialised as a copy of \code{settings} on
  #'   construction.
  revised_settings = NULL,

  #' @field metadata List containing tool metadata including
  #'   \code{created_datetime} and \code{modified_datetime}.
  metadata = list(
    created_datetime = NULL,
    modified_datetime = NULL
  ),


  # Initialization


  #' @description
  #' Initialize a new Tool object.
  #'
  #' @param name Optional name for the tool.
  #' @param survey Optional data.frame containing the survey sheet (master).
  #' @param choices Optional data.frame containing the choices sheet (master).
  #' @param settings Optional data.frame containing the settings sheet.
  #' @return A new Tool object.
  initialize = function(name = NULL,
                        survey = NULL,
                        choices = NULL,
                        settings = NULL) {
    private$.name <- name %||% "Untitled Tool"
    private$.created_at <- Sys.time()
    private$.modified_at <- Sys.time()
    self$metadata$created_datetime <- private$.created_at
    self$metadata$modified_datetime <- private$.modified_at
    private$.tool_type <- "generic"

    # Initialize data frames with required columns if not provided
    self$survey <- if (!is.null(survey)) {
      private$.validate_survey_structure(survey)
      survey
    } else {
      data.frame(type = character(0), name = character(0), label = character(0),
                 stringsAsFactors = FALSE)
    }

    self$choices <- if (!is.null(choices)) {
      private$.validate_choices_structure(choices)
      choices
    } else {
      data.frame(list_name = character(0), name = character(0), label = character(0),
                 stringsAsFactors = FALSE)
    }

    self$settings <- if (!is.null(settings)) {
      settings
    } else {
      data.frame(form_title = character(0), form_id = character(0),
                 version = character(0), stringsAsFactors = FALSE)
    }

    # Populate revised copies from the originals
    self$revised_survey   <- self$survey
    self$revised_choices  <- self$choices
    self$revised_settings <- self$settings

    private$.selected_indicators <- character(0)
    private$.validation_errors <- list()

    invisible(self)
  },


  # Data Accessors


  #' @description
  #' Update tool modification metadata timestamps.
  #' @return Invisibly returns \code{self}.
  touch = function() {
    private$.modified_at <- Sys.time()
    self$metadata$modified_datetime <- private$.modified_at
    invisible(self)
  },

  #' @description
  #' Get the tool name.
  #' @return Character string with tool name.
  get_name = function() {
    private$.name
  },

  #' @description
  #' Set the tool name.
  #' @param name Character string for the tool name.
  #' @return Invisibly returns self for method chaining.
  set_name = function(name) {
    if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
      stop("Tool name must be a non-empty character string")
    }
    private$.name <- name
    self$touch()
    invisible(self)
  },

  #' @description
  #' Get the tool type.
  #' @return Character string indicating the tool type.
  get_tool_type = function() {
    private$.tool_type
  },


  # Tool Modification Methods


  #' @description
  #' Change the default language stored in the revised_settings data frame.
  #' Updates the \code{default_language} column of \code{revised_settings}.
  #' Allowable options: \code{"english"}, \code{"french"}, \code{"arabic"},
  #' \code{"spanish"} (case-insensitive).
  #' @param language Character string for the new default language.
  #' @return Invisibly returns self for method chaining.
  change_default_language = function(language) {
    allowable       <- c("english", "french", "arabic", "spanish")
    allowable_title <- c("English", "French", "Arabic", "Spanish")
    if (!is.character(language) || length(language) != 1 || nchar(language) == 0) {
      stop("language must be a non-empty character string")
    }
    if (!tolower(language) %in% allowable) {
      stop(paste0("language must be one of: ",
                  paste(allowable_title, collapse = ", "),
                  ". Got: '", language, "'"))
    }
    # Store normalised to title-case
    language_norm  <- tolower(language)
    language_title <- paste0(toupper(substring(language_norm, 1, 1)),
                             substring(language_norm, 2))
    if (is.null(self$revised_settings) || nrow(self$revised_settings) == 0) {
      # Create a single-row revised_settings frame when none exists
      self$revised_settings <- data.frame(
        form_title       = NA_character_,
        form_id          = NA_character_,
        version          = NA_character_,
        default_language = language_title,
        stringsAsFactors = FALSE
      )
    } else {
      if (!"default_language" %in% names(self$revised_settings)) {
        self$revised_settings[["default_language"]] <- NA_character_
      }
      self$revised_settings[["default_language"]] <- language_title
    }
    self$touch()
    invisible(self)
  },

  #' @description
  #' Filter the master survey and choices by a vector of \code{indicator_code}
  #' values and store the results in \code{revised_survey} and
  #' \code{revised_choices}.
  #'
  #' Rows in \code{survey} whose \code{indicator_code} column contains at least
  #' one of the supplied codes are retained (using a grepl pattern match to
  #' support comma-separated multi-indicator cells).  Code \code{10000} is
  #' always included in the search regardless of the supplied argument, so
  #' general/structural questions shared across all indicators are preserved.
  #' The \code{revised_choices} is then derived from \code{revised_survey} by
  #' retaining only choice list rows whose \code{indicator_code} column matches
  #' one of the indicator codes present in \code{revised_survey}.
  #'
  #' @param indicator_codes Numeric or character vector (or list) of
  #'   \code{indicator_code} values to retain.
  #' @return Invisibly returns \code{self} for method chaining.
  filter_survey_by_indicator = function(indicator_codes) {
    if (is.null(indicator_codes) || length(indicator_codes) == 0) {
      stop("indicator_codes must be a non-empty vector or list")
    }
    indicator_codes <- as.character(unlist(indicator_codes))
    # Always include 10000
    if (!"10000" %in% indicator_codes) {
      indicator_codes <- c(indicator_codes, "10000")
    }

    # Create a vector of unique indicator codes ending in 00 (special lines)
    # These are based on the first three digits of passed indicator codes
    # e.g., from "14631" -> "14600"
    # Expected format: indicator codes are at least 3 characters long (e.g., "14631")
    special_codes <- paste0(substr(indicator_codes, 1, 3), "00")

    # Combine all codes: original and special codes ending in 00
    # (10000 is already included via indicator_codes at this point)
    all_filter_codes <- unique(c(indicator_codes, special_codes))

    sv <- self$survey

    if (nrow(sv) == 0 || !"indicator_code" %in% names(sv)) {
      self$revised_survey  <- sv
      self$revised_choices <- self$choices
      self$touch()
      return(invisible(self))
    }

    # Build a regex pattern that matches any of the indicator codes as
    # whole values in a possibly comma-separated cell.
    pattern <- paste(all_filter_codes, collapse = "|")

    col_vals    <- as.character(sv[["indicator_code"]])
    is_selected <- !is.na(col_vals) & grepl(pattern, col_vals)

    self$revised_survey  <- sv[is_selected, , drop = FALSE]
    self$revised_choices <- private$.filter_choices_from_survey(all_filter_codes)

    self$touch()
    invisible(self)
  },

  #' @description
  #' Safely update a specific choice list in the revised choices with new
  #' or revised values.  The existing entries for \code{list_name} are
  #' replaced entirely with \code{new_choices}.
  #'
  #' @param list_name Character. Name of the choice list to replace.
  #' @param new_choices Data frame with the new choice entries.  Must contain
  #'   at minimum a \code{name} column.  A \code{list_name} column will be
  #'   added/overwritten with the value of \code{list_name}.
  #' @return Invisibly returns self for method chaining.
  update_choice_list = function(list_name, new_choices) {
    if (!is.character(list_name) || length(list_name) != 1 || nchar(list_name) == 0) {
      stop("list_name must be a non-empty character string")
    }
    if (!is.data.frame(new_choices)) {
      stop("new_choices must be a data.frame")
    }
    if (!"name" %in% names(new_choices)) {
      stop("new_choices must have a 'name' column")
    }

    # Ensure list_name column is set correctly
    new_choices[["list_name"]] <- list_name

    # Target: revised_choices (falls back to master choices if revised is empty)
    target <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }

    # Align columns: add any missing columns as NA
    all_cols <- union(names(target), names(new_choices))
    for (col in setdiff(all_cols, names(target))) {
      target[[col]] <- NA
    }
    for (col in setdiff(all_cols, names(new_choices))) {
      new_choices[[col]] <- NA
    }
    new_choices <- new_choices[, names(target), drop = FALSE]

    # Remove existing entries for this list and append new ones
    self$revised_choices <- rbind(
      target[target$list_name != list_name, , drop = FALSE],
      new_choices
    )

    self$touch()
    invisible(self)
  },

  #' @description Add/replace a numbered choice list in revised choices.
  #' @param list_name Character choice list name.
  #' @param prefix_val Character value prefix.
  #' @param prefix_lbl Character label prefix.
  #' @param n Positive integer number of entries.
  #' @return Invisibly returns \code{self}.
  add_numbered_list = function(list_name, prefix_val, prefix_lbl, n) {
    if (!is.character(list_name) || length(list_name) != 1L || !nzchar(list_name)) {
      stop("list_name must be a non-empty character string")
    }
    if (!is.character(prefix_val) || length(prefix_val) != 1L) {
      stop("prefix_val must be a single character string")
    }
    if (!is.character(prefix_lbl) || length(prefix_lbl) != 1L) {
      stop("prefix_lbl must be a single character string")
    }
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) {
      stop("n must be a single positive integer")
    }
    n <- as.integer(n)

    choices <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }
    if (is.null(choices)) {
      choices <- data.frame(list_name = character(0), name = character(0),
                            label = character(0), stringsAsFactors = FALSE)
    }
    if ("list_name" %in% names(choices)) {
      choices <- choices[choices$list_name != list_name, , drop = FALSE]
    }
    new_rows <- lapply(seq_len(n), function(i) {
      row <- data.frame(
        list_name = list_name,
        name = paste0(prefix_val, i),
        label = paste0(prefix_lbl, i),
        stringsAsFactors = FALSE
      )
      for (col in setdiff(names(choices), names(row))) row[[col]] <- NA
      row
    })
    new_df <- do.call(rbind, new_rows)
    all_cols <- union(names(choices), names(new_df))
    for (col in setdiff(all_cols, names(choices))) choices[[col]] <- NA
    for (col in setdiff(all_cols, names(new_df))) new_df[[col]] <- NA
    self$revised_choices <- rbind(choices[, all_cols, drop = FALSE], new_df[, all_cols, drop = FALSE])
    self$touch()
    invisible(self)
  },

  #' @description Add/replace a \code{teams} choice list.
  #' @param n_teams Positive integer number of teams.
  #' @return Invisibly returns \code{self}.
  add_teams_to_choices = function(n_teams) {
    self$add_numbered_list(
      list_name = "teams",
      prefix_val = "team_",
      prefix_lbl = "Team ",
      n = n_teams
    )
  },

  #' @description Add/replace an \code{enumerators} choice list.
  #' @param n_enumerators Positive integer number of enumerators.
  #' @return Invisibly returns \code{self}.
  add_enumerators_to_choices = function(n_enumerators) {
    self$add_numbered_list(
      list_name = "enumerators",
      prefix_val = "enumerator_",
      prefix_lbl = "Enumerator ",
      n = n_enumerators
    )
  },

  #' @description
  #' Update a value in the settings data frame.
  #' @param key Character. Column name in the settings data frame to update.
  #' @param value The new value for the setting.
  #' @return Invisibly returns self for method chaining.
  update_settings = function(key, value) {
    if (!is.character(key) || length(key) != 1 || nchar(key) == 0) {
      stop("key must be a non-empty character string")
    }
    if (nrow(self$settings) == 0) {
      # Create a single-row settings frame when none exists
      self$settings <- data.frame(
        form_title = NA_character_,
        form_id    = NA_character_,
        version    = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    if (!key %in% names(self$settings)) {
      self$settings[[key]] <- NA
    }
    self$settings[[key]] <- value
    self$touch()
    invisible(self)
  },


  # Indicator Filtering


  #' @description
  #' Return a unique integer vector of all indicator codes present in the
  #' \code{indicator_code} column of \code{revised_survey} (falling back to
  #' \code{survey}).  Cells that contain multiple comma-separated codes (e.g.
  #' \code{"10501, 10502"}) are split so every individual code is returned as
  #' a separate element.
  #'
  #' @param prefer_revised Logical.  When \code{TRUE} (default) use
  #'   \code{revised_survey}; otherwise use the master \code{survey}.
  #' @return Integer vector of unique indicator codes.  Returns
  #'   \code{integer(0)} when the sheet is empty or has no
  #'   \code{indicator_code} column.
  get_indicator_codes = function(prefer_revised = TRUE) {
    sv <- if (prefer_revised && !is.null(self$revised_survey) &&
              is.data.frame(self$revised_survey) && nrow(self$revised_survey) > 0) {
      self$revised_survey
    } else {
      self$survey
    }
    if (is.null(sv) || !is.data.frame(sv) || nrow(sv) == 0 ||
        !"indicator_code" %in% names(sv)) {
      return(integer(0))
    }
    codes_raw <- as.character(sv[["indicator_code"]])
    codes_raw <- codes_raw[!is.na(codes_raw) & nzchar(trimws(codes_raw))]
    if (length(codes_raw) == 0L) return(integer(0))
    # Split on commas (with optional surrounding whitespace) to handle cells
    # that store multiple codes such as "10501, 10502".
    codes_split <- unlist(strsplit(codes_raw, "[[:space:]]*,[[:space:]]*"))
    codes_split <- trimws(codes_split)
    codes_split <- codes_split[nzchar(codes_split)]
    codes_int   <- suppressWarnings(as.integer(codes_split))
    codes_int   <- codes_int[!is.na(codes_int)]
    # Filter out codes ending in 00 (special "line" codes in the tool that represent
    # summary/aggregate indicators rather than actual indicators to measure)
    codes_int   <- codes_int[codes_int %% 100 != 0]
    unique(codes_int)
  },

  #' @description
  #' Get the currently selected indicators.
  #' @return Character vector of selected indicator names.
  get_selected_indicators = function() {
    private$.selected_indicators
  },

  #' @description
  #' Set the selected indicators for filtering.
  #' @param indicators Character vector of indicator names to select.
  #' @return Invisibly returns self for method chaining.
  set_selected_indicators = function(indicators) {
    if (!is.character(indicators)) {
      stop("Indicators must be a character vector")
    }
    private$.selected_indicators <- indicators
    self$touch()
    invisible(self)
  },

  #' @description
  #' Get the survey sheet filtered by selected indicators.
  #' Filters questions based on a mapping between indicators and question names.
  #' @param indicator_mapping Named list mapping indicator names to question names.
  #' @return Data frame with filtered survey rows.
  get_filtered_survey = function(indicator_mapping = NULL) {
    if (is.null(indicator_mapping) || length(private$.selected_indicators) == 0) {
      return(self$survey)
    }

    # Get question names for selected indicators
    selected_questions <- unlist(indicator_mapping[private$.selected_indicators])
    selected_questions <- unique(selected_questions)

    if (length(selected_questions) == 0) {
      return(self$survey)
    }

    # Filter survey to include selected questions and structural elements
    survey <- self$survey
    if (!"name" %in% names(survey)) {
      return(survey)
    }

    # Always include begin/end group, begin/end repeat, notes, and calculate types
    structural_types <- c("begin_group", "end_group", "begin_repeat", "end_repeat",
                          "note", "calculate", "start", "end", "today",
                          "deviceid", "username", "audit")

    is_structural <- if ("type" %in% names(survey)) {
      survey$type %in% structural_types
    } else {
      rep(FALSE, nrow(survey))
    }

    is_selected <- survey$name %in% selected_questions

    survey[is_structural | is_selected, , drop = FALSE]
  },

  #' @description
  #' Get the choices sheet filtered to only include choice lists used by the filtered survey questions.
  #' @param indicator_mapping Named list mapping indicator names to question names.
  #' @return Data frame with filtered choices rows.
  get_filtered_choices = function(indicator_mapping = NULL) {
    filtered_survey <- self$get_filtered_survey(indicator_mapping)

    if (!"type" %in% names(filtered_survey)) {
      return(self$choices)
    }

    # Extract choice list names from select_one and select_multiple questions
    select_types <- filtered_survey$type[grepl("^select_", filtered_survey$type)]
    list_names <- gsub("^select_(one|multiple)\\s+", "", select_types)
    list_names <- gsub("\\s+.*$", "", list_names)  # Remove anything after list name
    list_names <- unique(list_names)

    if (length(list_names) == 0 || !"list_name" %in% names(self$choices)) {
      return(self$choices)
    }

    self$choices[self$choices$list_name %in% list_names, , drop = FALSE]
  },


  # XLSForm Validation


  #' @description
  #' Validate the entire XLSForm structure.
  #' Checks for required columns, valid question types, choice list references,
  #' and coherence between revised survey and revised choices.
  #' @return Logical TRUE if valid, FALSE if there are validation errors.
  validate = function() {
    private$.validation_errors <- list()

    # Validate survey sheet structure and content
    private$.validate_survey_content()

    # Validate choices sheet structure and content
    private$.validate_choices_content()

    # Validate references between master survey and master choices
    private$.validate_references()

    # Validate coherence between revised survey and revised choices
    private$.validate_revised_coherence()

    length(private$.validation_errors) == 0
  },

  #' @description
  #' Get validation errors from the last validation run.
  #' @return List of validation error messages.
  get_validation_errors = function() {
    private$.validation_errors
  },

  #' @description
  #' Check if the tool is valid (has no validation errors).
  #' @return Logical indicating validity.
  is_valid = function() {
    self$validate()
  },

  #' @description
  #' Get all choices for a specific choice list (from master choices).
  #' @param list_name Name of the choice list.
  #' @return Data frame with choices for the specified list.
  get_choices_for_list = function(list_name) {
    self$choices[self$choices$list_name == list_name, , drop = FALSE]
  },

  #' @description
  #' Get all unique choice list names (from master choices).
  #' @return Character vector of choice list names.
  get_choice_list_names = function() {
    unique(self$choices$list_name)
  }
),

private = list(
  .name = NULL,
  .tool_type = NULL,
  .created_at = NULL,
  .modified_at = NULL,
  .selected_indicators = NULL,
  .validation_errors = NULL,

  # Load a default XLSForm from an xlsx file.
  # Reads the survey, choices, and settings sheets (if present) and stores
  # them as the master copies.  Called by subclass constructors.
  .load_default_tool = function(path) {
    if (!file.exists(path)) {
      warning(paste("Default tool file not found:", path))
      return(invisible(NULL))
    }

    available_sheets <- tryCatch(
      readxl::excel_sheets(path),
      error = function(e) {
        warning(paste("Could not read sheets from", path, ":", conditionMessage(e)))
        return(character(0))
      }
    )

    if ("survey" %in% available_sheets) {
      sv <- tryCatch(
        as.data.frame(readxl::read_excel(path, sheet = "survey")),
        error = function(e) {
          warning(paste("Could not read survey sheet:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(sv)) {
        self$survey <- sv
        self$revised_survey <- sv
      }
    }

    if ("choices" %in% available_sheets) {
      ch <- tryCatch(
        as.data.frame(readxl::read_excel(path, sheet = "choices")),
        error = function(e) {
          warning(paste("Could not read choices sheet:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(ch)) {
        self$choices <- ch
        self$revised_choices <- ch
      }
    }

    if ("settings" %in% available_sheets) {
      st <- tryCatch(
        as.data.frame(readxl::read_excel(path, sheet = "settings")),
        error = function(e) {
          warning(paste("Could not read settings sheet:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(st)) {
        self$settings <- st
        self$revised_settings <- st
      }
    }

    invisible(NULL)
  },

  # Validate survey data frame structure (called on construction / assignment)
  .validate_survey_structure = function(survey) {
    result <- xlsform_check_required_sheet_cols(survey, required_cols = c("type", "name"))
    if (!result$valid) {
      msgs <- vapply(result$issues, function(i) i$message, character(1L))
      stop(paste("Survey sheet missing required columns:", paste(msgs, collapse = "; ")))
    }
  },

  # Validate choices data frame structure (called on construction / assignment)
  .validate_choices_structure = function(choices) {
    result <- xlsform_check_required_sheet_cols(choices, required_cols = c("list_name", "name"))
    if (!result$valid) {
      msgs <- vapply(result$issues, function(i) i$message, character(1L))
      stop(paste("Choices sheet missing required columns:", paste(msgs, collapse = "; ")))
    }
  },

  # Validate survey content (types, duplicates, group balance) against master survey
  .validate_survey_content = function() {
    sv <- self$survey
    if (is.null(sv) || nrow(sv) == 0) return(invisible(NULL))

    # Check required columns
    struct_result <- xlsform_check_required_sheet_cols(sv, sheet = "survey")
    if (!struct_result$valid) {
      private$.validation_errors <- c(private$.validation_errors, struct_result$issues)
    }

    # Check group/repeat balance
    if ("type" %in% names(sv)) {
      grp_result <- xlsform_check_group_repeats(sv)
      if (!grp_result$valid) {
        private$.validation_errors <- c(private$.validation_errors, grp_result$issues)
      }
    }

    # Check duplicate variable names
    if ("name" %in% names(sv)) {
      dup_result <- xlsform_check_duplicate_names(sv)
      if (!dup_result$valid) {
        private$.validation_errors <- c(private$.validation_errors, dup_result$issues)
      }
    }

    # Check each question type using utility function.
    # Meta/system types that are always valid but not in xlsform_is_valid_type's
    # list are skipped here so we don't produce false positives.
    if ("type" %in% names(sv)) {
      meta_types <- c("start", "end", "today", "deviceid", "username",
                      "phonenumber", "simserial", "subscriberid", "audit",
                      "rank")
      for (i in seq_len(nrow(sv))) {
        raw_type  <- sv$type[i]
        base_type <- tolower(trimws(gsub("\\s+.*$", "", as.character(raw_type))))
        if (is.na(base_type) || base_type %in% meta_types) next
        type_result <- xlsform_is_valid_type(raw_type)
        if (!type_result$valid) {
          issues <- lapply(type_result$issues, function(iss) {
            iss$row <- as.integer(i)
            iss
          })
          private$.validation_errors <- c(private$.validation_errors, issues)
        }
      }
    }
    invisible(NULL)
  },

  # Validate choices content (duplicate names within each list) against master choices
  .validate_choices_content = function() {
    ch <- self$choices
    if (is.null(ch) || nrow(ch) == 0) return(invisible(NULL))

    # Check required columns
    struct_result <- xlsform_check_required_sheet_cols(ch, sheet = "choices")
    if (!struct_result$valid) {
      private$.validation_errors <- c(private$.validation_errors, struct_result$issues)
      return(invisible(NULL))
    }

    # Check for duplicate choice names within each list
    for (list_nm in unique(ch$list_name)) {
      if (is.na(list_nm)) next
      list_ch <- ch[ch$list_name == list_nm, , drop = FALSE]
      if ("name" %in% names(list_ch)) {
        dup_names <- list_ch$name[duplicated(list_ch$name) & !is.na(list_ch$name)]
        if (length(dup_names) > 0) {
          msg <- paste0("Duplicate choice names in list '", list_nm, "': ",
                        paste(unique(dup_names), collapse = ", "))
          private$.validation_errors <- c(
            private$.validation_errors,
            list(list(row = NA_integer_, column = "name",
                      message = phr_txt(msg, default = msg)))
          )
        }
      }
    }
    invisible(NULL)
  },

  # Validate that select questions in master survey reference valid master choice lists
  .validate_references = function() {
    sv <- self$survey
    ch <- self$choices
    if (is.null(sv) || is.null(ch)) return(invisible(NULL))
    if (!"type" %in% names(sv) || nrow(sv) == 0) return(invisible(NULL))
    if (!"list_name" %in% names(ch)) return(invisible(NULL))

    result <- xlsform_check_choice_references(sv, ch)
    if (!result$valid) {
      private$.validation_errors <- c(private$.validation_errors, result$issues)
    }
    invisible(NULL)
  },

  # Validate coherence between revised survey and revised choices
  .validate_revised_coherence = function() {
    sv <- if (!is.null(self$revised_survey) && nrow(self$revised_survey) > 0) {
      self$revised_survey
    } else {
      self$survey
    }
    ch <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }
    if (is.null(sv) || is.null(ch)) return(invisible(NULL))
    if (!"type" %in% names(sv) || nrow(sv) == 0) return(invisible(NULL))
    if (!"list_name" %in% names(ch)) return(invisible(NULL))

    result <- xlsform_check_choice_references(sv, ch)
    if (!result$valid) {
      # Tag these as revised-coherence issues so they're distinguishable
      issues <- lapply(result$issues, function(iss) {
        iss$message <- paste0("[revised] ", iss$message)
        iss
      })
      private$.validation_errors <- c(private$.validation_errors, issues)
    }
    invisible(NULL)
  },

  # Private helper: filter the master choices data frame using the supplied
  # indicator_codes vector.  Uses grepl to handle comma-separated multi-indicator
  # cells in the choices indicator_code column.
  .filter_choices_from_survey = function(indicator_codes) {
    ch <- self$choices
    if (is.null(ch) || !"indicator_code" %in% names(ch)) {
      return(if (is.null(ch)) NULL else ch[0L, , drop = FALSE])
    }
    if (is.null(indicator_codes) || length(indicator_codes) == 0) {
      return(ch[0L, , drop = FALSE])
    }
    indicator_codes <- as.character(unlist(indicator_codes))
    indicator_codes <- unique(trimws(indicator_codes))
    indicator_codes <- indicator_codes[nzchar(indicator_codes)]
    if (length(indicator_codes) == 0) return(ch[0L, , drop = FALSE])
    pattern  <- paste(indicator_codes, collapse = "|")
    ch_codes <- as.character(ch[["indicator_code"]])
    keep     <- !is.na(ch_codes) & grepl(pattern, ch_codes)
    ch[keep, , drop = FALSE]
  },

  # Private helper: check whether a question exists in revised_survey
  .has_question = function(name) {
    !is.null(self$revised_survey) &&
      "name" %in% names(self$revised_survey) &&
      name %in% self$revised_survey$name
  },

  # Private helper: return a specific question row from revised_survey (or NULL)
  .get_question = function(name) {
    if (!private$.has_question(name)) return(NULL)
    self$revised_survey[self$revised_survey$name == name, , drop = FALSE]
  },

  # Private helper: remove a question from revised_survey by name
  .remove_question = function(name) {
    if (!private$.has_question(name)) {
      warning(paste("Question", name, "not found in revised_survey"))
      return(invisible(self))
    }
    self$revised_survey <- self$revised_survey[
      self$revised_survey$name != name, , drop = FALSE
    ]
    self$touch()
    invisible(self)
  },

  # Private helper: update column values of a question in revised_survey
  .update_question = function(name, ...) {
    if (!private$.has_question(name)) {
      stop(paste("Question", name, "not found in revised_survey"))
    }
    updates <- list(...)
    row_idx <- which(self$revised_survey$name == name)
    for (col_name in names(updates)) {
      if (!col_name %in% names(self$revised_survey)) {
        self$revised_survey[[col_name]] <- NA
      }
      self$revised_survey[row_idx, col_name] <- updates[[col_name]]
    }
    self$touch()
    invisible(self)
  },

  # Private helper: insert a new question row into revised_survey at a specific position.
  # Use `after` (question name or row index) to insert after that position, or
  # `before` (question name or row index) to insert before it.
  # If neither is supplied the row is appended at the end.
  .add_question = function(new_row, after = NULL, before = NULL) {
    sv <- self$revised_survey
    if (is.null(sv) || nrow(sv) == 0) {
      self$revised_survey <- new_row
      self$touch()
      return(invisible(self))
    }

    # Align columns
    all_cols <- union(names(sv), names(new_row))
    for (col in setdiff(all_cols, names(sv)))     sv[[col]]      <- NA
    for (col in setdiff(all_cols, names(new_row))) new_row[[col]] <- NA
    new_row <- new_row[, all_cols, drop = FALSE]
    sv      <- sv[, all_cols, drop = FALSE]

    # Resolve insertion position as an integer index (insert AFTER this row)
    insert_after <- NULL
    if (!is.null(after)) {
      if (is.character(after) && "name" %in% names(sv)) {
        idx <- which(sv$name == after)
        if (length(idx) > 0) insert_after <- idx[1L]
      } else if (is.numeric(after)) {
        insert_after <- as.integer(after)
      }
    } else if (!is.null(before)) {
      if (is.character(before) && "name" %in% names(sv)) {
        idx <- which(sv$name == before)
        if (length(idx) > 0) insert_after <- idx[1L] - 1L
      } else if (is.numeric(before)) {
        insert_after <- as.integer(before) - 1L
      }
    }

    n <- nrow(sv)
    if (!is.null(insert_after) && insert_after >= 1L && insert_after < n) {
      self$revised_survey <- rbind(
        sv[seq_len(insert_after), , drop = FALSE],
        new_row,
        sv[seq(insert_after + 1L, n), , drop = FALSE]
      )
    } else if (!is.null(insert_after) && insert_after <= 0L) {
      self$revised_survey <- rbind(new_row, sv)
    } else {
      self$revised_survey <- rbind(sv, new_row)
    }
    self$touch()
    invisible(self)
  },

  # Private helper: add a single choice row to revised_choices
  .add_choice = function(list_name, name, label, ...) {
    ch <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }
    if (is.null(ch)) {
      ch <- data.frame(list_name = character(0), name = character(0),
                       label = character(0), stringsAsFactors = FALSE)
    }
    new_row <- data.frame(list_name = list_name, name = name, label = label,
                          stringsAsFactors = FALSE)
    extra_args <- list(...)
    for (col_name in names(extra_args)) new_row[[col_name]] <- extra_args[[col_name]]
    for (col in names(ch)) if (!col %in% names(new_row)) new_row[[col]] <- NA
    self$revised_choices <- rbind(ch, new_row[, names(ch), drop = FALSE])
    self$touch()
    invisible(self)
  },

  # Private helper: remove choice(s) from revised_choices by list name and optionally
  # by choice name. When name is NULL the entire list is removed.
  .remove_choice = function(list_name, name = NULL) {
    ch <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }
    if (is.null(ch)) return(invisible(self))
    if (is.null(name)) {
      ch <- ch[ch$list_name != list_name, , drop = FALSE]
    } else {
      ch <- ch[!(ch$list_name == list_name & ch$name == name), , drop = FALSE]
    }
    self$revised_choices <- ch
    self$touch()
    invisible(self)
  },

  # Private helper: update column values of a choice in revised_choices
  .update_choice = function(list_name, name, ...) {
    ch <- if (!is.null(self$revised_choices) && nrow(self$revised_choices) > 0) {
      self$revised_choices
    } else {
      self$choices
    }
    if (is.null(ch)) stop(paste("Choice", name, "in list", list_name, "not found"))
    row_idx <- which(ch$list_name == list_name & ch$name == name)
    if (length(row_idx) == 0) stop(paste("Choice", name, "in list", list_name, "not found"))
    updates <- list(...)
    for (col_name in names(updates)) {
      if (!col_name %in% names(ch)) ch[[col_name]] <- NA
      ch[row_idx, col_name] <- updates[[col_name]]
    }
    self$revised_choices <- ch
    self$touch()
    invisible(self)
  }
)
)
