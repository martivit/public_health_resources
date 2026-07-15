#' Orchestrator R6 Class
#'
#' @description
#' Base orchestration class that provides generalized nested-object access and
#' synchronization hooks.
#'
#' @importFrom R6 R6Class
Orchestrator <- R6::R6Class(
  "Orchestrator",
  public = list(
    #' @field metadata List containing orchestration metadata.
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    #' @description
    #' Creates a new Orchestrator object.
    #' @return A new Orchestrator object.
    initialize = function() {
      self$metadata$created_datetime <- Sys.time()
      self$metadata$modified_datetime <- Sys.time()
      invisible(self)
    },

    #' @description Hook executed before \code{sync_state()} logic.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    pre_sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      invisible(NULL)
    },

    #' @description Hook executed after \code{sync_state()} logic.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    post_sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      invisible(NULL)
    },

    #' @description Synchronize orchestrator state.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{self}.
    sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      private$..sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      invisible(self)
    },

    #' @description
    #' Generalized accessor for nested objects stored on the class.
    #'
    #' Resolves a top-level field (for example \code{tools} or \code{framework}),
    #' optionally resolves a named element if the field is a list, and then
    #' returns a nested field or invokes a nested method.
    #'
    #' @param field Character scalar naming the stored top-level field.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field}.
    #' @param member Optional character scalar naming a field or method on the
    #'   resolved object.
    #' @param role Optional character scalar used to resolve a list element in
    #'   \code{field} by role-like name (for example \code{"household"} for
    #'   \code{"tool_household_iphra_v2"}).
    #' @param ... Arguments passed to the nested method when \code{member}
    #'   resolves to a function.
    #' @return The requested nested value, or the nested method result.
    access_nested = function(
      field,
      name = NULL,
      member = NULL,
      role = NULL,
      update_sync = FALSE,
      update_modified = TRUE,
      ...
    ) {
      phr_try(
        {
          target <- private$..resolve_nested_target(
            field = field,
            name = name,
            role = role
          )

          if (is.null(member)) {
            if (update_sync) {
              private$..sync_state()
            }
            if (update_modified) {
              private$..touch()
            }
            return(target)
          }

          phr_assert(
            is.character(member) && length(member) == 1L && nzchar(member),
            message = phr_txt("member must be a non-empty character string."),
            origin = "Orchestrator$access_nested"
          )
          phr_assert(
            !is.null(target[[member]]),
            message = phr_txt(
              "Member '{member}' does not exist on the resolved target."
            ),
            origin = "Orchestrator$access_nested"
          )

          value <- target[[member]]
          out <- if (is.function(value)) {
            do.call(value, list(...))
          } else {
            value
          }

          if (update_sync) {
            private$..sync_state()
          }
          if (update_modified) {
            private$..touch()
          }
          out
        },
        on_error = "abort",
        origin = "Orchestrator$access_nested"
      )
    },

    #' @description
    #' Generalized nested field setter.
    #'
    #' Resolves a top-level field (optionally a named list element), sets
    #' \code{member} to \code{value}, then calls synchronization hooks and
    #' updates the modified timestamp.
    #'
    #' @param field Character scalar naming the stored top-level field.
    #' @param member Character scalar naming a writable field in the resolved
    #'   nested object.
    #' @param value Value to assign.
    #' @param name Optional character scalar naming a list element in
    #'   \code{field}.
    #' @return Invisibly returns \code{self}.
    #' @param role Optional character scalar used to resolve a list element in
    #'   \code{field} by role-like name.
    set_nested = function(field, member, value, name = NULL, role = NULL) {
      phr_try(
        {
          phr_assert(
            is.character(member) && length(member) == 1L && nzchar(member),
            message = phr_txt("member must be a non-empty character string."),
            origin = "Orchestrator$set_nested"
          )
          target <- private$..resolve_nested_target(
            field = field,
            name = name,
            role = role
          )
          target[[member]] <- value
          private$..sync_state()
          private$..touch()
        },
        on_error = "abort",
        origin = "Orchestrator$set_nested"
      )
      invisible(self)
    }
  ),

  private = list(
    #' @description Update modified timestamp metadata.
    #' @return Invisibly returns \code{NULL}.
    ..touch = function() {
      if (is.null(self$metadata) || !is.list(self$metadata)) {
        self$metadata <- list()
      }
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    #' @description Synchronize orchestrator state.
    #'
    #' When \code{field/member} are provided, returns the resolved nested value
    #' and optionally assigns it to \code{target_field}. Without arguments, this
    #' runs inherited synchronization hooks (\code{sync_*} members).
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path (supports \code{$}).
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns resolved value (targeted mode) or \code{NULL}.
    ..sync_state = function(
      field = NULL,
      member = NULL,
      target_field = NULL,
      name = NULL,
      role = NULL
    ) {
      self$pre_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      if (
        !is.null(field) ||
          !is.null(member) ||
          !is.null(target_field) ||
          !is.null(name) ||
          !is.null(role)
      ) {
        phr_assert(
          is.character(field) && length(field) == 1L && nzchar(field),
          message = phr_txt("field must be a non-empty character string."),
          origin = "Orchestrator$sync_state"
        )
        phr_assert(
          is.character(member) && length(member) == 1L && nzchar(member),
          message = phr_txt("member must be a non-empty character string."),
          origin = "Orchestrator$sync_state"
        )
        target <- private$..resolve_nested_target(
          field = field,
          name = name,
          role = role
        )
        phr_assert(
          !is.null(target[[member]]),
          message = phr_txt(
            "Member '{member}' does not exist on the resolved target."
          ),
          origin = "Orchestrator$sync_state"
        )
        value <- target[[member]]
        if (is.function(value)) {
          value <- value()
        }
        if (!is.null(target_field)) {
          private$..assign_sync_value(
            target_field = target_field,
            value = value
          )
        }
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(value))
      }

      # Backward compatibility: support older subclasses that still define
      # synchronize_state(), then fall back to sync_* members.
      sync_runner <- tryCatch(self$synchronize_state, error = function(e) NULL)
      if (is.function(sync_runner)) {
        sync_runner()
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(NULL))
      }

      sync_names <- setdiff(
        grep("^sync_", names(self), value = TRUE),
        "sync_state"
      )
      if (length(sync_names) == 0L) {
        self$post_sync_state(
          field = field,
          member = member,
          target_field = target_field,
          name = name,
          role = role
        )
        return(invisible(NULL))
      }
      for (nm in sync_names) {
        val <- tryCatch(self[[nm]], error = function(e) NULL)
        if (is.function(val)) {
          tryCatch(val(), error = function(e) NULL)
        }
      }
      self$post_sync_state(
        field = field,
        member = member,
        target_field = target_field,
        name = name,
        role = role
      )
      invisible(NULL)
    },

    #' @description Resolve a top-level or nested target object.
    #' @param field Top-level field name.
    #' @param name Optional exact list element name.
    #' @param role Optional role-style key for list lookup.
    #' @return Resolved object.
    ..resolve_nested_target = function(field, name = NULL, role = NULL) {
      phr_assert(
        is.character(field) && length(field) == 1L && nzchar(field),
        message = phr_txt("field must be a non-empty character string."),
        origin = "Orchestrator$.resolve_nested_target"
      )
      phr_assert(
        !is.null(self[[field]]),
        message = phr_txt("Field '{field}' is not available on this object."),
        origin = "Orchestrator$.resolve_nested_target"
      )
      container <- self[[field]]

      phr_assert(
        !(!is.null(name) && !is.null(role)),
        message = phr_txt("Provide only one of name or role."),
        origin = "Orchestrator$.resolve_nested_target"
      )

      if (is.null(name) && is.null(role)) {
        return(container)
      }

      phr_assert(
        is.list(container),
        message = phr_txt(
          "Field '{field}' must be a list when resolving name/role."
        ),
        origin = "Orchestrator$.resolve_nested_target"
      )

      if (!is.null(name)) {
        phr_assert(
          is.character(name) && length(name) == 1L && nzchar(name),
          message = phr_txt(
            "name must be a non-empty character string when provided."
          ),
          origin = "Orchestrator$.resolve_nested_target"
        )
        phr_assert(
          !is.null(container[[name]]),
          message = phr_txt("Name '{name}' was not found in field '{field}'."),
          origin = "Orchestrator$.resolve_nested_target"
        )
        return(container[[name]])
      }

      phr_assert(
        is.character(role) && length(role) == 1L && nzchar(role),
        message = phr_txt(
          "role must be a non-empty character string when provided."
        ),
        origin = "Orchestrator$.resolve_nested_target"
      )

      nms <- names(container)
      phr_assert(
        !is.null(nms) && length(nms) > 0L,
        message = phr_txt(
          "Field '{field}' has no named elements for role-based lookup."
        ),
        origin = "Orchestrator$.resolve_nested_target"
      )

      role_key <- private$..normalize_role_name(role)
      normalized_names <- vapply(
        nms,
        private$..normalize_role_name,
        character(1L)
      )
      idx <- which(normalized_names == role_key)

      if (length(idx) == 0L) {
        idx <- grep(
          paste0("^", role_key, "$|_", role_key, "_|_", role_key, "$"),
          normalized_names
        )
      }

      phr_assert(
        length(idx) == 1L,
        message = if (length(idx) == 0L) {
          phr_txt("Role '{role}' was not found in field '{field}'.")
        } else {
          phr_txt("Role '{role}' matched multiple elements in field '{field}'.")
        },
        origin = "Orchestrator$.resolve_nested_target"
      )
      container[[idx]]
    },

    #' @description Normalize role names for fuzzy list matching.
    #' @param x Character role/name input.
    #' @return Normalized character key.
    ..normalize_role_name = function(x) {
      x <- tolower(as.character(x %||% ""))
      x <- gsub("^tool_", "", x)
      x <- gsub("_iphra(_v[0-9]+)?$", "", x)
      x <- gsub("_v[0-9]+$", "", x)
      x
    },

    #' @description Assign synchronized values to a target field path.
    #' @param target_field Character path using \code{$} separators.
    #' @param value Value to assign.
    #' @return Invisibly returns \code{NULL}.
    ..assign_sync_value = function(target_field, value) {
      phr_assert(
        is.character(target_field) &&
          length(target_field) == 1L &&
          nzchar(target_field),
        message = phr_txt("target_field must be a non-empty character string."),
        origin = "Orchestrator$.assign_sync_value"
      )

      path <- strsplit(target_field, "\\$", fixed = FALSE)[[1L]]
      path <- path[nzchar(path)]
      phr_assert(
        length(path) >= 1L,
        message = phr_txt("target_field path is invalid."),
        origin = "Orchestrator$.assign_sync_value"
      )

      if (length(path) == 1L) {
        self[[path[[1L]]]] <- value
        return(invisible(NULL))
      }

      set_path <- function(x, keys, val) {
        if (length(keys) == 1L) {
          x[[keys[[1L]]]] <- val
          return(x)
        }
        key <- keys[[1L]]
        next_val <- x[[key]]
        if (
          is.null(next_val) || (!is.list(next_val) && !is.environment(next_val))
        ) {
          next_val <- list()
        }
        x[[key]] <- set_path(next_val, keys[-1L], val)
        x
      }

      root_name <- path[[1L]]
      root <- self[[root_name]]
      if (is.null(root) || (!is.list(root) && !is.environment(root))) {
        root <- list()
      }
      self[[root_name]] <- set_path(root, path[-1L], value)
      invisible(NULL)
    }
  )
)
