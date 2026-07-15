#' Document R6 Class
#'
#' @description
#' Subclass of \code{\link{Orchestrator}} that provides shared DOCX handling
#' helpers for protocol/report-like classes.
#'
#' @importFrom R6 R6Class
Document <- R6::R6Class(
  "Document",
  inherit = Orchestrator,
  public = list(
    #' @field reference_doc_filename Optional template filename/path used to
    #'   initialize \code{document}.
    reference_doc_filename = NULL,

    #' @field reference_ppt_filename Optional template filename/path used to
    #'   initialize \code{powerpoint}.
    reference_ppt_filename = NULL,

    #' @return A new Document object.
    initialize = function(
      reference_doc_filename = NULL,
      reference_ppt_filename = NULL
    ) {
      super$initialize()
      self$reference_doc_filename <- private$..default_template_filenames()
      self$reference_ppt_filename <- private$..default_ppt_template_filenames()

      invisible(self)
    },
    #' @description
    #' Generate a Word document using Quarto with template substitution.
    #'
    #' This method renders a Quarto template with specified parameters to
    #' produce a Word document output.
    #'
    #' @param output_file Character output path for the rendered document.
    #' @param template_file Character path to the Quarto template file. If NULL,
    #'   uses "quarto_doc_template.qmd" from package resources.
    #' @param params Named list of parameters to substitute in the template.
    #' @param content Character string containing the main document content
    #'   (Markdown formatted).
    #' @param open Logical indicating whether to open the output in a browser.
    #' @return Invisibly returns \code{self}.
    generate_quarto_doc = function(
      output_file = "document_report.docx",
      template_file = NULL,
      params = list(),
      open = FALSE
    ) {
      phr_try(
        {
          # Resolve template path
          if (is.null(template_file)) {
            template_file <- private$..default_word_template_path()
          }

          if (!file.exists(template_file)) {
            phr_error(
              phr_txt("Quarto template file not found: {template_file}"),
              origin = "Document$generate_quarto_doc"
            )
          }

          # Default parameters
          default_params <- list(
            title = "Document Report",
            author = "Author",
            date = format(Sys.Date(), "%Y-%m-%d")
          )

          # Merge with provided params
          all_params <- modifyList(default_params, self$get_quarto_params())
          all_params <- modifyList(all_params, params)

          # Keep only params declared in YAML
          declared_params <- names(
            rmarkdown::yaml_front_matter(template_file)$params
          )
          all_params <- all_params[names(all_params) %in% declared_params]

          # Render with quarto
          quarto::quarto_render(
            input = template_file,
            output_file = basename(output_file),
            output_format = "docx",
            execute_params = all_params
          )

          # Move to desired location if needed
          rendered_file <- file.path(
            dirname(template_file),
            basename(output_file)
          )
          if (
            normalizePath(rendered_file, mustWork = FALSE) !=
              normalizePath(output_file, mustWork = FALSE)
          ) {
            file.copy(rendered_file, output_file, overwrite = TRUE)
          }

          phr_message(
            phr_txt("Quarto document saved to: {output_file}"),
            origin = "Document$generate_quarto_doc"
          )

          if (isTRUE(open)) utils::browseURL(output_file)
        },
        on_error = "abort",
        origin = "Document$generate_quarto_doc"
      )
      invisible(self)
    },

    #' @description
    #' Generate a PowerPoint presentation using Quarto with template substitution.
    #'
    #' This method renders a Quarto template with specified parameters to
    #' produce a PowerPoint presentation output.
    #'
    #' @param output_file Character output path for the rendered presentation.
    #' @param template_file Character path to the Quarto template file. If NULL,
    #'   uses "quarto_ppt_template.qmd" from package resources.
    #' @param params Named list of parameters to substitute in the template.
    #' @param content Character string containing the main presentation content
    #'   (Markdown formatted).
    #' @param open Logical indicating whether to open the output in a browser.
    #' @return Invisibly returns \code{self}.
    generate_quarto_ppt = function(
      output_file = "presentation_report.pptx",
      template_file = NULL,
      params = list(),
      content = "",
      open = FALSE
    ) {
      phr_try(
        {
          # Resolve template path
          if (is.null(template_file)) {
            template_file <- system.file(
              "resources",
              "quarto_ppt_template.qmd",
              package = "phr"
            )
          }
          if (!nzchar(template_file) || !file.exists(template_file)) {
            template_file <- file.path(
              "inst",
              "resources",
              "quarto_ppt_template.qmd"
            )
          }
          if (!file.exists(template_file)) {
            phr_error(
              phr_txt("Quarto template file not found: {template_file}"),
              origin = "Document$generate_quarto_ppt"
            )
          }

          # Read template
          template_text <- paste(readLines(template_file), collapse = "\n")

          # Default parameters
          default_params <- list(
            title = "Presentation Report",
            author = "Author",
            date = format(Sys.Date(), "%Y-%m-%d"),
            reference_doc = "null",
            content = ""
          )

          # Merge with provided params
          all_params <- modifyList(default_params, params)
          all_params$content <- content

          # Substitute parameters in template
          for (param_name in names(all_params)) {
            pattern <- paste0("\\{\\{", param_name, "\\}\\}")
            replacement <- as.character(all_params[[param_name]])
            template_text <- gsub(
              pattern,
              replacement,
              template_text,
              fixed = FALSE
            )
          }

          # Write temporary qmd file
          temp_qmd <- tempfile(fileext = ".qmd")
          writeLines(template_text, temp_qmd)

          # Render with quarto
          quarto::quarto_render(
            input = temp_qmd,
            output_file = basename(output_file),
            output_format = "pptx"
          )

          # Move to desired location if needed
          rendered_file <- file.path(
            dirname(temp_qmd),
            basename(output_file)
          )
          if (
            normalizePath(rendered_file, mustWork = FALSE) !=
              normalizePath(output_file, mustWork = FALSE)
          ) {
            file.copy(rendered_file, output_file, overwrite = TRUE)
          }

          phr_message(
            phr_txt("Quarto presentation saved to: {output_file}"),
            origin = "Document$generate_quarto_ppt"
          )

          if (isTRUE(open)) utils::browseURL(output_file)
        },
        on_error = "abort",
        origin = "Document$generate_quarto_ppt"
      )
      invisible(self)
    },
    get_quarto_params = function() {
      list(
        r_version = self$.r_version
      )
    }
  ),
  active = list(
    #' @description
    #' Active binding that returns the R version string.
    #' @return Character string of the R version.
    .r_version = function(value) {
      version$version.string
    }
  ),

  private = list(
    #' @description
    #' Resolve the default Quarto Word template path.
    #'
    #' @return Character scalar giving the resolved path to the default Word
    #'   Quarto template.
    #'
    #' @keywords internal
    ..default_word_template_path = function() {
      template_file <- "quarto_doc_revised_template.qmd"

      template_path <- system.file(
        "resources",
        template_file,
        package = "phr"
      )

      if (!nzchar(template_path) || !file.exists(template_path)) {
        template_path <- file.path(
          "inst",
          "resources",
          template_file
        )
      }

      template_path
    },
    #' @description Return default template filename candidates.
    #' @return Character vector of template filenames.
    ..default_template_filenames = function() {
      c("reach_tor_template.docx", "protocol_report_template.docx")
    },

    #' @description Return default PowerPoint template filename candidates.
    #' @return Character vector of template filenames.
    ..default_ppt_template_filenames = function() {
      c("protocol_report_template.pptx")
    },
    ..sanitize_quarto_df = function(df) {
      if (!is.data.frame(df)) {
        return(df)
      }

      df[] <- lapply(df, function(x) {
        if (is.character(x)) {
          x[is.na(x)] <- ""
        } else if (is.numeric(x) || is.integer(x)) {
          x[is.na(x)] <- 0
        } else if (is.logical(x)) {
          x[is.na(x)] <- FALSE
        }
        x
      })

      df
    }
  )
)
