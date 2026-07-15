#' IPHRA document schema handlers
#'
#' @description
#' Standalone helpers for generating flextable objects used by schema-driven
#' document handlers.

.tool_indicator_codes <- function(
  tools,
  tool_names = NULL,
  prefer_revised = TRUE
) {
  if (is.null(tools) || length(tools) == 0L) {
    return(character(0))
  }
  names_to_use <- names(tools)
  if (!is.null(tool_names)) {
    names_to_use <- intersect(names_to_use, as.character(tool_names))
  }
  out <- character(0)
  for (tn in names_to_use) {
    tool <- tools[[tn]]
    if (!is.null(tool) && inherits(tool, "Tool")) {
      out <- c(
        out,
        as.character(tool$get_indicator_codes(prefer_revised = prefer_revised))
      )
    }
  }
  unique(out[!is.na(out) & nzchar(out)])
}

#' Build primary data sources table.
#' @param master_schema Framework master objectives schema.
#' @param tool_indicator_codes Named list of tool indicator-code vectors.
#' @return A flextable object, or \code{NULL} when table cannot be built.
table_primary_data_sources <- function(master_schema, tool_indicator_codes) {
  if (
    !is.data.frame(master_schema) ||
      nrow(master_schema) == 0 ||
      !all(
        c("objective_code", "indicator_code", "text_objective") %in%
          names(master_schema)
      )
  ) {
    return(NULL)
  }
  if (!is.list(tool_indicator_codes) || length(tool_indicator_codes) == 0L) {
    return(NULL)
  }

  tool_names <- names(tool_indicator_codes)
  all_codes <- unique(unlist(tool_indicator_codes, use.names = FALSE))
  if (length(all_codes) == 0L) {
    return(NULL)
  }

  obj_sub <- unique(master_schema[
    as.character(master_schema$indicator_code) %in% all_codes,
    c("objective_code", "text_objective"),
    drop = FALSE
  ])
  obj_sub <- obj_sub[!is.na(obj_sub$objective_code), , drop = FALSE]
  if (nrow(obj_sub) == 0L) {
    return(NULL)
  }
  obj_sub <- obj_sub[order(obj_sub$objective_code), , drop = FALSE]

  tool_labels <- gsub("^tool_", "", tool_names)
  tool_labels <- gsub("_iphra_v[0-9]+$", "", tool_labels)
  mat <- as.data.frame(
    matrix("", nrow = nrow(obj_sub), ncol = length(tool_names)),
    stringsAsFactors = FALSE
  )
  names(mat) <- tool_labels

  for (j in seq_along(tool_names)) {
    tc <- tool_indicator_codes[[tool_names[j]]]
    obj_covered <- unique(as.character(master_schema$objective_code[
      as.character(master_schema$indicator_code) %in% tc
    ]))
    mat[[j]] <- ifelse(obj_sub$objective_code %in% obj_covered, "X", "")
  }

  result_df <- cbind(
    data.frame(Objective = obj_sub$text_objective, stringsAsFactors = FALSE),
    mat
  )
  ft <- flextable::theme_zebra(flextable::flextable(result_df))
  n_tool_cols <- length(tool_names)
  ft <- flextable::width(ft, j = 1, width = 3.25)
  if (n_tool_cols > 0) {
    ft <- flextable::width(
      ft,
      j = seq(2, n_tool_cols + 1),
      width = 3.25 / n_tool_cols
    )
  }
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  flextable::set_table_properties(ft, layout = "fixed")
}

#' Build secondary data sources table.
#' @param master_schema Framework master objectives schema.
#' @param secondary_data Named character vector/list of objective code to source.
#' @return A flextable object.
table_secondary_data_sources <- function(master_schema, secondary_data) {
  if (!is.null(secondary_data) && length(secondary_data) > 0) {
    obj_labels <- setNames(
      vapply(
        names(secondary_data),
        function(code) {
          if (
            is.data.frame(master_schema) &&
              all(
                c("objective_code", "text_objective") %in% names(master_schema)
              )
          ) {
            idx <- which(as.character(master_schema$objective_code) == code)
            if (length(idx) > 0) {
              return(as.character(master_schema$text_objective[idx[1]]))
            }
          }
          code
        },
        character(1)
      ),
      names(secondary_data)
    )
    sdr_df <- data.frame(
      Objective = unname(obj_labels[names(secondary_data)]),
      Source = as.character(unname(secondary_data)),
      Purpose = "",
      stringsAsFactors = FALSE
    )
  } else {
    sdr_df <- data.frame(
      Objective = "",
      Source = "",
      Purpose = "",
      stringsAsFactors = FALSE
    )
  }

  ft <- flextable::theme_zebra(flextable::flextable(sdr_df))
  if (!is.null(secondary_data) && length(secondary_data) > 0) {
    ft <- flextable::merge_v(ft, j = "Objective")
  }
  ft <- flextable::width(ft, j = 1, width = 2.6)
  ft <- flextable::width(ft, j = 2, width = 1.95)
  ft <- flextable::width(ft, j = 3, width = 1.95)
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  flextable::set_table_properties(ft, layout = "fixed")
}

.table_sample_size_builder <- function(sample_table, param_rows, total_labels) {
  if (
    is.null(sample_table) ||
      !is.data.frame(sample_table) ||
      nrow(sample_table) == 0L
  ) {
    return(NULL)
  }

  strata_names <- if ("stratum_name" %in% names(sample_table)) {
    as.character(sample_table$stratum_name)
  } else {
    as.character(sample_table$stratum_id)
  }
  n_strata <- length(strata_names)
  col_names <- c("Parameter", strata_names, "Justification")
  rows_list <- lapply(param_rows, function(pr) {
    vals <- vapply(
      seq_len(n_strata),
      function(j) {
        tryCatch(
          pr$col_fn(sample_table[j, , drop = FALSE]),
          error = function(e) ""
        )
      },
      character(1L)
    )
    as.list(c(pr$label, vals, ""))
  })
  mat <- do.call(
    rbind,
    lapply(rows_list, function(r) {
      as.data.frame(r, stringsAsFactors = FALSE, col.names = col_names)
    })
  )
  names(mat) <- col_names

  return(mat)
}

#' Build general sample-size table.
#' @param sample_table Sample table data frame.
#' @return A flextable object, or \code{NULL} when unavailable.
table_sample_size_general <- function(sample_table) {
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) {
      as.character(r$pop_indicator %||% "")
    }),
    list(label = "Sampling Design", col_fn = function(r) {
      phr_fmt_sampling_method(r$sampling_method_site %||% "")
    }),
    list(label = "Estimated Prevalence (%)", col_fn = function(r) {
      phr_fmt_pct(r$pop_expected_prevalence)
    }),
    list(label = "Desired Precision", col_fn = function(r) {
      phr_fmt_pct(r$pop_precision)
    }),
    list(label = "Estimated population size", col_fn = function(r) {
      phr_fmt_n(r$total_population)
    }),
    list(label = "Design Effect", col_fn = function(r) {
      if (is.null(r$pop_design_effect) || is.na(r$pop_design_effect)) {
        ""
      } else {
        as.character(r$pop_design_effect)
      }
    }),
    list(
      label = "Finite Population Correction (FPC) used?",
      col_fn = function(r) phr_fmt_fpc(r$pop_fpc)
    ),
    list(label = "Non-Response Rate", col_fn = function(r) {
      phr_fmt_pct(r$pop_nonresponse)
    }),
    list(label = "Households to be Included", col_fn = function(r) {
      phr_fmt_n(r$General_HH_Sample_Size)
    })
  )
  .table_sample_size_builder(
    sample_table,
    params,
    total_labels = c(
      "Households to be Included",
      "Individuals to be Included",
      "Population to be Included",
      "Person-Time to be Included"
    )
  )
}

#' Build individual sample-size table.
#' @param sample_table Sample table data frame.
#' @param indicator_codes Included indicator codes from tools.
#' @return A flextable object, or \code{NULL} when unavailable.
table_sample_size_individual <- function(sample_table) {
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) {
      as.character(r$ind_indicator %||% "")
    }),
    list(label = "Sampling Design", col_fn = function(r) {
      phr_fmt_sampling_method(r$sampling_method_site %||% "")
    }),
    list(label = "Estimated Prevalence (%)", col_fn = function(r) {
      phr_fmt_pct(r$ind_expected_prevalence)
    }),
    list(label = "Desired Precision", col_fn = function(r) {
      phr_fmt_pct(r$ind_precision)
    }),
    list(label = "Estimated population size", col_fn = function(r) {
      phr_fmt_n(r$total_population)
    }),
    list(label = "Design Effect", col_fn = function(r) {
      if (is.null(r$ind_design_effect) || is.na(r$ind_design_effect)) {
        ""
      } else {
        as.character(r$ind_design_effect)
      }
    }),
    list(
      label = "Finite Population Correction (FPC) used?",
      col_fn = function(r) phr_fmt_fpc(r$ind_fpc)
    ),
    list(label = "Individuals to be Included", col_fn = function(r) {
      phr_fmt_n(r$Ind_Sample_Size)
    }),
    list(label = "Average Household Size", col_fn = function(r) {
      if (is.null(r$ind_avg_hh_size) || is.na(r$ind_avg_hh_size)) {
        ""
      } else {
        as.character(r$ind_avg_hh_size)
      }
    }),
    list(label = "% sub-population", col_fn = function(r) {
      phr_fmt_pct(r$ind_subpop_prop)
    }),
    list(label = "Non-Response Rate", col_fn = function(r) {
      phr_fmt_pct(r$ind_nonresponse)
    }),
    list(label = "Households to be Included", col_fn = function(r) {
      phr_fmt_n(r$Ind_HH_Sample_Size)
    })
  )
  .table_sample_size_builder(
    sample_table,
    params,
    total_labels = c(
      "Households to be Included",
      "Individuals to be Included",
      "Population to be Included",
      "Person-Time to be Included"
    )
  )
}

#' Build mortality sample-size table.
#' @param sample_table Sample table data frame.
#' @param indicator_codes Included indicator codes from tools.
#' @return A flextable object, or \code{NULL} when unavailable.
table_sample_size_rate <- function(sample_table) {
  params <- list(
    list(label = "Indicator Name", col_fn = function(r) {
      as.character(r$mort_indicator %||% "")
    }),
    list(label = "Sampling Design", col_fn = function(r) {
      phr_fmt_sampling_method(r$sampling_method_site %||% "")
    }),
    list(label = "Expected Rate", col_fn = function(r) {
      if (is.null(r$rate_expected_rate) || is.na(r$rate_expected_rate)) {
        ""
      } else {
        as.character(r$rate_expected_rate)
      }
    }),
    list(label = "Rate Precision", col_fn = function(r) {
      if (is.null(r$rate_precision) || is.na(r$rate_precision)) {
        ""
      } else {
        as.character(r$rate_precision)
      }
    }),
    list(label = "Rate Period", col_fn = function(r) {
      if (is.null(r$rate_recall_days) || is.na(r$rate_recall_days)) {
        ""
      } else {
        as.character(r$rate_recall_days)
      }
    }),
    list(label = "Population size (overall)", col_fn = function(r) {
      phr_fmt_n(r$total_population)
    }),
    list(label = "Design Effect", col_fn = function(r) {
      if (is.null(r$rate_design_effect) || is.na(r$rate_design_effect)) {
        ""
      } else {
        as.character(r$rate_design_effect)
      }
    }),
    list(
      label = "Finite Population Correction (FPC) used?",
      col_fn = function(r) phr_fmt_fpc(r$rate_fpc)
    ),
    list(label = "Population to be Included", col_fn = function(r) {
      phr_fmt_n(r$Rate_Ind_Sample_Size, "people")
    }),
    list(label = "Person-Time to be Included", col_fn = function(r) {
      phr_fmt_n(r$Rate_PT_Sample_Size, "person days")
    }),
    list(label = "Average Household Size", col_fn = function(r) {
      if (is.null(r$rate_avg_hh_size) || is.na(r$rate_avg_hh_size)) {
        ""
      } else {
        as.character(r$rate_avg_hh_size)
      }
    }),
    list(label = "% Non-Respondents", col_fn = function(r) {
      phr_fmt_pct(r$rate_nonresponse)
    }),
    list(label = "Households to be Included", col_fn = function(r) {
      phr_fmt_n(r$Rate_HH_Sample_Size, "households")
    })
  )
  .table_sample_size_builder(
    sample_table,
    params,
    total_labels = c(
      "Households to be Included",
      "Individuals to be Included",
      "Population to be Included",
      "Person-Time to be Included"
    )
  )
}
