#' SamplingFrame: Sampling Frame Log Class
#'
#' @description
#' A \code{\link{Log}} subclass for holding and validating the sampling frame
#' data used in survey sample drawing workflows.
#'
#' @details
#' The default structure is the full extended sampling frame with the following
#' columns:
#' \itemize{
#'   \item \code{stratum} — stratum identifier.
#'   \item \code{psu} — primary sampling unit identifier.
#'   \item \code{population_size} — population count for the PSU.
#'   \item \code{inclusion} — logical flag marking PSUs eligible for sampling.
#'   \item \code{sampled_psu} — cluster number(s) assigned by \code{draw_sample()};
#'     \code{NA} for unselected PSUs.  When a PSU is drawn more than once (as
#'     can happen with PPS cluster or RLC sampling), this field contains the
#'     comma-separated consecutive cluster numbers assigned to that PSU
#'     (e.g. \code{"9, 10, 11"} for a PSU drawn three times).
#'   \item \code{allocated_sample} — number of households allocated to the PSU
#'     by \code{draw_sample()}; \code{NA} for unselected PSUs.
#' }
#'
#' A \code{SamplingFrame} can be initialised with an existing data frame
#' (e.g. a pre-built frame loaded from a file) or left empty for later
#' population via \code{\link[=SurveyProtocol]{SurveyProtocol$set_sampling_frame()}}.
#'
#' @importFrom R6 R6Class
#' @export
SamplingFrame <- R6::R6Class(
  classname = "SamplingFrame",
  inherit = Log,

  public = list(
    #' @field drawn_sample Data frame with selected PSUs.
    drawn_sample = NULL,

    #' @field drawn_sample_full Full sampled frame with annotations.
    drawn_sample_full = NULL,

    #' @description
    #' Creates a new SamplingFrame object.
    #'
    #' @param log_df Optional data frame of sampling frame entries.  When
    #'   \code{NULL} (default), an empty frame with the standard columns is
    #'   created automatically.
    #' @param log_name Character. Display name for this log (default:
    #'   \code{"Sampling Frame"}).
    #' @param required_columns Character vector of required column names.
    #'   Defaults to the standard set: \code{stratum}, \code{psu},
    #'   \code{population_size}, \code{inclusion}, \code{sampled_psu},
    #'   \code{allocated_sample}.
    #' @param schema List with \code{types} and/or \code{allowed_values} for
    #'   validation.  Defaults to the standard sampling frame schema.
    #' @return A new SamplingFrame R6 object.
    initialize = function(
      log_df = NULL,
      log_name = "Sampling Frame",
      required_columns = NULL,
      schema = NULL
    ) {
      required_columns <- required_columns %||%
        c(
          "stratum",
          "psu",
          "population_size",
          "inclusion",
          "sampled_psu",
          "allocated_sample"
        )

      schema <- schema %||%
        list(
          types = list(
            stratum = "character",
            psu = "character",
            population_size = "numeric",
            inclusion = "logical",
            sampled_psu = "character",
            allocated_sample = "numeric"
          )
        )

      super$initialize(
        log_df = log_df,
        log_name = log_name,
        required_columns = required_columns,
        schema = schema
      )
    },

    #' Get Additional Accessible Fields
    #'
    #' @description
    #' Extends the parent class list of fields accessible via `get()`.
    #'
    #' @return Character vector of additional field names.
    additional_get_fields = function() {
      c(
        "drawn_sample",
        "drawn_sample_full"
      )
    },

    #' @description Draw a sample from a sampling frame.
    #' @param strata_table Optional strata table. Defaults to \code{sample_table}.
    #' @param seed Integer random seed.
    #' @return Invisibly returns \code{self}.
    draw_sample = function(strata_table = NULL, seed = 43) {
      frame <- private$log_df

      phrutils::phr_validate_dataframe(
        frame,
        origin = "Sample$draw_sample",
        soft = FALSE
      )

      if (is.null(strata_table)) {
        phrutils::phr_warning(
          origin = "strata_table",
          message = phr_txt(
            "No strata table was provided. Strata-based processing will be skipped."
          )
        )
      }

      phrutils::phr_validate_dataframe(
        strata_table,
        origin = "Sample$draw_sample",
        soft = FALSE
      )
      phrutils::phr_assert(
        "sampling_method_site" %in% names(strata_table),
        message = phr_txt(
          "strata_table must contain a 'sampling_method_site' column."
        ),
        origin = "Sample$draw_sample"
      )
      phrutils::phr_assert(
        "sampling_method_hh" %in% names(strata_table),
        message = phr_txt(
          "strata_table must contain a 'sampling_method_hh' column."
        ),
        origin = "Sample$draw_sample"
      )

      for (col in c(
        "n_psu",
        "cluster_size",
        "n_sites",
        "Final_HH_Sample_Size"
      )) {
        if (!col %in% names(strata_table)) strata_table[[col]] <- NA_real_
      }

      if ("inclusion" %in% names(frame)) {
        eligible_rows <- which(!is.na(frame$inclusion) & frame$inclusion)
      } else {
        eligible_rows <- seq_len(nrow(frame))
      }
      eligible_frame <- frame[eligible_rows, , drop = FALSE]
      frame$sampled_psu <- NA_character_
      frame$allocated_sample <- NA_real_

      is_stratified <- ("stratum" %in% names(eligible_frame)) &&
        ("stratum_id" %in% names(strata_table)) &&
        (nrow(strata_table) > 1L || !is.null(strata_table$stratum_id))

      if (is_stratified) {
        strata_ids <- unique(strata_table$stratum_id)
        cluster_offset <- 0L

        for (st_id in strata_ids) {
          st_row <- strata_table[
            strata_table$stratum_id == st_id,
            ,
            drop = FALSE
          ]
          if (nrow(st_row) == 0L) {
            next
          }

          st_eligible_rows <- which(eligible_frame$stratum == st_id)
          if (length(st_eligible_rows) == 0L) {
            phrutils::phr_warning(
              message = phr_txt(
                "Stratum '{st_id}' not found in sampling frame — skipping."
              ),
              origin = "Sample$draw_sample"
            )
            next
          }
          st_frame <- eligible_frame[st_eligible_rows, , drop = FALSE]
          st_params <- private$..params_from_strata_row(
            st_row,
            nrow(st_frame),
            nrow(eligible_frame)
          )
          st_result <- tryCatch(
            private$..apply_sampling_method(
              frame = st_frame,
              method_site = st_params$method_site,
              method_hh = st_params$method_hh,
              sample_size = st_params$sample_size,
              n_psu = st_params$n_psu,
              n_sites = st_params$n_sites,
              cluster_size = st_params$cluster_size,
              seed = seed
            ),
            error = function(e) {
              phrutils::phr_warning(
                message = phr_txt(
                  "Sampling for stratum '{st_id}' failed and will be skipped: {conditionMessage(e)}"
                ),
                origin = "Sample$draw_sample"
              )
              NULL
            }
          )
          if (is.null(st_result)) {
            next
          }

          sel_mask <- !is.na(st_result$sampled_psu)
          if (any(sel_mask)) {
            parse_cluster_labels <- function(s) {
              parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
              nums <- suppressWarnings(as.integer(parts))
              nums[!is.na(nums)]
            }
            apply_cluster_offset <- function(s, offset) {
              parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
              vapply(
                parts,
                function(p) {
                  n <- suppressWarnings(as.integer(p))
                  if (is.na(n)) p else as.character(n + offset)
                },
                character(1),
                USE.NAMES = FALSE
              )
            }
            st_result$sampled_psu[sel_mask] <- vapply(
              st_result$sampled_psu[sel_mask],
              function(s) {
                paste(apply_cluster_offset(s, cluster_offset), collapse = ", ")
              },
              character(1)
            )
            all_nums <- unlist(lapply(
              st_result$sampled_psu[sel_mask],
              parse_cluster_labels
            ))
            if (length(all_nums) > 0L) {
              cluster_offset <- cluster_offset + max(all_nums)
            }
          }

          full_frame_rows <- eligible_rows[st_eligible_rows]
          frame$sampled_psu[full_frame_rows] <- st_result$sampled_psu
          frame$allocated_sample[full_frame_rows] <- st_result$allocated_sample
        }
      } else {
        st_row <- strata_table[1L, , drop = FALSE]
        st_params <- private$..params_from_strata_row(
          st_row,
          nrow(eligible_frame),
          nrow(eligible_frame)
        )
        result <- tryCatch(
          private$..apply_sampling_method(
            frame = eligible_frame,
            method_site = st_params$method_site,
            method_hh = st_params$method_hh,
            sample_size = st_params$sample_size,
            n_psu = st_params$n_psu,
            n_sites = st_params$n_sites,
            cluster_size = st_params$cluster_size,
            seed = seed
          ),
          error = function(e) {
            phrutils::phr_warning(
              message = phr_txt("Sampling failed: {conditionMessage(e)}"),
              origin = "Sample$draw_sample"
            )
            NULL
          }
        )
        if (!is.null(result)) {
          frame$sampled_psu[eligible_rows] <- result$sampled_psu
          frame$allocated_sample[eligible_rows] <- result$allocated_sample
        }
      }

      self$drawn_sample_full <- frame
      self$drawn_sample <- frame[!is.na(frame$sampled_psu), , drop = FALSE]
      private$..touch()
      invisible(self)
    },

    #' @description Clear sampled columns from a frame.
    #' @param frame Data frame sampling frame.
    #' @return Cleared frame.
    clear_sample = function(frame) {
      out <- frame
      if (!is.null(out) && is.data.frame(out) && nrow(out) > 0) {
        if ("sampled_psu" %in% names(out)) {
          out$sampled_psu <- NA_character_
        }
        if ("allocated_sample" %in% names(out)) out$allocated_sample <- NA_real_
      }
      self$drawn_sample <- NULL
      self$drawn_sample_full <- NULL
      private$..touch()
      out
    },

    #' @description Sort PSUs from highest to lowest population within each stratum.
    #'
    #' Reorders the rows of the internal log data frame so that within each
    #' stratum, PSUs appear in descending order of \code{population_size}.
    #' PSUs with missing population values are placed last within their stratum.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    sort_psu_by_population = function() {
      df <- private$log_df
      if (is.null(df) || nrow(df) == 0) {
        return(invisible(self))
      }
      if (!all(c("stratum", "population_size") %in% names(df))) {
        return(invisible(self))
      }
      pop <- as.numeric(df$population_size)
      df <- df[
        order(df$stratum, ifelse(is.na(pop), -Inf, pop) * -1L, na.last = TRUE),
        ,
        drop = FALSE
      ]
      private$log_df <- df
      invisible(self)
    }
  ),
  private = list(
    # @description Apply a sampling method to a frame.
    # @param frame Data frame sampling frame.
    # @param method_site Character scalar site sampling method name.
    # @param method_hh Character scalar household sampling method name.
    # @param sample_size Integer sample size.
    # @param n_psu Integer number of primary sampling units.
    # @param n_sites Integer number of sites.
    # @param cluster_size Integer cluster size.
    # @param seed Integer random seed.
    # @return Data frame with sampled PSUs and allocated sample columns.
    # @keywords internal
    ..apply_sampling_method = function(
      frame,
      method_site,
      method_hh,
      sample_size,
      n_psu,
      n_sites,
      cluster_size,
      seed
    ) {
      origin <- "Sample$..apply_sampling_method"
      valid_methods_site <- c(
        "simple_random",
        "proportional",
        "cluster",
        "systematic",
        "purposive"
      )
      phrutils::phr_assert(
        method_site %in% valid_methods_site,
        message = phr_txt(
          "Unknown sampling method '{method_site}' — must be one of: {paste(valid_methods_site, collapse=', ')}."
        ),
        origin = origin
      )

      if (method_site == "simple_random") {
        if (method_hh == "rlc") {
          phrutils::phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'simple_random_rlc' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_srs_rlc(frame, sample_size, n_sites, cs, seed)
        } else {
          phrutils::phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'simple_random' method_site — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_srs(frame, n_sites, sample_size, seed)
        }
      } else if (method_site == "proportional") {
        if (method_hh == "rlc") {
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_proportional_rlc(frame, sample_size, cs, seed)
        } else {
          draw_sample_psu_proportional(frame, sample_size, seed)
        }
      } else if (method_site == "cluster") {
        if (method_hh == "rlc") {
          phrutils::phr_assert(
            !is.null(n_psu) && !is.na(n_psu),
            message = phr_txt(
              "n_psu is required for the 'cluster' method_site — set the 'n_psu' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          # Ensure cluster size is divisible by 3; round up if needed.
          if (cs %% 3L != 0L) {
            cs <- cs + (3L - (cs %% 3L))
          }
          draw_sample_psu_pps_cluster(frame, n_psu, cs, seed)
        } else {
          phrutils::phr_assert(
            !is.null(n_psu) && !is.na(n_psu),
            message = phr_txt(
              "n_psu is required for the 'cluster' method_site — set the 'n_psu' column in the strata table."
            ),
            origin = origin
          )
          phrutils::phr_assert(
            !is.null(cluster_size) && !is.na(cluster_size),
            message = phr_txt(
              "cluster_size is required for the 'cluster' method_site — set the 'cluster_size' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_pps_cluster(frame, n_psu, cluster_size, seed)
        }
      } else if (method_site == "systematic") {
        if (method_hh == "systematic_rlc") {
          phrutils::phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'systematic_rlc' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_systematic_rlc(frame, sample_size, n_sites, cs, seed)
        } else {
          phrutils::phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'systematic' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_systematic(frame, n_sites, sample_size, seed)
        }
      } else {
        draw_sample_psu_purposive(frame, seed)
      }
    },
    # @description Extract sampling parameters from a strata row.
    # @param st_row Data frame single strata row.
    # @param stratum_n_eligible Integer number of eligible units in stratum.
    # @param total_n_eligible Integer total number of eligible units.
    # @return List with elements: method_site, method_hh, sample_size, n_psu, cluster_size, n_sites.
    # @keywords internal
    ..params_from_strata_row = function(
      st_row,
      stratum_n_eligible,
      total_n_eligible
    ) {
      method_site <- as.character(st_row$sampling_method_site[1])
      method_hh <- as.character(st_row$sampling_method_hh[1])

      ss <- if (
        "Final_HH_Sample_Size" %in%
          names(st_row) &&
          !is.na(st_row$Final_HH_Sample_Size[1])
      ) {
        as.integer(st_row$Final_HH_Sample_Size[1])
      } else if (
        "General_HH_Sample_Size" %in%
          names(st_row) &&
          !is.na(st_row$General_HH_Sample_Size[1])
      ) {
        as.integer(st_row$General_HH_Sample_Size[1])
      } else {
        as.integer(round(100 * stratum_n_eligible / max(total_n_eligible, 1L)))
      }

      .na_as_null <- function(x) if (length(x) == 0L || is.na(x)) NULL else x
      list(
        method_site = method_site,
        method_hh = method_hh,
        sample_size = ss,
        n_psu = .na_as_null(
          if ("n_psu" %in% names(st_row)) st_row$n_psu[1] else NA
        ),
        cluster_size = .na_as_null(
          if ("cluster_size" %in% names(st_row)) st_row$cluster_size[1] else NA
        ),
        n_sites = .na_as_null(
          if ("n_sites" %in% names(st_row)) st_row$n_sites[1] else NA
        )
      )
    }
  )
)
