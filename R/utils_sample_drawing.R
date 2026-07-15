#' Sample Drawing Functions
#'
#' @description
#' Functions for drawing samples from sampling frames using various methods.
#' PSU-level sampling functions (prefixed \code{draw_sample_psu_}) operate on an
#' already-filtered frame of eligible primary sampling units and return that
#' frame augmented with two new columns:
#' \itemize{
#'   \item \code{sampled_psu} — sequential cluster number(s) for selected PSUs;
#'     \code{NA} for unselected PSUs.  For PPS cluster and RLC methods a PSU
#'     may be drawn more than once; in that case \code{sampled_psu} contains
#'     the comma-separated cluster numbers allocated to that PSU
#'     (e.g. \code{"9, 10, 11"} if selected three times).  Reserve clusters are
#'     marked \code{"RC"} and interleaved with the sequential numbers in draw
#'     order (e.g. \code{"1"}, \code{"2"}, \code{"RC"}, \code{"3"}).
#'   \item \code{allocated_sample} — number of households allocated to that
#'     PSU; \code{NA} for unselected or reserve-only PSUs.
#' }
#' The filtering of non-selected PSUs is performed by the caller (e.g.
#' \code{Protocol$draw_sample()}) after the full annotated frame is returned.

# ---------------------------------------------------------------------------
# PSU-level sampling utility functions (called from Protocol$draw_sample)
# ---------------------------------------------------------------------------

# Determine the number of reserve clusters to add based on the main draw size.
# Returns 3 for n_main <= 10, 4 for n_main <= 20, and 5 for n_main > 20.
n_reserve_clusters <- function(n_main) {
  if (n_main <= 10L) {
    3L
  } else if (n_main <= 20L) {
    4L
  } else {
    5L
  }
}

# Assign sequential labels and "RC" to a drawn set of n_total slots in draw
# order, where n_reserve randomly selected slots are labelled "RC" and the
# remaining n_main = n_total - n_reserve are labelled 1, 2, 3, ... (with
# numbering continuing over the RC positions).
# Returns a character vector of length n_total.
assign_reserve_labels <- function(n_total, n_reserve, seed) {
  if (n_reserve == 0L) {
    return(as.character(seq_len(n_total)))
  }
  # Use seed + 1L so the RC-position draw uses a distinct starting state from
  # the main draw seed, ensuring the two draws are statistically independent.
  # Callers that need the main draw to be reproducible should call set.seed(seed)
  # *after* this function.
  set.seed(seed + 1L)
  rc_positions <- sort(sample(seq_len(n_total), n_reserve, replace = FALSE))
  labels <- character(n_total)
  main_counter <- 0L
  for (i in seq_len(n_total)) {
    if (i %in% rc_positions) {
      labels[i] <- "RC"
    } else {
      main_counter <- main_counter + 1L
      labels[i] <- as.character(main_counter)
    }
  }
  labels
}

#' Draw PSUs using simple random sampling (SRS)
#'
#' Randomly selects \code{n_psu} primary sampling units with equal probability
#' (without replacement) and divides \code{sample_size} households evenly
#' across selected PSUs.  Population sizes are not required.
#'
#' In addition to the \code{n_psu} main PSUs, a number of reserve clusters
#' (RC) are drawn in the same pass and marked \code{"RC"} in
#' \code{sampled_psu}: 3 RC when \code{n_psu <= 10}, 4 when
#' \code{10 < n_psu <= 20}, and 5 when \code{n_psu > 20}.  Sequential
#' numbering continues across the RC positions (e.g. \code{1}, \code{2},
#' \code{"RC"}, \code{3}).
#'
#' @param frame Data frame. Eligible PSUs (already filtered for inclusion).
#' @param n_psu Integer. Number of main PSUs to select.
#' @param sample_size Integer. Total household sample size to allocate.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  Unselected PSUs receive \code{NA} in both columns.
#'   Reserve PSUs receive \code{"RC"} in \code{sampled_psu} and \code{NA} in
#'   \code{allocated_sample}.
#' @export
draw_sample_psu_srs <- function(frame, n_psu, sample_size, seed = 42) {
  origin <- "draw_sample_psu_srs"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        n_psu > 0,
        message = phr_txt("n_psu must be a positive integer."),
        origin = origin
      )
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )

      set.seed(seed)
      n_available <- nrow(frame)
      if (n_psu > n_available) {
        phr_warning(
          message = phr_txt(
            "n_psu ({n_psu}) exceeds available PSUs ({n_available}). Using all available PSUs."
          ),
          origin = origin
        )
        n_psu <- n_available
      }

      n_reserve <- n_reserve_clusters(n_psu)
      n_total <- n_psu + n_reserve
      if (n_total > n_available) {
        n_original_reserve <- n_reserve
        n_reserve <- max(0L, n_available - n_psu)
        n_total <- n_psu + n_reserve
        phr_warning(
          message = phr_txt(
            "Only {n_reserve} reserve cluster(s) can be drawn (wanted {n_original_reserve}); frame has too few PSUs."
          ),
          origin = origin
        )
      }

      selected_idx <- sample(seq_len(n_available), n_total, replace = FALSE)
      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      frame$sampled_psu <- NA_character_
      frame$allocated_sample <- NA_real_

      frame$sampled_psu[selected_idx] <- labels

      # Allocate households to main (non-RC) PSUs only
      main_positions <- which(labels != "RC")
      main_idx <- selected_idx[main_positions]
      per_psu <- floor(sample_size / n_psu)
      remainder <- sample_size - per_psu * n_psu
      frame$allocated_sample[main_idx] <- per_psu
      if (remainder > 0) {
        frame$allocated_sample[main_idx[1]] <-
          frame$allocated_sample[main_idx[1]] + remainder
      }

      frame
    },
    on_error = "abort",
    origin = origin
  )
}

#' Allocate households proportional to population size across all PSUs
#'
#' All eligible PSUs are included; household sample is allocated proportionally
#' to each PSU's population size.  Requires a \code{population_size} column.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param sample_size Integer. Total household sample size to allocate.
#' @param seed Integer. Random seed (not used in allocation itself but kept for
#'   interface consistency; default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  All PSUs are selected (allocated proportionally).
#' @export
draw_sample_psu_proportional <- function(frame, sample_size, seed = 42) {
  origin <- "draw_sample_psu_proportional"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        "population_size" %in% names(frame),
        message = phr_txt(
          "Frame must have a 'population_size' column for proportional method."
        ),
        origin = origin
      )
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )

      total_pop <- sum(frame$population_size, na.rm = TRUE)
      phr_assert(
        total_pop > 0,
        message = phr_txt(
          "Total population cannot be zero for proportional method."
        ),
        origin = origin
      )

      frame$sampled_psu <- seq_len(nrow(frame))

      # Hamilton (largest-remainder) method — guarantees sum == sample_size
      exact_alloc <- frame$population_size / total_pop * sample_size
      frame$allocated_sample <- floor(exact_alloc)
      remainder <- sample_size - sum(frame$allocated_sample)
      if (remainder > 0) {
        frac_idx <- order(
          exact_alloc - frame$allocated_sample,
          decreasing = TRUE
        )
        frame$allocated_sample[frac_idx[seq_len(remainder)]] <-
          frame$allocated_sample[frac_idx[seq_len(remainder)]] + 1
      }

      frame
    },
    on_error = "abort",
    origin = origin
  )
}

#' Allocate cluster slots proportional to population size across all PSUs
#'
#' All eligible PSUs are included; \code{ceiling(sample_size / cluster_size)}
#' main cluster slots are allocated proportionally to each PSU's population
#' size (Hamilton largest-remainder method).  Reserve clusters (RC) are
#' interleaved with the main slots: 3 RC when \code{n_clusters <= 10}, 4 when
#' \code{10 < n_clusters <= 20}, and 5 when \code{n_clusters > 20}.
#'
#' Requires a \code{population_size} column.  \code{n_sites} does not apply —
#' all eligible PSUs participate.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param sample_size Integer. Total household sample size (used to derive
#'   \code{n_clusters = ceiling(sample_size / cluster_size)}).
#' @param cluster_size Integer. Households per cluster (default \code{3}).
#' @param seed Integer. Random seed for RC position draw (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  All PSUs are selected.  PSUs that receive only reserve
#'   cluster slots have \code{allocated_sample = NA}.
#' @export
draw_sample_psu_proportional_rlc <- function(
  frame,
  sample_size,
  cluster_size = 3,
  seed = 42
) {
  origin <- "draw_sample_psu_proportional_rlc"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        "population_size" %in% names(frame),
        message = phr_txt(
          "Frame must have a 'population_size' column for proportional_rlc method."
        ),
        origin = origin
      )
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )
      phr_assert(
        cluster_size > 0,
        message = phr_txt("cluster_size must be a positive integer."),
        origin = origin
      )

      total_pop <- sum(frame$population_size, na.rm = TRUE)
      phr_assert(
        total_pop > 0,
        message = phr_txt(
          "Total population cannot be zero for proportional_rlc method."
        ),
        origin = origin
      )

      n_sites <- nrow(frame)
      n_clusters <- ceiling(sample_size / cluster_size)
      n_reserve <- n_reserve_clusters(n_clusters)
      n_total <- n_clusters + n_reserve

      # Allocate n_total cluster slots proportional to population size (Hamilton method)
      slot_alloc <- allocate_slots_to_sites(
        frame$population_size,
        n_total,
        n_sites
      )
      selected_sites <- seq_len(n_sites)

      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      assign_slots_to_frame(
        frame,
        selected_sites,
        slot_alloc,
        labels,
        cluster_size
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw PSUs using PPS cluster sampling (with replacement)
#'
#' Selects \code{n_clusters} clusters proportional to population size using PPS
#' with replacement (via \code{pps::ppswr}).  A single PSU may receive multiple
#' clusters.  Requires a \code{population_size} column.
#'
#' In addition to the \code{n_clusters} main cluster slots, reserve clusters
#' (RC) are drawn in the same pass: 3 RC when \code{n_clusters <= 10}, 4 when
#' \code{10 < n_clusters <= 20}, and 5 when \code{n_clusters > 20}.  Reserve
#' slots are labelled \code{"RC"} and main slots are numbered sequentially, with
#' numbering continuing over the RC positions.  A PSU assigned both main and
#' reserve clusters will have a comma-separated \code{sampled_psu} value such as
#' \code{"3, RC, 5"}.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param n_clusters Integer. Number of main clusters to allocate.
#' @param cluster_size Integer. Number of households per cluster.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  For PSUs receiving multiple clusters,
#'   \code{sampled_psu} contains comma-separated cluster labels including any
#'   \code{"RC"} markers.
#'   \code{allocated_sample} counts main (non-RC) clusters only
#'   (\code{= n_main_clusters_at_psu * cluster_size}).
#'   Unselected PSUs receive \code{NA}.  Reserve-only PSUs receive
#'   \code{sampled_psu = "RC"} and \code{allocated_sample = NA}.
#' @export
draw_sample_psu_pps_cluster <- function(
  frame,
  n_clusters,
  cluster_size,
  seed = 42
) {
  origin <- "draw_sample_psu_pps_cluster"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        "population_size" %in% names(frame),
        message = phr_txt(
          "Frame must have a 'population_size' column for pps_cluster method."
        ),
        origin = origin
      )
      phr_assert(
        n_clusters > 0,
        message = phr_txt("n_clusters must be a positive integer."),
        origin = origin
      )
      phr_assert(
        cluster_size > 0,
        message = phr_txt("cluster_size must be a positive integer."),
        origin = origin
      )

      n_reserve <- n_reserve_clusters(n_clusters)
      n_total <- n_clusters + n_reserve
      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      set.seed(seed)
      sizes <- frame$population_size
      selected <- pps::ppswr(sizes, n_total)

      frame$sampled_psu <- NA_character_
      frame$allocated_sample <- NA_real_

      for (psu_i in unique(selected)) {
        psu_slots <- which(selected == psu_i)
        psu_labels <- labels[psu_slots]
        frame$sampled_psu[psu_i] <- paste(psu_labels, collapse = ", ")
        n_main_at_psu <- sum(psu_labels != "RC")
        frame$allocated_sample[psu_i] <- if (n_main_at_psu > 0L) {
          n_main_at_psu * cluster_size
        } else {
          NA_real_
        }
      }

      frame
    },
    on_error = "abort",
    origin = origin
  )
}

# Allocate n_total cluster slots across n_sites using Hamilton's
# largest-remainder method when population sizes are available (sum > 0),
# otherwise distribute evenly.
# sub_sizes: numeric vector of length n_sites (population sizes for selected sites).
# Returns an integer vector of length n_sites with slot counts.
allocate_slots_to_sites <- function(sub_sizes, n_total, n_sites) {
  total_pop <- sum(sub_sizes, na.rm = TRUE)
  if (!is.na(total_pop) && total_pop > 0) {
    exact_alloc <- sub_sizes / total_pop * n_total
    slot_alloc <- floor(exact_alloc)
    slot_rem <- n_total - sum(slot_alloc)
    if (slot_rem > 0L) {
      fractions <- exact_alloc - slot_alloc
      frac_idx <- order(fractions, decreasing = TRUE)
      slot_alloc[frac_idx[seq_len(slot_rem)]] <-
        slot_alloc[frac_idx[seq_len(slot_rem)]] + 1L
    }
  } else {
    base <- n_total %/% n_sites
    slot_rem <- n_total %% n_sites
    slot_alloc <- rep(base, n_sites)
    if (slot_rem > 0L) {
      slot_alloc[seq_len(slot_rem)] <- slot_alloc[seq_len(slot_rem)] + 1L
    }
  }
  slot_alloc
}

# Populate sampled_psu and allocated_sample columns given a site-selection
# result (selected_sites: integer indices into frame) and per-site slot counts
# (slot_alloc).  labels is the global label vector from assign_reserve_labels().
# Returns the modified frame.
assign_slots_to_frame <- function(
  frame,
  selected_sites,
  slot_alloc,
  labels,
  cluster_size
) {
  n_sites <- length(selected_sites)
  frame$sampled_psu <- NA_character_
  frame$allocated_sample <- NA_real_

  cursor <- 1L
  for (sub_i in seq_len(n_sites)) {
    k <- slot_alloc[sub_i]
    if (k == 0L) {
      next
    } # guard: skip sites that received no slots (edge case)
    site_labels <- labels[cursor:(cursor + k - 1L)]
    cursor <- cursor + k
    frame_row <- selected_sites[sub_i]
    frame$sampled_psu[frame_row] <- paste(site_labels, collapse = ", ")
    n_main_at_psu <- sum(site_labels != "RC")
    frame$allocated_sample[frame_row] <- if (n_main_at_psu > 0L) {
      n_main_at_psu * cluster_size
    } else {
      NA_real_
    }
  }
  frame
}

#' Draw PSUs using random location cluster (RLC) sampling
#'
#' First selects \code{n_sites} primary sampling units using PPS sampling
#' (proportional-to-size, without replacement via \code{pps::ppswor}).  Then
#' distributes \code{ceiling(sample_size / cluster_size)} cluster slots
#' \strong{evenly} across those pre-selected sites (Hamilton largest-remainder
#' method on equal weights, i.e. simple floor + remainder distribution).
#' Requires a \code{population_size} column (needed for the PPS site-selection
#' step) and \code{n_sites} must be provided.
#'
#' Reserve clusters (RC) are drawn at the cluster-allocation stage: 3 RC when
#' \code{n_clusters <= 10}, 4 when \code{10 < n_clusters <= 20}, and 5 when
#' \code{n_clusters > 20}.  Reserve slots are labelled \code{"RC"} and main
#' slots are numbered sequentially, with numbering continuing over the RC
#' positions.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param sample_size Integer. Total household sample size (used to derive
#'   \code{n_clusters = ceiling(sample_size / cluster_size)}).
#' @param n_sites Integer. Number of sites (PSUs) to pre-select with PPS
#'   before the cluster allocation step.  Required.
#' @param cluster_size Integer. Households per cluster (default \code{3}).
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  PSUs outside the pre-selected sites receive \code{NA}.
#'   For pre-selected PSUs that receive at least one cluster slot,
#'   \code{sampled_psu} contains comma-separated cluster labels (numbers and/or
#'   \code{"RC"}).  \code{allocated_sample} counts main (non-RC) cluster
#'   households only.
#' @export
draw_sample_psu_rlc <- function(
  frame,
  sample_size,
  n_sites,
  cluster_size = 3,
  seed = 42
) {
  origin <- "draw_sample_psu_rlc"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        "population_size" %in% names(frame),
        message = phr_txt(
          "Frame must have a 'population_size' column for rlc method."
        ),
        origin = origin
      )
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )
      phr_assert(
        cluster_size > 0,
        message = phr_txt("cluster_size must be a positive integer."),
        origin = origin
      )
      phr_assert(
        !is.null(n_sites) && !is.na(n_sites) && n_sites > 0,
        message = phr_txt(
          "n_sites is required and must be a positive integer for the rlc method."
        ),
        origin = origin
      )

      n_available <- nrow(frame)
      if (n_sites > n_available) {
        phr_warning(
          message = phr_txt(
            "n_sites ({n_sites}) exceeds available PSUs ({n_available}). Using all available PSUs."
          ),
          origin = origin
        )
        n_sites <- n_available
      }

      # Step 1: Pre-select n_sites PSUs using PPS without replacement (Brewer's method)
      set.seed(seed)
      sizes <- frame$population_size
      selected_sites <- pps::ppswr(sizes, n_sites) # integer indices into frame

      # Step 2: Distribute n_total cluster slots evenly across the selected sites.
      n_clusters <- ceiling(sample_size / cluster_size)
      n_reserve <- n_reserve_clusters(n_clusters)
      n_total <- n_clusters + n_reserve

      base <- n_total %/% n_sites
      slot_rem <- n_total %% n_sites
      slot_alloc <- rep(base, n_sites)
      if (slot_rem > 0L) {
        slot_alloc[seq_len(slot_rem)] <- slot_alloc[seq_len(slot_rem)] + 1L
      }

      # Produce the global label sequence (sequential numbers interleaved with "RC")
      # and distribute them to sites in the order of site selection.
      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      # Step 3: Populate sampled_psu / allocated_sample on the full frame
      assign_slots_to_frame(
        frame,
        selected_sites,
        slot_alloc,
        labels,
        cluster_size
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw PSUs using simple-random-selection RLC sampling
#'
#' First selects \code{n_sites} primary sampling units using simple random
#' sampling (SRS, without replacement).  Then allocates
#' \code{ceiling(sample_size / cluster_size)} cluster slots across those
#' pre-selected sites \strong{proportional to population size} when a
#' \code{population_size} column is present and sums to a positive value;
#' otherwise slots are distributed evenly.  \code{n_sites} must be provided.
#'
#' Reserve clusters (RC) are drawn at the cluster-allocation stage: 3 RC when
#' \code{n_clusters <= 10}, 4 when \code{10 < n_clusters <= 20}, and 5 when
#' \code{n_clusters > 20}.
#'
#' @param frame Data frame. Eligible PSUs.  A \code{population_size} column
#'   is optional; if absent or all-zero the cluster allocation is evenly
#'   distributed across the selected sites.
#' @param sample_size Integer. Total household sample size (used to derive
#'   \code{n_clusters = ceiling(sample_size / cluster_size)}).
#' @param n_sites Integer. Number of sites (PSUs) to select.  Required.
#' @param cluster_size Integer. Households per cluster (default \code{3}).
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  PSUs outside the pre-selected sites receive \code{NA}.
#' @export
draw_sample_psu_srs_rlc <- function(
  frame,
  sample_size,
  n_sites,
  cluster_size = 3,
  seed = 42
) {
  origin <- "draw_sample_psu_srs_rlc"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )
      phr_assert(
        cluster_size > 0,
        message = phr_txt("cluster_size must be a positive integer."),
        origin = origin
      )
      phr_assert(
        !is.null(n_sites) && !is.na(n_sites) && n_sites > 0,
        message = phr_txt(
          "n_sites is required and must be a positive integer for the simple_random_rlc method."
        ),
        origin = origin
      )

      n_available <- nrow(frame)
      if (n_sites > n_available) {
        phr_warning(
          message = phr_txt(
            "n_sites ({n_sites}) exceeds available PSUs ({n_available}). Using all available PSUs."
          ),
          origin = origin
        )
        n_sites <- n_available
      }

      # Step 1: Select n_sites PSUs using SRS without replacement
      set.seed(seed)
      selected_sites <- sort(sample(
        seq_len(n_available),
        n_sites,
        replace = FALSE
      ))

      # Step 2: Allocate n_total cluster slots proportional to population size
      #   (Hamilton method), falling back to equal distribution when population
      #   size is unavailable.
      sub_sizes <- if ("population_size" %in% names(frame)) {
        frame$population_size[selected_sites]
      } else {
        rep(0L, n_sites)
      }
      n_clusters <- ceiling(sample_size / cluster_size)
      n_reserve <- n_reserve_clusters(n_clusters)
      n_total <- n_clusters + n_reserve
      slot_alloc <- allocate_slots_to_sites(sub_sizes, n_total, n_sites)

      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      # Step 3: Populate sampled_psu / allocated_sample on the full frame
      assign_slots_to_frame(
        frame,
        selected_sites,
        slot_alloc,
        labels,
        cluster_size
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw PSUs using systematic-random-selection RLC sampling
#'
#' First selects \code{n_sites} primary sampling units via systematic random
#' sampling.  Then allocates \code{ceiling(sample_size / cluster_size)} cluster
#' slots across those pre-selected sites \strong{proportional to population
#' size} when a \code{population_size} column is present and sums to a positive
#' value; otherwise slots are distributed evenly.  \code{n_sites} must be
#' provided.
#'
#' Reserve clusters (RC) are drawn at the cluster-allocation stage: 3 RC when
#' \code{n_clusters <= 10}, 4 when \code{10 < n_clusters <= 20}, and 5 when
#' \code{n_clusters > 20}.
#'
#' @param frame Data frame. Eligible PSUs.  A \code{population_size} column
#'   is optional; if absent or all-zero the cluster allocation is evenly
#'   distributed across the selected sites.
#' @param sample_size Integer. Total household sample size (used to derive
#'   \code{n_clusters = ceiling(sample_size / cluster_size)}).
#' @param n_sites Integer. Number of sites (PSUs) to select.  Required.
#' @param cluster_size Integer. Households per cluster (default \code{3}).
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  PSUs outside the pre-selected sites receive \code{NA}.
#' @export
draw_sample_psu_systematic_rlc <- function(
  frame,
  sample_size,
  n_sites,
  cluster_size = 3,
  seed = 42
) {
  origin <- "draw_sample_psu_systematic_rlc"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )
      phr_assert(
        cluster_size > 0,
        message = phr_txt("cluster_size must be a positive integer."),
        origin = origin
      )
      phr_assert(
        !is.null(n_sites) && !is.na(n_sites) && n_sites > 0,
        message = phr_txt(
          "n_sites is required and must be a positive integer for the systematic_rlc method."
        ),
        origin = origin
      )

      n_available <- nrow(frame)
      if (n_sites > n_available) {
        phr_warning(
          message = phr_txt(
            "n_sites ({n_sites}) exceeds available PSUs ({n_available}). Using all available PSUs."
          ),
          origin = origin
        )
        n_sites <- n_available
      }

      # Step 1: Select n_sites PSUs using systematic random sampling
      set.seed(seed)
      interval <- n_available / n_sites
      random_start <- sample(seq_len(max(1L, round(interval))), 1)
      selected_sites <- round(seq(
        random_start,
        by = interval,
        length.out = n_sites
      ))
      selected_sites <- pmin(selected_sites, n_available)

      # Step 2: Allocate n_total cluster slots proportional to population size
      #   (Hamilton method), falling back to equal distribution when population
      #   size is unavailable.
      sub_sizes <- if ("population_size" %in% names(frame)) {
        frame$population_size[selected_sites]
      } else {
        rep(0L, n_sites)
      }
      n_clusters <- ceiling(sample_size / cluster_size)
      n_reserve <- n_reserve_clusters(n_clusters)
      n_total <- n_clusters + n_reserve
      slot_alloc <- allocate_slots_to_sites(sub_sizes, n_total, n_sites)

      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      # Step 3: Populate sampled_psu / allocated_sample on the full frame
      assign_slots_to_frame(
        frame,
        selected_sites,
        slot_alloc,
        labels,
        cluster_size
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw PSUs using systematic random sampling
#'
#' Selects \code{n_sites} PSUs systematically (population size is ignored).
#' The sampling interval is \code{n_available / n_total} and a random start
#' is drawn uniformly from \code{1} to \code{round(interval)}.
#'
#' In addition to the \code{n_sites} main PSUs, reserve clusters (RC) are
#' drawn in the same systematic pass: 3 RC when \code{n_sites <= 10}, 4 when
#' \code{10 < n_sites <= 20}, and 5 when \code{n_sites > 20}.  Sequential
#' numbering continues across the RC positions.
#'
#' @param frame Data frame. Eligible PSUs.
#' @param n_sites Integer. Number of main sites (PSUs) to select.
#' @param sample_size Integer. Total household sample size to allocate evenly
#'   across selected main PSUs.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  Unselected PSUs receive \code{NA}.  Reserve PSUs receive
#'   \code{"RC"} in \code{sampled_psu} and \code{NA} in
#'   \code{allocated_sample}.
#' @export
draw_sample_psu_systematic <- function(frame, n_sites, sample_size, seed = 42) {
  origin <- "draw_sample_psu_systematic"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        n_sites > 0,
        message = phr_txt("n_sites must be a positive integer."),
        origin = origin
      )
      phr_assert(
        sample_size > 0,
        message = phr_txt("sample_size must be positive."),
        origin = origin
      )

      set.seed(seed)
      n_available <- nrow(frame)
      if (n_sites > n_available) {
        phr_warning(
          message = phr_txt(
            "n_sites ({n_sites}) exceeds available PSUs ({n_available}). Using all available PSUs."
          ),
          origin = origin
        )
        n_sites <- n_available
      }

      n_reserve <- n_reserve_clusters(n_sites)
      n_total <- n_sites + n_reserve
      if (n_total > n_available) {
        n_original_reserve <- n_reserve
        n_reserve <- max(0L, n_available - n_sites)
        n_total <- n_sites + n_reserve
        phr_warning(
          message = phr_txt(
            "Only {n_reserve} reserve cluster(s) can be drawn (wanted {n_original_reserve}); frame has too few PSUs."
          ),
          origin = origin
        )
      }

      interval <- n_available / n_total
      random_start <- sample(seq_len(max(1L, round(interval))), 1)
      sample_idx <- round(seq(
        random_start,
        by = interval,
        length.out = n_total
      ))
      sample_idx <- pmin(sample_idx, n_available)

      labels <- assign_reserve_labels(n_total, n_reserve, seed)

      frame$sampled_psu <- NA_character_
      frame$allocated_sample <- NA_real_

      frame$sampled_psu[sample_idx] <- labels

      # Allocate households to main (non-RC) PSUs only
      main_positions <- which(labels != "RC")
      main_sample_idx <- sample_idx[main_positions]
      per_site <- floor(sample_size / n_sites)
      remainder <- sample_size - per_site * n_sites
      frame$allocated_sample[main_sample_idx] <- per_site
      if (remainder > 0) {
        frame$allocated_sample[main_sample_idx[1]] <-
          frame$allocated_sample[main_sample_idx[1]] + remainder
      }

      frame
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw PSUs using purposive / convenience sampling
#'
#' Marks the frame as having purposive sampling selected — all PSUs are
#' retained but \code{sampled_psu} and \code{allocated_sample} are left as
#' \code{NA}.  The user is expected to manually designate selected PSUs after
#' this call.
#'
#' @param frame Data frame. Eligible PSUs.
#' @param seed Integer. Not used; kept for interface consistency (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added, both set to \code{NA}.
#' @export
draw_sample_psu_purposive <- function(frame, seed = 42) {
  origin <- "draw_sample_psu_purposive"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)

      frame$sampled_psu <- NA_integer_
      frame$allocated_sample <- NA_real_

      frame
    },
    on_error = "abort",
    origin = origin
  )
}

# ---------------------------------------------------------------------------
# Legacy helpers kept for backward compatibility
# ---------------------------------------------------------------------------

#' Draw simple random sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_srs <- function(frame, n, seed = NULL) {
  origin <- "draw_sample_srs"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)

      if (n > nrow(frame)) {
        phr_warning(
          message = phr_txt(
            "Sample size {n} exceeds frame size {nrow(frame)}. Drawing all available units."
          ),
          origin = origin
        )
        n <- nrow(frame)
      }

      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }
      set.seed(seed)

      sample_idx <- sample(seq_len(nrow(frame)), n, replace = FALSE)
      sample_data <- frame[sample_idx, ]

      attr(sample_data, "sampling_method") <- "simple_random"
      attr(sample_data, "seed") <- seed
      attr(sample_data, "n_planned") <- n
      attr(sample_data, "n_drawn") <- nrow(sample_data)
      attr(sample_data, "date_drawn") <- Sys.time()

      sample_data
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw probability proportional to size (PPS) sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame with population_size column
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_pps <- function(frame, n, seed = NULL) {
  origin <- "draw_sample_pps"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      phr_assert(
        "population_size" %in% names(frame),
        message = phr_txt(
          "Frame must have a 'population_size' column for PPS sampling."
        ),
        origin = origin
      )
      if (n > nrow(frame)) {
        phr_warning(
          message = phr_txt(
            "Sample size {n} exceeds frame size {nrow(frame)}. Drawing all available units."
          ),
          origin = origin
        )
        n <- nrow(frame)
      }

      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }
      set.seed(seed)

      total_pop <- sum(frame$population_size, na.rm = TRUE)
      probs <- frame$population_size / total_pop
      sample_idx <- sample(
        seq_len(nrow(frame)),
        n,
        replace = FALSE,
        prob = probs
      )
      sample_data <- frame[sample_idx, ]

      sample_data$selection_probability <- probs[sample_idx]
      sample_data$sampling_weight <- 1 / sample_data$selection_probability

      attr(sample_data, "sampling_method") <- "pps"
      attr(sample_data, "seed") <- seed
      attr(sample_data, "n_planned") <- n
      attr(sample_data, "n_drawn") <- nrow(sample_data)
      attr(sample_data, "date_drawn") <- Sys.time()

      sample_data
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw systematic sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_systematic <- function(frame, n, seed = NULL) {
  origin <- "draw_sample_systematic"

  phr_try(
    {
      phr_validate_dataframe(frame, origin = origin, soft = FALSE)
      if (n > nrow(frame)) {
        phr_warning(
          message = phr_txt(
            "Sample size {n} exceeds frame size {nrow(frame)}. Drawing all available units."
          ),
          origin = origin
        )
        n <- nrow(frame)
      }

      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }
      set.seed(seed)

      interval <- floor(nrow(frame) / n)
      if (interval < 1) {
        interval <- 1
      }
      start <- sample(seq_len(interval), 1)
      sample_idx <- seq(start, nrow(frame), by = interval)[seq_len(n)]
      sample_data <- frame[sample_idx, ]

      attr(sample_data, "sampling_method") <- "systematic"
      attr(sample_data, "seed") <- seed
      attr(sample_data, "interval") <- interval
      attr(sample_data, "start") <- start
      attr(sample_data, "n_planned") <- n
      attr(sample_data, "n_drawn") <- nrow(sample_data)
      attr(sample_data, "date_drawn") <- Sys.time()

      sample_data
    },
    on_error = "abort",
    origin = origin
  )
}

#' Draw stratified sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame with stratum column
#' @param sample_table Data frame. Sample table with stratum_id and sample_size
#' @param method Character. Sampling method within strata: "srs", "pps", "systematic"
#' @param seed Integer. Random seed for reproducibility
#' @return List with drawn samples by stratum and combined sample
#' @export
draw_sample_stratified <- function(
  frame,
  sample_table,
  method = "srs",
  seed = NULL
) {
  origin <- "draw_sample_stratified"

  phr_try(
    {
      phr_assert(
        is.data.frame(frame) && is.data.frame(sample_table),
        message = phr_txt("Both frame and sample_table must be data frames."),
        origin = origin
      )
      phr_assert(
        "stratum" %in% names(frame),
        message = phr_txt("Frame must have a 'stratum' column."),
        origin = origin
      )
      phr_assert(
        all(c("stratum_id", "sample_size") %in% names(sample_table)),
        message = phr_txt(
          "sample_table must have 'stratum_id' and 'sample_size' columns."
        ),
        origin = origin
      )
      valid_methods <- c("srs", "pps", "systematic")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt(
          "Unknown sampling method '{method}'. Must be one of: {paste(valid_methods, collapse=', ')}."
        ),
        origin = origin
      )

      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }

      samples_by_stratum <- list()

      for (i in seq_len(nrow(sample_table))) {
        stratum_id <- sample_table$stratum_id[i]
        n_needed <- sample_table$sample_size[i]
        stratum_frame <- frame[frame$stratum == stratum_id, ]

        if (nrow(stratum_frame) == 0) {
          phr_warning(
            message = phr_txt(
              "No units found in frame for stratum: {stratum_id}."
            ),
            origin = origin
          )
          next
        }

        stratum_sample <- if (method == "srs") {
          draw_sample_srs(stratum_frame, n_needed, seed = seed + i)
        } else if (method == "pps") {
          draw_sample_pps(stratum_frame, n_needed, seed = seed + i)
        } else {
          draw_sample_systematic(stratum_frame, n_needed, seed = seed + i)
        }

        samples_by_stratum[[stratum_id]] <- stratum_sample
      }

      if (length(samples_by_stratum) > 0) {
        combined_sample <- do.call(rbind, samples_by_stratum)
        rownames(combined_sample) <- NULL
        attr(combined_sample, "sampling_design") <- "stratified"
        attr(combined_sample, "within_stratum_method") <- method
        attr(combined_sample, "base_seed") <- seed
      } else {
        combined_sample <- NULL
      }

      list(
        by_stratum = samples_by_stratum,
        combined = combined_sample,
        method = method,
        seed = seed
      )
    },
    on_error = "abort",
    origin = origin
  )
}

#' Allocate households to selected clusters (legacy helper)
#'
#' @param sample Data frame. Drawn cluster sample
#' @param households_per_cluster Integer. Number of households to allocate per cluster
#' @param method Character. Allocation method: "equal", "pps"
#' @return Data frame with household allocations
#' @export
allocate_households <- function(
  sample,
  households_per_cluster,
  method = "equal"
) {
  origin <- "allocate_households"

  phr_try(
    {
      phr_validate_dataframe(sample, origin = origin, soft = FALSE)
      valid_methods <- c("equal", "pps")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt(
          "method must be one of: {paste(valid_methods, collapse=', ')}."
        ),
        origin = origin
      )

      if (method == "equal") {
        sample$households_allocated <- households_per_cluster
      } else {
        # pps
        phr_assert(
          "population_size" %in% names(sample),
          message = phr_txt(
            "Sample must have 'population_size' column for PPS allocation."
          ),
          origin = origin
        )
        total_pop <- sum(sample$population_size, na.rm = TRUE)
        total_households <- households_per_cluster * nrow(sample)
        sample$households_allocated <- pmax(
          round((sample$population_size / total_pop) * total_households),
          1
        )
        diff_val <- total_households - sum(sample$households_allocated)
        if (diff_val != 0) {
          largest_idx <- which.max(sample$population_size)
          sample$households_allocated[largest_idx] <-
            sample$households_allocated[largest_idx] + diff_val
        }
      }

      sample
    },
    on_error = "abort",
    origin = origin
  )
}

#' Generate replacement samples (legacy helper)
#'
#' @param frame Data frame. The sampling frame
#' @param drawn_sample Data frame. Already drawn sample
#' @param n_replacements Integer. Number of replacements per selected unit
#' @param method Character. Sampling method: "srs", "pps"
#' @param seed Integer. Random seed
#' @return Data frame with replacement samples
#' @export
generate_replacements <- function(
  frame,
  drawn_sample,
  n_replacements = 2,
  method = "srs",
  seed = NULL
) {
  origin <- "generate_replacements"

  phr_try(
    {
      phr_assert(
        is.data.frame(frame) && is.data.frame(drawn_sample),
        message = phr_txt("Both frame and drawn_sample must be data frames."),
        origin = origin
      )
      phr_assert(
        "id" %in% names(frame) && "id" %in% names(drawn_sample),
        message = phr_txt(
          "Both frame and drawn_sample must have an 'id' column."
        ),
        origin = origin
      )
      valid_methods <- c("srs", "pps")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt(
          "method must be one of: {paste(valid_methods, collapse=', ')}."
        ),
        origin = origin
      )

      if (is.null(seed)) {
        seed <- as.integer(Sys.time())
      }
      set.seed(seed)

      remaining_frame <- frame[!frame$id %in% drawn_sample$id, ]

      if (nrow(remaining_frame) == 0) {
        phr_warning(
          message = phr_txt("No units remaining in frame for replacements."),
          origin = origin
        )
        return(NULL)
      }

      n_needed <- nrow(drawn_sample) * n_replacements

      replacements <- if (method == "srs") {
        draw_sample_srs(
          remaining_frame,
          min(n_needed, nrow(remaining_frame)),
          seed = seed
        )
      } else {
        draw_sample_pps(
          remaining_frame,
          min(n_needed, nrow(remaining_frame)),
          seed = seed
        )
      }

      replacements$replacement_for <- rep(
        drawn_sample$id,
        each = n_replacements
      )[seq_len(nrow(replacements))]
      replacements$replacement_order <- rep(
        seq_len(n_replacements),
        length.out = nrow(replacements)
      )

      replacements
    },
    on_error = "abort",
    origin = origin
  )
}

#' Get sampling metadata from drawn sample (legacy helper)
#'
#' @param sample Data frame. Drawn sample with metadata attributes
#' @return List with sampling metadata
#' @export
get_sample_metadata <- function(sample) {
  metadata <- list(
    sampling_method = attr(sample, "sampling_method"),
    seed = attr(sample, "seed"),
    n_planned = attr(sample, "n_planned"),
    n_drawn = attr(sample, "n_drawn"),
    date_drawn = attr(sample, "date_drawn")
  )

  if (!is.null(attr(sample, "sampling_design"))) {
    metadata$sampling_design <- attr(sample, "sampling_design")
    metadata$within_stratum_method <- attr(sample, "within_stratum_method")
  }
  if (!is.null(attr(sample, "interval"))) {
    metadata$interval <- attr(sample, "interval")
    metadata$start <- attr(sample, "start")
  }

  return(metadata)
}
