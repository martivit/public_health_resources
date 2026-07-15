# Shared formatting helpers used across protocol classes.

# Format a date value to "DD/MM/YYYY". Returns "" for NULL/NA.
phr_fmt_date_tor <- function(d) {
  if (is.null(d) || (length(d) == 1 && is.na(d))) return("")
  tryCatch(format(as.Date(d), "%d/%m/%Y"), error = function(e) as.character(d))
}

# Format numeric value as a percentage string.
phr_fmt_pct <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("")
  paste0(x, "%")
}

# Format numeric value as comma-separated integer string.
phr_fmt_n <- function(x, suffix = "") {
  if (is.null(x) || is.na(x)) return("")
  s <- formatC(as.integer(round(x)), format = "d", big.mark = ",")
  if (nzchar(suffix)) paste(s, suffix) else s
}

# Format logical FPC flag as "Yes" / "No".
phr_fmt_fpc <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  if (isTRUE(x)) "Yes" else "No"
}

# Human-readable sampling method label.
phr_fmt_sampling_method <- function(m) {
  switch(as.character(m),
    simple_random      = "Simple Random",
    systematic         = "Systematic",
    pps_cluster        = "Cluster (PPS)",
    pps_rlc            = "Cluster (PPS-RLC)",
    simple_random_rlc  = "Simple Random (RLC)",
    systematic_rlc     = "Systematic (RLC)",
    proportional       = "Proportional",
    proportional_rlc   = "Proportional (RLC)",
    purposive          = "Purposive",
    as.character(m)
  )
}
