# ------------------------------------------------
# IPHRA Text Translation / Localization Utility
# ------------------------------------------------

# Translation dictionary (extend later)
phr_translations <- list(
  en = list(
    export_tor = "Export ToR",
    validation_passed = "Validation checks passed (dummy mode).",
    tor_export_success = "ToR export simulated successfully \u2705",
    "To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality." = "To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality.",
    # ---- auto-title static parts ----
    "Distribution of"            = "Distribution of",
    "Cumulative Distribution of" = "Cumulative Distribution of",
    "Z-Score Distribution of"    = "Z-Score Distribution of",
    "Age Distribution of"        = "Age Distribution of",
    "Prevalence of"              = "Prevalence of",
    "Mean of"                    = "Mean of",
    "Treemap of"                 = "Treemap of",
    "Response Distribution"      = "Response Distribution",
    "Domain Radar"               = "Domain Radar",
    "Correlation Matrix"         = "Correlation Matrix",
    "Age-Sex Pyramid"            = "Age-Sex Pyramid",
    "IYCF Area Graph"            = "IYCF Area Graph",
    "Flow Diagram"               = "Flow Diagram",
    "Frequency Table of"         = "Frequency Table of",
    "Cumulative Mean of"         = "Cumulative Mean of",
    "Cumulative SD of"           = "Cumulative SD of",
    "Digit Preference Score of"  = "Digit Preference Score of",
    "Cumulative Count of"        = "Cumulative Count of",
    "Cumulative Ratio of"        = "Cumulative Ratio of",
    ", by"                       = ", by"
  ),
  fr = list(
    export_tor = "Exporter les TdR",
    validation_passed = "V\u00e9rifications termin\u00e9es avec succ\u00e8s (mode fictif).",
    tor_export_success = "Exportation simul\u00e9e des TdR r\u00e9ussie \u2705",
    "To assess the severity of the public health outcomes and identify initial public health priorities for response to mitigate excess morbidity, malnutrition, and mortality." = "\u00c9valuer la s\u00e9v\u00e9rit\u00e9 des r\u00e9sultats de sant\u00e9 publique et identifier les priorit\u00e9s initiales de sant\u00e9 publique pour la r\u00e9ponse afin d'atténuer l'excès de morbidit\u00e9, de malnutrition et de mortalit\u00e9.",
    # ---- auto-title static parts ----
    "Distribution of"            = "Distribution de",
    "Cumulative Distribution of" = "Distribution cumulative de",
    "Z-Score Distribution of"    = "Distribution des z-scores de",
    "Age Distribution of"        = "Distribution par \u00e2ge de",
    "Prevalence of"              = "Pr\u00e9valence de",
    "Mean of"                    = "Moyenne de",
    "Treemap of"                 = "Treemap de",
    "Response Distribution"      = "Distribution des r\u00e9ponses",
    "Domain Radar"               = "Radar des domaines",
    "Correlation Matrix"         = "Matrice de corr\u00e9lation",
    "Age-Sex Pyramid"            = "Pyramide \u00e2ge-sexe",
    "IYCF Area Graph"            = "Graphique ANJE",
    "Flow Diagram"               = "Diagramme de flux",
    "Frequency Table of"         = "Tableau de fr\u00e9quence de",
    "Cumulative Mean of"         = "Moyenne cumulative de",
    "Cumulative SD of"           = "\u00c9cart-type cumulatif de",
    "Digit Preference Score of"  = "Score de pr\u00e9f\u00e9rence num\u00e9rique de",
    "Cumulative Count of"        = "Nombre cumulatif de",
    "Cumulative Ratio of"        = "Ratio cumulatif de",
    ", by"                       = ", par"
  )
)

# Placeholder for current language (can later live in session$userData)
phr_current_lang <- "en"

# ---- Safe Translation Lookup ----
phr_txt <- function(key, lang = NULL, default = NULL, session = NULL) {
  # Lazily obtain the default Shiny reactive domain if shiny is available
  if (is.null(session) && requireNamespace("shiny", quietly = TRUE)) {
    session <- shiny::getDefaultReactiveDomain()
  }
  # [ FUTURE] When language reactivity is connected, use session$userData$lang()
  # If no reactive session available, fallback to phr_current_lang or "en"

  # Use glue::glue to evaluate expressions in {} within the key string
  # This allows dynamic content like phr_txt("Processing {n} items")
  key <- glue::glue(key, .envir = parent.frame())

  if (is.null(lang)) {
    if (!is.null(session) && !is.null(session$userData$lang)) {
      # Safe reactive access \u2014 only works once session$userData$lang is defined
      lang <- tryCatch(session$userData$lang(), error = function(e) NULL)
    }
  }

  # Fallback chain
  if (is.null(lang) || !lang %in% names(phr_translations)) {
    if (exists("phr_current_lang", envir = .GlobalEnv)) {
      lang <- get("phr_current_lang", envir = .GlobalEnv)
    } else {
      lang <- "en"
    }
  }

  # ---- Lookup ----
  value <- phr_translations[[lang]][[key]]

  # ---- Fallback logic ----
  if (is.null(value)) {
    if (!is.null(default)) return(default)
    return(paste0("\u29eb", key, "\u29eb"))  # visually marks missing keys
  }

  return(value)
}
