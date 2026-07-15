test_that("IPHRAProtocol initializes with an ANAFramework", {
  skip_if_not(
    (file.exists(system.file("resources", "reference_objectives.xlsx", package = "phr")) ||
       file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file("resources", "reference_indicator_bank.xlsx", package = "phr")) ||
         file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  expect_true(inherits(p, "IPHRAProtocol"))
  expect_true(inherits(p, "Protocol"))
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("IPHRAProtocol$get_allowable_tools returns expected tool names", {
  p <- IPHRAProtocol$new()
  tools <- p$get_allowable_tools()
  expect_true(is.character(tools))
  expect_true("tool_household_iphra_v2"                      %in% tools)
  expect_true("tool_kii_community_iphra_v2"                  %in% tools)
  expect_true("tool_kii_health_service_provider_iphra_v2"    %in% tools)
  expect_true("tool_kii_wash_service_provider_iphra_v2"      %in% tools)
  expect_true("tool_kii_nutrition_service_provider_iphra_v2" %in% tools)
  expect_true("tool_obs_water_point_iphra_v2"                %in% tools)
  expect_length(tools, 12L)
})

test_that("IPHRAProtocol$add_tools rejects unknown tool names", {
  p <- IPHRAProtocol$new()
  expect_error(p$add_tools("unknown_tool_xyz"))
})

test_that("IPHRAProtocol$add_tools stores the tool in the tools list by name", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_true("tool_household_iphra_v2" %in% names(p$tools))
  expect_true(inherits(p$tools[["tool_household_iphra_v2"]], "HouseholdTool"))
})

test_that("IPHRAProtocol$add_tools creates KeyInformantTool for KII tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_kii_community_iphra_v2")
  expect_true(inherits(p$tools[["tool_kii_community_iphra_v2"]], "KeyInformantTool"))
})

test_that("IPHRAProtocol$add_tools creates ObservationTool for obs tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_true(inherits(p$tools[["tool_obs_water_point_iphra_v2"]], "ObservationTool"))
})

test_that("Protocol initializes with framework_type='none' by default", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
  expect_false(inherits(p$framework, "ANAFramework"))
})

# ── New optional metadata fields ────────────────────────────────────────────

test_that("IPHRAProtocol stores optional metadata on initialize", {
  p <- IPHRAProtocol$new(
    assessment_title    = "Test IPHRA",
    country_name        = "Somalia",
    month_year          = "March 2025",
    version             = 2L,
    type_of_emergency   = "conflict",
    type_of_crisis      = "protracted",
    mandating_body      = "UNHCR",
    project_code        = "SOM123",
    pilot_date          = "2025-03-01",
    data_start_date     = "2025-03-05",
    data_end_date       = "2025-03-20",
    `audience_type.strategic`    = TRUE,
    `audience_type.programmatic` = TRUE,
    population          = c("idp_camp", "host_community"),
    gender_disaggregation = TRUE,
    access              = "public"
  )
  expect_equal(p$metadata$assessment_title, "Test IPHRA")
  expect_equal(p$metadata$version, 2L)
  expect_equal(p$metadata$type_of_emergency, "conflict")
  expect_equal(p$metadata$type_of_crisis, "protracted")
  expect_equal(p$metadata$mandating_body, "UNHCR")
  expect_equal(p$metadata$project_code, "SOM123")
  expect_equal(p$metadata$pilot_date, "01/03/2025")
  expect_equal(p$metadata$data_start_date, "05/03/2025")
  expect_equal(p$metadata$data_end_date, "20/03/2025")
  expect_true(p$metadata[["audience_type.strategic"]])
  expect_true(p$metadata[["audience_type.programmatic"]])
  expect_false(p$metadata[["audience_type.other"]])
  expect_equal(p$metadata$population, c("idp_camp", "host_community"))
  expect_true(p$metadata$gender_disaggregation)
  expect_equal(p$metadata$access, "public")
})

test_that("IPHRAProtocol defaults audience_type.operational and .programmatic to TRUE", {
  p <- IPHRAProtocol$new()
  expect_false(p$metadata[["audience_type.strategic"]])
  expect_true(p$metadata[["audience_type.operational"]])
  expect_true(p$metadata[["audience_type.programmatic"]])
  expect_false(p$metadata[["audience_type.other"]])
})

test_that("IPHRAProtocol stores general_objective with IPHRA default", {
  p <- IPHRAProtocol$new()
  expect_true(is.character(p$metadata$general_objective))
  expect_true(nzchar(p$metadata$general_objective))
  expect_true(grepl("public health", p$metadata$general_objective))
})

test_that("IPHRAProtocol stores geographic_coverage", {
  p <- IPHRAProtocol$new(geographic_coverage = "Northern Somalia")
  expect_equal(p$metadata$geographic_coverage, "Northern Somalia")
})

test_that("IPHRAProtocol backward-compat: geographic_description populates geographic_coverage", {
  p <- IPHRAProtocol$new(geographic_description = "Southern Somalia")
  expect_equal(p$metadata$geographic_coverage, "Southern Somalia")
})

test_that("IPHRAProtocol stores pop boolean fields", {
  p <- IPHRAProtocol$new(pop_idpcamp = TRUE, pop_host = TRUE)
  expect_true(p$metadata[["pop_idpcamp"]])
  expect_true(p$metadata[["pop_host"]])
  expect_false(p$metadata[["pop_refugee"]])
  expect_false(p$metadata[["pop_other"]])
})

test_that("Protocol base class metadata contains new fields", {
  p <- Protocol$new()
  expect_null(p$metadata$mandating_body)
  expect_null(p$metadata$general_objective)
  expect_false(p$metadata[["audience_type.strategic"]])
  expect_false(p$metadata[["pop_idpcamp"]])
})

test_that("Protocol and SurveyProtocol initialize with blank protocol_schema", {
  p <- Protocol$new()
  sp <- SurveyProtocol$new()
  req_cols <- c("tag_name", "handling", "condition", "default_value")

  expect_true(is.data.frame(p$protocol_schema))
  expect_true(is.data.frame(sp$protocol_schema))
  expect_true(all(req_cols %in% names(p$protocol_schema)))
  expect_true(all(req_cols %in% names(sp$protocol_schema)))
})

test_that("IPHRAProtocol initializes protocol_schema from bundled iphra schema", {
  p <- IPHRAProtocol$new()
  req_cols <- c("tag_name", "handling", "condition", "default_value")

  expect_true(is.data.frame(p$protocol_schema))
  expect_true(all(req_cols %in% names(p$protocol_schema)))
  expect_true(nrow(p$protocol_schema) > 0)
  expect_true(any(p$protocol_schema$handling == "row_delete"))
})

test_that("IPHRAProtocol stores secondary_data", {
  p <- IPHRAProtocol$new()
  p$secondary_data <- list(OBJ01 = "ACLED conflict database",
                           OBJ02 = "UNHCR population figures")
  expect_equal(length(p$secondary_data), 2L)
  expect_equal(p$secondary_data$OBJ01, "ACLED conflict database")
})

# ── Protocol helpers ─────────────────────────────────────────────────────────

test_that("Protocol touch updates modified_datetime via update_metadata", {
  p <- Protocol$new()
  t_before <- p$metadata$modified_datetime
  Sys.sleep(0.01)
  p$update_metadata(country_name = "TestLand")
  expect_true(p$metadata$modified_datetime > t_before)
})

test_that("Protocol can access framework schema via access_nested", {
  skip_if_not(
    (file.exists(system.file("resources", "reference_objectives.xlsx", package = "phr")) ||
       file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file("resources", "reference_indicator_bank.xlsx", package = "phr")) ||
         file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  schema <- p$access_nested(field = "framework", member = "master_objectives_schema")
  expect_true(is.data.frame(schema))
  expect_true(nrow(schema) > 0)
})

test_that("Protocol can derive indicator codes from framework schema via access_nested", {
  skip_if_not(
    (file.exists(system.file("resources", "reference_objectives.xlsx", package = "phr")) ||
       file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file("resources", "reference_indicator_bank.xlsx", package = "phr")) ||
         file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  schema <- p$access_nested(field = "framework", member = "master_objectives_schema")
  codes <- unique(as.character(schema$indicator_code))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  expect_true(is.character(codes))
  expect_true(length(codes) > 0)
})

test_that("Protocol$get_tool_names returns empty vector before add_tools", {
  p <- IPHRAProtocol$new()
  expect_equal(p$get_tool_names(), character(0))
})

test_that("Protocol$get_tool_names returns tool names after add_tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_equal(p$get_tool_names(), "tool_household_iphra_v2")
})

test_that("Protocol$is_tool_included returns FALSE for unknown tool", {
  p <- IPHRAProtocol$new()
  expect_false(p$is_tool_included("tool_household_iphra_v2"))
})

test_that("Protocol$is_tool_included returns TRUE after add_tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_true(p$is_tool_included("tool_household_iphra_v2"))
})

test_that("Protocol can derive indicator codes from tools via access_nested", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_kii_community_iphra_v2")
  tool_names <- p$get_tool_names()
  codes <- character(0)
  for (tn in tool_names) {
    codes <- c(
      codes,
      as.character(
        p$access_nested(
          field = "tools",
          name = tn,
          member = "get_indicator_codes",
          prefer_revised = TRUE
        )
      )
    )
  }
  codes <- unique(codes[nzchar(codes)])
  expect_true(is.character(codes))
})

test_that("Protocol can filter framework schema rows without wrapper methods", {
  skip_if_not(
    (file.exists(system.file("resources", "reference_objectives.xlsx", package = "phr")) ||
       file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file("resources", "reference_indicator_bank.xlsx", package = "phr")) ||
         file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  master_schema <- p$access_nested(field = "framework", member = "master_objectives_schema")
  all_codes <- unique(as.character(master_schema$indicator_code))
  all_codes <- all_codes[!is.na(all_codes) & nzchar(all_codes)]
  if (length(all_codes) >= 2) {
    sub_codes <- all_codes[1:2]
    filtered  <- master_schema[as.character(master_schema$indicator_code) %in% sub_codes, , drop = FALSE]
    expect_true(is.data.frame(filtered))
    expect_true(nrow(filtered) > 0)
    expect_true(all(as.character(filtered$indicator_code) %in% sub_codes))
  }
})

# ── New metadata fields ───────────────────────────────────────────────────────

test_that("IPHRAProtocol stores stakeholder_mapping field", {
  p <- IPHRAProtocol$new(stakeholder_mapping = TRUE)
  expect_true(p$metadata$stakeholder_mapping)
  p2 <- IPHRAProtocol$new()
  expect_false(p2$metadata$stakeholder_mapping)
})

test_that("IPHRAProtocol stores num_geographic_units as numeric", {
  p <- IPHRAProtocol$new(num_geographic_units = 5)
  expect_equal(p$metadata$num_geographic_units, 5)
  expect_true(is.numeric(p$metadata$num_geographic_units))
})

test_that("IPHRAProtocol stores popsize_known_geographic_unit field", {
  p <- IPHRAProtocol$new(popsize_known_geographic_unit = TRUE)
  expect_true(p$metadata$popsize_known_geographic_unit)
  p2 <- IPHRAProtocol$new()
  expect_false(p2$metadata$popsize_known_geographic_unit)
})

test_that("IPHRAProtocol num_strata_units defaults to 0", {
  p <- IPHRAProtocol$new()
  expect_equal(p$metadata$num_strata_units, 0L)
})

test_that("IPHRAProtocol stores popsize_known_strata_unit field", {
  p <- IPHRAProtocol$new(popsize_known_strata_unit = TRUE)
  expect_true(p$metadata$popsize_known_strata_unit)
})

test_that("IPHRAProtocol stores user-defined numeric target fields", {
  p <- IPHRAProtocol$new(
    num_kii_health_target    = 10,
    num_kii_market_target    = 5,
    num_kii_fsl_target       = 3,
    num_kii_wash_target      = 4,
    num_kii_nutrition_target = 6,
    num_obs_health_target    = 8,
    num_obs_latrine_target   = 12,
    num_obs_waterpoint_target = 7
  )
  expect_equal(p$metadata$num_kii_health_target, 10)
  expect_equal(p$metadata$num_kii_market_target, 5)
  expect_equal(p$metadata$num_kii_fsl_target, 3)
  expect_equal(p$metadata$num_kii_wash_target, 4)
  expect_equal(p$metadata$num_kii_nutrition_target, 6)
  expect_equal(p$metadata$num_obs_health_target, 8)
  expect_equal(p$metadata$num_obs_latrine_target, 12)
  expect_equal(p$metadata$num_obs_waterpoint_target, 7)
})

test_that("Protocol$update_metadata updates fields and touch()", {
  p <- IPHRAProtocol$new()
  t_before <- p$metadata$modified_datetime
  Sys.sleep(0.01)
  p$update_metadata(country_name = "Kenya", num_geographic_units = 3)
  expect_equal(p$metadata$country_name, "Kenya")
  expect_equal(p$metadata$num_geographic_units, 3)
  expect_true(p$metadata$modified_datetime > t_before)
})

test_that("Protocol$update_metadata errors with no named arguments", {
  p <- Protocol$new()
  expect_error(p$update_metadata())
})

test_that("IPHRAProtocol general_objective default contains 'public health'", {
  p <- IPHRAProtocol$new()
  expect_true(grepl("public health", p$metadata$general_objective, ignore.case = TRUE))
})

test_that("IPHRAProtocol add_stratum updates num_strata_units", {
  p <- IPHRAProtocol$new()
  expect_equal(p$metadata$num_strata_units, 0L)
  p$add_stratum(stratum_id = "north", Population_Name = "Northern Region")
  expect_equal(p$metadata$num_strata_units, 1L)
  p$add_stratum(stratum_id = "south", Population_Name = "Southern Region")
  expect_equal(p$metadata$num_strata_units, 2L)
})

test_that("IPHRAProtocol access_nested remove_stratum updates num_strata_units", {
  p <- IPHRAProtocol$new()
  p$access_nested(field = "sample_object", member = "add_stratum",
                  stratum_id = "north", stratum_name = "Northern Region", sampling_method = "purposive")
  p$access_nested(field = "sample_object", member = "add_stratum",
                  stratum_id = "south", stratum_name = "Southern Region", sampling_method = "purposive")

  p$access_nested(field = "sample_object", member = "remove_stratum", "Northern Region")

  expect_equal(p$metadata$num_strata_units, 1L)
  expect_equal(
    p$access_nested(field = "sample_object", member = "get_strata_names"),
    "Southern Region"
  )
})

test_that("IPHRAProtocol schema 'replace' handling uses protocol_schema default_value", {
  p <- IPHRAProtocol$new()
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "@tor_title", style = "Normal")
  schema <- p$protocol_schema
  row <- schema[schema$tag_name == "@tor_title", c("tag_name", "handling", "condition", "default_value"), drop = FALSE]
  expect_gt(nrow(row), 0L, info = "Expected @tor_title in protocol_schema.")
  doc <- p$.__enclos_env__$private$..handle_replace(doc, row[1, , drop = FALSE])
  body_xml <- officer::docx_body_xml(doc)
  txt <- paste(
    xml2::xml_text(xml2::xml_find_all(body_xml, ".//w:t", xml2::xml_ns(body_xml))),
    collapse = ""
  )
  expect_true(grepl("Research Terms of Reference", txt, fixed = TRUE))
})

test_that("IPHRAProtocol normalizes schema tag to @kii_nut_inc", {
  p <- IPHRAProtocol$new()
  expect_true("@kii_nut_inc" %in% p$protocol_schema$tag_name)
  expect_false("@kii_nutrition_inc" %in% p$protocol_schema$tag_name)
})

test_that("IPHRAProtocol .replace avoids replacing tag prefixes in longer placeholders", {
  p <- IPHRAProtocol$new()
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "@sample_size_hh_ind_table", style = "Normal")
  doc <- officer::body_add_par(doc, "@sample_size_hh_ind", style = "Normal")

  doc <- p$.__enclos_env__$private$..replace(
    doc,
    "@sample_size_hh_ind",
    "placeholder ind sampling"
  )

  body_xml <- officer::docx_body_xml(doc)
  txt <- paste(
    xml2::xml_text(xml2::xml_find_all(body_xml, ".//w:t", xml2::xml_ns(body_xml))),
    collapse = ""
  )

  expect_true(grepl("@sample_size_hh_ind_table", txt, fixed = TRUE))
  expect_false(grepl("placeholder ind sampling_table", txt, fixed = TRUE))
  expect_true(grepl("placeholder ind sampling", txt, fixed = TRUE))
})

test_that("replace_tag_in_cell preserves item order for objective headers and bullets", {
  p <- IPHRAProtocol$new()
  doc <- officer::read_docx()
  doc <- officer::body_add_table(doc, value = data.frame(col1 = "@specific_objectives"))
  items <- list(
    list(text = "IncomeCoping", bold = TRUE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Coping strategies", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Household income", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "LivingConditions", bold = TRUE, space_before_pt = 6L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Crowdedness", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)
  )

  ok <- p$.__enclos_env__$private$..replace_tag_in_cell(doc, "@specific_objectives", items)
  expect_true(ok)

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  tc <- xml2::xml_find_first(body_xml, ".//w:tc", ns = ns)
  paras <- xml2::xml_find_all(tc, ".//w:p", ns = ns)
  texts <- vapply(paras, xml2::xml_text, character(1L))
  texts <- texts[nzchar(texts)]

  idx_income <- match("IncomeCoping", texts)
  idx_coping <- match("\u2022 Coping strategies", texts)
  idx_income_b <- match("\u2022 Household income", texts)
  idx_living <- match("LivingConditions", texts)
  idx_crowded <- match("\u2022 Crowdedness", texts)

  expect_true(all(!is.na(c(idx_income, idx_coping, idx_income_b, idx_living, idx_crowded))))
  expect_true(idx_income < idx_coping)
  expect_true(idx_coping < idx_income_b)
  expect_true(idx_living < idx_crowded)
})

test_that("IPHRAProtocol initializes conditional_metadata with FALSE defaults", {
  p <- IPHRAProtocol$new()
  expected <- c(
    "srs_srs", "srs_systematic", "srs_rlc", "systematic", "systematic_rlc",
    "proportional_rlc", "cluster_rlc", "cluster", "proportional", "purposive"
  )
  expect_true(is.list(p$conditional_metadata))
  expect_true(all(expected %in% names(p$conditional_metadata)))
  expect_true(all(vapply(p$conditional_metadata[expected], isFALSE, logical(1))))
  schema_conditions <- if ("condition" %in% names(p$protocol_schema)) {
    trimws(as.character(p$protocol_schema$condition))
  } else {
    character(0)
  }
  schema_conditions <- schema_conditions[nzchar(schema_conditions)]
  expect_true(all(schema_conditions %in% names(p$conditional_metadata)))
})

test_that("IPHRAProtocol syncs sampling conditional_metadata from sample_table", {
  p <- IPHRAProtocol$new()

  p$add_stratum(
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "simple_random",
    n_sites = 2
  )

  expect_true(isTRUE(p$conditional_metadata$srs_srs))
  expect_false(isTRUE(p$conditional_metadata$purposive))

  p$add_stratum(
    stratum_id = "s1",
    stratum_name = "S1 Updated",
    sampling_method = "purposive"
  )

  expect_false(isTRUE(p$conditional_metadata$srs_srs))
  expect_true(isTRUE(p$conditional_metadata$purposive))
})

test_that("IPHRAProtocol conditional handling uses active bindings and runs empty conditions", {
  p <- IPHRAProtocol$new()
  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "simple_random"
  )

  rows <- data.frame(
    tag_name = c("@tag_true", "@tag_false", "@tag_empty"),
    handling = rep("conditional_replace", 3),
    condition = c("srs_srs", "srs_systematic", ""),
    default_value = c("YES", "NO", "EMPTY"),
   function_name = c("", "", ""),
   stringsAsFactors = FALSE
  )

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "@tag_true", style = "Normal")
  doc <- officer::body_add_par(doc, "@tag_false", style = "Normal")
  doc <- officer::body_add_par(doc, "@tag_empty", style = "Normal")
  doc <- p$.__enclos_env__$private$..apply_protocol_schema_sections(doc, rows)

  body_xml <- officer::docx_body_xml(doc)
  txt <- paste(xml2::xml_text(xml2::xml_find_all(body_xml, ".//w:t", xml2::xml_ns(body_xml))),
              collapse = "")

  expect_true(grepl("YES", txt, fixed = TRUE))
  expect_true(grepl("@tag_false", txt, fixed = TRUE))
  expect_true(grepl("EMPTY", txt, fixed = TRUE))
  expect_false(grepl("@tag_empty", txt, fixed = TRUE))
})

test_that("IPHRAProtocol active bindings detect requested tool states", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.kii_community))
  expect_false(isTRUE(p$.kii_service_providers))

  p$add_tools("tool_kii_community_iphra_v2")
  p$add_tools("tool_kii_fsl_service_provider_iphra_v2")
  expect_true(isTRUE(p$.kii_community))
  expect_true(isTRUE(p$.kii_fsl_provider))
  expect_true(isTRUE(p$.kii_service_providers))
})

test_that("IPHRAProtocol ind_ecfies and rlc_household_selection bindings evaluate from nested state", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.ind_ecfies))
  expect_false(isTRUE(p$.rlc_household_selection))

  p$add_tools("tool_household_iphra_v2")
  p$set_nested(
    field = "tools",
    role = "household",
    member = "revised_survey",
    value = data.frame(indicator_code = c("10801"), stringsAsFactors = FALSE)
  )
  expect_true(isTRUE(p$.ind_ecfies))

  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "pps_rlc",
    n_sites = 1
  )
  expect_true(isTRUE(p$.rlc_household_selection))
})

test_that("IPHRAProtocol sampling active bindings evaluate from nested sample object", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.cluster_site_selection))
  expect_false(isTRUE(p$.exhaustive_site_selection))
  expect_false(isTRUE(p$.purposive_site_selection))
  expect_false(isTRUE(p$.multiple_methods_yes))
  expect_false(isTRUE(p$.multiple_strata))

  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s1",
    stratum_name = "S1",
    sampling_method = "proportional"
  )
  expect_true(isTRUE(p$.exhaustive_site_selection))
  expect_true(isTRUE(p$.multiple_methods_no))
  expect_false(isTRUE(p$.multiple_methods_yes))
  expect_false(isTRUE(p$.multiple_strata))

  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s2",
    stratum_name = "S2",
    sampling_method = "pps_cluster"
  )
  expect_true(isTRUE(p$.cluster_site_selection))
  expect_true(isTRUE(p$.multiple_methods_yes))
  expect_false(isTRUE(p$.multiple_methods_no))
  expect_true(isTRUE(p$.multiple_strata))

  p$access_nested(
    field = "sample_object",
    member = "add_stratum",
    stratum_id = "s3",
    stratum_name = "S3",
    sampling_method = "purposive"
  )
  expect_true(isTRUE(p$.purposive_site_selection))
})

test_that("IPHRAProtocol indicator and observation active bindings evaluate from nested tools", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.mortality_survey))
  expect_false(isTRUE(p$.muac_survey))
  expect_false(isTRUE(p$.obs_community))
  expect_false(isTRUE(p$.obs_crops_livestock))
  expect_false(isTRUE(p$.obs_health_facility))
  expect_false(isTRUE(p$.obs_latrines))
  expect_false(isTRUE(p$.obs_water_point))

  p$add_tools("tool_household_iphra_v2")
  p$set_nested(
    field = "tools",
    role = "household",
    member = "revised_survey",
    value = data.frame(indicator_code = c("10501", "10702"), stringsAsFactors = FALSE)
  )
  expect_true(isTRUE(p$.mortality_survey))
  expect_true(isTRUE(p$.muac_survey))

  p$add_tools("tool_obs_community_iphra_v2")
  p$add_tools("tool_obs_crop_livestock_iphra_v1")
  p$add_tools("tool_obs_health_facility_iphra_v2")
  p$add_tools("tool_obs_latrine_iphra_v2")
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_true(isTRUE(p$.obs_community))
  expect_true(isTRUE(p$.obs_crops_livestock))
  expect_true(isTRUE(p$.obs_health_facility))
  expect_true(isTRUE(p$.obs_latrines))
  expect_true(isTRUE(p$.obs_water_point))
})

test_that("Protocol objects inherit cached document field from Document base class", {
  p <- Protocol$new()
  expect_true(inherits(p$document, "rdocx"))
})
