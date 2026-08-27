test_that("Framework initializes with NULL fields", {
  suppressMessages(fw <- Framework$new())
  expect_null(fw$master_objectives_schema)
  expect_null(fw$master_indicator_bank)
  expect_null(fw$modified_objectives_schema)
  expect_null(fw$modified_indicator_bank)
  expect_null(fw$master_svg)
  expect_null(fw$adjusted_svg)
})

test_that("Framework$set_master_schema validates and stores the schema", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = "10001",
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute Illness",
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  expect_equal(nrow(fw$master_objectives_schema), 2)
  expect_equal(fw$master_objectives_schema$short_objective, c("H1", "H2"))
})

test_that("Framework$set_master_schema rejects invalid schema", {
  suppressMessages(fw <- Framework$new())
  bad <- data.frame(x = 1:3)
  expect_error(fw$set_master_schema(bad))
})

test_that("Framework$set_master_svg stores the SVG content", {
  suppressMessages(fw <- Framework$new())
  svg <- '<svg><rect id="H1"/></svg>'
  suppressMessages(fw$set_master_svg(svg))
  expect_equal(fw$master_svg, svg)
})

test_that("Framework$set_master_svg rejects non-character input", {
  suppressMessages(fw <- Framework$new())
  expect_error(fw$set_master_svg(123))
})

test_that("Framework$modify_adjusted_schema filters correctly", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001", "10002", "10003"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema(c("10001", "10003")))
  expect_equal(nrow(fw$modified_objectives_schema), 2)
  expect_setequal(fw$modified_objectives_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$modify_adjusted_schema with NULL resets to full schema", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001", "10002"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema(NULL))
  expect_equal(nrow(fw$modified_objectives_schema), 2)
})

test_that("Framework$modify_adjusted_schema errors without master_schema", {
  suppressMessages(fw <- Framework$new())
  expect_error(fw$modify_adjusted_schema(c("H1")))
})

test_that("Framework$modify_adjusted_schema filters by objective codes", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001", "10002", "10003"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema(c("10001", "10003")))
  expect_equal(nrow(fw$modified_objectives_schema), 2)
  expect_setequal(fw$modified_objectives_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$modify_adjusted_schema with NULL uses all objective codes", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001", "10002", "10003"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema())
  expect_equal(nrow(fw$modified_objectives_schema), 3)
  expect_setequal(
    fw$modified_objectives_schema$short_objective,
    c("H1", "H2", "H3")
  )
})

test_that("Framework$modify_adjusted_schema accepts a list of codes", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001", "10002", "10003"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema(list("10002", "10003")))
  expect_equal(nrow(fw$modified_objectives_schema), 2)
  expect_setequal(fw$modified_objectives_schema$short_objective, c("H2", "H3"))
})

test_that("Framework$modify_adjusted_schema errors without master_schema", {
  suppressMessages(fw <- Framework$new())
  expect_error(fw$modify_adjusted_schema(c("H1")))
})

test_that("Framework$modify_adjusted_svg runs without error when primary objectives set", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  svg <- '<svg><g id="H1"><rect fill="white"/></g><g id="H2"><rect fill="white"/></g></svg>'
  suppressMessages(fw$set_master_svg(svg))
  fw$primary_objectives <- c(1)
  suppressMessages(fw$modify_adjusted_svg())

  expect_false(is.null(fw$adjusted_svg))
})

test_that("Protocol framework data is serialisable", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  p$framework <- fw
  expect_true(inherits(p$framework, "Framework"))
  expect_equal(nrow(p$framework$master_objectives_schema), 1)
})

test_that("ANAFramework initializes with master_objectives_schema from reference_objectives.xlsx", {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  suppressMessages(af <- ANAFramework$new())
  expect_true(is.data.frame(af$master_objectives_schema))
  expect_gt(nrow(af$master_objectives_schema), 0)
})

test_that("ANAFramework initializes with master_indicator_bank from reference_indicator_bank.xlsx", {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  suppressMessages(af <- ANAFramework$new())
  expect_true(is.data.frame(af$master_indicator_bank))
  expect_gt(nrow(af$master_indicator_bank), 0)
})

test_that("ANAFramework initializes modified_indicator_bank equal to master_indicator_bank", {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  suppressMessages(af <- ANAFramework$new())
  expect_equal(nrow(af$modified_indicator_bank), nrow(af$master_indicator_bank))
})

test_that("ANAFramework inherits Framework methods", {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  suppressMessages(af <- ANAFramework$new())
  expect_true(inherits(af, "Framework"))
})

test_that("create_framework returns a Framework object", {
  suppressMessages(fw <- create_framework())
  expect_true(inherits(fw, "Framework"))
})

test_that("Protocol$new() with framework_type='ana' creates an ANAFramework", {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "reference_objectives.xlsx or reference_indicator_bank.xlsx not available"
  )
  suppressMessages(p <- Protocol$new(framework_type = "ana"))
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("restore_framework reconstructs a Framework from exported data", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$set_master_svg('<svg><rect id="H1"/></svg>'))
  fw_data <- list(
    class = class(fw)[1],
    master_objectives_schema = fw$master_objectives_schema,
    modified_objectives_schema = fw$modified_objectives_schema,
    master_svg = fw$master_svg,
    adjusted_svg = fw$adjusted_svg,
    primary_objectives = fw$primary_objectives,
    secondary_objectives = fw$secondary_objectives
  )
  suppressMessages(restored <- restore_framework(fw_data))
  expect_true(inherits(restored, "Framework"))
  expect_equal(nrow(restored$master_objectives_schema), 1)
  expect_false(is.null(restored$master_svg))
})

test_that("Protocol initializes with a Framework object by default (framework_type='none')", {
  suppressMessages(p <- Protocol$new())
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol$new() rejects invalid framework_type", {
  expect_error(Protocol$new(framework_type = "bogus"))
})

test_that("Protocol framework field can be replaced with another Framework object", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(fw <- Framework$new())
  p$framework <- fw
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol includes framework when set", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  p$framework <- fw
  expect_false(is.null(p$framework))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol initializes with a default framework", {
  suppressMessages(p <- Protocol$new())
  expect_false(is.null(p$framework))
  expect_true(inherits(p$framework, "Framework"))
})

# ---- ANAFramework additional method tests ----

.skip_if_no_ana_resources <- function() {
  skip_if_not(
    (file.exists(system.file(
      "resources",
      "reference_objectives.xlsx",
      package = "phr"
    )) ||
      file.exists(file.path("resources", "reference_objectives.xlsx"))) &&
      (file.exists(system.file(
        "resources",
        "reference_indicator_bank.xlsx",
        package = "phr"
      )) ||
        file.exists(file.path("resources", "reference_indicator_bank.xlsx"))),
    "ANA resources not available"
  )
}

test_that("ANAFramework auto-loads master_svg from ana_framework.svg", {
  .skip_if_no_ana_resources()
  suppressMessages(af <- ANAFramework$new())
  expect_false(is.null(af$master_svg))
  expect_true(nzchar(af$master_svg))
  expect_true(grepl("<svg", af$master_svg))
})

test_that("ANAFramework inherits modify_adjusted_schema from Framework and filters by objective_code", {
  .skip_if_no_ana_resources()
  suppressMessages(af <- ANAFramework$new())
  skip_if(is.null(af$master_objectives_schema))
  skip_if(!"objective_code" %in% names(af$master_objectives_schema))

  available_codes <- unique(af$master_objectives_schema$objective_code)
  skip_if(length(available_codes) == 0)

  code <- available_codes[[1]]
  suppressMessages(af$modify_adjusted_schema(code))
  expect_true(is.data.frame(af$modified_objectives_schema))
  expect_true(nrow(af$modified_objectives_schema) > 0)
  expect_true(all(af$modified_objectives_schema$objective_code == code))
})

test_that("ANAFramework inherited modify_adjusted_schema errors without master_objectives_schema", {
  suppressMessages(af <- ANAFramework$new())
  af$master_objectives_schema <- NULL
  expect_error(af$modify_adjusted_schema(101))
})

test_that("ANAFramework$modify_adjusted_svg colours correct sub_pillar blocks", {
  .skip_if_no_ana_resources()
  suppressMessages(af <- ANAFramework$new())
  skip_if(is.null(af$master_objectives_schema) || is.null(af$master_svg))
  skip_if(!"objective_code" %in% names(af$master_objectives_schema))

  available_codes <- unique(af$master_objectives_schema$objective_code)
  available_codes <- available_codes[!is.na(available_codes)]
  skip_if(length(available_codes) == 0)

  af$primary_objectives <- available_codes[1]
  suppressMessages(af$modify_adjusted_svg())

  expect_false(is.null(af$adjusted_svg))
  expect_true(nzchar(af$adjusted_svg))
})

test_that("ANAFramework$modify_adjusted_svg warns when master_svg is NULL", {
  suppressMessages(af <- ANAFramework$new())
  af$master_svg <- NULL
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "HealthStatus",
    short_objective = "H1",
    text_objective = "Obj 1",
    objective_code = 1L,
    stringsAsFactors = FALSE
  )
  af$master_objectives_schema <- schema
  af$modified_objectives_schema <- schema
  expect_warning(af$modify_adjusted_svg())
})

# ---- Framework$modify_indicator_bank tests ----

test_that("Framework$modify_indicator_bank filters by objective_code", {
  suppressMessages(fw <- Framework$new())
  bank <- data.frame(
    objective_code = c(1L, 1L, 2L, 3L),
    indicator_code = c("I1", "I2", "I3", "I4"),
    stringsAsFactors = FALSE
  )
  fw$master_indicator_bank <- bank
  fw$modified_indicator_bank <- bank
  suppressMessages(fw$modify_indicator_bank(c(1, 2)))
  expect_equal(nrow(fw$modified_indicator_bank), 3)
  expect_setequal(fw$modified_indicator_bank$objective_code, c(1L, 1L, 2L))
})

test_that("Framework$modify_indicator_bank with NULL resets to full master", {
  suppressMessages(fw <- Framework$new())
  bank <- data.frame(
    objective_code = c(1L, 2L, 3L),
    indicator_code = c("I1", "I2", "I3"),
    stringsAsFactors = FALSE
  )
  fw$master_indicator_bank <- bank
  fw$modified_indicator_bank <- bank[1, , drop = FALSE]
  suppressMessages(fw$modify_indicator_bank(NULL))
  expect_equal(nrow(fw$modified_indicator_bank), 3)
})

test_that("Framework$modify_indicator_bank warns when master_indicator_bank is NULL", {
  suppressMessages(fw <- Framework$new())
  expect_warning(fw$modify_indicator_bank(c(1, 2)))
})

test_that("Framework$modify_indicator_bank warns when no objective_code column present", {
  suppressMessages(fw <- Framework$new())
  bank <- data.frame(indicator_code = c("I1", "I2"), stringsAsFactors = FALSE)
  fw$master_indicator_bank <- bank
  expect_warning(fw$modify_indicator_bank(c(1)))
})

# ---- Protocol$add_tools / validate_objective_schema tests ----

test_that("Protocol$add_tools stores tools by name for $ access", {
  suppressMessages(p <- Protocol$new())
  p$add_tools("household")
  expect_true("household" %in% names(p$tools))
  expect_true(inherits(p$tools[["household"]], "HouseholdTool"))
})

test_that("Protocol$add_tools uses tool_name when provided", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools("household", tool_name = "my_hh_tool"))
  expect_true("my_hh_tool" %in% names(p$tools))
})

test_that("Protocol$add_tools auto-increments duplicate tool types", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools("household"))
  suppressMessages(p$add_tools("household"))
  nms <- names(p$tools)
  expect_true("household" %in% nms)
  expect_true(any(grepl("^household_", nms)))
})

test_that("Protocol$validate_objective_schema works as a method", {
  suppressMessages(p <- Protocol$new())
  good <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "P1",
    sub_pillar = "SP1",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  expect_true(p$validate_objective_schema(good))
})

test_that("Protocol$validate_objective_schema errors on bad schema via method", {
  suppressMessages(p <- Protocol$new())
  bad <- data.frame(x = 1:3)
  expect_error(p$validate_objective_schema(bad))
})

# ---- Tool public fields tests ----

test_that("Tool$survey is a public field accessible directly", {
  suppressMessages(t <- Tool$new())
  expect_true(is.data.frame(t$survey))
  new_survey <- data.frame(
    type = "text",
    name = "q1",
    label = "Question 1",
    stringsAsFactors = FALSE
  )
  t$survey <- new_survey
  expect_equal(t$survey$name, "q1")
})

test_that("Tool$choices is a public field", {
  suppressMessages(t <- Tool$new())
  expect_true(is.data.frame(t$choices))
})

test_that("Tool$settings is a public field", {
  suppressMessages(t <- Tool$new())
  expect_true(is.data.frame(t$settings))
})

test_that("Tool$touch updates metadata$modified_datetime", {
  suppressMessages(t <- Tool$new())
  before <- t$metadata$modified_datetime
  Sys.sleep(0.01)
  t$touch()
  expect_true(t$metadata$modified_datetime >= before)
})

test_that("Tool mutating methods update metadata$modified_datetime", {
  suppressMessages(
    t <- Tool$new(
      survey = data.frame(
        type = "text",
        name = "q1",
        indicator_code = "10001",
        stringsAsFactors = FALSE
      ),
      choices = data.frame(
        list_name = "yes_no",
        name = "yes",
        label = "Yes",
        indicator_code = "10001",
        stringsAsFactors = FALSE
      )
    )
  )
  before <- t$metadata$modified_datetime
  Sys.sleep(0.01)
  suppressMessages(t$set_selected_indicators("ind_1"))
  expect_true(t$metadata$modified_datetime >= before)
})

test_that("Protocol generalized nested accessor targets a specific tool", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools("generic", tool_name = "my_tool"))

  expect_equal(p$access_nested("tools", "my_tool", "get_name"), "my_tool")
  expect_equal(p$access_nested("tools", "my_tool", "get_tool_type"), "generic")

  suppressMessages(p$access_nested(
    field = "tools",
    name = "my_tool",
    member = "set_name",
    new_name = "renamed_tool"
  ))
  expect_equal(p$access_nested("tools", "my_tool", "get_name"), "renamed_tool")

  expect_silent(p$access_nested(
    field = "tools",
    name = "my_tool",
    member = "change_default_language",
    language = "english"
  ))
  expect_equal(
    p$access_nested("tools", "my_tool", "survey"),
    p$tools[["my_tool"]]$survey
  )
  expect_equal(
    p$access_nested("tools", "my_tool", "choices"),
    p$tools[["my_tool"]]$choices
  )
  expect_true(is.integer(p$access_nested(
    "tools",
    "my_tool",
    "get_indicator_codes",
    prefer_revised = FALSE
  )))
  expect_equal(
    p$access_nested("tools", "my_tool", "get_selected_indicators"),
    character(0)
  )

  p$access_nested(
    "tools",
    "my_tool",
    "set_selected_indicators",
    indicators = c("a", "b")
  )
  expect_equal(
    p$access_nested("tools", "my_tool", "get_selected_indicators"),
    c("a", "b")
  )

  p$access_nested(
    "tools",
    "my_tool",
    "update_settings",
    key = "form_title",
    value = "Test Form"
  )
  expect_equal(p$tools[["my_tool"]]$settings$form_title[1], "Test Form")

  new_choices <- data.frame(
    name = c("a", "b"),
    label = c("A", "B"),
    stringsAsFactors = FALSE
  )
  p$access_nested(
    "tools",
    "my_tool",
    "update_choice_list",
    list_name = "my_list",
    new_choices = new_choices
  )
  expect_true(any(p$tools[["my_tool"]]$revised_choices$list_name == "my_list"))

  expect_silent(p$access_nested(
    "tools",
    "my_tool",
    "filter_survey_by_indicator",
    indicator_codes = "10000"
  ))
  expect_true(is.data.frame(p$access_nested(
    "tools",
    "my_tool",
    "revised_survey"
  )))
})

test_that("Protocol nested accessor touches protocol modified_datetime", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools("generic", tool_name = "my_tool"))
  before <- p$metadata$modified_datetime
  Sys.sleep(0.01)
  p$access_nested("tools", "my_tool", "get_name")
  expect_true(p$metadata$modified_datetime >= before)
})

test_that("Protocol nested accessor tool validation calls return logical/list outputs", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools("generic", tool_name = "my_tool"))
  expect_type(p$access_nested("tools", "my_tool", "validate"), "logical")
  expect_type(p$access_nested("tools", "my_tool", "is_valid"), "logical")
  expect_true(is.list(p$access_nested(
    "tools",
    "my_tool",
    "get_validation_errors"
  )))
})

test_that("Protocol nested accessor supports role-based lookup for list fields", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(p$add_tools(
    "household",
    tool_name = "tool_household_iphra_v2"
  ))

  expect_equal(
    p$access_nested(field = "tools", role = "household", member = "get_name"),
    "tool_household_iphra_v2"
  )
})

test_that("Framework$render_framework_svg errors when no SVG is set", {
  suppressMessages(fw <- Framework$new())
  expect_error(fw$render_framework_svg())
})

test_that("Framework$render_framework_svg accepts version='master' and version='adjusted'", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$set_master_svg('<svg><rect id="H1"/></svg>'))
  # Both version values should run without error (rsvg/grid may not be available,
  # in which case a warning is issued and the temp file path is returned).
  suppressMessages(expect_no_error(fw$render_framework_svg(version = "master")))
  suppressMessages(expect_no_error(fw$render_framework_svg(
    version = "adjusted"
  )))
})

test_that("Framework$render_framework_svg errors on invalid version argument", {
  suppressMessages(fw <- Framework$new())
  suppressMessages(fw$set_master_svg('<svg><rect id="H1"/></svg>'))
  expect_error(fw$render_framework_svg(version = "bad_version"))
})

test_that("Framework$render_framework_svg version='master' uses master_svg even when adjusted_svg is set", {
  skip_if_not(
    requireNamespace("rsvg", quietly = TRUE) &&
      requireNamespace("grid", quietly = TRUE),
    "rsvg/grid not available"
  )
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = "SP",
    short_objective = c("H1", "H2"),
    text_objective = c("O1", "O2"),
    objective_code = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$set_master_svg(
    '<svg><g id="H1"><rect fill="white"/></g><g id="H2"><rect fill="white"/></g></svg>'
  ))
  fw$primary_objectives <- c(1)
  suppressMessages(fw$modify_adjusted_svg())
  expect_false(is.null(fw$adjusted_svg))
})

# ---- New field and modify_adjusted_svg tests ----

test_that("Framework initializes primary_objectives and secondary_objectives as NULL", {
  suppressMessages(fw <- Framework$new())
  expect_null(fw$primary_objectives)
  expect_null(fw$secondary_objectives)
})

test_that("Framework primary_objectives and secondary_objectives can be set", {
  suppressMessages(fw <- Framework$new())
  fw$primary_objectives <- c(101, 102)
  fw$secondary_objectives <- c(103, 104)
  expect_equal(fw$primary_objectives, c(101, 102))
  expect_equal(fw$secondary_objectives, c(103, 104))
})

test_that("Protocol framework includes primary and secondary objectives", {
  suppressMessages(p <- Protocol$new())
  suppressMessages(fw <- Framework$new())
  fw$primary_objectives <- c(101)
  fw$secondary_objectives <- c(102)
  p$framework <- fw
  expect_equal(p$framework$primary_objectives, c(101))
  expect_equal(p$framework$secondary_objectives, c(102))
})

test_that("restore_framework restores primary_objectives and secondary_objectives", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    objective_code = c("10001"),
    sector = "Health",
    pillar = "P",
    sub_pillar = "SP",
    short_objective = "H1",
    text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  fw$primary_objectives <- c(101, 102)
  fw$secondary_objectives <- c(103)
  fw_data <- list(
    class = class(fw)[1],
    master_objectives_schema = fw$master_objectives_schema,
    modified_objectives_schema = fw$modified_objectives_schema,
    master_svg = fw$master_svg,
    adjusted_svg = fw$adjusted_svg,
    primary_objectives = fw$primary_objectives,
    secondary_objectives = fw$secondary_objectives
  )
  suppressMessages(restored <- restore_framework(fw_data))
  expect_equal(restored$primary_objectives, c(101, 102))
  expect_equal(restored$secondary_objectives, c(103))
})

test_that("Framework$modify_adjusted_svg warns when master_svg is NULL", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = "SP1",
    short_objective = "H1",
    text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_objectives_schema <- schema
  expect_warning(fw$modify_adjusted_svg())
})

test_that("Framework$modify_adjusted_svg warns when master_objectives_schema is NULL", {
  suppressMessages(fw <- Framework$new())
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  expect_warning(fw$modify_adjusted_svg())
})

test_that("Framework$modify_adjusted_svg colours primary sub-pillars light green", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = c("SP1", "SP2"),
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    stringsAsFactors = FALSE
  )
  fw$master_objectives_schema <- schema
  fw$master_svg <- paste0(
    '<svg>',
    '<g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g>',
    '<g id="SP2"><rect x="20" y="0" width="10" height="10" fill="white"/></g>',
    '</svg>'
  )
  fw$primary_objectives <- c(101)
  suppressMessages(fw$modify_adjusted_svg())
  expect_true(grepl('#90EE90', fw$adjusted_svg))
  # SP1 should not be coloured as secondary or both
  expect_false(grepl('#ADD8E6', fw$adjusted_svg))
  expect_false(grepl('#DDA0DD', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg colours secondary sub-pillars light blue", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = "SP1",
    short_objective = "H1",
    text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_objectives_schema <- schema
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  fw$secondary_objectives <- c(101)
  suppressMessages(fw$modify_adjusted_svg())
  expect_true(grepl('#ADD8E6', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg colours both sub-pillars light purple", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = "SP1",
    short_objective = "H1",
    text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_objectives_schema <- schema
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  fw$primary_objectives <- c(101)
  fw$secondary_objectives <- c(101)
  suppressMessages(fw$modify_adjusted_svg())
  expect_true(grepl('#DDA0DD', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg leaves unselected sub-pillars white", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "P",
    sub_pillar = c("SP1", "SP2"),
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    stringsAsFactors = FALSE
  )
  fw$master_objectives_schema <- schema
  fw$master_svg <- paste0(
    '<svg>',
    '<g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g>',
    '<g id="SP2"><rect x="20" y="0" width="10" height="10" fill="white"/></g>',
    '</svg>'
  )
  fw$primary_objectives <- c(101)
  suppressMessages(fw$modify_adjusted_svg())
  # SP2 (code 102) is not in primary or secondary, should not have a special colour
  expect_false(
    grepl('#90EE90', fw$adjusted_svg) && grepl('#ADD8E6', fw$adjusted_svg)
  )
  expect_false(grepl('#DDA0DD', fw$adjusted_svg))
  # The adjusted SVG must still contain SP2
  expect_true(grepl('id="SP2"', fw$adjusted_svg))
})

test_that("ANAFramework initializes adjusted_svg to match master_svg", {
  .skip_if_no_ana_resources()
  suppressMessages(af <- ANAFramework$new())
  skip_if(is.null(af$master_svg))
  expect_equal(af$adjusted_svg, af$master_svg)
})

test_that("ANAFramework$modify_adjusted_svg uses objective codes to colour sub-pillars", {
  .skip_if_no_ana_resources()
  suppressMessages(af <- ANAFramework$new())
  skip_if(is.null(af$master_objectives_schema) || is.null(af$master_svg))
  skip_if(!"objective_code" %in% names(af$master_objectives_schema))

  available_codes <- unique(af$master_objectives_schema$objective_code)
  available_codes <- available_codes[!is.na(available_codes)]
  skip_if(length(available_codes) < 2)

  af$primary_objectives <- available_codes[1]
  af$secondary_objectives <- available_codes[2]
  suppressMessages(af$modify_adjusted_svg())

  expect_false(is.null(af$adjusted_svg))
  expect_true(nzchar(af$adjusted_svg))
  # At least one coloured block should appear
  has_colour <- grepl("#90EE90", af$adjusted_svg) ||
    grepl("#ADD8E6", af$adjusted_svg) ||
    grepl("#DDA0DD", af$adjusted_svg)
  expect_true(has_colour)
})

# ---- New default-objective behaviour for modify_adjusted_schema ----

test_that("Framework$modify_adjusted_schema(NULL) uses primary_objectives when set", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    objective_code = c(101L, 102L, 103L),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  fw$primary_objectives <- c(101L, 103L)
  suppressMessages(fw$modify_adjusted_schema())
  expect_equal(nrow(fw$modified_objectives_schema), 2)
  expect_setequal(fw$modified_objectives_schema$objective_code, c(101L, 103L))
})

test_that("Framework$modify_adjusted_schema(NULL) uses secondary_objectives when set", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    objective_code = c(101L, 102L, 103L),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  fw$secondary_objectives <- c(102L)
  suppressMessages(fw$modify_adjusted_schema())
  expect_equal(nrow(fw$modified_objectives_schema), 1)
  expect_equal(fw$modified_objectives_schema$objective_code, 102L)
})

test_that("Framework$modify_adjusted_schema(NULL) combines primary and secondary objectives", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective = c("Obj 1", "Obj 2", "Obj 3"),
    objective_code = c(101L, 102L, 103L),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$primary_objectives <- c(101L)
  fw$secondary_objectives <- c(103L)
  fw$modify_adjusted_schema()
  expect_equal(nrow(fw$modified_objectives_schema), 2)
  expect_setequal(fw$modified_objectives_schema$objective_code, c(101L, 103L))
})

test_that("Framework$modify_adjusted_schema(NULL) falls back to all rows when no objectives set", {
  suppressMessages(fw <- Framework$new())
  schema <- data.frame(
    sector = "Health",
    pillar = "Morbidity",
    sub_pillar = "Acute",
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    stringsAsFactors = FALSE
  )
  suppressMessages(fw$set_master_schema(schema))
  suppressMessages(fw$modify_adjusted_schema())
  expect_equal(nrow(fw$modified_objectives_schema), 2)
})

test_that("Framework set_primary_indicators and set_secondary_indicators accept vectors/lists", {
  suppressMessages(fw <- Framework$new())
  suppressMessages(fw$set_primary_indicators(c("1001", "1002")))
  suppressMessages(fw$set_secondary_indicators(list("2001", "2002")))

  expect_equal(fw$primary_indicator_codes, c("1001", "1002"))
  expect_equal(fw$secondary_indicator_codes, c("2001", "2002"))
})

test_that("Framework builds modified primary/secondary indicator caches from adjusted schema", {
  suppressMessages(fw <- Framework$new())
  suppressMessages(fw$set_master_schema(data.frame(
    sector = "Health",
    pillar = "P1",
    sub_pillar = "SP1",
    short_objective = c("H1", "H2"),
    text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    indicator_code = c("1001", "2001"),
    stringsAsFactors = FALSE
  )))
  suppressMessages(fw$set_primary_objectives(101L))
  suppressMessages(fw$set_secondary_objectives(102L))
  suppressMessages(fw$modify_adjusted_schema(c(101L, 102L)))

  expect_true(is.data.frame(fw$modified_primary_indicator_codes))
  expect_true(is.data.frame(fw$modified_secondary_indicator_codes))
  expect_equal(fw$modified_primary_indicator_codes$indicator_code, "1001")
  expect_equal(fw$modified_secondary_indicator_codes$indicator_code, "2001")
})

# ---- Protocol renamed catalog fields ----

test_that("Protocol initializes new framework_* catalog fields as empty lists", {
  suppressMessages(p <- Protocol$new())
  expect_true(is.null(p$framework$modified_indicator_bank))
  expect_true(is.null(p$framework$master_indicator_bank))
  expect_true(is.null(p$master_objectives_schema))
  expect_true(is.null(p$framework$modified_objectives_schema))
})

test_that("Protocol no longer has objectives, objective_schema, or selected_indicators fields", {
  suppressMessages(p <- Protocol$new())
  expect_false("objectives" %in% names(p))
  expect_false("objective_schema" %in% names(p))
  expect_false("selected_indicators" %in% names(p))
})
