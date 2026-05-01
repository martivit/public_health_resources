test_that("Framework initializes with NULL fields", {
  fw <- Framework$new()
  expect_null(fw$master_schema)
  expect_null(fw$adjusted_schema)
  expect_null(fw$master_svg)
  expect_null(fw$adjusted_svg)
})

test_that("Framework$set_master_schema validates and stores the schema", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute Illness",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  expect_equal(nrow(fw$master_schema), 2)
  expect_equal(fw$master_schema$short_objective, c("H1", "H2"))
})

test_that("Framework$set_master_schema rejects invalid schema", {
  fw <- Framework$new()
  bad <- data.frame(x = 1:3)
  expect_error(fw$set_master_schema(bad))
})

test_that("Framework$set_master_svg stores the SVG content", {
  fw <- Framework$new()
  svg <- '<svg><rect id="H1"/></svg>'
  fw$set_master_svg(svg)
  expect_equal(fw$master_svg, svg)
})

test_that("Framework$set_master_svg rejects non-character input", {
  fw <- Framework$new()
  expect_error(fw$set_master_svg(123))
})

test_that("Framework$update_adjusted_schema filters correctly", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$update_adjusted_schema(c("H1", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$update_adjusted_schema with NULL resets to full schema", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$update_adjusted_schema(NULL)
  expect_equal(nrow(fw$adjusted_schema), 2)
})

test_that("Framework$update_adjusted_schema errors without master_schema", {
  fw <- Framework$new()
  expect_error(fw$update_adjusted_schema(c("H1")))
})

test_that("Framework$create_adjusted_schema filters by objective codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$create_adjusted_schema(c("H1", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$create_adjusted_schema with NULL uses all objective codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$create_adjusted_schema()
  expect_equal(nrow(fw$adjusted_schema), 3)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H2", "H3"))
})

test_that("Framework$create_adjusted_schema accepts a list of codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$create_adjusted_schema(list("H2", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H2", "H3"))
})

test_that("Framework$create_adjusted_schema errors without master_schema", {
  fw <- Framework$new()
  expect_error(fw$create_adjusted_schema(c("H1")))
})

test_that("Framework$update_adjusted_svg hides unselected elements", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  svg <- '<svg><rect id="H1"/><rect id="H2"/></svg>'
  fw$set_master_svg(svg)
  fw$update_adjusted_schema(c("H1"))
  fw$update_adjusted_svg()

  expect_false(is.null(fw$adjusted_svg))
  # H2 should be hidden
  expect_true(grepl('visibility:hidden', fw$adjusted_svg))
  # H1 should not be hidden
  expect_false(grepl('id="H1"[^>]*visibility:hidden', fw$adjusted_svg))
})

test_that("Framework$update_adjusted_svg warns when master_svg is NULL", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$update_adjusted_schema(c("H1"))
  expect_warning(fw$update_adjusted_svg())
})

test_that("Framework$export_framework returns a serialisable list", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  exported <- fw$export_framework()
  expect_true(is.list(exported))
  expect_equal(exported$class, "Framework")
  expect_equal(nrow(exported$master_schema), 1)
})

test_that("ANAFramework initializes with master_schema from reference.xlsx", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  af <- ANAFramework$new()
  expect_true(is.data.frame(af$master_schema))
  expect_gt(nrow(af$master_schema), 0)
})

test_that("ANAFramework inherits Framework methods", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  af <- ANAFramework$new()
  expect_true(inherits(af, "Framework"))
  # update_adjusted_schema should work
  if (!is.null(af$master_schema) && nrow(af$master_schema) > 0) {
    first_obj <- af$master_schema$short_objective[[1]]
    af$update_adjusted_schema(first_obj)
    expect_equal(nrow(af$adjusted_schema), 1)
  }
})

test_that("create_framework returns a Framework object", {
  fw <- create_framework()
  expect_true(inherits(fw, "Framework"))
})

test_that("Protocol$new() with framework_type='ana' creates an ANAFramework", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- Protocol$new(framework_type = "ana")
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("restore_framework reconstructs a Framework from exported data", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  exported <- fw$export_framework()
  restored <- restore_framework(exported)
  expect_true(inherits(restored, "Framework"))
  expect_equal(nrow(restored$master_schema), 1)
  expect_false(is.null(restored$master_svg))
})

test_that("Protocol initializes with a Framework object by default (framework_type='none')", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol$new() rejects invalid framework_type", {
  expect_error(Protocol$new(framework_type = "bogus"))
})

test_that("Protocol framework field can be replaced with another Framework object", {
  p   <- Protocol$new()
  fw  <- Framework$new()
  p$framework <- fw
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol$export_protocol includes framework when set", {
  p  <- Protocol$new()
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  p$framework <- fw
  exported <- p$export_protocol()
  expect_false(is.null(exported$framework))
  expect_equal(exported$framework$class, "Framework")
})

test_that("Protocol$export_protocol includes default framework", {
  p        <- Protocol$new()
  exported <- p$export_protocol()
  expect_false(is.null(exported$framework))
})

test_that("restore_protocol restores framework field", {
  p  <- Protocol$new(assessment_title = "Test", country_name = "TestLand")
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  p$framework <- fw
  exported  <- p$export_protocol()
  restored  <- restore_protocol(exported)
  expect_true(inherits(restored$framework, "Framework"))
  expect_equal(nrow(restored$framework$master_schema), 1)
})

# ---- ANAFramework additional method tests ----

.skip_if_no_ana_resources <- function() {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "ANA resources not available"
  )
}

test_that("ANAFramework auto-loads master_svg from ana_framework.svg", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  expect_false(is.null(af$master_svg))
  expect_true(nzchar(af$master_svg))
  expect_true(grepl("<svg", af$master_svg))
})

test_that("ANAFramework$filter_schema_by_pillar returns subset", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_schema))

  available_pillars <- unique(af$master_schema$pillar)
  skip_if(length(available_pillars) == 0)

  pillar <- available_pillars[[1]]
  result <- af$filter_schema_by_pillar(pillar)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(all(result$pillar == pillar))
})

test_that("ANAFramework$filter_schema_by_pillar errors without master_schema", {

test_that("ANAFramework$update_adjusted_svg highlights correct sub_pillar blocks", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_schema) || is.null(af$master_svg))

  # Pick the first sub_pillar that appears in the SVG
  all_blocks <- unique(af$master_schema$sub_pillar)
  all_blocks  <- all_blocks[!is.na(all_blocks) & nzchar(all_blocks)]
  skip_if(length(all_blocks) == 0)

  first_block <- all_blocks[[1]]
  # Select only objectives for that sub_pillar
  objs_in_block <- af$master_schema$short_objective[
    !is.na(af$master_schema$sub_pillar) & af$master_schema$sub_pillar == first_block
  ]
  af$update_adjusted_schema(objs_in_block)
  af$update_adjusted_svg(highlight_colour = "lightgreen", default_colour = "white")

  expect_false(is.null(af$adjusted_svg))
  expect_true(nzchar(af$adjusted_svg))
  # The selected block should have lightgreen fill
  if (grepl(paste0('id="', first_block, '"'), af$master_svg)) {
    expect_true(grepl("lightgreen", af$adjusted_svg))
  }
})

test_that("ANAFramework$update_adjusted_svg warns when master_svg is NULL", {
  af <- ANAFramework$new()
  af$master_svg <- NULL
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "HealthStatus",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  af$master_schema <- schema
  af$adjusted_schema <- schema
  expect_warning(af$update_adjusted_svg())
})

# ---- Protocol$add_tools / validate_objective_schema tests ----

test_that("Protocol$add_tools stores tools by name for $ access", {
  p <- Protocol$new()
  p$add_tools("household")
  expect_true("household" %in% names(p$tools))
  expect_true(inherits(p$tools[["household"]], "HouseholdTool"))
})

test_that("Protocol$add_tools uses tool_name when provided", {
  p <- Protocol$new()
  p$add_tools("household", tool_name = "my_hh_tool")
  expect_true("my_hh_tool" %in% names(p$tools))
})

test_that("Protocol$add_tools auto-increments duplicate tool types", {
  p <- Protocol$new()
  p$add_tools("household")
  p$add_tools("household")
  nms <- names(p$tools)
  expect_true("household" %in% nms)
  expect_true(any(grepl("^household_", nms)))
})

test_that("Protocol$validate_objective_schema works as a method", {
  p <- Protocol$new()
  good <- data.frame(
    sector = "Health", pillar = "P1", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  expect_true(p$validate_objective_schema(good))
})

test_that("Protocol$validate_objective_schema errors on bad schema via method", {
  p <- Protocol$new()
  bad <- data.frame(x = 1:3)
  expect_error(p$validate_objective_schema(bad))
})

# ---- Tool public fields tests ----

test_that("Tool$survey is a public field accessible directly", {
  t <- Tool$new()
  expect_true(is.data.frame(t$survey))
  new_survey <- data.frame(type = "text", name = "q1", label = "Question 1",
                           stringsAsFactors = FALSE)
  t$survey <- new_survey
  expect_equal(t$survey$name, "q1")
})

test_that("Tool$choices is a public field", {
  t <- Tool$new()
  expect_true(is.data.frame(t$choices))
})

test_that("Tool$settings is a public field", {
  t <- Tool$new()
  expect_true(is.data.frame(t$settings))
})

test_that("Framework$render_framework_svg errors when no SVG is set", {
  fw <- Framework$new()
  expect_error(fw$render_framework_svg())
})

test_that("Framework$render_framework_svg accepts version='master' and version='adjusted'", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  # Both version values should run without error (rsvg/grid may not be available,
  # in which case a warning is issued and the temp file path is returned).
  expect_no_error(fw$render_framework_svg(version = "master"))
  expect_no_error(fw$render_framework_svg(version = "adjusted"))
})

test_that("Framework$render_framework_svg errors on invalid version argument", {
  fw <- Framework$new()
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  expect_error(fw$render_framework_svg(version = "bad_version"))
})

test_that("Framework$render_framework_svg version='master' uses master_svg even when adjusted_svg is set", {
  skip_if_not(
    requireNamespace("rsvg", quietly = TRUE) && requireNamespace("grid", quietly = TRUE),
    "rsvg/grid not available"
  )
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP",
    short_objective = c("H1", "H2"), text_objective = c("O1", "O2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><rect id="H1"/><rect id="H2"/></svg>')
  fw$update_adjusted_schema("H1")
  fw$update_adjusted_svg()
  expect_false(is.null(fw$adjusted_svg))
  # version='master' should not error
  expect_silent(fw$render_framework_svg(version = "master"))
})

