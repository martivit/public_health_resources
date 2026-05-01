test_that("IPHRAProtocol initializes with an ANAFramework", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
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
  expect_true("tool_household_iphra_v2" %in% tools)
  expect_true("tool_kii_community_iphra_v2" %in% tools)
  expect_true("tool_obs_water_point_iphra_v2" %in% tools)
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
