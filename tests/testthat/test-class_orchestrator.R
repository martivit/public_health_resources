library(testthat)

context("Orchestrator class tests")

# Basic initialization
test_that("Orchestrator initialize sets metadata", {
  inst <- Orchestrator$new()
  expect_s3_class(inst$metadata$created_datetime, "POSIXct")
  expect_s3_class(inst$metadata$modified_datetime, "POSIXct")
  expect_true(as.numeric(inst$metadata$modified_datetime) >= as.numeric(inst$metadata$created_datetime))
})

# access_nested: top-level, name lookup, role lookup, member invocation
test_that("access_nested returns top-level and resolves name/role/member", {
  inst <- Orchestrator$new()
  inst$tools <- list(
    tool_household_iphra_v2 = list(
      get = function() 7,
      val = "x",
      foo = 1
    ),
    tool_other = list(
      get = function() 9,
      val = "y"
    )
  )

  expect_equal(inst$access_nested("tools"), inst$tools)
  expect_equal(inst$access_nested("tools", name = "tool_household_iphra_v2"), inst$tools$tool_household_iphra_v2)
  expect_equal(inst$access_nested("tools", name = "tool_household_iphra_v2", member = "get"), 7)
  expect_equal(inst$access_nested("tools", role = "household", member = "val"), "x")
})

# set_nested should update nested value and touch modified timestamp
test_that("set_nested updates nested member and updates modified timestamp", {
  inst <- Orchestrator$new()
  inst$tools <- list(tool_household_iphra_v2 = list(foo = 1))
  old_mod <- inst$metadata$modified_datetime
  Sys.sleep(0.01)
  inst$set_nested("tools", "foo", 999, name = "tool_household_iphra_v2")

  expect_equal(inst$access_nested("tools", name = "tool_household_iphra_v2", member = "foo"), 999)
  expect_true(as.numeric(inst$metadata$modified_datetime) >= as.numeric(old_mod))
})

# sync_state should assign to target_field when provided
test_that("sync_state assigns target_field when provided", {
  inst <- Orchestrator$new()
  inst$tools <- list(tool_household_iphra_v2 = list(get = function() 3))
  inst$sync_state(field = "tools", member = "get", target_field = "assigned$got", name = "tool_household_iphra_v2")
  expect_equal(inst$assigned$got, 3)
})

# Error conditions
test_that("access_nested and sync_state produce informative errors for bad inputs", {
  inst <- Orchestrator$new()
  inst$tools <- list(tool_household_iphra_v2 = list(get = function() 1))
  expect_error(inst$access_nested("missing_field"))
  expect_error(inst$access_nested("tools", name = "tool_household_iphra_v2", member = "nope"))
  expect_error(inst$sync_state(field = "tools", member = "nope", name = "tool_household_iphra_v2"))
})
