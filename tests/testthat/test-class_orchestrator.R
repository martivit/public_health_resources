library(testthat)

context("Orchestrator class tests")

# Basic initialization
test_that("Orchestrator initialize sets metadata", {
  inst <- Orchestrator$new()
  expect_s3_class(inst$metadata$created_datetime, "POSIXct")
  expect_s3_class(inst$metadata$modified_datetime, "POSIXct")
  expect_true(
    as.numeric(inst$metadata$modified_datetime) >=
      as.numeric(inst$metadata$created_datetime)
  )
})

# Helper subclass exposing public/private fields for set() tests
TestOrchestrator <- R6::R6Class(
  "TestOrchestrator",
  inherit = Orchestrator,
  public = list(
    tools = NULL,
    initialize = function() {
      super$initialize()
      self$tools <- list(
        tool_household_iphra_v2 = list(name = "household"),
        tool_health_iphra_v2 = list(name = "health")
      )
    }
  ),
  private = list(
    secret = "init_secret"
  )
)

test_that("set() replaces a public top-level field directly", {
  inst <- TestOrchestrator$new()
  inst$set(field = "tools", value = list(new_tool = list(name = "new")))
  expect_equal(names(inst$tools), "new_tool")
})

test_that("set() writes a member on a resolved public field", {
  inst <- TestOrchestrator$new()
  inst$set(field = "metadata", member = "custom_flag", value = TRUE)
  expect_true(inst$metadata$custom_flag)
})

test_that("set() writes a member on a name-resolved list element", {
  inst <- TestOrchestrator$new()
  inst$set(
    field = "tools",
    name = "tool_household_iphra_v2",
    member = "name",
    value = "renamed"
  )
  expect_equal(inst$tools$tool_household_iphra_v2$name, "renamed")
})

test_that("set() writes a member on a role-resolved list element", {
  inst <- TestOrchestrator$new()
  inst$set(
    field = "tools",
    role = "health",
    member = "name",
    value = "renamed_health"
  )
  expect_equal(inst$tools$tool_health_iphra_v2$name, "renamed_health")
})

test_that("set() can write to a private field", {
  inst <- TestOrchestrator$new()
  inst$set(field = "secret", value = "updated_secret")
  expect_equal(inst$.__enclos_env__$private$secret, "updated_secret")
})

test_that("set() updates the modified timestamp by default", {
  inst <- TestOrchestrator$new()
  Sys.sleep(0.01)
  before <- inst$metadata$modified_datetime
  inst$set(field = "secret", value = "again")
  expect_true(inst$metadata$modified_datetime > before)
})

test_that("set() errors for an unknown field", {
  inst <- TestOrchestrator$new()
  expect_error(inst$set(field = "does_not_exist", value = 1))
})

test_that("set() refuses to overwrite a function member", {
  inst <- TestOrchestrator$new()
  expect_error(inst$set(field = "initialize", value = 1))
})

test_that("set() errors when both name and role are supplied", {
  inst <- TestOrchestrator$new()
  expect_error(
    inst$set(
      field = "tools",
      name = "tool_household_iphra_v2",
      role = "health",
      member = "name",
      value = "x"
    )
  )
})
