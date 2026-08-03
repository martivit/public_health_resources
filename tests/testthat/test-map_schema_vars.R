library(testthat)
library(tibble)


# Test: map_schema_vars method


test_that("map_schema_vars returns invisible self when no schema is defined", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Should not error and return self

  result <- d$map_schema_vars()
  expect_s3_class(result, "Data")
})

test_that("map_schema_vars maps columns based on col_names in schema", {
  # Create data with alternative column names
  df <- tibble::tibble(
    id = 1:5,
    survey_id = paste0("survey_", 1:5),
    person_sex = c("male", "female", "male", "female", "male"),
    individual_age = c(25, 30, 45, 22, 60)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Set a schema with col_names
  schema <- list(
    types = list(
      uuid = "character",
      sex = "character",
      age = "numeric"
    ),
    col_names = list(
      uuid = c("uuid", "survey_id", "submission_id"),
      sex = c("sex", "person_sex", "individual_sex"),
      age = c("age", "individual_age", "person_age")
    ),
    allowed_values = list(
      sex = c("male", "female")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Check that variables were mapped
  expect_equal(d$variable_map$uuid, "id")  # Already set at init
  expect_equal(d$variable_map$sex, "person_sex")
  expect_equal(d$variable_map$age, "individual_age")

  # Check that values were mapped for non-numeric type with allowed_values
  expect_true("sex" %in% names(d$value_map))
  expect_true(all(d$value_map$sex %in% c("male", "female")))
})

test_that("map_schema_vars does not map values for numeric types", {
  df <- tibble::tibble(
    id = 1:5,
    my_score = c(1, 2, 3, 2, 1)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema with numeric type and allowed_values (which shouldn't be value-mapped)
  schema <- list(
    types = list(
      score = "numeric"
    ),
    col_names = list(
      score = c("score", "my_score", "test_score")
    ),
    allowed_values = list(
      score = c(1, 2, 3, 4, 5)  # Even with allowed values, should not map
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  expect_equal(d$variable_map$score, "my_score")
  expect_false("score" %in% names(d$value_map))  # Should NOT map values for numeric
})

test_that("map_schema_vars does not overwrite existing mappings", {
  df <- tibble::tibble(
    id = 1:5,
    custom_sex = c("m", "f", "m", "f", "m"),
    person_sex = c("male", "female", "male", "female", "male")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Pre-set a mapping
  d$variable_map$sex <- "custom_sex"

  schema <- list(
    types = list(sex = "character"),
    col_names = list(sex = c("sex", "person_sex", "individual_sex"))
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should NOT overwrite existing mapping
  expect_equal(d$variable_map$sex, "custom_sex")
})

test_that("map_schema_vars handles missing columns gracefully", {
  df <- tibble::tibble(
    id = 1:5,
    other_col = c("a", "b", "c", "d", "e")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(
      missing_var = "character"
    ),
    col_names = list(
      missing_var = c("not_in_data", "also_not_here", "nope")
    )
  )

  d$set_variable_schema(schema)

  # Should not error
  expect_no_error(d$map_schema_vars())

  # Should not have added the mapping
  expect_null(d$variable_map$missing_var)
})

test_that("map_schema_vars only maps found allowed values", {
  df <- tibble::tibble(
    id = 1:5,
    my_category = c("cat_a", "cat_b", "cat_a", "cat_c", "cat_a")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(
      category = "character"
    ),
    col_names = list(
      category = c("category", "my_category")
    ),
    allowed_values = list(
      category = c("cat_a", "cat_b", "cat_c", "cat_d", "cat_e")  # cat_d and cat_e not in data
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should only contain values found in the data
  expect_true(all(d$value_map$category %in% c("cat_a", "cat_b", "cat_c")))
  expect_false("cat_d" %in% d$value_map$category)
  expect_false("cat_e" %in% d$value_map$category)
})



# Test: repair_maps method


test_that("repair_maps updates variable_map from dataframe", {
  df <- tibble::tibble(
    id = 1:5,
    col_a = c("x", "y", "x", "y", "x"),
    col_b = c(1, 2, 3, 4, 5)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  var_map_df <- data.frame(
    role = c("category", "score"),
    column_name = c("col_a", "col_b"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(variable_map_df = var_map_df)

  expect_true(result$success)
  expect_equal(result$variables_updated, 2)
  expect_equal(d$variable_map$category, "col_a")
  expect_equal(d$variable_map$score, "col_b")
})

test_that("repair_maps updates value_map from dataframe", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  val_map_df <- data.frame(
    role = c("status", "category"),
    values = c("active,inactive,pending", "cat1,cat2,cat3"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(value_map_df = val_map_df)

  expect_true(result$success)
  expect_equal(result$values_updated, 2)
  expect_equal(d$value_map$status, c("active", "inactive", "pending"))
  expect_equal(d$value_map$category, c("cat1", "cat2", "cat3"))
})


test_that("repair_maps with mode='replace' clears existing mappings", {
  df <- tibble::tibble(
    id = 1:3,
    new_col = c("a", "b", "c")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Set initial mappings
  d$variable_map$old_role <- "some_col"
  d$value_map$old_values <- c("val1", "val2")

  var_map_df <- data.frame(
    role = c("new_role"),
    column_name = c("new_col"),
    stringsAsFactors = FALSE
  )

  val_map_df <- data.frame(
    role = c("new_vals"),
    values = c("x,y,z"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(
    variable_map_df = var_map_df,
    value_map_df = val_map_df,
    mode = "replace"
  )

  # Old mappings should be gone (except uuid)
  expect_null(d$variable_map$old_role)
  expect_null(d$value_map$old_values)

  # New mappings should exist
  expect_equal(d$variable_map$new_role, "new_col")
  expect_equal(d$value_map$new_vals, c("x", "y", "z"))

  # UUID should still be preserved
  expect_equal(d$variable_map$uuid, "id")
})

test_that("repair_maps with mode='merge' keeps existing mappings", {
  df <- tibble::tibble(
    id = 1:3,
    col_a = c("a", "b", "c"),
    col_b = c(1, 2, 3)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Set initial mappings
  d$variable_map$old_role <- "col_a"
  d$value_map$old_values <- c("val1", "val2")

  var_map_df <- data.frame(
    role = c("new_role"),
    column_name = c("col_b"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(
    variable_map_df = var_map_df,
    mode = "merge"
  )

  # Old mappings should still exist
  expect_equal(d$variable_map$old_role, "col_a")
  expect_equal(d$value_map$old_values, c("val1", "val2"))

  # New mappings should also exist
  expect_equal(d$variable_map$new_role, "col_b")
})

test_that("repair_maps removes mapping when column_name is NA", {
  df <- tibble::tibble(
    id = 1:3,
    col_a = c("a", "b", "c")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$variable_map$to_remove <- "col_a"

  var_map_df <- data.frame(
    role = c("to_remove"),
    column_name = c(NA),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(variable_map_df = var_map_df)

  expect_null(d$variable_map$to_remove)
})

test_that("repair_maps never removes uuid mapping", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  var_map_df <- data.frame(
    role = c("uuid"),
    column_name = c(NA),  # Try to remove uuid
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(variable_map_df = var_map_df)

  # UUID should still be there
  expect_equal(d$variable_map$uuid, "id")
})

test_that("repair_maps validates column existence and returns warnings", {
  df <- tibble::tibble(
    id = 1:3,
    existing_col = c("a", "b", "c")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  var_map_df <- data.frame(
    role = c("good_mapping", "bad_mapping"),
    column_name = c("existing_col", "nonexistent_col"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(variable_map_df = var_map_df, validate_columns = TRUE)

  # Good mapping should work
  expect_equal(d$variable_map$good_mapping, "existing_col")

  # Bad mapping should generate warning
  expect_false(result$success)
  expect_true(length(result$warnings) > 0)

  # Bad mapping should NOT be added
  expect_null(d$variable_map$bad_mapping)
})

test_that("repair_maps skips validation when validate_columns=FALSE", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  var_map_df <- data.frame(
    role = c("some_role"),
    column_name = c("nonexistent_col"),
    stringsAsFactors = FALSE
  )

  result <- d$repair_maps(
    variable_map_df = var_map_df,
    validate_columns = FALSE
  )

  # Should add mapping without validation
  expect_equal(d$variable_map$some_role, "nonexistent_col")
  expect_true(result$success)
})

test_that("repair_maps errors on invalid input structure", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Missing required columns
  bad_df <- data.frame(
    wrong_col = c("a", "b"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    d$repair_maps(variable_map_df = bad_df),
    regexp = "missing required columns:"
  )
})



# Test: get_maps_as_df method


test_that("get_maps_as_df returns correct dataframes", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$variable_map$category <- "some_col"
  d$variable_map$score <- "other_col"
  d$value_map$category <- c("a", "b", "c")

  maps <- d$get_maps_as_df()

  # Check variable_map_df
  expect_true(is.data.frame(maps$variable_map_df))
  expect_true("role" %in% names(maps$variable_map_df))
  expect_true("column_name" %in% names(maps$variable_map_df))
  expect_true("uuid" %in% maps$variable_map_df$role)
  expect_true("category" %in% maps$variable_map_df$role)

  # Check value_map_df
  expect_true(is.data.frame(maps$value_map_df))
  expect_true("role" %in% names(maps$value_map_df))
  expect_true("values" %in% names(maps$value_map_df))
  expect_true("category" %in% maps$value_map_df$role)
  expect_equal(maps$value_map_df$values[maps$value_map_df$role == "category"], "a,b,c")
})

test_that("get_maps_as_df returns empty dataframes when maps are empty", {
  df <- tibble::tibble(id = 1:3)

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$value_map <- list()  # Empty value map

  maps <- d$get_maps_as_df()

  expect_true(nrow(maps$value_map_df) == 0)
  expect_true(nrow(maps$variable_map_df) > 0)  # Should at least have uuid
})



# Test: Integration with HouseholdData


test_that("HouseholdData calls map_schema_vars on initialize", {
  # Create mock data with alternative column names that should be auto-mapped
  mock_data <- tibble::tibble(
    uuid = paste0("hh_", 1:10),
    consent = sample(c("yes", "no"), 10, replace = TRUE),
    interview_date = Sys.Date() - sample(1:30, 10),
    enumerator_id = paste0("E", sprintf("%02d", sample(1:5, 10, replace = TRUE)))
  )

  # This should trigger map_schema_vars during initialization
  hh <- suppressMessages(
    HouseholdData$new(
      data = mock_data,
      dataset_name = "TestHH"
    )
  )

  # The HouseholdData should have schema set
  expect_true(!is.null(hh$variable_schema))

  # Variables that match col_names in the schema should be auto-mapped
  # (the exact mappings depend on the schema template content)
  expect_true("uuid" %in% names(hh$variable_map))
})



# Test: map_schema_vars with select_multiple questions


test_that("map_schema_vars handles select_multiple with value_map", {
  # Create data with select_multiple column (space-separated values)
  df <- tibble::tibble(
    id = 1:5,
    livelihood = c(
      "farming fishing",
      "trading",
      "farming trading other",
      "fishing",
      "farming"
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema with select_multiple question type and nested value_map
  schema <- list(
    types = list(
      livelihood = "character"
    ),
    col_names = list(
      livelihood = c("livelihood", "income_source")
    ),
    question_types = list(
      livelihood = "select_multiple"
    ),
    value_map = list(
      livelihood = list(
        agriculture = c("farming", "agriculture", "crop"),
        fishing = c("fishing", "fishery"),
        business = c("trading", "business", "commerce"),
        other = c("other", "autre")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Variable should be mapped
  expect_equal(d$variable_map$livelihood, "livelihood")

  # Value map should be built with found values only
  expect_true("livelihood" %in% names(d$value_map))

  # Check that canonical values are mapped correctly
  value_map <- d$value_map$livelihood

  # Should find "farming" for agriculture
  expect_true("agriculture" %in% names(value_map))
  expect_true("farming" %in% value_map$agriculture)

  # Should find "fishing" for fishing
  expect_true("fishing" %in% names(value_map))
  expect_true("fishing" %in% value_map$fishing)

  # Should find "trading" for business
  expect_true("business" %in% names(value_map))
  expect_true("trading" %in% value_map$business)

  # Should find "other" for other
  expect_true("other" %in% names(value_map))
  expect_true("other" %in% value_map$other)

  # Should NOT include schema-defined allowed values that are not in the actual data
  # "agriculture" and "crop" are allowed for agriculture canonical value, but not in data
  expect_false("agriculture" %in% value_map$agriculture)
  expect_false("crop" %in% value_map$agriculture)
  # "fishery" is allowed for fishing canonical value, but not in data
  expect_false("fishery" %in% value_map$fishing)
})

test_that("map_schema_vars select_multiple handles unordered values", {
  # Test that order doesn't matter in space-separated values
  df <- tibble::tibble(
    id = 1:4,
    skills = c(
      "reading writing",
      "writing reading math",
      "math reading",
      "reading"
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(skills = "character"),
    col_names = list(skills = c("skills", "abilities")),
    question_types = list(skills = "select_multiple"),
    value_map = list(
      skills = list(
        literacy = c("reading", "writing", "lecture", "ecriture"),
        numeracy = c("math", "mathematics", "maths", "calcul")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should find both literacy skills regardless of order
  expect_true("literacy" %in% names(d$value_map$skills))
  expect_true(all(c("reading", "writing") %in% d$value_map$skills$literacy))

  # Should find numeracy skill
  expect_true("numeracy" %in% names(d$value_map$skills))
  expect_true("math" %in% d$value_map$skills$numeracy)

  # Should NOT find French translations as they're not in data
  expect_false("lecture" %in% d$value_map$skills$literacy)
  expect_false("ecriture" %in% d$value_map$skills$literacy)
  expect_false("calcul" %in% d$value_map$skills$numeracy)
})

test_that("map_schema_vars select_multiple with allowed_values (backward compat)", {
  # Test that select_multiple also works with old allowed_values structure
  df <- tibble::tibble(
    id = 1:3,
    transport = c("car bus", "bike", "car bike")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(transport = "character"),
    col_names = list(transport = c("transport", "vehicle")),
    question_types = list(transport = "select_multiple"),
    allowed_values = list(
      transport = c("car", "bus", "bike", "walk", "taxi")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should find values that exist in data
  expect_true("transport" %in% names(d$value_map))
  expect_true(all(c("car", "bus", "bike") %in% d$value_map$transport))

  # Should NOT include values not in data
  expect_false("walk" %in% d$value_map$transport)
  expect_false("taxi" %in% d$value_map$transport)
})

test_that("map_schema_vars select_multiple handles empty and NA values", {
  df <- tibble::tibble(
    id = 1:5,
    food = c("rice beans", NA, "", "rice", "beans rice maize")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(food = "character"),
    col_names = list(food = c("food", "diet")),
    question_types = list(food = "select_multiple"),
    value_map = list(
      food = list(
        cereals = c("rice", "maize", "wheat"),
        legumes = c("beans", "lentils", "peas")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should handle NA and empty gracefully and still find other values
  expect_true("food" %in% names(d$value_map))
  expect_true("rice" %in% d$value_map$food$cereals)
  expect_true("maize" %in% d$value_map$food$cereals)
  expect_true("beans" %in% d$value_map$food$legumes)

  # Should not include values not in data
  expect_false("wheat" %in% d$value_map$food$cereals)
  expect_false("lentils" %in% d$value_map$food$legumes)
})

test_that("map_schema_vars select_multiple only maps canonical values with found data", {
  # If none of the allowed values for a canonical value exist,
  # that canonical value should not be in the value_map
  df <- tibble::tibble(
    id = 1:3,
    food = c("rice", "rice beans", "rice")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(food = "character"),
    col_names = list(food = c("food")),
    question_types = list(food = "select_multiple"),
    value_map = list(
      food = list(
        cereals = c("rice", "wheat", "maize"),
        legumes = c("beans", "lentils"),
        vegetables = c("carrot", "tomato", "onion")  # None in data
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should include cereals and legumes
  expect_true("cereals" %in% names(d$value_map$food))
  expect_true("legumes" %in% names(d$value_map$food))

  # Should NOT include vegetables since none found in data
  expect_false("vegetables" %in% names(d$value_map$food))
})

test_that("map_schema_vars select_multiple uses word boundaries for matching", {
  # Test that partial matches don't work (e.g., "farm" shouldn't match "farming")
  df <- tibble::tibble(
    id = 1:3,
    activity = c("farm fishing", "farmer", "farm")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(activity = "character"),
    col_names = list(activity = c("activity")),
    question_types = list(activity = "select_multiple"),
    value_map = list(
      activity = list(
        agriculture = c("farm", "farming"),  # "farm" should match, "farming" not in data
        fishing = c("fishing", "fish")       # "fishing" should match, "fish" shouldn't
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should find "farm" (exact word boundary match)
  expect_true("agriculture" %in% names(d$value_map$activity))
  expect_true("farm" %in% d$value_map$activity$agriculture)

  # Should find "fishing"
  expect_true("fishing" %in% names(d$value_map$activity))
  expect_true("fishing" %in% d$value_map$activity$fishing)

  # Should NOT find "farming" or "fish" as they're not in data
  expect_false("farming" %in% d$value_map$activity$agriculture)
  expect_false("fish" %in% d$value_map$activity$fishing)

  # Note: "farmer" in data should NOT match "farm" because of word boundaries
})

test_that("map_schema_vars select_multiple handles special regex characters", {
  # Test that allowed values with regex special characters work correctly
  df <- tibble::tibble(
    id = 1:3,
    code = c("A+ B-", "C* A+", "B-")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(code = "character"),
    col_names = list(code = c("code")),
    question_types = list(code = "select_multiple"),
    value_map = list(
      code = list(
        positive = c("A+", "B+"),
        negative = c("A-", "B-"),
        special = c("C*", "D?")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should find values with special regex characters
  expect_true("code" %in% names(d$value_map))
  expect_true("positive" %in% names(d$value_map$code))
  expect_true("A+" %in% d$value_map$code$positive)

  expect_true("negative" %in% names(d$value_map$code))
  expect_true("B-" %in% d$value_map$code$negative)

  expect_true("special" %in% names(d$value_map$code))
  expect_true("C*" %in% d$value_map$code$special)


})

# Test: map_schema_vars column priority (preferential mapping) ####

test_that("map_schema_vars maps first matching column when multiple exist", {
  # Create data with BOTH columns present
  df <- tibble::tibble(
    id = 1:5,
    linked_num_deaths = c(2, 0, 1, 3, 0),
    num_deaths = c(1, 0, 2, 1, 0)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema where linked_num_deaths is listed FIRST
  schema <- list(
    types = list(
      deaths = "numeric"
    ),
    col_names = list(
      deaths = c("linked_num_deaths", "num_deaths")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should map to linked_num_deaths since it's listed first
  expect_equal(d$variable_map$deaths, "linked_num_deaths")
})

test_that("map_schema_vars maps second column if first not present", {
  # Create data with ONLY second column present
  df <- tibble::tibble(
    id = 1:5,
    num_deaths = c(1, 0, 2, 1, 0)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema where linked_num_deaths is listed FIRST but not in data
  schema <- list(
    types = list(
      deaths = "numeric"
    ),
    col_names = list(
      deaths = c("linked_num_deaths", "num_deaths")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should map to num_deaths since linked_num_deaths is not available
  expect_equal(d$variable_map$deaths, "num_deaths")
})

test_that("map_schema_vars respects column order priority with three options", {
  # Create data with three possible columns
  df <- tibble::tibble(
    id = 1:5,
    deaths_total = c(5, 3, 2, 4, 1),
    num_deaths = c(1, 0, 2, 1, 0),
    death_count = c(2, 1, 0, 3, 2)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema with priority: linked_num_deaths (missing), num_deaths, death_count
  schema <- list(
    types = list(
      deaths = "numeric"
    ),
    col_names = list(
      deaths = c("linked_num_deaths", "num_deaths", "death_count", "deaths_total")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should map to num_deaths (second in list) since linked_num_deaths is missing
  expect_equal(d$variable_map$deaths, "num_deaths")
})

test_that("map_schema_vars with reversed order maps correctly", {
  # Create data with BOTH columns present but in reversed priority
  df <- tibble::tibble(
    id = 1:5,
    linked_num_deaths = c(2, 0, 1, 3, 0),
    num_deaths = c(1, 0, 2, 1, 0)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Schema where num_deaths is listed FIRST (reversed priority)
  schema <- list(
    types = list(
      deaths = "numeric"
    ),
    col_names = list(
      deaths = c("num_deaths", "linked_num_deaths")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should map to num_deaths since it's listed first now
  expect_equal(d$variable_map$deaths, "num_deaths")
})

test_that("map_schema_vars remaps if existing mapping points to non-existent column", {
  # Create data with only one column
  df <- tibble::tibble(
    id = 1:5,
    linked_num_deaths = c(2, 0, 1, 3, 0)
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  # Pre-map to a column that doesn't exist in data
  d$variable_map$deaths <- "nonexistent_column"

  # Schema where linked_num_deaths is available
  schema <- list(
    types = list(
      deaths = "numeric"
    ),
    col_names = list(
      deaths = c("linked_num_deaths", "num_deaths")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars()

  # Should remap to linked_num_deaths since existing mapping is invalid
  expect_equal(d$variable_map$deaths, "linked_num_deaths")
})


# ============================================================================
# Comprehensive Test: map_schema_vars ensures ALL allowable values found in
# data are added to value_map
# ============================================================================

test_that("map_schema_vars adds ALL allowable values found in data to value_map", {
    # This test verifies the core requirement:
    # When using map_schema_vars to map variable and value names in Data Class,
    # we ensure we are adding to the value_map ALL allowable values that are:
    # 1. Found in the data
    # 2. Listed as allowable in the variable schema

    # Create test data with a mix of values
    df <- tibble::tibble(
      id = 1:10,
      consent_status = c(
        "yes", "oui", "si",          # 3 different "yes" variants
        "no", "non", "nein",         # 3 different "no" variants
        "maybe", "perhaps",          # 2 different "maybe" variants
        "yes", "no"                  # Duplicates to verify uniqueness
      ),
      education = c(
        "primary", "secondary", "university",
        "primary", "none", "secondary",
        "primary", "university", "none", "secondary"
      )
    )

    d <- suppressMessages(
      Data$new(data = df, dataset_name = "TestData", uuid = "id")
    )

    # Define schema with nested value_map structure
    # Some allowed values ARE in data, some are NOT
    schema <- list(
      types = list(
        consent = "character",
        education = "character"
      ),
      col_names = list(
        consent = c("consent_status", "consent_col"),
        education = c("education", "edu_level")
      ),
      value_map = list(
        consent = list(
          # For "yes" canonical value:
          # - "yes", "oui", "si" ARE in data (should be included)
          # - "ja", "sí" are NOT in data (should NOT be included)
          yes = c("yes", "oui", "si", "ja", "sí"),

          # For "no" canonical value:
          # - "no", "non", "nein" ARE in data (should be included)
          # - "nee" is NOT in data (should NOT be included)
          no = c("no", "non", "nein", "nee"),

          # For "maybe" canonical value:
          # - "maybe", "perhaps" ARE in data (should be included)
          # - "possibly" is NOT in data (should NOT be included)
          maybe = c("maybe", "perhaps", "possibly"),

          # For "declined" canonical value:
          # - NONE of these are in data (entire canonical value should be EXCLUDED)
          declined = c("declined", "refused", "reject")
        ),
        education = list(
          basic = c("none", "primary"),           # Both ARE in data
          secondary = c("secondary", "high"),     # "secondary" IS, "high" is NOT
          tertiary = c("university", "college"),  # "university" IS, "college" is NOT
          vocational = c("technical", "trade")    # NONE in data (should be EXCLUDED)
        )
      )
    )

    d$set_variable_schema(schema)
    d$map_schema_vars(stage = "raw")

    # ===== VERIFY VARIABLE MAPPINGS
    expect_equal(d$variable_map$consent, "consent_status")
    expect_equal(d$variable_map$education, "education")

    # ===== VERIFY VALUE MAPPINGS FOR CONSENT

    # Canonical value "yes" should include ALL found allowable values
    expect_true("yes" %in% names(d$value_map$consent))
    expect_true(all(c("yes", "oui", "si") %in% d$value_map$consent$yes))
    expect_length(d$value_map$consent$yes, 3)  # Exactly 3 values
    # Should NOT include values not in data
    expect_false("ja" %in% d$value_map$consent$yes)
    expect_false("sí" %in% d$value_map$consent$yes)

    # Canonical value "no" should include ALL found allowable values
    expect_true("no" %in% names(d$value_map$consent))
    expect_true(all(c("no", "non", "nein") %in% d$value_map$consent$no))
    expect_length(d$value_map$consent$no, 3)  # Exactly 3 values
    # Should NOT include values not in data
    expect_false("nee" %in% d$value_map$consent$no)

    # Canonical value "maybe" should include ALL found allowable values
    expect_true("maybe" %in% names(d$value_map$consent))
    expect_true(all(c("maybe", "perhaps") %in% d$value_map$consent$maybe))
    expect_length(d$value_map$consent$maybe, 2)  # Exactly 2 values
    # Should NOT include values not in data
    expect_false("possibly" %in% d$value_map$consent$maybe)

    # Canonical value "declined" should NOT be in value_map at all
    # because NONE of its allowable values are in the data
    expect_false("declined" %in% names(d$value_map$consent))

    # ===== VERIFY VALUE MAPPINGS FOR EDUCATION

    # Canonical value "basic" should include ALL found values
    expect_true("basic" %in% names(d$value_map$education))
    expect_true(all(c("none", "primary") %in% d$value_map$education$basic))
    expect_length(d$value_map$education$basic, 2)

    # Canonical value "secondary" should include only found value
    expect_true("secondary" %in% names(d$value_map$education))
    expect_true("secondary" %in% d$value_map$education$secondary)
    expect_false("high" %in% d$value_map$education$secondary)
    expect_length(d$value_map$education$secondary, 1)

    # Canonical value "tertiary" should include only found value
    expect_true("tertiary" %in% names(d$value_map$education))
    expect_true("university" %in% d$value_map$education$tertiary)
    expect_false("college" %in% d$value_map$education$tertiary)
    expect_length(d$value_map$education$tertiary, 1)

    # Canonical value "vocational" should NOT be in value_map
    expect_false("vocational" %in% names(d$value_map$education))

    # ===== VERIFY COMPLETENESS
    # Ensure ALL data values that are listed as allowable are captured

    # Get all unique values from data
    data_consent_values <- unique(df$consent_status)
    data_education_values <- unique(df$education)

    # Flatten all values in value_map
    all_mapped_consent <- unlist(d$value_map$consent, use.names = FALSE)
    all_mapped_education <- unlist(d$value_map$education, use.names = FALSE)

    # Every data value that's in the schema should be in value_map
    # (This is the KEY requirement being tested)
    schema_consent_all <- unlist(schema$value_map$consent, use.names = FALSE)
    schema_education_all <- unlist(schema$value_map$education, use.names = FALSE)

    data_values_in_schema_consent <- intersect(data_consent_values, schema_consent_all)
    data_values_in_schema_education <- intersect(data_education_values, schema_education_all)

    # Every data value that's allowable should be in the value_map
    expect_true(all(data_values_in_schema_consent %in% all_mapped_consent),
                info = "All data values listed in schema should be in value_map")
    expect_true(all(data_values_in_schema_education %in% all_mapped_education),
                info = "All data values listed in schema should be in value_map")

    # Conversely, no values should be in value_map that aren't in the data
    expect_true(all(all_mapped_consent %in% data_consent_values),
                info = "Only data values should be in value_map")
    expect_true(all(all_mapped_education %in% data_education_values),
                info = "Only data values should be in value_map")
  })


test_that("map_schema_vars handles select_multiple with ALL allowable values", {
  # Verify select_multiple also captures ALL allowable values found in data

  df <- tibble::tibble(
    id = 1:5,
    income_source = c(
      "farming fishing hunting",
      "trading selling business",
      "farming trading",
      "fishing hunting",
      "farming business hunting"
    )
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(income = "character"),
    col_names = list(income = c("income_source", "livelihood")),
    question_types = list(income = "select_multiple"),
    value_map = list(
      income = list(
        # Multiple synonyms for agriculture - some in data, some not
        agriculture = c("farming", "agriculture", "cultivation", "crop_growing"),
        # Multiple synonyms for fishing - some in data, some not
        aquatic = c("fishing", "fishery", "aquaculture"),
        # Multiple synonyms for hunting - some in data, some not
        hunting = c("hunting", "trapping", "game"),
        # Multiple synonyms for business - some in data, some not
        commerce = c("trading", "selling", "business", "commerce", "merchant"),
        # NOT in data at all
        livestock = c("cattle", "goats", "sheep", "animals")
      )
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars(stage = "raw")

  # Check agriculture - should have "farming" but not other synonyms
  expect_true("agriculture" %in% names(d$value_map$income))
  expect_true("farming" %in% d$value_map$income$agriculture)
  expect_false("agriculture" %in% d$value_map$income$agriculture)
  expect_false("cultivation" %in% d$value_map$income$agriculture)
  expect_false("crop_growing" %in% d$value_map$income$agriculture)

  # Check aquatic - should have "fishing" but not other synonyms
  expect_true("aquatic" %in% names(d$value_map$income))
  expect_true("fishing" %in% d$value_map$income$aquatic)
  expect_false("fishery" %in% d$value_map$income$aquatic)
  expect_false("aquaculture" %in% d$value_map$income$aquatic)

  # Check hunting - should have "hunting" but not other synonyms
  expect_true("hunting" %in% names(d$value_map$income))
  expect_true("hunting" %in% d$value_map$income$hunting)
  expect_false("trapping" %in% d$value_map$income$hunting)
  expect_false("game" %in% d$value_map$income$hunting)

  # Check commerce - should have ALL THREE values found in data
  expect_true("commerce" %in% names(d$value_map$income))
  expect_true("trading" %in% d$value_map$income$commerce)
  expect_true("selling" %in% d$value_map$income$commerce)
  expect_true("business" %in% d$value_map$income$commerce)
  expect_length(d$value_map$income$commerce, 3)  # Exactly 3 found
  # But not the ones not in data
  expect_false("commerce" %in% d$value_map$income$commerce)
  expect_false("merchant" %in% d$value_map$income$commerce)

  # Check livestock - should NOT be in value_map at all
  expect_false("livestock" %in% names(d$value_map$income))
})


test_that("map_schema_vars with allowed_values (backward compat) includes ALL found values", {
  # Test the older allowed_values structure also captures all found values

  df <- tibble::tibble(
    id = 1:8,
    status = c("active", "inactive", "pending", "active",
               "suspended", "active", "pending", "inactive")
  )

  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  schema <- list(
    types = list(account_status = "character"),
    col_names = list(account_status = c("status", "account_status")),
    # Old-style allowed_values instead of nested value_map
    allowed_values = list(
      account_status = c("active", "inactive", "pending", "suspended",
                         "deleted", "archived", "banned")
    )
  )

  d$set_variable_schema(schema)
  d$map_schema_vars(stage = "raw")

  # Should include ALL four values found in data
  expect_true("account_status" %in% names(d$value_map))
  expect_true(all(c("active", "inactive", "pending", "suspended") %in%
                    d$value_map$account_status))
  expect_length(d$value_map$account_status, 4)

  # Should NOT include values not in data
  expect_false("deleted" %in% d$value_map$account_status)
  expect_false("archived" %in% d$value_map$account_status)
  expect_false("banned" %in% d$value_map$account_status)
})


# Test: map_schema_vars is called after each indicator in standardize


test_that("map_schema_vars is called after each add_* function in standardize", {
  # Create a mock add_ function that adds a column
  add_test_indicator_1 <- function(.dataset) {
    .dataset$indicator_1 <- rep("value_a", nrow(.dataset))
    return(.dataset)
  }

  # Create a mock add_ function that depends on indicator_1
  add_test_indicator_2 <- function(.dataset, dep_col) {
    # This function checks if dep_col exists (should be mapped from indicator_1)
    if (!is.null(dep_col) && dep_col %in% names(.dataset)) {
      .dataset$indicator_2 <- paste0("depends_on_", .dataset[[dep_col]])
    } else {
      .dataset$indicator_2 <- "no_dependency"
    }
    return(.dataset)
  }

  # Create test data
  df <- tibble::tibble(
    id = 1:5,
    existing_col = c("x", "y", "x", "y", "x")
  )

  # Create schema with indicator schema
  variable_schema <- list(
    types = list(
      uuid = "character",
      test_indicator_1 = "character",
      test_indicator_2 = "character"
    ),
    col_names = list(
      uuid = c("id"),
      test_indicator_1 = c("indicator_1", "test_ind_1"),
      test_indicator_2 = c("indicator_2", "test_ind_2")
    ),
    allowed_values = list(
      test_indicator_1 = c("value_a", "value_b", "value_c")
    )
  )

  indicator_schema <- list(
    test_indicator_1 = list(
      function_name = "add_test_indicator_1",
      variables = character(0),
      arguments = list()
    ),
    test_indicator_2 = list(
      function_name = "add_test_indicator_2",
      variables = c("test_indicator_1"),  # Depends on test_indicator_1
      arguments = list(
        dep_col = "@variable_map$test_indicator_1"  # Should resolve to "indicator_1"
      )
    )
  )

  # Create data object
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$set_variable_schema(variable_schema)
  d$set_indicator_schema(indicator_schema)

  # Run standardize
  d$standardize()

  # Check that both indicators were computed
  expect_true("indicator_1" %in% names(d$standardized_data))
  expect_true("indicator_2" %in% names(d$standardized_data))

  # Check that variable mappings were created
  expect_equal(d$variable_map$test_indicator_1, "indicator_1")
  expect_equal(d$variable_map$test_indicator_2, "indicator_2")

  # Check that value mapping was created for indicator_1
  expect_true("test_indicator_1" %in% names(d$value_map))
  expect_true("value_a" %in% d$value_map$test_indicator_1)

  # Most importantly: check that indicator_2 successfully used the dependency
  # If map_schema_vars was not called after indicator_1, this would fail
  expect_true(all(grepl("^depends_on_", d$standardized_data$indicator_2)))
  expect_false(any(d$standardized_data$indicator_2 == "no_dependency"))
})

test_that("map_schema_vars updates to more preferred column when available", {
  # Test that if a more preferred column becomes available, the mapping is updated

  # Create a mock add_ function that adds a preferred column
  add_preferred_column <- function(.dataset) {
    # Add a column with the most preferred name
    .dataset$preferred_name <- .dataset$less_preferred_name
    return(.dataset)
  }

  # Create test data with less preferred column
  df <- tibble::tibble(
    id = 1:5,
    less_preferred_name = c("a", "b", "c", "d", "e")
  )

  # Create schema with column name preferences
  variable_schema <- list(
    types = list(
      uuid = "character",
      my_var = "character"
    ),
    col_names = list(
      uuid = c("id"),
      my_var = c("preferred_name", "less_preferred_name", "fallback_name")  # Order matters
    )
  )

  indicator_schema <- list(
    add_preferred = list(
      function_name = "add_preferred_column",
      variables = c("my_var"),
      arguments = list()
    )
  )

  # Create data object
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$set_variable_schema(variable_schema)
  d$set_indicator_schema(indicator_schema)

  # Initially, should map to less_preferred_name
  d$map_schema_vars(stage = "raw")
  expect_equal(d$variable_map$my_var, "less_preferred_name")

  # Run standardize (which adds preferred_name column via indicator)
  d$standardize()

  # After standardize, should have updated to preferred_name
  expect_equal(d$variable_map$my_var, "preferred_name")

  # Check that the preferred column exists
  expect_true("preferred_name" %in% names(d$standardized_data))
})

test_that("map_schema_vars does not downgrade to less preferred column", {
  # Test that if we already have a preferred column, we don't switch to less preferred

  # Create test data with both columns
  df <- tibble::tibble(
    id = 1:5,
    preferred_name = c("a", "b", "c", "d", "e"),
    less_preferred_name = c("f", "g", "h", "i", "j")
  )

  # Create schema
  variable_schema <- list(
    types = list(
      uuid = "character",
      my_var = "character"
    ),
    col_names = list(
      uuid = c("id"),
      my_var = c("preferred_name", "less_preferred_name")
    )
  )

  # Create data object
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$set_variable_schema(variable_schema)

  # Should map to preferred_name initially
  d$map_schema_vars(stage = "raw")
  expect_equal(d$variable_map$my_var, "preferred_name")

  # Run map_schema_vars again - should stay on preferred_name
  d$map_schema_vars(stage = "raw")
  expect_equal(d$variable_map$my_var, "preferred_name")
})

test_that("map_schema_vars updates value_map when variable_map is updated", {
  # Test that when a more preferred column is found, value_map is also updated

  # Create a mock add_ function
  add_better_column <- function(.dataset) {
    # Add a column with different values
    .dataset$better_col <- c("new_val_1", "new_val_2", "new_val_1", "new_val_2", "new_val_1")
    return(.dataset)
  }

  # Create test data
  df <- tibble::tibble(
    id = 1:5,
    old_col = c("old_val_1", "old_val_2", "old_val_1", "old_val_2", "old_val_1")
  )

  # Create schema
  variable_schema <- list(
    types = list(
      uuid = "character",
      status = "character"
    ),
    col_names = list(
      uuid = c("id"),
      status = c("better_col", "old_col")
    ),
    allowed_values = list(
      status = c("new_val_1", "new_val_2", "old_val_1", "old_val_2")
    )
  )

  indicator_schema <- list(
    add_better = list(
      function_name = "add_better_column",
      variables = character(0),
      arguments = list()
    )
  )

  # Create data object
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )
  d$set_variable_schema(variable_schema)
  d$set_indicator_schema(indicator_schema)

  # Initially should map to old_col with old values
  d$map_schema_vars(stage = "raw")
  expect_equal(d$variable_map$status, "old_col")
  expect_true(all(c("old_val_1", "old_val_2") %in% d$value_map$status))

  # Run standardize
  d$standardize()

  # Should now map to better_col with new values
  expect_equal(d$variable_map$status, "better_col")
  expect_true(all(c("new_val_1", "new_val_2") %in% d$value_map$status))
  expect_false("old_val_1" %in% d$value_map$status)
})


# Test: map_schema_labels method

test_that("map_schema_labels returns invisible self when no schema is defined", {
  df <- tibble::tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "TestData", uuid = "id")
  )

  result <- d$map_schema_labels()
  expect_s3_class(result, "Data")
  expect_length(d$variable_label, 0)
  expect_length(d$value_label, 0)
})

test_that("map_schema_labels populates variable_label and value_label from schema (english)", {
  df <- tibble::tibble(
    id     = 1:4,
    gender = c("male", "female", "male", "female"),
    age    = c(25L, 30L, 45L, 22L)
  )
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "LabelTest", uuid = "id")
  )

  schema <- list(
    types      = list(sex = "character", age = "integer"),
    col_names  = list(sex = c("sex", "gender"), age = c("age")),
    value_map  = list(sex = list(male = c("male"), female = c("female"))),
    variable_labels = list(
      en = list(sex = "Sex of Respondent", age = "Age in Years"),
      fr = list(sex = "Sexe du répondant",  age = "Âge en années"),
      ar = list(sex = "جنس المستجيب",        age = "العمر بالسنوات")
    ),
    value_labels = list(
      en = list(sex = c(male = "Male", female = "Female")),
      fr = list(sex = c(male = "Homme", female = "Femme")),
      ar = list(sex = c(male = "ذكر",   female = "أنثى"))
    )
  )
  d$set_variable_schema(schema)
  d$map_schema_vars()
  d$map_schema_labels()

  expect_equal(d$variable_label$sex, "Sex of Respondent")
  expect_equal(d$variable_label$age, "Age in Years")
  expect_equal(d$value_label$sex[["male"]],   "Male")
  expect_equal(d$value_label$sex[["female"]], "Female")
})

test_that("map_schema_labels respects language argument", {
  df <- tibble::tibble(id = 1:2, gender = c("male", "female"))
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "LangTest", uuid = "id")
  )

  schema <- list(
    types     = list(sex = "character"),
    col_names = list(sex = c("sex", "gender")),
    value_map = list(sex = list(male = c("male"), female = c("female"))),
    variable_labels = list(
      en = list(sex = "Sex"),
      fr = list(sex = "Sexe"),
      ar = list(sex = "جنس")
    ),
    value_labels = list(
      en = list(sex = c(male = "Male",  female = "Female")),
      fr = list(sex = c(male = "Homme", female = "Femme")),
      ar = list(sex = c(male = "ذكر",   female = "أنثى"))
    )
  )
  d$set_variable_schema(schema)
  d$map_schema_vars()

  d$map_schema_labels(language = "french")
  expect_equal(d$variable_label$sex, "Sexe")
  expect_equal(d$value_label$sex[["male"]], "Homme")

  d$map_schema_labels(language = "arabic")
  expect_equal(d$variable_label$sex, "جنس")
  expect_equal(d$value_label$sex[["male"]], "ذكر")
})

test_that("map_schema_labels defaults to english for unknown language", {
  df <- tibble::tibble(id = 1:2, gender = c("male", "female"))
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "LangFallback", uuid = "id")
  )

  schema <- list(
    types     = list(sex = "character"),
    col_names = list(sex = c("sex", "gender")),
    variable_labels = list(en = list(sex = "Sex"), fr = list(sex = "Sexe"))
  )
  d$set_variable_schema(schema)
  d$map_schema_vars()
  d$map_schema_labels(language = "spanish")

  expect_equal(d$variable_label$sex, "Sex")
})

test_that("map_schema_labels only labels variables present in variable_map", {
  df <- tibble::tibble(id = 1:2, gender = c("male", "female"))
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "PartialLabel", uuid = "id")
  )

  schema <- list(
    types     = list(sex = "character", age = "integer"),
    col_names = list(sex = c("sex", "gender"), age = c("age")),
    variable_labels = list(
      en = list(sex = "Sex", age = "Age")
    )
  )
  d$set_variable_schema(schema)
  d$map_schema_vars()
  d$map_schema_labels()

  # sex is in variable_map (gender column matched), age is NOT (no age column)
  expect_true("sex" %in% names(d$variable_label))
  expect_false("age" %in% names(d$variable_label))
})

test_that("map_schema_labels is called automatically after map_schema_vars in standardize", {
  df <- tibble::tibble(id = 1:3, gender = c("male", "female", "male"))
  d <- suppressMessages(
    Data$new(data = df, dataset_name = "AutoLabel", uuid = "id")
  )

  schema <- list(
    types     = list(sex = "character"),
    col_names = list(sex = c("sex", "gender")),
    value_map = list(sex = list(male = c("male"), female = c("female"))),
    variable_labels = list(en = list(sex = "Sex")),
    value_labels    = list(en = list(sex = c(male = "Male", female = "Female")))
  )
  d$set_variable_schema(schema)
  d$standardize()

  expect_equal(d$variable_label$sex, "Sex")
  expect_equal(d$value_label$sex[["male"]], "Male")
})

