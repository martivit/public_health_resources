
# =========================================================
# Tests for R/utils_tools_validation.R
# =========================================================


# ---- 1. xlsform_extract_variables -----------------------

test_that("xlsform_extract_variables extracts single variable", {
  expect_equal(xlsform_extract_variables("${age}"), "age")
})

test_that("xlsform_extract_variables extracts multiple variables", {
  result <- xlsform_extract_variables("${age} > 18 and ${consent} = 'yes'")
  expect_equal(result, c("age", "consent"))
})

test_that("xlsform_extract_variables returns character(0) when no matches", {
  expect_equal(xlsform_extract_variables("no variables here"), character(0))
})

test_that("xlsform_extract_variables returns character(0) for NA input", {
  expect_equal(xlsform_extract_variables(NA_character_), character(0))
})

test_that("xlsform_extract_variables returns character(0) for NULL input", {
  expect_equal(xlsform_extract_variables(NULL), character(0))
})

test_that("xlsform_extract_variables returns character(0) for non-character input", {
  expect_equal(xlsform_extract_variables(42), character(0))
})

test_that("xlsform_extract_variables handles variables with underscores and digits", {
  result <- xlsform_extract_variables("${hh_size_01} + ${num_children_05}")
  expect_equal(result, c("hh_size_01", "num_children_05"))
})

test_that("xlsform_extract_variables accepts a custom pattern", {
  result <- xlsform_extract_variables("ref(age) and ref(sex)", pattern = "ref\\(([^)]+)\\)")
  expect_equal(result, c("age", "sex"))
})


# ---- 2. xlsform_collect_variables -----------------------

test_that("xlsform_collect_variables returns unique variables across rows", {
  df <- data.frame(
    relevant = c(
      "${age} > 5",
      "${consent} = 'yes' and ${age} > 0",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  result <- xlsform_collect_variables(df, "relevant")
  expect_setequal(result, c("age", "consent"))
})

test_that("xlsform_collect_variables returns all occurrences when only_unique = FALSE", {
  df <- data.frame(
    relevant = c("${age} > 5", "${age} < 99"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_collect_variables(df, "relevant", only_unique = FALSE)
  expect_equal(result, c("age", "age"))
})

test_that("xlsform_collect_variables returns character(0) when no references exist", {
  df <- data.frame(relevant = c("no vars", NA_character_), stringsAsFactors = FALSE)
  expect_equal(xlsform_collect_variables(df, "relevant"), character(0))
})

test_that("xlsform_collect_variables errors on missing column", {
  df <- data.frame(other = "value", stringsAsFactors = FALSE)
  expect_error(xlsform_collect_variables(df, "relevant"))
})

test_that("xlsform_collect_variables errors when df is not a data frame", {
  expect_error(xlsform_collect_variables(list(a = 1), "a"))
})

test_that("xlsform_collect_variables accepts a custom pattern", {
  df <- data.frame(
    expr = c("ref(age)", "ref(sex) and ref(age)"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_collect_variables(df, "expr", pattern = "ref\\(([^)]+)\\)")
  expect_setequal(result, c("age", "sex"))
})


# ---- 3. xlsform_is_valid_varname ------------------------

test_that("xlsform_is_valid_varname returns valid TRUE for correct names", {
  expect_true(xlsform_is_valid_varname("my_variable")$valid)
  expect_true(xlsform_is_valid_varname("_ok_name")$valid)
  expect_true(xlsform_is_valid_varname("var01")$valid)
})

test_that("xlsform_is_valid_varname returns valid FALSE for names with spaces", {
  result <- xlsform_is_valid_varname("my variable")
  expect_false(result$valid)
  expect_length(result$issues, 1L)
})

test_that("xlsform_is_valid_varname returns valid FALSE for names starting with digit", {
  result <- xlsform_is_valid_varname("123start")
  expect_false(result$valid)
})

test_that("xlsform_is_valid_varname returns valid FALSE for names with apostrophes", {
  result <- xlsform_is_valid_varname("it's")
  expect_false(result$valid)
})

test_that("xlsform_is_valid_varname returns valid FALSE for NA or empty input", {
  expect_false(xlsform_is_valid_varname(NA_character_)$valid)
  expect_false(xlsform_is_valid_varname("")$valid)
  expect_false(xlsform_is_valid_varname(NULL)$valid)
})

test_that("xlsform_is_valid_varname issues include NA row and a message", {
  result <- xlsform_is_valid_varname("bad name")
  expect_true(is.na(result$issues[[1]]$row))
  expect_true(nchar(result$issues[[1]]$message) > 0L)
})


# ---- 4. xlsform_varname_in_survey -----------------------

test_that("xlsform_varname_in_survey returns valid TRUE when name is present", {
  declared <- c("age", "sex", "consent")
  expect_true(xlsform_varname_in_survey("age", declared)$valid)
})

test_that("xlsform_varname_in_survey returns valid FALSE when name is absent", {
  declared <- c("age", "sex")
  result <- xlsform_varname_in_survey("weight", declared)
  expect_false(result$valid)
  expect_length(result$issues, 1L)
})

test_that("xlsform_varname_in_survey issues include NA row and a message", {
  result <- xlsform_varname_in_survey("weight", c("age"))
  expect_true(is.na(result$issues[[1]]$row))
  expect_true(nchar(result$issues[[1]]$message) > 0L)
})

test_that("xlsform_varname_in_survey returns valid FALSE and emits a warning for NA varname", {
  # phr_warning calls base::warning(), so a warning is expected
  expect_warning(xlsform_varname_in_survey(NA_character_, c("age")))
  result <- suppressWarnings(xlsform_varname_in_survey(NA_character_, c("age")))
  expect_false(result$valid)
})

test_that("xlsform_varname_in_survey errors when name_vector is not character", {
  expect_error(xlsform_varname_in_survey("age", 1:5))
})


# ---- 5. xlsform_check_brackets --------------------------

test_that("xlsform_check_brackets returns valid TRUE for balanced brackets", {
  expect_true(xlsform_check_brackets("(a + b) * (c - d)")$valid)
  expect_length(xlsform_check_brackets("(a + b) * (c - d)")$issues, 0L)
})

test_that("xlsform_check_brackets returns valid FALSE for unmatched paren", {
  result <- xlsform_check_brackets("if(a > 1, 'yes'")
  expect_false(result$valid)
  expect_true(any(grepl("Parentheses", sapply(result$issues, `[[`, "message"))))
})

test_that("xlsform_check_brackets returns valid FALSE for unmatched bracket", {
  result <- xlsform_check_brackets("[1 + 2")
  expect_false(result$valid)
  expect_true(any(grepl("[Bb]racket", sapply(result$issues, `[[`, "message"))))
})

test_that("xlsform_check_brackets returns valid TRUE for NA or empty cell", {
  expect_true(xlsform_check_brackets(NA_character_)$valid)
  expect_true(xlsform_check_brackets("")$valid)
})

test_that("xlsform_check_brackets issues include NA row", {
  result <- xlsform_check_brackets("(bad")
  expect_true(is.na(result$issues[[1]]$row))
})


# ---- 6. xlsform_orphan_square_brackets ------------------

test_that("xlsform_orphan_square_brackets returns valid TRUE for variable ref brackets", {
  expect_true(xlsform_orphan_square_brackets("${var}")$valid)
})

test_that("xlsform_orphan_square_brackets returns valid FALSE for standalone bracket", {
  result <- xlsform_orphan_square_brackets("[1]")
  expect_false(result$valid)
  expect_length(result$issues, 1L)
})

test_that("xlsform_orphan_square_brackets returns valid TRUE when no brackets", {
  expect_true(xlsform_orphan_square_brackets("${age} > 5")$valid)
})

test_that("xlsform_orphan_square_brackets returns valid TRUE for NA or empty", {
  expect_true(xlsform_orphan_square_brackets(NA_character_)$valid)
  expect_true(xlsform_orphan_square_brackets("")$valid)
})

test_that("xlsform_orphan_square_brackets issues include NA row and message", {
  result <- xlsform_orphan_square_brackets("[x]")
  expect_true(is.na(result$issues[[1]]$row))
  expect_true(nchar(result$issues[[1]]$message) > 0L)
})


# ---- 7. xlsform_check_group_repeats ---------------------

test_that("xlsform_check_group_repeats returns valid TRUE for matched groups", {
  survey <- data.frame(
    type = c("text", "begin_group", "text", "end_group",
             "begin_repeat", "text", "end_repeat"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_group_repeats returns valid FALSE for unclosed begin_group", {
  survey <- data.frame(
    type = c("begin_group", "text"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  messages <- sapply(result$issues, `[[`, "message")
  expect_true(any(grepl("begin_group", messages)))
})

test_that("xlsform_check_group_repeats returns valid FALSE for unmatched end_group", {
  survey <- data.frame(
    type = c("text", "end_group"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
})

test_that("xlsform_check_group_repeats returns valid FALSE for unclosed begin_repeat", {
  survey <- data.frame(
    type = c("begin_repeat", "text"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  messages <- sapply(result$issues, `[[`, "message")
  expect_true(any(grepl("begin_repeat", messages)))
})

test_that("xlsform_check_group_repeats issues contain row index and message", {
  survey <- data.frame(
    type = c("begin_group", "text"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(is.integer(result$issues[[1]]$row))
  expect_true(nchar(result$issues[[1]]$message) > 0L)
})

test_that("xlsform_check_group_repeats returns valid for survey with no groups", {
  survey <- data.frame(
    type = c("text", "integer", "select_one yn"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_group_repeats errors when type column is missing", {
  df <- data.frame(name = "q1", stringsAsFactors = FALSE)
  expect_error(xlsform_check_group_repeats(df))
})


# ---- 8. xlsform_check_required_sheet_cols ---------------

test_that("xlsform_check_required_sheet_cols passes for complete survey sheet", {
  df <- data.frame(type = "text", name = "q1", label = "Q1",
                   stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "survey")
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_required_sheet_cols flags missing columns in survey", {
  df <- data.frame(type = "text", stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "survey")
  expect_false(result$valid)
  missing_msgs <- sapply(result$issues, `[[`, "message")
  expect_true(any(grepl("name", missing_msgs)))
  expect_true(any(grepl("label", missing_msgs)))
})

test_that("xlsform_check_required_sheet_cols passes for complete choices sheet", {
  df <- data.frame(list_name = "yn", name = "yes", label = "Yes",
                   stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "choices")
  expect_true(result$valid)
})

test_that("xlsform_check_required_sheet_cols flags missing list_name in choices", {
  df <- data.frame(name = "yes", label = "Yes", stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "choices")
  expect_false(result$valid)
  expect_true(any(grepl("list_name", sapply(result$issues, `[[`, "message"))))
})

test_that("xlsform_check_required_sheet_cols accepts custom required_cols", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, required_cols = c("a", "c"))
  expect_false(result$valid)
  expect_true(any(grepl("'c'", sapply(result$issues, `[[`, "message"))))
})

test_that("xlsform_check_required_sheet_cols issues have NA row", {
  df <- data.frame(type = "text", stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "survey")
  expect_true(all(is.na(sapply(result$issues, `[[`, "row"))))
})

test_that("xlsform_check_required_sheet_cols errors when df is not a data frame", {
  expect_error(xlsform_check_required_sheet_cols(list(type = "text"), sheet = "survey"))
})


# ---- 9. xlsform_check_duplicate_names -------------------

test_that("xlsform_check_duplicate_names returns valid TRUE for unique names", {
  df <- data.frame(type = c("text", "integer"), name = c("q1", "q2"),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_duplicate_names detects duplicate names", {
  df <- data.frame(type = c("text", "text", "integer"),
                   name = c("q1", "q1", "q2"),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_false(result$valid)
  messages <- sapply(result$issues, `[[`, "message")
  expect_true(any(grepl("q1", messages)))
})

test_that("xlsform_check_duplicate_names reports a row index per duplicate occurrence", {
  df <- data.frame(type = c("text", "text"),
                   name = c("q1", "q1"),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  rows <- sapply(result$issues, `[[`, "row")
  expect_setequal(rows, c(1L, 2L))
})

test_that("xlsform_check_duplicate_names ignores structural rows", {
  df <- data.frame(
    type = c("begin_group", "text", "text", "end_group"),
    name = c("grp", "q1", "q2", "grp"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
})

test_that("xlsform_check_duplicate_names ignores NA names", {
  df <- data.frame(type = c("text", "text"), name = c("q1", NA),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
})

test_that("xlsform_check_duplicate_names errors when name column is missing", {
  df <- data.frame(type = "text", stringsAsFactors = FALSE)
  expect_error(xlsform_check_duplicate_names(df))
})


# ---- 10. xlsform_is_valid_type --------------------------

test_that("xlsform_is_valid_type returns valid TRUE for basic types", {
  expect_true(xlsform_is_valid_type("text")$valid)
  expect_true(xlsform_is_valid_type("integer")$valid)
  expect_true(xlsform_is_valid_type("decimal")$valid)
  expect_true(xlsform_is_valid_type("date")$valid)
  expect_true(xlsform_is_valid_type("calculate")$valid)
  expect_true(xlsform_is_valid_type("note")$valid)
  expect_true(xlsform_is_valid_type("geopoint")$valid)
})

test_that("xlsform_is_valid_type returns valid TRUE for select types with list names", {
  expect_true(xlsform_is_valid_type("select_one yes_no")$valid)
  expect_true(xlsform_is_valid_type("select_multiple region")$valid)
  expect_true(xlsform_is_valid_type("select one yes_no")$valid)
  expect_true(xlsform_is_valid_type("select multiple region")$valid)
})

test_that("xlsform_is_valid_type returns valid TRUE for structural types", {
  expect_true(xlsform_is_valid_type("begin_group")$valid)
  expect_true(xlsform_is_valid_type("end_group")$valid)
  expect_true(xlsform_is_valid_type("begin_repeat")$valid)
  expect_true(xlsform_is_valid_type("end_repeat")$valid)
  expect_true(xlsform_is_valid_type("begin group")$valid)
  expect_true(xlsform_is_valid_type("end group")$valid)
})

test_that("xlsform_is_valid_type returns valid FALSE for non-standard types", {
  expect_false(xlsform_is_valid_type("freetext")$valid)
  expect_false(xlsform_is_valid_type("number")$valid)
  expect_false(xlsform_is_valid_type("dropdown")$valid)
})

test_that("xlsform_is_valid_type returns valid FALSE for NA or empty input", {
  expect_false(xlsform_is_valid_type(NA_character_)$valid)
  expect_false(xlsform_is_valid_type("")$valid)
  expect_false(xlsform_is_valid_type(NULL)$valid)
})

test_that("xlsform_is_valid_type issues have NA row", {
  result <- xlsform_is_valid_type("freetext")
  expect_true(is.na(result$issues[[1]]$row))
  expect_true(nchar(result$issues[[1]]$message) > 0L)
})


# ---- 11. xlsform_check_choice_references ----------------

test_that("xlsform_check_choice_references passes when all lists exist", {
  survey <- data.frame(
    type = c("text", "select_one yes_no", "select_multiple region"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "yes_no", "region", "region"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_choice_references flags missing list with row index", {
  survey <- data.frame(
    type = c("select_one yes_no", "select_one not_defined"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "yes_no"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_false(result$valid)
  rows     <- sapply(result$issues, `[[`, "row")
  messages <- sapply(result$issues, `[[`, "message")
  expect_true(2L %in% rows)
  expect_true(any(grepl("not_defined", messages)))
})

test_that("xlsform_check_choice_references returns valid when no selects present", {
  survey  <- data.frame(type = c("text", "integer"), stringsAsFactors = FALSE)
  choices <- data.frame(list_name = "unused", stringsAsFactors = FALSE)
  result  <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
})

test_that("xlsform_check_choice_references handles space-variant select types", {
  survey <- data.frame(
    type = c("select one yes_no", "select multiple region"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "region"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
})

test_that("xlsform_check_choice_references errors when type column is missing", {
  survey  <- data.frame(name = "q1", stringsAsFactors = FALSE)
  choices <- data.frame(list_name = "yn", stringsAsFactors = FALSE)
  expect_error(xlsform_check_choice_references(survey, choices))
})

test_that("xlsform_check_choice_references errors when list_name column is missing", {
  survey  <- data.frame(type = "select_one yn", stringsAsFactors = FALSE)
  choices <- data.frame(name = "yes", stringsAsFactors = FALSE)
  expect_error(xlsform_check_choice_references(survey, choices))
})


# ---- 12. xlsform_check_label_presence -------------------

test_that("xlsform_check_label_presence passes when all questions have labels", {
  survey <- data.frame(
    type  = c("text", "integer"),
    name  = c("q1", "q2"),
    label = c("Question 1", "Question 2"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_label_presence flags rows with NA label", {
  survey <- data.frame(
    type  = c("text", "integer"),
    name  = c("q1", "q2"),
    label = c("Question 1", NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_false(result$valid)
  rows <- sapply(result$issues, `[[`, "row")
  expect_equal(rows, 2L)
})

test_that("xlsform_check_label_presence includes question name in message", {
  survey <- data.frame(
    type  = c("text"),
    name  = c("q1"),
    label = c(NA_character_),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_true(grepl("q1", result$issues[[1]]$message))
})

test_that("xlsform_check_label_presence does not flag calculate or structural rows", {
  survey <- data.frame(
    type  = c("text", "calculate", "begin_group", "end_group"),
    name  = c("q1", "calc1", "grp", "grp"),
    label = c("Q1", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_label_presence flags user-facing rows when label column absent", {
  survey <- data.frame(
    type = c("text", "integer", "calculate"),
    name = c("q1", "q2", "c1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_false(result$valid)
  rows <- sapply(result$issues, `[[`, "row")
  expect_setequal(rows, c(1L, 2L))
})

test_that("xlsform_check_label_presence errors when type column is missing", {
  df <- data.frame(name = "q1", label = "Q1", stringsAsFactors = FALSE)
  expect_error(xlsform_check_label_presence(df))
})


# ---- 13. xlsform_check_calculate_expression -------------

test_that("xlsform_check_calculate_expression passes when all calculate rows have expressions", {
  survey <- data.frame(
    type        = c("text", "calculate"),
    name        = c("q1", "age_yrs"),
    calculation = c(NA, "${age} div 1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_calculate_expression flags missing calculation with row index", {
  survey <- data.frame(
    type        = c("text", "calculate", "calculate"),
    name        = c("q1", "c1", "c2"),
    calculation = c(NA, "${age} div 1", NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_false(result$valid)
  rows <- sapply(result$issues, `[[`, "row")
  expect_equal(rows, 3L)
})

test_that("xlsform_check_calculate_expression includes variable name in message", {
  survey <- data.frame(
    type        = c("calculate"),
    name        = c("bmi"),
    calculation = c(NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_true(grepl("bmi", result$issues[[1]]$message))
})

test_that("xlsform_check_calculate_expression returns valid when no calculate rows", {
  survey <- data.frame(
    type = c("text", "integer"),
    name = c("q1", "q2"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_calculate_expression flags all calculate rows when column absent", {
  survey <- data.frame(
    type = c("text", "calculate"),
    name = c("q1", "c1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_false(result$valid)
  expect_equal(sapply(result$issues, `[[`, "row"), 2L)
})

test_that("xlsform_check_calculate_expression errors when type column is missing", {
  df <- data.frame(name = "q1", calculation = "x", stringsAsFactors = FALSE)
  expect_error(xlsform_check_calculate_expression(df))
})


# ---- 14. xlsform_check_undefined_references -------------

test_that("xlsform_check_undefined_references passes when all refs are declared", {
  survey <- data.frame(
    type     = c("text", "text"),
    name     = c("age", "consent"),
    relevant = c(NA, "${age} > 0"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_undefined_references(survey)
  expect_true(result$valid)
  expect_length(result$issues, 0L)
})

test_that("xlsform_check_undefined_references flags undeclared variable with row index", {
  survey <- data.frame(
    type     = c("text", "text"),
    name     = c("age", "weight"),
    relevant = c(NA, "${height} > 0"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_undefined_references(survey)
  expect_false(result$valid)
  rows <- sapply(result$issues, `[[`, "row")
  expect_equal(rows, 2L)
  messages <- sapply(result$issues, `[[`, "message")
  expect_true(any(grepl("height", messages)))
})

test_that("xlsform_check_undefined_references checks multiple expression columns", {
  survey <- data.frame(
    type        = c("text", "calculate"),
    name        = c("age", "bmi"),
    relevant    = c(NA, "${consent} = 'yes'"),
    calculation = c(NA, "${weight} div (${height} * ${height})"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_undefined_references(survey)
  expect_false(result$valid)
  messages <- sapply(result$issues, `[[`, "message")
  # consent, weight, height are all undefined
  expect_true(any(grepl("consent", messages)))
  expect_true(any(grepl("weight", messages)))
})

test_that("xlsform_check_undefined_references returns valid when no check cols present", {
  survey <- data.frame(
    type = c("text"),
    name = c("q1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_undefined_references(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_undefined_references errors when name column is missing", {
  df <- data.frame(type = "text", relevant = "${age} > 0",
                   stringsAsFactors = FALSE)
  expect_error(xlsform_check_undefined_references(df))
})
