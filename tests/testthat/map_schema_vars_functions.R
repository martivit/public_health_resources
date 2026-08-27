# Create a mock add_ function for testing
  add_test_indicator <- function(.dataset, multiplier = "2") {
    mult <- as.numeric(multiplier)
    .dataset$result <- .dataset$value * mult
    return(.dataset)
  }

  # Create a mock add_ function
  add_test_indicator_y <- function(.dataset, col1 = "col1") {
    .dataset$result <- .dataset[[col1]] * 2
    return(.dataset)
  }

# Create a mock add_ function
  add_test_var_map <- function(.dataset, input_col = "value") {
    .dataset$output <- .dataset[[input_col]] * 2
    return(.dataset)
  }

# Create a mock add_ function
  add_test_vars <- function(.dataset, var1 = "a", var2 = "b") {
    .dataset$result <- .dataset[[var1]] + .dataset[[var2]]
    return(.dataset)
  }

  # Create a mock add_ function that uses canonical variable references
  add_wash_indicator <- function(.dataset, plans_col = "wash_hwise_plans", drink_col = "wash_hwise_drink") {
    # Simple indicator that combines two columns
    .dataset$wash_indicator <- paste(.dataset[[plans_col]], .dataset[[drink_col]], sep = "_")
    return(.dataset)
  }

# Create two mock add_ functions
  add_step1 <- function(.dataset, input = "value") {
    .dataset$step1_result <- .dataset[[input]] * 2
    return(.dataset)
  }

  add_step2 <- function(.dataset, input = "step1_result") {
    .dataset$step2_result <- .dataset[[input]] + 10
    return(.dataset)
  }

# Create a mock add_ function
add_better_column <- function(.dataset) {
  # Add a column with different values
  .dataset$better_col <- c(
    "new_val_1",
    "new_val_2",
    "new_val_1",
    "new_val_2",
    "new_val_1"
  )
  return(.dataset)
}

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

# Create a mock add_ function that adds a preferred column
add_preferred_column <- function(.dataset) {
  # Add a column with the most preferred name
  .dataset$preferred_name <- .dataset$less_preferred_name
  return(.dataset)
}
