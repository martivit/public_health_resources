rm(list = ls())

# library(phr)
devtools::load_all()
library(tibble)
library(dplyr)
source("dev/generate_household_samples.R")
library(phrutils)
library(phrindicators)

df <- readxl::read_xlsx("tests/manual/test fcs data.xlsx", sheet = "hh data")


good_hh <- HouseholdData$new(
  data = df,
  dataset_name = "GoodHouseholdSample",
  variable_map = list(uuid = "parent_uuid"),
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)

good_hh$validate()
good_hh$validated


good_hh$standardize()
good_hh$standardized

good_hh$data_diagnose(stage = "standardized")
View(good_hh$data_diagnostics)

good_hh$generate_cleaning_log()

View(good_hh$cleaning_log$log_df)
