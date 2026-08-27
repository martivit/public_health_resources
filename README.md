# phr — Public Health Resources

**phr** is an R package that provides a suite of R6-based data classes, validation utilities, quality-control tests, and visualisation helpers for public health survey data.

Core functionality covers:

- **Food Security & Livelihoods (FSL)** — FCS, HDDS, LCSI, rCSI
- **Nutrition** — MUAC, z-scores (WHZ/HAZ/WAZ), IYCF
- **Mortality** — Crude Death Rate, Under-5 Death Rate
- **WASH** — water access, sanitation, hygiene practices
- **Demographics** — population structure, household composition
- **Protocol design** — objective setting, sample size, sampling frame, tool management

## Installation

The package is not yet on CRAN. Install the development version directly from GitHub using the [remotes](https://remotes.r-lib.org/) package:

### From the main branch

```r
# install.packages("remotes")
remotes::install_github("impact-initiatives/public_health_resources")
```

### From the copilot/amend-readme-install-instruction branch

```r
# install.packages("remotes")
remotes::install_github("impact-initiatives/public_health_resources", ref = "copilot/develop-quant_data_pipeline")
```

Then load the package:

```r
library(phr)
```

### System requirements

- R ≥ 4.1.0
- The package imports several CRAN packages (see `DESCRIPTION` for the full list). These are installed automatically by `remotes::install_github()`.

## Quick start

```r
library(phr)

# 1. Create a data object from a raw survey data frame
data <- HouseholdData$new(data = df, uuid = "uuid")

# 2. Attach a variable schema and standardise
data$set_variable_schema(my_variable_schema)
data$standardize()

# 3. Run quality checks
quality <- data$generate_data_quality(stage = "standardized")
quality$run_quality_checks()

# 4. Clean data using the generated cleaning log
data$generate_cleaning_log(stage = "standardized")
data$clean()

# 5. Run analytics (quality + quantitative analysis combined)
analytics <- data$generate_data_analytics(stage = "clean")
analytics$run_quality_checks()
analytics$run_analysis()
```

## Documentation

Full documentation lives in the [`docs/`](docs/README.md) folder:

| Topic | Link |
|-------|------|
| Data classes | [docs/Data_Structures/Data_Classes/](docs/Data_Structures/Data_Classes/data_classes_overview.md) |
| Analytics classes | [docs/Data_Structures/Analytics_Classes/](docs/Data_Structures/Analytics_Classes/analytics_classes_overview.md) |
| Log classes | [docs/Data_Structures/Log_Classes/](docs/Data_Structures/Log_Classes/log_classes_overview.md) |
| Tool classes | [docs/Data_Structures/Tool_Classes/](docs/Data_Structures/Tool_Classes/tool_classes_overview.md) |
| Protocol class | [docs/Data_Structures/Protocol_Class/](docs/Data_Structures/Protocol_Class/protocol_overview.md) |
| Schema overview | [docs/Data_Structures/schema_overview.md](docs/Data_Structures/schema_overview.md) |
| Processes | [docs/Processes/](docs/README.md) |

Individual function documentation is available inside R:

```r
?HouseholdData
?DataAnalytics
?Protocol
```

## Class overview

```
Data (base)
├── HouseholdData
├── IndividualData
│   ├── WomenIndividualData
│   ├── HealthIndividualData
│   ├── DeathIndividualData
│   └── NutritionIndividualData
├── WaterContainerData
└── MUACDataset

DataAnalytics (base — unified quality + analysis)
├── FSLDataAnalytics
├── HealthDataAnalytics
├── DemographicsDataAnalytics
├── MortalityDataAnalytics
├── NutritionDataAnalytics
├── WASHDataAnalytics
├── IYCFDataAnalytics
├── GeneralDataAnalytics
└── WaterContainerDataAnalytics

Log (base)
├── CleaningLog
├── DeletionLog
└── QuantDataAnalysisPlanLog

Tool (base — XLSForm management)
├── HouseholdTool
├── KeyInformantTool
└── ObservationTool

Protocol
```

## License

MIT — see [LICENSE.md](LICENSE.md).
