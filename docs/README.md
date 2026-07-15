# phr Package Documentation

Welcome to the **phr** (Public Health Resources) package documentation. This documentation is organized into two main sections: **Data Structures** and **Processes**.

For installation instructions and a package overview, see the [root README](../README.md).

## Documentation Structure

### Data_Structures/

Documentation for the R6 classes that form the foundation of the phr package.

#### Analytics_Classes/ *(new)*
- **[analytics_classes_overview.md](Data_Structures/Analytics_Classes/analytics_classes_overview.md)** - Overview of the unified DataAnalytics class hierarchy
  - Base `DataAnalytics` class (combines quality checks + quantitative analysis)
  - `FSLDataAnalytics`, `HealthDataAnalytics`, `DemographicsDataAnalytics`
  - `MortalityDataAnalytics`, `NutritionDataAnalytics`, `WASHDataAnalytics`
  - `IYCFDataAnalytics`, `GeneralDataAnalytics`, `WaterContainerDataAnalytics`
  - Relationship to parent Data objects and the DAP log

#### Data_Classes/
- **[data_classes_overview.md](Data_Structures/Data_Classes/data_classes_overview.md)** - Overview of the Data class hierarchy
  - Base `Data` class
  - `HouseholdData`, `IndividualData` and subclasses
  - `WomenIndividualData`, `HealthIndividualData`, `DeathIndividualData`, `NutritionIndividualData`
  - `WaterContainerData`, `MUACDataset`
  - Relationship to Log, Quality, and Analytics objects

#### Log_Classes/
- **[log_classes_overview.md](Data_Structures/Log_Classes/log_classes_overview.md)** - Overview of the Log class hierarchy
  - Base `Log` class
  - `CleaningLog` - Track data cleaning operations
  - `DeletionLog` - Track data deletion operations
  - `QuantDataAnalysisPlanLog` - Manage quantitative data analysis plans
  - Ownership by Data and Analytics objects (composition relationship)

#### Tool_Classes/ *(new)*
- **[tool_classes_overview.md](Data_Structures/Tool_Classes/tool_classes_overview.md)** - Overview of the Tool class hierarchy
  - Base `Tool` class — XLSForm management for Kobo/ODK surveys
  - `HouseholdTool` - Household survey XLSForms
  - `KeyInformantTool` - Key informant interview XLSForms
  - `ObservationTool` - Observation/checklist XLSForms
  - Managed by `Protocol` objects

#### Protocol_Class/ *(new)*
- **[protocol_overview.md](Data_Structures/Protocol_Class/protocol_overview.md)** - Overview of the Protocol class
  - Assessment design pipeline (objectives → sample size → sampling → tools → indicators)
  - `Protocol` class fields and methods
  - Sampling methods: SRS, proportional, PPS cluster, RLC, systematic

#### Schema Documentation
- **[schema_overview.md](Data_Structures/schema_overview.md)** - Comprehensive overview of all schema types
- **[indicator_schema.md](Data_Structures/indicator_schema.md)** - Indicator schema structure and usage
- **[variable_schema_harmonization.md](Data_Structures/variable_schema_harmonization.md)** - Variable schema harmonization
- **[variable_value_mapping_guide.md](Data_Structures/variable_value_mapping_guide.md)** - Guide to value mapping
- **[schema_enhancements_examples.md](Data_Structures/schema_enhancements_examples.md)** - Schema enhancement examples
- **[dependency_schema_migration.md](Data_Structures/dependency_schema_migration.md)** - Dependency schema migration guide
- **[na_handling_in_dependency_rules.md](Data_Structures/na_handling_in_dependency_rules.md)** - NA handling in dependencies

### Processes/

Documentation for the major processes and workflows in the public_health_resources package.

#### error_handling/
- **[error_handling.md](Processes/error_handling/error_handling.md)** - Error handling patterns and practices

#### validation/
- **[validation_process.md](Processes/validation/validation_process.md)** - Data validation workflows

#### standardization/
- **[standardization_process.md](Processes/standardization/standardization_process.md)** - Data standardization process

#### cleaning/
- **[cleaning_process.md](Processes/cleaning/cleaning_process.md)** - Data cleaning workflows

#### quality_checks/
- **[quality_variable_mapping.md](Processes/quality_checks/quality_variable_mapping.md)** - Quality variable mapping

#### analysis/
- **[analysis_process.md](Processes/analysis/analysis_process.md)** - Statistical analysis workflows

### General Documentation

- **[coding_conventions.md](coding_conventions.md)** - Coding standards and naming conventions used throughout the codebase

## Quick Start Guide

### 1. Understanding Data Structures

Start with the overview documents for each class hierarchy:
1. Read [Data_Classes/data_classes_overview.md](Data_Structures/Data_Classes/data_classes_overview.md) to understand data management
2. Read [Analytics_Classes/analytics_classes_overview.md](Data_Structures/Analytics_Classes/analytics_classes_overview.md) to understand the unified analytics classes
3. Read [Log_Classes/log_classes_overview.md](Data_Structures/Log_Classes/log_classes_overview.md) to understand logging
4. Read [Tool_Classes/tool_classes_overview.md](Data_Structures/Tool_Classes/tool_classes_overview.md) to understand XLSForm tool management
5. Read [Protocol_Class/protocol_overview.md](Data_Structures/Protocol_Class/protocol_overview.md) to understand assessment design

### 2. Understanding Schemas

Schemas are central to public_health_resources's functionality:
1. Read [schema_overview.md](Data_Structures/schema_overview.md) for a comprehensive introduction
2. Read [indicator_schema.md](Data_Structures/indicator_schema.md) for indicator calculations
3. Read [variable_value_mapping_guide.md](Data_Structures/variable_value_mapping_guide.md) for value mapping

### 3. Understanding Processes

Learn the key workflows:
1. [Validation](Processes/validation/validation_process.md) - How to validate survey data
2. [Standardization](Processes/standardization/standardization_process.md) - How to standardize data
3. [Cleaning](Processes/cleaning/cleaning_process.md) - How to clean data
4. [Quality Checks](Processes/quality_checks/quality_variable_mapping.md) - How to assess data quality
5. [Analysis](Processes/analysis/analysis_process.md) - How to analyze data

## Common Workflows

### Basic Data Processing Workflow (using unified DataAnalytics)

```r
# 1. Load and create data object
data <- HouseholdData$new(data = df, uuid = "uuid")

# 2. Set schema
data$set_variable_schema(schema)

# 3. Standardize
data$standardize()

# 4. Generate unified analytics object (quality + analysis combined)
analytics <- data$generate_data_analytics(stage = "standardized")

# 5. Run quality checks
analytics$run_quality_checks()
cat("Quality score:", analytics$overall_score)

# 6. Clean data using the generated cleaning log
data$generate_cleaning_log(stage = "standardized")
data$clean()

# 7. Run analysis on clean data
analytics_clean <- data$generate_data_analytics(stage = "clean")
analytics_clean$run_analysis()

# 8. Generate outputs
analytics_clean$run_outputs()
```

## Key Concepts

### R6 Classes
phr uses R6 object-oriented programming with inheritance hierarchies for Data, Analytics, Log, Tool, and Protocol classes.

### DataAnalytics (unified)
The `DataAnalytics` hierarchy combines quality checks and quantitative analysis into a single object per domain.  Each subclass ships with default schemas for its sector.

### Schemas
Schemas define the structure, validation rules, and transformations for survey data. Four main types:
- **Variable Schema**: Define variables, types, and validation rules
- **Dependency Schema**: Define logical consistency rules
- **Indicator Schema**: Define indicators to calculate
- **Quality Schema**: Define quality checks and thresholds

### Standardization
The process of transforming raw survey data into a consistent format with:
- Type coercion
- Value mapping
- Select multiple expansion
- Date formatting
- Missing value handling

### Quality Assessment
Systematic checking of data for:
- Plausibility issues
- Outliers
- Missing data patterns
- Logical inconsistencies
- Statistical anomalies

### Protocol Design
The `Protocol` class orchestrates the full assessment design lifecycle, from objective setting through sample drawing and tool management.

## Coding and Naming Conventions

The package uses documented coding and naming standards. See [coding_conventions.md](coding_conventions.md) for details on:
- Dummy columns use **period (`.`)** separator: `skills.reading`, `skills.other`
- Text columns use **underscore (`_`)** separator: `skills_other_text`
- Standard variable names use **underscore (`_`)**: `household_id`, `respondent_age`
- R6 classes use **PascalCase**: `Data`, `HouseholdData`, `DataAnalytics`
- Functions use **snake_case**: `set_variable_schema()`, `standardize()`

## Additional Resources

For package-level documentation, see:
- Function reference: Use `?function_name` in R
- Package vignettes: `browseVignettes("phr")`
- GitHub repository: https://github.com/SaeedR1987/public_health_resources

## Contributing to Documentation

When adding new documentation:
1. Place class documentation in the appropriate `Data_Structures/` subfolder
2. Place process documentation in the appropriate `Processes/` subfolder
3. Include clear examples and use cases
4. Follow the existing documentation structure and style
5. Update this README if adding new major sections

## Getting Help

If you need help:
1. Check the relevant overview document for your class/process
2. Review the schema documentation if working with schemas
3. Look at examples in the process documentation
4. Check the function documentation with `?function_name`
5. Open an issue on GitHub

---

**Last Updated**: 2026-04-14
