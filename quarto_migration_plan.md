# Plan: Migrate IPHRA Protocol Document Generation to Quarto

## Executive Summary

This plan outlines how to migrate the current `officer` (DOCX/PPTX) based protocol document generation system to **Quarto**, maintaining all functionality while gaining improved maintainability, version control, and modern authoring workflows.

---

## Current System Architecture

### 1. Class Hierarchy
```
Orchestrator
  └─ Document
       └─ Protocol
            └─ SurveyProtocol
                 └─ IPHRAProtocol
```

### 2. Current Workflow Components

#### A. **Template System**
- **Word Template**: `inst/resources/reach_tor_iphra_template.docx` (268 KB)
  - Pre-formatted DOCX with styles, headers, footers
  - Contains placeholder tags like `@country`, `@release_date`, `@specific_objectives`
  
- **PowerPoint Template**: `inst/resources/protocol_report_template.pptx`
  - Slide layouts with placeholder labels

#### B. **Schema-Driven Tag Replacement**
- **Schema File**: `inst/resources/protocol_schema_iphra.xlsx` (604 rows × 5 columns)
  - Columns: `tag_name`, `handling`, `condition`, `default_value`, `function_name`
  
- **Tag Handling Types** (7 types):
  1. **`replace`** (372 tags): Static text replacement
     - Example: `@tor_title` → "Research Terms of Reference"
  
  2. **`input`** (29 tags): Metadata field injection
     - Example: `@country` → `self$metadata$country_name`
  
  3. **`calculate`** (52 tags): Active binding evaluation
     - Example: `@release_date` → `Sys.Date()`
     - Example: `@specific_objectives` → computed from framework catalog
  
  4. **`checkbox_replace`** (105 tags): Boolean to checkbox symbol
     - Example: `@crisis_conflict` → "X" or "☐"
  
  5. **`row_delete`** (22 tags): Conditional removal
     - Example: `@household_tool_inc` → removed if not included
  
  6. **`table`** (6 tags): Flextable insertion
     - `@primary_data_sources_table` → calls `table_primary_data_sources()`
     - `@sample_size_hh_gen_table` → calls `table_sample_size_general()`
  
  7. **`image`** (1 tag): SVG/image insertion
     - `@modified_framework_svg` → framework diagram

#### C. **Conditional Logic System**
- **124 conditional replacements** based on:
  - Protocol state (sampling methods, tools included)
  - Active bindings (`.mortality_survey`, `.kii_community_yes`)
  - Metadata flags (`self$conditional_metadata`)
  
- **Example conditions**:
  ```
  condition = "srs_site_selection, srs_household_selection, multiple_methods_no"
  condition = "mortality_survey"
  condition = "num_report"  # checks if metadata field is non-NA
  ```

#### D. **Document Processing Flow**
1. `IPHRAProtocol$generate_doc(output_file)`
2. → `Document$generate_doc()`
3. → `..apply_protocol_schema_sections(doc, schema)`
4. → Process in handler order: `row_delete` → `replace` → `input` → `checkbox_replace` → `calculate` → `table` → `image`
5. → `..remove_remaining_tags()` - strip unprocessed `@tags`
6. → `officer::print(doc, target = output_file)`

#### E. **Table Generation**
- **Helper functions** in `R/utils_doc_schema_handlers_iphra.R`:
  - `table_primary_data_sources()` - tools × objectives matrix
  - `table_secondary_data_sources()` - secondary data table
  - `table_sample_size_general()` - sample size parameters by stratum
  - `table_sample_size_individual()` - MUAC/nutrition indicators
  - `table_sample_size_mortality()` - mortality survey parameters
  
- **Data sources**:
  - `self$sample_table` - from nested `Sample` object
  - `self$framework$master_objectives_schema` - indicator metadata
  - `self$tools` - tool indicator codes
  - `self$secondary_data` - external data sources

---

## Proposed Quarto Migration Architecture

### 1. **Core Concept: Parametrized Quarto Documents**

Replace the Word template + schema-driven tag system with:
- **Quarto template**: `.qmd` file with YAML parameters and R code chunks
- **R markdown syntax**: Native conditional blocks, computed values, tables
- **Flexible rendering**: HTML, PDF, DOCX, or PPTX output

### 2. **File Structure**

```
inst/
  quarto/
    templates/
      iphra_tor_template.qmd           # Main TOR template
      iphra_tor_slides.qmd              # Presentation template
      _metadata.yml                     # Shared Quarto config
      _extensions/                      # Custom Quarto extensions (if needed)
        reach-theme/
    
    partials/                           # Reusable qmd fragments
      executive_summary.qmd
      methodology_overview.qmd
      sampling_design.qmd
      data_sources.qmd
      
    styles/
      reach_docx_reference.docx         # Pandoc reference doc for styling
      reach_pptx_reference.pptx         # Pandoc reference for slides
```

### 3. **Quarto Template Structure**

#### `iphra_tor_template.qmd`
```yaml
---
title: "Research Terms of Reference"
subtitle: "Integrated Public Health Rapid Assessment (IPHRA)"
format:
  docx:
    reference-doc: styles/reach_docx_reference.docx
    toc: true
    number-sections: true
  pdf:
    toc: true
  html:
    theme: cosmo

params:
  country: "Country Name"
  assessment_title: "Assessment Title"
  month_year: "January 2024"
  release_date: !expr Sys.Date()
  
  # Framework objects (passed as R objects)
  framework_objectives: NULL
  research_questions: NULL
  secondary_data: NULL
  
  # Sample design
  sample_table: NULL
  sampling_methods: []
  strata_names: []
  
  # Tool inclusion flags
  mortality_survey: false
  muac_survey: false
  kii_community: false
  
  # Crisis metadata
  type_of_emergency: "natural_disaster"
  type_of_crisis: "sudden_onset"
---

# Executive Summary

**Country:** `r params$country`  
**Release Date:** `r params$release_date`

<!-- Conditional content based on crisis type -->
:::{.content-visible when-meta="type_of_emergency == 'natural_disaster'"}
This assessment addresses a natural disaster emergency...
:::

:::{.content-visible when-meta="type_of_emergency == 'conflict'"}
This assessment addresses a conflict-related emergency...
:::

# Research Objectives

`r params$framework_objectives`

# Methodology

## Sampling Design

The assessment employed a **`r params$sampling_methods[1]`** sampling approach.

```{r}
#| echo: false
#| output: asis

if (params$mortality_survey) {
  cat("### Mortality Recall Survey\n\n")
  cat("A retrospective mortality survey was conducted...\n")
}
```

## Sample Size Calculations

```{r}
#| echo: false
#| tbl-cap: "Sample Size Parameters by Stratum"

if (!is.null(params$sample_table) && nrow(params$sample_table) > 0) {
  # Render flextable or knitr::kable
  phr::table_sample_size_general(params$sample_table)
}
```

# Data Sources

## Primary Data Collection

```{r}
#| echo: false
#| tbl-cap: "Primary Data Sources"

if (!is.null(params$framework_objectives)) {
  phr::table_primary_data_sources(
    master_schema = params$framework_objectives,
    tool_indicator_codes = params$tool_codes
  )
}
```

```
#### **Key Features**:
1. **Native conditionals**: `.content-visible`, `when-meta`, R `if` statements
2. **Computed values**: R expressions in YAML (`!expr Sys.Date()`)
3. **Code chunks**: Tables, plots, dynamic content generation
4. **Cross-references**: `@tbl-sample-size`, `@fig-framework`
5. **Callouts**: `:::{.callout-note}` for warnings/notes

### 4. **Class Integration: New `QuartoDocument` Subclass**

```r
QuartoDocument <- R6::R6Class(
  "QuartoDocument",
  inherit = Document,
  
  public = list(
    quarto_template = NULL,
    quarto_params = list(),
    
    initialize = function(template_name = "iphra_tor_template.qmd") {
      self$quarto_template <- system.file(
        "quarto/templates", template_name, 
        package = "phr"
      )
    },
    
    generate_doc = function(output_file = "protocol_report.docx", 
                           format = "docx", 
                           open = FALSE) {
      # Build params list from protocol state
      params <- private$..build_quarto_params()
      
      # Render via quarto R package
      quarto::quarto_render(
        input = self$quarto_template,
        execute_params = params,
        output_format = format,
        output_file = output_file
      )
      
      if (open) utils::browseURL(output_file)
      invisible(self)
    }
  ),
  
  private = list(
    ..build_quarto_params = function() {
      list(
        # Metadata
        country = self$metadata$country_name,
        assessment_title = self$metadata$assessment_title,
        release_date = Sys.Date(),
        
        # Framework data
        framework_objectives = self$framework_objective_catalog_adjusted,
        research_questions = private$..extract_research_questions(),
        secondary_data = self$secondary_data,
        
        # Sample design
        sample_table = self$sample_table,
        sampling_methods = self$sampling_methods,
        strata_names = self$strata_names,
        
        # Tool/survey flags (from active bindings)
        mortality_survey = self$.mortality_survey,
        muac_survey = self$.muac_survey,
        kii_community = self$.kii_community_yes,
        
        # Tool codes for tables
        tool_codes = lapply(self$tools, function(t) {
          t$get_indicator_codes(prefer_revised = TRUE)
        })
      )
    }
  )
)
```

### 5. **Migration of Schema-Driven Logic**

| Current System | Quarto Equivalent |
|---|---|
| `@country` tag in DOCX | `` `r params$country` `` inline code |
| `condition = "mortality_survey"` | `when-meta="mortality_survey"` or R `if` |
| `handling = "calculate"` | R code chunk with `#| echo: false` |
| `handling = "checkbox_replace"` | `` `r if(params$flag) "☑" else "☐"` `` |
| `handling = "table"` | R code chunk calling table function |
| `handling = "row_delete"` | Conditional div: `::: {when-meta=...}` |
| `..remove_remaining_tags()` | Not needed - Quarto fails on undefined params |

### 6. **Conditional Content Patterns**

#### Pattern A: Simple Boolean Flags
```markdown
::: {.content-visible when-meta="mortality_survey"}
## Mortality Survey Methodology
Retrospective mortality data was collected...
:::
```

#### Pattern B: Multiple Conditions (AND logic)
```r
```{r}
#| echo: false
#| output: asis

if (params$srs_site_selection && params$multiple_methods_no) {
  cat("The assessment used simple random sampling...")
}
```
```

#### Pattern C: Complex Logic (mimics current schema)
```r
```{r}
#| echo: false
#| output: asis

# Equivalent to current condition parsing
show_section <- function(conditions) {
  all(sapply(strsplit(conditions, ",")[[1]], function(c) {
    trimws(c) %in% names(params) && isTRUE(params[[trimws(c)]])
  }))
}

if (show_section("srs_site_selection, srs_household_selection, multiple_methods_no")) {
  cat("### Simple Random Sampling\n\n")
  cat("Households were selected using simple random selection...\n")
}
```
```

### 7. **Table Generation Adaptation**

**Current**: `flextable` objects via `officer::body_add_flextable()`  
**Quarto**: `flextable` objects render natively in Quarto!

```r
```{r}
#| label: tbl-sample-sizes
#| tbl-cap: "Sample Size Calculations"
#| echo: false

table_sample_size_general(params$sample_table)
```
```

**No changes needed** to existing table functions - `flextable` works in Quarto DOCX/HTML/PDF.

### 8. **SVG/Image Handling**

**Current**: Active binding `.modified_framework_svg` returns temp SVG path  
**Quarto**:

```r
```{r}
#| label: fig-framework
#| fig-cap: "Modified Analytical Framework"
#| echo: false

svg_path <- tempfile(fileext = ".svg")
writeLines(params$adjusted_svg_text, svg_path)
knitr::include_graphics(svg_path)
```
```

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
1. **Create Quarto infrastructure**
   - Set up `inst/quarto/` directory structure
   - Build minimal `iphra_tor_template.qmd` template
   - Convert 1-2 sections (e.g., Executive Summary, Objectives)

2. **Implement `QuartoDocument` class**
   - Inherit from `Document`
   - Implement `..build_quarto_params()` private method
   - Override `generate_doc()` to call `quarto::quarto_render()`

3. **Testing infrastructure**
   - Create test protocol objects with known states
   - Render test documents and compare with current DOCX outputs

### Phase 2: Template Migration (Week 3-4)
4. **Port schema sections to Quarto conditionals**
   - Mapping: Create spreadsheet mapping 604 schema rows → QMD patterns
   - Priority order: 
     - `replace` tags → inline R code (easiest)
     - `input` tags → `params$*` references
     - `calculate` tags → R code chunks with active bindings
     - Conditional blocks → `.content-visible` or R `if`

5. **Table integration**
   - Verify `flextable` rendering in DOCX/HTML/PDF
   - Adapt table functions if needed for Quarto compatibility
   - Test complex sample size tables with real protocol data

6. **Image/SVG handling**
   - Test SVG rendering in Quarto DOCX
   - Implement fallback to PNG conversion if needed

### Phase 3: Advanced Features (Week 5-6)
7. **Styling and formatting**
   - Create Pandoc reference DOCX (`reach_docx_reference.docx`)
   - Match current template styles (fonts, colors, spacing)
   - Header/footer customization

8. **PowerPoint support**
   - Create `iphra_tor_slides.qmd` template
   - Test `format: pptx` output
   - Adapt table sizing for slides

9. **Partial templates**
   - Extract reusable sections to `partials/`
   - Use `{{< include partials/methodology.qmd >}}` syntax

### Phase 4: User Experience (Week 7-8)
10. **Integration with existing workflows**
    - Add `use_quarto = TRUE` parameter to `IPHRAProtocol$initialize()`
    - Maintain backward compatibility with `officer` system
    - Deprecation warnings for legacy system

11. **Documentation**
    - Vignette: "Migrating from Officer to Quarto"
    - Template customization guide
    - roxygen2 docs for `QuartoDocument` class

12. **Testing & validation**
    - Generate 10+ real-world protocol documents
    - User acceptance testing with REACH staff
    - Performance benchmarking

---

## Benefits of Quarto Migration

### Technical Benefits
1. **Version Control Friendly**: Plain text `.qmd` files vs binary DOCX
2. **Maintainability**: No more XML manipulation, cleaner conditional logic
3. **Flexibility**: Single source → multiple formats (DOCX, PDF, HTML, PPTX)
4. **Extensibility**: Quarto extension ecosystem (custom formats, filters)
5. **Debugging**: Easier to troubleshoot rendering issues
6. **Performance**: Parallel chunk execution, caching support

### User Benefits
1. **Interactive HTML output**: Clickable TOC, collapsible sections
2. **Web publishing**: Easy GitHub Pages / Quarto Pub deployment
3. **Reproducibility**: Self-contained `.qmd` + render script
4. **Modern syntax**: Familiar markdown for non-R users
5. **Rich features**: Callouts, tabs, accordions, mermaid diagrams

### Organizational Benefits
1. **Knowledge transfer**: Less specialized knowledge (no `officer` expertise)
2. **Template updates**: Easier for non-developers to edit
3. **Multi-language**: Python/Julia code chunks if needed
4. **Standardization**: Aligns with emerging Posit ecosystem standards

---

## Risks & Mitigations

| Risk | Mitigation |
|---|---|
| **Quarto dependency** | Pin quarto CLI version, vendor if critical |
| **DOCX output fidelity** | Extensive testing, fallback to `officer` if needed |
| **Learning curve** | Training sessions, comprehensive documentation |
| **Breaking changes** | Maintain parallel systems during transition |
| **Complex tables** | Some `flextable` features may not work in Quarto - test early |
| **Legacy template lock-in** | Keep `officer` system as option for 12 months |

---

## Decision Points for User Approval

### 1. **Hybrid vs Full Migration?**
   - **Option A**: Dual system (both `officer` and Quarto available)
   - **Option B**: Full migration with deprecation period
   - **Recommendation**: Option A for 6-12 months, then Option B

### 2. **Default Output Format?**
   - Keep DOCX as default or switch to HTML?
   - **Recommendation**: DOCX default, HTML as opt-in

### 3. **Breaking Changes Acceptable?**
   - Method signature changes for `generate_doc()`?
   - **Recommendation**: Add new `generate_quarto_doc()`, deprecate old method

### 4. **Timeline Constraints?**
   - 8-week timeline acceptable?
   - **Recommendation**: Phase 1-2 (4 weeks) for MVP, Phase 3-4 optional

### 5. **Testing Requirements?**
   - How many real protocols must render correctly before launch?
   - **Recommendation**: 10 diverse protocols across all survey types

---

## Open Questions

1. **Quarto version requirement**: Quarto 1.3+ or 1.4+?
2. **R package dependencies**: Add `quarto` to Imports or Suggests?
3. **Backward compatibility**: Support old `.Rds` protocol files?
4. **Custom Quarto extensions**: Need for REACH-specific Quarto format?
5. **Accessibility**: WCAG compliance for HTML outputs?

---

## Estimated Effort

- **Phase 1-2 (MVP)**: 60-80 hours (4 weeks part-time)
- **Phase 3-4 (Polish)**: 40-60 hours (4 weeks part-time)
- **Total**: 100-140 hours (8-10 weeks part-time or 4-5 weeks full-time)

---

## Next Steps

**Upon approval of this plan:**

1. Create feature branch `feature/quarto-document-generation`
2. Install Quarto CLI in development environment
3. Set up basic `inst/quarto/` structure
4. Build proof-of-concept with 1 section rendering correctly
5. Schedule checkpoint review after Phase 1 completion

**Approval needed for:**
- [ ] Overall migration strategy
- [ ] Timeline and resource allocation
- [ ] Hybrid vs full migration approach
- [ ] Output format defaults
- [ ] Any custom requirements not captured here

