# Coding Conventions in public_health_resources

## Purpose

This document defines the **expected coding standard** for contributors to `public_health_resources` (`phr`).  
When adding or changing code, follow these rules unless there is a clear, documented reason not to.

---

## 1) R6 Design Standard: Public vs Private vs Active Bindings

### Public: what must be exposed

Use `public` for:
- Core state that users and downstream methods must read (`raw_data`, `clean_data`, `metadata`, etc.)
- Stable user-facing operations (`validate()`, `standardize()`, `clean()`, `run_quality_checks()`)
- Explicit accessors and mutators (`get_*()`, `set_*()`, `resolve_*()`)
- Extension hooks meant for subclasses (`pre_*`, `post_*`)

Do **not** place helper implementation details in public unless subclass override is required.

### Private: what must be encapsulated

Use `private` for:
- Internal implementation details that should not be part of the external API
- Low-level helper routines used only inside one class
- Operations that should not be overridden by subclasses
- Internal document/schema manipulation steps that are orchestration internals

If a function is implementation-only and not intended for subclass specialization, it belongs in `private`.

### Active bindings: computed/synchronized interface fields

Use `active` when a member should behave like a field but is computed or synchronized:
- Derived values that should be accessed like properties
- State synchronization points where read/write behavior needs controlled side effects
- Values that should not be stored redundantly

Do **not** use active bindings for expensive work that surprises users on simple field access.

### Concrete placement guide

Use this decision order:
1. Is this user-facing behavior? → **public**
2. Is this internal-only logic with no subclass contract? → **private**
3. Is this a computed/synchronized property-like interface? → **active**
4. Is subclass override expected? → **public hook** (`pre_*`/`post_*`), not private

---

## 2) Parent vs Subclass Responsibilities

### What belongs in parent classes

Parent classes must contain:
- Shared lifecycle orchestration
- Shared validation and state-transition skeleton
- Common contracts and method signatures used by subclasses
- Generic defaults that are safe across domains

Parent classes should avoid domain-specific assumptions.

### What belongs in subclasses

Subclasses must contain:
- Domain-specific constraints and defaults
- Domain-specific schema extensions and checks
- Specialized calculations and transformations tied to that subclass data context

If logic only makes sense for one subclass dataset/domain, keep it in that subclass.

---

## 3) Standard Use of `pre_*` / `post_*` Hooks vs Main Functions

### Main function responsibilities

Main methods (e.g., `validate()`, `standardize()`, `clean()`) should:
- Define the authoritative execution pipeline
- Enforce ordering and critical guards
- Handle state updates and finalization consistently

### Hook responsibilities

`pre_*` hooks should contain:
- Setup and prerequisite preparation
- Domain-specific checks needed **before** core pipeline steps

`post_*` hooks should contain:
- Domain-specific final checks or enrichment
- Outputs that depend on completed core pipeline steps

Hooks should not re-implement the parent pipeline.  
If a subclass needs a new required stage, add a clear hook call point in the parent pipeline rather than bypassing it.

---

## 4) Error Handling Standard (`phr_try` vs `phr_try_step`)

### Use `phr_try` for operation boundaries

Wrap top-level method operations in `phr_try` with clear `origin` and intended `on_error` behavior:
- `"abort"` for critical failures
- `"warn"` for recoverable flows
- `"return"` for structured pass-through behavior

### Use `phr_try_step` for nested, stepwise pipelines

Inside a multi-step operation, use `phr_try_step` for each meaningful step, then short-circuit with `phr_failed()`:
- Improves context quality in error messages
- Preserves step-level traceability
- Keeps large pipelines maintainable

### Required pattern

- One outer `phr_try` per public operation boundary
- `phr_try_step` for major internal phases in long workflows
- Always provide meaningful `origin`/`step` text

---

## 5) Methods vs `utils_*` Functions

### Define as a class method when:
- Logic is specific to a class’s state or lifecycle
- It relies on `self$...` fields or class contracts
- It is part of the class API or extension points

### Define in `utils_*` when:
- Logic is reused across multiple classes/files
- It does not require class-specific internal state
- It represents package-level reusable behavior (validation helpers, formatting helpers, shared computations)

Rule of thumb:  
If the function should work independently of one class instance, it belongs in `utils_*`.

---

## 6) Accessor Methods for Nested Objects

When interacting with nested objects (e.g., framework/tool/log/sampling objects):
- Prefer explicit accessor methods over direct deep field traversal in external call sites
- Keep navigation through nested structures centralized in class methods
- Validate object presence/type before delegating
- Return clear, predictable shapes from accessors

This keeps class boundaries stable and reduces cross-object coupling.

---

## 7) Roxygen2 Documentation Standard

### Exported functions/classes

Must include:
- `@description`
- `@param` for each input
- `@return`
- `@export` (when public API)

### R6 classes

Must include:
- Class-level description
- `@field` entries for important fields
- Method-level roxygen for public methods

### Internal helpers

Use:
- `@keywords internal` and/or `@noRd`

Documentation should describe intent and contract, not just restate code.

---

## 8) `cloneable = FALSE` Default

Set `cloneable = FALSE` by default for R6 classes unless cloning is explicitly required.  
Enable cloning only with a clear use case and documented reason.

This reduces accidental duplication of heavy objects and makes object lifecycle expectations explicit.

---

## 9) Return Consistency: `invisible(self)`

For mutating methods that are primarily side-effect operations, return `invisible(self)` to support chaining and a consistent API style.

Use other return types only when the method’s primary contract is to return data/results.

Expected convention:
- Mutate object state → `invisible(self)`
- Query data/result → explicit value return

---

## 10) Naming Conventions (Consolidated)

This section is the canonical naming standard.

### Variables and columns

- Standard variable names: `snake_case` (e.g., `household_id`, `respondent_age`)
- Select-multiple generated dummy columns: period separator (e.g., `skills.reading`, `skills.other`)
- Text “other” columns from source data: underscore separator (e.g., `skills_other_text`)

### Functions

- Function names: `snake_case`
- Exported package-level functions: `phr_*` prefix for package-scoped clarity
- Internal helpers in utility files: leading dot allowed for internal-only functions

### R6 classes

- Class names: `PascalCase` (e.g., `Data`, `HouseholdData`, `Protocol`, `SurveyProtocol`)

### Files

- R source files: `snake_case` with existing class/utils patterns (e.g., `class_data.R`, `utils_validators.R`)
- Test files: `test-` prefix plus source-family naming (e.g., `test-class_data.R`, `test-utils_errors.R`)
- Documentation files: `snake_case` or existing docs folder conventions

---

## Practical Review Checklist for New Code

Before submitting:
- Is each method/function in the right place (public/private/active/utils)?
- Are parent/subclass boundaries respected?
- Are hooks used for extension instead of pipeline duplication?
- Is error handling using `phr_try` / `phr_try_step` appropriately?
- Are nested objects accessed through stable accessors/delegation methods?
- Is roxygen complete and consistent?
- Is `cloneable` explicitly justified if enabled?
- Do mutating methods return `invisible(self)`?
- Does naming follow the consolidated conventions above?

