# Coding Conventions Standard

> Version 0.2 (Draft)

## Purpose

This document defines the **normative coding standard** for a family of R packages. Unless an approved exception is documented, contributors **MUST** follow these conventions.

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as described in RFC 2119. These imperatives MUST be used sparingly and only where required for interoperability, safety, or to prevent harmful behavior.

------------------------------------------------------------------------

# 1. Scope

This document contains ecosystem-wide standards for phr related repositories. Package-specific conventions (such as function prefixes like `phr_*`) belong in package-level appendices.

------------------------------------------------------------------------

# 2. Terminology

The following terms are used throughout this standard and are intended to have the meanings defined below.

- **Package** – An R package that provides a cohesive set of functions, classes, documentation, and tests.
- **Function** – A reusable unit of code that performs a specific task and may be called directly by users or other code.
- **Method** – A function associated with a class and executed on an object instance (for example, an R6 method).
- **Class** – A blueprint that defines an object's structure, state, and behavior.
- **Object** – An instantiated class containing data and methods that operate on that data.
- **R6 Class** – An object-oriented class implemented using the `R6` package that supports encapsulation, inheritance, and mutable state.
- **Public Method** – A method intended to be called by users or other components as part of the supported API.
- **Private Method** – An internal method used only within a class implementation and not intended as part of the public API.
- **Active Binding** – A property-like interface that computes, validates, or synchronizes values when accessed.
- **Parent Class** – A base class that defines shared contracts, behavior, and workflows for one or more subclasses.
- **Subclass** – A class that inherits from a parent class and extends or specializes its behavior.
- **Hook** – A designated extension point (such as `pre_*` or `post_*`) intended for customization by subclasses.
- **API (Application Programming Interface)** – The set of supported public functions, methods, classes, and behaviors exposed to users.
- **Utility Function** – A reusable function that is independent of object state and may be shared across multiple classes or modules.
- **Helper Function** – A function that supports a larger operation, typically with a narrower or more implementation-specific purpose.
- **Internal Helper** – A helper function that is not part of the public API and is intended only for package internals.
- **Validator** – A function that checks inputs, data structures, or business rules and reports failures in a standardized way.
- **Workflow** – A sequence of related operations that move an object or dataset through a defined lifecycle.
- **Step** – A meaningful phase within a larger workflow used to provide traceable execution and error-reporting context.
- **Contract** – The documented expectations, inputs, outputs, and behavior that consumers of a function or class can rely upon.
- **Public API** – The subset of package functionality intended for direct use and supported across releases.


# 3. Naming Conventions

This section sets a consistent naming standard for various code elements.

### Packages

Package names SHOULD use lowercase snake_case and be concise, descriptive, and representative of the package's primary responsibility. Names should avoid abbreviations unless they are already well established within the organization or domain. Package names should remain stable over time to minimize disruption to downstream dependencies and documentation.

### Variables and columns

Variables and data frame columns MUST use snake_case naming (for example, household_id and respondent_age). Select-multiple dummy variables generated from source instruments SHOULD use a period separator between the parent field and option value (for example, skills.reading and skills.other) to clearly preserve the relationship to the originating question. Text fields that capture free-text "other" responses SHOULD use an underscore suffix convention (for example, skills_other) to maintain consistency with source data structures and downstream processing workflows.

Where able, thematic or sectoral specific variables should have a relevant prefix (e.g. fsl_, wash_) to help easily identify accountability lines for their behaviours and outputs.

### Functions and Methods

Function names MUST use snake_case and should describe the action performed as clearly as possible. Within R6 classes, active binding methods SHOULD use a leading period (for example, .tool_household()) to indicate they are for pre-defined returns of current state information. Private methods SHOULD use double leading period to indicate they are internal helpers (e.g. ..access_nested_sample_table).

Function and method arguments should ideally have suffixes indicating the type of input, such as a column name, value name, or other type of input (e.g. health_barriers_col = "main_health_barriers", quality_barriers_val = "Poor Quality")

### R6 classes

R6 class names MUST use PascalCase and should be named using nouns that represent domain concepts, workflows, or managed objects. Class names should be descriptive, stable, and consistent with existing ecosystem terminology. Sub-class names MAY also reflect their inherited class to make the inheritence chains clear. Examples include Data, HouseholdData, Protocol, and SurveyProtocol.

### Files

File names SHOULD use snake_case and reflect the primary responsibility of the code they contain. R source files should follow established project patterns such as class_* for R6 classes and utils_* for reusable utility modules (for example, class_data.R and utils_validators.R). Test files MUST use the test- prefix and should mirror the source file or functional area being tested (for example, test-class_data.R and test-utils_errors.R). Documentation files SHOULD use snake_case naming or follow established conventions within the repository's documentation structure.

# 4. Documentation

Documentation is part of the public contract of a package and MUST be maintained alongside code changes. All exported functions, R6 classes, public methods, exported datasets, and other user-facing objects MUST be documented using `roxygen2`. Documentation should explain purpose, behavior, inputs, outputs, assumptions, and side effects. Documentation should describe intent and expected usage rather than simply restating implementation details or code structure.

**Changes to documented public API behavior MUST be accompanied by an appropriate Semantic Versioning increment.**

### Public Functions

All exported functions MUST include complete `roxygen2` documentation sufficient for a user to understand how and when to use the function. At a minimum, exported functions should include:

- `@title`
- `@description`
- `@param` for every argument
- `@return`
- `@examples` where practical
- `@export`

Parameter documentation should describe valid values, expected formats, defaults, and important constraints. Return documentation should clearly describe the object type, structure, and meaning of returned values.


### R6 Classes and Methods

R6 classes SHOULD be documented as user-facing objects rather than collections of implementation details. Class documentation should explain the purpose of the class, its role within a workflow, important public fields, and the expected object lifecycle. Public methods should be documented with the same rigor as exported functions, including parameters, return values, side effects, and state changes where applicable.

When documenting R6 classes:

- Document the class itself with a dedicated class-level block.
- Document all public methods intended for user interaction.
- Document active bindings when they form part of the public interface.
- Focus on observable behavior and class contracts rather than internal implementation.

Private fields and private methods generally SHOULD NOT be documented in user-facing reference materials.


### Examples

Documentation SHOULD include concise, executable examples whenever practical. Examples should demonstrate normal usage patterns and help users understand expected inputs and outputs. Examples must remain lightweight, deterministic, and suitable for execution during package checking. Long-running operations, external dependencies, and environment-specific behavior should be avoided or appropriately guarded.


### Internal Functions

Functions that are not part of the public API SHOULD be marked as internal and excluded from user documentation where appropriate. Internal documentation should still be sufficient for maintainers to understand intent and usage.

For internal helpers, use:

- `@keywords internal` and/or
- `@noRd`

Internal documentation should explain why the helper exists, any important assumptions, and its relationship to the broader implementation.


### NAMESPACE Management

Package exports and imports MUST be managed through `roxygen2` directives rather than manual editing of the `NAMESPACE` file. The generated `NAMESPACE` file should be treated as a build artifact and automatically regenerated when documentation changes.

Use appropriate directives such as:

- `@export`
- `@exportClass` (when applicable)
- `@importFrom`
- `@importClassesFrom`
- `@rawNamespace` only in exceptional cases

Dependencies SHOULD be imported as narrowly as possible, favoring `@importFrom` over broad package imports to maintain namespace clarity and reduce unnecessary coupling.

For internal GitHub‑based dependencies declared via Remotes:, packages MUST pin to a specific tag or commit rather than an unversioned branch, except during active co‑development.

### Documentation Quality

Documentation MUST remain synchronized with code behavior. Changes to function signatures, return values, public fields, class behavior, or package interfaces MUST be accompanied by corresponding documentation updates. Documentation should be reviewed as part of code review and treated as a required component of any public API change.

# 5. Formatting & Linting


All repositories SHOULD follow a common formatting and linting standard to promote consistency, readability, and maintainability across the ecosystem. Source files MUST use UTF-8 encoding and SHOULD use LF line endings where practical. Naming conventions defined elsewhere in this standard MUST be followed, including `snake_case` for functions, methods, variables, and files, and `PascalCase` for R6 classes. Code formatting SHOULD be applied automatically using a shared `styler` configuration, and static analysis SHOULD be enforced through a shared `lintr` configuration. Non-base package functions SHOULD be explicitly namespaced (for example, `dplyr::mutate()`) unless imported through approved package-level conventions. Formatting and linting requirements should be enforced through automated tooling and CI wherever possible to ensure consistent code style across repositories.

------------------------------------------------------------------------

# 6. Error Handling

Errors are part of the package's public contract and SHOULD be handled consistently across all repositories. Functions and methods MUST validate inputs early, fail predictably, and provide actionable messages that help users understand both the cause of the failure and, where possible, how to correct it. Error messages should describe the problem, not merely report the symptom, and should include relevant contextual information such as the operation, object, or workflow stage involved.

Public operations SHOULD define a single top-level error boundary using `phr_try()`, or `tryCatch()` for short steps that may not be relevant for showing messages in Shiny environments. This boundary is responsible for capturing unexpected failures, adding workflow context, and determining whether errors should abort execution, emit warnings, or return structured failure objects. Within larger workflows, meaningful processing phases SHOULD be wrapped using `phr_try_step()` to preserve step-level traceability and improve debugging. All operation boundaries and steps SHOULD provide clear and stable `origin` and `step` identifiers so that failures can be traced consistently across logs, console output, and Shiny applications.

Conditions should be communicated using the most appropriate mechanism. Input validation SHOULD be performed as early as possible using the standardized phr_validate_*() functions provided by the ecosystem (for example, type validation, schema validation, column validation, date validation, and other common contract checks). Contributors SHOULD prefer these shared validators over ad hoc conditional checks to ensure consistent behavior, messaging, and user experience across packages. Where a required validation is not covered by an existing validator, phr_assert() SHOULD be used to enforce preconditions and business rules. Unrecoverable failures SHOULD use phr_error(), recoverable conditions SHOULD use phr_warning(), and informational status updates SHOULD use phr_message(). Expected conditions should be detected and handled explicitly rather than relying on generic error trapping. Error handling code should preserve the original failure context whenever possible and avoid silently suppressing exceptions unless an alternative behavior is intentionally defined and documented.

Recommended practices:

- Public functions and methods SHOULD follow a consistent workflow: validate inputs using applicable `phr_validate_*()` functions, apply `phr_assert()` for domain-specific rules not covered by the standard validation framework, execute business logic, and handle unexpected failures through `phr_try()` operation boundaries.
- Existing `phr_validate_*()` functions SHOULD be preferred over ad hoc validation logic to ensure consistent behavior, messaging, and user experience across packages.
- Public operations SHOULD be wrapped in a single top-level `phr_try()` boundary, with `phr_try_step()` used for significant workflow phases where additional error context would improve debugging and traceability.
- Include a corrective `hint` whenever a clear user action can resolve the issue.
- Error messages SHOULD identify what failed, where it failed, and why it failed.
- Warnings SHOULD indicate any impact on outputs, assumptions, or downstream processing.
- Avoid generic messages such as "Error occurred" or "Invalid input" when more specific context can be provided.
- Errors SHOULD NOT be silently caught, discarded, or hidden from callers.
- Tests SHOULD verify expected validation failures, assertions, errors, warnings, and other failure paths.


------------------------------------------------------------------------

# 7. R6 Design Standard

This ecosystem adopts R6 because it manages complex, stateful workflows that progress through multiple stages, such as protocol development, data collection, validation, analysis, and reporting. R6 enables related data and behavior to be encapsulated within reusable objects, improving maintainability, reducing parameter passing, and supporting clear object lifecycles. Inheritance and composition allow common functionality to be shared across packages while enabling domain-specific extensions with minimal code duplication. Stateless, reusable computations should continue to be implemented as standalone functions, using R6 primarily for workflow orchestration and object management.

## Bindings

The following section elaborates on when public, private, and active bindings should be used. 

In general, use this decision order:
1. Is this field or method user-facing behavior or behaviour that needs to be linked to Shiny application actions? → **public**
2. Is this field or method internal-only logic with no subclass contract? → **private**
3. Is this a computed value or dataframe, or value reflecting the current state of the object for reporting? → **active**

### Public

Use `public` for:
- Core state that users and downstream methods must read (`raw_data`, `clean_data`, `metadata`, etc.)
- Stable user-facing operations (`validate()`, `standardize()`, `clean()`, `run_quality_checks()`)
- Explicit accessors and mutators (`get_*()`, `set_*()`, `resolve_*()`)
- Extension hooks meant for subclasses (`pre_*`, `post_*`)

Do **not** place helper implementation details in public unless subclass override is required.

### Private

Use `private` for:
- Core data fields. These should be created during initilization, only modified through internal private helpers which are called from public accessor methods.
- Internal helpers used to modify fields implementation details that should not be part of the external API
- Low-level helper routines used only inside one class
- Operations that should not be overridden by subclasses

If a function is implementation-only and not intended for subclass specialization, it belongs in `private`.

### Active bindings

Use `active` when a method should return a computed or synchronized value or object:
- Derived values that should be accessed like properties (e.g. is a specific tool included in the object, a custom dataframe from current state information needed for reporting, etc.)
- Values that should not be stored, only returned.
- Current state information, values, dataframes, or formatted objects that need to be passed to Quarto documents.

Do **not** use active bindings for modifying fields within objects or nested objects. Only private methods should do this. Active bindings with specific reporting purposes should be defined in sub-classes.

### Parent vs Subclass Responsibilities

Parent classes define the common contract and lifecycle for a family of related objects. They SHOULD contain shared workflow orchestration, validation frameworks, state-transition logic, extension hooks, and other behavior that is consistent across multiple domains or datasets. Parent classes should establish the authoritative execution pipeline and provide safe, generic defaults that can be reused broadly. To remain reusable, parent classes MUST avoid domain-specific assumptions, dataset-specific rules, and specialized business logic that only applies to a subset of implementations.

Subclasses extend the parent contract to implement domain-specific behavior. They SHOULD contain specialized validation rules, schema requirements, defaults, calculations, transformations, and other logic that is unique to a particular dataset, workflow, or subject area. If a piece of logic would not reasonably apply to other implementations of the parent class, it belongs in the subclass. As a general rule, place shared behavior in the highest appropriate parent class and reserve subclasses for specialized behavior that customizes or extends the common workflow.

**Backward‑incompatible changes to public R6 methods, required fields, or lifecycle contracts MUST trigger a MAJOR version increment under Semantic Versioning.**

## Extension Hooks

Subclasses SHOULD extend parent class behavior through hooks or class-specific methods rather than replacing core parent workflows. Authoritative lifecycle methods such as `validate()`, `standardize()`, `clean()`, and `generate_doc()` should remain responsible for defining execution order, enforcing critical guards, managing state transitions, and ensuring consistent finalization. This preserves a common workflow across implementations while allowing controlled customization where needed.

Hooks provide designated extension points for domain-specific behavior. `pre_*` hooks SHOULD be used for prerequisite preparation, configuration, and validations that must occur before core processing, while `post_*` hooks SHOULD be used for enrichment, post-processing, and validations that depend on completed workflow stages. Hooks should complement the parent pipeline rather than re-implement it. If subclasses require an additional mandatory processing stage, the parent class SHOULD be extended with an explicit hook point so that all implementations continue to follow a consistent lifecycle.

## Methods vs Utility Functions

Reusable logic that is independent of object state SHOULD live in
utility modules.

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

------------------------------------------------------------------------

# 7. Testing Standards

Automated testing is a required component of package quality and long-term maintainability. Tests should serve as executable specifications of expected behavior, providing confidence that refactoring, dependency updates, and feature additions do not introduce regressions. Contributors SHOULD write tests concurrently with implementation rather than treating testing as a post-development activity.

## Unit Testing Requirements
All exported functions, public R6 methods, and public class behaviors MUST have unit tests. Internal helpers that contain non-trivial logic SHOULD also be tested directly or indirectly through higher-level behavior tests.

At a minimum, test suites SHOULD verify:

- Expected ("happy path") behavior
- Invalid or malformed inputs
- Boundary and edge cases
- Empty inputs
- Missing values (NA, NULL, and equivalent representations where applicable)
- Error, warning, and message conditions
- Returned object structure, classes, and key attributes

Tests should validate observable behavior and public contracts rather than implementation details. Refactoring internal code should not require widespread test changes unless public behavior has intentionally changed.

### R6 Testing
For R6 classes, tests SHOULD focus on public interfaces, state transitions, and lifecycle behavior. Tests should verify that objects are initialized correctly, methods produce expected state changes, and extension points behave according to documented contracts.
Private methods SHOULD generally be tested indirectly through public functionality. Direct testing of private implementation details should be avoided unless necessary to validate complex or safety-critical logic.

### Deterministic Tests
Tests MUST be deterministic and reproducible across environments.

- Random processes MUST use set.seed().
- Tests MUST NOT depend on execution order.
- Tests SHOULD NOT depend on external services, network availability, local machine configuration, or manually maintained files.
- Time-dependent logic SHOULD use mocked or controlled timestamps where practical.

A test suite should produce identical results when run repeatedly on a developer workstation or within CI.

### Test Organization
Tests MUST use the testthat framework unless an approved exception is documented.
Test files should mirror the source structure where practical. Related functionality should be grouped within a common test file, and test descriptions should clearly communicate the expected behavior being verified. Tests should remain focused and concise, with each expectation validating a single behavioral concern whenever practical.

### Coverage Expectations
Code coverage is a useful indicator of test completeness but SHOULD NOT be treated as the primary measure of quality. Meaningful behavioral tests are preferred over coverage-driven tests that provide little validation value.
New features, public APIs, and bug fixes SHOULD include corresponding tests. Significant areas of untested production code should be addressed as part of ongoing maintenance work.

------------------------------------------------------------------------

# 8. Dependency Policy

Dependencies should be managed deliberately to promote maintainability, reproducibility, and long-term stability across the ecosystem. Each package should depend only on libraries that provide clear value, with preference given to well-established, actively maintained packages that are widely adopted within the R community. Contributors should periodically review dependencies to remove unused packages, replace deprecated or unmaintained libraries, and minimize unnecessary dependency chains.

Package dependencies should follow these principles:

- Prefer Imports over Depends unless attaching a package is explicitly required.
- Select minimum package versions intentionally based on required functionality, rather than always specifying the latest available version.
- Explicitly namespace non-base package functions (e.g., dplyr::mutate()) to improve code clarity and reduce namespace conflicts.
- Remove unused dependencies promptly and avoid introducing dependencies for functionality that can be reasonably implemented within the package.
- Evaluate new dependencies for maintenance status, licensing, community adoption, and long-term sustainability before adding them.
- For internal GitHub Remotes dependencies, pin all references to a specific tag or commit (e.g., org/pkg@v1.2.0) rather than an unpinned branch.
- Use unpinned Remotes only for active co‑development on shared feature branches, and bump pinned versions deliberately through reviewed changes so updates are explicit and auditable.

**Version numbers MUST follow Semantic Versioning (MAJOR.MINOR.PATCH).  
Backward‑incompatible changes to the public API MUST trigger a MAJOR version increment.  
Backward‑compatible additions or deprecations MUST trigger a MINOR increment.  
Backward‑compatible bug fixes MUST trigger a PATCH increment.  
Released versions MUST NOT be modified; fixes MUST be released as new versions.**

# 9. R Environment Management

Development environments should be managed using renv to ensure reproducible package development and consistent dependency versions across contributors and continuous integration environments. Each repository should maintain an up-to-date renv.lock file that accurately reflects the project's development environment. Changes to package dependencies should be accompanied by an updated lockfile, and contributors should restore the project environment using renv::restore() before development when appropriate. The lockfile should be committed to version control, while the project library itself should remain excluded from source control.

When packages within the ecosystem depend on one another, developers should avoid unnecessarily constraining internal package versions during active development. Stable releases should specify appropriate minimum version requirements, while development workflows should be coordinated to ensure compatibility across repositories before release. Dependency updates that introduce breaking changes should follow the ecosystem's API stability and release management policies.

------------------------------------------------------------------------

# 10. CI Enforcement

Continuous Integration (CI) provides an automated quality gate that ensures all packages within the ecosystem consistently adhere to established coding standards before changes are merged. Wherever possible, conventions should be enforced through automated tooling rather than manual review, allowing reviewers to focus on architectural decisions, code quality, and maintainability. All repositories within the ecosystem should implement a common CI workflow to provide a consistent development experience and quality baseline.

CI pipelines should, at a minimum, verify the following:

- Code formatting and style compliance (styler)
- Static code analysis and linting (lintr)
- Successful package build and validation (R CMD check)
- Unit and integration test execution (testthat)
- Documentation generation and validation
- Test coverage reporting (covr), where applicable
- Dependency integrity and package installation

Conventions related to software architecture, object-oriented design, naming clarity, API design, and other decisions requiring engineering judgment should be evaluated through peer review rather than automated checks.

**CI SHOULD verify that version increments comply with Semantic Versioning rules when public API changes are detected.**

------------------------------------------------------------------------

# 11. Code Review Standard

All substantive code changes should be reviewed by at least one contributor other than the original author before being merged. Code reviews should focus on ensuring consistency with the ecosystem's architectural principles, coding conventions, and maintainability standards. Automated CI checks should pass prior to approval, allowing reviewers to concentrate on design decisions and overall code quality rather than formatting or routine validation.

Reviewers should verify that the following have been appropriately addressed:

- Compliance with the documented coding conventions and architectural standards.
- Appropriate class design, including public/private interfaces, inheritance, and object responsibilities.
- Clear, consistent naming and adequate documentation for new or modified code.
- Sufficient testing for new functionality, bug fixes, and edge cases.
- Documentation of any changes to the public API and any approved exceptions to the coding standards.
- Successful completion of all required CI quality checks prior to merge.

**Reviewers MUST confirm that any change affecting the public API is accompanied by a correct Semantic Versioning increment.**
