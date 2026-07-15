# Roxygen2 Documentation Additions

# This file contains the missing roxygen2 documentation for:
# - R/class_document.R (25 methods)
# - R/class_protocol.R (5 methods)
# - R/class_survey_protocol.R (2 methods)
# - R/class_iphra_protocol.R (16 methods)

# ============================================================================
# R/class_document.R - Private Methods
# ============================================================================

#' @description Apply protocol schema from schema object.
#' @param obj officer::rdocx or officer::rpptx object.
#' @param schema Data frame with schema rows.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..generate_from_schema

#' @description Apply schema rows to document in handling order.
#' @param doc officer::rdocx or officer::rpptx object.
#' @param schema Data frame with schema rows.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..apply_protocol_schema_sections

#' @description Handle schema row with handling == "replace".
#' @param doc officer::rdocx or officer::rpptx object.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..handle_replace

#' @description Handle schema row with handling == "input".
#' @param doc officer::rdocx or officer::rpptx object.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..handle_input

#' @description Evaluate condition in schema row.
#' @param row Single-row schema data frame.
#' @return Logical indicating whether condition is met.
..evaluate_condition_row

#' @description Handle schema row with handling == "calculate".
#' @param doc officer::rdocx or officer::rpptx object.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..handle_calculate

#' @description Handle schema row with handling == "checkbox_replace".
#' @param doc officer::rdocx or officer::rpptx object.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..handle_checkbox_replace

#' @description Handle schema row with handling == "row_delete".
#' @param doc officer::rdocx or officer::rpptx object.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..handle_row_delete

#' @description Load protocol schema metadata with blank fallback.
#' @return Data frame with required schema columns.
..load_protocol_schema

#' @description Load protocol PowerPoint schema metadata with blank fallback.
#' @return Data frame with required schema columns.
..load_protocol_ppt_schema

#' @description Replace tag text in document.
#' @param doc officer::rdocx or officer::rpptx object.
#' @param old Old tag text.
#' @param new_val Replacement value.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..replace

#' @description Return required schema column names.
#' @return Character vector of required column names.
..schema_required_cols

#' @description Resolve active binding name from key.
#' @param name Binding key (possibly with leading dot).
#' @return Binding name or NULL if not found.
..resolve_active_binding_name

#' @description Read value from active binding or field.
#' @param binding_name Name of binding to read.
#' @return Binding value or NULL.
..read_active_binding

#' @description Insert image at tag location.
#' @param doc officer::rdocx document object.
#' @param tag Tag name.
#' @param image_value File path to image.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Updated document object.
..insert_image_at_tag

#' @description Dispatch function from schema row.
#' @param row Single-row schema data frame.
#' @param schema_kind Document kind ("docx" or "pptx").
#' @return Result from dispatched function or NULL.
..dispatch_schema_function

#' @description Extract schema row  for tag.
#' @param tag Tag name to lookup.
#' @return Single-row schema data frame or NULL.
..schema_row

#' @description Check if tag is missing from schema.
#' @param tag Tag name to check.
#' @return Logical indicating whether tag is missing.
..tag_is_missing_from_schema

#' @description Render checkbox symbol for condition.
#' @param doc officer::rdocx document object.
#' @param tag Tag name.
#' @param condition Logical condition value.
#' @return Updated document object.
..checkbox

#' @description Make Word paragraph XML element.
#' @param text Paragraph text content.
#' @param bold Logical indicating bold formatting.
#' @param space_before_pt Space before in points.
#' @param space_after_pt Space after in points.
#' @param font_size_pt Font size in points.
#' @return xml2::xml_document paragraph element.
..make_w_para

#' @description Replace tag in table cell.
#' @param doc officer::rdocx document object.
#' @param tag Tag name.
#' @param items List of items to insert.
#' @return Logical indicating success.
..replace_tag_in_cell

#' @description Remove unreplaced tags from document.
#' @param doc officer::rdocx document object.
#' @return Updated document object.
..remove_remaining_tags

#' @description Resolve condition flag from metadata.
#' @param condition Condition string or flag key.
#' @return Logical indicating whether condition is true.
..resolve_condition_flag

#' @description Determine if schema row should be applied.
#' @param row Single-row schema data frame.
#' @return Logical indicating whether to apply row.
..should_apply_schema_row

#' @description Replace tag across multiple text runs.
#' @param doc officer::rdocx document object.
#' @param tag Tag name.
#' @param new_val Replacement value.
#' @return Updated document object.
.._replace_across_runs

# ============================================================================
# R/class_protocol.R - Private Methods
# ============================================================================

#' @description Synchronize framework objective and indicator catalogs.
#' @return Invisibly returns NULL.
..sync_framework_catalog_fields

#' @description Synchronize tool indicator catalogs from tools.
#' @return Invisibly returns NULL.
..sync_tool_indicator_catalog_fields

#' @description Build objective metadata catalog from schema.
#' @param schema Data frame with objective schema.
#' @return Named list of objective metadata by objective code.
..build_objective_catalog

#' @description Build objective catalog for a specific tool.
#' @param fw_schema Framework master objectives schema.
#' @param indicator_codes Character vector of indicator codes from tool.
#' @return Named list of objective metadata keyed by objective code.
..build_tool_objective_catalog

#' @description Build indicator metadata catalog from schema.
#' @param schema Data frame with objective/indicator schema.
#' @return Named list of indicator metadata by indicator code.
..build_indicator_catalog

# ============================================================================
# R/class_survey_protocol.R - Private Methods
# ============================================================================

#' @description Synchronize sampling state from sample object.
#' @return Invisibly returns NULL.
..sync_sampling_state

#' @description Synchronize sampling frame strata state.
#' @return Invisibly returns NULL.
..sync_sample_frame_state

# ============================================================================
# R/class_iphra_protocol.R - Private Methods
# ============================================================================

#' @description Return default IPHRA template filenames.
#' @return Character vector of template filename candidates.
..default_template_filenames

#' @description Resolve metadata key from schema tag.
#' @param tag Schema tag name (with @ prefix).
#' @return Normalized metadata key.
..schema_metadata_key

#' @description Get metadata value for schema tag.
#' @param tag Schema tag name (with @ prefix).
#' @return Metadata value or NULL.
..schema_metadata_value

#' @description Check if tool with given role exists.
#' @param role Tool role name (e.g., "household", "kii_community").
#' @return Logical indicating tool presence.
..has_tool_role

#' @description Extract sample table from nested sample object.
#' @return Data frame sample table or NULL.
..sample_table_from_nested

#' @description Get sampling methods used across strata.
#' @return Character vector of unique sampling methods.
..sample_methods_used

#' @description Check if any sampling method matches given set.
#' @param methods Character vector of methods to check.
#' @return Logical indicating any method match.
..sample_has_any_method

#' @description Get strata names using specific sampling method.
#' @param method Sampling method name.
#' @return Logical indicating whether method is used in any strata.
..strata_names_for_method

#' @description Check if household tool has any indicator.
#' @param indicator_codes Character vector of indicator codes.
#' @return Logical indicating any indicator match.
..household_has_any_indicator

#' @description Initialize conditional metadata structure.
#' @return Invisibly returns NULL.
..initialize_conditional_metadata

#' @description Extract condition keys from schema.
#' @return Character vector of condition keys.
..condition_keys_from_schema

#' @description Synchronize sampling conditional metadata flags.
#' @return Invisibly returns NULL.
..sync_sampling_conditional_metadata

#' @description Resolve conditional flag value from schema tag.
#' @param tag Schema tag name (with @ prefix).
#' @return Logical indicating flag value.
..schema_flag_from_tag

#' @description Load tool XLSForm from file path.
#' @param tool Tool object to populate.
#' @param path File path to XLSForm.
#' @return Invisibly returns NULL.
..load_tool_from_path

#' @description Load IPHRA protocol schema from resource file.
#' @return Invisibly returns NULL.
..load_protocol_schema

#' @description Normalize schema tag names.
#' @param schema Data frame with schema rows.
#' @return Data frame with normalized tags.
..normalize_schema_tags
