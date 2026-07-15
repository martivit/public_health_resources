# Suppress "no visible binding for global variable" notes from R CMD CHECK.
# These column names are created and referenced within dplyr::mutate() calls
# where intra-mutate sequential evaluation prevents use of the .data pronoun,
# or are bare column names used in dplyr/ggplot2 NSE contexts across the package.
utils::globalVariables(c(

  # ----- utils_quality_analyisis_outputs.R (from PR #63) -----
  "estimate",
  "estimate_low",
  "estimate_upp",
  "estimate_pct",
  "estimate_val",
  "lower_ci",
  "upper_ci",
  "cumsum_num",
  "cumsum_den",

  # ----- utils_quality_analyisis_outputs.R (additional) -----
  # Donut / pie chart columns
  "pct",
  "ymax",
  "ymin",
  "label_pos",
  "label",
  # Stacked-bar chart columns
  "percentage",
  "weighted_n",
  "variable",
  "variable_index",
  "group_var",
  "category",
  "x_var",
  "plot_group",
  # Data-quality summary table columns
  "group_penalty_sum",
  "group_max_penalty_sum",
  "check_group",
  "check_name",
  # Frequency / summary table columns
  "n",
  "total_n",
  "total_value",
  # Crosstab heatmap column
  "gradient_value",
  # Treemap / sankey columns
  "value",
  "stratum",
  "label_stats",
  # IYCF chart columns
  "age_group",
  # IYCF helper columns (intra-mutate and cross-mutate references)
  "any_food",
  "no_food",
  "no_liquid",
  "bf",
  # Frequency table display columns (capitalized column names)
  "Variable",
  "Value",
  "Unit",
  # Data-quality detail table columns
  "penalty",
  "max_penalty",
  "check_label",
  "group_total",

  # ----- utils_data_household_fsl.R -----
  # FCS weighted components (intra-mutate sequential references)
  "fcs_weight_cereal1",
  "fcs_weight_legume2",
  "fcs_weight_dairy3",
  "fcs_weight_meat4",
  "fcs_weight_veg5",
  "fcs_weight_fruit6",
  "fcs_weight_oil7",
  "fcs_weight_sugar8",
  # FCS score and category
  "fsl_fcs_score",
  "fsl_fcs_cat",
  # HHS intermediate numeric columns (intra-mutate sequential references)
  "hhs_nofoodhh_numeric",
  "hhs_nofoodhh_freq_numeric",
  "hhs_sleephungry_numeric",
  "hhs_sleephungry_freq_numeric",
  "hhs_alldaynight_numeric",
  "hhs_alldaynight_freq_numeric",
  # HHS component / score / category columns
  "fsl_hhs_comp1",
  "fsl_hhs_comp2",
  "fsl_hhs_comp3",
  "fsl_hhs_score",
  "fsl_hhs_cat",
  "fsl_hhs_cat_ipc",
  # RCSI weighted components (intra-mutate sequential references)
  "rcsi_lessquality_weighted",
  "rcsi_borrow_weighted",
  "rcsi_mealsize_weighted",
  "rcsi_mealadult_weighted",
  "rcsi_mealnb_weighted",
  "any_weighted_na",
  "fsl_rcsi_score",
  "fsl_rcsi_cat",
  # LCSI strategy columns (intra-mutate sequential references)
  "fsl_lcsi_stress_yes",
  "fsl_lcsi_stress_exhaust",
  "fsl_lcsi_stress",
  "fsl_lcsi_crisis_yes",
  "fsl_lcsi_crisis_exhaust",
  "fsl_lcsi_crisis",
  "fsl_lcsi_emergency_yes",
  "fsl_lcsi_emergency_exhaust",
  "fsl_lcsi_emergency",
  # LCSI temporary helper column (select removal)
  "complete_lcsi_inputs",

  # ----- utils_data_individual_nutrition.R -----
  # ECFIES columns (intra-mutate sequential references)
  "nut_ecfies_score",
  "nut_ecfies_cat",
  # MUAC columns (intra-mutate sequential references)
  "age_months",
  "sam_muac",
  "mam_muac",
  "gam_muac",
  "nut_muac_cat",
  "flag_muac_extreme",
  # MFAZ columns (added by zscorer / calculate_flags, referenced in subsequent mutate)
  "mfaz",
  "severe_mfaz",
  "moderate_mfaz",
  "global_mfaz",
  "flag_sd_mfaz",
  "nut_mfaz_cat",
  "nut_mfaz_cat_noflag",

  # ----- utils_data_individual_roster.R -----
  # Calculated date / age columns (cross-mutate references)
  "calc_date_birth_final",
  "calc_date_death_final",
  "calc_age_months",
  "calc_age_days",

  # ----- utils_data_household_wash.R -----
  # HWISE score columns (intra-mutate sequential references)
  "wash_hwise4_score",
  "wash_hwise12_score",
  # LPPD derived columns (intra-mutate sequential references)
  "liters_pppd",
  "liters_log",
  "liters_pppd_log",
  "wash_jmp_ladder_drinking_water_cat",

  # ----- utils_data_individual_death.R -----
  # Calculated date / person-time columns (cross-mutate references)
  "entry_date",
  "exit_date",
  "person_time",

  # ----- utils_data_water_container.R -----
  # Total litres column (cross-mutate reference)
  "wash_container_total_litres",

  # ----- utils_data_individual_woman.R -----
  # Intermediate MUAC classification column (intra-mutate reference)
  "muac_for_classification",

  # ----- utils_data_household_fsl.R (additional) -----
  # LCSI NA count column (intra-mutate reference)
  "lcsi_count_na",

  # ----- class_protocol.R / utils_sample_drawing.R / utils_protocol.R -----
  # Sampling output columns
  "sampled_psu",
  "allocated_sample",
  "Final_HH_Sample_Size",
  "General_HH_Sample_Size",
  "Ind_Sample_Size",
  "Ind_HH_Sample_Size",
  "Mort_Ind_Sample_Size",
  "Mort_PT_Sample_Size",
  "Mort_HH_Sample_Size",
  # Field plan output columns (written into existing strata table columns)
  "num_interview_per_enum_per_day",
  "num_days",
  "n_psu",
  "cluster_size"

))
