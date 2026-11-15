# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  incidence_rate_ratio = list(result_type = "incidence_rate_ratio"),
  propensity_score_coeficcients = list(result_type = "propensity_score_coeficcients"),
  propensity_scores = list(result_type = "propensity_scores"),
  summarise_sampling = list(result_type = "summarise_sampling"),
  summarise_standardised_mean_differences = list(result_type = "summarise_standardised_mean_differences"),
  cohort_exit = list(result_type = "cohort_exit"),
  gestational_time_distributions = list(result_type = "gestational_time_distributions")
  # kaplan_meier = list(result_type = "kaplan_meier")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  dplyr::mutate(cdm_name = dplyr::if_else(cdm_name == "SCIFI-PEARLv2", "SCIFI-PEARL", .data$cdm_name)) |>
  dplyr::filter(!grepl("miscarriage_codelist", additional_level)) |>
  dplyr::filter(!(grepl("miscarriage", additional_level) & cdm_name %in% c("CPRD GOLD", "SCIFI-PEARL"))) |>
  dplyr::filter(!(grepl("ectopic_pregnancy", additional_level) & cdm_name %in% c("SCIFI-PEARL"))) |>
  dplyr::filter(!(grepl("dysfunctional_labour", additional_level) & cdm_name %in% c("CPRD GOLD"))) |>
  dplyr::filter(!grepl("miscarriage_codelist", group_level)) |>
  dplyr::filter(!(grepl("miscarriage", group_level) & cdm_name %in% c("CPRD GOLD", "SCIFI-PEARL"))) |>
  dplyr::filter(!(grepl("ectopic_pregnancy", group_level) & cdm_name %in% c("SCIFI-PEARL"))) |>
  dplyr::filter(!(grepl("dysfunctional_labour", group_level) & cdm_name %in% c("CPRD GOLD")))

# rename cohorts (source_ cohorts are renamed through their population_ names)
cohortNameMap <- c(
  "population_objective_1" = "First-dose vaccinated vs. Unvaccinated",
  "population_objective_3" = "Boosted vs. Unboosted",
  "population_miscarriage_objective_1" = "First-dose vaccinated vs. Unvaccinated (Miscarriage)",
  "population_miscarriage_objective_3" = "Boosted vs. Unboosted (Miscarriage)",
  "population_preterm_labour_objective_1" = "First-dose vaccinated vs. Unvaccinated (Preterm delivery)",
  "population_preterm_labour_objective_3" = "Boosted vs. Unboosted (Preterm delivery)"
)
renameCohorts <- function(x) {
  for (old in names(cohortNameMap)) x <- gsub(old, cohortNameMap[[old]], x, fixed = TRUE)
  x
}
result <- result |>
  dplyr::mutate(group_level = renameCohorts(.data$group_level))

# capitalise exposure levels for display (exposed/comparator -> Exposed/Comparator)
result <- result |>
  dplyr::mutate(
    strata_name = gsub("\\bexposed\\b", "Exposed", .data$strata_name),
    strata_name = gsub("\\bcomparator\\b", "Comparator", .data$strata_name),
    strata_level = gsub("\\bexposed\\b", "Exposed", .data$strata_level),
    strata_level = gsub("\\bcomparator\\b", "Comparator", .data$strata_level)
  )

# nice display names for outcomes, exit reasons, and count estimates
outcomeNames <- unique(c(
  "acquired_hypothyroidism", "acute_conjunctivitis", "anaphylaxis", "antepartum_haemorrhage",
  "atopic_dermatitis", "bells_palsy", "blepharitis", "central_nervous_system_immune", "covid",
  "curvature_of_spine", "deep_vein_thrombosis", "disseminated_intravascular_coagulation",
  "dysfunctional_labour", "eclampsia", "encephalitis", "epidermoid_cyst", "epilepsy",
  "foot_pain", "fracture_of_humerus", "gallstone", "glaucoma", "guillain_barre_syndrome",
  "haemorrhagic_stroke", "hearing_loss", "hellp", "hemorrhoids", "immune_thrombocytopenia",
  "impacted_cerumen", "ingrowing_nail", "inguinal_hernia", "injury_of_abdomen",
  "injury_of_eye_region", "iron_deficiency_anemia", "irritable_bowel_syndrome",
  "ischaemic_stroke", "low_back_pain", "lump_in_right_breast", "maternal_death", "migraine",
  "mild_depression", "miscarriage", "myocardial_infarction", "myocarditis_or_pericarditis",
  "narcolepsy", "open_wound_of_lower_leg", "osteoporosis", "otitis_externa", "parasomnia",
  "pityriasis_versicolor", "polyp_of_colon", "postpartum_endometritis", "postpartum_haemorrhage",
  "preterm_labour", "pulmonary_embolism", "raynaud_s_disease", "rectal_hemorrhage", "salmonella",
  "sprain_of_ankle", "stillbirth", "transverse_myelitis", "tts", "urinary_incontinence",
  "urinary_tract_infectious_disease", "venous_thromboembolism", "vitamin_d_deficiency",
  "wax_in_ear_canal"
))
sentenceCase <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
outcomeNameMap <- stats::setNames(
  sentenceCase(gsub("_", " ", outcomeNames)),
  outcomeNames
)
outcomeNameMap[outcomeNames] <- dplyr::recode(
  outcomeNames,
  "bells_palsy" = "Bell's palsy",
  "covid" = "COVID-19",
  "guillain_barre_syndrome" = "Guillain-Barré syndrome",
  "hellp" = "HELLP syndrome",
  "myocarditis_or_pericarditis" = "Myocarditis or pericarditis",
  "raynaud_s_disease" = "Raynaud's disease",
  "tts" = "Thrombosis with thrombocytopenia syndrome (TTS)",
  "urinary_tract_infectious_disease" = "Urinary tract infection",
  "vitamin_d_deficiency" = "Vitamin D deficiency",
  .default = unname(outcomeNameMap[outcomeNames])
)
exitReasonMap <- c(
  "overall" = "Overall",
  "counterpart_censoring" = "Counterpart censored (matched pair)",
  "covid_infection" = "COVID-19 infection",
  "covid_infection; next_covid_vaccine" = "COVID-19 infection; next COVID-19 vaccine dose",
  "date_of_death" = "Death",
  "next_covid_vaccine" = "Next COVID-19 vaccine dose",
  "observation_end" = "End of observation"
)
variableNameMap <- c(
  "number_records" = "Number records",
  "number_subjects" = "Number subjects",
  "excluded_records" = "Excluded records",
  "excluded_subjects" = "Excluded subjects"
)
recodeTokens <- function(x, map) {
  parts <- strsplit(x, " &&& ", fixed = TRUE)
  vapply(parts, function(p) paste(dplyr::recode(p, !!!map), collapse = " &&& "), character(1))
}
result <- result |>
  dplyr::mutate(
    group_level = dplyr::recode(.data$group_level, !!!outcomeNameMap),
    additional_level = recodeTokens(.data$additional_level, outcomeNameMap),
    additional_level = recodeTokens(.data$additional_level, exitReasonMap),
    variable_name = dplyr::recode(.data$variable_name, !!!variableNameMap)
  )

attr(result, "settings") <- omopgenerics::settings(result) |>
  dplyr::mutate(
    table_name = dplyr::case_when(
      table_name == "aesi" ~ "AESI",
      table_name == "covariates_1" ~ "Covariates at 1 year prior",
      table_name == "covariates_5" ~ "Covariates at 5 years prior",
      table_name == "covariates_inf" ~ "Covariates anytime prior",
      table_name == "covid" ~ "COVID-19",
      table_name == "covid_vaccines" ~ "COVID-19 vaccines",
      table_name == "covid_vaccines_dose" ~ "COVID-19 vaccines by dose",
      table_name == "nco" ~ "NCO",
      table_name == "mae" ~ "MAE",
      .default = stringr::str_to_sentence(gsub("_", " ", table_name))
    )
  )

if (length(unique(result$cdm_name)) == 1) {
  irr <- result |> 
    omopgenerics::filterSettings(result_type == "incidence_rate_ratio") |>
    omopgenerics::splitAdditional() |>
    dplyr::mutate(empirical_calibration = FALSE) |>
    omopgenerics::uniteAdditional(cols = c("outcome_name", "confidence_interval", "empirical_calibration")) |>
    dplyr::select(!"follow_up_end")
} else {
  # irr
  irr <- result |> 
    omopgenerics::addSettings(settingsColumn = c("outcome_group", "study_analysis", "weighting")) |>
    omopgenerics::filterSettings(result_type == "incidence_rate_ratio") |> 
    dplyr::select(!"variable_name") |>
    omopgenerics::pivotEstimates(pivotEstimatesBy = c("variable_level", "estimate_name")) 
  
  # empirical calibration
  irr <- irr |>
    omopgenerics::splitAdditional() |>
    dplyr::mutate(follow_up_end_ec = dplyr::if_else(grepl("sensitivity", study_analysis), "sensitivity", "main")) |>
    dplyr::group_by(cdm_name, group_name, group_level, strata_name, strata_level, follow_up_end_ec, weighting) |>
    tidyr::nest() |>
    dplyr::mutate(
      ec = purrr::map(data, empiricalCalibration)
    ) |>
    dplyr::select(!"data") |>
    tidyr::unnest(ec) |>
    omopgenerics::uniteAdditional(cols = c("outcome_name", "confidence_interval", "empirical_calibration")) |>
    dplyr::select(!c("follow_up_end_ec", "follow_up_end"))
  
  # meta-analysis
  metaResult <- irr |>
    dplyr::filter(outcome_group %in% c("Adverse Events of Special Interest", "Maternal Adverse Events")) |>
    dplyr::group_by(result_id, group_name, group_level, strata_name, strata_level, additional_name, additional_level) |>
    tidyr::nest() |>
    dplyr::mutate(
      meta = purrr::map(data, metaAnalysis)
    ) |>
    dplyr::select(!data) |>
    tidyr::unnest(meta)
  
  irr <- irr |>
    tidyr::pivot_longer(
      cols = c(
        'comparator_person_days', 'exposed_person_days', 'comparator_outcome_count', 
        'exposed_outcome_count', 'comparator_median', 'exposed_median', 'comparator_q25',
        'exposed_q25', 'comparator_q75', 'exposed_q75', 'comparator_min', 'exposed_min', 
        'comparator_max', 'exposed_max', 'comparator_record_count', 'exposed_record_count', 
        'coef', 'lower_ci', 'upper_ci'
      ),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      variable_name = dplyr::case_when(
        estimate_name %in% c("coef", "lower_ci", "upper_ci", "heterogeneity") ~ "Relative Risk",
        grepl("record_count", estimate_name) ~ "Number pregnancies",
        grepl("outcome_count", estimate_name) ~ "Number events",
        .default = "Person-Days"
      ),
      variable_level = dplyr::case_when(
        grepl("exposed", estimate_name) ~ "Exposed",
        grepl("comparator", estimate_name) ~ "Comparator",
        .default = NA_character_
      ),
      estimate_type = dplyr::if_else(grepl("count", estimate_name), "integer", "numeric"),
      estimate_name = gsub("exposed_|comparator_", "", estimate_name),
      estimate_value = as.character(estimate_value)
    ) |>
    dplyr::select(omopgenerics::resultColumns()) |> 
    omopgenerics::newSummarisedResult(settings = omopgenerics::settings(result) |> dplyr::filter(result_id %in% irr$result_id)) |>
    omopgenerics::bind(
      metaResult |>
        omopgenerics::newSummarisedResult(settings = omopgenerics::settings(result) |> dplyr::filter(result_id %in% irr$result_id))
    ) 
}

attr(irr, "settings") <- omopgenerics::settings(irr) |>
  dplyr::mutate(
    additional = "outcome_name &&& confidence_interval &&& empirical_calibration"
  )  

# substitute irr in results
result <- result |>
  omopgenerics::filterSettings(result_type != "incidence_rate_ratio") |> 
  omopgenerics::bind(irr)

data <- prepareResult(result, resultList)
values <- getValues(result, resultList)
# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
