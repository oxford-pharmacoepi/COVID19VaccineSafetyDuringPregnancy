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
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data copy")) |>
  dplyr::filter(!grepl("miscarriage_codelist", additional_level)) |>
  dplyr::filter(!(grepl("miscarriage", additional_level) & cdm_name == "CPRD GOLD")) |>
  dplyr::filter(!grepl("miscarriage_codelist", group_level)) |>
  dplyr::filter(!(grepl("miscarriage", group_level) & cdm_name == "CPRD GOLD"))
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
      grepl("exposed", estimate_name) ~ "exposed",
      grepl("comparator", estimate_name) ~ "comparator",
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
