# AESI ----
info(logger, "- Create AESI outcome cohort")
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))
toKeep <- c(
  "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", 
  "cohort_end_date", "cohort_end_date_sensitivity", "pregnancy_end_date",
  "exposed_match_id", "pregnancy_id", unique(unlist(strata)), "weight",
  "exposure", covariatesPS, unlist(selectedLassoFeatures)
) |> unique()
cdm$aesi_outcome <- cdm$study_population |>
  dplyr::select(dplyr::all_of(toKeep)) %>% 
  mutate(start_42 = as.Date(!!CDMConnector::dateadd("cohort_start_date", 42))) |>
  mutate(
    end_42_days_or_pregnancy =  if_else(pregnancy_end_date < cohort_end_date, pregnancy_end_date, cohort_end_date),
    end_42_days_or_pregnancy =  if_else(start_42 < end_42_days_or_pregnancy, start_42, end_42_days_or_pregnancy),
    end_42_days = if_else(start_42 < cohort_end_date, start_42, cohort_end_date),
    end_42_days_or_pregnancy_sensitivity =  if_else(pregnancy_end_date < cohort_end_date_sensitivity, pregnancy_end_date, cohort_end_date_sensitivity),
    end_42_days_or_pregnancy_sensitivity =  if_else(start_42 < end_42_days_or_pregnancy_sensitivity, start_42, end_42_days_or_pregnancy_sensitivity),
    end_42_days_sensitivity = if_else(start_42 < cohort_end_date_sensitivity, start_42, cohort_end_date_sensitivity)
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_90",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_30",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  )

aesiOutcomes <- colnames(cdm$aesi_outcome)
aesiOutcomes <- aesiOutcomes[!aesiOutcomes %in% toKeep & !grepl("_42", aesiOutcomes)]
endDates <- c("end_42_days_or_pregnancy", "end_42_days", "end_42_days_or_pregnancy_sensitivity", "end_42_days_sensitivity")
aesiResults <- list()
jj <- 1
for (end in endDates) {
  info(logger, glue::glue("- Get IRR for AESI : analysis '{end}'"))
  aesiResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$aesi_outcome, outcomes = aesiOutcomes, outcomeGroup = "Adverse Events of Special Interest",
    end = end, strata = strata, group = "cohort_name", weights = allCovariatesPS, ci = ci
  )
  jj <- jj + 1
}

# MAE ----
info(logger, "- Create MAE outcome cohort")
cdm$mae_outcome <- cdm$study_population |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "mae_outcome"
  )
maeResults <- list()
jj <- 1

## Group 1: < 20 weeks (miscarriage)
info(logger, "Get IRR for miscarriage")
cdm$mea_miscarriage <- cdm$mae_outcome %>% 
  mutate(week_19_end = !!dateadd("pregnancy_start_date", 19*7 + 6)) |>
  filter(cohort_start_date < week_19_end) |>
  mutate(
    week_19_end =  if_else(week_19_end < cohort_end_date, week_19_end, cohort_end_date),
    week_19_end_sensitivity =  if_else(week_19_end < cohort_end_date_sensitivity, week_19_end, cohort_end_date_sensitivity)
  ) |>
  compute(name = "mea_miscarriage", temporary = FALSE)
for (endDate in c("week_19_end", "week_19_end_sensitivity")) {
  maeResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$mea_miscarriage, outcomes = "miscarriage", outcomeGroup = "Maternal Adverse Events",
    end = endDate, strata = strata, group = "cohort_name", weights = allCovariatesPS, ci = ci
  )
  jj <- 1 + jj
}

## Group 2: during pregnancy
info(logger, "Get IRR for MAE during pregnancy")
cdm$mea_pregnancy <- cdm$mae_outcome %>% 
  mutate(
    pregnancy_end =  if_else(pregnancy_end_date < cohort_end_date, pregnancy_end_date, cohort_end_date),
    pregnancy_end_sensitivity =  if_else(pregnancy_end_date < cohort_end_date_sensitivity, pregnancy_end_date, cohort_end_date_sensitivity)
  ) |>
  compute(name = "mea_pregnancy", temporary = FALSE)
for (endDate in c("pregnancy_end", "pregnancy_end_sensitivity")) {
  outcomes <- c('antepartum_haemorrhage', 'eclampsia', 'hellp', 'preterm_labour', 'dysfunctional_labour')
  maeResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$mea_pregnancy, outcomes = outcomes, outcomeGroup = "Maternal Adverse Events",
    end = endDate, strata = strata, group = "cohort_name", weights = allCovariatesPS, ci = ci
  )
  jj <- 1 + jj
}

## Group 3: up to 6 weeks after pregnancy
info(logger, "Get IRR for 6 weeks postpartum")
cdm$mae_postpartum_6 <- cdm$mae_outcome %>% 
  mutate(postpartum_6_weeks = !!dateadd("pregnancy_end_date", 6*7)) |>
  mutate(
    postpartum_6_weeks =  if_else(postpartum_6_weeks < cohort_end_date, postpartum_6_weeks, cohort_end_date),
    postpartum_6_weeks_sensitivity =  if_else(postpartum_6_weeks < cohort_end_date_sensitivity, postpartum_6_weeks, cohort_end_date_sensitivity)
  ) |>
  compute(name = "mae_postpartum_6", temporary = FALSE)
for (endDate in c("postpartum_6_weeks", "postpartum_6_weeks_sensitivity")) {
  outcomes <- c('postpartum_endometritis', 'maternal_death')
  maeResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$mae_postpartum_6, outcomes = outcomes, outcomeGroup = "Maternal Adverse Events",
    end = endDate, strata = strata, group = "cohort_name", weights = allCovariatesPS, ci = ci
  )
  jj <- 1 + jj
}

## Group 4: up to 12 weeks after pregnancy
info(logger, "Get IRR for 12 weeks after pregnancy")
cdm$mea_postpartum_12 <- cdm$mae_outcome %>% 
  mutate(postpartum_12_weeks = !!dateadd("pregnancy_end_date", 12*7)) |>
  mutate(
    postpartum_12_weeks =  if_else(postpartum_12_weeks < cohort_end_date, postpartum_12_weeks, cohort_end_date),
    postpartum_12_weeks_sensitivity =  if_else(postpartum_12_weeks < cohort_end_date_sensitivity, postpartum_12_weeks, cohort_end_date_sensitivity)
  ) |>
  compute(name = "mea_postpartum_12", temporary = FALSE)
for (endDate in c("postpartum_12_weeks", "postpartum_12_weeks_sensitivity")) {
  outcomes <- c('postpartum_haemorrhage')
  maeResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$mea_postpartum_12, outcomes = outcomes, outcomeGroup = "Maternal Adverse Events",
    end = endDate, strata = strata, group = "cohort_name", weights = allCovariatesPS, ci = ci
  )
  jj <- 1 + jj
}


# Export results ----
info(logger, "Export outcome risk results")
aesiResults <- omopgenerics::bind(aesiResults) 
maeResults <- omopgenerics::bind(maeResults) 
omopgenerics::bind(aesiResults, maeResults) |>
  exportSummarisedResult(fileName = paste0("outcome_risk_estimates_", cdmName(cdm), ".csv"), path = output_folder)

## NCO (not weighted) ----
info(logger, "- Negative Control Outcomes")
cdm$study_population_nco <- cdm$study_population |>
  select(any_of(c(
    "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", "cohort_end_date",
    "cohort_end_date_sensitivity", "exposure", "exposed_match_id", "pregnancy_id",
    unlist(strata), "weight", , covariatesPS, unlist(selectedLassoFeatures)
  ))) |>
  compute(name = "study_population_nco", temporary = FALSE) |>
  newCohortTable(.softValidation = TRUE) |>
  addCohortIntersectDate(
    targetCohortTable = "nco",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "study_population_nco"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "study_population_nco"
  )

if (getNCO) {
  nco <- estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
    end = "cohort_end_date", strata = strata, group = "cohort_name", 
    weights = allCovariatesPS, outcomeGroup = "Negative Control Outcomes", ci = ci
  )
  nco_sensitivity <-   estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
    end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
    weights = allCovariatesPS, outcomeGroup = "Negative Control Outcomes", ci = ci
  )
} else {
  nco_ <- NULL
  nco_sensitivity <- NULL
}

if (getPCO) {
  pco <- estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = "covid", 
    end = "cohort_end_date", strata = strata, group = "cohort_name", 
    weights = allCovariatesPS, outcomeGroup = "Positive Control Outcomes", ci = ci
  )
  pco_sensitivity <- estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = "covid", 
    end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
    weights = allCovariatesPS, outcomeGroup = "Positive Control Outcomes", ci = ci
  )
} else {
  pco <- NULL
  pco_sensitivity <- NULL
}

nco_pco <- bind(nco, nco_sensitivity, pco, pco_sensitivity) |>
  suppressRiskEstimates()

nco_pco_weighted |> 
  exportSummarisedResult(fileName = paste0("nco_", cdmName(cdm), ".csv"), path = output_folder)

