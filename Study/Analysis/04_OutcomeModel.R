# AESI ----
toKeep <- c(
  "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", 
  "cohort_end_date", "cohort_end_date_sensitivity", "pregnancy_end_date",
  unique(unlist(strata)), "weight"
)
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))
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
    targetCohortTable = "aesi90",
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi30",
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    nameStyle = "{cohort_name}",
    name = "aesi_outcome"
  )

aesiOutcomes <- colnames(cdm$aesi_outcome)
aesiOutcomes <- aesiOutcomes[!aesiOutcomes %in% toKeep & !grepl("_42", aesiOutcomes)]
endDates <- c("end_42_days_or_pregnancy", "end_42_days", "end_42_days_or_pregnancy_sensitivity", "end_42_days_sensitivity")
aesiResults <- list()
jj <- 1
for (end in endDates) {
  aesiResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$aesi_outcome, outcomes = aesiOutcomes,
    end = end, strata = strata, group = "cohort_name", weights = NULL
  )
  aesiResults[[jj]] <- estimateSurvivalRisk(
    cohort = cdm$aesi_outcome, outcomes = aesiOutcomes,
    end = end, strata = strata, group = "cohort_name", weights = "weight"
  )
  jj <- jj + 1
}

aesiResults <- omopgenerics::bind(!!!aesiResults)



# MAE ----
## Group 1: < 20 weeks (miscarriage)
### Main
### Sensitvity
## Group 2: during pregnancy
## Group 3: up to 6 weeks after pregnancy
## Group 4: up to 12 weeks after pregnancy