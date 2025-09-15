# Cumulative incidence ----
info(logger, "Cumulative incidence")
## Prepare denominator ----
info(logger, "- Prepare denominator")
### Pregnancy and postpartum denominator for CIF
cdm$pregnancy_denominator <- cdm$mother_table |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", 
    "pregnancy_start_date", "pregnancy_end_date", "observation_period_end_date"
  ))) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("pregnancy_episode")
    )
  ) |>
  # Start in 2018 or after
  requireInDateRange(dateRange = as.Date(c("2018-01-01", NA))) |>
  # End date to 12 weeks postpartum
  mutate(
    postpartum_6_weeks = !!dateadd("pregnancy_end_date", 6*7),
    cohort_end_date = !!dateadd("pregnancy_end_date", 12*7)
  ) |>
  mutate(
    cohort_end_date = if_else(
      observation_period_end_date < cohort_end_date,
      observation_period_end_date,
      cohort_end_date
    )
  ) |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    cohort_end_date = if_else(
      next_pregnancy > cohort_end_date | is.na(next_pregnancy),
      cohort_end_date,
      as.Date(!!dateadd("next_pregnancy", -1))
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE)
### Add strata
cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
  addAge(
    ageName = "maternal_age", 
    ageGroup = list("12 to 17" = c(12, 17), "18 to 34" = c(12, 17), "35 to 50" = c(35, 50))
  ) |>
  mutate(
    pregnancy_start_period = case_when(
      year(pregnancy_start_date) %in% 2018:2019 ~ "Pre COVID-19",
      year(pregnancy_start_date) %in% 2020:2021 ~ "COVID-19 main outbreak",
      .default = "Post COVID-19 main outbreak"
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE)

strata <- list("maternal_age", "pregnancy_start_period")
if (cdmName(cdm) %in% c("CPRD AURUM", "CPRD GOLD", "SIDIAP")) {
  cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
    addSES() |>
    addEthnicity() |>
    compute(name = "pregnancy_denominator", temporary = FALSE)
  strata <- c(strata, list("socioeconomic_status", "ethnicity"))
}

## Estimate cumulative incidence ----
info(logger, "- Estimate cumulative incidence")
### AESI
cif_aesi_30 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_30",
  outcomeWashout = 30,
  censorOnCohortExit = TRUE,
  censorOnDate = "pregnancy_end_date",
  strata = strata,
)
cif_aesi_90 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_90",
  outcomeWashout = 90,
  censorOnCohortExit = TRUE,
  censorOnDate = "pregnancy_end_date",
  strata = strata,
)
cif_aesi_inf <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_inf",
  outcomeWashout = Inf,
  censorOnCohortExit = TRUE,
  censorOnDate = "pregnancy_end_date",
  strata = strata,
)
### MAE pregnancy
cif_mae_pregnancy <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = c(
    "miscarriage", "stillbirth", "maternal_death", "antepartum_haemorrhage", 
    "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
    "gestational_diabetes", "hellp", "preeclampsia", "preterm_labour"
  ),
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  censorOnDate = "pregnancy_end_date",
  strata = strata,
)
cif_mae_postpartum_6 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = c("postpartum_haemorrhage"),
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  censorOnDate = "postpartum_6_weeks",
  strata = strata,
)
cif_mae_postpartum_12 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = c("maternal_death", "postpartum_endometritis"),
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata,
)

## Export CIF ----
exportSummarisedResult(
  cif_aesi_30, cif_aesi_90, cif_aesi_inf, cif_mae_pregnancy, cif_mae_postpartum_6,
  cif_mae_postpartum_12,
  path = output_folder, 
  fileName = paste0("cumulative_incidence_", cdmName(cdm), ".csv")
)

# Incidence Rates ----
info(logger, "Incidence Rates")


