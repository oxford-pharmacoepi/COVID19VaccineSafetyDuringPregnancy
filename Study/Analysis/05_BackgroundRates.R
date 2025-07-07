# Background rates ----
## Miscarriage - ends week 19+6days ----
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "miscarriage_denominator",
  targetCohortTable = "mother_table",
  targetCohortId = NULL,
  cohortDateRange = as.Date(c("2018-01-01", NA)),
  timeAtRisk = c(0, 7*19 + 6),
  ageGroup = list(c(12, 17), c(18, 34), c(35, 55), c(12, 55)),
  sex = "Female",
  daysPriorObservation = 365,
  requirementsAtEntry = TRUE,
  requirementInteractions = TRUE
)
miscarriageIncidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "miscarriage_denominator",
  outcomeTable = "mae",
  censorTable = NULL,
  denominatorCohortId = NULL,
  outcomeCohortId = "miscarriage",
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)

## AESI + MAE pregnancy - during pregnancy ----
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "pregnancy_denominator",
  targetCohortTable = "mother_table",
  targetCohortId = NULL,
  cohortDateRange = as.Date(c("2018-01-01", NA)),
  timeAtRisk = list(c(0, Inf), c(0, 90), c(91, 180), c(181, Inf)),
  ageGroup = list(c(12, 17), c(18, 34), c(35, 55), c(12, 55)),
  sex = "Female",
  daysPriorObservation = 365,
  requirementsAtEntry = TRUE,
  requirementInteractions = TRUE
)
maePregnancyIncidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "pregnancy_denominator",
  outcomeTable = "mae",
  censorTable = NULL,
  denominatorCohortId = NULL,
  outcomeCohortId = c(
    'antepartum_haemorrhage', 'dysfunctional_labour', 'eclampsia', 'hellp', 'maternal_death',
    'gestational_diabetes', 'preeclampsia', 'ectopic_pregnancy', 'preterm_labour'
  ),
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)
cdm <- omopgenerics::bind(cdm$aesi_30, cdm$aesi_90, cdm$aesi_inf, name = "aesi")
aesiIncidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "pregnancy_denominator",
  outcomeTable = "aesi",
  censorTable = NULL,
  denominatorCohortId = NULL,
  outcomeCohortId = NULL,
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)

## Postpartum 1: 6 and 12 postpartum weeks ----
cdm$postpartum_denominator <- cdm$mother_table |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) |> 
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    next_pregnancy = !!dateadd("next_pregnancy", -1),
    cohort_start_date = pregnancy_end_date,
    cohort_end_date = if_else(is.na(next_pregnancy), observation_period_end_date, next_pregnancy)
  ) |>
  dplyr::compute(name = "postpartum_denominator", temporary = FALSE) |>
  newCohortTable(cohortSetRef = settings(cdm$mother_table) |> mutate(cohort_name = "postpartum_period"))
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "postpartum_denominator",
  targetCohortTable = "postpartum_denominator",
  targetCohortId = NULL,
  cohortDateRange = as.Date(c("2018-01-01", NA)),
  timeAtRisk = list(c(0, 6*7), c(0, 12*7)),
  ageGroup = list(c(12, 17), c(18, 34), c(35, 55), c(12, 55)),
  sex = "Female",
  daysPriorObservation = 365,
  requirementsAtEntry = TRUE,
  requirementInteractions = TRUE
)
ids6 <- settings(cdm$postpartum_denominator) |>
  filter(time_at_risk == "0 to 42") |>
  pull("cohort_definition_id")
maePostpartum6Incidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "postpartum_denominator",
  outcomeTable = "mae",
  censorTable = NULL,
  denominatorCohortId = ids6,
  outcomeCohortId = c("postpartum_endometritis", "maternal_death"),
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)
ids12 <- settings(cdm$postpartum_denominator) |>
  filter(time_at_risk == "0 to 84") |>
  pull("cohort_definition_id")
maePostpartum12Incidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "postpartum_denominator",
  outcomeTable = "mae",
  censorTable = NULL,
  denominatorCohortId = ids12,
  outcomeCohortId = "postpartum_haemorrhage",
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)

## Maternal death (overall, pregnancy and postpartum) ---- 
cdm$pregnancy_and_postpartum <- cdm$mother_table |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    next_pregnancy = !!dateadd("next_pregnancy", -1),
    cohort_start_date = pregnancy_start_date,
    cohort_end_date = if_else(is.na(next_pregnancy), observation_period_end_date, next_pregnancy)
  ) %>% 
  mutate(
    end6 = !!dateadd("pregnancy_end_date", 6*7),
    cohort_end_date = if_else(end6 > cohort_end_date, cohort_end_date, end6)
  ) |>
  dplyr::compute(name = "pregnancy_and_postpartum", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mother_table) |> 
      mutate(cohort_name = "pregnancy_and_postpartum_period")
  ) 
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "pregnancy_and_postpartum_denominator",
  targetCohortTable = "pregnancy_and_postpartum",
  targetCohortId = NULL,
  cohortDateRange = as.Date(c("2018-01-01", NA)),
  timeAtRisk = list(c(0, Inf)),
  ageGroup = list(c(12, 17), c(18, 34), c(35, 55), c(12, 55)),
  sex = "Female",
  daysPriorObservation = 365,
  requirementsAtEntry = TRUE,
  requirementInteractions = TRUE
)
maternalDeathIncidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "pregnancy_and_postpartum_denominator",
  outcomeTable = "mae",
  censorTable = NULL,
  denominatorCohortId = NULL,
  outcomeCohortId = "maternal_death",
  censorCohortId = NULL,
  interval = c("years", "overall"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  strata = list(),
  includeOverallStrata = TRUE
)

exportSummarisedResult(
  miscarriageIncidence, maePregnancyIncidence, aesiIncidence, 
  maePostpartum6Incidence, maePostpartum12Incidence, maternalDeathIncidence,
  path = output_folder, 
  fileName = paste0("background_rates_", cdmName(cdm), ".csv")
)
