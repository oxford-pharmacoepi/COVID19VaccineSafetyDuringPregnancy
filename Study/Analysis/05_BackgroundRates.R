# Denominator ----
info(logger, "Prepare denominator:")
## Overall pregnancy ----
info(logger, "-  Overall pregnancy")
cdm$pregnancy_denominator <- cdm$mother_table |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("pregnancy_episode")
    )
  )
## By trimester ----
info(logger, "-  By trimester")
cdm$trimester_denominator <- cdm$mother_table %>% 
  mutate(
    first_trimester = pregnancy_start_date,
    second_trimester = as.Date(!!dateadd("pregnancy_start_date", 91)),
    third_trimester = as.Date(!!dateadd("pregnancy_start_date", 181))
  ) |>
  select(pregnancy_id, subject_id, first_trimester, second_trimester, third_trimester, pregnancy_end_date, pregnancy_start_date) |>
  pivot_longer(
    cols = c("first_trimester", "second_trimester", "third_trimester"),
    names_to = "trimester",
    values_to = "cohort_start_date"
  ) %>% 
  mutate(
    cohort_end_date = case_when(
      trimester == "first_trimester" ~ as.Date(!!dateadd("pregnancy_start_date", 90)),
      trimester == "second_trimester" ~ as.Date(!!dateadd("pregnancy_start_date", 180)),
      .default = pregnancy_end_date
    )
  ) |>
  filter(
    # preganancies making it to the trimester of interest
    pregnancy_end_date >= cohort_start_date
  ) |>
  mutate(
    cohort_end_date = if_else(
      pregnancy_end_date < cohort_end_date, pregnancy_end_date, cohort_end_date
    ),
    cohort_definition_id = case_when(
      trimester == "first_trimester" ~ 1,
      trimester == "second_trimester" ~ 2,
      .default = 3
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "trimester_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1:3, cohort_name = c("first_trimester", "second_trimester", "third_trimester")
    ),
    cohortAttritionRef = NULL
  )
## Miscarriage risk ----
info(logger, "-  Miscarriage")
cdm$miscarriage_denominator <- cdm$mother_table %>% 
  mutate(
    cohort_end_date = as.Date(!!dateadd("pregnancy_start_date", 19*7 + 6))
  ) |>
  mutate(
    cohort_end_date = if_else(
      cohort_end_date > pregnancy_end_date, pregnancy_end_date, cohort_end_date
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "miscarriage_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("miscarriage_denominator")
    )
  )
## Stillbirth risk ----
info(logger, "-  Stillbirth")
cdm$stillbirth_denominator <- cdm$mother_table %>% 
  mutate(
    cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
  ) |>
  filter(cohort_start_date <= cohort_end_date) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "stillbirth_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("stillbirth_denominator")
    )
  )
## 6 and 12 weeks postpartum ----
info(logger, "-  12 and 6 weeks postpartum")
cdm$postpartum_denominator <- cdm$mother_table %>% 
  mutate(
    cohort_start_date = pregnancy_end_date,
    postpartum_6weeks = !!dateadd("pregnancy_end_date", 6*7),
    postpartum_12weeks = !!dateadd("pregnancy_end_date", 12*7)
  ) |>
  select(!"cohort_end_date") |>
  pivot_longer(
    cols = c("postpartum_6weeks", "postpartum_12weeks"),
    names_to = "postpartum",
    values_to = "cohort_end_date"
  ) |>
  mutate(
    cohort_end_date = if_else(
      observation_period_end_date < cohort_end_date,
      observation_period_end_date,
      cohort_end_date
    ),
    cohort_definition_id = if_else(postpartum == "postpartum_6weeks", 1, 2)
  ) |>
  group_by(cohort_definition_id, subject_id) |>
  arrange(cohort_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    cohort_end_date = if_else(
      next_pregnancy <= cohort_end_date & !is.na(next_pregnancy),
      as.Date(!!dateadd("next_pregnancy", -1)),
      cohort_end_date
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "postpartum_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1:2, cohort_name = c("postpartum_6weeks", "postpartum_12weeks")
    ),
    cohortAttritionRef = NULL
  )
## Maternal death ----
info(logger, "-  Maternal death")
cdm$maternal_death_denominator <- cdm$mother_table %>% 
  mutate(cohort_end_date = !!dateadd("pregnancy_end_date", 6*7)) |>
  mutate(
    cohort_end_date = if_else(
      observation_period_end_date < cohort_end_date,
      observation_period_end_date,
      cohort_end_date
    )
  )|>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    cohort_end_date = if_else(
      next_pregnancy <= cohort_end_date & !is.na(next_pregnancy),
      as.Date(!!dateadd("next_pregnancy", -1)),
      cohort_end_date
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date"
  ))) |>
  compute(name = "maternal_death_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("maternal_death_denominator")
    )
  )
## Bind and apply requirements ----
info(logger, "-  Bind all")
cdm <- bind(
  cdm$pregnancy_denominator, cdm$trimester_denominator, cdm$miscarriage_denominator, 
  cdm$stillbirth_denominator, cdm$postpartum_denominator, cdm$maternal_death_denominator,
  name = "denominator"
)
cdm$denominator <- cdm$denominator |>
  requireDemographics(
    sex = "Female",
    ageRange = list(c(12, 55)),
    minPriorObservation = 365
  ) |>
  mutate(cohort_end_date = as.Date(cohort_end_date)) |>
  addAge(
    indexDate = "pregnancy_start_date",
    ageGroup = list("maternal_age" = list(c(12, 17), c(18, 34), c(35, 55))),
    name = "overall_period"
  )
## IncidencePrevalence denominator ----
### Overall
info(logger, "-  Trim denominator: overall period")
cdm$overall_period <- cdm$denominator |>
  trimToDateRange(
    dateRange = as.Date(c("2018-01-01", NA)),
    name = "overall_period"
  )
cdm$overall_period <- asIncidencePrevalence(
  cohort = cdm$overall_period, startDate = as.Date("2018-01-01"), 
  endDate = as.Date(NA)
)
### Pre COVID
info(logger, "-  Trim denominator: pre-covid period")
cdm$pre_covid_period <- cdm$denominator |>
  trimToDateRange(
    dateRange = as.Date(c("2018-01-01", "2019-12-31")),
    name = "pre_covid_period"
  )
cdm$pre_covid_period <- asIncidencePrevalence(
  cohort = cdm$pre_covid_period, startDate = as.Date("2018-01-01"), 
  endDate = as.Date("2019-12-31")
)
### During COVID
info(logger, "-  Trim denominator: covid period")
cdm$main_covid_period <- cdm$denominator |>
  trimToDateRange(
    dateRange = as.Date(c("2020-01-01", "2021-12-31")),
    name = "main_covid_period"
  )
cdm$main_covid_period <- asIncidencePrevalence(
  cohort = cdm$main_covid_period, startDate = as.Date("2020-01-01"), 
  endDate = as.Date("2021-12-31")
)
### Post COVID
info(logger, "-  Trim denominator: post-covid period")
cdm$post_main_covid_period <- cdm$denominator |>
  trimToDateRange(
    dateRange = as.Date(c("2022-01-01", NA)),
    name = "post_main_covid_period"
  )
cdm$post_main_covid_period <- asIncidencePrevalence(
  cohort = cdm$post_main_covid_period, startDate = as.Date("2022-01-01"), 
  endDate = as.Date(NA)
)

# Background rates ----
info(logger, "Background rates:")
## Miscarriage 
info(logger, "- Miscarriage")
if ("miscarriage" %in% settings(cdm$mae)$cohort_name) {
  miscarriage <- getIncidence(
    cdm = cdm, targetCohortDenominator = "miscarriage_denominator", 
    outcomeTable = "mae", outcomeCohortId = "miscarriage"
  )
} else {
  miscarriage <- emptySummarisedResult()
}
## Stillbirth 
info(logger, "- Stillbirth")
if ("stillbirth" %in% settings(cdm$mae)$cohort_name) {
  stillbirth <- getIncidence(
    cdm = cdm, targetCohortDenominator = "stillbirth_denominator", 
    outcomeTable = "mae", outcomeCohortId = "stillbirth"
  )
} else {
  stillbirth <- emptySummarisedResult()
}
## Maternal death 
info(logger, "- Maternal death")
if ("maternal_death" %in% settings(cdm$mae)$cohort_name) {
  maternal_death <- getIncidence(
    cdm = cdm, targetCohortDenominator = "maternal_death_denominator", 
    outcomeTable = "mae", outcomeCohortId = "maternal_death"
  )
} else {
  maternal_death <- emptySummarisedResult()
}
## Postpartum 6 weeks
info(logger, "- 6 weeks postpartum")
postpartum_6weeks <- getIncidence(
  cdm = cdm, targetCohortDenominator = "postpartum_6weeks", 
  outcomeTable = "mae", outcomeCohortId = c("maternal_death", "postpartum_endometritis")
)
## Postpartum 12 weeks
info(logger, "- 12 weeks postpartum")
postpartum_12weeks <- getIncidence(
  cdm = cdm, targetCohortDenominator = "postpartum_12weeks", 
  outcomeTable = "mae", outcomeCohortId = "postpartum_haemorrhage"
)
## Pregnancy MAE
info(logger, "- Pregnancy MAE")
pregnancy_mae <- getIncidence(
  cdm = cdm, 
  targetCohortDenominator = c("pregnancy_episode", "first_trimester", "second_trimester", "third_trimester"), 
  outcomeTable = "mae", 
  outcomeCohortId = c(
    "maternal_death", "antepartum_haemorrhage", "dysfunctional_labour", "eclampsia",
    "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia", "preterm_labour"
  )
)
## AESI
info(logger, "- AESI")
cdm <- bind(cdm$aesi_30, cdm$aesi_90, cdm$aesi_inf, name = "aesi")
aesi <- getIncidence(
  cdm = cdm, 
  targetCohortDenominator = c(
    "pregnancy_episode", "first_trimester", "second_trimester", 
    "third_trimester", "postpartum_6weeks"
  ), 
  outcomeTable = "aesi", 
  outcomeCohortId = NULL
)
## Export results
info(logger, "Exposrt background rates results")
exportSummarisedResult(
  miscarriage, stillbirth, maternal_death, postpartum_6weeks, postpartum_12weeks,
  pregnancy_mae, aesi,
  path = output_folder, 
  fileName = paste0("background_rates_", cdmName(cdm), ".csv")
)
