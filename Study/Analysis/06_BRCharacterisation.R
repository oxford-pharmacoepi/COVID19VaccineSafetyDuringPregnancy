# Characteristics denominator ----
cdm$overall_period <- cdm$overall_period |> 
  select(!any_of(c("season", "season_yearly", "ethnicity", "socioeconomic_status"))) |>
  addSeason() |>
  addEthnicity() |>
  addSocioeconomicStatus(database = "CPRD GOLD") 
denominatorBaseline <- cdm$overall_period |>
  getBRDenominatorCharacteristics()
denominatorBaseline |>
  exportSummarisedResult(path = output_folder, fileName = paste0("characteristics_denominator", cdmName(cdm), ".csv"))

# Characteristics outcome ----
## AESI ----
cdm$aesi_outcome_char <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "aesi_outcome_char") |>
  inner_join(
    cdm$aesi |>
      addCohortName() |>
      select("cohort_name", "subject_id", "outcome_date" = "cohort_start_date"),
    by = "subject_id"
  ) |>
  compute(name = "aesi_outcome_char", temporary = FALSE) |>
  group_by(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, cohort_name) |>
  filter(outcome_date <= cohort_end_date & outcome_date >= cohort_start_date) |>
  filter(outcome_date == min(outcome_date, na.rm = TRUE)) |>
  ungroup() %>% 
  mutate(gestational_age = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    gestational_age = case_when(
      gestational_age >= 0 & gestational_age <= 90 ~ "Trimester 1",
      gestational_age >= 91 & gestational_age <= 180 ~ "Trimester 2",
      gestational_age >= 181 ~ "Trimester 3"
    ),
    pregnancy_end_date = cohort_end_date
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "cohort_end_date", "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "gestational_age", "pregnancy_start_date",
    "pregnancy_end_date"
  ))) |>
  inner_join(
    settings(cdm$aesi) |> select("cohort_definition_id", "cohort_name"),
    by = "cohort_name",
    copy = TRUE
  ) |>
  compute(name = "aesi_outcome_char", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$aesi) |> select("cohort_definition_id", "cohort_name"),
    cohortAttritionRef = NULL
  )

## Pregnancy and delivery MAE ----
cdm$pregnancy_outcome_char <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "pregnancy_outcome_char") |>
  inner_join(
    cdm$mae |>
      addCohortName() |>
      filter(
        ! cohort_name %in% c("postpartum_endometritis", "postpartum_haemorrhage", "maternal_death")
      ) |>
      select("cohort_name", "subject_id", "outcome_date" = "cohort_start_date"),
    by = "subject_id"
  ) |>
  compute(name = "pregnancy_outcome_char", temporary = FALSE) |>
  group_by(subject_id, cohort_start_date, cohort_end_date, cohort_name) |>
  filter(outcome_date <= cohort_end_date & outcome_date >= cohort_start_date) |>
  filter(outcome_date == min(outcome_date, na.rm = TRUE)) |>
  ungroup() %>% 
  mutate(gestational_age = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    gestational_age = case_when(
      gestational_age >= 0 & gestational_age <= 90 ~ "Trimester 1",
      gestational_age >= 91 & gestational_age <= 180 ~ "Trimester 2",
      gestational_age >= 181 ~ "Trimester 3"
    ),
    pregnancy_end_date = cohort_end_date
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "cohort_end_date", "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "gestational_age", "pregnancy_start_date",
    "pregnancy_end_date"
  ))) |>
  inner_join(
    settings(cdm$mae),
    by = "cohort_name",
    copy = TRUE
  ) |>
  compute(name = "pregnancy_outcome_char", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mae) |> 
      filter(
        ! cohort_name %in% c("postpartum_endometritis", "postpartum_haemorrhage", "maternal_death")
      ) |> 
      select("cohort_definition_id", "cohort_name"),
    cohortAttritionRef = NULL
  )

## Postpartum 6 week MAE ----
cdm$postpartum6_outcome_char <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "postpartum6_outcome_char") |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) %>% 
  mutate(
    postpartum_end_date = !!dateadd("cohort_end_date", 7*12),
    pregnancy_start_date_lead = dplyr::lead(pregnancy_start_date)
  ) %>% 
  mutate(
    postpartum_end_date = if_else(
      postpartum_end_date < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    )
  ) |>
  ungroup() |>
  inner_join(
    cdm$mae |>
      addCohortName() |>
      filter(cohort_name %in% c("postpartum_endometritis")) |>
      select("cohort_name", "subject_id", "outcome_date" = "cohort_start_date"),
    by = "subject_id"
  ) |>
  compute(name = "postpartum6_outcome_char", temporary = FALSE) |>
  group_by(subject_id, cohort_start_date, cohort_end_date, cohort_name) %>% 
  filter(outcome_date <= postpartum_end_date & outcome_date >= cohort_end_date) |>
  filter(outcome_date == min(outcome_date, na.rm = TRUE)) |>
  ungroup() |>
  distinct() |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "pregnancy_start_date",
    "pregnancy_end_date" = "cohort_end_date"
  ))) |>
  inner_join(
    settings(cdm$mae) |> select("cohort_definition_id", "cohort_name"),
    by = "cohort_name",
    copy = TRUE
  ) |>
  mutate(cohort_end_date = cohort_start_date) |>
  compute(name = "postpartum6_outcome_char", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mae)  |> 
      select("cohort_definition_id", "cohort_name") |> 
      filter(cohort_name %in% c("postpartum_endometritis")),
    cohortAttritionRef = NULL
  )

## Postpartum 12 weks MAE ----
cdm$postpartum12_outcome_char <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "postpartum12_outcome_char") |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) %>% 
  mutate(
    postpartum_end_date = !!dateadd("cohort_end_date", 7*12),
    pregnancy_start_date_lead = dplyr::lead(pregnancy_start_date)
  ) %>% 
  mutate(
    postpartum_end_date = if_else(
      postpartum_end_date < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    )
  ) |>
  ungroup() |>
  inner_join(
    cdm$mae |>
      addCohortName() |>
      filter(cohort_name %in% c("postpartum_haemorrhage")) |>
      select("cohort_name", "subject_id", "outcome_date" = "cohort_start_date"),
    by = "subject_id"
  ) |>
  compute(name = "postpartum12_outcome_char", temporary = FALSE) |>
  group_by(subject_id, cohort_start_date, cohort_end_date, cohort_name) %>% 
  filter(outcome_date <= postpartum_end_date & outcome_date >= cohort_end_date) |>
  filter(outcome_date == min(outcome_date, na.rm = TRUE)) |>
  distinct() |>
  ungroup() |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "pregnancy_start_date",
    "pregnancy_end_date" = "cohort_end_date"
  ))) |>
  inner_join(
    settings(cdm$mae) |> select("cohort_definition_id", "cohort_name"),
    by = "cohort_name",
    copy = TRUE
  ) |>
  mutate(cohort_end_date = cohort_start_date) |>
  compute(name = "postpartum12_outcome_char", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mae)  |> 
      select("cohort_definition_id", "cohort_name") |> 
      filter(cohort_name %in% c("postpartum_haemorrhage")),
    cohortAttritionRef = NULL
  )

## Maternal death ----
cdm$mdeath_outcome_char <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "mdeath_outcome_char") |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) %>% 
  mutate(
    postpartum_end_date = !!dateadd("cohort_end_date", 7*6),
    pregnancy_start_date_lead = dplyr::lead(pregnancy_start_date)
  ) %>% 
  mutate(
    postpartum_end_date = if_else(
      postpartum_end_date < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    )
  ) |>
  ungroup() |>
  inner_join(
    cdm$mae |>
      addCohortName() |>
      filter(cohort_name %in% c("maternal_death")) |>
      select("cohort_name", "subject_id", "outcome_date" = "cohort_start_date"),
    by = "subject_id"
  ) |>
  compute(name = "mdeath_outcome_char", temporary = FALSE) |>
  group_by(subject_id, cohort_start_date, cohort_end_date, cohort_name) %>% 
  filter(outcome_date <= postpartum_end_date & outcome_date >= cohort_start_date) |>
  filter(outcome_date == min(outcome_date, na.rm = TRUE)) |>
  distinct() |>
  ungroup() %>% 
  mutate(gestational_age = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    gestational_age = case_when(
      outcome_date > cohort_end_date ~ "Postpartum",
      gestational_age >= 0 & gestational_age <= 90 ~ "Trimester 1",
      gestational_age >= 91 & gestational_age <= 180 ~ "Trimester 2",
      gestational_age >= 181 ~ "Trimester 3"
    )
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id", 
    "gestational_age", "maternal_age", "season_yearly", "season", "ethnicity",
    "socioeconomic_status", "pregnancy_start_date",
    "pregnancy_end_date" = "cohort_end_date"
  ))) |>
  inner_join(
    settings(cdm$mae) |> select("cohort_definition_id", "cohort_name"),
    by = "cohort_name",
    copy = TRUE
  ) |>
  mutate(cohort_end_date = cohort_start_date) |>
  compute(name = "mdeath_outcome_char", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mae) |> 
      select("cohort_definition_id", "cohort_name") |> 
      filter(cohort_name %in% c("maternal_death")),
    cohortAttritionRef = NULL
  )

## Bind outcome cohorts ----
cdm <- bind(
  cdm$aesi_outcome_char, cdm$pregnancy_outcome_char, name = "outcome_characterisation_pregnancy"
)
cdm <- bind(
  cdm$postpartum6_outcome_char, cdm$postpartum12_outcome_char,  name = "outcome_characterisation_postpartum"
)
## Create match cohorts ----
source(here::here("Analysis", "06.1_BRMatchCohorts.R"))

## Characterise ----

# Variables to add: socioeconomic status, ethnicity, season pregnancy start
# Pregnancy: previous pregnancy, gestational age at outcome
# Comorbidities: alchohol, obesity, diabetes, hypertension, asthma, depression/anxiety, epilepsy
# Medications: opioids, antidepressants, antiepilepsy, diabetes treatment/s, nsaids, omeprazole/antiacids, corticosteroids