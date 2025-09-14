info(logger, "- AESI outcome cohort")
## AESI ----
cdm$aesi_outcome_char <- cdm$overall_period |>
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
  mutate(trimester = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    trimester = case_when(
      trimester >= 0 & trimester <= 90 ~ "Trimester 1",
      trimester >= 91 & trimester <= 180 ~ "Trimester 2",
      trimester >= 181 ~ "Trimester 3"
    ),
    pregnancy_end_date = cohort_end_date
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "cohort_end_date", "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "trimester", "pregnancy_start_date",
    "pregnancy_end_date"
  ))) |>
  compute(name = "aesi_outcome_char", temporary = FALSE) |>
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

info(logger, "- Pregnancy outcome cohort")
## Pregnancy and delivery MAE ----
cdm$pregnancy_outcome_char <- cdm$overall_period |>
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
  mutate(trimester = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    trimester = case_when(
      trimester >= 0 & trimester <= 90 ~ "Trimester 1",
      trimester >= 91 & trimester <= 180 ~ "Trimester 2",
      trimester >= 181 ~ "Trimester 3"
    ),
    pregnancy_end_date = cohort_end_date
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id",
    "cohort_end_date", "maternal_age", "season_yearly", "season", "ethnicity", 
    "socioeconomic_status", "trimester", "pregnancy_start_date",
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

info(logger, "- Postpartum 6 weeks outcome cohort")
## Postpartum 6 week MAE ----
cdm$postpartum6_outcome_char <- cdm$overall_period |>
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
  compute(name = "postpartum6_outcome_char", temporary = FALSE) |>
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

info(logger, "- Postpartum 12 weeks outcome cohort")
## Postpartum 12 weks MAE ----
cdm$postpartum12_outcome_char <- cdm$overall_period |>
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
  compute(name = "postpartum12_outcome_char", temporary = FALSE) |>
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

info(logger, "- Maternal death outcome cohort")
## Maternal death ----
cdm$mdeath_outcome_char <- cdm$overall_period |>
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
  compute(name = "mdeath_outcome_char", temporary = FALSE) |>
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
  mutate(trimester = !!datediff("cohort_start_date", "outcome_date")) |>
  mutate(
    trimester = case_when(
      outcome_date > cohort_end_date ~ "Postpartum",
      trimester >= 0 & trimester <= 90 ~ "Trimester 1",
      trimester >= 91 & trimester <= 180 ~ "Trimester 2",
      trimester >= 181 ~ "Trimester 3"
    )
  ) |>
  select(any_of(c(
    "cohort_name", "cohort_start_date" = "outcome_date", "subject_id", 
    "trimester", "maternal_age", "season_yearly", "season", "ethnicity",
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