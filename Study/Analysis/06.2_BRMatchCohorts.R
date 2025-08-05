# Pregnancy match ----
info(logger, "- Match pregnancy outcome cohort")
## Pregnancy outcome match
cdm$outcome_match_pregnancy <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "outcome_match_pregnancy") |>
  anti_join(
    cdm$outcome_characterisation_pregnancy |> distinct(subject_id), by = "subject_id"
  ) |>
  compute(name = "outcome_match_pregnancy", temporary = FALSE) |>
  group_by(subject_id) |>
  mutate(
    pregnancy_start_band = if_else(
      day(pregnancy_start_date) <= 15, 
      paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
      paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
    )
  ) |>
  ungroup() |>
  select(
    any_of(c(
      "subject_id", "maternal_age", "pregnancy_start_band", "pregnancy_start_date", 
      "pregnancy_end_date" = "cohort_end_date",
      "season_yearly", "season", "ethnicity", "socioeconomic_status"
    ))
  )  |>
  compute(name = "outcome_match_pregnancy", temporary = FALSE) |>
  inner_join(
    cdm$outcome_characterisation_pregnancy %>% 
      mutate(
        pregnancy_start_band = if_else(
          day(pregnancy_start_date) <= 15, 
          paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
          paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
        )
      ) |>
      select(
        "target_subject_id" = "subject_id", "outcome_date" = "cohort_start_date", 
        "maternal_age", "cohort_definition_id", "cohort_name", "pregnancy_start_band"
      ),
    by = c("maternal_age", "pregnancy_start_band"),
    relationship = "many-to-many"
  ) |>
  compute(name = "outcome_match_pregnancy", temporary = FALSE) |>
  # pregnant during outcome for pregnancy outcomes
  filter(pregnancy_start_date <= outcome_date & pregnancy_end_date >= outcome_date) |>
  slice_sample(
    n =  1,
    by = all_of(c("cohort_definition_id", "cohort_name", "target_subject_id"))
  ) |>
  mutate(cohort_start_date = outcome_date, cohort_end_date = outcome_date) |>
  compute(name = "outcome_match_pregnancy", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$outcome_characterisation_pregnancy) |>
      mutate(cohort_name = paste0(cohort_name, "_matched")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )
## Pregnancy outcome sample
cdm$outcome_sample_pregnancy <- cdm$outcome_characterisation_pregnancy |>
  inner_join(
    cdm$outcome_match_pregnancy  |>
      distinct(target_subject_id) |>
      rename("subject_id" = "target_subject_id"),
    by = "subject_id"
  ) |>
  compute(name = "outcome_sample_pregnancy", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$outcome_characterisation_pregnancy) |>
      mutate(cohort_name = paste0(cohort_name, "_sampled")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )

# Postpartum match ----
info(logger, "- Match postpartum outcome cohort")
## Postpartum outcome match
cdm$outcome_match_postpartum <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "outcome_match_postpartum") |>
  anti_join(
    cdm$outcome_characterisation_postpartum |> distinct(subject_id), by = "subject_id"
  ) |>
  compute(name = "outcome_match_postpartum", temporary = FALSE) |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) %>% 
  mutate(
    postpartum_end_date_6 = !!dateadd("cohort_end_date", 7*6),
    postpartum_end_date_12 = !!dateadd("cohort_end_date", 7*12),
    pregnancy_start_date_lead = dplyr::lead(pregnancy_start_date)
  ) %>% 
  mutate(
    postpartum_end_date_6 = if_else(
      postpartum_end_date_6 < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date_6, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    ),
    postpartum_end_date_12 = if_else(
      postpartum_end_date_12 < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date_12, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    ),
    pregnancy_start_band = if_else(
      day(pregnancy_start_date) <= 15, 
      paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
      paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
    ),
    pregnancy_end_band = if_else(
      day(cohort_end_date) <= 15, 
      paste0("01", month(cohort_end_date), year(cohort_end_date)),
      paste0("02", month(cohort_end_date), year(cohort_end_date))
    )
  ) |>
  ungroup() |>
  select(
    any_of(c(
      "subject_id", "postpartum_end_date_6",  "postpartum_end_date_12", 
      "maternal_age", "pregnancy_start_band", "pregnancy_end_band", 
      "pregnancy_start_date", "pregnancy_end_date" = "cohort_end_date",
      "season_yearly", "season", "ethnicity", "socioeconomic_status"
    ))
  )  |>
  compute(name = "outcome_match_postpartum", temporary = FALSE) |>
  inner_join(
    cdm$outcome_characterisation_postpartum %>% 
      mutate(
        pregnancy_start_band = if_else(
          day(pregnancy_start_date) <= 15, 
          paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
          paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
        ),
        pregnancy_end_band = if_else(
          day(pregnancy_end_date) <= 15, 
          paste0("01", month(pregnancy_end_date), year(pregnancy_end_date)),
          paste0("02", month(pregnancy_end_date), year(pregnancy_end_date))
        )
      ) |>
      select(
        "target_subject_id" = "subject_id", "outcome_date" = "cohort_start_date", 
        "maternal_age", "cohort_definition_id", "cohort_name", "pregnancy_start_band",
        "pregnancy_end_band"
      ),
    by = c("maternal_age", "pregnancy_start_band", "pregnancy_end_band"),
    relationship = "many-to-many"
  ) |>
  compute(name = "outcome_match_postpartum", temporary = FALSE) |>
  # postpartum period during outcome 
  mutate(
    postpartum_end_date = if_else(
      cohort_name == "postpartum_haemorrhage", 
      postpartum_end_date_12,
      postpartum_end_date_6
    )
  ) |>
  filter(pregnancy_end_date <= outcome_date & postpartum_end_date >= outcome_date) |>
  slice_sample(
    n =  1,
    by = all_of(c("cohort_definition_id", "cohort_name", "target_subject_id", "cohort_name"))
  ) |>
  mutate(cohort_start_date = outcome_date, cohort_end_date = outcome_date) |>
  compute(name = "outcome_match_postpartum", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$outcome_characterisation_postpartum) |>
      mutate(cohort_name = paste0(cohort_name, "_matched")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )
## Postpartum outcome sample
cdm$outcome_sample_postpartum <- cdm$outcome_characterisation_postpartum |>
  inner_join(
    cdm$outcome_match_postpartum  |>
      distinct(target_subject_id) |>
      rename("subject_id" = "target_subject_id"),
    by = "subject_id"
  ) |>
  compute(name = "outcome_sample_postpartum", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$outcome_characterisation_postpartum) |>
      mutate(cohort_name = paste0(cohort_name, "_sampled")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )
# Maternal death match ----
info(logger, "- Match maternal death outcome cohort")
## Maternal death outcome match
cdm$outcome_match_mdeath <- cdm$overall_period |>
  subsetCohorts(cohortId = "pregnancy_episode", name = "outcome_match_mdeath") |>
  anti_join(
    cdm$mdeath_outcome_char |> distinct(subject_id), by = "subject_id"
  ) |>
  compute(name = "outcome_match_mdeath", temporary = FALSE) |>
  group_by(subject_id) |>
  arrange(pregnancy_start_date) %>% 
  mutate(
    postpartum_end_date_6 = !!dateadd("cohort_end_date", 7*6),
    pregnancy_start_date_lead = dplyr::lead(pregnancy_start_date)
  ) %>% 
  mutate(
    postpartum_end_date_6 = if_else(
      postpartum_end_date_6 < pregnancy_start_date_lead | is.na(pregnancy_start_date_lead), 
      postpartum_end_date_6, 
      as.Date(!!dateadd("pregnancy_start_date_lead", -1))
    ),
    pregnancy_start_band = if_else(
      day(pregnancy_start_date) <= 15, 
      paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
      paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
    ),
    pregnancy_end_band = if_else(
      day(cohort_end_date) <= 15, 
      paste0("01", month(cohort_end_date), year(cohort_end_date)),
      paste0("02", month(cohort_end_date), year(cohort_end_date))
    )
  ) |>
  ungroup() |>
  select(
    any_of(c(
      "subject_id", "postpartum_end_date_6", "pregnancy_start_date",
      "maternal_age", "pregnancy_start_band", "pregnancy_end_band", 
      "pregnancy_end_date" = "cohort_end_date",
      "season_yearly", "season", "ethnicity", "socioeconomic_status"
    ))
  )  |>
  compute(name = "outcome_match_mdeath", temporary = FALSE) |>
  inner_join(
    cdm$mdeath_outcome_char %>% 
      mutate(
        pregnancy_start_band = if_else(
          day(pregnancy_start_date) <= 15, 
          paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
          paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
        ),
        pregnancy_end_band = if_else(
          day(pregnancy_end_date) <= 15, 
          paste0("01", month(pregnancy_end_date), year(pregnancy_end_date)),
          paste0("02", month(pregnancy_end_date), year(pregnancy_end_date))
        )
      ) |>
      select(
        "target_subject_id" = "subject_id", "outcome_date" = "cohort_start_date", 
        "maternal_age", "cohort_definition_id", "cohort_name", "pregnancy_start_band",
        "pregnancy_end_band"
      ),
    by = c("maternal_age", "pregnancy_start_band", "pregnancy_end_band"),
    relationship = "many-to-many"
  ) |>
  compute(name = "outcome_match_mdeath", temporary = FALSE) |>
  # pregnancy or postpartum period during outcome 
  filter(pregnancy_start_date <= outcome_date & postpartum_end_date_6 >= outcome_date) |>
  slice_sample(
    n =  1,
    by = all_of(c("cohort_definition_id", "cohort_name", "target_subject_id", "cohort_name"))
  ) |>
  mutate(cohort_start_date = outcome_date, cohort_end_date = outcome_date) |>
  compute(name = "outcome_match_mdeath", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mdeath_outcome_char) |>
      mutate(cohort_name = paste0(cohort_name, "_matched")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )
## Maternal death outcome sample
cdm$outcome_sample_mdeath <- cdm$mdeath_outcome_char |>
  inner_join(
    cdm$outcome_match_mdeath  |>
      distinct(target_subject_id) |>
      rename("subject_id" = "target_subject_id"),
    by = "subject_id"
  ) |>
  compute(name = "outcome_sample_mdeath", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$mdeath_outcome_char) |>
      mutate(cohort_name = paste0(cohort_name, "_sampled")), 
    cohortAttritionRef = NULL,
    .softValidation = TRUE
  )

# Bind all ----
cdm <- bind(
  cdm$outcome_match_pregnancy, cdm$outcome_sample_pregnancy, cdm$outcome_characterisation_pregnancy,
  cdm$outcome_match_postpartum, cdm$outcome_sample_postpartum, cdm$outcome_characterisation_postpartum,
  cdm$outcome_match_mdeath, cdm$outcome_sample_mdeath, cdm$mdeath_outcome_char,
  name = "outcome_characterisation"
)
