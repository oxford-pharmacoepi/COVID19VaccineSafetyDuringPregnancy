getPregnantCohorts <- function(db, cdm, mother_table_schema, mother_table_name) {
  cdm$mother_table_original <- tbl(
    db, inSchema(schema = mother_table_schema, table = mother_table_name)
  ) |>
    compute(
      name = inSchema(results_database_schema, "mother_table_original"), 
      temporary = FALSE, 
      overwrite = TRUE
    ) 
  
  cdm$mother_table <- cdm$mother_table_original |>
    mutate(
      cohort_definition_id = 1L,
      cohort_start_date = pregnancy_start_date,
      cohort_end_date = pregnancy_end_date,
      person_id = as.numeric(person_id),
      pregnancy_id = as.numeric(pregnancy_id)
    ) |>
    rename("subject_id" = "person_id") |>
    compute(name = "mother_table", temporary = FALSE, overwrite = TRUE) |>
    newCohortTable(
      cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "mother_table"), 
      .softValidation = TRUE
    ) |>
    # Only pregnancies in continuous observation from start to end
    left_join(
      cdm$observation_period |>
        select(
          subject_id = person_id, observation_period_start_date, observation_period_end_date
        ),
      by = "subject_id"
    ) |>
    filter(
      pregnancy_start_date >= observation_period_start_date,
      pregnancy_end_date <= observation_period_end_date,
    ) |>
    recordCohortAttrition(reason = "Pregnancy in observation") |>
    filter(pregnancy_start_date < pregnancy_end_date) |>
    mutate(cohort_definition_id = 1L) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date") %>% 
    mutate(gestational_length = !!datediff("pregnancy_start_date", "pregnancy_end_date")) |>
    filter(gestational_length < 308) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length < 308 days") |>
    filter(gestational_length_in_day != 0) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length days != 0")
  cdm$mother_table <- cdm$mother_table |>
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      window = list(c(0, Inf)),
      indexDate = "pregnancy_start_date",
      censorDate = "pregnancy_end_date",
      targetStartDate = "pregnancy_start_date",
      targetEndDate = NULL,
      nameStyle = "overlap"
    ) |>
    filter(overlap == 1) |>
    select(
      !c(
        "gestational_length_in_day", "prev_pregnancy_gravidity", "pregnancy_single", 
        "overlap", "pregnancy_mode_delivery"
      )
    ) |>
    mutate(
      pregnancy_outcome_study = case_when(
        pregnancy_outcome == 4092289  & gestational_length <= 37*7~ "preterm_labour",
        pregnancy_outcome == 4092289 ~ "livebirth",
        pregnancy_outcome == 4067106 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 4081422 ~ "elective_termination",
        pregnancy_outcome == 4095714 ~ "discordant",
        .default = "unknown"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records") 
  
  return(cdm$mother_table)
}


getSourcePopulation <- function(cdm, objective, enrollment) {
  name <- paste0("source_", objective)
  cdm[[name]] <- cdm$mother_table |>
    mutate(cohort_definition_id = objective) |>
    # pregnancy end after enrollment start
    filter(.data$cohort_end_date > !!enrollment[1]) |>
    compute(name = name, temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = objective, cohort_name = name
      ),
      cohortAttritionRef = attrition(cdm$mother_table) |>
        mutate(cohort_definition_id = objective)
    ) |>
    recordCohortAttrition("Pregnancy end date > {enrollment[1]}") |>
    # pregnancy starts before enrollment ends
    filter(.data$cohort_start_date < !!enrollment[2]) |>
    compute(name = name, temporary = FALSE) |>
    recordCohortAttrition("Pregnancy start date < {enrollment[2]}") |>
    trimToDateRange(
      dateRange = c(enrollment[1], NA), 
      name = name
    ) 
  
  # Eligible for exposure
  if (objective == 1) {
    ## 1st Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |> 
          filter(cohort_definition_id == 1) |>
          select("subject_id", "vaccine_date" = "cohort_start_date"),
        by = "subject_id"
      ) %>% 
      filter(vaccine_date > cohort_start_date | is.na(vaccine_date)) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Unvaccinated before pregnancy start")
  }
  
  if (objective == 2) {
    ## 2nd Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |> 
          addCohortName() |>
          filter(cohort_definition_id %in% 1:2) |>
          select("subject_id", "cohort_start_date", "cohort_name") |>
          pivot_wider(names_from = "cohort_name", values_from = "cohort_start_date"),
        by = "subject_id"
      ) |>
      rename("previous_dose" = "any_covid_vaccine_1", "vaccine_date" = "any_covid_vaccine_2") |>
      filter(vaccine_date >= pregnancy_start_date | is.na(vaccine_date)) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "No 2nd dose before pregnancy") %>% 
      mutate(new_cohort_start = !!dateadd("previous_dose", 16)) |>
      filter(new_cohort_start < .data$cohort_end_date) %>% 
      mutate(
        cohort_start_date = if_else(
          new_cohort_start < cohort_start_date, cohort_start_date, new_cohort_start
        )) |>
      select(!c("new_cohort_start")) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Eligible for 2nd during pregnancy") 
  }
  
  if (objective == 3) {
    ## 2nd Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |> 
          filter(cohort_definition_id == 2) |>
          select("subject_id", "any_covid_vaccine_2" = "cohort_start_date"),
        by = "subject_id"
      ) |>
      filter(.data$any_covid_vaccine_2 < .data$cohort_end_date) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "2 doses any time before pregnancy end") |>
      addCohortIntersectDate(
        targetCohortTable = "covid_vaccines_booster",
        targetCohortId = NULL,
        indexDate = "cohort_start_date",
        censorDate = NULL,
        targetDate = "cohort_start_date",
        order = "first",
        window = c(-Inf, 0),
        nameStyle = "booster_previous",
        name = name
      ) |>
      addCohortIntersectDate(
        targetCohortTable = "covid_vaccines_booster",
        targetCohortId = NULL,
        indexDate = "cohort_start_date",
        censorDate = NULL,
        targetDate = "cohort_start_date",
        order = "first",
        window = c(0, Inf),
        nameStyle = "vaccine_date",
        name = name
      ) %>% 
      mutate(
        previous_dose = if_else(
          is.na(booster_previous), any_covid_vaccine_2, booster_previous
        )
      ) %>% 
      mutate(new_cohort_start = !!dateadd("previous_dose", 90)) |>
      filter(new_cohort_start < .data$cohort_end_date) %>% 
      mutate(
        cohort_start_date = if_else(
          new_cohort_start < cohort_start_date, cohort_start_date, new_cohort_start
        )) |>
      select(!c("any_covid_vaccine_2", "booster_previous", "new_cohort_start")) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Eligible for booster during pregnancy") 
  }
  
  return(cdm[[name]])
}

startsInPregnancy <- function(cohort, 
                              start = "pregnancy_start_date", 
                              end = "pregnancy_end_date", 
                              reason = "During pregnancy") {
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)
  cdm[[name]]  <- cdm[[name]] |> 
    dplyr::inner_join(
      cdm$mother_table |>
        dplyr::select("subject_id", start, end),
      by = "subject_id"
    ) |>
    dplyr::filter(cohort_start_date >= .data[[start]] & cohort_start_date <= .data[[end]]) |>
    dplyr::select(!dplyr::starts_with("pregnancy")) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = reason)
  return(cdm[[name]])
}
