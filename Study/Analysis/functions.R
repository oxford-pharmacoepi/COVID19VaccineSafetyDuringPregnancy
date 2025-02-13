getId <- function(cohort, name) {
  settings(cohort) |> 
    filter(cohort_name %in% name) |>
    pull(cohort_definition_id)
}

getPregnantCohorts <- function(db, cdm, mother_table_schema, mother_table_name) {
  cdm$mother_table_original <- tbl(
    db, inSchema(schema = mother_table_schema, table = mother_table_name)
  ) %>% 
    {if (grepl("CPRD", database_name)) {
      rename(., "pregnancy_outcome_id" = "original_outcome")
    } else . } |>
    compute(
      name = inSchema(results_database_schema, "mother_table_original"), 
      temporary = FALSE, 
      overwrite = TRUE
    ) 
  
  cdm$mother_table <- cdm$mother_table_original |>
    mutate(
      cohort_definition_id = 1L,
      cohort_start_date = pregnancy_start_date,
      cohort_end_date = pregnancy_end_date
    ) |>
    rename("subject_id" = "person_id") |>
    compute(name = "mother_table", temporary = FALSE, overwrite = TRUE) |>
    newCohortTable(
      cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "mother_table"), 
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
    mutate(cohort_definition_id = 1) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date") %>% 
    filter(!!datediff("pregnancy_start_date", "pregnancy_end_date") < 308) |>
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
        "observation_period_end_date", 
        "overlap", "pregnancy_mode_delivery", "pregnancy_outcome_id"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records") 
  return(cdm$mother_table)
}

getSourcePopulation <- function(mother, objective, enrollment) {
  name <- paste0("source_", objective)
  cdm[[name]] <- mother |>
    mutate(cohort_definition_id = objective) |>
    # pregnancy end after enrollment start
    filter(.data$cohort_end_date > !!enrollment[1]) |>
    compute(name = name, temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = objective, cohort_name = name
      ),
      cohortAttritionRef = attrition(mother) |>
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
    ) |>
    # previous doses (any type)
    requireCohortIntersect(
      targetCohortTable = "covid_vaccines",
      window = list(c(-Inf, -1)),
      intersections = objective-1,
      targetCohortId = "any_covid_vaccine",
      indexDate = "cohort_start_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    )
  return(cdm[[name]])
}

getWashOut <- function(washout, source) {
  washout |>
    inner_join(
      source |> distinct(subject_id, pregnancy_start_date, pregnancy_end_date), 
      by = "subject_id"
    ) |> 
    filter(
      cohort_start_date >= as.Date("2021-01-01"),
      cohort_start_date >= as.Date(clock::add_days("pregnancy_start_date", .data$days)),
      cohort_start_date <= as.Date(clock::add_days("pregnancy_start_date", 12*7))
    ) |>
    select(!c("pregnancy_start_date", "pregnancy_end_date", "days")) |>
    distinct() 
}

samplingSummary <- function(sampling_source, when, results = NULL) {
  omopgenerics::bind(
    results,
    sampling_source |>
      group_by(cohort_name, exposed_id) |> 
      tally() |>
      summariseResult(group = "cohort_name", variables = "n", counts = FALSE) |>
      mutate(variable_name = "exposed:comparator", variable_level = "post_sampling"),
    sampling_source |> 
      mutate(
        subject_id = exposed_id, cohort_start_date = exposure_date, cohort_end_date = exposure_date
      ) |>
      distinct(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
      summariseResult(group = "cohort_name") |>
      filter(variable_name == "number_records") |>
      mutate(variable_name = "number_exposed", variable_level = paste0(when, "_sampling")),
    sampling_source |> 
      distinct(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
      summariseResult(group = "cohort_name") |>
      filter(variable_name == "number_records") |>
      mutate(variable_name = "number_comparator", variable_level = paste0(when, "_sampling"))
  )
}