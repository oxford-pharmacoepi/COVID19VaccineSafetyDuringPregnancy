# RISK SET SAMPLING ----
# Potential comparator cohort
info(logger, "- Potential comparator cohort")
cdm$comparator_source <- cdm$source_population |>
  addCohortName() %>% 
  mutate(
    cohort_start_date = if_else(
      cohort_start_date < vaccine_date | is.na(vaccine_date), cohort_start_date, NA
    ),
    cohort_end_date = if_else(
      cohort_end_date <= vaccine_date | is.na(vaccine_date), cohort_end_date, as.Date(!!dateadd("vaccine_date", -1))
    )
  ) |>
  filter(!is.na(cohort_start_date) & !is.na(cohort_end_date)) |>
  compute(name = "comparator_source", temporary = FALSE)

# Potential comparator cohort
info(logger, "- Potential exposed cohort")
cdm$exposed_source <- cdm$source_population |>
  addCohortName() |>
  mutate(
    exposure_date = if_else(vaccine_date < cohort_end_date, vaccine_date, NA)
  ) |>
  filter(!is.na(exposure_date)) |>
  compute(name = "exposed_source", temporary = FALSE) |>
  newCohortTable() |>
  # No COVID-19
  requireCohortIntersect(
    targetCohortTable = "covid_washout",
    window = list(c(-90, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  ) %>% 
  # Days since last exposure 
  mutate(
    any_covid_vaccine_1 = !!datediff("any_covid_vaccine_1", "exposure_date"),
    any_covid_vaccine_2 = !!datediff("any_covid_vaccine_2", "exposure_date")
  ) |>
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & any_covid_vaccine_1 >= 20 |
      cohort_definition_id == 3 & any_covid_vaccine_2 >= 90
  ) |>
  compute(name = "exposed_source", temporary = FALSE)

# summary sampling
sampling_summary <- omopgenerics::bind(
  cdm$exposed_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "number_exposed", 
      variable_level = "before_washout-match"
    ),
  cdm$comparator_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "number_comparator", 
      variable_level = "before_washout-match"
    )
)

# Matching and washout
info(logger, "- Matching and washout")
sampling_source <- cdm$exposed_source |>
  select(
    "cohort_name", "exposed_id" = "subject_id", "exposure_date", "age", "pregnancy_start_band"
  ) |>
  left_join(
    cdm$comparator_source |>
      select(
        "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", "cohort_end_date", "age",
        "pregnancy_start_band", "any_covid_vaccine_1", "any_covid_vaccine_2" # to look back 21/90 days
      ),
    by = c("cohort_name", "age", "pregnancy_start_band"),
    relationship = "many-to-many"
  ) |>
  # filter for potential comprators
  # pregnant and exposure-free at exposure date
  filter(
    exposure_date >= cohort_start_date & exposure_date <= cohort_end_date
  ) %>% 
  # days since previous vaccine 
  mutate(
    any_covid_vaccine_1 = !!datediff("any_covid_vaccine_1", "exposure_date"),
    any_covid_vaccine_2 = !!datediff("any_covid_vaccine_2", "exposure_date")
  ) |>
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & any_covid_vaccine_1 >= 20 |
      cohort_definition_id == 3 & any_covid_vaccine_2 >= 90
  )  |>
  select(!c("any_covid_vaccine_1", "any_covid_vaccine_2")) |>
  compute(name = "sampling_source", temporary = FALSE) |>
  # no covid 90 days
  requireCohortIntersect(
    targetCohortTable = "covid_washout",
    window = list(c(-90, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  )

# PRE summary 
samplingSummary <- samplingSummary(sampling_source, "pre", sampling_summary)

# Sample 
info(logger, "- Sampling")
sampling_source <- sampling_source |>
  slice_sample(
    n =  samplig_fraction, 
    by = all_of(c("cohort_definition_id", "cohort_name", "exposed_id", "exposure_date"))
  ) |>
  compute(name = "sampling_source", temporary = FALSE)

samplingSummary <- samplingSummary(sampling_source, "post", sampling_summary)

# Study population ----
info(logger, "- Study population cohort")
cdm$study_population <- sampling_source |>
  mutate(exposed_match_id = paste0(exposed_id))|>
  pivot_longer(
    cols = c("subject_id", "exposed_id"), names_to = "exposure", values_to = "subject_id"
  ) |>
  mutate(
    exposure = if_else(exposure == "subject_id", "comparator", "exposed"),
    cohort_end_date = exposure_date
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date" = "exposure_date", 
    "cohort_end_date", "exposure", "exposed_match_id"
  ))) |>
  distinct() |>
  compute(name = "study_population", temporary = FALSE) |>
  newCohortTable(.softValidation = TRUE) |>
  recordCohortAttrition("Risk Set Sampling")

# End date (TODO: add truncating events)
info(logger, "- Study population cohort - set end dates")
cdm$study_population <- cdm$study_population |>
  addDeathDate() |>
  addFutureObservation(
    futureObservationName = "observation_end_date",
    futureObservationType = "date"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid_vaccines",
    targetCohortId = getId(cdm$covid_vaccines, "any_covid_vaccine"),
    window = c(1, Inf),
    nameStyle = "next_covid_vaccine"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid_washout",
    window = c(1, Inf),
    nameStyle = "covid_infection",
    name = "study_population"
  ) |>
  exitAtFirstDate(
    dateColumns = c("date_of_death", "next_covid_vaccine", "observation_end_date")
  )

# strata
info(logger, "- Study population cohort - set strata variables")

# characterise

# smd

# nco
