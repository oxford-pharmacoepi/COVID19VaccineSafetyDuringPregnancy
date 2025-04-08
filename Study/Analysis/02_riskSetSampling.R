# Risk Set Sampling ----
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

# Potential exposed cohort 
info(logger, "- Potential exposed cohort")
cdm$exposed_source <- cdm$source_population |>
  addCohortName() |>
  mutate(
    exposure_date = if_else(vaccine_date < cohort_end_date, vaccine_date, NA)
  ) |>
  filter(!is.na(exposure_date)) |>
  # vaccine brand and dose
  left_join(
    cdm$covid_vaccines_dose |>
      select(all_of(c(
        "subject_id", "vaccine_date" = "cohort_start_date", "vaccine_brand", "vaccine_dose" = "cohort_definition_id"
      ))) |>
      mutate(vaccine_dose = paste0("Vaccine dose: ", vaccine_dose)),
    by = c("subject_id", "vaccine_date")
  ) |>
  compute(name = "exposed_source", temporary = FALSE) |>
  newCohortTable() 

# summary sampling
sampling_summary <- omopgenerics::bind(
  cdm$exposed_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "Number exposed", 
      variable_level = "Qualifying initial records"
    ),
  cdm$comparator_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "Number comparator", 
      variable_level = "Qualifying initial records"
    )
)

# Matching and washout
info(logger, "- Matching and washout")
sampling_source <- cdm$exposed_source |>
  select(
    "cohort_name", "exposed_id" = "subject_id", "exposure_date", "age", 
    "age_group_sample", "pregnancy_start_band", "vaccine_brand", "vaccine_dose",
    "exposed_pregnancy_id" = "pregnancy_id",
    "exposed_pregnancy_start_date" = "pregnancy_start_date", 
    "exposed_pregnancy_end_date" = "pregnancy_end_date",
    "exposed_observation_start" = "observation_period_start_date",
    "exposed_observation_end" = "observation_period_end_date",
    "exposed_previous_dose" = "previous_dose",
    "exposed_pregnancy_outcome_study" = "pregnancy_outcome_study",
    "exposed_age_group" = "age_group"
  ) |>
  left_join(
    cdm$comparator_source |>
      select(
        "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", 
        "cohort_end_date", "pregnancy_start_band", "age_group_sample",
        "comparator_pregnancy_id" = "pregnancy_id",
        "comparator_pregnancy_start_date" = "pregnancy_start_date", 
        "comparator_pregnancy_end_date" = "pregnancy_end_date",
        "comparator_observation_start" = "observation_period_start_date",
        "comparator_observation_end" = "observation_period_end_date",
        "comparator_previous_dose" = "previous_dose",
        "comparator_pregnancy_outcome_study" = "pregnancy_outcome_study",
        "comparator_age_group" = "age_group"
      ),
    by = c("cohort_name", "age_group_sample", "pregnancy_start_band"),
    relationship = "many-to-many"
  ) |>
  # pregnant and exposure-free at exposure date
  filter(
    exposure_date >= cohort_start_date & exposure_date <= cohort_end_date
  )  |>
  compute(name = "sampling_source", temporary = FALSE)

## Summarise counts at matching
sampling_summary <- samplingSummary(sampling_source, "Age & gestational age matching", sampling_summary)

sampling_source <- sampling_source %>% 
  # days since previous vaccine - comparator
  mutate(
    comparator_previous_dose = !!datediff("comparator_previous_dose", "exposure_date")
  ) |>
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & comparator_previous_dose >= 16 |
      cohort_definition_id == 3 & comparator_previous_dose >= 90
  ) %>% 
  # days since previous vaccine - exposed
  mutate(
    exposed_previous_dose = !!datediff("exposed_previous_dose", "exposure_date")
  ) |>
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & exposed_previous_dose >= 16 |
      cohort_definition_id == 3 & exposed_previous_dose >= 90
  ) |>
  select(!c("age_group_sample")) |>
  compute(name = "sampling_source", temporary = FALSE) |>
  ## WASH OUT
  # No COVID-19
  requireCohortIntersect(
    targetCohortTable = "covid_washout",
    window = list(c(-90, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  )  |>
  # No acute AESI (90)
  requireCohortIntersect(
    targetCohortTable = "aesi90_washout",
    window = list(c(-90, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  ) |>
  # No recurrent AESI (30)
  requireCohortIntersect(
    targetCohortTable = "aesi30_washout",
    window = list(c(-30, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  ) |>
  # No chronic AESI (Inf)
  requireCohortIntersect(
    targetCohortTable = "aesi_inf",
    window = list(c(-Inf, 0)),
    intersections = 0,
    indexDate = "exposure_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = NULL
  ) # TODO MAE WASHOUT

# Wash-out summary 
sampling_summary <- samplingSummary(sampling_source, "Apply wash-out", sampling_summary)

# Sample 
info(logger, "- Sampling")
sampling_source <- sampling_source |>
  slice_sample(
    n =  samplig_fraction, 
    by = all_of(c("cohort_definition_id", "cohort_name", "exposed_id", "exposure_date", "exposed_pregnancy_id"))
  ) |>
  compute(name = "sampling_source", temporary = FALSE)

sampling_summary <- samplingSummary(sampling_source, "Sampling", sampling_summary)
sampling_summary |>
  exportSummarisedResult(fileName = paste0("sampling_summary", cdmName(cdm), ".csv"), path = output_folder)

# Study population ----
info(logger, "- Study population cohort")
cdm$study_population <- sampling_source |>
  mutate(exposed_match_id = exposed_id)|>
  pivot_longer(
    cols = c("subject_id", "exposed_id"), names_to = "exposure", values_to = "subject_id"
  ) |>
  mutate(
    exposure = if_else(exposure == "subject_id", "comparator", "exposed"),
    cohort_end_date = exposure_date,
    !!!datesPivotLongerExprs(
      c("pregnancy_start_date", "pregnancy_end_date", "pregnancy_id", "pregnancy_outcome_study",
        "observation_start", "observation_end", "previous_dose")
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date" = "exposure_date", 
    "cohort_end_date", "exposure", "exposed_match_id", "pregnancy_id", "vaccine_brand", "vaccine_dose",
    "pregnancy_start_date", "pregnancy_end_date", "age", "age_group", "observation_start",
    "observation_end", "previous_dose", "cohort_name", "pregnancy_outcome_study"
  ))) |>
  distinct() |>
  compute(name = "study_population", temporary = FALSE) |>
  newCohortTable(.softValidation = TRUE) |>
  recordCohortAttrition("Risk Set Sampling")

# End date (TODO: add miscarriage events - without including in exitAtFirstDate)
info(logger, "- Study population cohort - set end dates")
cdm$study_population <- cdm$study_population |>
  addDeathDate() |>
  addCohortIntersectDate(
    targetCohortTable = "covid_vaccines",
    targetCohortId = getId(cdm$covid_vaccines, "any_covid_vaccine"),
    window = c(1, Inf),
    nameStyle = "next_covid_vaccine"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid",
    window = c(1, Inf),
    nameStyle = "covid_infection",
    name = "study_population"
  ) |>
  mutate(next_covid_vaccine_comparator = if_else(exposure == "exposed", NA, next_covid_vaccine)) |>
  exitAtFirstDateStudy(
    dateColumns = c("date_of_death", "next_covid_vaccine_comparator", "observation_end"),
    endColumn = "cohort_end_date",
    keepDates = TRUE,
    reason = "exit_reason",
    name = "study_population"
  ) |>
  exitAtFirstDateStudy(
    dateColumns = c("date_of_death", "next_covid_vaccine", "covid_infection", "observation_end"),
    endColumn = "cohort_end_date_sensitivity",
    keepDates = FALSE,
    reason = "exit_reason_sensitivty",
    name = "study_population"
  ) |>
  select(!"next_covid_vaccine_comparator")

# Strata and covariates ----
info(logger, "- Study population cohort - set strata variables")
cdm$study_population <- cdm$study_population %>% 
  mutate(
    gestational_day = !!datediff("pregnancy_start_date", "cohort_start_date"),
    gestational_trimester = case_when(
      gestational_day <= 90 ~ "T1",
      gestational_day >= 181 ~ "T3",
      .default = "T2"
    )
  ) |>
  # Geographic location
  left_join(
    cdm$person |> select("subject_id" = "person_id", "care_site_id"),
    by = "subject_id"
  ) %>% 
  # previous observation and vaccines
  mutate(
    previous_observation = !!datediff("observation_start", "cohort_start_date"),
    previous_dose = if_else(previous_dose > 0, previous_dose, NA)
  ) |>
  rename("days_previous_dose" = "previous_dose") |>
  # smoking status
  addCohortIntersectDate(
    targetCohortTable = "smoking",
    indexDate = "cohort_start_date",
    censorDate = NULL,
    targetDate = "cohort_start_date",
    window = list(c(-Inf, 0)),
    nameStyle = "{cohort_name}"
  ) |>
  mutate(
    smoking_status = pmax(non_smoker, smoker, former_smoker, na.rm = TRUE),
    smoking_status = case_when(
      is.na(smoking_status) ~ "Missing/Other",
      smoking_status == .data$non_smoker ~ "Non-smoker",
      smoking_status == .data$smoker ~ "Smoker",
      smoking_status == .data$former_smoker ~ "Former smoker"
    )
  ) |>
  select(!any_of(c("non_smoker", "smoker", "former_smoker" ))) |>
  compute(name = "study_population", temporary = FALSE) |>
  # more covariates that will be used later
  addCohortIntersectCount(
    targetCohortTable = "mother_table", 
    window = list(c(-Inf, -1)), 
    nameStyle = "previous_pregnancies",
    name = "study_population"
  ) |>
  addTableIntersectCount(
    tableName = "visit_occurrence", 
    window = list(c(-365, 0)), 
    nameStyle = "previous_healthcare_visits",
    name = "study_population"
  ) |>
  addCohortIntersectFlag(
    targetCohortTable = "covariates_5", 
    window = list(c(-Inf, 0)), 
    nameStyle = "{cohort_name}",
    name = "study_population"
  ) |>
  # TODO: previous vax (pregnant and all time)
  newCohortTable(.softValidation = TRUE)

summaryCohort(cdm$study_population) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("unweighted_study_cohort_summary_", cdmName(cdm), ".csv"))

# Characterise ---- 
info(logger, "- Baseline characteristics")
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))

## table one
## TODO previou doses and previous pregannt doses (category)
baseline_characteristics <- getBaselineCharacteristics(cdm, strata, weights = NULL)

## large scale
info(logger, "- Large Scale characteristics")
cdm <- getFeaturesTable(cdm, strata)
large_scale_characteristics <- getLargeScaleCharacteristics(cdm, strata, weights = NULL)

## censoring 
info(logger, "- Censoring summary")
censoring <- summariseCohortExit(cdm = cdm, strata = strata, weights = NULL)

## index date and gestational age
timeDistribution <- summariseTimeDistribution(cdm = cdm, strata = strata, weights = NULL)

# Confounding ----
## SMD
info(logger, "- Standardised Mean Differences")
smdBinary <- summariseBinarySMD(large_scale_characteristics) |>
  filter(!is.na(estimate_value))
smdNumeric <- summariseNumericSMD(baseline_characteristics) |>
  filter(!is.na(estimate_value))
bind(baseline_characteristics, large_scale_characteristics, smdBinary, smdNumeric, censoring, timeDistribution) |>
  exportSummarisedResult(fileName = paste0("unweighted_characteristics_", cdmName(cdm), ".csv"), path = output_folder)

## NCO 
info(logger, "- Negative Control Outcomes")
cdm$study_population_nco <- cdm$study_population |>
  select(any_of(c(
    "cohort_definition_id", "cohort_name", "subject_id", "cohort_start_date", "cohort_end_date",
    "cohort_end_date_sensitivity", "exposure", "exposed_match_id", "pregnancy_id",
    unlist(strata)
  ))) |>
  compute(name = "study_population_nco", temporary = FALSE) |>
  newCohortTable(.softValidation = TRUE) |>
  addCohortIntersectDate(
    targetCohortTable = "nco",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(1, Inf),
    nameStyle = "{cohort_name}",
    name = "study_population_nco"
  )

nco_unweighted <- bind(
  estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
    end = "cohort_end_date", strata = strata, group = "cohort_name", weights = NULL
  ), 
  estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
    end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", weights = NULL
  )
)

nco_unweighted <- nco_unweighted |>
  newSummarisedResult(
    settings = settings(nco_unweighted) |> mutate(result_type = "negative_control_outcomes")
  ) |>
  suppressRiskEstimates()

nco_unweighted |> 
  exportSummarisedResult(fileName = paste0("unweighted_nco", cdmName(cdm), ".csv"), path = output_folder)
