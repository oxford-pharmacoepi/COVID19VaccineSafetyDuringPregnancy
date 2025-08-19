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
      cohort_end_date < vaccine_date | is.na(vaccine_date), cohort_end_date, as.Date(!!dateadd("vaccine_date", -1))
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
      variable_level = "Source population"
    ),
  cdm$comparator_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "Number comparator", 
      variable_level = "Source population"
    )
)

cdm$exposed_source <- cdm$exposed_source |>
  filter(vaccine_brand %in% c("pfizer", "moderna")) |>
  compute(name = "exposed_source", temporary = FALSE) 

sampling_summary <- omopgenerics::bind(
  sampling_summary,
  cdm$exposed_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "Number exposed", 
      variable_level = "Vaccinated with recommended vaccines"
    )
)

cdm$exposed_source <- cdm$exposed_source |>
  applyPopulationWashout() %>% 
  mutate(
    previous_dose = !!datediff("previous_dose", "exposure_date")
  ) |>
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & previous_dose >= 16 |
      cohort_definition_id == 3 & previous_dose >= 90
  ) |>
  compute(name = "exposed_source", temporary = FALSE)

sampling_summary <- omopgenerics::bind(
  sampling_summary,
  cdm$exposed_source |> 
    summariseResult(group = "cohort_name") |> 
    filter(variable_name == "number records") |>
    mutate(
      variable_name = "Number exposed", 
      variable_level = "Eligible to contirbute at vaccination day"
    )
)

# Matching and washout
info(logger, "- Matching and washout")
sampling_source <- cdm$exposed_source |>
  select(
    "cohort_name", "exposed_id" = "subject_id", "exposure_date", 
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
  # control must be pregnant and exposure-free at potential index date
  filter(
    exposure_date >= cohort_start_date & exposure_date <= cohort_end_date
  )  |>
  compute(name = "sampling_source", temporary = FALSE)

## Summarise counts at matching
sampling_summary <- samplingSummary(sampling_source, "Maternal (2-year band) and gestational age (2-weeks band) matching", sampling_summary)

# Match on previous dose time
sampling_source <- sampling_source %>% 
  mutate(
    comparator_previous_dose = !!datediff("comparator_previous_dose", "exposure_date"),
    comparator_previous_dose = cut(comparator_previous_dose, !!seq(0, 90000, 90), include.lowest = TRUE),
    exposed_previous_dose = cut(exposed_previous_dose, !!seq(0, 90000, 90), include.lowest = TRUE)
  ) |>
  filter((is.na(exposed_previous_dose) & is.na(comparator_previous_dose)) | (comparator_previous_dose == exposed_previous_dose)) |>
  compute(name = "sampling_source", temporary = FALSE) 

sampling_summary <- samplingSummary(sampling_source, "Previous vaccine time-window (90-days band) matching", sampling_summary)


sampling_source <- sampling_source |> 
  # days since previous vaccine - comparator
  filter(
    cohort_definition_id == 1 |
      cohort_definition_id == 2 & comparator_previous_dose >= 16 |
      cohort_definition_id == 3 & comparator_previous_dose >= 90
  ) %>% 
  select(!c("age_group_sample")) |>
  compute(name = "sampling_source", temporary = FALSE) |>
  ## WASH OUT
  applyPopulationWashout(censorDate = "comparator_pregnancy_start_date")

# Wash-out summary 
sampling_summary <- samplingSummary(sampling_source, "Eligible to contribute at matched vaccination day", sampling_summary)

# Sample 
info(logger, "- Sampling")
sampling_source <- sampling_source |>
  slice_sample(
    n =  samplig_fraction, # TODO update based on PhenotypeR
    by = all_of(c("cohort_definition_id", "cohort_name", "exposed_id", "exposure_date", "exposed_pregnancy_id"))
  ) |>
  compute(name = "sampling_source", temporary = FALSE)

sampling_summary <- samplingSummary(sampling_source, "Sampling", sampling_summary)
sampling_summary |>
  newSummarisedResult(
    settings = settings(sampling_summary) |> mutate(result_type = "summarise_sampling")
  ) |>
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
        "observation_start", "observation_end", "previous_dose", "age_group")
    )
  ) |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date" = "exposure_date", 
    "cohort_end_date", "exposure", "exposed_match_id", "pregnancy_id", 
    "vaccine_brand", "vaccine_dose", "pregnancy_start_date", 
    "pregnancy_end_date", "age_group", "observation_start",
    "observation_end", "previous_dose", "cohort_name", "pregnancy_outcome_study"
  ))) |>
  distinct() |>
  compute(name = "study_population", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = settings(cdm$source_population) |>
      mutate(
        cohort_name = case_when(
          cohort_name == "source_population_objective_1" ~ "population_objective_1",
          cohort_name == "source_population_objective_2" ~ "population_objective_2",
          cohort_name == "source_population_objective_3" ~ "population_objective_3"
        )
      ),
    # cohortAttritionRef = attrition(cdm$source_population) |>
      # filter((reason_id < 14 & cohort_definition_id == 1) | (reason_id < 15 & cohort_definition_id != 1)),
    .softValidation = TRUE
  ) |>
  recordCohortAttrition("Risk Set Sampling")

# End date 
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
  )  |>
  mutate(
    date_of_death_1 = date_of_death,
    next_covid_vaccine_1 = next_covid_vaccine,
    observation_end_1 = observation_end
  ) |>
  exitAtFirstDateStudy(
    dateColumns = c("date_of_death", "next_covid_vaccine", "observation_end"),
    endColumn = "cohort_end_date",
    keepDates = FALSE,
    reason = "exit_reason",
    name = "study_population"
  ) |>
  rename(
    "date_of_death" = "date_of_death_1",
    "next_covid_vaccine" = "next_covid_vaccine_1",
    "observation_end" = "observation_end_1"
  ) |>
  exitAtFirstDateStudy(
    dateColumns = c("date_of_death", "next_covid_vaccine", "covid_infection", "observation_end"),
    endColumn = "cohort_end_date_sensitivity",
    keepDates = FALSE,
    reason = "exit_reason_sensitivty",
    name = "study_population"
  )  |>
  mutate(
    "exit_reason" = case_when(
      exit_reason == "date_of_death; observation_end" ~ "date_of_death",
      exit_reason == "observation_end; date_of_death" ~ "date_of_death",
      exit_reason == "next_covid_vaccine; observation_end" ~ "next_covid_vaccine",
      exit_reason == "observation_end; next_covid_vaccine" ~ "next_covid_vaccine",
      .default = exit_reason
    ),
    "exit_reason_sensitivty" = case_when(
      exit_reason_sensitivty == "date_of_death; observation_end" ~ "date_of_death",
      exit_reason_sensitivty == "observation_end; date_of_death" ~ "date_of_death",
      exit_reason_sensitivty == "next_covid_vaccine; observation_end" ~ "next_covid_vaccine",
      exit_reason_sensitivty == "observation_end; next_covid_vaccine" ~ "next_covid_vaccine",
      .default = exit_reason_sensitivty
    )
  )

# Strata and covariates ----
info(logger, "- Study population cohort - set strata and covariates")
cdm$study_population <- cdm$study_population |>
  addAge(indexDate = "pregnancy_start_date") %>% 
  mutate(
    gestational_day = !!datediff("pregnancy_start_date", "cohort_start_date"),
    gestational_trimester = case_when(
      gestational_day <= 90 ~ "T1",
      gestational_day >= 181 ~ "T3",
      .default = "T2"
    )
  ) |>
  # Geographic location
  getRegion(database_name = database_name) %>% 
  # previous observation and vaccines
  mutate(
    previous_observation = !!datediff("observation_start", "cohort_start_date"),
    previous_dose = if_else(previous_dose > 0, previous_dose, NA)
  ) |>
  rename("days_previous_dose" = "previous_dose") |>
  compute(name = "study_population", temporary = FALSE) |>
  # more covariates that will be used later
  addCohortIntersectCount(
    targetCohortTable = "mother_table", 
    indexDate = "pregnancy_start_date",
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
  # obesisty, and alcohol and substance missuse dependenacy - 5 years back
  addCohortIntersectFlag(
    targetCohortTable = "covariates_5", 
    targetCohortId = c("obesity", "alcohol_misuse_dependence"),
    window = list(c(-5*365, 0)), 
    nameStyle = "{cohort_name}",
    name = "study_population"
  ) |>
  addCohortIntersectCount(
    targetCohortTable = "covid_vaccines", 
    targetCohortId = "any_covid_vaccine",
    window = list(c(-Inf, -1)), 
    nameStyle = "previous_covid_vaccines",
    name = "study_population"
  ) |>
  addCohortIntersectCount(
    targetCohortTable = "covid_vaccines", 
    targetCohortId = "any_covid_vaccine",
    window = list(c(-Inf, -1)),
    censorDate = "pregnancy_start_date",
    nameStyle = "previous_pregnant_covid_vaccines",
    name = "study_population"
  ) |>
  mutate(
    previous_covid_vaccines = as.character(previous_covid_vaccines),
    previous_pregnant_covid_vaccines = as.character(previous_pregnant_covid_vaccines)
  ) |>
  newCohortTable(.softValidation = TRUE) |>
  addCohortName()

# Characterise ---- 
info(logger, "- Baseline characteristics")
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))

## table one
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
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid",
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
    end = "cohort_end_date", strata = strata, group = "cohort_name", 
    weights = NULL, outcomeGroup = "Negative Control Outcomes"
  ), 
  estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
    end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
    weights = NULL, outcomeGroup = "Negative Control Outcomes"
  )
)

pco_unweighted <- bind(
  estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = "covid", 
    end = "cohort_end_date", strata = strata, group = "cohort_name", 
    weights = NULL, outcomeGroup = "Positive Control Outcomes"
  ), 
  estimateSurvivalRisk(
    cohort = cdm$study_population_nco, outcomes = "covid", 
    end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
    weights = NULL, outcomeGroup = "Positive Control Outcomes"
  )
)

nco_unweighted <- bind(nco_unweighted, pco_unweighted) |>
  suppressRiskEstimates()

nco_unweighted |> 
  exportSummarisedResult(fileName = paste0("unweighted_nco_", cdmName(cdm), ".csv"), path = output_folder)