# Cumulative incidence ----
info(logger, "Cumulative incidence")
# Prepare denominator ----
info(logger, "- Prepare denominator")
### Pregnancy and postpartum denominator for CIF
cdm$pregnancy_denominator <- cdm$mother_table |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", 
    "pregnancy_start_date", "pregnancy_end_date", "observation_period_end_date",
    "pregnancy_outcome_study"
  ))) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("pregnancy_episode")
    )
  ) |>
  #
  requireDemographics(
    ageRange = c(12, 55), sex = "Female", minPriorObservation = 365
  ) |>
  # Start in 2018 and end 9 months before end of data
  requireInDateRange(dateRange = c(as.Date("2018-01-01"), dataCutDate - lubridate::month(9))) %>% 
  
  # Add postpartum dates
  mutate(
    postpartum_6_weeks = !!dateadd("pregnancy_end_date", 6*7),
    postpartum_12_weeks = !!dateadd("pregnancy_end_date", 12*7)
  ) |>
  mutate(
    postpartum_6_weeks = if_else(
      observation_period_end_date < postpartum_6_weeks,
      observation_period_end_date,
      postpartum_6_weeks
    ),
    postpartum_12_weeks = if_else(
      observation_period_end_date < postpartum_12_weeks,
      observation_period_end_date,
      postpartum_12_weeks
    )
  ) |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    postpartum_6_weeks = if_else(
      next_pregnancy > postpartum_6_weeks | is.na(next_pregnancy),
      as.Date(postpartum_6_weeks),
      as.Date(!!dateadd("next_pregnancy", -1))
    ),
    postpartum_12_weeks = if_else(
      next_pregnancy > postpartum_12_weeks | is.na(next_pregnancy),
      as.Date(postpartum_12_weeks),
      as.Date(!!dateadd("next_pregnancy", -1))
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) %>% 
  mutate(
    trimester_1_start = cohort_start_date,
    trimester_2_start =  as.Date(!!dateadd("pregnancy_start_date", 91)),
    trimester_3_start = as.Date(!!dateadd("pregnancy_start_date", 181)),
    postpartum_6_start = pregnancy_end_date,
    postpartum_12_start = pregnancy_end_date,
    trimester_1_end = as.Date(!!dateadd("pregnancy_start_date", 90)),
    trimester_2_end =  as.Date(!!dateadd("pregnancy_start_date", 180)),
    trimester_3_end = cohort_end_date,
    postpartum_6_end = postpartum_6_weeks,
    postpartum_12_end = postpartum_12_weeks
  ) |>
  mutate(
    trimester_2_start = if_else(trimester_2_start < pregnancy_end_date, trimester_2_start, NA),
    trimester_3_start = if_else(trimester_3_start < pregnancy_end_date, trimester_3_start, NA),
    trimester_2_end = case_when(
      is.na(trimester_2_start) ~ NA,
      trimester_2_end > pregnancy_end_date ~ pregnancy_end_date,
      .default = trimester_2_end
    ),
    trimester_3_end = if_else(is.na(trimester_3_start), NA, trimester_3_end)
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  renameCohort(cohortId = 1, newCohortName = "pregnancy_denominator")

### Add strata
cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
  addAge(
    ageName = "maternal_age", 
    ageGroup = list("maternal_age_group" = list("12 to 17" = c(12, 17), "18 to 34" = c(18, 34), "35 to 55" = c(35, 55)))
  ) |>
  mutate(
    pregnancy_start_period = case_when(
      year(pregnancy_start_date) %in% 2018:2019 ~ "Pre COVID-19",
      year(pregnancy_start_date) %in% 2020:2021 ~ "COVID-19 main outbreak",
      .default = "Post COVID-19 main outbreak"
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE)

strata <- list("maternal_age_group", "pregnancy_start_period")
if (cdmName(cdm) %in% c("CPRD AURUM", "CPRD GOLD", "SIDIAP")) {
  cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
    addSocioeconomicStatus() |>
    addEthnicity() |>
    compute(name = "pregnancy_denominator", temporary = FALSE)
  strata <- c(strata, list("socioeconomic_status", "ethnicity"))
}

## Denominators ----
info(logger, "- Specific denominators")
cdm$miscarriage_denominator <- getMiscarriageDenominator(cdm$pregnancy_denominator)
cdm$stillbirth_denominator <- getStillbirhtDenominator(cdm$pregnancy_denominator)
cdm$preterm_labour_denominator <- getPretermDenominator(cdm$pregnancy_denominator)
cdm$postpartum_6_weeks_denominator <- getPostpartum6Denominator(cdm$pregnancy_denominator)
cdm$postpartum_12_weeks_denominator <- getPostpartum12Denominator(cdm$pregnancy_denominator)
cdm$maternal_death_denominator <- getMaternalDeathDenominator(cdm$pregnancy_denominator)

## AESI sensitivity wash-out ----
cdm$thrombocytopenia_180 <- cdm$thrombocytopenia |>
  padCohortEnd(days = 180, name = "thrombocytopenia_180")

cdm$tts <- cdm$base |>
  subsetCohorts(cohortId = "thrombosis", name = "tts") |>
  padCohortEnd(days = 180) |>
  requireCohortIntersect(
    targetCohortTable = "thrombocytopenia_180",
    targetEndDate = NULL,
    window = c(-10, 10)
  )
cdm$tts  <- cdm$tts |>
  newCohortTable(
    cohortSetRef = settings(cdm$tts) |> mutate(cohort_name = "tts")
  )

cdm$aesi_180 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "myocardial_infarction", "ischaemic_stroke", "pulmonary_embolism",
      "bells_palsy", "guillain_barre_syndrome", "transverse_myelitis",
      "haemorrhagic_stroke", "encephalitis", "immune_thrombocytopenia",
      "disseminated_intravascular_coagulation", "deep_vein_thrombosis",
      "myocarditis_or_pericarditis"
    ),
    name = "aesi_180"
  ) |>
  padCohortEnd(days = 180)

cdm$cnsi_180 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "bells_palsy", "encephalitis", "guillain_barre_syndrome", "transverse_myelitis"
    ),
    name = "cnsi_180"
  ) |> unionCohorts(cohortName = "central_nervous_system_immune") |>
  padCohortEnd(days = 180)

cdm <- bind(cdm$aesi_180, cdm$cnsi_180, cdm$tts, name = "aesi_180")

cdm$aesi_180 <- cdm$aesi_180 |>
  newCohortTable(
    cohortSetRef = settings(cdm$aesi_180) |> mutate(cohort_name = paste0(cohort_name, "_sens"))
  )

if (!any(grepl("_sens", settings(cdm$aesi)$cohort_name))) {
  cdm <- bind(cdm$aesi, cdm$aesi_180, name = "aesi")
}

## Summarise denominators ----
info(logger, "- Summarise cohorts")
cdm <- bind(
  cdm$pregnancy_denominator, 
  cdm$miscarriage_denominator, 
  cdm$stillbirth_denominator, 
  cdm$preterm_labour_denominator, 
  cdm$postpartum_6_weeks_denominator, 
  cdm$postpartum_12_weeks_denominator, 
  cdm$maternal_death_denominator,
  name = "denominator"
)
bind(
  summaryCohort(cdm$denominator), 
  summaryCohort(cdm$mother_table), 
  summaryCohort(cdm$aesi), 
  summaryCohort(cdm$mae), 
  summaryCohort(cdm$comedications), 
  summaryCohort(cdm$covariates_inf),
  summaryCohort(cdm$other_vaccines), 
  summaryCohort(cdm$covariates_5)
) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cohort_summary_br_", cdmName(cdm), ".csv"))

bind(
  cohortCodeUseFromCohort(cdm$aesi),
  cohortCodeUseFromCohort(cdm$mae)
) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cohort_code_use_br_", cdmName(cdm), ".csv"))

# Estimate cumulative incidence ----
info(logger, "- Estimate cumulative incidence")
### AESI
cif_aesi_30 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_30",
  outcomeWashout = 30,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_90 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_90",
  outcomeWashout = 90,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_180 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_180",
  outcomeWashout = 180,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_inf <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_inf",
  outcomeWashout = Inf,
  censorOnCohortExit = TRUE,
  strata = strata
)
### MAE pregnancy
maePregnancy <- c(
  "miscarriage", "miscarriage_codelist", "stillbirth", "antepartum_haemorrhage", 
  "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
  "gestational_diabetes", "hellp", "preeclampsia", "preterm_labour"
)
maePregnancy <- maePregnancy[maePregnancy %in% settings(cdm$mae)$cohort_name]
cif_mae_pregnancy <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = maePregnancy,
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_postpartum_12 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "postpartum_12_weeks_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "postpartum_haemorrhage",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_postpartum_6 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "postpartum_6_weeks_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "postpartum_endometritis",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_maternal_death <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "maternal_death_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "maternal_death",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)

## Export CIF ----
exportSummarisedResult(
  cif_aesi_30, cif_aesi_90, cif_aesi_180, cif_aesi_inf, cif_mae_pregnancy, 
  cif_mae_postpartum_6, cif_mae_postpartum_12,
  path = output_folder,
  fileName = paste0("cumulative_incidence_", cdmName(cdm), ".csv")
)

# Incidence Rates ----
info(logger, "Incidence Rates")
## Get time-to-event data ----
info(logger, "- Get time to event")
### AESI 30
cdm$ir_aesi_30 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_30",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_30"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_30",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-30, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_30"
  ) |>
  getTimeToEvent(washOut = 30, outcomes = settings(cdm$aesi_30)$cohort_name)

### AESI Inf
cdm$ir_aesi_inf <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_inf"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-Inf, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_inf"
  ) |>
  getTimeToEvent(washOut = 9999999, outcomes = settings(cdm$aesi_inf)$cohort_name)

### AESI 90
cdm$ir_aesi_90 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_90",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_90"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_90",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-90, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_90"
  ) |>
  getTimeToEvent(washOut = 90, outcomes = settings(cdm$aesi_90)$cohort_name)

### AESI 180
cdm$ir_aesi_180 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_180",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_180"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_180",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-180, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_180"
  ) |>
  getTimeToEvent(washOut = 180, outcomes = settings(cdm$aesi_180)$cohort_name)

### MAE 
cdm$ir_mae <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("antepartum_haemorrhage", "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_mae"
  ) |>
  getTimeToEvent(
    washOut = 0, 
    outcomes = c("antepartum_haemorrhage", "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia")
  )

### Maternal death
cdm$ir_maternal_death <- cdm$maternal_death_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("maternal_death"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_maternal_death"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("maternal_death"))

### Postpartum endometritis
cdm$ir_postpartum_endometritis <- cdm$postpartum_6_weeks_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("postpartum_endometritis"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_postpartum_endometritis"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("postpartum_endometritis"))

### Postpartum haemorrhage
cdm$ir_postpartum_haemorrhage <- cdm$postpartum_12_weeks_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("postpartum_haemorrhage"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_postpartum_haemorrhage"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("postpartum_haemorrhage"))

### Miscarriage
if (any(grepl("miscarriage", settings(cdm$mae)$cohort_name))) {
  cdm$ir_miscarriage <- cdm$miscarriage_denominator |>
    addCohortIntersectDate(
      targetCohortTable = "mae",
      targetCohortId = c("miscarriage", "miscarriage_codelist"),
      indexDate = "cohort_start_date",
      censorDate = "cohort_end_date",
      targetDate = "cohort_start_date",
      order = "first",
      window = c(0, Inf),
      nameStyle = "{cohort_name}",
      name = "ir_miscarriage"
    ) |>
    getTimeToEvent(washOut = 0,  outcomes = c("miscarriage", "miscarriage_codelist"))
}

### Stillbirth
if ("stillbirth" %in% settings(cdm$mae)$cohort_name) {
  cdm$ir_stillbirth <- cdm$stillbirth_denominator |>
    addCohortIntersectDate(
      targetCohortTable = "mae",
      targetCohortId = c("stillbirth"),
      indexDate = "cohort_start_date",
      censorDate = "cohort_end_date",
      targetDate = "cohort_start_date",
      order = "first",
      window = c(0, Inf),
      nameStyle = "{cohort_name}",
      name = "ir_stillbirth"
    ) |>
    getTimeToEvent(washOut = 0,  outcomes = c("stillbirth"))
}

### Preterm
cdm$ir_preterm_labour <- cdm$preterm_labour_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("preterm_labour"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_preterm_labour"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("preterm_labour"))

## Estimates ----
info(logger, "- Get estimates")
ir_aesi_30 <- estimateIncidenceRate(cdm$ir_aesi_30, strata, settings(cdm$aesi_30)$cohort_name)
ir_aesi_inf <- estimateIncidenceRate(cdm$ir_aesi_inf, strata, settings(cdm$aesi_inf)$cohort_name)
ir_aesi_90 <- estimateIncidenceRate(cdm$ir_aesi_90, strata, settings(cdm$aesi_90)$cohort_name)
ir_aesi_180 <- estimateIncidenceRate(cdm$ir_aesi_180, strata, settings(cdm$aesi_180)$cohort_name)
ir_mae <- estimateIncidenceRate(cdm$ir_mae, strata, c("antepartum_haemorrhage", "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"))
ir_maternal_death <- estimateIncidenceRate(cdm$ir_maternal_death, strata, "maternal_death")
ir_postpartum_endometritis <- estimateIncidenceRate(cdm$ir_postpartum_endometritis, strata, "postpartum_endometritis")
ir_postpartum_haemorrhage <- estimateIncidenceRate(cdm$ir_postpartum_haemorrhage, strata, "postpartum_haemorrhage")
ir_preterm_labour <- estimateIncidenceRate(cdm$ir_preterm_labour, strata, "preterm_labour")
if (any(grepl("miscarriage", settings(cdm$mae)$cohort_name))) {
  ir_miscarriage <- estimateIncidenceRate(cdm$ir_miscarriage, strata, c("miscarriage", "miscarriage_codelist"))
} else {
  ir_miscarriage <- NULL
}
if ("stillbirth" %in% settings(cdm$mae)$cohort_name) {
  ir_stillbirth <- estimateIncidenceRate(cdm$ir_stillbirth, strata, "stillbirth")
} else {
  ir_stillbirth <- NULL
}

## Export IR ----
exportSummarisedResult(
  ir_aesi_30, ir_aesi_90, ir_aesi_180, ir_aesi_inf, ir_mae, ir_maternal_death,
  ir_postpartum_endometritis, ir_postpartum_haemorrhage, ir_preterm_labour,
  ir_miscarriage, ir_stillbirth, 
  path = output_folder,
  fileName = paste0("incidence_rates_", cdmName(cdm), ".csv")
)
