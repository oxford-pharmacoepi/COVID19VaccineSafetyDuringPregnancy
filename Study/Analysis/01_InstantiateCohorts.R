# Base cohorts subsetted to pregnant people ----
# pregnant table clean
info(logger, "- Mother table")
cdm$mother_table <- getPregnantCohorts(db, cdm, mother_table_schema, mother_table_name)
# read codes
info(logger, "- Base cohort")
csvs <- list.files(here("Codelists"))
codes <- NULL
for (csv in csvs) {
  codes <- codes |>
    union_all(read_csv(here("Codelists", csv)))
}
ncoNames <- read_csv(here("Codelists", "nco.csv")) |> 
  pull("codelist_name") |> 
  unique() |> 
  omopgenerics::toSnakeCase()
# construct codelsit
codelist <- split(codes$concept_id, codes$codelist_name)
# base cohorts subsetted
baseCodelist <- codelist[!names(codelist) %in% c("platelet_measurement")] # if any measurement
cdm$base <- conceptCohort(
  cdm = cdm,
  conceptSet = baseCodelist,
  exit = "event_start_date",
  subsetCohort = "mother_table",
  name = "base"
) 

# COVID-19 vaccines ----
info(logger, "- COVID-19 vaccines cohorts")
cdm$covid_vaccines <- subsetCohorts(
  cohort = cdm$base,
  cohortId = c("any_covid_vaccine", "moderna", "pfizer"),
  name = "covid_vaccines"
) |>
  # two records in less than 5 days --> considered same record
  CohortConstructor::padCohortEnd(days = 5) 

# Dose-specific cohorts cohorts
cdm$covid_vaccines_dose <- cdm$covid_vaccines |>
  subsetCohorts(
    cohortId = "any_covid_vaccine", 
    name = "covid_vaccines_dose"
  ) |>
  group_by(subject_id) |>
  window_order(cohort_start_date) |>
  mutate(dose = row_number()) |>
  stratifyCohorts(strata = list("dose")) |>
  left_join(
    cdm$covid_vaccines |>
      addCohortName() |>
      filter(cohort_name %in% c("pfizer", "moderna")) |>
      select(subject_id, cohort_start_date, vaccine_brand = cohort_name),
    by = c("subject_id", "cohort_start_date")
  ) |>
  mutate(vaccine_brand = if_else(is.na(.data$vaccine_brand), "other", .data$vaccine_brand)) |>
  compute(name = "covid_vaccines_dose", temporary = FALSE) |>
  group_by(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
  mutate(vaccine_brand = if_else(n()>1, "unkown", vaccine_brand)) |>
  ungroup() |>
  distinct() |>
  compute(name = "covid_vaccines_dose", temporary = FALSE) 

cdm$covid_vaccines_booster <- unionCohorts(
  cdm$covid_vaccines_dose, 
  cohortId = 3:max(settings(cdm$covid_vaccines_dose)$cohort_definition_id), 
  name = "covid_vaccines_booster", 
  cohortName = "booster"
)

# Source cohorts ----
info(logger, "- Source cohorts")
# objective-specific source populations
cdm$source_1 <- getSourcePopulation(cdm, 1, enrollment1)
cdm$source_2 <- getSourcePopulation(cdm, 2, enrollment2)
cdm$source_3 <- getSourcePopulation(cdm, 3, enrollment3)

# bind source population cohorts
cdm <- omopgenerics::bind(cdm$source_1, cdm$source_2, cdm$source_3, name = "source_population")

# add index vaccine dose date and other important covariates
cdm$source_population <- cdm$source_population |>
  mutate(cohort_start_date = as.Date(cohort_start_date)) |>
  requireSex(sex = "Female") %>% 
  filter(!!datediff("observation_period_start_date", "pregnancy_start_date") >= 365) |>
  compute(name = "source_population", temporary = FALSE) |>
  recordCohortAttrition("Prior observation requirement: 365 days") |>
  addAge(indexDate = "pregnancy_start_date", ageGroup = list(c(12, 17), c(18, 34), c(35, 55))) |>
  filter(age >= 12 & age <= 55) |>
  compute(name = "source_population", temporary = FALSE) |> 
  recordCohortAttrition("Age requirement: 12 to 55") %>% 
  mutate(
    pregnancy_start_band = if_else(
      day(pregnancy_start_date) <= 15, 
      paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
      paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
    ),
    age_group_sample = cut(age, !!seq(12, 56, 2), include.lowest = TRUE)
  ) |>
  compute(name = "source_population", temporary = FALSE)

# subset base cohorts 
cdm$base <- cdm$base |>
  inner_join(cdm$source_population |> distinct(subject_id), by = "subject_id") |>
  compute(name = "base", temporary = FALSE) |>
  recordCohortAttrition("Subset to source population")

# COVID-19 cohort ----
info(logger, "- COVID-19 cohort")
cdm$positive_covid_test <- measurementCohort(
  cdm = cdm,
  conceptSet = codelist["covid_test"],
  name = "positive_covid_test",
  valueAsConcept = c(4126681, 45877985, 9191, 45884084, 4181412, 45879438)
)
cdm$covid_diagnosis <- subsetCohorts(
  cdm$base, "covid_diagnosis", name = "covid_diagnosis"
)
cdm <- omopgenerics::bind(cdm$covid_diagnosis, cdm$positive_covid_test, name = "covid")
cdm$covid <- CohortConstructor::unionCohorts(
  cdm$covid, gap = 42, cohortName = "covid"
)

# Obesity ----
# diagnostics cohort
cdm$obesity <- conceptCohort(
  cdm = cdm, conceptSet = codelist["obesity"], name = "obesity"
)
# bmi cohort
cdm$bmi_measurement <- measurementCohort(
  cdm = cdm, conceptSet = codelist["bmi_measurement"], name = "bmi_measurement",
  valueAsNumber = list(c(30, 60))
)
# body weight cohort
cdm$body_weight <- measurementCohort(
  cdm = cdm, conceptSet = codelist["body_weight"], name = "body_weight",
  valueAsNumber = list("9529" = c(120, 200), "3195625" = c(265, 440))
)
cdm <- omopgenerics::bind(cdm$obesity, cdm$bmi_measurement, cdm$body_weight, name = "obesity")
cdm$obesity <- unionCohorts(cdm$obesity, cohortName = "obesity")

# Covariates ----
# covid test, influenza, tdap and smoking apart
covariatesInf <- c(
  "asthma", "diabetes", "essential_hypertension", "hiv", "uterus_malformations",       
  "polycystic_ovary_syndrome", "systemic_lupus_erythematosus", "thyroid_disorder",
  "epilepsy"
)
covariates5 <- c(
  "alcohol_misuse_dependence"
)
covariates1 <- c("anxiety", "depression")
## All history
cdm$covariates_inf <- cdm$base |>
  subsetCohorts(cohortId = covariatesInf, name = "covariates_inf") |>
  requireIsFirstEntry()
## 5 years
cdm$covariates_5 <-  cdm$base |>
  subsetCohorts(cohortId = covariates5, name = "covariates_5")  
cdm <- omopgenerics::bind(cdm$obesity, cdm$covariates_5, name = "covariates_5")
cdm$covariates_5 <- cdm$covariates_5 |>
  mutate(days = 365*5) |>
  getWashOut(cdm$source_population)
## 1 year
cdm$covariates_1 <-  cdm$base |>
  subsetCohorts(cohortId = covariates1, name = "covariates_1")  |>
  mutate(days = 365) |>
  getWashOut(cdm$source_population)
# other vax
cdm$other_vaccines <- cdm$base |>
  subsetCohorts(cohortId = c("influenza_vaccine", "tdap_vaccine"), name = "other_vaccines")
# covid test
cdm$covid_test <- cdm$base |>
  subsetCohorts(cohortId = "covid_test", name = "covid_test")

# Comedications ----
comedications <- c(
  "nsaids", "antidepressants", "antiepileptics", "antiinflammatory_antirheumatic", 
  "diabetes_treatments", "opioids", "treatment_acid_related_disorder"
)
cdm$comedications <- cdm$base |>
  subsetCohorts(cohortId = comedications, name = "comedications") 

# AESI ----
info(logger, "- AESI")
info(logger, "  - Thombosis with Thrombocytopenia Syndrome") 
## complex thrombocytopenia
cdm$platelet_measurement <- measurementCohort(
  cdm = cdm,
  conceptSet = codelist["platelet_measurement"],
  valueAsNumber  = list(
    "8848" = c(10, 150), "8961" = c(10, 150), "8785" = c(10, 150), "8686" = c(10, 150),
    "9444" = c(10, 150), "9254" = c(10, 150), "8816" = c(10, 150), "8647" = c(10000, 150000)
  ), 
  name = "platelet_measurement"
)
cdm$thrombocytopenia <- cdm$base |> subsetCohorts("thrombocytopenia", "thrombocytopenia")
cdm <- bind(cdm$platelet_measurement, cdm$thrombocytopenia, name = "thrombocytopenia")
cdm$thrombocytopenia <- cdm$thrombocytopenia|>
  unionCohorts(cohortName = "thrombocytopenia") |> 
  padCohortEnd(days = 90) 

cdm$tts <- cdm$base |>
  subsetCohorts(cohortId = "thrombosis", name = "tts") |> 
  padCohortEnd(days = 90) |>
  requireCohortIntersect(
    targetCohortTable = "thrombocytopenia", 
    targetEndDate = NULL,
    window = c(-10, 10)
  ) 
cdm$tts  <- cdm$tts |>
  newCohortTable(
    cohortSetRef = settings(cdm$tts) |> mutate(cohort_name = "tts")
  ) 

## AESI acute
info(logger, "  - Acute") 
cdm$aesi_90 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "myocardial_infarction", "ischaemic_stroke", "pulmonary_embolism",                    
      "bells_palsy", "guillain_barre_syndrome", "transverse_myelitis",                   
      "haemorrhagic_stroke", "encephalitis", "immune_thrombocytopenia",               
      "disseminated_intravascular_coagulation", "deep_vein_thrombosis",
      "myocarditis_or_pericarditis"
    ),
    name = "aesi_90"
  ) |> 
  padCohortEnd(days = 90) 

## AESI recurrent
info(logger, "  - Recurrent") 
cdm$aesi_30 <- cdm$base |>
  subsetCohorts(cohortId = "anaphylaxis", name = "aesi_30") |>
  padCohortEnd(days = 30)

## AESI chronic
info(logger, "  - Chronic") 
cdm$aesi_inf <- cdm$base |>
  subsetCohorts(cohortId = "narcolepsy", name = "aesi_inf") |>
  exitAtObservationEnd()

# MAE ----
info(logger, "- MAE from OMOP tables")
## From OMOP table
cdm$mae_omop <- cdm$mother_table |>
  select("subject_id", "cohort_start_date" = "pregnancy_end_date", "cohort_name" = "pregnancy_outcome_study") |>
  mutate("cohort_end_date" = .data$cohort_start_date) |>
  filter(cohort_name %in% c("preterm_labour", "miscarriage", "stillbirth")) |>
  compute(name = "mae_omop", temporary = FALSE)
settingsSQL <- cdm$mae_omop |>
  distinct(cohort_name) |> 
  mutate(cohort_definition_id = row_number() |> as.integer()) |>
  compute()
cdm$mae_omop <- cdm$mae_omop |>
  inner_join(settingsSQL, by = "cohort_name") |>
  select(omopgenerics::cohortColumns("cohort")) |>
  compute(name = "mae_omop", temporary = FALSE) |>
  newCohortTable(cohortSetRef = settingsSQL |> collect(), cohortAttritionRef = NULL)

## Maternal death
cdm$maternal_death <- deathCohort(cdm = cdm, name = "maternal_death") |>
  inner_join(
    cdm$mother_table |> 
      select("subject_id", "pregnancy_start_date", "pregnancy_end_date"), 
    by = "subject_id"
  ) %>% 
  filter(cohort_start_date <= !!dateadd("pregnancy_end_date", 7*6)) |>
  filter(cohort_start_date >= pregnancy_start_date) |>
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ) |>
  compute(name = "maternal_death", temporary = FALSE) |>
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "maternal_death"), cohortAttritionRef = NULL)
  
# From phenotype
cdm$mae_pregnancy <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "antepartum_haemorrhage",
      "gestational_diabetes", "hellp", "preeclampsia"
    ),
    name = "mae_pregnancy"
  ) |>
  startsInPregnancy() 
cdm$mother_table <- cdm$mother_table %>% 
  mutate(
    end_6 = !!CDMConnector::dateadd("pregnancy_end_date", 42),
    end_12 = !!CDMConnector::dateadd("pregnancy_end_date", 84)
  )
cdm$mae_postpartum_6weeks <- cdm$base |>
  subsetCohorts(
    cohortId = "postpartum_endometritis",
    name = "mae_postpartum_6weeks"
  ) |>
  startsInPregnancy(start = "pregnancy_end_date", end = "end_6", reason = "In the firsts 6 weeks postpartum")
cdm$mae_postpartum_12weeks <- cdm$base |>
  subsetCohorts(
    cohortId = "postpartum_haemorrhage",
    name = "mae_postpartum_12weeks"
  ) |>
  startsInPregnancy(start = "pregnancy_end_date", end = "end_12", reason = "In the firsts 12 weeks postpartum")

# Bind all
cdm <- bind(
  cdm$mae_pregnancy, cdm$mae_postpartum_6weeks, cdm$mae_postpartum_12weeks, 
  cdm$maternal_death, cdm$mae_omop, name = "mae"
)

# NCO ----
info(logger, "- NCO")
cdm$nco <- cdm$base |>
  subsetCohorts(cohortId = ncoNames, name = "nco")

# Small wash-out cohorts ----
info(logger, "- Wash-out reduced cohort")
cdm$covid_washout <- cdm$covid |>
  mutate(days = 90) |>
  getWashOut(cdm$source_population) |>
  compute(name = "covid_washout", temporary = FALSE)
cdm$aesi_90_washout <- cdm$aesi_90 |>
  mutate(days = 90) |>
  getWashOut(cdm$source_population) |>
  mutate(cohort_end_date = cohort_start_date) |>
  compute(name = "aesi_90_washout", temporary = FALSE) |>
  CohortConstructor::unionCohorts(cohortName = "aesi_90")
cdm$aesi_30_washout <- cdm$aesi_30 |>
  mutate(days = 30) |>
  getWashOut(cdm$source_population) |>
  compute(name = "aesi_30_washout", temporary = FALSE) 
cdm$mae_washout <- cdm$mae_pregnancy |> 
  unionCohorts(cohortName = "mae_during_pregnancy", name = "mae_washout")

# Export cohort counts ----
bind(
  summaryCohort(cdm$nco), summaryCohort(cdm$covariates_1), summaryCohort(cdm$covariates_5), 
  summaryCohort(cdm$covariates_inf), summaryCohort(cdm$covid), summaryCohort(cdm$covid_vaccines),
  summaryCohort(cdm$covid_vaccines_dose), summaryCohort(cdm$aesi_30), summaryCohort(cdm$aesi_90), 
  summaryCohort(cdm$aesi_inf), summaryCohort(cdm$other_vaccines), summaryCohort(cdm$source_population), 
  summaryCohort(cdm$mother_table), summaryCohort(cdm$mae)
) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cohort_summary_base_", cdmName(cdm), ".csv"))
