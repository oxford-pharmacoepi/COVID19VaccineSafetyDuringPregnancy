output_folder <- here(paste0("Results_", cdmName(cdm)))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

source(here("Scripts", "functions.R"))

# SQL ----
# dir_sql <- file.path(here::here(output_folder), "sql_diagnostics")
# dir.create(dir_sql)
# options("omopgenerics.log_sql_path" = dir_sql)
# 
# dir_explain <- file.path(here::here(output_folder), "sql_diagnostics_explain")
# dir.create(dir_explain)
# options("omopgenerics.log_sql_explain_path" = dir_explain)

# Logger ----
log_file <- here(output_folder, paste0("log", "_", gsub("-", "", Sys.Date()), ".txt"))
if (file.exists(log_file)) {
  unlink(log_file)
}
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# Codelist ----
csvs <- list.files(here("Codelists"))
codes <- NULL
for (csv in csvs) {
  codes <- codes |>
    union_all(read_csv(here("Codelists", csv)))
}

# construct codelsit
codelist <- split(codes$concept_id, codes$codelist_name)
baseCodelist <- codelist[!names(codelist) %in% c(
  "platelet_measurement", "bmi_measurement", "body_weight", "asthma", "diabetes", 
  "essential_hypertension", "hiv", "uterus_malformations", "polycystic_ovary_syndrome", 
  "systemic_lupus_erythematosus", "thyroid_disorder", "substance_misuse_dependence", 
  "anxiety", "depression"
  )] # if any measurement or non contributing cohort to phenotypeR


# Base cohorts subsetted to pregnant people ----
# pregnant table clean
info(logger, "- Mother table")
cdm$mother_table <- getPregnantCohorts(db, cdm, mother_table_schema, mother_table_name)

cdm$base <- conceptCohort(
  cdm = cdm,
  conceptSet = baseCodelist,
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
  compute(name = "covid_vaccines_dose", temporary = FALSE)

cdm$covid_vaccines_booster <- unionCohorts(
  cdm$covid_vaccines_dose, 
  cohortId = 3:max(settings(cdm$covid_vaccines_dose)$cohort_definition_id), 
  name = "covid_vaccines_booster", 
  cohortName = "booster"
)

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

# Smoking cohort ----
info(logger, "- Smoking cohort")
cdm$smoking <- cdm$base |>
  subsetCohorts(
    cohortId = c("non_smoker", "smoker", "stop_smoking"),
    name = "smoking"
  ) 
# former smoker: codes indicating former smoker (stop smoking cohort) or 
# non smoker codes after smoking codes
cdm$former_smoker <- cdm$smoking |>
  requireCohortIntersect(
    cohortId = "non_smoker",
    targetCohortTable = "smoking",
    targetCohortId = "smoker",
    intersections = c(1, Inf),
    window = c(-Inf, 0),
    name = "former_smoker"
  ) |>
  unionCohorts(cohortId = c("stop_smoking", "non_smoker"), cohortName = "former_smoker")

cdm$non_smoker <- cdm$smoking |>
  subsetCohorts(cohortId = "non_smoker", name = "non_smoker") |>
  requireCohortIntersect(
    targetCohortTable = "smoking",
    targetCohortId = c("smoker"),
    intersections = 0,
    window = c(-Inf, 0),
    name = "non_smoker"
  )

cdm <- bind(
  cdm$smoking |> subsetCohorts("smoker"),
  cdm$non_smoker,
  cdm$former_smoker,
  name = "smoking"
)

# fix for PhenotypeR:
cdm$smoking <- cdm$smoking |>
  newCohortTable(
    cohortCodelistRef = attr(cdm$smoking, "cohort_codelist") |>
      inner_join(
        settings(cdm$smoking) |> select("cohort_definition_id", "cohort_name"), 
        by = "cohort_definition_id",
        copy = TRUE
      ) |>
      filter(cohort_name == codelist_name) |>
      select(!"cohort_name")
  )

# Obesity ----
info(logger, "- Obesity cohort")
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
# bind
cdm <- omopgenerics::bind(cdm$obesity, cdm$bmi_measurement, cdm$body_weight, name = "obesity")
# union
cdm$obesity <- unionCohorts(cdm$obesity, cohortName = "obesity")

# AESI ----
info(logger, "- AESI")
info(logger, "  - Thrombocytopenia") 
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
cdm$thrombocytopenia <- cdm$thrombocytopenia |> unionCohorts(cohortName = "thrombocytopenia")

cdm$thrombosis_thrombocytopenia <- cdm$base |>
  subsetCohorts(cohortId = "thrombosis", name = "thrombosis_thrombocytopenia") |>
  requireCohortIntersect(
    targetCohortTable = "thrombocytopenia", 
    targetEndDate = NULL,
    window = c(-10, 10)
  ) 
cdm$thrombosis_thrombocytopenia  <- cdm$thrombosis_thrombocytopenia |>
  newCohortTable(
    cohortSetRef = settings(cdm$thrombosis_thrombocytopenia) |> mutate(cohort_name = "thrombosis_thrombocytopenia")
  ) |>
  requireCohortIntersect(
    targetCohortTable = "mother_table",
    window = list(c(0, 0)),
    intersections = 1,
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    censorDate = NULL
  )

## AESI acute
info(logger, "  - Acute") 
cdm$aesi90 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "myocardial_infarction", "ischaemic_stroke", "pulmonary_embolism",                    
      "bells_palsy", "guillain_barre_syndrome", "transverse_myelitis",                   
      "haemorrhagic_stroke", "encephalitis", "immune_thrombocytopenia",               
      "disseminated_intravascular_coagulation", "deep_vein_thrombosis",
      "myocarditis_or_pericarditis"
    ),
    name = "aesi90"
  ) |> 
  padCohortEnd(days = 90) |>
  requireCohortIntersect(
    targetCohortTable = "mother_table",
    window = list(c(0, 0)),
    intersections = 1,
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    censorDate = NULL
  )

## AESI recurrent
info(logger, "  - Recurrent") 
cdm$aesi30 <- cdm$base |>
  subsetCohorts(cohortId = "anaphylaxis", name = "aesi30") |>
  padCohortEnd(days = 30) |>
  requireCohortIntersect(
    targetCohortTable = "mother_table",
    window = list(c(0, 0)),
    intersections = 1,
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    censorDate = NULL
  )

## AESI chronic
info(logger, "  - Chronic") 
cdm$aesi_inf <- cdm$base |>
  subsetCohorts(cohortId = "narcolepsy", name = "aesi_inf") |>
  exitAtObservationEnd(limitToCurrentPeriod = FALSE) |>
  requireCohortIntersect(
    targetCohortTable = "mother_table",
    window = list(c(0, 0)),
    intersections = 1,
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    censorDate = NULL
  )


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
  mutate(cohort_definition_id = row_number()) |>
  compute()
cdm$mae_omop <- cdm$mae_omop |>
  inner_join(settingsSQL, by = "cohort_name") |>
  select(omopgenerics::cohortColumns("cohort")) |>
  compute(name = "mae_omop", temporary = FALSE) |>
  newCohortTable(cohortSetRef = settingsSQL |> collect(), cohortAttritionRef = NULL)

## Maternal death
cdm$maternal_death <- cdm$death |>
  rename("subject_id" = "person_id") |>
  inner_join(cdm$mother_table, by = "subject_id") %>% 
  filter(death_date <= !!dateadd("pregnancy_end_date", 7*6)) |>
  filter(death_date >= pregnancy_start_date) |>
  mutate(cohort_definition_id = 1L) |>
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date" = "pregnancy_start_date", "cohort_end_date" = "pregnancy_end_date"
  ) |>
  compute(name = "maternal_death", temporary = FALSE) |>
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "maternal_death"), cohortAttritionRef = NULL)

# From phenotype
cdm$phenotyper <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "antepartum_haemorrhage",  "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
      "gestational_diabetes", "hellp", "postpartum_endometritis", "postpartum_haemorrhage",  
      "preeclampsia", "alcohol_misuse_dependence"
    ),
    name = "phenotyper"
  )

# BIND ----
cdm <- bind(
  cdm$phenotyper, cdm$maternal_death, cdm$mae_omop, cdm$aesi30, cdm$aesi90, 
  cdm$aesi_inf, cdm$thrombosis_thrombocytopenia, cdm$smoking, cdm$obesity, 
  cdm$covid_vaccines, cdm$mother_table, 
  name = "phenotyper"
)
