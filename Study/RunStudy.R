output_folder <- here::here(results)
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(results, paste0("log", "_", gsub("-", "", Sys.Date()), ".txt"))
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS ----")

# Load study functions and parameters
info(logger, "Load study functions and parameters")
source(here("Analysis", "functions.R"))
dataCutDate <- cdm$observation_period |> dplyr::pull("observation_period_end_date") |> max()
enrollment1 <- as.Date(c("2021-04-01", "2022-02-28"))
enrollment2 <- as.Date(c("2021-05-01", "2022-03-31"))
enrollment3 <- c(as.Date("2021-10-01"), dataCutDate - lubridate::years(1))
set.seed(123)

# Hand-picked PS variables + confounders with SMD > 0.1
covariatesPS <- getCovariateList(cdm)

if (sensitvitySCIFIPEARL) {
  locations <- readr::read_csv(here::here("Data", "locations_sweden.csv"))
  cdm$person <- cdm$person |>
    inner_join(locations |> select("location_id"), by = "location_id", copy = TRUE) |>
    compute()
  subjects <- cdm$person |> distinct(person_id) |> compute()
  cdm$observation_period <- cdm$observation_period |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$observation <- cdm$observation |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$condition_occurrence <- cdm$condition_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$drug_exposure <- cdm$drug_exposure |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$measurement <- cdm$measurement |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$visit_occurrence <- cdm$visit_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$procedure_occurrence <- cdm$procedure_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
}

# Database snapshot:
summariseOmopSnapshot(cdm) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cdm_snapshot_", cdmName(cdm), ".csv"))

if (runInstantiateCohorts) {
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("Analysis", "01_InstantiateCohorts.R"))
}

if (runRiskSetSampling) {
  if (!runInstantiateCohorts) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "base", "covid_vaccines", "covid_vaccines_dose", 
        "covid", "covariates_inf", "covariates_5", "other_vaccines",
        "covid_test", "aesi_90", "aesi_30", "aesi_inf", "nco", "covid_washout",
        "aesi_90_washout", "aesi_30_washout", "covid_vaccines_booster", "mae",
        "mae_washout", "comedications", "non_mrna_covid_vaccines",
        "non_mrna_covid_vaccines", "source_population"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 2 RISK SET SAMPLING ----")
  source(here("Analysis", "02_RiskSetSampling.R"))
}

if (runPSWeighting) {
  if (!runRiskSetSampling) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "base", "covid_vaccines", "covid_vaccines_dose", 
        "covid", "covariates_inf", "covariates_5", "other_vaccines",
        "covid_test", "aesi_90", "aesi_30", "aesi_inf", "nco", "study_population", 
        "features", "mae", "comedications", "source_population"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 3 PROPENSITY SCORE WEIGHTING ----")
  source(here("Analysis", "03_PSWeighting.R"))
}

if (runOutcomeModel) {
  if (!runPSWeighting) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "base", "covid_vaccines", "covid_vaccines_dose", 
        "covid", "covariates_inf", "covariates_5", "other_vaccines",
        "covid_test", "aesi_90", "aesi_30", "aesi_inf", "nco", "covid_washout", 
        "aesi_90_washout", "aesi_30_washout", "study_population", 
        "mae", "features", "comedications", "source_population"
      ),
      .softValidation = TRUE
    )
    load(here::here(output_folder, "lasso.RData"))
  }
  info(logger, "STEP 4 OUTCOME MODEL ----")
  source(here("Analysis", "04_OutcomeModel.R"))
}

if (runBackgroundRates) {
  if (!runInstantiateCohorts | !runOutcomeModel) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "aesi_90", "aesi_30", "aesi_inf", "mae", "comedications",
        "covariates_inf", "covariates_5", "other_vaccines", 
        "covid", "covid_test", "base", "thrombocytopenia", "aesi"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 5 BACKGROUND RATES ----")
  source(here("Analysis", "05_BackgroundRates.R"))
}

if (runBRCharacteristics) {
  if (!runBackgroundRates) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "aesi", "mae", "comedications",
        "covariates_inf", "covariates_5", "other_vaccines",
        "covid", "covid_test", "ir_aesi_30", "ir_aesi_inf", "ir_aesi_90", "ir_aesi_180",
        "ir_mae", "ir_maternal_death", "ir_postpartum_endometritis",
        "ir_postpartum_haemorrhage", "ir_preterm_labour", "ir_miscarriage", "ir_stillbirth"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 6 BACKGROUND RATES CHARACTERISTICS ----")
  source(here("Analysis", "06_BRCharacterisation.R"))
}

info(logger, "STEP 6 ZIP RESULTS ----")
output_folder <- basename(output_folder)
zip(
  zipfile = paste0(output_folder, "_", gsub("-", "", Sys.Date()), ".zip"),
  files = list.files(output_folder, full.names = TRUE)
)

dbDisconnect(db)

info(logger, " -- DONE! --")
