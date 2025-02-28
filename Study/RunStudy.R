output_folder <- here(results)
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
samplig_fraction <- 3

# Database snapshot:
readr::write_csv(CDMConnector::snapshot(cdm), here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

if (runInstantiateCohorts) {
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("Analysis", "01_instantiateCohorts.R"))
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
        "mother_table", "base", "covid_vaccines", "covid_vaccines_dose", "source_population", 
        "covid", "smoking", "covariates_inf", "covariates_5", "covariates_1", "other_vaccines",
        "covid_test", "thrombocytopenia", "aesi90", "aesi30", "aesi_inf", "nco", "covid_washout",
        "aesi90_washout", "aesi30_washout"
      ),
      .softValidation = TRUE
    )
    
  }
  info(logger, "STEP 2 RISK SET SAMPLING ----")
  source(here("Analysis", "02_riskSetSampling.R"))
}


# info(logger, "STEP XXX ZIP RESULTS ----")
# output_folder <- basename(output_folder)
# zip(
#   zipfile = paste0(output_folder, "_", gsub("-", "", Sys.Date()), ".zip"),
#   files = list.files(output_folder, full.names = TRUE)
# )
# 
# dbDisconnect(db)
# 
# info(logger, " -- DONE! --")
