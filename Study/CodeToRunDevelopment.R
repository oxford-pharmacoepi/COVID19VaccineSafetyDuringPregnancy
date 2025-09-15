library(DBI)
library(here)
library(zip)
library(dbplyr)
library(dplyr)
library(CDMConnector)
library(tidyr)
library(readr)
library(PatientProfiles)
library(log4r)
library(SqlRender)
library(omopgenerics)
library(CohortConstructor)
library(CohortCharacteristics)
library(CodelistGenerator)
library(OmopSketch)
library(glmnet)
library(Hmisc)
library(glue)
library(IncidencePrevalence)
library(clock)
library(purrr)
library(furrr)
library(CohortSurvival)

# Database name
database_name <- "CPRD GOLD"

# Connection details
server_dbi <- Sys.getenv("DB_SERVER_DBI_gd")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm_database_schema <- "public"
results_database_schema <- "results"

# cohort stem where cohorts will be instantiated
table_stem <- "nmb_saf"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = tolower(table_stem),
  cdmName = database_name,
  .softValidation = TRUE
)

# Pregnancy tables details:
mother_table_schema <- results_database_schema
mother_table_name <- "pregnancy_episode"

# minimum counts to report
minimum_counts <- 5

# output folder
results <- paste0("Results_", cdmName(cdm))

# Choose code to run
runInstantiateCohorts <- FALSE
runRiskSetSampling <- TRUE
runPSWeighting <- TRUE
runOutcomeModel <- TRUE
runBackgroundRates <- FALSE
runBRCharacteristics <- FALSE

source(here("RunStudy.R"))

print("Thanks for running the analysis!! :D")
