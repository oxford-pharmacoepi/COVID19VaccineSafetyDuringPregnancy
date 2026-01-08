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
library(odbc)
library(RPostgres)
library(stringr)

# Database name
# Database names must contain the following words so database-specific code works:
# - Catalonia, Spain: SIDIAP
# - Sweden: SCIFI-PEARL
# - Norway: NLHR@UiO
# - UK: CPRD
# Additional characters are allowed (e.g. CPRD GOLD, SCIFI-PEARL-v2...)
database_name <- "..."

# Connection details
server_dbi <- Sys.getenv("...")
user <- Sys.getenv("...")
password <- Sys.getenv("...")
port <- Sys.getenv("...")
host <- Sys.getenv("...")

db <- dbConnect(
  "...",
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm_database_schema <- "..."
results_database_schema <- "..."

# cohort stem where cohorts will be instantiated
table_stem <- "..."

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = tolower(table_stem),
  cdmName = database_name,
  .softValidation = TRUE
)

# Pregnancy tables details:
mother_table_schema <- "..."
mother_table_name <- "..."


# minimum counts to report
minimum_counts <- 5

# output folder
results <- paste0("Results_", cdmName(cdm))

# NCO/PCO run
getNCO <- TRUE

# Confidence Interval Method
ci = "bootstrap" 

# Only subjects with primary care data in SCIFI-PEARL
sensitvitySCIFIPEARL <- FALSE

# Choose code to run
runInstantiateCohorts <- TRUE
runRiskSetSampling <- TRUE
runPSWeighting <- TRUE
runOutcomeModel <- TRUE
runBackgroundRates <- TRUE
runBRCharacteristics <- TRUE

source(here("RunStudy.R"))

print("Thanks for running the analysis!! :D")
