library(DBI)
library(here)
library(zip)
library(dbplyr)
library(dplyr)
library(CDMConnector)
library(readr)
library(PatientProfiles)
library(log4r)
library(SqlRender)
library(omopgenerics)
library(CohortConstructor)
library(PhenotypeR)
library(tidyr)

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
achilles_database_schema <- "results"

# cohort stem where cohorts will be instantiated
table_stem <- "nmb_safe_ph"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = tolower(table_stem),
  achillesSchema = achilles_database_schema,
  cdmName = database_name,
  .softValidation = TRUE
)

# Pregnancy tables details:
mother_table_schema <- "results"
mother_table_name <- "pregnancy_episode"

# minimum counts to report
minimum_counts <- 5

# Diagnostics
source(here("Scripts", "InstantiateCohorts.R"))
source(here("Scripts", "Diagnostics.R"))

# Drop cohorts
# cdm <- cdmFromCon(
#   con = db,
#   cdmSchema = cdm_database_schema,
#   writeSchema = results_database_schema,
#   cdmName = database_name,
#   .softValidation = TRUE
# )
# cdm <- dropSourceTable(cdm = cdm, name = starts_with(table_stem))

# Disconnect
cdmDisconnect(cdm)

# Done!
print("Thanks for running the study!")
