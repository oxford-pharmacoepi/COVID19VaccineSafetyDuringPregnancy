cdm_name <- cdmName(cdm)
output_folder <- here(paste0("Results_", cdm_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

dataCutDate <- cdm$observation_period |> dplyr::pull("observation_period_end_date") |> max()
enrollment1 <- as.Date(c("2021-04-01", "2022-02-28"))
enrollment2 <- as.Date(c("2021-05-01", "2022-03-31"))
enrollment3 <- c(as.Date("2021-10-01"), dataCutDate - lubridate::years(1))

# PhenotypeR ----
info(logger, "- PhenotypeR")
results <- phenotypeDiagnostics(
  cohort = cdm$phenotyper,
  databaseDiagnostics = TRUE,
  codelistDiagnostics = TRUE,
  cohortDiagnostics = TRUE,
  survival = FALSE,
  match = FALSE,
  matchedSample = 1000,
  populationDiagnostics = TRUE,
  populationSample = 100000,
  populationDateRange = as.Date(c("2015-01-01", NA))
)

exportSummarisedResult(results, fileName = "phenotyper_{cdm_name}_{date}.csv", path = here::here(output_folder))

# Ratio Exposed - Unexposed ----
# objective-specific source populations
info(logger, "- Getting source population 1")
cdm$source_1 <- getSourcePopulation(cdm, 1, enrollment1)
info(logger, "- Getting source population 2")
cdm$source_2 <- getSourcePopulation(cdm, 2, enrollment2)
info(logger, "- Getting source population 3")
cdm$source_3 <- getSourcePopulation(cdm, 3, enrollment3)
# bind source population cohorts
cdm <- omopgenerics::bind(cdm$source_1, cdm$source_2, cdm$source_3, name = "source_population")

CohortCharacteristics::summariseCohortAttrition(cdm$source_population) |>
  exportSummarisedResult(fileName = "attrition_source_{cdm_name}_{date}.csv", path = here::here(output_folder))

info(logger, "- Exposed:Comparator ratio")
cdm$source_population |>
  addCohortName() |>
  mutate(
    exposed = if_else(
      vaccine_date >= cohort_start_date & vaccine_date <= cohort_end_date & !is.na(vaccine_date),
      "exposed", "comparator"
    )) |>
  group_by(cohort_name, exposed) |>
  tally() |>
  collect() |>
  pivot_wider(names_from = "exposed", values_from = "n") |>
  mutate(ratio = comparator/exposed) |>
  write_csv(file = here::here(output_folder, paste0("ratio_exposed_comparator_", cdm_name, ".csv")))

