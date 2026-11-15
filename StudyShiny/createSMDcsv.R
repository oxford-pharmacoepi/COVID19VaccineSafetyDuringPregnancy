cdm_name <- c("SIDIAP")
# csv to review ----
fileData <- file.path(getwd(), "data", "shinyData.RData")
load(fileData)
data$summarise_standardised_mean_differences |>
  omopgenerics::pivotEstimates() |>
  dplyr::mutate(asmd = abs(smd)) |>
  dplyr::filter(asmd > 0.1, .data$cdm_name %in% .env$cdm_name) |>
  dplyr::distinct(.data$group_level, .data$variable_name, .data$additional_level, .data$variable_level) |>
  dplyr::rename(
    "cohort_name" = "group_level",
    "concept_id" = "additional_level", 
    "concept_name" = "variable_name", 
    "window" = "variable_level"
  ) |>
  dplyr::filter(
    !concept_name %in% c(
      "region", "nationallity", "pre_pregnancy_smoking", "Age", "Previous healthcare visits", 
      "Covid-19 infections in the last year", "Days previous dose", "Previous pregnancies", 
      "Previous observation"
    )
  ) |>
  readr::write_csv("unbalanced_smd.csv")

# csv to add to csv in study code ----
previous <- readr::read_csv("largeScaleSMD.csv") |>
  dplyr::filter(cdm_name != "SIDIAP") 
data$summarise_standardised_mean_differences |>
  omopgenerics::pivotEstimates() |>
  dplyr::mutate(asmd = abs(smd)) |>
  dplyr::filter(asmd > 0.1, .data$cdm_name %in% .env$cdm_name) |>
  dplyr::mutate(
    additional_level = paste0(additional_level, "_", gsub("-", "m", gsub(" to ", "_", variable_level)))
  ) |>
  dplyr::select(
    "cdm_name",
    "cohort_name" = "group_level",
    "strata_name",
    "strata_level", 
    "concept_name" = "variable_name", 
    "window" = "variable_level",
    "identifier" = "additional_level",
    "smd",
    "asmd"
  ) |>
  dplyr::bind_rows(previous) |>
  readr::write_csv("largeScaleSMD.csv")
