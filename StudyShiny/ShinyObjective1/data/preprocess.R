# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  survival = list(result_type = c("survival_probability", "survival_events", "survival_summary", "survival_attrition")),
  incidence = list(result_type = "")
)

source(file.path(getwd(), "functions.R"))

mae <- c(
  "preterm_labour", "miscarriage", "miscarriage_codelist", 
  "stillbirth", "maternal_death", 'elective_termination',
  "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
  "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia", 
  "postpartum_endometritis", "postpartum_haemorrhage"
)

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  dplyr::mutate(cdm_name = gsub("v2", "", .data$cdm_name)) 
incidenceID <- omopgenerics::settings(result) |> dplyr::filter(result_type == "") |> dplyr::pull("result_id")
result <- result |>
  dplyr::mutate(
    additional_name = dplyr::if_else(
      .data$result_id %in% incidenceID, "outcome_group", .data$additional_name
    ),
    additional_level = dplyr::case_when(
      .data$result_id %in% incidenceID & .data$group_level %in% mae ~ "Maternal Adverse Events",
      .data$result_id %in% incidenceID & grepl("_sens", .data$group_level) ~ "AESI Sensitivity (180 wash-out)",
      .data$result_id %in% incidenceID & !.data$group_level %in% mae ~ "Adverse Events of Special Interest",
      .default = .data$additional_level
    )
  ) |>
  omopgenerics::newSummarisedResult()

cumulativeID <- omopgenerics::settings(result) |> dplyr::filter(result_type %in% c("survival_probability", "survival_events", "survival_summary", "survival_attrition")) |> dplyr::pull("result_id")
attr(result, "settings") <- omopgenerics::settings(result) |>
  dplyr::mutate(
    outcome_group = dplyr::case_when(
      .data$result_id %in% cumulativeID & .data$outcome %in% mae ~ "Maternal Adverse Events",
      .data$result_id %in% cumulativeID & grepl("_sens", .data$outcome) ~ "AESI Sensitivity (180 wash-out)",
      .data$result_id %in% cumulativeID & !.data$outcome %in% mae ~ "Adverse Events of Special Interest",
      .default = NA
    )
  )

data <- prepareResult(result, resultList)
attr(data$incidence, "settings") <- attr(data$incidence, "settings") |> dplyr::mutate(additional = "outcome_group")
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
