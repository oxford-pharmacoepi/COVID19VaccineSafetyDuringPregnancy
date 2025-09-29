# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  survival = list(result_type = c("survival_probability", "survival_events", "survival_summary", "survival_attrition")),
  incidence = list(result_type = "")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
incidenceID <- omopgenerics::settings(result) |> dplyr::filter(result_type == "") |> dplyr::pull("result_id")
result <- result |>
  dplyr::mutate(
    additional_name = dplyr::if_else(
      .data$result_id %in% incidenceID, "outcome_group", .data$additional_name
    ),
    additional_level = dplyr::case_when(
      .data$result_id %in% incidenceID & .data$group_level %in% c(
        "preterm_labour", "miscarriage", "stillbirth", "maternal_death", 
        "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
        "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia", 
        "postpartum_endometritis", "postpartum_haemorrhage"
      ) ~ "Maternal Adverse Events",
      .data$result_id %in% incidenceID & !.data$group_level %in% c(
        "preterm_labour", "miscarriage", "stillbirth", "maternal_death", 
        "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
        "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia", 
        "postpartum_endometritis", "postpartum_haemorrhage"
      ) ~ "Adverse Events of Special Interest",
      .default = .data$additional_level
    )
  ) |>
  omopgenerics::newSummarisedResult()

data <- prepareResult(result, resultList)
attr(data$incidence, "settings") <- attr(data$incidence, "settings")  |> dplyr::mutate(additional = "outcome_group")
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
