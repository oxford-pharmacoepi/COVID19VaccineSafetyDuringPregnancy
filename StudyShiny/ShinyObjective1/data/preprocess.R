# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  incidence = list(result_type = "incidence"),
  incidence_attrition = list(result_type = "incidence_attrition")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  omopgenerics::filterSettings(!.data$table_name %in% c("nco", "source_population", "covid", "covid_vaccines", "covid_vaccines_dose", "other_vaccines")) 

data <- prepareResult(result, resultList)ç

mae <- c(
  "miscarriage", "maternal_death", "postpartum_endometritis", "postpartum_haemorrhage", 
  "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "gestational_diabetes",                  
  "hellp", "preeclampsia", "preterm_labour", "antepartum_haemorrhage"
)
x <- data$incidence |> 
  omopgenerics::splitGroup() |> 
  dplyr::mutate(
    outcome_table_name = dplyr::if_else(.data$outcome_cohort_name %in% mae, "MAE", "AESI")
  ) |>
  omopgenerics::uniteGroup(c("denominator_cohort_name", "outcome_cohort_name")) 
data$incidence <- x |>
  dplyr::select(!"outcome_table_name") |>
  omopgenerics::newSummarisedResult(
    settings = omopgenerics::settings(x) |>
      dplyr::inner_join(
        x |> dplyr::distinct(.data$result_id, .data$outcome_table_name),
        by = "result_id"
      )
  )

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
