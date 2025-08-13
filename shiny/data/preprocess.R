# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  incidence_rate_ratio = list(result_type = "incidence_rate_ratio"),
  propensity_score_coeficcients = list(result_type = "propensity_score_coeficcients"),
  propensity_scores = list(result_type = "propensity_scores"),
  summarise_sampling = list(result_type = "summarise_sampling"),
  summarise_standardised_mean_differences = list(result_type = "summarise_standardised_mean_differences"),
  cohort_exit = list(result_type = "cohort_exit"),
  gestational_time_distributions = list(result_type = "gestational_time_distributions"),
  negative_control_outcomes = list(result_type = "negative_control_outcomes")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
attr(result, "settings") <- omopgenerics::settings(result) |>
  dplyr::mutate(
    table_name = dplyr::case_when(
      table_name == "aesi_30" ~ "AESI recurrent",
      table_name == "aesi_90" ~ "AESI acute",
      table_name == "aesi_inf" ~ "AESI chronic",
      table_name == "covariates_1" ~ "Covariates at 1 year prior",
      table_name == "covariates_5" ~ "Covariates at 5 years prior",
      table_name == "covariates_inf" ~ "Covariates anytime prior",
      table_name == "covid" ~ "COVID-19",
      table_name == "covid_vaccines" ~ "COVID-19 vaccines",
      table_name == "covid_vaccines_dose" ~ "COVID-19 vaccines by dose",
      table_name == "nco" ~ "NCO",
      table_name == "mae" ~ "MAE",
      # table_name == "study_population" & result_type %in% c("summarise_cohort_count", "summarise_cohort_attrition") ~ "Study population",
      .default = stringr::str_to_sentence(gsub("_", " ", table_name))
    )
  )

# idsCohortSum <- omopgenerics::settings(result) |> 
#   dplyr::filter(result_type %in% c("summarise_cohort_attrition", "summarise_cohort_count"))
# result <- resultCrude |>
#   dplyr::filter(!(group_level %in% paste0("source_", 1:3) & result_id %in% idsCohortSum))

data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
