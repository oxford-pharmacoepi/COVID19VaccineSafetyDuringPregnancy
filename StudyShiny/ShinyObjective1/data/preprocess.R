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
  "postpartum_endometritis", "postpartum_haemorrhage", "postpartum_haemorrhage_sens"
)

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  dplyr::mutate(cdm_name = gsub("v2", "", .data$cdm_name), cdm_name = gsub("@UiO", "", .data$cdm_name)) 
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
    ),
    group_level = dplyr::if_else(group_level == "overall_denominator", "pregnancy_and_12_weeks_postpartum", .data$group_level),
    strata_level = dplyr::case_when(
      strata_level == "0" ~ "No previous pregnancy",
      strata_level == "1" ~ "≥1 previous pregnancy",
      .default = .data$strata_level
    ), 
    dplyr::across(
      c("strata_level", "variable_level"), 
      ~ dplyr::case_when(
        .x == "Q1" & cdm_name == "SCIFI-PEARL" ~ "Q5 (most disadvantaged)",
        .x == "Q2" & cdm_name == "SCIFI-PEARL" ~ "Q4",
        .x == "Q4" & cdm_name == "SCIFI-PEARL" ~ "Q2",
        .x == "Q5" & cdm_name == "SCIFI-PEARL" ~ "Q1 (least disadvantaged)",
        .x == "0N" | .x == "0n" ~ "Missing",
        .x == "Q5" ~ "Q5 (most disadvantaged)",
        .x == "Q1" ~ "Q1 (least disadvantaged)",
        .x == "U1" ~ "Q1 (least disadvantaged)",
        .x == "U2" ~ "Q2",
        .x == "U3" ~ "Q3",
        .x == "U4" ~ "Q4",
        .x == "U5" ~ "Q5 (most disadvantaged)",
        .default = .x
      )
    )
  )  |>
  dplyr::filter(!(cdm_name == "CPRD GOLD" & group_level %in% c("miscarriage", "dysfunctional_labour"))) |>
  dplyr::filter(!(cdm_name == "SCIFI-PEARL" & group_level %in% "ectopic_pregnancy")) |>
  dplyr::filter(!(cdm_name == "CPRD GOLD" & variable_level %in% c("miscarriage", "dysfunctional_labour"))) |>
  dplyr::filter(!(cdm_name == "SCIFI-PEARL" & variable_level %in% "ectopic_pregnancy")) |>
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
data$summarise_characteristics <- data$summarise_characteristics |>
  filter(variable_level != "Previous maternal death") |>
  dplyr::mutate(
    variable_level = dplyr::case_when(
      .data$variable_name %in% c(
        "Eclampsia", "Ectopic pregnancy", "Gestational diabetes", "Hellp",
        "Preeclampsia", "Dysfunctional labour", "Antepartum haemorrhage",
        "Postpartum endometritis","Postpartum haemorrhage", "Miscarriage",
        "Stillbirth", "Preterm labour", "Maternal death"
      ) ~ omopgenerics::toSnakeCase(.data$variable_name),
      .data$variable_name %in% c(
        "Previous eclampsia", "Previous ectopic pregnancy", "Previous gestational diabetes", "Previous hellp",
        "Previous preeclampsia", "Previous dysfunctional labour", "Previous antepartum haemorrhage",
        "Previous postpartum endometritis","Previous postpartum haemorrhage", "Previous miscarriage",
        "Previous stillbirth", "Previous preterm labour"
      ) ~ omopgenerics::toSnakeCase(gsub("Previous ", "", .data$variable_name)),
      .default = .data$variable_level
    ), 
    variable_name = dplyr::case_when(
      .data$variable_name %in% c(
        "Eclampsia", "Ectopic pregnancy", "Gestational diabetes", "Hellp",
        "Preeclampsia", "Dysfunctional labour", "Antepartum haemorrhage",
        "Postpartum endometritis","Postpartum haemorrhage", "Miscarriage",
        "Stillbirth", "Preterm labour", "Maternal death"
      ) ~ "Maternal Adverse Events during pregnancy",
      .data$variable_name %in% c(
        "Previous eclampsia", "Previous ectopic pregnancy", "Previous gestational diabetes", "Previous hellp",
        "Previous preeclampsia", "Previous dysfunctional labour", "Previous antepartum haemorrhage",
        "Previous postpartum endometritis","Previous postpartum haemorrhage", "Previous miscarriage",
        "Previous stillbirth", "Previous preterm labour", "Previous maternal death"
      ) ~ "Maternal Adverse Events during previous pregnancy",
      .data$variable_name == "Pre pregnancy smoking" ~ "Pre-Pregnancy Smoke",
      .data$variable_name == "Prev pregnancy parity" ~ "Previous pregnancy parity",
      .default = .data$variable_name
    )
  ) |>
  dplyr::filter(!(cdm_name == "CPRD GOLD" & variable_level %in% c("miscarriage", "dysfunctional_labour"))) |>
  dplyr::filter(!(cdm_name == "SCIFI-PEARL" & variable_level %in% "ectopic_pregnancy")) 

values <- getValues(result, resultList)
values$survival_target_cohort <- unique(gsub("2_", "_12_", gsub("_1", "", values$survival_target_cohort)))
values$survival_target_cohort <- values$survival_target_cohort[values$survival_target_cohort != "overall_denominator"]
values$summarise_characteristics_variable_name <- c(
  "Number records", "Number subjects", "Trimester", "Maternal age group", 
  "Maternal age group narrow", "In vitro fertilisation", 
  "Pregnancy induced hypertension", "Age", 
  "Pregnancy start period", "Socioeconomic status", "Nationallity", "Season",
  "Ethnicity", "Birth continent",
  "Maternal age", "Pre-Pregnancy Smoke", "Previous pregnancies", 
  "Pregnancy outcome study", "Previous pregnancy parity", 
  "Pregnancy mode delivery", "Pregnancy single",
  "Maternal Adverse Events during pregnancy", 
  "Maternal Adverse Events during previous pregnancy",
  "Prior observation", "Days in cohort", "History of comorbidities",
  "Covariates in the past year", "Medications in the past 6 months"
)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)
selected$survival_target_cohort <- "pregnancy_denominator"

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
