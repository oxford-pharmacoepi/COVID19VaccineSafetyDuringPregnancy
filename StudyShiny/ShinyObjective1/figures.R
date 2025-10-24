library(bslib)
library(CohortCharacteristics)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(DT)
library(ggplot2)
library(gt)
library(IncidencePrevalence)
library(jsonlite)
library(markdown)
library(omopgenerics)
library(OmopSketch)
library(plotly)
library(purrr)
library(reactable)
library(readr)
library(rlang)
library(rsvg)
library(shiny)
library(shiny.fluent)
library(shinycssloaders)
library(shinyWidgets)
library(sortable)
library(tidyr)
library(visOmopResults)
library(yaml)
library(CohortSurvival)

# load shiny data
load(file.path(getwd(), "data", "shinyData.RData"))

# INCIDENCE ----
## Table ----
data$incidence |>
  dplyr::filter(strata_name == "overall", grepl("incidence", estimate_name)) |>
  visOmopResults::visOmopTable(
    estimateName = c(
      "Pregnancies" = "<denominator_count>",
      "Outcomes" = "<outcome_count>",
      "Outcomes in period" = "<outcome_in_period_count> (<outcome_in_period_percentage>%)",
      "Person-Days" = "<person_days_count>",
      "Person-Years" = "<person_years>",
      "Incidence (100,000 person-years)" = c("<incidence_100000_pys> (<incidence_100000_pys_95CI_lower> - <incidence_100000_pys_95CI_upper>)")
    ),
    header = c("cdm_name", "estimate_name"),
    groupColumn = "outcome_group",
    hide = c("maternal_age_group", "variable_name", "variable_level", "gestational_trimester", "gestational_trimester", "pregnancy_start_period", "socioeconomic_status", "ethnicity"),
    factor = list(
      "outcome_group" = c("Adverse Events of Special Interest", "Maternal Adverse Events"),
      "maternal_age_group" = c("overall", "12 to 17", "18 to 34", "35 to 55"),
      "pregnancy_start_period" = c("overall", "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak"),
      "socioeconomic_status" = c("overall", paste0(1:10)),
      "ethnicity" = c("overall", "White", "Black", "Asian", "Missing"),
      "gestational_trimester" = c("overall", "Trimester 1", "Trimester 2", "Trimester 3")
    )
  )
## Plot ----
incidenceTrimester <- data$incidence |>
  dplyr::filter(strata_name %in% c("gestational_trimester")) |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    outcome_group = factor(
      .data$outcome_group, 
      levels = c("Adverse Events of Special Interest", "Maternal Adverse Events")
    ),
    maternal_age_group = factor(
      maternal_age_group, levels = c("overall", "12 to 17", "18 to 34", "35 to 55")
    ),
    pregnancy_start_period = factor(
      pregnancy_start_period, levels = c("overall", "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak")
    ),
    socioeconomic_status = factor(
      socioeconomic_status, levels = c("overall", paste0(1:10))
    ),
    ethnicity = factor(
      ethnicity, levels = c("overall", "White", "Black", "Asian", "Missing")
    ),
    gestational_trimester = factor(
      gestational_trimester, levels = c("overall", "Trimester 1", "Trimester 2", "Trimester 3")
    )
  ) 

incidenceTrimester |>
  dplyr::filter(
    outcome_group == "Adverse Events of Special Interest",
    outcome_cohort_name != "central_nervous_system_immune",
  ) |>
  dplyr::arrange() |>
  visOmopResults::scatterPlot(
    x = "gestational_trimester",
    y = "incidence_100000_pys",
    line = TRUE,
    point = TRUE,
    ribbon = FALSE,
    ymin = "incidence_100000_pys_95CI_lower",
    ymax = "incidence_100000_pys_95CI_upper",
    facet = "outcome_cohort_name",
    colour = "cdm_name",
    style = "default",
    label = c('cdm_name', 'outcome_cohort_name', 'gestational_trimester', 'maternal_age_group', 'pregnancy_start_period', 'socioeconomic_status', 'ethnicity', 'variable_name', 'variable_level', 'outcome_count', 'person_days_count', 'denominator_count', 'person_years', 'incidence_100000_pys', 'incidence_100000_pys_95CI_lower', 'incidence_100000_pys_95CI_upper')
  ) + 
  # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggplot2::facet_wrap(vars(outcome_cohort_name), scales = "free_y", nrow = 3)

incidenceTrimester <- data$incidence |>
  dplyr::filter(strata_name %in% c("maternal_age_group")) |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    outcome_group = factor(
      .data$outcome_group, 
      levels = c("Adverse Events of Special Interest", "Maternal Adverse Events")
    ),
    maternal_age_group = factor(
      maternal_age_group, levels = c("overall", "12 to 17", "18 to 34", "35 to 55")
    ),
    pregnancy_start_period = factor(
      pregnancy_start_period, levels = c("overall", "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak")
    ),
    socioeconomic_status = factor(
      socioeconomic_status, levels = c("overall", paste0(1:10))
    ),
    ethnicity = factor(
      ethnicity, levels = c("overall", "White", "Black", "Asian", "Missing")
    ),
    gestational_trimester = factor(
      gestational_trimester, levels = c("overall", "Trimester 1", "Trimester 2", "Trimester 3")
    )
  ) 

incidenceTrimester |>
  dplyr::filter(
    outcome_group != "Adverse Events of Special Interest",
    outcome_cohort_name != "central_nervous_system_immune",
    !grepl("postpartum", outcome_cohort_name)
  ) |>
  dplyr::arrange() |>
  visOmopResults::scatterPlot(
    x = "maternal_age_group",
    y = "incidence_100000_pys",
    line = TRUE,
    point = TRUE,
    ribbon = FALSE,
    ymin = "incidence_100000_pys_95CI_lower",
    ymax = "incidence_100000_pys_95CI_upper",
    facet = "outcome_cohort_name",
    colour = "cdm_name",
    style = "default",
    label = c('cdm_name', 'outcome_cohort_name', 'gestational_trimester', 'maternal_age_group', 'pregnancy_start_period', 'socioeconomic_status', 'ethnicity', 'variable_name', 'variable_level', 'outcome_count', 'person_days_count', 'denominator_count', 'person_years', 'incidence_100000_pys', 'incidence_100000_pys_95CI_lower', 'incidence_100000_pys_95CI_upper')
  ) + 
  # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggplot2::facet_wrap(vars(outcome_cohort_name), scales = "free_y", nrow = 3)

incidenceTrimester <- data$incidence |>
  dplyr::filter(strata_name %in% c("pregnancy_start_period")) |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    outcome_group = factor(
      .data$outcome_group, 
      levels = c("Adverse Events of Special Interest", "Maternal Adverse Events")
    ),
    maternal_age_group = factor(
      maternal_age_group, levels = c("overall", "12 to 17", "18 to 34", "35 to 55")
    ),
    pregnancy_start_period = factor(
      pregnancy_start_period, levels = c("overall", "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak")
    ),
    socioeconomic_status = factor(
      socioeconomic_status, levels = c("overall", paste0(1:10))
    ),
    ethnicity = factor(
      ethnicity, levels = c("overall", "White", "Black", "Asian", "Missing")
    ),
    gestational_trimester = factor(
      gestational_trimester, levels = c("overall", "Trimester 1", "Trimester 2", "Trimester 3")
    )
  ) 

incidenceTrimester |>
  dplyr::filter(
    outcome_group != "Adverse Events of Special Interest",
    outcome_cohort_name != "central_nervous_system_immune",
    !grepl("postpartum", outcome_cohort_name)
  ) |>
  dplyr::arrange() |>
  visOmopResults::scatterPlot(
    x = "pregnancy_start_period",
    y = "incidence_100000_pys",
    line = TRUE,
    point = TRUE,
    ribbon = FALSE,
    ymin = "incidence_100000_pys_95CI_lower",
    ymax = "incidence_100000_pys_95CI_upper",
    facet = "outcome_cohort_name",
    colour = "cdm_name",
    style = "default",
    label = c('cdm_name', 'outcome_cohort_name', 'gestational_trimester', 'maternal_age_group', 'pregnancy_start_period', 'socioeconomic_status', 'ethnicity', 'variable_name', 'variable_level', 'outcome_count', 'person_days_count', 'denominator_count', 'person_years', 'incidence_100000_pys', 'incidence_100000_pys_95CI_lower', 'incidence_100000_pys_95CI_upper')
  ) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggplot2::facet_wrap(vars(outcome_cohort_name), scales = "free_y", nrow = 3)

# CUMULATIVE INCIDENCE ----
## Table ----
survival <- data$survival  |> 
  dplyr::filter(strata_name %in% c("gestational_trimester", "overall")) |>
  omopgenerics::filterSettings(grepl("probability", .data$result_type) | grepl("summary", .data$result_type)) |>
  dplyr::filter(estimate_name %in% c("n_events_count", "number_records_count")) 

survival <- dplyr::bind_rows(
  survival, 
  survival |>
    omopgenerics::pivotEstimates() |>
    dplyr::mutate(n_events_percentage = if_else(!is.na(n_events_count), as.character(n_events_count/number_records_count * 100), NA_character_)) |>
    dplyr::select(!c("n_events_count", "number_records_count")) |>
    dplyr::mutate(
      estimate_name = "n_events_percentage",
      estimate_type = "percentage"
    ) |>
    dplyr::rename("estimate_value" = "n_events_percentage")
)

survival |> 
  newSummarisedResult(
    settings = settings(survival) |> 
      select(!c("outcome_washout"))
  ) |>
  filter(estimate_name != "number_records_count") |>
  mutate(
    estimate_name = factor(
      .data$estimate_name,
      levels = c(
        "number_records_count", "n_events_count", "n_events_percentage"
      )),
    group_level = "pregnancy_denominator"
  ) %>%
  dplyr::arrange(.data$estimate_name) %>%
  dplyr::mutate("estimate_name" = as.character(.data$estimate_name)) |>
  dplyr::filter(!is.na(variable_name)) |>
  visOmopResults::visOmopTable(
    estimateName = c(
      "Number pregnancies" = "<number_records_count>",
      "Number outcomes" = "<n_events_count> (<n_events_percentage>%)"
    ),
    rename = c("Outcome name" = "variable_level"),
    header = c("estimate_name", "cdm_name"),
    groupColumn = "gestational_trimester",
    hide = c("variable_name", "time", "target_cohort", "maternal_age_group", "variable_name", "pregnancy_start_period", "socioeconomic_status", "ethnicity"),
    factor = list(
      "maternal_age_group" = c("overall", "12 to 17", "18 to 34", "35 to 55"),
      "pregnancy_start_period" = c("overall", "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak"),
      "socioeconomic_status" = c("overall", paste0(1:10)),
      "ethnicity" = c("overall", "White", "Black", "Asian", "Missing")
    )
  )

## Plot ----
data$survival |>
  dplyr::filter(
    variable_level %in% c(
      "preterm_labour", "miscarriage", "stillbirth", "maternal_death", 
      "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
      "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia"
    ),
  ) |>
  dplyr::filter(strata_name %in% c("gestational_trimester", "overall")) |>
  CohortSurvival::plotSurvival(
    timeScale = "days",
    ribbon = TRUE,
    facet = "outcome",
    colour = "cdm_name",
    cumulativeFailure = TRUE
  ) +
  guides(fill = "none") +
  ggplot2::facet_wrap(vars(outcome), scales = "free_y", nrow = 3) +
  geom_vline(xintercept = 90) +
  geom_vline(xintercept = 180)

data$survival |>
  dplyr::filter(
    variable_level %in% c(
      "preterm_labour", "miscarriage", "stillbirth", "maternal_death", 
      "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
      "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia"
    ),
  ) |>
  dplyr::filter(strata_name %in% c("ethnicity"), cdm_name == "CPRD GOLD") |>
  CohortSurvival::plotSurvival(
    timeScale = "days",
    ribbon = TRUE,
    facet = "outcome",
    colour = "ethnicity",
    cumulativeFailure = TRUE
  ) +
  guides(fill = "none") +
  ggplot2::facet_wrap(vars(outcome), scales = "free_y", nrow = 3) +
  geom_vline(xintercept = 90) +
  geom_vline(xintercept = 180)
