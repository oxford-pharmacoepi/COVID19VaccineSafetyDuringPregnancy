# Base cohorts subsetted to pregnant people ----
# pregnant table clean
info(logger, "- Mother table")
cdm$mother_table <- getPregnantCohorts(db, cdm, mother_table_schema, mother_table_name)
# read codes
info(logger, "- Base cohort")
csvs <- list.files(here("Codelists"))
codes <- NULL
for (csv in csvs) {
  codes <- codes |>
    union_all(read_csv(here("Codelists", csv)))
}
# construct codelsit
codelist <- split(codes$concept_id, codes$codelist_name)
# base cohorts subsetted
baseCodelist <- codelist[!names(codelist) %in% c("covid_test")]
cdm$base <- conceptCohort(
  cdm = cdm,
  conceptSet = baseCodelist,
  name = "base"
) |>
  inner_join(cdm$mother_table |> select(subject_id), by = "subject_id") |>
  compute(name = "base", temporary = FALSE) |>
  recordCohortAttrition("Subset to pregnant cohort")

# COVID-19 vaccines ----
info(logger, "- COVID-19 vaccines cohorts")
cdm$covid_vaccines <- subsetCohorts(
  cohort = cdm$base,
  cohortId = c("any_covid_vaccine", "moderna", "pfizer"),
  name = "covid_vaccines"
) |>
  # two records in less than 5 days --> considered same record
  CohortConstructor::padCohortEnd(days = 5) 

# Dose-specific cohorts cohorts
cdm$covid_vaccines_dose <- cdm$covid_vaccines |>
  subsetCohorts(
    cohortId = "any_covid_vaccine", 
    name = "covid_vaccines_dose"
  ) |>
  group_by(subject_id) |>
  window_order(cohort_start_date) |>
  mutate(dose = row_number()) |>
  stratifyCohorts(strata = list("dose")) |>
  left_join(
    cdm$covid_vaccines |>
      addCohortName() |>
      filter(cohort_name %in% c("pfizer", "moderna")) |>
      select(subject_id, cohort_start_date, brand = cohort_name),
    by = c("subject_id", "cohort_start_date")
  ) |>
  mutate(brand = if_else(is.na(.data$brand), "other", .data$brand)) |>
  compute(name = "covid_vaccines_dose", temporary = FALSE)

# COVID-19 cohort ----
info(logger, "- COVID-19 cohort")
cdm$positive_covid_test <- measurementCohort(
  cdm = cdm,
  conceptSet = codelist["covid_test"],
  name = "positive_covid_test",
  valueAsConcept = c(4126681, 45877985, 9191, 45884084, 4181412, 45879438)
)
cdm$covid_diagnosis <- subsetCohorts(
  cdm$base, "covid_diagnosis", name = "covid_diagnosis"
)
cdm <- omopgenerics::bind(cdm$covid_diagnosis, cdm$positive_covid_test, name = "covid")
cdm$covid <- CohortConstructor::unionCohorts(
  cdm$covid, gap = 42, cohortName = "covid"
)

# Source cohorts ----
info(logger, "- Source cohorts")
# objective-specific source populations
cdm$source_1 <- getSourcePopulation(cdm$mother_table, 1, enrollment1)
cdm$source_2 <- getSourcePopulation(cdm$mother_table, 2, enrollment2)
cdm$source_3 <- getSourcePopulation(cdm$mother_table, 3, enrollment3)

# bind source population cohorts
cdm <- omopgenerics::bind(cdm$source_1, cdm$source_2, cdm$source_3, name = "source_population")

# add index vaccine dose date
cdm$source_population <- cdm$source_population |>
  requireSex(sex = "Female") %>% 
  filter(!!datediff("observation_period_start_date", "pregnancy_start_date") >= 365) |>
  select(!"observation_period_start_date") |>
  compute(name = "source_population", temporary = FALSE) |>
  recordCohortAttrition("Prior observation requirement: 365 days") |>
  left_join(
    cdm$covid_vaccines_dose |>
      select(all_of(c(
        "cohort_definition_id", "subject_id", "vaccine_date" = "cohort_start_date", "brand"
      ))),
    by = c("cohort_definition_id", "subject_id")
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "covid_vaccines_dose",
    targetCohortId = getId(cdm$covid_vaccines_dose, c("any_covid_vaccine_1", "any_covid_vaccine_2")),
    window = c(-Inf, Inf),
    nameStyle = "{cohort_name}",
    name = "source_population"
  ) |>
  addAge(indexDate = "pregnancy_start_date") |>
  filter(age >= 12 & age <= 55) |>
  compute(name = "source_population", temporary = FALSE) |> 
  recordCohortAttrition("Age requirement: 12 to 55") %>% 
  mutate(
    pregnancy_start_band = if_else(
      day(pregnancy_start_date) <= 15, 
      paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
      paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
    ),
    age = cut(age, !!seq(12, 56, 2), include.lowest = TRUE)
  ) |>
  compute(name = "source_population", temporary = FALSE)

# Small wash-out cohorts ----
info(logger, "- Wash-out reduced cohort")
cdm$covid_washout <- cdm$covid |>
  mutate(days = 90) |>
  getWashOut(cdm$source_population) |>
  compute(name = "covid_washout", temporary = FALSE)
