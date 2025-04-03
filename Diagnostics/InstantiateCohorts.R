
dir_sql <- file.path(here::here(), "sql_folder")
dir.create(dir_sql)
options("omopgenerics.log_sql_path" = dir_sql)

# read codelists
csvs <- list.files(here("Codelists"))
codes <- NULL
for (csv in csvs) {
  codes <- codes |>
    union_all(read_csv(here("Codelists", csv)))
}
# construct codelsit
codelist <- split(codes$concept_id, codes$codelist_name)

# diagnostics cohort
cdm$obesity <- conceptCohort(
  cdm = cdm, conceptSet = codelist["obesity"], name = "obesity"
)
# bmi cohort
cdm$bmi_measurement <- measurementCohort(
  cdm = cdm, conceptSet = codelist["bmi_measurement"], name = "bmi_measurement",
  valueAsNumber = list(c(30, 60))
)

# body weight cohort
cdm$body_weight <- measurementCohort(
  cdm = cdm, conceptSet = codelist["body_weight"], name = "body_weight",
  valueAsNumber = list("9529" = c(120, 200), "3195625" = c(265, 440))
)

# bind
cdm <- omopgenerics::bind(cdm$obesity, cdm$bmi_measurement, cdm$body_weight, name = "obesity")

# union
cdm$obesity <- unionCohorts(cdm$obesity, cohortName = "obesity")
