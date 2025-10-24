# Characterisation ----
## Matching ----
info(logger, "- Matching")
cdm$characteristics_aesi_30 <- getMatchedCohort(cdm$ir_aesi_30, settings(cdm$aesi_30)$cohort_name, name = "characteristics_aesi_30")
cdm$characteristics_aesi_inf <- getMatchedCohort(cdm$ir_aesi_inf, settings(cdm$aesi_inf)$cohort_name, name = "characteristics_aesi_inf")
cdm$characteristics_aesi_90 <- getMatchedCohort(cdm$ir_aesi_90, settings(cdm$aesi_90)$cohort_name, name = "characteristics_aesi_90")
cdm$characteristics_aesi_180 <- getMatchedCohort(cdm$ir_aesi_180, settings(cdm$aesi_180)$cohort_name, name = "characteristics_aesi_180")
cdm$characteristics_mae <- getMatchedCohort(cdm$ir_mae, c("antepartum_haemorrhage", "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"), name = "characteristics_mae")
cdm$characteristics_maternal_death <- getMatchedCohort(cdm$ir_maternal_death, "maternal_death", name = "characteristics_maternal_death")
cdm$characteristics_postpartum_endometritis <- getMatchedCohort(cdm$ir_postpartum_endometritis, "postpartum_endometritis", name = "characteristics_postpartum_endometritis")
cdm$characteristics_postpartum_haemorrhage <- getMatchedCohort(cdm$ir_postpartum_haemorrhage, "postpartum_haemorrhage", name = "characteristics_postpartum_haemorrhage")
cdm$characteristics_preterm_labour <- getMatchedCohort(cdm$ir_preterm_labour, "preterm_labour", name = "characteristics_preterm_labour")
if (grepl("miscarriage", settings(cdm$mae)$cohort_name)){
 cdm$characteristics_miscarriage <- getMatchedCohort(cdm$ir_miscarriage, c("miscarriage", "miscarriage_codelist"), name = "characteristics_miscarriage")
} else {
 cdm$characteristics_miscarriage <- omopgenerics::emptyCohortTable(cdm = cdm , name = "characteristics_miscarriage")
}
if ("stillbirth" %in% settings(cdm$mae)$cohort_name){
 cdm$characteristics_stillbirth <- getMatchedCohort(cdm$ir_stillbirth, "stillbirth", name = "characteristics_stillbirth")
} else {
 cdm$characteristics_stillbirth <- omopgenerics::emptyCohortTable(cdm = cdm , name = "characteristics_stillbirth")
}

cdm <- bind(
  cdm$characteristics_aesi_30, cdm$characteristics_aesi_90, cdm$characteristics_aesi_inf, 
  cdm$characteristics_aesi_180, cdm$characteristics_mae, cdm$characteristics_maternal_death,
  cdm$characteristics_postpartum_endometritis, cdm$characteristics_postpartum_haemorrhage, 
  cdm$characteristics_preterm_labour, cdm$characteristics_miscarriage, cdm$characteristics_stillbirth, 
  cdm$pregnancy_denominator,
  name = "characteristics_br"
)

## Characterisation ----
info(logger, "- Characterisation")
cdm$characteristics_br <- cdm$characteristics_br %>% 
  mutate(trimester = !!datediff("pregnancy_start_date", "cohort_start_date")) |>
  mutate(
    trimester = case_when(
      cohort_start_date > pregnancy_end_date ~ "Postpartum",
      trimester >= 0 & trimester <= 90 ~ "Trimester 1",
      trimester >= 91 & trimester <= 180 ~ "Trimester 2",
      trimester >= 181 ~ "Trimester 3"
    )
  ) |>
  compute(name = "characteristics_br", temporary = FALSE) |>
  addSeason()

strata <- c(list("trimester"), strata)

info(logger, "- 1. Baseline Characterisation")
baselineChars <- getBRCharacteristics(cdm$characteristics_br, strata = strata)
info(logger, "- 2. Large Scale Characterisation")
lsChars <- summariseLargeScaleCharacteristics(
  cohort = cdm$characteristics_br,
  strata = strata,
  window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365), c(366, Inf)),
  eventInWindow = "condition_occurrence",
  episodeInWindow = "drug_exposure",
  indexDate = "cohort_start_date",
  censorDate = NULL,
  includeSource = FALSE,
  minimumFrequency = 0.05,
  excludedCodes = c(0)
)

## Export ----
info(logger, "- Export results")
exportSummarisedResult(baselineChars, lsChars, path = output_folder, fileName = paste0("background_characteristics_", cdmName(cdm), ".csv"))
