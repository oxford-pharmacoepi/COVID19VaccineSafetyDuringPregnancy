info(logger, "Characterise denominator")
# Characteristics denominator ----
cdm$overall_period <- cdm$overall_period |> 
  subsetCohorts(cohortId = "pregnancy_episode", name = "overall_period") |>
  select(!any_of(c("season", "season_yearly", "ethnicity", "socioeconomic_status"))) |>
  addSeason() |>
  addEthnicity() |>
  addSocioeconomicStatus(database = "CPRD GOLD") 
denominatorBaseline <- cdm$overall_period |>
  getBRCharacteristics()

# Characteristics outcome ----
info(logger, "Create outcome cohorts")
## Create outcome cohorts
source(here::here("Analysis", "06.1_BROutcomeCohorts.R"))

info(logger, "Create matched outcome cohorts")
## Create match cohorts ----
source(here::here("Analysis", "06.2_BRMatchCohorts.R"))

info(logger, "Outcome characterisation")
## Characterise ----
info(logger, "- Baseline characteristics")
### Baseline Characteristcs
cdm$outcome_characterisation <- cdm$outcome_characterisation %>% 
  mutate(trimester = !!datediff("pregnancy_start_date", "cohort_start_date")) |>
  mutate(
    trimester = case_when(
      cohort_start_date > pregnancy_end_date ~ "Postpartum",
      trimester >= 0 & trimester <= 90 ~ "Trimester 1",
      trimester >= 91 & trimester <= 180 ~ "Trimester 2",
      trimester >= 181 ~ "Trimester 3"
    )
  ) |>
  compute(name = "outcome_characterisation", temporary = FALSE)
outcomeCharacteristics <- cdm$outcome_characterisation |>
  getBRCharacteristics()

info(logger, "- Large scale characteristics")
### Large Scale Characteristics
lsChracteristics <- summariseLargeScaleCharacteristics(
  cohort = cdm$outcome_characterisation,
  window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365),
                c(366, Inf)),
  eventInWindow = "condition_occurrence",
  episodeInWindow = "drug_exposure",
  indexDate = "cohort_start_date",
  censorDate = NULL,
  includeSource = FALSE,
  minimumFrequency = 0.005,
  excludedCodes = c(0)
)

# Export results ---
bind(denominatorBaseline, outcomeCharacteristics, lsChracteristics) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("background_rates_characteristics_", cdmName(cdm), ".csv"))