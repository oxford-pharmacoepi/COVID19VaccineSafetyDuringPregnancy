# Propensity Score Weighting ----
info(logger, "- Compute PS and Weights")
cohortNames <- settings(cdm$study_population)$cohort_name
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))
selectedLassoFeatures <- vector("list", 3)  
names(selectedLassoFeatures) <- cohortNames
ps <- selectedLassoFeatures
psCovariates <- selectedLassoFeatures
cdm$study_population <- cdm$study_population |>
  addCohortName() |>
  dplyr::select(!dplyr::starts_with("ps")) |>
  dplyr::select(!dplyr::starts_with("weight"))
for (nm in cohortNames) {
  info(logger, paste0("  - Cohort: ", nm))

  ## LASSO 
  lassoData <- cdm$features |>
    filter(cohort_name == nm) |> 
    inner_join(
      cdm$study_population |>
        mutate(unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
        filter(cohort_name == nm) |>
        select(any_of(c(
          "subject_id", "unique_id", "exposure", "age", "gestational_day", "cohort_start_date",
          "previous_observation", "previous_pregnancies", "previous_healthcare_visits", "cohort_name",
          "alcohol_misuse_dependence", "obesity", "anxiety", "depression", "exposed_match_id", "pregnancy_id"
        )))
    ) |>
    select(
      !any_of(c(
        "cohort_name", "cohort_definition_id", "pregnancy_id", "cohort_end_date",
        "exposed_match_id", "vaccine_brand", "gestational_trimester", "weight",
        "cohort_definition_id", "region"
      ))) |>
    collect()
  # drop any columsn with 1 level
  columns <- sapply(lapply(lassoData, unique), length)
  columns <- colnames(columns)[columns > 1]
  x <- data.matrix(lassoData |> select(-any_of(c("subject_id", "exposure", "unique_id", columns))))
  y <- lassoData |> pull("exposure")
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg <- cv.glmnet(x, y, lambda = lambdas, standardize = FALSE, nfolds = 5, family = "binomial", alpha = 1)
  coef.lasso_reg <- coef(lasso_reg, s = lasso_reg$lambda.1se)
  featNms <- names(coef.lasso_reg[,1])
  featNms <- featNms[coef.lasso_reg[,1]!=0]
  selectedLassoFeatures[[nm]] <- featNms[featNms != "(Intercept)"]
  rm(x, y, lasso_reg)
  
  ## Propensity Score 
  psData <- lassoData |>
      select(any_of(c(
        "subject_id", "unique_id", "exposure", "age", "gestational_day", "cohort_start_date",
        "previous_observation", "previous_pregnancies", "previous_healthcare_visits", 
        "alcohol_misuse_dependence", "obesity", "anxiety", "depression", selectedLassoFeatures[[nm]]
      )))|>
      mutate(exposure = factor(exposure, levels = c("comparator", "exposed")))
  
  glmResult <- glm(exposure ~ ., data = psData |> select(-c("unique_id", "subject_id")), family = binomial(link = "logit"))
  
  ps[[nm]] <- psData |>
    select(any_of(c(
      "subject_id", "unique_id", "exposure", "age", "gestational_day", "cohort_start_date",
      "previous_observation", "previous_pregnancies", "previous_healthcare_visits", 
      "alcohol_misuse_dependence", "obesity", "anxiety", "depression", selectedLassoFeatures[[nm]]
    ))) |>
    bind_cols(
      predict.glm(glmResult, newdata = psData |> select(!c("unique_id", "subject_id", "exposure")), type = "response") |>
        as_tibble() |>
        rename("ps" = "value")
    ) |>
    filter(!is.na(ps)) |>
    mutate(weight = if_else(exposure == "exposed", 1-ps, ps))
  
  psCovariates[[nm]] <- tibble(
    variable = names(glmResult$coefficients),
    coefficient = glmResult$coefficients
  )
  
  cohName <- paste0("weighted_", nm)
  set <- settings(cdm$study_population) |> filter(cohort_name == nm) |> mutate(cohort_name = cohort_name)
  cdm[[cohName]] <- cdm$study_population |>
    mutate(unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
    inner_join(ps[[nm]], copy = TRUE) |>
    compute(name = cohName, temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = set,
      cohortAttritionRef = attrition(cdm$study_population) |> filter(cohort_definition_id == set$cohort_definition_id),
      cohortCodelistRef = attr(cdm$study_population, "cohort_codelist") |> filter(cohort_definition_id == set$cohort_definition_id),
      .softValidation = TRUE
    ) |>
    recordCohortAttrition("Weighting") 
}

info(logger, "- Export PS results")
# Save PS covariates
save(selectedLassoFeatures, file = here::here(output_folder, "lasso.RData"))

# Export cohort summary
cdm <- bind(cdm$weighted_population_objective_1, cdm$weighted_population_objective_2, cdm$weighted_population_objective_3, name = "study_population")

sumCohort <- summaryCohort(cdm$study_population)
newSummarisedResult(sumCohort, settings = settings(sumCohort) |> mutate(weighting = "TRUE")) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("weighted_study_cohort_summary_", cdmName(cdm), ".csv"))

# Export LASSO features
psCovariates |>
  bind_rows(.id = "cohort_name") |>
  mutate(
    variable = gsub("`", "", variable),
    concept_id = gsub("_m.*", "", variable),
    concept_id = as.numeric(concept_id),
    variable_level = case_when(
      grepl("m30_0", variable) ~ "-30 to 0",
      grepl("m180_m31", variable) ~ "-180 to -31",
      grepl("m365_m31", variable) ~ "-365 to -181",
      grepl("minf_m366", variable) ~ "-Inf to -366",
      .default = NA
    )
  ) |>
  left_join(cdm$concept |> select("concept_id", "concept_name") |> collect(), by = "concept_id") |>
  mutate(variable = if_else(is.na(concept_name), variable, concept_name)) |>
  select(
    "cohort_name", "variable_name" = "variable", "variable_level",
    "estimate_value" = "coefficient"
  ) |>
  uniteGroup("cohort_name") |>
  uniteStrata() |>
  uniteAdditional() |>
  mutate(
    result_id = 1L,
    cdm_name = cdmName(cdm),
    estimate_name = "coefficient",
    estimate_type = "numeric",
    estimate_value = as.character(estimate_value)
  ) |>
  newSummarisedResult(
    settings = tibble(result_id = 1L, package_name = "study_code", package_version = "v0.0.1", result_type = "propensity_score_coeficcients")
  ) |>
  exportSummarisedResult(fileName = paste0("ps_coefficients_", cdmName(cdm)), path = output_folder)

# Export PS distribution
ps |>
  bind_rows(.id = "cohort_name") |>
  select(!any_of(c(
    "subject_id", "unique_id", "age", "gestational_day", "cohort_start_date", "weight",
    "previous_observation", "previous_pregnancies", "previous_healthcare_visits", 
    "alcohol_misuse_dependence", "obesity", "anxiety", "depression", unlist(selectedLassoFeatures)
  ))) |>
  rename("estimate_value" = "ps") |>
  uniteGroup("cohort_name") |>
  uniteStrata("exposure") |>
  uniteAdditional() |>
  mutate(
    result_id = 1L,
    cdm_name = cdmName(cdm),
    variable_name = "propensity_score",
    variable_level = row_number(),
    estimate_name = "propensity_score",
    estimate_type = "numeric",
    estimate_value = as.character(estimate_value)
  ) |>
  newSummarisedResult(
    settings = tibble(result_id = 1L, package_name = "study_code", package_version = "v0.0.1", result_type = "propensity_scores")
  ) |>
  exportSummarisedResult(fileName = paste0("ps_values_", cdmName(cdm)), path = output_folder)

# Characterise ---- 
info(logger, "- Baseline characteristics (weighted)")
strata <- selectStrata(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group"))

## table one
baseline_characteristics <- getBaselineCharacteristics(cdm, strata, weights = selectedLassoFeatures)

## large scale
info(logger, "- Large Scale characteristics (weighted)")
cdm$features <- cdm$features |>
  inner_join(
    cdm$study_population |> select("unique_id", "weight"), by = "unique_id"
  ) |>
  compute(name = "features", temporary = FALSE)
large_scale_characteristics <- getLargeScaleCharacteristics(cdm, strata, weights = selectedLassoFeatures)

## censoring 
info(logger, "- Censoring summary (weighted)")
censoring <- summariseCohortExit(cdm = cdm, strata = strata, weights = selectedLassoFeatures)

## index date and gestational age
timeDistribution <- summariseTimeDistribution(cdm = cdm, strata = strata, weights = selectedLassoFeatures)

# Confounding ----
## SMD
info(logger, "- Standardised Mean Differences (weighted)")
smdBinary <- summariseBinarySMD(large_scale_characteristics) |>
  filter(!is.na(estimate_value))
smdNumeric <- summariseNumericSMD(baseline_characteristics) |>
  filter(!is.na(estimate_value))
bind(baseline_characteristics, large_scale_characteristics, smdBinary, smdNumeric, censoring, timeDistribution) |>
  exportSummarisedResult(fileName = paste0("weighted_characteristics_", cdmName(cdm), ".csv"), path = output_folder)

## NCO 
info(logger, "- Negative Control Outcomes (weighted)")
cdm$study_population_nco <- cdm$study_population_nco |>
  mutate(unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
  inner_join(
    cdm$study_population |> select("unique_id", "weight"), by = "unique_id"
  ) |>
  compute(name = "study_population_nco", temporary = FALSE)

nco_weighted <- estimateSurvivalRisk(
  cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
  end = "cohort_end_date", strata = strata, group = "cohort_name", 
  weights = selectedLassoFeatures, outcomeGroup = "Negative Control Outcomes"
)
nco_weighted_sensitivity <-   estimateSurvivalRisk(
  cohort = cdm$study_population_nco, outcomes = settings(cdm$nco)$cohort_name, 
  end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
  weights = selectedLassoFeatures, outcomeGroup = "Negative Control Outcomes"
)

pco_weighted <- estimateSurvivalRisk(
  cohort = cdm$study_population_nco, outcomes = "covid", 
  end = "cohort_end_date", strata = strata, group = "cohort_name", 
  weights = selectedLassoFeatures, outcomeGroup = "Positive Control Outcomes"
)
pco_weighted_sensitivity <- estimateSurvivalRisk(
  cohort = cdm$study_population_nco, outcomes = "covid", 
  end = "cohort_end_date_sensitivity", strata = strata, group = "cohort_name", 
  weights = selectedLassoFeatures, outcomeGroup = "Positive Control Outcomes"
)

nco_pco_weighted <- bind(nco_weighted, nco_weighted_sensitivity, pco_weighted, pco_weighted_sensitivity) |>
  suppressRiskEstimates()

nco_pco_weighted |> 
  exportSummarisedResult(fileName = paste0("weighted_nco_", cdmName(cdm), ".csv"), path = output_folder)
