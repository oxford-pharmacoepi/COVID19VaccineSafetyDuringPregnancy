# Propensity Score Weighting ----
info(logger, "- Compute PS and Weights")
cohortNames <- settings(cdm$study_population)$cohort_name
allCovariatesPS <- vector("list", 3)  
ps <- NULL
psCovariates <- NULL
cdm$study_population <- cdm$study_population |>
  addCohortName() |>
  dplyr::select(!dplyr::starts_with("ps")) |>
  dplyr::select(!dplyr::starts_with("weight")) %>% 
  dplyr::mutate(
    overall = "overall",
    miscarriage = if_else(cohort_start_date < !!dateadd("pregnancy_start_date", 19*7 + 6), "miscarriage", NA_character_),
    preterm_labour = if_else(cohort_start_date < !!dateadd("pregnancy_start_date", 37*7), "preterm_labour", NA_character_),
  )
for (nm in cohortNames) {
  info(logger, paste0("  - Cohort: ", nm))
  for (stName in c("overall", "vaccine_brand", "gestational_trimester", "age_group", "miscarriage", "preterm_labour")) {
    strataLevels <- cdm$study_population |>
      pull(.data[[stName]]) |>
      unique()
    if (stName == "miscarriage") strataLevels <- "miscarriage"
    if (stName == "preterm_labour") strataLevels <- "preterm_labour"
    for (stLevel in strataLevels) {
      info(logger, paste0("    - Strata: ", stLevel))
      ## LASSO 
      lassoData <- cdm$features |>
        filter(cohort_name == nm) |> 
        inner_join(
          cdm$study_population |>
            mutate(unique_id = paste0(as.character(subject_id), "_", as.character(exposed_match_id), "_", as.character(pregnancy_id))) |>
            filter(cohort_name == nm, .data[[stName]] == stLevel) |>
            select(any_of(c(
              "subject_id", "unique_id", "cohort_name", "exposed_match_id", "pregnancy_id", unique(unlist(covariatesPS))
            )))
        ) |>
        select(
          !any_of(c(
            "cohort_name", "cohort_definition_id", "pregnancy_id", "cohort_end_date",
            "exposed_match_id", "vaccine_brand", "gestational_trimester", 
            "cohort_definition_id", "age_group", "weight", "region"
          ))) |>
        collect()
      if (nrow(lassoData) > 5) {
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
        allCovariatesPS[[nm]][[stLevel]] <- unique(c(featNms[featNms != "(Intercept)"], covariatesPS[[nm]][[stLevel]])) 
        rm(x, y, lasso_reg)
        
        ## Propensity Score
        psData <- lassoData |>
          select(any_of(c(
            "subject_id", "unique_id", "exposure", allCovariatesPS[[nm]][[stLevel]]
          )))|>
          mutate(exposure = factor(exposure, levels = c("comparator", "exposed")))
        
        glmResult <- glm(exposure ~ ., data = psData |> select(-c("unique_id", "subject_id")), family = binomial(link = "logit"))
        
        ps <- bind_rows(
          ps, 
          psData |>
            select(any_of(c(
              "subject_id", "unique_id", "exposure", allCovariatesPS[[nm]][[stLevel]]
            ))) |>
            bind_cols(
              predict.glm(glmResult, newdata = psData |> select(!c("unique_id", "subject_id", "exposure")), type = "response") |>
                as_tibble() |>
                rename("ps" = "value")
            ) |>
            filter(!is.na(ps)) |>
            select(exposure, ps) |>
            mutate(
              cohort_name = nm,
              strata_name = stName,
              strata_level = stLevel
            ) 
        )
        
        psCovariates <- bind_rows(
          psCovariates,
          tibble(
            cohort_name = nm,
            strata_name = stName,
            strata_level = stLevel,
            variable = names(glmResult$coefficients),
            coefficient = glmResult$coefficients
          )
        )
      }
    }
  }
}

info(logger, "- Export PS results")
# Save PS covariates
save(allCovariatesPS, file = here::here(output_folder, "lasso.RData"))

# Update study populationa cohort with covariates and export cohort summary
cdm$study_population <- cdm$study_population |>
  left_join(
    cdm$features |> 
      select(any_of(c(
        "cohort_name", "subject_id", "cohort_start_date", "exposure", 
        "pregnancy_id", "exposed_matched_id", unique(unlist(allCovariatesPS))
      )))) |>
  compute(name = "study_population", temporary = FALSE) |>
  newCohortTable(.softValidation = TRUE)
sumCohort <- summaryCohort(cdm$study_population)
newSummarisedResult(sumCohort, settings = settings(sumCohort) |> mutate(weighting = "TRUE")) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("weighted_study_cohort_summary_", cdmName(cdm), ".csv"))

# Export LASSO features
psCovariates |>
  mutate(
    variable = gsub("`", "", variable),
    concept_id = gsub("_m.*", "", variable),
    concept_id = as.numeric(concept_id),
    variable_level = case_when(
      grepl("m30_m1", variable) ~ "-30 to -1",
      grepl("m180_m31", variable) ~ "-180 to -31",
      grepl("m365_m31", variable) ~ "-365 to -181",
      grepl("minf_m366", variable) ~ "-Inf to -366",
      .default = NA
    )
  ) |>
  left_join(cdm$concept |> select("concept_id", "concept_name") |> collect(), by = "concept_id") |>
  mutate(variable = if_else(is.na(concept_name), variable, concept_name)) |>
  select(
    "cohort_name", "strata_name", "strata_level", "variable_name" = "variable", 
    "variable_level", "estimate_value" = "coefficient"
  ) |>
  uniteGroup("cohort_name") |>
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
  nest(.by = c("cohort_name", "strata_name", "strata_level")) |>
  mutate(newdata = purrr::map(.x = data, .f = ~ asDensity(data = .x))) |>
  select(!data) |>
  unnest(newdata) |>
  uniteGroup("cohort_name") |>
  uniteAdditional("exposure") |>
  mutate(
    result_id = 1L,
    cdm_name = cdmName(cdm),
    variable_name = "propensity_score",
    variable_level = as.character(row_number()),
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
baseline_characteristics <- getBaselineCharacteristics(cdm, strata, weights = allCovariatesPS)

## large scale
info(logger, "- Large Scale characteristics (weighted)")
cdm$features <- cdm$features |>
  inner_join(
    cdm$study_population |> select("unique_id", "weight"), by = "unique_id"
  ) |>
  compute(name = "features", temporary = FALSE)
large_scale_characteristics <- getLargeScaleCharacteristics(cdm, strata, weights = allCovariatesPS)

## censoring 
info(logger, "- Censoring summary (weighted)")
censoring <- summariseCohortExit(cdm = cdm, strata = strata, weights = allCovariatesPS)

## index date and gestational age
timeDistribution <- summariseTimeDistribution(cdm = cdm, strata = strata, weights = allCovariatesPS)

# Confounding ----
## SMD
info(logger, "- Standardised Mean Differences (weighted)")
smdBinary <- summariseBinarySMD(large_scale_characteristics) |>
  filter(!is.na(estimate_value))
smdNumeric <- summariseNumericSMD(baseline_characteristics) |>
  filter(!is.na(estimate_value))
bind(baseline_characteristics, large_scale_characteristics, smdBinary, smdNumeric, censoring, timeDistribution) |>
  exportSummarisedResult(fileName = paste0("weighted_characteristics_", cdmName(cdm), ".csv"), path = output_folder)
