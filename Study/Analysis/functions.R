getId <- function(cohort, name) {
  settings(cohort) |>
    filter(cohort_name %in% name) |>
    pull(cohort_definition_id)
}

getPregnantCohort <- function(db, cdm, mother_table_schema, mother_table_name) {
  cdm$mother_table_original <- tbl(
    db, inSchema(schema = mother_table_schema, table = mother_table_name)
  ) |>
    compute(
      name = inSchema(results_database_schema, "mother_table_original"),
      temporary = FALSE,
      overwrite = TRUE
    )
  
  cdm$mother_table <- cdm$mother_table_original |>
    mutate(
      cohort_definition_id = 1L,
      cohort_start_date = pregnancy_start_date,
      cohort_end_date = pregnancy_end_date,
      person_id = as.numeric(person_id),
      pregnancy_id = as.numeric(pregnancy_id)
    ) |>
    rename("subject_id" = "person_id") |>
    compute(name = "mother_table", temporary = FALSE, overwrite = TRUE) |>
    newCohortTable(
      cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "mother_table"),
      .softValidation = TRUE
    ) |>
    # Only pregnancies in continuous observation from start to end
    left_join(
      cdm$observation_period |>
        select(
          subject_id = person_id, observation_period_start_date, observation_period_end_date
        ),
      by = "subject_id"
    ) |>
    filter(
      pregnancy_start_date >= observation_period_start_date,
      pregnancy_start_date <= observation_period_end_date,
      pregnancy_end_date <= observation_period_end_date,
      pregnancy_end_date >= observation_period_start_date
    ) |>
    recordCohortAttrition(reason = "Pregnancy in observation") |>
    filter(pregnancy_start_date < pregnancy_end_date) |>
    mutate(cohort_definition_id = 1L) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date") %>%
    mutate(gestational_length = !!datediff("pregnancy_start_date", "pregnancy_end_date")) |>
    filter(gestational_length < 308) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length < 308 days") |>
    filter(gestational_length_in_day != 0) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length days != 0")
  cdm$mother_table <- cdm$mother_table |>
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      window = list(c(0, Inf)),
      indexDate = "pregnancy_start_date",
      censorDate = "pregnancy_end_date",
      targetStartDate = "pregnancy_start_date",
      targetEndDate = NULL,
      nameStyle = "overlap"
    ) |>
    filter(overlap == 1) |>
    select(!c(
      "gestational_length_in_day", "prev_pregnancy_gravidity", "pregnancy_single",
      "overlap", "pregnancy_mode_delivery"
    )) |>
    mutate(
      pregnancy_outcome_study = case_when(
        pregnancy_outcome == 4092289  & gestational_length <= 37*7~ "preterm_labour",
        pregnancy_outcome == 4092289 ~ "livebirth",
        pregnancy_outcome == 4067106 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 4067106 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 443213 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 4081422 ~ "elective_termination",
        pregnancy_outcome == 4095714 ~ "discordant",
        .default = "unknown"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records")
  
  if (cdmName(cdm) %in% "CPRD GOLD") {
    smoking_observation <- cdm$observation |>
      filter(observation_concept_id %in% c(903653, 40766579, 903657)) |>
      mutate(
        pre_pregnancy_smoking = case_when(
          observation_concept_id == 40766579 & value_as_number == 0 ~ "No smoker", 
          observation_concept_id == 40766579 & value_as_number > 0 ~ "Smoker", 
          observation_concept_id == 903657 ~ "Smoker", 
          observation_concept_id == 903653 ~ "No smoker", 
          .default = "Missing")
      ) |>
      select(subject_id = person_id, observation_date, pre_pregnancy_smoking) |>
      inner_join(cdm$mother_table)  |>
      filter(observation_date < pregnancy_end_date) %>% 
      filter(observation_date > !!dateadd("pregnancy_start_date", -5, interval = "year")) |>
      compute() |>
      group_by(subject_id, pregnancy_id, pregnancy_start_date) |>
      filter(observation_date == max(observation_date)) |>
      ungroup() |>
      select(subject_id, observation_date, pre_pregnancy_smoking) |>
      compute()
    cdm$mother_table <- cdm$mother_table |>
      left_join(smoking_observation, by = "subject_id") |>
      compute(name = "mother_table", temporary = FALSE) |>
      mutate(pre_pregnancy_smoking = if_else(is.na(pre_pregnancy_smoking), "Missing", pre_pregnancy_smoking)) |>
      select(!observation_date) |>
      distinct() |>
      # check different records in same day
      group_by(across(-pre_pregnancy_smoking)) |>
      summarise(
        pre_pregnancy_smoking = case_when(
          any(pre_pregnancy_smoking == "Smoker") ~ "Smoker", 
          any(pre_pregnancy_smoking == "No smoker") ~ "No smoker", 
          .default = "Missing"
        ),
        .groups = "drop" 
      ) |>
      compute(name = "mother_table", temporary = FALSE) |>
      recordCohortAttrition(reason = "Add smoking status")
    
  } else if (cdmName(cdm) %in% "SIDIAP") {
    smoking_observation <- cdm$observation |>
      filter(value_as_concept_id %in% c(45879404, 45883458, 45884037)) |>
      mutate(
        pre_pregnancy_smoking = case_when(
          value_as_concept_id == 45879404 ~ "Never smoker",
          value_as_concept_id == 45883458 ~ "Former smoker",
          value_as_concept_id == 45884037 ~ "Current some day smoker", 
          .default = "Missing")
      ) |>
      select(subject_id = person_id, observation_date, pre_pregnancy_smoking) |>
      inner_join(cdm$mother_table)  |>
      filter(observation_date < pregnancy_end_date) %>% 
      filter(observation_date > !!dateadd("pregnancy_start_date", -5, interval = "year")) |>
      compute() |>
      group_by(subject_id, pregnancy_id, pregnancy_start_date) |>
      filter(observation_date == max(observation_date)) |>
      ungroup() |>
      select(subject_id, observation_date, pre_pregnancy_smoking) |>
      compute()
    cdm$mother_table <- cdm$mother_table |>
      left_join(smoking_observation, by = "subject_id") |>
      compute(name = "mother_table", temporary = FALSE) |>
      mutate(pre_pregnancy_smoking = if_else(is.na(pre_pregnancy_smoking), "Missing", pre_pregnancy_smoking)) |>
      select(!observation_date) |>
      distinct() |>
      # check different records in same day
      group_by(across(-pre_pregnancy_smoking)) |>
      summarise(
        pre_pregnancy_smoking = case_when(
          any(pre_pregnancy_smoking == "Current some day smoker") ~ "Current some day smoker", 
          any(pre_pregnancy_smoking == "Former smoker") ~ "Former smoker", 
          any(pre_pregnancy_smoking == "Never smoker") ~ "Never smoker", 
          .default = "Missing"
        ),
        .groups = "drop" 
      ) |>
      compute(name = "mother_table", temporary = FALSE) |>
      recordCohortAttrition(reason = "Add smoking status")
    
  } else {
    cdm$mother_table <- cdm$mother_table |>
      mutate(
        pre_pregnancy_smoking = case_when(
          pre_pregnancy_smoking == 4188540 ~ "No",
          pre_pregnancy_smoking == 4188539 ~ "Yes",
          .default = "Missing"
        )
      ) |>
      compute(name = "mother_table", temporary = FALSE)
  }
  
  return(cdm$mother_table)
}

getSourcePopulation <- function(cdm, objective, enrollment, codelist) {
  name <- paste0("source_", objective)
  cohortName <- paste0("source_population_objective_", objective)
  cdm[[name]] <- cdm$mother_table |>
    mutate(cohort_definition_id = objective) |>
    # pregnancy end after enrollment start
    filter(.data$cohort_end_date > !!enrollment[1]) |>
    compute(name = name, temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = objective, cohort_name = cohortName
      ),
      cohortAttritionRef = attrition(cdm$mother_table) |>
        mutate(cohort_definition_id = objective)
    ) |>
    recordCohortAttrition("Pregnancy end date > {enrollment[1]}") |>
    # pregnancy starts before enrollment ends
    filter(.data$cohort_start_date < !!enrollment[2]) |>
    compute(name = name, temporary = FALSE) |>
    recordCohortAttrition("Pregnancy start date < {enrollment[2]}") |>
    trimToDateRange(
      dateRange = c(enrollment[1], NA),
      name = name
    )
  
  # Eligible for exposure
  if (objective == 1) {
    ## 1st Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |>
          filter(cohort_definition_id == !!getId(cdm$covid_vaccines_dose, "any_covid_vaccine_1")) |>
          select("subject_id", "vaccine_date" = "cohort_start_date"),
        by = "subject_id"
      ) %>%
      filter(vaccine_date > cohort_start_date | is.na(vaccine_date)) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Unvaccinated before pregnancy start")
  }
  
  if (objective == 2) {
    ## 2nd Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |>
          addCohortName() |>
          filter(cohort_definition_id %in% 1:2) |>
          select("subject_id", "cohort_start_date", "cohort_name") |>
          pivot_wider(names_from = "cohort_name", values_from = "cohort_start_date"),
        by = "subject_id"
      ) |>
      rename("previous_dose" = "any_covid_vaccine_1", "vaccine_date" = "any_covid_vaccine_2") |>
      filter(vaccine_date >= pregnancy_start_date | is.na(vaccine_date)) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "No 2nd dose before pregnancy") %>%
      mutate(new_cohort_start = !!dateadd("previous_dose", 16)) |>
      filter(new_cohort_start < .data$cohort_end_date) %>%
      mutate(
        cohort_start_date = if_else(
          new_cohort_start < cohort_start_date, cohort_start_date, new_cohort_start
        )) |>
      select(!c("new_cohort_start")) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Eligible for 2nd during pregnancy")
  }
  
  if (objective == 3) {
    ## 2nd Objective
    cdm[[name]] <- cdm[[name]] |>
      left_join(
        cdm$covid_vaccines_dose |>
          filter(cohort_definition_id == 2) |>
          select("subject_id", "any_covid_vaccine_2" = "cohort_start_date"),
        by = "subject_id"
      ) |>
      filter(.data$any_covid_vaccine_2 < .data$cohort_end_date) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "At least 2 doses any time before pregnancy end") |>
      addCohortIntersectDate(
        targetCohortTable = "covid_vaccines_booster",
        targetCohortId = NULL,
        indexDate = "cohort_start_date",
        censorDate = NULL,
        targetDate = "cohort_start_date",
        order = "first",
        window = c(-Inf, 0),
        nameStyle = "booster_previous",
        name = name
      ) |>
      addCohortIntersectDate(
        targetCohortTable = "covid_vaccines_booster",
        targetCohortId = NULL,
        indexDate = "cohort_start_date",
        censorDate = NULL,
        targetDate = "cohort_start_date",
        order = "first",
        window = c(0, Inf),
        nameStyle = "vaccine_date",
        name = name
      ) %>%
      mutate(
        previous_dose = if_else(
          is.na(booster_previous), any_covid_vaccine_2, booster_previous
        )
      ) %>%
      mutate(new_cohort_start = !!dateadd("previous_dose", 90)) |>
      filter(new_cohort_start < .data$cohort_end_date) %>%
      mutate(
        cohort_start_date = if_else(
          new_cohort_start < cohort_start_date, cohort_start_date, new_cohort_start
        )) |>
      select(!c("any_covid_vaccine_2", "booster_previous", "new_cohort_start")) |>
      compute(name = name, temporary = FALSE) |>
      recordCohortAttrition(reason = "Eligible for booster during pregnancy") |>
      addCohortIntersectCount(
        targetCohortTable = "covid_vaccines",
        targetCohortId = "any_covid_vaccine",
        indexDate = "cohort_start_date",
        window = list(c(-Inf, 0)),
        nameStyle = "number_previous_doses",
        name = name
      )
  }
  
  return(cdm[[name]])
}

startsInPregnancy <- function(cohort,
                              start = "pregnancy_start_date",
                              end = "pregnancy_end_date",
                              reason = "During pregnancy") {
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)
  cdm[[name]]  <- cdm[[name]] |>
    dplyr::inner_join(
      cdm$mother_table |>
        dplyr::select("subject_id", start, end),
      by = "subject_id"
    ) |>
    dplyr::filter(cohort_start_date >= .data[[start]] & cohort_start_date <= .data[[end]]) |>
    dplyr::select(!dplyr::starts_with("pregnancy")) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = reason)
  return(cdm[[name]])
}


getWashOut <- function(washout, source) {
  washout |>
    inner_join(
      source |> distinct(subject_id, pregnancy_start_date, pregnancy_end_date),
      by = "subject_id"
    ) |>
    filter(
      cohort_start_date >= as.Date(clock::add_days(.data$pregnancy_start_date, -.data$days)),
      cohort_start_date <= .data$pregnancy_end_date
    ) |>
    select(!c("pregnancy_start_date", "pregnancy_end_date", "days")) |>
    distinct()
}


samplingSummary <- function(sampling_source, reason, results, cohortId = 1:3, variable = c("exposed", "comparator", "ratio")) {
  x <- sampling_source |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::collect()
  
  if ("ratio" %in% variable) {
    ratio <- x |>
      group_by(cohort_name, exposed_id) |>
      tally() |>
      summariseResult(
        group = list("cohort_name"), variables = "n", counts = FALSE,
        estimates = c("min", "max", "median", "q25", "q75")
      ) |>
      mutate(variable_name = "exposed:comparator", variable_level = reason)
  } else {
    ratio <- NULL
  }
  
  if ("comparator" %in% variable) {
    comparator <- x |>
      summariseResult(group = list("cohort_name")) |>
      mutate(
        variable_name = dplyr::case_when(
          variable_name == "number records" ~ "Number comparators",
          variable_name == "number subjects" ~ "Number unique comparators",
          .default = NA
        ),
        variable_level = reason
      ) |>
      filter(!is.na(.data$variable_name))
  } else {
    comparator <- NULL
  }
  
  if ("exposed" %in% variable) {
    exposed <- x |>
      select(
        cohort_name, subject_id = exposed_id, cohort_start_date = exposure_date, cohort_end_date = exposure_date
      )  |>
      distinct() |>
      summariseResult(group = list("cohort_name")) |>
      mutate(
        variable_name = dplyr::case_when(
          variable_name == "number records" ~ "Number exposed",
          variable_name == "number subjects" ~ "Number unique exposed",
          .default = NA
        ),
        variable_level = reason
      ) |>
      filter(!is.na(.data$variable_name))
  } else {
    exposed <- NULL
  }
  
  omopgenerics::bind(results, ratio, comparator, exposed)
}

exitAtFirstDateStudy <- function(cohort, dateColumns, endColumn, keepDates, reason, name) {
  atDateFunction <- rlang::expr(min(.data$new_date_0123456789, na.rm = TRUE)) # NA always removed in SQL
  newDate <- endColumn
  keptDate <- "cohort_start_date"
  reason <- reason
  
  tmpPrefix <- omopgenerics::tmpPrefix()
  tmpName <- omopgenerics::uniqueTableName(prefix = tmpPrefix)
  
  newCohort <- cohort |>
    dplyr::mutate(
      "cohort_start_date_0123456789" = .data$cohort_start_date,
      "cohort_end_date_0123456789" = .data$cohort_end_date
    ) |>
    dplyr::compute(name = tmpName, temporary = FALSE)
  
  newCohort <- newCohort |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dateColumns),
      names_to = reason,
      values_to = "new_date_0123456789"
    ) |>
    dplyr::group_by(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$cohort_start_date_0123456789,
      .data$cohort_end_date_0123456789,
      .data$pregnancy_id,
      .data$exposure,
      .data$exposed_match_id
    ) |>
    dplyr::filter(.data$new_date_0123456789 == !!atDateFunction) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(!dplyr::all_of(reason))) |>
    dplyr::arrange(.data[[reason]]) |>
    dplyr::summarise(
      !!reason := stringr::str_flatten(.data[[reason]], collapse = '; '),
      .groups = "drop"
    ) |>
    dplyr::mutate(!!newDate := .data$new_date_0123456789, !!keptDate := .data[[paste0(keptDate, "_0123456789")]]) |>
    dplyr::select(!c("new_date_0123456789", "cohort_start_date_0123456789", "cohort_end_date_0123456789")) |>
    dplyr::distinct() |>
    dplyr::compute(name = tmpName, temporary = FALSE)
  
  newCohort <- newCohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort")))
  
  if (keepDates) {
    newCohort <- newCohort |>
      dplyr::inner_join(
        cohort |>
          dplyr::select(!any_of(c(newDate, "cohort_start_date_0123456789", "cohort_end_date_0123456789")))
      ) |>
      dplyr::compute(name = tmpName, temporary = FALSE)
  }
  
  newCohort <- newCohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE)
  
  omopgenerics::dropSourceTable(cdm, tmpName)
  return(newCohort)
}

datesPivotLongerExprs <- function(cols, data) {
  cols <- cols[cols %in% colnames(data)]
  expr <- character()
  for (col in cols) {
    expr <- c(expr, glue::glue(
      "if_else(
      .data$exposure == 'comparator',
      .data$comparator_{col},
      .data$exposed_{col}
    )"
    ))
  }
  expr |> rlang::parse_exprs() |> rlang::set_names(cols)
}

getBaselineCharacteristics <- function(cdm, strata, weights) {
  estimates = list(
    'age' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    'days_previous_dose' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    'previous_observation' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    "previous_healthcare_visits" = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    "previous_pregnancies" = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    'gestationa_day' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    'age_group' = c('count', 'percentage'),
    'gestational_trimester' = c('count', 'percentage'),
    'vaccine_brand' = c('count', 'percentage'),
    "previous_covid_vaccines" = c('count', 'percentage'),
    "previous_pregnant_covid_vaccines" = c('count', 'percentage'),
    "number_pregnancies_weighted" = 'sum'
  )
  if (length(weights) != 0) {
    cdm$study_population <- cdm$study_population |>
      mutate(number_pregnancies_weighted = NA)
  }
  estimates <- estimates[names(estimates) %in% colnames(cdm$study_population)]
  otherVariables = names(estimates)
  if (length(weights) == 0) {
    baseline <- cdm$study_population |>
      summariseCharacteristics(
        strata = strata,
        counts = TRUE,
        demographics = FALSE,
        cohortIntersectFlag = list(
          # covariatesInf (-365, 0)
          "Comorbidities in the last year" = list(
            targetCohortTable = "covariates_inf", window = c(-365, 0)
          ),
          # covariates1 (-365, 0)
          "Comorbidities in the last year" = list(
            targetCohortTable = "covariates_5", window = c(-365, 0)
          ),
          # influenza and tdap (-5, 5)
          "Other vaccines within 5 days" = list(
            targetCohortTable = "other_vaccines", window = c(-5, 5)
          ),
          # Comedications
          "Medications in the past year" = list(
            targetCohortTable = "comedications", window = c(-365, 0)
          )
        ),
        cohortIntersectCount = list(
          # covid infections (-365, 0)
          "COVID-19 infections in the last year" = list(
            targetCohortTable = "covid", window = c(-365, 0)
          )
        ),
        otherVariables = otherVariables,
        estimates = estimates
      )
  } else {
    baseline <- NULL
    strata <- unlist(strata)
    strata <- unique(c(strata[strata != "exposure"], "overall"))
    data <- cdm$study_population |> mutate(overall = "overall")
    for (group in settings(cdm$study_population)$cohort_name) { # group level
      for (strata.k in strata) { # strata name
        strataLevels <- unique(data |> pull(strata.k))
        for(strataLevel.k in strataLevels) { # strata level
          # data
          data.k <- data |>
            filter(cohort_name == group, .data[[strata.k]] == strataLevel.k) |>
            collect()
          if (nrow(data.k) > 10) {
            # weights
            data.k <- getWeights(data.k, weights[[group]][[strataLevel.k]])
            # characteristics
            baselineData <- data |>
              select(!any_of(c("weight", "ps"))) |>
              inner_join(
                data.k |>
                  select(cohort_definition_id, subject_id, exposure, cohort_start_date, exposed_match_id, pregnancy_id, weight),
                by = c("cohort_definition_id", "subject_id", "exposure", "cohort_start_date", "exposed_match_id", "pregnancy_id"),
                copy = TRUE
              ) |>
              compute()
            
            if (nrow(data.k) != pull(tally(baselineData))) cli::cli_abort("Error in getBaselineCharacteristics")
            
            strataBaseline <- "exposure"
            if (strata.k != "overall") strataBaseline <- (c("exposure", strata.k))
            
            baseline.k <- baselineData |>
              mutate(number_pregnancies_weighted = weight)|>
              summariseCharacteristics(
                cohortId = group,
                strata = strataBaseline,
                counts = TRUE,
                demographics = FALSE,
                cohortIntersectFlag = list(
                  # covariatesInf (-365, 0)
                  "Comorbidities in the last year" = list(
                    targetCohortTable = "covariates_inf", window = c(-365, 0)
                  ),
                  # covariates1 (-365, 0)
                  "Comorbidities in the last year" = list(
                    targetCohortTable = "covariates_5", window = c(-365, 0)
                  ),
                  # influenza and tdap (-5, 5)
                  "Other vaccines within 5 days" = list(
                    targetCohortTable = "other_vaccines", window = c(-5, 5)
                  ),
                  # Comedications
                  "Medications in the past year" = list(
                    targetCohortTable = "comedications", window = c(-365, 0)
                  )
                ),
                cohortIntersectCount = list(
                  # covid infections (-365, 0)
                  "COVID-19 infections in the last year" = list(
                    targetCohortTable = "covid", window = c(-365, 0)
                  )
                ),
                otherVariables = otherVariables,
                estimates = estimates,
                weights = "weight"
              )
            if (strata.k != "overall") {
              baseline.k <- baseline.k |>
                filter(strata_name != "overall")
            }
            baseline <- omopgenerics::bind(baseline, baseline.k)
          }
        }
      }
    }
  }
  
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  baseline |>
    mutate(cdm_name = cdmName(cdm)) |>
    newSummarisedResult(
      settings = settings(baseline) |>
        mutate(weighting = weighting) |>
        select(!any_of(c("weights")))
    )
}

getFeaturesTable <- function(cdm, strata, covariatesPS) {
  features <- cdm$condition_occurrence |>
    select(
      subject_id = person_id, concept_id = condition_concept_id,
      start_date = condition_start_date, end_date = condition_end_date,
    ) |>
    inner_join(
      cdm$study_population |>
        distinct(cohort_name, subject_id, pregnancy_id, cohort_start_date),
      by = "subject_id"
    ) |>
    filter(concept_id != 0 & start_date < cohort_start_date) %>%
    mutate(
      time_start = !!datediff("start_date", "cohort_start_date")
    ) |>
    filter(time_start > 0) |>
    mutate(
      window = case_when(
        time_start < 30 ~ "m30_m1",
        time_start < 181 ~ "m180_m31",
        time_start < 366 ~ "m365_m181",
        .default = "none"
      ),
      feature = paste0(as.character(concept_id), "_", as.character(window))
    ) |>
    filter(window != "none") |>
    compute(name = "features", temporary = FALSE) |>
    union_all(
      cdm$drug_exposure |>
        select(
          subject_id = person_id, concept_id = drug_concept_id,
          start_date = drug_exposure_start_date, end_date = drug_exposure_end_date,
        ) |>
        inner_join(
          cdm$study_population |>
            distinct(cohort_name, subject_id, pregnancy_id, cohort_start_date),
          by = "subject_id"
        ) |>
        filter(concept_id != 0 & start_date < cohort_start_date) %>%
        mutate(
          time_start = !!datediff("start_date", "cohort_start_date")
        ) |>
        filter(time_start > 0) |>
        mutate(
          window = case_when(
            time_start < 30 ~ "m30_m1",
            time_start < 181 ~ "m180_m31",
            .default = "none"
          ),
          feature = paste0(as.character(concept_id), "_", as.character(window))
        )
    ) |>
    filter(window != "none") |>
    distinct(cohort_name, subject_id, cohort_start_date, feature) |>
    compute(name = "features", temporary = FALSE)
  
  countsFeatures <- features |>
    inner_join(
      cohortCount(cdm$study_population) |>
        inner_join(settings(cdm$study_population)) |>
        select(cohort_name, number_records),
      by = "cohort_name",
      copy = TRUE
    ) |>
    group_by(cohort_name, feature, number_records) |>
    summarise(n = as.numeric(n())) |>
    mutate(freq = n / number_records) |>
    ungroup() |>
    filter(freq >= 0.005) |>
    distinct(cohort_name, feature) |>
    compute()
  
  cdm$features <- features |>
    inner_join(countsFeatures, by = c("cohort_name", "feature")) |>
    select(any_of(c(
      "cohort_name", "subject_id", "cohort_start_date", "feature"
    ))) |>
    mutate(value = 1) |>
    right_join(
      cdm$study_population |>
        select(any_of(c(
          "cohort_definition_id", "cohort_name", "subject_id", "exposure",
          "pregnancy_id", "cohort_start_date", "cohort_end_date", "exposed_match_id",
          "region", "ethnicity", "socioecnomic_status", "birth_continent", "nationallity",
          "pre_pregnancy_smoking", unlist(strata), unique(unlist(covariatesPS))
        )))
    ) |>
    mutate(unique_id = paste0(as.character(subject_id), "_", as.character(exposed_match_id), "_", as.character(pregnancy_id))) |>
    pivot_wider(names_from = "feature", values_from = "value", values_fill = 0) |>
    select(!"NA") |>
    mutate(across(contains("_m"), ~ if_else(is.na(.x), 0, .x))) |>
    compute(name = "features", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = settings(cdm$study_population),
      cohortAttritionRef = NULL,
      cohortCodelistRef = NULL,
      .softValidation = TRUE
    )
  
  return(cdm)
}

getLargeScaleCharacteristics <- function(cdm, strata, weights) {
  features <- colnames(cdm$features)
  features <- features[grepl("_minf_m366|_m30_m1|_m365_31|_m180_m31", features) | features %in% c("region", "ethnicity", "socioecnomic_status", "birth_continent", "nationallity", "pre_pregnancy_smoking")]
  if (length(weights) == 0) {
    summarisedResult <- cdm$features |>
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = FALSE,
        strata = strata,
        includeOverallStrata = FALSE,
        variables = features,
        estimates = c("count", "percentage"),
        counts = FALSE
      )
  } else {
    summarisedResult <- NULL
    strata <- unlist(strata)
    strata <- c(strata[strata != "exposure"], "overall") %>% unique()
    data <- cdm$features |> mutate(overall = "overall")
    for (group in settings(cdm$features)$cohort_name) { # group level
      for (strata.k in strata) { # strata name
        strataLevels <- unique(data |> pull(strata.k))
        for(strataLevel.k in strataLevels) { # strata level
          # data
          data.k <- data |>
            filter(cohort_name == group, .data[[strata.k]] == strataLevel.k) |>
            collect()
          if (nrow(data.k) > 10) {
            # weights
            data.k <- getWeights(data.k, weights[[group]][[strataLevel.k]])
            
            strataBaseline <- "exposure"
            if (strata.k != "overall") strataBaseline <- (c("exposure", strata.k))
            
            # characteristics
            summarisedResult.k <- data.k |>
              mutate(exposure = as.character(exposure)) |>
              summariseResult(
                group = list("cohort_name"),
                includeOverallGroup = FALSE,
                strata = strataBaseline,
                includeOverallStrata = FALSE,
                variables = features,
                estimates = c("count", "percentage"),
                counts = FALSE,
                weights = "weight"
              )
            if (strata.k != "overall") {
              summarisedResult.k <- summarisedResult.k |>
                filter(strata_name != "overall")
            }
            summarisedResult <- omopgenerics::bind(summarisedResult, summarisedResult.k)
          }
        }
      }
    }
  }
  
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  lsc <- summarisedResult |>
    filter(!variable_name %in% c("region", "ethnicity", "socioecnomic_status", "birth_continent", "nationallity", "pre_pregnancy_smoking")) |>
    mutate(
      cdm_name = cdmName(cdm),
      additional_level = gsub("_minf_m366|_m30_m1|_m365_31|_m180_m31", "", variable_name),
      additional_name = "concept_id",
      variable_level = gsub(".*(m30_m1|minf_m366|m365_31|m180_m31)", "\\1", variable_name),
      variable_level = gsub("m", "-", gsub("_", " to ", variable_level))
    ) |>
    select(!variable_name) |>
    inner_join(
      cdm$concept |>
        select(additional_level = concept_id, variable_name = concept_name) |>
        mutate(additional_level = as.character(additional_level)),
      by = "additional_level",
      copy = TRUE
    ) |>
    bind_rows(
      summarisedResult |>
        filter(variable_name %in% c("region", "ethnicity", "socioecnomic_status", "birth_continent", "nationallity", "pre_pregnancy_smoking")) |>
        mutate(
          cdm_name = cdmName(cdm),
          additional_level = "overall",
          additional_name = "overall"
        )
    ) |>
    newSummarisedResult(
      settings = settings(summarisedResult) |>
        mutate(
          result_type = "summarise_large_scale_characteristics",
          analysis = "standard", table_name = NA, type = "event",
          weighting = weighting
        ) |>
        select(!any_of("weights"))
    )
  return(lsc)
}

selectStrata <- function(cdm, strata = c("vaccine_brand", "gestational_trimester", "age_group")) {
  strataLevels <- cdm$study_population  |>
    select(any_of(strata)) |>
    colnames()
  strata <- list("exposure")
  for(s in strataLevels) {
    strata <- c(strata, list(c("exposure", s)))
  }
  return(strata)
}

summariseBinarySMD <- function(lsc) {
  strata <- strataColumns(lsc)
  strata <- strata[strata != "exposure"]
  smd <- lsc |>
    # filter(strata_name != "exposure") |>
    splitStrata() |>
    uniteStrata(cols = strata) |>
    mutate(
      estimate_name = paste0(estimate_name, "_", exposure),
      estimate_value = as.numeric(estimate_value)
    ) |>
    select(-"exposure", -"estimate_type") |>
    pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
    mutate(
      smd = if_else(
        percentage_comparator == percentage_exposed, 0,
        (percentage_comparator/100 - percentage_exposed/100) / sqrt((percentage_exposed/100*(1-percentage_exposed/100) + percentage_comparator/100*(1-percentage_comparator/100))/2)
      ),
      across(.cols = contains("count"), .fn = ~if_else(.x < 5 & .x > 0, "-", as.character(.x))),
      estimate_value = if_else(count_comparator == "-" | count_exposed == "-", "-", as.character(smd)),
      estimate_type = "numeric",
      estimate_name = "smd"
    ) |>
    select(all_of(resultColumns()))
  
  smd |>
    newSummarisedResult(
      settings(lsc) |>
        mutate(
          strata = gsub("exposure &&& ", "", .data$strata),
          min_cell_count = "5",
          result_type = "summarise_standardised_mean_differences",
          package_name = "study_code",
          package_version = "v0.0.1"
        )
    )
}

summariseNumericSMD <- function(baseline) {
  strata <- strataColumns(baseline)
  strata <- strata[strata != "exposure"]
  smd <- baseline |>
    filter(!strata_name %in% "overall") |>
    filter(estimate_name %in% c("mean", "sd")) |>
    splitStrata() |>
    uniteStrata(cols = strata) |>
    mutate(
      estimate_name = paste0(estimate_name, "_", exposure),
      estimate_value = as.numeric(estimate_value)
    ) |>
    select(-"exposure", -"estimate_type") |>
    pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
    left_join(
      baseline |>
        filter(!strata_name %in% "overall") |>
        filter(variable_name %in% c("Number subjects")) |>
        splitStrata() |>
        uniteStrata(cols = strata) |>
        mutate(
          estimate_name = paste0(estimate_name, "_", exposure),
          estimate_value = as.numeric(estimate_value)
        ) |>
        select(-c("exposure", "estimate_type", "additional_name", "additional_level", "variable_name", "variable_level")) |>
        pivot_wider(names_from = estimate_name, values_from = estimate_value),
      by = c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level")
    ) |>
    mutate(
      smd = (mean_comparator-mean_exposed)/sqrt(sd_comparator+sd_exposed),
      across(.cols = contains("count"), .fn = ~if_else(.x < 5 & .x > 0, "-", as.character(.x))),
      estimate_value = if_else(count_comparator == "-" | count_exposed == "-", "-", as.character(smd)),
      estimate_type = "numeric",
      estimate_name = "smd"
    ) |>
    select(all_of(resultColumns()))
  smd |>
    newSummarisedResult(
      settings(baseline) |>
        mutate(
          strata = gsub("exposure &&&", "", .data$strata),
          min_cell_count = "5",
          result_type = "summarise_standardised_mean_differences",
          package_name = "study_code",
          package_version = "v0.0.1"
        )
    )
}

cohortExit <- function(x, strata, weights) {
  main <- x %>%
    collect() %>%
    mutate(
      "follow-up time" = date_count_between(cohort_start_date, cohort_end_date, "day")
    ) |>
    summariseResult(
      group = "cohort_name",
      strata = strata,
      variables = c("follow-up time", "number_records_weighted"),
      weights = weights
    ) |>
    mutate("analysis" = "main") |>
    addExitReasonPercentages()
  sensitvity <- x %>%
    collect() %>%
    mutate(
      "follow-up time" = date_count_between(cohort_start_date, cohort_end_date_sensitivity, "day"),
      "exit_reason" = exit_reason_sensitivty
    ) |>
    summariseResult(
      group = "cohort_name",
      strata = strata,
      variables = c("follow-up time"),
      weights = weights
    ) |>
    mutate("analysis" = "sensitivity") |>
    addExitReasonPercentages()
  
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  bind(main, sensitvity) |>
    newSummarisedResult(
      settings = settings(main) |>
        mutate(result_type = "cohort_exit", weighting = weighting) |>
        select(!any_of("weights"))
    )
}

summariseCohortExit <- function(cdm, strata, weights) {
  strataCens <- list()
  for (st in strata) {
    strataCens <- c(strataCens, list(c(st, "exit_reason")))
  }
  strataCens <- c(strataCens, strata, "exit_reason")
  
  if (length(weights) == 0) {
    summaryExit <- cohortExit(cdm$study_population, strataCens, NULL)
  } else {
    summaryExit <- NULL
    strata <- unlist(strata)
    strata <- c(strata[strata != "exposure"], "overall")
    data <- cdm$study_population |> mutate(overall = "overall")
    for (group in settings(cdm$study_population)$cohort_name) { # group level
      for (strata.k in strata) { # strata name
        strataLevels <- unique(data |> pull(strata.k))
        for(strataLevel.k in strataLevels) { # strata level
          # data
          data.k <- data |>
            filter(cohort_name == group, .data[[strata.k]] == strataLevel.k) |>
            collect()
          if (nrow(data.k) > 10) {
            # weights
            data.k <- getWeights(data.k, weights[[group]][[strataLevel.k]])
            # characteristics
            summaryExit.k <- cohortExit(data.k, list("exit_reason", "exposure", c("exposure", "exit_reason")), "weight")
            if (strata.k != "overall") {
              summaryExit.k <- summaryExit.k  |>
                splitStrata() |>
                mutate(!!strata.k := strataLevel.k) |>
                uniteStrata(c("exposure", strata.k)) |>
                newSummarisedResult(settings(summaryExit.k) |> mutate(strata = paste0("exposure &&& ", strata.k)))
            }
            summaryExit <- omopgenerics::bind(summaryExit, summaryExit.k)
          }
        }
      }
    }
  }
  return(summaryExit |> mutate(cdm_name = cdmName(cdm)))
}

addExitReasonPercentages <- function(x) {
  overall <- x |>
    splitStrata() |>
    filter(exit_reason == "overall") |>
    filter(estimate_name == "count") |>
    mutate(estimate_value = as.numeric(estimate_value)) |>
    rename("overall" = "estimate_value") |>
    select(!"exit_reason")
  percentages <- x |>
    filter(estimate_name == "count") |>
    splitStrata() |>
    filter(exit_reason != "overall") |>
    mutate(estimate_value = as.numeric(estimate_value)) |>
    inner_join(overall) |>
    mutate(
      estimate_value = as.character(estimate_value/overall*100),
      estimate_name = "percentage",
      estimate_type = "percentage"
    ) |>
    select(!"overall")
  strata <- strataColumns(x)
  strata <- strata[strata != "exit_reason"]
  rbind(
    x |> splitStrata(),
    percentages
  ) |>
    select(!starts_with("additional")) |>
    uniteAdditional(cols = c("exit_reason", "analysis")) |>
    uniteStrata(cols = unique(unlist(strata))) |>
    newSummarisedResult()
}

getSurvivalData <- function(data, outcomes, group, strata,
                            start = "cohort_start_date",
                            end = "cohort_end_date",
                            weights = NULL) {
  tmp <- omopgenerics::uniqueTableName()
  
  # Work on a renamed copy so we don't clobber original
  outdata <- data |>
    dplyr::rename(start_date = !!rlang::sym(start),
                  end_date   = !!rlang::sym(end))
  
  # For each requested outcome, create two columns: <outcome>_status and <outcome>_time
  for (outcome in outcomes) {
    # compute status: 1 if event date between start_date and end_date, else 0
    outdata <- outdata |>
      mutate(
        # event_date might be NA; .data[[outcome]] references column by name
        !!paste0(outcome, "_status") := if_else(
          !is.na(.data[[outcome]]) & .data[[outcome]] >= start_date & .data[[outcome]] <= end_date,
          1L, 0L
        ),
        # compute time in days: if event occurs, event - start; else end - start
        !!paste0(outcome, "_time") := if_else(
          !!rlang::sym(paste0(outcome, "_status")) == 1L,
          as.numeric(difftime(start_date, .data[[outcome]], units = "days")),
          as.numeric(difftime(start_date, end_date, units = "days"))
        )
      ) |>
      compute(name = tmp, temporary = FALSE)
  }
  
  # remove original outcome date columns 
  outdata <- outdata |> 
    select(-any_of(outcomes)) |>
    compute(name = tmp, temporary = FALSE)
  
  return(outdata)
}

pivotSurvivalData <- function(data, outcomes) {
  time_cols <- intersect(colnames(data), paste0(outcomes, "_time"))
  status_cols <- intersect(colnames(data), paste0(outcomes, "_status"))
  if ("tbl_sql" %in% class(data)) {
    data |>
      pivot_longer(
        cols = c(time_cols, status_cols),
        names_to = "outcome_name",
        values_to = "value"
      ) |>  
      compute(name = "irr_temp", temporary = FALSE) |> 
      rename_with(.fn = ~gsub("\`|\`", "", .x)) |>  
      mutate(
        variable = if_else(str_detect(outcome_name, fixed("time")), "time", "status"),
        outcome_name = REGEXP_REPLACE(outcome_name, "_time|_status", "" )
      ) |>
      pivot_wider(names_from = "variable", values_from = "value") |>
      compute(name = "irr_temp", temporary = FALSE)
  } else {
    data |>
      pivot_longer(
        cols = c(time_cols, status_cols),
        names_to = "outcome_name",
        values_to = "value"
      ) |> 
      mutate(
        variable = if_else(grepl("time", outcome_name), "time", "status"),
        outcome_name = gsub("_time|_status", "", outcome_name)
      ) |>
      pivot_wider(names_from = "variable", values_from = "value")
  }
}

getIRR <- function(x, ci, outcomes) {

  summary_tbl <- bind_rows(
    # weighted summary
    x |>
      collect() |>
      group_by(outcome_name, exposure) |>
      summarise(
        person_days = sum(as.numeric(time) * coalesce(.data$weight, 1), na.rm = TRUE),
        cases = sum(as.numeric(status) * coalesce(.data$weight, 1), na.rm = TRUE),
        person_days_median = as.numeric(Hmisc::wtd.quantile(as.numeric(time), weights = .data$weight, probs = 0.5, na.rm = TRUE)),
        person_days_q25 = as.numeric(Hmisc::wtd.quantile(as.numeric(time), weights = .data$weight, probs = 0.25, na.rm = TRUE)),
        person_days_q75 = as.numeric(Hmisc::wtd.quantile(as.numeric(time), weights = .data$weight, probs = 0.75, na.rm = TRUE)),
        person_days_min = min(as.numeric(time), na.rm = TRUE),
        person_days_max = max(as.numeric(time), na.rm = TRUE),
        subject_count = sum(.data$weight, na.rm = TRUE),
        weighting = "TRUE",
        .groups = "drop"
      ) , 
    # unweighted summary
    x |>
      group_by(outcome_name, exposure) |>
      summarise(
        person_days = sum(as.numeric(time), na.rm = TRUE),
        cases = sum(as.numeric(status), na.rm = TRUE),
        person_days_median = as.numeric(quantile(as.numeric(time), probs = 0.5, na.rm = TRUE)),
        person_days_q25 = as.numeric(quantile(as.numeric(time), probs = 0.25, na.rm = TRUE)),
        person_days_q75 = as.numeric(quantile(as.numeric(time), probs = 0.75, na.rm = TRUE)),
        person_days_min = min(as.numeric(time), na.rm = TRUE),
        person_days_max = max(as.numeric(time), na.rm = TRUE),
        subject_count = n(),
        weighting = "FALSE",
        .groups = "drop"
      ) |>
      collect() |>
      mutate(subject_count = as.numeric(subject_count))
  )
  
  # add 0.01 if 0 cases - avoid denominator 0
  summary_tbl <- summary_tbl |>
    ungroup() |>
    mutate(
      person_days_fix = if_else(cases == 0, person_days + 0.01, person_days),
      cases_fix = if_else(cases == 0, cases + 0.01, cases)
    )
  
  # one row per coeficcient
  wide <- summary_tbl |>
    pivot_wider(
      id_cols = c("outcome_name", "weighting"),
      names_from = "exposure",
      values_from = c("person_days", "cases", "person_days_median", "person_days_q25", "person_days_q75", "person_days_min", "person_days_max", "subject_count", "cases_fix", "person_days_fix"),
      names_sep = "_"
    )
  
  # compute coef and guard against division by zero / NA
  wide <- wide |>
    mutate(
      person_days_fix_exposed = coalesce(person_days_fix_exposed, NA_real_),
      person_days_fix_comparator = coalesce(person_days_fix_comparator, NA_real_),
      cases_fix_exposed = coalesce(cases_fix_exposed, NA_real_),
      cases_fix_comparator = coalesce(cases_fix_comparator, NA_real_),
      coef = case_when(
        is.na(cases_fix_exposed) | is.na(cases_fix_comparator) | is.na(person_days_fix_exposed) | is.na(person_days_fix_comparator) ~ NA_real_,
        person_days_fix_exposed == 0 | person_days_fix_comparator == 0 ~ NA_real_,
        .default = (cases_fix_exposed / person_days_fix_exposed) / (cases_fix_comparator / person_days_fix_comparator)
      ),
      coef = if_else(
        cases_comparator < 5 | cases_exposed < 5, NA, coef
      )
    )
  
  # confidence intervals using mid-p / normal approximation on log(IRR)
  if (!is.null(ci) && ci == "midp") {
    wide <- wide |>
      mutate(
        se = sqrt(1 / cases_fix_exposed + 1 / cases_fix_comparator),
        lower_ci = exp(log(coef) - 1.96 * se),
        upper_ci = exp(log(coef) + 1.96 * se)
      ) |>
      select(-"se")
  } else {
    # keep lower/upper as NA if not computed here (bootstrap handled elsewhere)
    wide <- wide |>
      mutate(lower_ci = NA_real_, upper_ci = NA_real_)
  }
  
  wide <- wide |>
    select(!c("cases_fix_exposed", "cases_fix_comparator", "person_days_fix_exposed", "person_days_fix_comparator"))
  
  return(wide)
}

getWeights <- function(x, coefs) {
  psData <- x |>
    mutate(unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
    mutate(exposure = factor(exposure, levels = c("comparator", "exposed")))
  
  # drop constant columns
  columns <- sapply(lapply(psData, unique), length)
  columns <- names(columns)[columns <= 1]
  
  glm_data <- psData |> select(any_of(coefs)) |> select(!any_of(columns))
  glmResult <- glm(exposure ~ ., data = glm_data, family = binomial(link = "logit"))
  
  ps_vals <- predict(glmResult, newdata = glm_data, type = "response") |> as.numeric()
  
  psData <- psData |>
    select(-any_of(c("ps", "weight"))) |>
    bind_cols(tibble(ps = ps_vals)) |>
    filter(!is.na(ps)) |>
    # overlap weights 
    mutate(weight = if_else(exposure == "exposed", 1 - ps, ps))
  
  rm(glmResult, ps_vals)
  
  return(psData)
}

processGroupStrata <- function(data, groupLevel, strataLevel, weights, ci, outcomes) {
  data <- data |> ungroup()
  
  # Basic safety checks
  if (pull(tally(data)) <= 20) {
    return(tibble(
      variable_level = character(),
      estimate_type = character(),
      estimate_name = character(),
      estimate_value = character(),
      variable_name = character(),
      outcome_name = character(),
      weighting = character()
    ))
  }
  
  set.seed(123)
  
  # main estimate on original data
  # if (!is.null(weights) && length(weights) != 0) {
  if ("tbl_sql" %in% class(data)) {
    dataCol <- data |> collect()
    dataCol <- getWeights(dataCol, weights[[groupLevel]][[strataLevel]])
    cdm <- omopgenerics::cdmReference(data)
    cdm <- omopgenerics::insertTable(cdm = cdm, name = "irr_temp", table = dataCol)
    rm(dataCol)
    data <- cdm$irr_temp |> pivotSurvivalData(outcomes)
  } else {
    data <- getWeights(data, weights[[groupLevel]][[strataLevel]])
    data <- data |> pivotSurvivalData(outcomes)
  }
  # }
  
  if (ci == "bootstrap") {
    # bootstrap across the whole nested data
    coefBootstrap <- tibble()
    nboot <- 200
    n <- as.integer(pull(tally(data)))
    
    data <- data |>
      mutate(
        unique_id = paste0(as.character(cohort_definition_id), as.character(subject_id), as.character(start_date), as.character(exposed_match_id), as.character(pregnancy_id), as.character(exposure))
      ) |>
      compute(name = 'irr_temp', temporary = FALSE)
    uniqueId <- data |> select(unique_id) |> collect()
    
    for (ii in seq_len(nboot)) {
      if ("tbl_sql" %in% class(data)) {
        uniqueId.ii <- uniqueId |> slice_sample(n = n, replace = TRUE)
        cdm <- omopgenerics::insertTable(cdm = cdm, name = "sample_data_ii", table = uniqueId.ii)
        data.ii <- cdm$sample_data_ii |>
          left_join(data, by = "unique_id") |>
          compute(name = "sample_data_ii", temporary = FALSE)
      } else {
        data.ii <- data |> slice_sample(n = n, replace = TRUE)
      }
      irr_tbl <- getIRR(data.ii, ci = NULL, outcomes = outcomes) |> mutate(bootstrap = ii)
      coefBootstrap <- bind_rows(coefBootstrap, irr_tbl)
    }
    
    # Compute bootstrap percentiles grouped by outcome_name & weighting
    ci_tbl <- coefBootstrap |>
      group_by(outcome_name, weighting) |>
      summarise(
        lower_ci = quantile(coef, 0.025, na.rm = TRUE),
        upper_ci = quantile(coef, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
    rm(coefBootstrap)
    
    # # main estimate on original data
    # if (!is.null(weights) && length(weights) != 0) {
    #   data <- getWeights(data, weights[[groupLevel]][[strataLevel]])
    # }
    main_est <- getIRR(data, ci = NULL, outcomes = outcomes)
    
    results <- main_est |>
      select(!c("lower_ci", "upper_ci")) |>
      left_join(ci_tbl, by = c("outcome_name", "weighting"))
    
  } else if (ci == "midp") {
    # if (!is.null(weights) && length(weights) != 0) {
    #   data <- getWeights(data, weights[[groupLevel]][[strataLevel]])
    # }
    # data <- data |> pivotSurvivalData(outcomes)
    results <- getIRR(data, ci = "midp", outcomes = outcomes)
    
  } else {
    stop("Unsupported ci method: ", ci)
  }
  
  # tidy the risk estimates
  results <- results |>
    pivot_longer(
      cols = c(
        'person_days_comparator', 'person_days_exposed', 
        'cases_comparator', 'cases_exposed', 'person_days_median_comparator', 
        'person_days_median_exposed', 'person_days_q25_comparator', 
        'person_days_q25_exposed', 'person_days_q75_comparator', 'person_days_q75_exposed',
        'person_days_min_comparator', 'person_days_min_exposed', 'person_days_max_comparator', 
        'person_days_max_exposed', 'subject_count_comparator', 'subject_count_exposed', 
        'coef', 'lower_ci', 'upper_ci'
      ),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    mutate(
      variable_level = case_when(
        grepl("exposed", estimate_name) ~ "exposed",
        grepl("comparator", estimate_name) ~ "comparator",
        .default = NA
      ),
      estimate_name = case_when(
        grepl("cases", estimate_name) ~ "outcome_count",
        grepl("subject_count", estimate_name)  ~ "record_count",
        .default = gsub("_exposed|_comparator", "", estimate_name)
      ),
      variable_name = case_when(
        estimate_name %in% c('coef', 'lower_ci', 'upper_ci') ~ "Relative Risk",
        grepl("person_days", estimate_name) ~ "Person-Days",
        estimate_name == "outcome_count" ~ "Number events",
        estimate_name == "record_count" ~ "Number pregnancies"
      ),
      estimate_value = as.character(estimate_value),
      estimate_name = gsub("person_days_", "", estimate_name)
    )
  
  return(results)
}

getRiskEstimate <- function(data, group, strata, outcomes, weights = NULL, ci = "midp") {
  # proceed only if we have reasonable size
  if (data |> tally() |> pull() <= 10) {
    return(tibble(
      variable_level = character(),
      estimate_type = character(),
      estimate_name = character(),
      estimate_value = character(),
      variable_name = character(),
      outcome_name = character(),
      weighting = character()
    ))
  }
  
  strata <- unlist(strata)
  strata <- unique(c(strata[strata != "exposure"], "overall"))
  
  if (!grepl("CPRD GOLD|SIDIAP", cdmName(cdm))) {
    # eveyrthing possible in the server in the server
    dataProcess <- data |>
      mutate(overall = "overall") |>
      compute()
    
    for (nm in unique(dataProcess |> pull(cohort_name))) {
      for (strataName in strata) {
        strataLevels <- unique(dataProcess |> pull(.data[[strataName]]))
        for (strataLevel in strataLevels) {
          dataProcess |>
            filter(.data[[strataName]] == .env$strataLevel) |>
            processGroupStrata(
              groupLevel = nm,
              strataLevel = strataLevel,
              outcomes = outcomes,
              weights = weights,
              ci = ci
            )
        }
      }
    }
    
  } else {
    # all local
    nestedData <- data |>
      mutate(overall = "overall") |>
      tidyr::pivot_longer(
        cols = all_of(strata),
        names_to = "strata_name",
        values_to = "strata_level"
      ) |>
      rename_with(~ gsub("`", "", .x)) |>
      group_by(
        group_name = group,               
        group_level = .data[[group]],
        strata_name,
        strata_level
      ) |>
      collect() |>
      nest()
    
    results <- nestedData |>
      mutate(
        results = purrr::pmap(
          .l = list(data, group_level, strata_level),
          .f = function(data, groupLevel, strataLevel) {
            processGroupStrata(
              data = data,
              groupLevel = groupLevel,
              strataLevel = strataLevel,
              outcomes = outcomes,
              weights = weights,
              ci = ci
            )
          }
        )
      ) |>
      select(-data) |>
      unnest(results)
  }
  
  
  return(results)
}

estimateSurvivalRisk <- function(cohort, outcomes, outcomeGroup, end, strata, group, weights = NULL, ci = "midp") {
  cdm <- omopgenerics::cdmReference(cohort)
  
  studyAnalysis <- switch(
    end,
    "cohort_end_date" = "main",
    "cohort_end_date_sensitivity" = "sensitivity",
    "end_42_days_or_pregnancy" = "main",
    "end_42_days" = "secondary",
    "end_42_days_or_pregnancy_sensitivity" = "sensitivity",
    "end_42_days_sensitivity" = "secondary_sensitivity",
    "week_19_end" = "main",
    "week_19_end_sensitivity" = "sensitivity",
    "week_37_end" = "main",
    "week_37_end_sensitivity" = "sensitivity",
    "pregnancy_end" = "main",
    "pregnancy_end_sensitivity" = "sensitivity",
    "postpartum_6_weeks" = "main",
    "postpartum_6_weeks_sensitivity" = "sensitivity",
    "postpartum_12_weeks" = "main",
    "postpartum_12_weeks_sensitivity" = "sensitivity",
    NA_character_
  )
  
  survival_data <- getSurvivalData(cohort, outcomes = outcomes, end = end, strata = strata, group = group, weights = weights)
  
  results <- getRiskEstimate(survival_data, group = group, strata = strata, outcomes = outcomes, weights = weights, ci = ci) |>
    mutate(
      follow_up_end = end, 
      confidence_interval = ci, 
      result_id = if_else(weighting == "TRUE", 1L, 2L),
      estimate_type = "numeric",
      cdm_name = cdmName(cdm)
    ) |>
    select(!weighting) 
  
  results <- results |>
    omopgenerics::uniteAdditional(cols = c("outcome_name", "follow_up_end", "confidence_interval")) |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1:2L,
        result_type = "incidence_rate_ratio", 
        package_name = "study_code",
        package_version = "v0.0.1",
        outcome_group = outcomeGroup,
        study_analysis = studyAnalysis,
        weighting = c("TRUE", "FALSE")
      )
    )
  
  return(results)
}

suppressRiskEstimates <- function(result) {
  set <- settings(result)
  # result <- result |>
  #   group_by(result_id, cdm_name, group_name, group_level, strata_name, strata_level, additional_name, additional_level) |>
  #   mutate(
  #     sup_group = if_else(any(estimate_name == "count" & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5), TRUE, FALSE),
  #     sup_row = if_else(grepl("count", estimate_name) & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5, TRUE, FALSE),
  #     sup_estimate = if_else(sum(variable_name == "Number events" & estimate_value == "0") == 2, TRUE, FALSE)
  #   ) |>
  #   ungroup() |>
  #   mutate(
  #     estimate_value = case_when(
  #       estimate_name != "count" & sup_group ~ "-",
  #       estimate_name == "count" & sup_row ~ "-",
  #       estimate_name %in% c("coef", "lower_ci", "upper_ci") & sup_estimate ~ NA_character_,
  #       .default = estimate_value
  #     )
  #   ) |>
  #   select(!c("sup_group", "sup_row", "sup_estimate"))
  
  result <- result |>
    group_by(result_id, cdm_name, group_name, group_level, strata_name, strata_level, additional_name, additional_level) |>
    mutate(
      sup_group = if_else(any(variable_name == "Number pregnancies" & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5), TRUE, FALSE), # will save number of records later if >5
      sup_subgroup = if_else(any(variable_name == "Number events" & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5), TRUE, FALSE), # will save number of records later if >5
      sup_row = if_else(grepl("count", estimate_name) & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5, TRUE, FALSE),
      sup_estimate = if_else(any(sum(variable_name == "Number events" & estimate_value == "0") == 2), TRUE, FALSE)
    ) |>
    ungroup() |>
    mutate(
      estimate_value = case_when(
        !grepl("count", estimate_name) & sup_group ~ "-",
        grepl("count", estimate_name) & sup_row ~ "-",
        .data$variable_name == "Relative Risk" & sup_estimate ~ NA_character_,
        .data$variable_name == "Relative Risk" & sup_subgroup ~ NA_character_,
        .default = estimate_value
      )
    ) |>
    select(!c("sup_group", "sup_row", "sup_estimate", "sup_subgroup"))
  
  result |>
    newSummarisedResult(
      settings = set |> mutate(min_cell_count = "5")
    )
}

summaryCohort <- function(cohort) {
  bind(summariseCohortCount(cohort), summariseCohortAttrition(cohort))
}

summariseGestationalWeek <- function(x, strata.k) {
  if (strata.k == "overall") strata.k <- NULL
  x |>
    group_by_at(c("cohort_name", strata.k, "exposure", "gestational_week")) |>
    tally(name = "estimate_value", wt = .data$weight) |>
    ungroup() |>
    uniteGroup("cohort_name") |>
    uniteStrata(c(strata.k, "exposure")) |>
    uniteAdditional() |>
    mutate(
      result_id = 1L,
      variable_name = "gestational_week",
      variable_level = as.character(gestational_week),
      estimate_name = "count",
      estimate_type = "numeric"
    )
}

summariseCalendarWeek <- function(x, strata.k) {
  if (strata.k == "overall") strata.k <- NULL
  x |>
    group_by_at(c("cohort_name", strata.k, "exposure", "calendar_week")) |>
    tally(name = "estimate_value", wt = .data$weight) |>
    ungroup() |>
    uniteGroup("cohort_name") |>
    uniteStrata(c(strata.k, "exposure")) |>
    uniteAdditional() |>
    mutate(
      result_id = 1L,
      variable_name = "calendar_week",
      variable_level = as.character(calendar_week),
      estimate_name = "count",
      estimate_type = "numeric"
    )
}

summariseTimeDistribution <- function(cdm, strata, weights = NULL) {
  if (is.null(weights)) {
    weighting <- "FALSE"
    tab <- cdm$study_population |>
      mutate(weight = 1)
  } else {
    weighting <- "TRUE"
    tab <- cdm$study_population
  }
  
  timeDistribution <- NULL
  strata <- unlist(strata) |> unique()
  strataNew <- c(strata[strata != "exposure"], "overall")
  tab <- tab |>
    collect() |>
    mutate(
      overall = "overall",
      calendar_week = lubridate::floor_date(cohort_start_date, unit = "week"),
      gestational_week = as.numeric(.data$gestational_day)
    )  |>
    mutate(
      gestational_week = cut(
        x = .data$gestational_week,
        breaks = seq.int(0, 400, 7),
        labels = paste0("Week ", 0:56),
        right = FALSE
      )
    )
  for (group in settings(cdm$study_population)$cohort_name) { # group level
    for (strata.k in strataNew) { # strata name
      strataLevels <- unique(tab[,strata.k]) |> pull()
      for(strataLevel.k in strataLevels) { # strata level
        # data
        data.k <- tab |>
          filter(cohort_name == group, .data[[strata.k]] == strataLevel.k)
        if (nrow(data.k) > 10) {
          # weights
          if (length(weights) != 0) {
            data.k <- getWeights(data.k, weights[[group]][[strataLevel.k]])
          }
          # time distribution
          timeDistribution <- dplyr::bind_rows(
            timeDistribution,
            summariseGestationalWeek(data.k, strata.k),
            summariseCalendarWeek(data.k, strata.k)
          )
        }
      }
    }
  }
  
  # summarised result
  timeDistribution <- timeDistribution |>
    dplyr::mutate(
      cdm_name = cdmName(cdm),
      estimate_value = as.character(estimate_value)
    ) |>
    select(omopgenerics::resultColumns()) |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        result_type = "gestational_time_distributions",
        package_name = "study_code",
        package_version = "v0.0.1",
        group = "cohort_name",
        strata = strata |> paste0(collapse = " &&& "),
        additional = "",
        weighting = weighting
      )
    )
  
  return(timeDistribution)
}

getRegion <- function(x) {
  database_name <- omopgenerics::cdmName(omopgenerics::cdmReference(x))
  name <- omopgenerics::tableName(x)
  if (database_name == "CPRD GOLD") {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "care_site_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$care_site |>
          select("care_site_id", "location_id") |>
          left_join(cdm$location |> select("location_id", "region" = "location_source_value"), by = "location_id"),
        by = "care_site_id"
      ) |>
      select(!"care_site_id")
  } else if (database_name == "NLHR@UiO") {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "location_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$location |> select("location_id", "region" = "county"),
        by = "location_id"
      ) |>
      select(!"location_id")
  } else {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "location_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$location |> select("location_id", "region" = "location_source_value"),
        by = "location_id"
      ) |>
      select(!"location_id")
  }
  x |>
    mutate(region = as.character(region)) |>
    compute(name = name, temporary = FALSE)
}

applyPopulationWashout <- function(x, censorDate = "pregnancy_start_date") {
  x |>
    # No COVID-19
    requireCohortIntersect(
      targetCohortTable = "covid_washout",
      window = list(c(-90, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    )  |>
    # No acute AESI (90)
    requireCohortIntersect(
      targetCohortTable = "aesi_90_washout",
      window = list(c(-90, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    # No recurrent AESI (30)
    requireCohortIntersect(
      targetCohortTable = "aesi_30_washout",
      window = list(c(-30, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    # No chronic AESI (Inf)
    requireCohortIntersect(
      targetCohortTable = "aesi_inf",
      window = list(c(-Inf, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    ) |>
    # MAE washout during pregnancy
    requireCohortIntersect(
      targetCohortTable = "mae_washout",
      window = list(c(-Inf, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL,
      censorDate = censorDate
    ) |>
    # NON mRNA washout
    requireCohortIntersect(
      targetCohortTable = "non_mrna_covid_vaccines",
      window = list(c(-Inf, 0)),
      intersections = 0,
      indexDate = "exposure_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL,
      censorDate = censorDate
    )
}

addSeason <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  cohort |>
    dplyr::mutate(
      season_yearly = dplyr::case_when(
        clock::get_month(.data$cohort_start_date) %in% 3:5 ~ paste0("Spring ", as.character(clock::get_year(.data$cohort_start_date))),
        clock::get_month(.data$cohort_start_date) %in% 6:8 ~ paste0("Summer ", as.character(clock::get_year(.data$cohort_start_date))),
        clock::get_month(.data$cohort_start_date) %in% 9:11 ~ paste0("Autumn ", as.character(clock::get_year(.data$cohort_start_date))),
        clock::get_month(.data$cohort_start_date) %in% c(12, 1:2) ~ paste0("Winter ", as.character(clock::get_year(.data$cohort_start_date)))
      ),
      season = dplyr::case_when(
        clock::get_month(.data$cohort_start_date) %in% 3:5 ~ "Spring",
        clock::get_month(.data$cohort_start_date) %in% 6:8 ~ "Summer",
        clock::get_month(.data$cohort_start_date) %in% 9:11 ~ "Autumn",
        clock::get_month(.data$cohort_start_date) %in% c(12, 1:2) ~ "Winter"
      )
    ) |>
    dplyr::compute(name = name, temporary = FALSE)
}

addEthnicity <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  database <- omopgenerics::cdmName(omopgenerics::cdmReference(cohort))
  if (grepl("CPRD", database)) {
    cohort <- cohort |>
      inner_join(
        cdm$person |>
          dplyr::select("subject_id" = "person_id", "concept_id" = "race_concept_id"),
        by = "subject_id"
      ) |>
      inner_join(
        cdm$concept |>
          dplyr::select("concept_id", "ethnicity" = "concept_name")
      ) |>
      dplyr::mutate(
        ethnicity = dplyr::if_else(.data$concept_id == 0, "Missing", .data$ethnicity)
      ) |>
      dplyr::select(!"concept_id") |>
      dplyr::compute(name = name, temporary = FALSE)
  } else if (grepl("SIDIAP", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_concept_id == 4087925) |>
          mutate(
            nationallity = case_when(
              value_as_string %in% c("Espanya", "Europa meridional", "Europa occidental", "Europa oriental", "Europa septentrional") ~ "Europe",
              value_as_string %in% c("Àsia central", "Àsia meridional", "Àsia occidental", "Àsia oriental", "Àsia sud-oriental") ~ "Asia",
              value_as_string %in% c("Amèrica del Nord") ~ "North America",
              value_as_string %in% c("Austràlia i Nova Zelanda") ~ "Oceania",
              value_as_string %in% c("Amèrica central", "Amèrica del Sud", "Carib") ~ "Central/South America",
              value_as_string %in% c("Àfrica central", "Àfrica meridional", "Àfrica occidental", "Àfrica oriental", "Àfrica septentrional") ~ "Africa",
              .default = "Missing"
            )
          ) |>
          select("subject_id" = "person_id", "nationallity"),
        by = "subject_id"
      ) |>
      mutate(nationallity = if_else(is.na(nationallity), "Missing", nationallity)) |>
      compute(name = name, temporary = FALSE)
    
  } else if (grepl("SCIFI-PEARL", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_concept_id == 4197735) |>
          mutate(
            birth_continent = case_when(
              value_as_string %in% c("EU28 utom Norden", "Europa utom EU28 och Norden", "Norden utom Sverige", "Sverige", "Sovjetunionen") ~ "Europe",
              value_as_string %in% c("Asien") ~ "Asia",
              value_as_string %in% c("Nordamerika") ~ "North America",
              value_as_string %in% c("Oceanien") ~ "Oceania",
              value_as_string %in% c("Sydamerika") ~ "Central/South America",
              value_as_string %in% c("Afrika") ~ "Africa",
              .default = "Missing"
            )
          ) |>
          select("subject_id" = "person_id", "birth_continent"),
        by = "subject_id"
      ) |>
      mutate(birth_continent = if_else(is.na(birth_continent), "Missing", birth_continent)) |>
      dplyr::compute(name = name, temporary = FALSE)
    
  } else if (grepl("NLHR@UiO", database)) {
    cohort <- cohort |>
      inner_join(
        cdm$mother_table |> 
          mutate(
            birth_continent = case_when(
              birth_region == "Africa" ~ "Africa",
              birth_region == "America" ~ "America",
              birth_region == "Asia" ~ "Asia",
              birth_region == "Oceania" ~ "Oceania",
              birth_region == "Europe/Norway" ~ "Europe",
              birth_region == "Norway" ~ "Europe",
              .default = "Missing"
            )
          ) |>
          select("subject_id", "pregnancy_start_date", "pregnancy_end_date", "birth_continent")
      ) |>
      mutate(birth_continent = if_else(is.na(birth_continent), "Missing", birth_continent)) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  return(cohort)
}

addSocioeconomicStatus <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  database <- omopgenerics::cdmName(omopgenerics::cdmReference(cohort))
  if (grepl("CPRD", database)) {
    cohort <- cohort |>
      dplyr::left_join(
        cdm$measurement |>
          dplyr::filter(measurement_concept_id == 715996) |>
          dplyr::select("subject_id" = "person_id", "socioeconomic_status" = "value_as_number") |> 
          dplyr::mutate(
            socioeconomic_status = case_when(
              socioeconomic_status %in% 1:2 ~ "Q1",
              socioeconomic_status %in% 3:4 ~ "Q2",
              socioeconomic_status %in% 5:6 ~ "Q3",
              socioeconomic_status %in% 7:8 ~ "Q4",
              socioeconomic_status %in% 9:10 ~ "Q5",
              .default = "Missing"
            )
          ),
        by = "subject_id"
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
    
  } else if (grepl("SIDIAP", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_source_value == "qmedea11") |>
          select("subject_id" = "person_id", "socioeconomic_status" = "value_as_string"),
        by = "subject_id"
      ) |>
      mutate(socioeconomic_status = if_else(is.na(socioeconomic_status), "Missing", socioeconomic_status)) |>
      compute(name = name, temporary = FALSE)
    
  } else if (grepl("SCIFI-PEARL", database)) {
    values2020 <- cdm$observation |>
      filter(observation_concept_id == 4076114, year(observation_date) == 2020) |>
      pull(value_as_number)
    quintiles <- quantile(values2020, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
    q1 <- as.numeric(quintiles[[1]])
    q2 <- as.numeric(quintiles[[2]])
    q3 <- as.numeric(quintiles[[3]])
    q4 <- as.numeric(quintiles[[4]])
    
    # people who have both 2020 and 2021
    tableSES <- cdm$observation |>
      filter(
        observation_concept_id == 4076114,
        lubridate::year(observation_date) %in% c(2020, 2021)
      ) |>
      group_by(person_id) |>
      mutate(has2020 = max(if_else(lubridate::year(observation_date) == 2020, 1L, 0L))) |>
      filter(
        (has2020 == 1 & year(observation_date) == 2020) |
          (has2020 == 0 & year(observation_date) == 2021) # if has 2020, drop 2021, otherwise keep 2021
      ) |>
      mutate(
        socioeconomic_status = case_when(
          is.na(value_as_number) ~ "Missing",
          value_as_number <= q1  ~ "Q1",
          value_as_number <= q2  ~ "Q2",
          value_as_number <= q3  ~ "Q3",
          value_as_number <= q4  ~ "Q4",
          value_as_number >  q4  ~ "Q5",
          .default               = "Missing"
        )
      ) |>
      select(subject_id = person_id, socioeconomic_status) |>
      distinct() |>
      compute()
    
    # add column
    cohort <- cohort |>
      left_join(
        tableSES,
        by = "subject_id"
      ) |>
      mutate(socioeconomic_status = if_else(is.na(socioeconomic_status), "Missing", socioeconomic_status)) |>
      compute(name = name, temporary = FALSE)
  }
  
  return(cohort)
}

getBRCharacteristics <- function(cohort, strata) {
  
  # Variables to add: socioeconomic status, ethnicity, season and period pregnancy start
  # Pregnancy: previous pregnancy
  # Comorbidities: alchohol, obesity, diabetes, hypertension, asthma, depression/anxiety, epilepsy
  # Medications: omeprazole/antiacids, diabetes treatment/s, nsaids, opioids, antidepressants, antiepilepsy, corticosteroids
  estimates = list(
    'season' = c('count', 'percentage'),
    'season_yearly' = c('count', 'percentage'),
    'ethnicity' = c('count', 'percentage'),
    'socioeconomic_status' = c('count', 'percentage'),
    'maternal_age' = c('min', 'max', 'q25', 'q75', 'median', 'sd', 'mean'),
    'maternal_age_group' = c('count', 'percentage'),
    'trimester' = c('count', 'percentage'),
    'nationallity' = c('count', 'percentage'),
    'birth_continent' = c('count', 'percentage'),
    'pregnancy_start_period' = c('count', 'percentage')
  )
  estimates <- estimates[names(estimates) %in% colnames(cohort)]
  otherVariables = names(estimates)
  cohort |>
    summariseCharacteristics(
      counts = TRUE,
      demographics = TRUE,
      strata = strata,
      cohortIntersectFlag = list(
        # covariatesInf (-Inf, 0)
        "History of comorbidities" = list(
          targetCohortTable = "covariates_inf", window = c(-Inf, 0)
        ),
        # covariates1 (-365, 0)
        "Covariates in the past 5 years" = list(
          targetCohortTable = "covariates_5", window = c(-365*5, 0)
        ),
        # Comedications
        "Medications in the past year" = list(
          targetCohortTable = "comedications", window = c(-365, 0)
        )
      ),
      cohortIntersectCount = list(
        # covid infections (-Inf, 0)
        "Previous pregnancies" = list(
          targetCohortTable = "mother_table", window = c(-Inf, 0)
        )
      ),
      otherVariables = otherVariables,
      estimates = estimates
    )
}

getMiscarriageDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_end_date = as.Date(!!dateadd("pregnancy_start_date", 19*7 + 6))
    ) |>
    mutate(
      cohort_end_date = if_else(
        cohort_end_date > pregnancy_end_date, pregnancy_end_date, cohort_end_date
      ), 
      trimester_2_end = if_else(!is.na(trimester_2_end), cohort_end_date, trimester_2_end),
      trimester_3_start = as.Date(NA),
      trimester_3_end = as.Date(NA)
    ) |>
    compute(name = "miscarriage_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("miscarriage_denominator")
      )
    )
}

getStillbirhtDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "stillbirth_denominator", temporary = FALSE) |>
    recordCohortAttrition("Stillbirth denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("stillbirth_denominator")
      )
    )
}

getPretermDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "preterm_labour_denominator", temporary = FALSE) |>
    recordCohortAttrition("Preterm labour denominator") %>%
    mutate(
      cohort_end_date = as.Date(!!dateadd("pregnancy_start_date", 37*7))
    ) |>
    mutate(
      cohort_end_date = if_else(
        cohort_end_date > pregnancy_end_date, pregnancy_end_date, cohort_end_date
      )
    ) |>
    compute(name = "preterm_labour_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("preterm_labour_denominator")
      )
    )
}

getPostpartum6Denominator <- function(cohort) {
  cohort |>
    filter(pregnancy_outcome_study == "livebirth") |>
    mutate(
      cohort_start_date = postpartum_6_start,
      cohort_end_date = postpartum_6_end
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "postpartum_6_weeks_denominator", temporary = FALSE) |>
    recordCohortAttrition("Postpartum 6 weeks denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("postpartum_6_weeks_denominator")
      )
    )
}

getPostpartum12Denominator <- function(cohort) {
  cohort |>
    filter(pregnancy_outcome_study == "livebirth") |>
    mutate(
      cohort_start_date = postpartum_12_start,
      cohort_end_date = postpartum_12_end
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "postpartum_12_weeks_denominator", temporary = FALSE) |>
    recordCohortAttrition("Postpartum 12 weeks denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("postpartum_12_weeks_denominator")
      )
    )
}

getMaternalDeathDenominator <- function(cohort) {
  cohort |>
    mutate(
      cohort_start_date = pregnancy_end_date,
      cohort_end_date = postpartum_6_weeks
    ) |>
    compute(name = "maternal_death_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("maternal_death_denominator")
      )
    )
}

getTimeToEvent <- function(cohort, washOut, outcomes) {
  name <- omopgenerics::tableName(cohort)
  postpartum <- "maternal_death"
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  
  if (washOut != 0) {
    for (outcome in outcomes) {
      colsExclude <- c("time_t4", "status_t4", "pregnancies_t4")
      if (outcome %in% postpartum) colsExclude <- NULL
      if (outcome %in% onlyPostpartum) colsExclude <- c("time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3")
      colsRename <- c("time", "status", "pregnancies", "time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3", "time_t4", "status_t4", "pregnancies_t4")
      colsRename <- colsRename[!colsRename %in% colsExclude]
      
      postpartumColStart <- "postpartum_6_start"
      postpartumColEnd <- "postpartum_6_end"
      if (outcome == "postpartum_haemorrhage") {
        postpartumColStart <- "postpartum_12_start"
        postpartumColEnd <- "postpartum_12_end"
      }
      
      cohort <- cohort %>%
        mutate(prior_outcome_washout = !!dateadd(glue::glue("prior_{outcome}"), washOut)) %>%
        mutate(
          # time at risk considering wash-out
          date_time_at_risk_start = case_when(
            is.na(.data$prior_outcome_washout) ~ cohort_start_date,
            .data$prior_outcome_washout < .data$cohort_start_date ~ cohort_start_date,
            .data$prior_outcome_washout >= .data$cohort_start_date ~ prior_outcome_washout
          ),
          # time-status overall
          status = case_when(
            date_time_at_risk_start > cohort_end_date ~ NA, # don't contribute
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute (either no time at risk or outcome before)
            .data[[outcome]] >= date_time_at_risk_start ~ 1
          ),
          time = case_when(
            status == 1 ~ !!datediff("date_time_at_risk_start", outcome),
            status == 0 ~ !!datediff("date_time_at_risk_start", "cohort_end_date"),
            is.na(status) ~ NA
          ),
          pregnancies = if_else(is.na(status), 0, 1),
          # time-status trimester 1
          status_t1 = case_when(
            date_time_at_risk_start > trimester_1_end ~ NA, # don't contribute
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= date_time_at_risk_start & .data[[outcome]] <= trimester_1_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_1_end ~ 0 # outcome after time at risk
          ),
          time_t1 = case_when(
            status_t1 == 1 ~ !!datediff("date_time_at_risk_start", outcome),
            status_t1 == 0 ~ !!datediff("date_time_at_risk_start", "trimester_1_end"),
            is.na(status_t1) ~ NA
          ),
          pregnancies_t1 = if_else(is.na(status_t1), 0, 1),
          # time-status trimester 2
          status_t2 = case_when(
            is.na(trimester_2_start) | date_time_at_risk_start > trimester_2_end ~ NA, # don't contribute
            .data[[outcome]] < trimester_2_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= trimester_2_start & .data[[outcome]] <= trimester_2_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_2_end ~ 0 # outcome after time at risk
          ),
          time_t2 = case_when(
            status_t2 == 1 ~ !!datediff("trimester_2_start", outcome) + 1,
            status_t2 == 0 ~ !!datediff("trimester_2_start", "trimester_2_end") + 1,
            is.na(status_t2) ~ NA
          ),
          pregnancies_t2 = if_else(is.na(status_t2), 0, 1),
          # time-status trimester 3
          status_t3 = case_when(
            is.na(trimester_3_start) | date_time_at_risk_start > trimester_3_end ~ NA, # don't contribute
            .data[[outcome]] < trimester_3_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= trimester_3_start & .data[[outcome]] <= trimester_3_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_3_end ~ 0 # outcome after time at risk
          ),
          time_t3 = case_when(
            status_t3 == 1 ~ !!datediff("trimester_3_start", outcome) + 1,
            status_t3 == 0 ~ !!datediff("trimester_3_start", "trimester_3_end") + 1,
            is.na(status_t3) ~ NA
          ),
          pregnancies_t3 = if_else(is.na(status_t3), 0, 1),
          # time-status postpatum 6 weeks
          status_t4 = case_when(
            is.na(.data[[postpartumColStart]]) | date_time_at_risk_start > .data[[postpartumColEnd]] ~ NA, # don't contribute
            .data[[outcome]] < .data[[postpartumColStart]] & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= .data[[postpartumColStart]] & .data[[outcome]] <= .data[[postpartumColEnd]] ~ 1, # outcome during time at risk
            .data[[outcome]] > .data[[postpartumColEnd]] ~ 0 # outcome after time at risk
          ),
          time_t4 = case_when(
            status_t4 == 1 ~ !!datediff(postpartumColStart, outcome) + 1,
            status_t4 == 0 ~ !!datediff(postpartumColStart, postpartumColEnd) + 1,
            is.na(status_t4) ~ NA
          ),
          pregnancies_t4 = if_else(is.na(status_t4), 0, 1)
        ) |>
        select(!all_of(c(outcome, "prior_outcome_washout", "date_time_at_risk_start", glue::glue("prior_{outcome}"), colsExclude))) |>
        rename_with(
          .fn = \(x){paste0(outcome, "_", x)},
          .cols = colsRename
        ) |>
        compute()
    }
  } else {
    for (outcome in outcomes) {
      colsExclude <- c("time_t4", "status_t4", "pregnancies_t4")
      if (outcome %in% postpartum) colsExclude <- NULL
      if (outcome %in% onlyPostpartum) colsExclude <- c("time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3")
      colsRename <- c("time", "status", "pregnancies", "time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3", "time_t4", "status_t4", "pregnancies_t4")
      colsRename <- colsRename[!colsRename %in% colsExclude]
      
      postpartumColStart <- "postpartum_6_start"
      postpartumColEnd <- "postpartum_6_end"
      if (outcome == "postpartum_haemorrhage") {
        postpartumColStart <- "postpartum_12_start"
        postpartumColEnd <- "postpartum_12_end"
      }
      
      cohort <- cohort %>%
        mutate(
          # overall
          status = if_else(is.na(.data[[outcome]]), 0, 1),
          time = if_else(is.na(.data[[outcome]]), !!datediff("cohort_start_date", "cohort_end_date"), !!datediff("cohort_start_date", outcome)),
          pregnancies = if_else(is.na(status), 0, 1),
          # trimester 1
          status_t1 = case_when(
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_1_end ~ 0, # outcome after trimester 1
            .data[[outcome]] <= trimester_1_end ~ 1
          ),
          time_t1 = if_else(status_t1 == 0, !!datediff("trimester_1_start", "trimester_1_end"), !!datediff("cohort_start_date", outcome)),
          pregnancies_t1 = if_else(is.na(status_t1), 0, 1),
          # trimester 2
          status_t2 = case_when(
            is.na(trimester_2_start) ~ NA, # don't get to trimester 2
            .data[[outcome]] < trimester_2_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_2_end ~ 0, # outcome after trimester 2
            .data[[outcome]] <= trimester_2_end | .data[[outcome]] >= trimester_2_start  ~ 1
          ),
          time_t2 = case_when(
            status_t2 == 1 ~ !!datediff("trimester_2_start", outcome) + 1,
            status_t2 == 0 ~ !!datediff("trimester_2_start", "trimester_2_end") + 1,
            is.na(status_t2) ~ NA
          ),
          pregnancies_t2 = if_else(is.na(status_t2), 0, 1),
          # trimester 3
          status_t3 = case_when(
            is.na(trimester_3_start) ~ NA, # don't get to trimester 3
            .data[[outcome]] < trimester_3_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_3_end ~ 0, # outcome after trimester 3
            .data[[outcome]] >= trimester_3_start  ~ 1
          ),
          time_t3 = case_when(
            status_t3 == 1 ~ !!datediff("trimester_3_start", outcome) + 1,
            status_t3 == 0 ~ !!datediff("trimester_3_start", "trimester_3_end") + 1,
            is.na(status_t3) ~ NA
          ),
          pregnancies_t3 = if_else(is.na(status_t3), 0, 1),
          # time-status postpatum 6 weeks
          status_t4 = case_when(
            is.na(.data[[postpartumColStart]]) ~ NA, # don't get to postpartum 
            .data[[outcome]] < .data[[postpartumColStart]] & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before 
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] >= .data[[postpartumColStart]] & .data[[outcome]] <= .data[[postpartumColEnd]] ~ 1, # outcome during time at risk
            .data[[outcome]] > .data[[postpartumColEnd]] ~ 0 # outcome after time at risk
          ),
          time_t4 = case_when(
            status_t4 == 1 ~ !!datediff(postpartumColStart, outcome) + 1,
            status_t4 == 0 ~ !!datediff(postpartumColStart, postpartumColEnd) + 1,
            is.na(status_t4) ~ NA
          ),
          pregnancies_t4 = if_else(is.na(status_t4), 0, 1)
        ) |>
        select(!all_of(c(outcome, colsExclude))) %>% 
        {if (outcome %in% onlyPostpartum) {
          mutate(., time_t4 = time, status_t4 = status, pregnancies_t4 = pregnancies)
        } else .} |>
        rename_with(
          .fn = \(x){paste0(outcome, "_", x)},
          .cols = colsRename
        ) |>
        compute()
    }
  }
  cohort <- cohort |>
    compute(name = name, temporary = FALSE, overwrite = TRUE)
  return(cohort)
}

addBRDenominatorStrata <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  cohort <- cohort |>
    addAge(
      ageName = "maternal_age",
      ageGroup = list("12 to 17" = c(12, 17), "18 to 34" = c(18, 34), "35 to 50" = c(35, 50))
    ) |>
    mutate(
      pregnancy_start_period = case_when(
        year(pregnancy_start_date) %in% 2018:2019 ~ "Pre COVID-19",
        year(pregnancy_start_date) %in% 2020:2021 ~ "COVID-19 main outbreak",
        .default = "Post COVID-19 main outbreak"
      )
    ) |>
    compute(name = "pregnancy_denominator", temporary = FALSE)
  
  strata <- list("maternal_age", "pregnancy_start_period")
  if (cdmName(cdm) %in% c("CPRD AURUM", "CPRD GOLD", "SIDIAP")) {
    cohort <- cohort |>
      addSocioeconomicStatus() |>
      addEthnicity() |>
      compute(name = "pregnancy_denominator", temporary = FALSE)
    strata <- c(strata, list("socioeconomic_status", "ethnicity"))
  }
  return(cohort)
}

estimateIncidenceRate <- function(cohort, strata, outcomes) {
  variables <- c(
    paste0(outcomes, "_status"), paste0(outcomes, "_time"), paste0(outcomes, "_pregnancies"),
    paste0(outcomes, "_status_t1"), paste0(outcomes, "_time_t1"), paste0(outcomes, "_pregnancies_t1"),
    paste0(outcomes, "_status_t2"), paste0(outcomes, "_time_t2"), paste0(outcomes, "_pregnancies_t2"),
    paste0(outcomes, "_status_t3"), paste0(outcomes, "_time_t3"), paste0(outcomes, "_pregnancies_t3"),
    paste0(outcomes, "_status_t4"), paste0(outcomes, "_time_t4"), paste0(outcomes, "_pregnancies_t4")
  )
  variables <- variables[variables %in% colnames(cohort)]
  summarise_ir <- cohort |>
    summariseResult(
      strata = strata,
      includeOverallStrata = TRUE,
      variables = variables,
      estimates = "sum",
      counts = FALSE,
      weights = NULL
    ) |>
    reformatResult() |>
    addIncidenceRate()
  if ("pregnancy_start_period" %in% unlist(strata)) {
    summarise_ir <- summarise_ir |> addByPeriodEvents(cohort)
  }
  
  if (all(outcomes %in% c(
    "preterm_labour", "miscarriage", "stillbirth", "maternal_death",
    "dysfunctional_labour", "eclampsia", "ectopic_pregnancy",
    "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia",
    "postpartum_endometritis", "postpartum_haemorrhage", "miscarriage_codelist"
  ))) {
    summarise_ir <- summarise_ir |>
      mutate(
        additional_name = "outcome_group",
        additional_level = "Maternal Adverse Events"
      ) |>
      omopgenerics::newSummarisedResult()
  } else {
    summarise_ir <- summarise_ir |>
      mutate(
        additional_name = "outcome_group",
        additional_level = "Adverse Events of Special Interest"
      ) |>
      omopgenerics::newSummarisedResult()
  }
  
  return(summarise_ir)
}

reformatResult <- function(x) {
  x |>
    splitStrata() |>
    mutate(
      group_name = "outcome_cohort_name",
      group_level = gsub("_status|_time|_pregnancies|_t1|_t2|_t3|_t4", "", .data$variable_name),
      gestational_trimester = case_when(
        grepl("_t1", .data$variable_name) ~ "Trimester 1",
        grepl("_t2", .data$variable_name) ~ "Trimester 2",
        grepl("_t3", .data$variable_name) ~ "Trimester 3",
        grepl("_t4", .data$variable_name) ~ "Postpartum",
        .default = "overall"
      ),
      estimate_name = case_when(
        grepl("pregnancies", .data$variable_name) ~ "denominator_count",
        grepl("time", .data$variable_name) ~ "person_days_count",
        grepl("status", .data$variable_name) ~ "outcome_count",
      ),
      estimate_type = "integer"
    ) |>
    mutate(
      variable_name = "Incidence Rates"
    ) |>
    uniteStrata(cols = c(strataColumns(x), "gestational_trimester")) |>
    newSummarisedResult(settings = NULL)
}

addIncidenceRate <- function(x) {
  bind_rows(
    x,
    x |>
      select(!variable_name) |>
      mutate(estimate_value = as.numeric(estimate_value)) |>
      pivot_wider(values_from = "estimate_value", names_from = "estimate_name") |>
      mutate(
        person_years = round(.data$person_days_count / 365.25, 3),
        incidence_100000_pys = round(((.data$outcome_count / .data$person_years) * 100000), 3),
        incidence_100000_pys_95CI_lower = round(((stats::qchisq(p = 0.025, df = 2 * .data$outcome_count) / 2) / .data$person_years) * 100000, 3),
        incidence_100000_pys_95CI_upper = round(((stats::qchisq(p = 0.975, df = 2 * (.data$outcome_count + 1)) / 2) / .data$person_years) * 100000, 3)
      ) |>
      select(!c("denominator_count", "outcome_count", "person_days_count")) |>
      pivot_longer(
        cols = c("person_years", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper"),
        names_to = "estimate_name", values_to = "estimate_value"
      ) |>
      mutate(
        variable_name ="Incidence Rates",
        estimate_type = "numeric",
        estimate_value = as.character(estimate_value)
      )
  )
}

addByPeriodEvents <- function(x, cohort) {
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  postpartum <- "maternal_death"
  outcomes <- x$group_level |> unique()
  overallCounts <- x |>
    filter(estimate_name == "outcome_count") |>
    filterStrata(pregnancy_start_period %in% c("Pre COVID-19", "COVID-19 main outbreak")) |>
    splitStrata() |>
    mutate(
      estimate_value = as.numeric(estimate_value),
      gestational_trimester = omopgenerics::toSnakeCase(gestational_trimester)
    ) |>
    pivot_wider(names_from = "gestational_trimester", values_from = "estimate_value") |>
    select(any_of(c("group_level", "pregnancy_start_period", "overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
  result <- NULL
  for (outcome in outcomes) {
    periodResult <- cohort |>
      filter(.data[[paste0(outcome, "_status")]] == 1, pregnancy_start_period %in% c("Pre COVID-19", "COVID-19 main outbreak")) |>
      mutate(
        # socioeconomic_status = as.character(socioeconomic_status),
        days_to_end_period = case_when(
          pregnancy_start_period == "Pre COVID-19" ~ as.Date("2019-12-31"),
          pregnancy_start_period == "COVID-19 main outbreak" ~ as.Date("2021-12-31"),
          .default = as.Date(NA)
        )
      ) %>%
      mutate(
        days_to_end_period = !!datediff("pregnancy_start_date", "days_to_end_period")
      ) |>
      select(any_of(c("pregnancy_start_period", "days_to_end_period", paste0(outcome, c("_time", "_status", "_time_t1", "_status_t1", "_time_t2", "_status_t2", "_time_t3", "_status_t3", "_time_t4", "_status_t4"))))) |>
      collect()
    
    if (outcome %in% onlyPostpartum) {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t4_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t4")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t4_count = sum(outcome_in_period_t4_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t4_percentage = if_else(outcome_in_period_t4_count != 0, outcome_in_period_t4_count/postpartum * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    } else if (outcome %in% postpartum) {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t1_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t1")]] == 1, 1, 0),
          outcome_in_period_t2_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t2")]]  == 1, 1, 0),
          outcome_in_period_t3_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t3")]]  == 1, 1, 0),
          outcome_in_period_t4_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t4")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t1_count = sum(outcome_in_period_t1_count, na.rm = TRUE),
          outcome_in_period_t2_count = sum(outcome_in_period_t2_count, na.rm = TRUE),
          outcome_in_period_t3_count = sum(outcome_in_period_t3_count, na.rm = TRUE),
          outcome_in_period_t4_count = sum(outcome_in_period_t4_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t1_percentage = if_else(outcome_in_period_t1_count != 0, outcome_in_period_t1_count/trimester_1 * 100, 0),
          outcome_in_period_t2_percentage = if_else(outcome_in_period_t2_count != 0, outcome_in_period_t2_count/trimester_2 * 100, 0),
          outcome_in_period_t3_percentage = if_else(outcome_in_period_t3_count != 0, outcome_in_period_t3_count/trimester_3 * 100, 0),
          outcome_in_period_t4_percentage = if_else(outcome_in_period_t4_count != 0, outcome_in_period_t4_count/postpartum * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    } else {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t1_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t1")]] == 1, 1, 0),
          outcome_in_period_t2_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t2")]]  == 1, 1, 0),
          outcome_in_period_t3_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t3")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t1_count = sum(outcome_in_period_t1_count, na.rm = TRUE),
          outcome_in_period_t2_count = sum(outcome_in_period_t2_count, na.rm = TRUE),
          outcome_in_period_t3_count = sum(outcome_in_period_t3_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t1_percentage = if_else(outcome_in_period_t1_count != 0, outcome_in_period_t1_count/trimester_1 * 100, 0),
          outcome_in_period_t2_percentage = if_else(outcome_in_period_t2_count != 0, outcome_in_period_t2_count/trimester_2 * 100, 0),
          outcome_in_period_t3_percentage = if_else(outcome_in_period_t3_count != 0, outcome_in_period_t3_count/trimester_3 * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    }
    
    # bind results
    result <- bind_rows(result, periodResult)
  }
  if (nrow(result) == 0) return(x)
  result <- result |>
    pivot_longer(
      cols = any_of(c(
        paste0("outcome_in_period", c("_count", "_percentage")), paste0("outcome_in_period_t1", c("_count", "_percentage")),
        paste0("outcome_in_period_t2", c("_count", "_percentage")), paste0("outcome_in_period_t3", c("_count", "_percentage")),
        paste0("outcome_in_period_t4", c("_count", "_percentage")))
      ),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    mutate(
      result_id = 1L,
      cdm_name = unique(x$cdm_name),
      group_name = "outcome_cohort_name",
      variable_name = "Incidence Rates",
      variable_level = NA_character_,
      gestational_trimester = case_when(
        grepl("t1", .data$estimate_name) ~ "Trimester 1",
        grepl("t2", .data$estimate_name) ~ "Trimester 2",
        grepl("t3", .data$estimate_name) ~ "Trimester 3",
        grepl("t4", .data$estimate_name) ~ "Postpartum",
        .default = "overall"
      ),
      estimate_name = gsub("_t1|_t2|_t3|_t4", "", .data$estimate_name),
      estimate_type = if_else(grepl("count", estimate_name), "integer", "percentage"),
      estimate_value = as.character(estimate_value),
      additional_name = "overall",
      additional_level = "overall"
    ) |>
    uniteStrata(cols = c("pregnancy_start_period", "gestational_trimester")) |>
    distinct()
  
  return(bind_rows(x, result))
}

getMatchedCohort <- function(cohort, outcomes, name) {
  tmp <- tmpPrefix()
  tabName <- paste0(tmp, "matching")
  cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = tabName)
  
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  postpartum <- "maternal_death"
  
  strata <- c("maternal_age_group", "pregnancy_start_period")
  if (grepl("SIDIAP", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "nationallity")
  }
  if (grepl("CPRD", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "ethnicity")
  }
  if (grepl("NLHR@UiO", cdmName(cdm))) {
    strata <- c(strata, "birth_continent")
  }
  if (grepl("SCIFI-PEARL", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "birth_continent")
  }
  
  for (outcome in outcomes) {
    nameMatch <- paste0(tmp, "match")
    nameSample <- paste0(tmp, "sample")
    nameOriginal <- paste0(tmp, "original")
    outcome_cohort <- cohort |>
      filter(.data[[paste0(outcome, "_status")]] == 1) |>
      mutate(
        cohort_definition_id = 1L,
        cohort_start_date = as.Date(clock::add_days(.data$cohort_start_date, .data[[paste0(outcome, "_time")]])),
        cohort_end_date = cohort_start_date
      ) |>
      select(all_of(c(
        "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date", "pregnancy_end_date", "maternal_age", strata
      ))) |>
      compute(name = nameOriginal, temporary = FALSE, overwrite = TRUE) |>
      newCohortTable(
        cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = outcome),
        cohortAttritionRef = NULL,
        cohortCodelistRef = NULL
      )
    if (outcome_cohort |> tally() |> pull() > 5) {
      outcome_match <- cohort |>
        filter(.data[[paste0(outcome, "_status")]] == 0) |>
        select(all_of(c(
          "subject_id", "pregnancy_start_date", "pregnancy_end_date", "maternal_age", "cohort_end_date", strata
        ))) |>
        mutate(
          pregnancy_start_band = if_else(
            day(pregnancy_start_date) <= 15,
            paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
            paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
          ),
          age_group_sample = cut(maternal_age, !!seq(12, 56, 2), include.lowest = TRUE, right = FALSE)
        ) |>
        compute(name = nameMatch, temporary = FALSE, overwrite = TRUE)
      outcome_sampled <- outcome_cohort |>
        mutate(
          pregnancy_start_band = if_else(
            day(pregnancy_start_date) <= 15,
            paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
            paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
          ),
          age_group_sample = cut(maternal_age, !!seq(12, 56, 2), include.lowest = TRUE, right = FALSE)
        ) |>
        inner_join(
          outcome_match |>
            rename(
              "matched_subject_id" = "subject_id",
              "matched_pregnancy_start_date" = "pregnancy_start_date",
              "matched_pregnancy_end_date" = "pregnancy_end_date",
              "matched_maternal_age" = "maternal_age",
              "matched_cohort_end_date" = "cohort_end_date"
            ) |>
            rename_with(.fn = \(x){glue("matched_{x}")}, .cols = strata),
          by = c("age_group_sample", "pregnancy_start_band")
        ) |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) 
      if (outcome %in% onlyPostpartum) {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_end_date < cohort_start_date & matched_cohort_end_date > cohort_start_date)
      } else if (outcome %in% postpartum) {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_start_date < cohort_start_date & matched_cohort_end_date > cohort_start_date)
      } else {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_start_date < cohort_start_date & matched_pregnancy_end_date > cohort_start_date) 
      }
      outcome_sampled <- outcome_sampled |>
        slice_sample(n = 1, by = "matched_subject_id") |>
        slice_sample(n = 1, by = c("subject_id", "cohort_start_date")) |>
        select(!"matched_cohort_end_date") |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) 
      outcome_match <- outcome_sampled  |>
        select(!c(
          "subject_id",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          strata
        )) |>
        rename_with(.fn = \(x){gsub("matched_", "", x)}) |>
        select(c(
          "cohort_definition_id", "subject_id",
          "cohort_start_date", "cohort_end_date",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          strata
        )) |>
        compute(name = nameMatch, temporary = FALSE, overwrite = TRUE) |>
        newCohortTable(
          cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = paste0(outcome, "_matched")),
          cohortAttritionRef = NULL,
          cohortCodelistRef = NULL
        )
      outcome_sampled <- outcome_sampled |>
        select(c(
          "cohort_definition_id", "subject_id",
          "cohort_start_date", "cohort_end_date",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          strata
        )) |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) |>
        newCohortTable(
          cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = paste0(outcome, "_sampled")),
          cohortAttritionRef = NULL,
          cohortCodelistRef = NULL
        )
      cdm <- bind(outcome_cohort, outcome_match, outcome_sampled, cdm[[tabName]], name = tabName)
    }
  }
  cdm[[name]] <- cdm[[tabName]] |>
    compute(name = name, temporary = FALSE, overwrite = TRUE) |>
    newCohortTable()
  dropSourceTable(cdm = cdm, starts_with(tmp))
  return(cdm[[name]])
}

getCovariateList <- function(cdm) {
  covariatesPS <- c(
    "exposure", "age", "gestational_day", "cohort_start_date",
    "previous_observation", "previous_pregnancies", "previous_healthcare_visits",
    "alcohol_misuse_dependence", "obesity", "anxiety", "depression", 
    "pre_pregnancy_smoking"
  )
  covariatesSMD <- readr::read_csv(here::here("Codelists", "largeScaleSMD.csv")) |>
    filter(grepl(cdmName(cdm), cdm_name)) 
  
  if (grepl("CPRD GOLD", cdmName(cdm))) {
    covariatesPS <- c(covariatesPS, "ethnicity", "socioeconomic_status")
  }
  if (grepl("NLHR@UiO", cdmName(cdm))) {
    covariatesPS <- c(covariatesPS, "birth_continent")
  }
  if (grepl("SCIFI-PEARL", cdmName(cdm))) {
    covariatesPS <- c(covariatesPS, "birth_continent", "socioeconomic_status")
  }
  if (grepl("SIDIAP", cdmName(cdm))) {
    covariatesPS <- c(covariatesPS, "nationallity", "socioeconomic_status")
  }
  
  strataLevel <- c("overall","T1","T2","T3","pfizer","moderna","12 to 17","18 to 34","35 to 55")
  innerList <- setNames(lapply(strataLevel, \(s) covariatesPS), strataLevel)
  covariatesPS <- setNames(
    rep(list(innerList), 9), 
    c(paste0("population_objective_", 1:3), 
      paste0("population_miscarriage_objective_", 1:3), 
      paste0("population_preterm_labour_objective_", 1:3)
    )
  )
  
  for (nm in unique(covariatesSMD$cohort_name)) {
    for(stLevel in unique(covariatesSMD$strata_level)) {
      covariatesAdd <- covariatesSMD |>
        filter(cohort_name == nm, strata_level == stLevel) |>
        pull(identifier)
      covariatesPS[[nm]][[stLevel]] <- c(covariatesPS[[nm]][[stLevel]], covariatesAdd)
    }
  }
  
  return(covariatesPS)
}

asDensity <- function(data) {
  exposed <- density(data |> filter(exposure == "exposed") |> pull(ps))
  comparator <- density(data |> filter(exposure == "comparator") |> pull(ps))
  
  tibble(x = exposed$x, y = exposed$y, exposure = "exposed", variable_level = as.character(1:length(exposed$x))) |>
    bind_rows(tibble(x = comparator$x,  y = comparator$y, exposure = "comparator", variable_level = as.character(1:length(comparator$x)))) |>
    pivot_longer(cols = c("x", "y"), names_to = "estimate_name", values_to = "estimate_value")
}

cohortCodeUseFromCohort <- function(cohort) {
  cdm <- cdmReference(cohort)
  name <- tableName(cohort)
  summaryCodeUse <- NULL
  codelist <- attr(cohort, "cohort_codelist") |> collect()
  for (id in settings(cohort)$cohort_definition_id) {
    codelist.id <- codelist |>
      filter(cohort_definition_id == id)
    if (nrow(codelist.id) > 0) {
      codelist.id <- split(as.integer(codelist.id$concept_id), codelist.id$codelist_name)
      summaryCodeUse <- bind(
        summaryCodeUse,
        summariseCohortCodeUse(codelist.id, cdm, name, cohortId = id, timing = "entry")
      ) 
    }
  }
  return(summaryCodeUse)
}
