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
    select(
      !c(
        "gestational_length_in_day", "prev_pregnancy_gravidity", "pregnancy_single", 
        "overlap", "pregnancy_mode_delivery"
      )
    ) |>
    mutate(
      pregnancy_outcome_study = case_when(
        pregnancy_outcome == 4092289  & gestational_length <= 37*7~ "preterm_labour",
        pregnancy_outcome == 4092289 ~ "livebirth",
        pregnancy_outcome == 4067106 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 4081422 ~ "elective_termination",
        pregnancy_outcome == 4095714 ~ "discordant",
        .default = "unknown"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records") 
  
  return(cdm$mother_table)
}


getSourcePopulation <- function(cdm, objective, enrollment) {
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
          filter(cohort_definition_id == 1) |>
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
      cohort_start_date >= as.Date(clock::add_days("pregnancy_start_date", -.data$days)),
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
      mutate(
        subject_id = exposed_id, cohort_start_date = exposure_date, cohort_end_date = exposure_date
      )  |>
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

datesPivotLongerExprs <- function(cols) {
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
      mutate(number_pregnancies_weighted = weight)
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
          if (nrow(data.k) > 0) {
            # weights
            data.k <- getWeights(data.k, weights[[group]])
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

getFeaturesTable <- function(cdm, strata) {
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
    filter(concept_id != 0 & start_date <= cohort_start_date) %>% 
    mutate(
      time_start = !!datediff("start_date", "cohort_start_date")
    ) |>
    filter(time_start > 0) |>
    mutate(
      window = case_when(
        time_start < 30 ~ "m30_0",
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
        filter(concept_id != 0 & start_date <= cohort_start_date) %>% 
        mutate(
          time_start = !!datediff("start_date", "cohort_start_date")
        ) |>
        filter(time_start > 0) |>
        mutate(
          window = case_when(
            time_start < 30 ~ "m30_0",
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
    right_join(
      cdm$study_population |> 
        select(any_of(c(
          "cohort_definition_id", "cohort_name", "subject_id", "exposure",
          "pregnancy_id", "cohort_start_date", "cohort_end_date", "exposed_match_id", 
          "region", unlist(strata)
        ))),
      by = c("cohort_name", "subject_id", "cohort_start_date")
    ) |>
    mutate(value = 1, unique_id = paste0(as.character(subject_id), "_", as.character(exposed_match_id), "_", as.character(pregnancy_id))) |>
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
  features <- features[grepl("_minf_m366|_m30_0|_m365_31|_m180_m31", features) | features == "region"]
  if ("region" %in% colnames(cdm$features)) {
    cdm$features <- cdm$features |>
      mutate(region = as.character(region))
  }
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
          if (nrow(data.k) > 0) {
            # weights
            data.k <- getWeights(data.k, weights[[group]]) 
            
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
    filter(variable_name != "region") |>
    mutate(
      cdm_name = cdmName(cdm),
      additional_level = gsub("_minf_m366|_m30_0|_m365_31|_m180_m31", "", variable_name),
      additional_name = "concept_id",
      variable_level = gsub(".*(m30_0|minf_m366|m365_31|m180_m31)", "\\1", variable_name),
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
        filter(variable_name == "region") |>
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
      variables = c("follow-up time"),
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
          if (nrow(data.k) > 0) {
            # weights
            data.k <- getWeights(data.k, weights[[group]])
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
    inner_join(
      overall
      # by = c(
      #   "result_id", "cdm_name", "group_name", "group_level", "exposure", "vaccine_brand",
      #   "gestational_trimester", "age_group", "variable_name", "variable_level", "estimate_name",
      #   "estimate_type", "additional_name", "additional_level", "analysis"
      # )
    ) |>
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
getSurvivalData <- function(data, outcome, group, strata, start = "cohort_start_date", end = "cohort_end_date", weights = NULL) {
  data |>
    dplyr::rename("start_date" := !!start, "end_date" := !!end) %>% 
    mutate(
      # status 1 if outcome in window
      status = if_else(.data[[outcome]] >= start_date & .data[[outcome]] <= .data$end_date, 1, 0),
      status = if_else(is.na(status), 0, status),
      time = if_else(status == 1, !!datediff("start_date", outcome), !!datediff("start_date", "end_date"))
    ) |>
    select(any_of(unique(c(
      "cohort_name", "subject_id", "exposed_match_id", "pregnancy_id", "exposure",
      unlist(group), unlist(strata), "weight", "time", "status"
    )))) |>
    compute()
}

getIRR <- function(x) {
  x <- x |>
    group_by(exposure) |>
    summarise(person_days = sum(time*.data$weight), cases = sum(status*.data$weight))
  res <- (x$cases[x$exposure == "exposed"]/x$person_days[x$exposure == "exposed"])/
    (x$cases[x$exposure == "comparator"]/x$person_days[x$exposure == "comparator"])
}

getWeights <- function(x, coefs) {
  
  pred <- c(
    "exposure", "age", "gestational_day", "cohort_start_date", 
    "previous_observation", "previous_pregnancies", "previous_healthcare_visits", 
    "alcohol_misuse_dependence", "obesity", "anxiety", "depression", coefs
  )
  psData <- x |>
    mutate(unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
    mutate(exposure = factor(exposure, levels = c("comparator", "exposed")))
  
  columns <- sapply(lapply(psData, unique), length)
  columns <- names(columns)[columns <= 1]
  
  glmResult <- glm(exposure ~ ., data = psData |> select(any_of(pred)) |> select(!any_of(columns)), family = binomial(link = "logit"))
  
  psData |>
    select(!any_of(c("ps", "weight"))) |>
    bind_cols(
      predict.glm(glmResult, newdata = psData |> select(any_of(pred)) |> select(!any_of(columns)), type = "response") |>
        as_tibble() |>
        rename("ps" = "value")
    ) |>
    filter(!is.na(ps)) |>
    mutate(weight = if_else(exposure == "exposed", 1-ps, ps))
}

processGroupStrata <- function(data, group, weights) {
  if (nrow(data) > 10 & sum(data$status) > 5) {
    set.seed(123)
    
    coefBootstrap <- NULL
    for (ii in 1:500) {
      data.ii <- data |> dplyr::sample_n(size = nrow(data), replace = TRUE)
      if (length(weights) != 0) {
        data.ii <- getWeights(data.ii, weights[[group]])
      }
      coefBootstrap <- c(coefBootstrap, getIRR(data.ii))
    }
    
    # calculate main estimate
    if (length(weights) != 0) {
      data <- getWeights(data, weights[[group]])
    }
    res <- getIRR(data)
    
    # risk estimate tibble
    resultsRisk <- tibble(
      lower_ci = quantile(coefBootstrap, 0.025, na.rm = TRUE),
      upper_ci = quantile(coefBootstrap, 0.975, na.rm = TRUE),
      coef = res
    ) |>
      regressionToSummarised(cols = c("coef", "lower_ci", "upper_ci")) |>
      mutate(
        estimate_type = "numeric",
        variable_name = "Risk estimate",
        variable_level = NA_character_
      )
    
    # follow-up stats tibble
    resultsStats <- data |>
      group_by(exposure) |>
      summarise(
        median = median(time * .data$weight),
        q25 = quantile(time * .data$weight, 0.25),
        q75 = quantile(time * .data$weight, 0.75),
        min = min(time * .data$weight),
        max = max(time * .data$weight),
        subject_count = sum(.data$weight),
        outcome_count = sum(.data$status * .data$weight),
        .groups = "drop"
      ) |>
      mutate(estimate_type = "numeric") |>
      rename("variable_level" = "exposure") |>
      regressionToSummarised(
        cols = c("median", "q25", "q75", "min", "max", "subject_count", "outcome_count")
      ) |>
      mutate(
        variable_name = case_when(
          estimate_name == "subject_count" ~ "Number persons",
          estimate_name == "outcome_count" ~ "Number events",
          .default = "Person-Days"
        ),
        estimate_name = if_else(grepl("_count", estimate_name), "count", estimate_name)
      )
    
    # combine and return
    bind_rows(resultsRisk, resultsStats)
  } else {
    tibble(
      variable_level = character(),
      estimate_type = character(),
      estimate_name = character(),
      estimate_value = character(),
      variable_name = character()
    )
  }
}

getRiskEstimate <- function(data, group, strata, weights = NULL) {
  
  if (sum(data |> pull(status)) >= 5) {
    
    # prep data
    strata <- unlist(strata)
    strata <- c(strata[strata != "exposure"], "overall")
    if (is.null(weights)) {
      data <- data |> mutate(weight = 1)
    }
    
    # nest the data by group and strata
    nestedData <- data |>
      mutate(overall = "overall") |>
      tidyr::pivot_longer(
        cols = all_of(strata),
        names_to = "strata_name",
        values_to = "strata_level"
      ) |>
      group_by(
        group_name = "cohort_name",
        group_level = .data[[group]],
        strata_name,
        strata_level
      ) |>
      collect() |> 
      nest() 
    
    # get risk estimiate within each nest
    results <- nestedData |>
      mutate(
        results = future_map(
          .x = data,
          .y = group_level,
          .f = ~ processGroupStrata(data = .x, group = .y, weights = weights)
        )) |>
      select(-data) |>
      tidyr::unnest(results)
    
  } else {
    results <- tibble(
      variable_level = character(),
      estimate_type = character(),
      estimate_name = character(),
      estimate_value = character(),
      variable_name = character()
    )
  }
  
  return(results)
}

regressionToSummarised <- function(
    x, 
    cols = c("coef", "lower_ci", "upper_ci"),
    estimate = "numeric") {
  x |>
    pivot_longer(
      cols = all_of(cols),
      names_to = "estimate_name", values_to = "estimate_value"
    ) |>
    mutate(estimate_value = as.character(estimate_value), estimate_type = estimate)
}

estimateSurvivalRisk <- function(cohort, outcomes, outcomeGroup, end, strata, group, weights = NULL) {
  cdm <- omopgenerics::cdmReference(cohort)
  studyAnalysis <- switch (end,
                           "cohort_end_date" = "main",
                           "cohort_end_date_sensitivity" = "sensitivity",
                           "end_42_days_or_pregnancy" = "main",
                           "end_42_days" = "secondary",
                           "end_42_days_or_pregnancy_sensitivity" = "sensitvity",
                           "end_42_days" = "secondary_sensitivity",
                           "week_19_end" = "main",
                           "week_19_end_sensitivity" = "sensitvity",
                           "pregnancy_end" = "main",
                           "pregnancy_end_sensitivity" = "sensitvity",
                           "postpartum_6_weeks" = "main",
                           "postpartum_6_weeks_sensitivity" = "sensitivity",
                           "postpartum_12_weeks" = "main",
                           "postpartum_12_weeks_sensitivity" = "sensitivity"
  )
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  results <- list()
  kk <- 1
  
  # check if parallelization is possible
  cores <- parallel::detectCores(logical = FALSE)
  os_type <- .Platform$OS.type
  future::plan(future::multisession, workers = cores)
  
  for (outcome in outcomes) {
    survival_data <- getSurvivalData(cohort, outcome, end = end, strata = strata, group = group, weights = weights)
    results[[kk]] <- getRiskEstimate(survival_data, group = group, strata = strata, weights = weights) |>
      mutate(outcome_name = outcome, follow_up_end = end) |>
      omopgenerics::uniteAdditional(cols = c("outcome_name", "follow_up_end"))
    kk <- kk + 1
  }
  future::plan(future::sequential)
  results |> 
    bind_rows() |> 
    mutate(result_id = 1L, cdm_name = cdmName(cdm)) |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        result_type = "incidence_rate_ratio",
        package_name = "study_code",
        package_version = "v0.0.1",
        weighting = weighting,
        outcome_group = outcomeGroup,
        study_analysis = studyAnalysis
      )
    )
}

suppressRiskEstimates <- function(result) {
  set <- settings(result)
  result <- result |>
    group_by(result_id, cdm_name, group_name, group_level, strata_name, strata_level, additional_name, additional_level) |>
    mutate(
      sup_group = if_else(any(estimate_name == "count" & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5), TRUE, FALSE),
      sup_row = if_else(estimate_name == "count" & as.numeric(estimate_value) > 0 & as.numeric(estimate_value) < 5, TRUE, FALSE),
      sup_estimate = if_else(sum(variable_name == "Number events" & estimate_value == "0") == 2, TRUE, FALSE)
    ) |>
    ungroup() |>
    mutate(
      estimate_value = case_when(
        estimate_name != "count" & sup_group ~ "-",
        estimate_name == "count" & sup_row ~ "-",
        estimate_name %in% c("coef", "lower_ci", "upper_ci") & sup_estimate ~ NA_character_,
        .default = estimate_value
      )
    ) |>
    select(!c("sup_group", "sup_row", "sup_estimate"))
  
  result <- result |>
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
    mutate(
      overall = "overall",
      calendar_week = lubridate::floor_date(cohort_start_date, unit = "week"),
      gestational_week = as.numeric(.data$gestational_day)
    )  |>
    collect() |>
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
        if (nrow(data.k) > 0) {
          # weights
          if (length(weights) != 0) {
            data.k <- getWeights(data.k, weights[[group]])
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

getRegion <- function(x, database_name) {
  if (database_name == "CPRD GOLD") {
    x |>
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
    x |>
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
    x |>
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
    )
}

getIncidence <- function(cdm, targetCohortDenominator, outcomeTable, outcomeCohortId, washOut) {
  # overall
  overallIds <- settings(cdm$overall_period) |>
    filter(cohort_name %in% targetCohortDenominator) |>
    pull(cohort_definition_id)
  overall <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "overall_period",
    outcomeTable = outcomeTable,
    censorTable = NULL,
    denominatorCohortId = overallIds,
    outcomeCohortId = outcomeCohortId,
    censorCohortId = NULL,
    interval = c("overall", "years"),
    completeDatabaseIntervals = FALSE,
    outcomeWashout = washOut,
    repeatedEvents = TRUE,
    strata = list("maternal_age"),
    includeOverallStrata = TRUE
  )
  overall <- omopgenerics::newSummarisedResult(
    overall, settings = omopgenerics::settings(overall) |> dplyr::mutate(denominator_table_name = "overall")
  )
  # pre-covid
  preCovidIds <- settings(cdm$pre_covid_period) |>
    filter(cohort_name %in% targetCohortDenominator) |>
    pull(cohort_definition_id)
  preCovid <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "pre_covid_period",
    outcomeTable = outcomeTable,
    censorTable = NULL,
    denominatorCohortId = preCovidIds,
    outcomeCohortId = outcomeCohortId,
    censorCohortId = NULL,
    interval = c("overall"),
    completeDatabaseIntervals = FALSE,
    outcomeWashout = washOut,
    repeatedEvents = TRUE,
    strata = list("maternal_age"),
    includeOverallStrata = TRUE
  )
  preCovid <- omopgenerics::newSummarisedResult(
    preCovid, settings = omopgenerics::settings(preCovid) |> dplyr::mutate(denominator_table_name = "pre_covid")
  )
  # covid
  covidIds <- settings(cdm$main_covid_period) |>
    filter(cohort_name %in% targetCohortDenominator) |>
    pull(cohort_definition_id)
  covid <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "main_covid_period",
    outcomeTable = outcomeTable,
    censorTable = NULL,
    denominatorCohortId = covidIds,
    outcomeCohortId = outcomeCohortId,
    censorCohortId = NULL,
    interval = c("overall"),
    completeDatabaseIntervals = FALSE,
    outcomeWashout = washOut,
    repeatedEvents = TRUE,
    strata = list("maternal_age"),
    includeOverallStrata = TRUE
  )
  covid <- omopgenerics::newSummarisedResult(
    covid, settings = omopgenerics::settings(covid) |> dplyr::mutate(denominator_table_name = "covid")
  )
  # post-covid
  postCovidIds <- settings(cdm$post_main_covid_period) |>
    filter(cohort_name %in% targetCohortDenominator) |>
    pull(cohort_definition_id)
  postCovid <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "post_main_covid_period",
    outcomeTable = outcomeTable,
    censorTable = NULL,
    denominatorCohortId = postCovidIds,
    outcomeCohortId = outcomeCohortId,
    censorCohortId = NULL,
    interval = c("overall"),
    completeDatabaseIntervals = FALSE,
    outcomeWashout = washOut,
    repeatedEvents = TRUE,
    strata = list("maternal_age"),
    includeOverallStrata = TRUE
  )
  postCovid <- omopgenerics::newSummarisedResult(
    postCovid, settings = omopgenerics::settings(postCovid) |> dplyr::mutate(denominator_table_name = "post_covid")
  )
  # return results
  bind(overall, preCovid, covid, postCovid)
}

asIncidencePrevalence <- function(cohort, startDate, endDate) {
  cohort |>
    newCohortTable(
      settings(cohort) |>
        mutate(
          start_date = startDate,
          end_date = endDate
        ) |>
        rename("days_prior_observation" = "min_prior_observation") |>
        select(!c("min_future_observation", "sex", "age_range"))
    )
}

addSeason <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  cohort |>
    dplyr::mutate(
      season_yearly = dplyr::case_when(
        clock::get_month(.data$cohort_start_date) %in% 3:5 ~ paste0("Spring ", clock::get_year(.data$cohort_start_date)),
        clock::get_month(.data$cohort_start_date) %in% 6:8 ~ paste0("Summer ", clock::get_year(.data$cohort_start_date)),
        clock::get_month(.data$cohort_start_date) %in% 9:11 ~ paste0("Autumn ", clock::get_year(.data$cohort_start_date)),
        clock::get_month(.data$cohort_start_date) %in% c(12, 1:2) ~ paste0("Winter ", clock::get_year(.data$cohort_start_date))
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
  cohort |>
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
}

addSocioeconomicStatus <- function(cohort, database) {
  name <- omopgenerics::tableName(cohort)
  if (database == "CPRD GOLD") {
    cohort <- cohort |>
      dplyr::left_join(
        cdm$measurement |>
          dplyr::filter(measurement_concept_id == 715996) |>
          dplyr::select("subject_id" = "person_id", "socioeconomic_status" = "value_as_number"),
        by = "subject_id"
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  return(cohort)
}

getBRCharacteristics <- function(cohort) {
  # Variables to add: socioeconomic status, ethnicity, season pregnancy start
  # Pregnancy: previous pregnancy
  # Comorbidities: alchohol, obesity, diabetes, hypertension, asthma, depression/anxiety, epilepsy
  # Medications: omeprazole/antiacids, diabetes treatment/s, nsaids, opioids, antidepressants, antiepilepsy, corticosteroids
  
  estimates = list(
    'season' = c('count', 'percentage'), 
    'season_yearly' = c('count', 'percentage'), 
    'ethnicity' = c('count', 'percentage'),
    'socioeconomic_status' = c('count', 'percentage'),
    'maternal_age' = c('count', 'percentage'),
    'trimester' = c('count', 'percentage')
  )
  estimates <- estimates[names(estimates) %in% colnames(cohort)]
  otherVariables = names(estimates)
  cohort |> 
    summariseCharacteristics(
      counts = TRUE,
      demographics = TRUE,
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
