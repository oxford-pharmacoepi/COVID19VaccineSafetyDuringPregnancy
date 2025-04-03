getId <- function(cohort, name) {
  settings(cohort) |> 
    filter(cohort_name %in% name) |>
    pull(cohort_definition_id)
}

getPregnantCohorts <- function(db, cdm, mother_table_schema, mother_table_name) {
  cdm$mother_table_original <- tbl(
    db, inSchema(schema = mother_table_schema, table = mother_table_name)
  ) %>% 
    {if (grepl("CPRD", database_name)) {
      rename(., "pregnancy_outcome_id" = "original_outcome")
    } else . } |>
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
      cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "mother_table"), 
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
      pregnancy_end_date <= observation_period_end_date,
    ) |>
    recordCohortAttrition(reason = "Pregnancy in observation") |>
    filter(pregnancy_start_date < pregnancy_end_date) |>
    mutate(cohort_definition_id = 1) |>
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
        pregnancy_outcome_id == 4092289  & gestational_length <= 37*7~ "preterm_labour",
        pregnancy_outcome_id == 4092289 ~ "livebirth",
        pregnancy_outcome_id == 4067106 ~ "miscarriage",
        pregnancy_outcome_id == 443213 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome_id == 443213 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome_id == 4081422 ~ "elective_termination",
        pregnancy_outcome_id == 4095714 ~ "discordant",
        .default = "unknown"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records") 
  
  return(cdm$mother_table)
}

getSourcePopulation <- function(mother, objective, enrollment) {
  name <- paste0("source_", objective)
  cdm[[name]] <- mother |>
    mutate(cohort_definition_id = objective) |>
    # pregnancy end after enrollment start
    filter(.data$cohort_end_date > !!enrollment[1]) |>
    compute(name = name, temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = objective, cohort_name = name
      ),
      cohortAttritionRef = attrition(mother) |>
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
    ) |>
    # previous doses (any type)
    requireCohortIntersect(
      targetCohortTable = "covid_vaccines",
      window = list(c(-Inf, -1)),
      intersections = objective-1,
      targetCohortId = "any_covid_vaccine",
      indexDate = "cohort_start_date",
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL
    )
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
      cohort_start_date <= as.Date(clock::add_days("pregnancy_start_date", 12*7))
    ) |>
    select(!c("pregnancy_start_date", "pregnancy_end_date", "days")) |>
    distinct() 
}

samplingSummary <- function(sampling_source, reason, results = NULL) {
  omopgenerics::bind(
    results,
    sampling_source |>
      group_by(cohort_name, exposed_id) |> 
      tally() |>
      summariseResult(
        group = list("cohort_name"), variables = "n", counts = FALSE, 
        estimates = c("min", "max", "median", "q25", "q75")
      ) |>
      mutate(variable_name = "exposed:comparator", variable_level = reason),
    sampling_source |> 
      mutate(
        subject_id = exposed_id, cohort_start_date = exposure_date, cohort_end_date = exposure_date
      ) |>
      distinct(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, cohort_name) |>
      summariseResult(group = list("cohort_name")) |>
      filter(variable_name == "number records") |>
      mutate(variable_name = "Number exposed", variable_level = reason),
    sampling_source |> 
      distinct(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, cohort_name) |>
      summariseResult(group = list("cohort_name")) |>
      filter(variable_name == "number records") |>
      mutate(variable_name = "Number comparator", variable_level = reason)
  )
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
      .data$cohort_end_date_0123456789
    ) |>
    dplyr::filter(.data$new_date_0123456789 == !!atDateFunction) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(!dplyr::all_of(reason))) |>
    dplyr::arrange(.data[[reason]]) |>
    dplyr::summarise(!!reason := stringr::str_flatten(.data[[reason]], collapse = '; '),
                     .groups = "drop") |>
    dplyr::mutate(!!newDate := .data$new_date_0123456789, !!keptDate := .data[[paste0(keptDate, "_0123456789")]]) |>
    dplyr::select(
      !c(
        "new_date_0123456789",
        "cohort_end_date_0123456789",
        "cohort_start_date_0123456789"
      )
    ) |>
    dplyr::distinct() |>
    dplyr::compute(name = tmpName, temporary = FALSE)
  
  # checks with informative errors
  # CohortConstructor:::validateNewCohort(newCohort, cdm, tmpPrefix)
  
  newCohort <- newCohort |>
    dplyr::relocate(dplyr::all_of(omopgenerics::cohortColumns("cohort")))
  
  if (keepDates) {
    newCohort <- newCohort |>
      dplyr::inner_join(
        cohort |> 
          dplyr::select(any_of(c("cohort_definition_id", "subject_id", "cohort_start_date", dateColumns)))
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
    'days_first_vaccine' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"), 
    'days_second_vaccine' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"), 
    'previous_observation' = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"), 
    "previous_healthcare_visits" = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"),
    "previous_pregnancies" = c('min', 'q25', 'median', 'q75', 'max', "mean", "sd"), 
    'age_group' = c('count', 'percentage'), 
    'gestational_trimester' = c('count', 'percentage'), 
    'vaccine_brand' = c('count', 'percentage'), 
    'smoking_status' = c('count', 'percentage'),
    "alcohol_misuse_dependence" = c('count', 'percentage'), 
    "obesity" = c('count', 'percentage'),
    "substance_misuse_dependence" = c('count', 'percentage')
  )
  estimates <- estimates[names(estimates) %in% colnames(cdm$study_population)]
  otherVariables = c(
    "age", "age_group", "gestational_trimester", "vaccine_brand", "days_first_vaccine", 
    "days_second_vaccine", "previous_observation", "smoking_status",
    "alcohol_misuse_dependence", "obesity", "substance_misuse_dependence",
    "previous_healthcare_visits", "previous_pregnancies"
  )
  otherVariables <- otherVariables[otherVariables %in% colnames(cdm$study_population)]
  baseline <- cdm$study_population |> 
    summariseCharacteristics(
      strata = strata,
      counts = TRUE,
      demographics = FALSE,
      cohortIntersectFlag = list(
        # covariatesInf (-Inf, 0)
        "History of comorbidities" = list(
          targetCohortTable = "covariates_inf", window = c(-Inf, 0)
        ),
        # covariates1 (-365, 0)
        "Mental heatlh problems in the last year" = list(
          targetCohortTable = "covariates_1", window = c(-365, 0)
        ),
        # influenza and tdap (-5, 5)
        "Other vaccines within 5 days" = list(
          targetCohortTable = "other_vaccines", window = c(-5, 5)
        )
      ),
      cohortIntersectCount = list(
        # covid infections (-Inf, 0)
        "Previous COVID-19 infections" = list(
          targetCohortTable = "covid", window = c(-Inf, 0)
        ),
        # covid test (-Inf, 0)
        "Previous COVID-19 tests" = list(
          targetCohortTable = "covid_test", window = c(-Inf, 0)
        )
      ),
      otherVariables = otherVariables,
      estimates = estimates,
      weights = weights
    )
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  baseline |>
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
        # addCohortName() |>
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
        time_start < 366 ~ "m365_m31",
        time_start > 365 ~ "minf_m366",
        .default = "none"
      ),
      feature = paste0(concept_id, "_", window) 
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
            # addCohortName() |>
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
          feature = paste0(concept_id, "_", window) 
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
        # addCohortName() |>
        select(any_of(c(
          "cohort_definition_id", "cohort_name", "subject_id", "exposure",
          "pregnancy_id", "cohort_start_date", "cohort_end_date", "exposed_match_id", 
          "care_site_id", unlist(strata)
        ))),
      by = c("cohort_name", "subject_id", "cohort_start_date")
    ) |>
    mutate(value = 1, unique_id = paste0(subject_id, "_", exposed_match_id, "_", pregnancy_id)) |>
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
  features <- features[grepl("_minf_m366|_m30_0|_m365_31|_m180_m31", features) | features == "care_site_id"]
  summarisedResult <- cdm$features |>
    mutate(care_site_id = as.character(care_site_id)) |>
    summariseResult(
      group = list("cohort_name"),
      includeOverallGroup = FALSE,
      strata = strata,
      includeOverallStrata = FALSE,
      variables = features,
      estimates = c("count", "percentage"),
      counts = FALSE,
      weights = weights
    )
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  lsc <- summarisedResult |>
    filter(variable_name != "care_site_id") |>
    mutate(
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
        filter(variable_name == "care_site_id") |>
        mutate(
          additional_level = variable_level,
          additional_name = "concept_id",
          variable_level = "-inf to inf"
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

selectStrata <- function(cdm, strata = c("vaccine_brand", "gestational_trimester", "vaccine_valency")) {
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
          strata = strata |> paste0(collapse = " &&& "),
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
          strata = strata |> paste0(collapse = " &&& "),
          min_cell_count = "5",
          result_type = "summarise_standardised_mean_differences",
          package_name = "study_code",
          package_version = "v0.0.1"
        )
    )
}

summariseCohortExit <- function(cdm, strata, weights) {
  strataCens <- list()
  for (st in strata) {
    strataCens <- c(strataCens, list(c(st, "exit_reason")))
  }
  main <- cdm$study_population %>%  
    mutate(
      "follow-up" = !!datediff("cohort_start_date", "cohort_end_date")
    ) |>
    summariseResult(
      group = "cohort_name", 
      strata = strataCens,
      variables = c("follow-up"),
      weights = weights
    ) |>
    mutate(additional_name = "analysis", additional_level = "main")
  sensitvity <- cdm$study_population %>%  
    mutate(
      "follow-up" = !!datediff("cohort_start_date", "cohort_end_date_sensitivity"),
      "exit_reason" = exit_reason_sensitivty
    ) |>
    summariseResult(
      group = "cohort_name", 
      strata = strataCens,
      variables = c("follow-up"),
      weights = weights
    ) |>
    mutate(additional_name = "analysis", additional_level = "sensitivity")
  
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  
  bind(main, sensitvity) |>
    newSummarisedResult(
      settings = settings(main) |> 
        mutate(additional = "analysis", result_type = "cohort_exit", weighting = weighting) |>
        select(!any_of("weights"))
    ) 
}

getSurvivalData <- function(data, outcome, group, strata, start = "cohort_start_date", end = "cohort_end_date", weights = NULL) {
  data |>
    rename("start_date" := !!start, "end_date" := !!end) %>% 
    mutate(
      # status 1 if outcome in window
      status = if_else(.data[[outcome]] > start_date & .data[[outcome]] <= .data$end_date, 1, 0),
      status = if_else(is.na(status), 0, status),
      time = if_else(status == 1, !!datediff("start_date", outcome)-1, !!datediff("start_date", "end_date")-1),
    ) |>
    select(any_of(unique(c(
      "cohort_name", "subject_id", "exposed_match_id", "pregnancy_id", "exposure",
      unlist(group), unlist(strata), weights, "time", "status"
    )))) |>
    compute()
}

getRiskEstimate <- function(data, group, strata, weights = NULL) {
  # for survival formatted data - 1 outcome
  results <- list()
  groupLevel = unique(data |> pull(.data[[group]]))
  k <- 1
  if (is.null(weights)) {
    data <- data |> mutate(weight = 1)
    weights <- "weight"
  }
  strata <- unlist(strata)
  strata <- c(strata[strata != "exposure"], "overall")
  data <- data |> mutate(overall = "overall")
  for (group.k in groupLevel) { # group level
    for (strata.k in strata) { # strata name
      strataLevels <- unique(data |> pull(strata.k))
      for(strataLevel.k in strataLevels) { # strata level
        # data
        data.k <- data |>
          filter(cohort_name == group.k, .data[[strata.k]] == strataLevel.k) |>
          collect() 
        # boostrap rate ratio
        exp_coefs <- NULL
        sampleSize <- round(nrow(data.k)*0.9)
        for (ii in 1:500) {
          data.ii <- data.k |>
            slice_sample(n = sampleSize) |>
            group_by(exposure) |>
            summarise(person_days = sum(time*.data[[weights]]), cases = sum(status*.data[[weights]]))
          res <- exp(
            (data.ii$cases[data.ii$exposure == "exposed"]/data.ii$person_days[data.ii$exposure == "exposed"])/
              (data.ii$cases[data.ii$exposure == "comparator"]/data.ii$person_days[data.ii$exposure == "comparator"])
          )
          exp_coefs <- c(exp_coefs, res)
        }
        
        data.all <- data.k |>
          group_by(exposure) |>
          summarise(person_days = sum(time*.data[[weights]]), cases = sum(status*.data[[weights]]))
        res <- exp(
          (data.all$cases[data.all$exposure == "exposed"]/data.all$person_days[data.all$exposure == "exposed"])/
            (data.all$cases[data.all$exposure == "comparator"]/data.all$person_days[data.all$exposure == "comparator"])
        )
        
        results[[k]] <- tibble(
          exp_coef = res,
          lower_ci = quantile(exp_coefs, 0.025, na.rm = TRUE),
          upper_ci = quantile(exp_coefs, 0.975, na.rm = TRUE)
        ) |>
          regressionToSummarised(
            cols = c("exp_coef", "lower_ci", "upper_ci")
          ) |>
          mutate(
            group_name = "cohort_name",
            group_level = group.k,
            strata_name = strata.k, 
            strata_level = strataLevel.k,
            estimate_type = "numeric",
            variable_name = "Risk estimate",
            variable_level = NA
          ) 
        k <- k + 1
        
        # follow-up stats 
        results[[k]] <- data.k |>
          group_by(exposure) |>
          summarise(
            median = median(time*.data[[weights]]),
            q25 = quantile(time*.data[[weights]], 0.25),
            q75 = quantile(time*.data[[weights]], 0.75),
            min = min(time*.data[[weights]]),
            max = max(time*.data[[weights]]),
            subject_count = n(),
            outcome_count = sum(.data$status == 1)
          ) |>
          mutate(
            group_name = "cohort_name",
            group_level = group.k,
            strata_name = strata.k, 
            strata_level = strataLevel.k,
            estimate_type = "numeric"
          ) |>
          rename("variable_level" = "exposure") |>
          regressionToSummarised(
            cols = c("median", "q25", "q75","min", "max", "subject_count", "outcome_count")
          ) |>
          mutate(
            variable_name = case_when(
              estimate_name == "subject_count" ~ "Number persons",
              estimate_name == "outcome_count" ~ "Number events",
              .default = "Person-Time"
            ),
            estimate_name = if_else(grepl("_count", estimate_name), "count", estimate_name)
          ) 
        k <- k + 1
      }
    }
  }
  return(results |> bind_rows())
}

regressionToSummarised <- function(
    x, 
    cols = c("coef", "se_coef", "exp_coef", "z", "p", "lower_ci", "upper_ci"),
    estimate = "numeric") {
  x |>
    pivot_longer(
      cols = all_of(cols),
      names_to = "estimate_name", values_to = "estimate_value"
    ) |>
    mutate(estimate_value = as.character(estimate_value), estimate_type = estimate)
}

estimateSurvivalRisk <- function(cohort, outcomes, end, strata, group, weights = NULL) {
  cdm <- cdmReference(cohort)
  study <- "main"
  if (end != "cohort_end_date") study <- "sensitivity"
  weighting <- "FALSE"
  if (!is.null(weights)) weighting <- "TRUE"
  results <- list()
  kk <- 1
  for (outcome in outcomes) {
    survival_data <- getSurvivalData(cohort, outcome, end = end, strata = strata, group = group)
    results[[kk]] <- getRiskEstimate(survival_data, group = group, strata = strata, weights = NULL) |>
      mutate(additional_name = "outcome_name", additional_level = outcome)
    kk <- kk + 1
  }
  results |> 
    bind_rows() |> 
    mutate(result_id = 1L, cdm_name = cdmName(cdm)) |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        result_type = "incidence_rate_ratio",
        package_name = "study_code",
        package_version = "v0.0.1",
        study = study,
        weighting = weighting
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
        estimate_name %in% c("exp_coef", "lower_ci", "upper_ci") & sup_estimate ~ NA_character_,
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

summariseTimeDistribution <- function(cdm, strata, weights = NULL) {
  if (is.null(weights)) {
    weighting <- "FALSE"
    weights <- "weight"
    tab <- cdm$study_population |>
      mutate(weight = 1) 
  } else {
    weighting <- "TRUE"
    tab <- cdm$study_population
  }
  results <- NULL
  # gestational day
  for (ii in 1:length(strata)) {
    strataLevel = strata[[ii]]
    results <- results |>
      union_all(
        tab |>
          collect() |>
          group_by_at(c("cohort_name", strataLevel, "gestational_day")) |>
          tally(name = "estimate_value", wt = .data$weight) |>
          ungroup() |>
          uniteGroup("cohort_name") |>
          uniteStrata(strataLevel) |>
          uniteAdditional() |>
          mutate(
            result_id = 1L,
            cdm_name = cdmName(cdm),
            variable_name = "gestational_day",
            variable_level = as.character(gestational_day),
            estimate_name = "count",
            estimate_type = "numeric"
          ) |>
          select(omopgenerics::resultColumns())
      )
  }
  # index date
  tab <- tab |>
    mutate(index_week = lubridate::floor_date(cohort_start_date, unit = "week")) 
  for (ii in 1:length(strata)) {
    strataLevel = strata[[ii]]
    results <- results |>
      union_all(
        tab |>
          collect() |>
          group_by_at(c("cohort_name", strataLevel, "index_week")) |>
          tally(name = "estimate_value", wt = .data$weight) |>
          ungroup() |>
          uniteGroup("cohort_name") |>
          uniteStrata(strataLevel) |>
          uniteAdditional() |>
          mutate(
            result_id = 1L,
            cdm_name = cdmName(cdm),
            variable_name = "index_week",
            variable_level = as.character(index_week),
            estimate_name = "count",
            estimate_type = "numeric"
          ) |>
          select(omopgenerics::resultColumns())
      )
  }
  results <- results |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        result_type = "time_distributions",
        package_name = "study_code",
        package_version = "v0.0.1",
        weighting = weighting
      )
  )
  return(results)
}
