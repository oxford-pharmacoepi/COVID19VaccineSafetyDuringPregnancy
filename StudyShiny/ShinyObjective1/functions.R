backgroundCard <- function(fileName) {
  # read file
  content <- readLines(fileName)
  
  # extract yaml metadata
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]
  
  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }
  
  tmpFile <- tempfile(fileext = ".md")
  writeLines(text = content, con = tmpFile)
  
  # metadata referring to keys
  backgroundKeywords <- list(
    header = "bslib::card_header",
    footer = "bslib::card_footer"
  )
  keys <- names(backgroundKeywords) |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (x %in% names(metadata)) {
        paste0(backgroundKeywords[[x]], "(metadata[[x]])") |>
          rlang::parse_expr() |>
          rlang::eval_tidy()
      } else {
        NULL
      }
    }) |>
    purrr::compact()
  
  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # content
    list(
      keys$header,
      bslib::card_body(shiny::HTML(markdown::markdownToHTML(
        file = tmpFile, fragment.only = TRUE
      ))),
      keys$footer
    ) |>
      purrr::compact()
  )
  
  unlink(tmpFile)
  
  do.call(bslib::card, arguments)
}
summaryCdmName <- function(data) {
  if (length(data) == 0) {
    return(list("<b>CDM names</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(.data$cdm_name) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop")
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$cdm_name) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = paste0(.data$cdm_name, " (", .data$number_rows, ")")) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  list("<b>CDM names</b>" = x)
}
summaryPackages <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Packages versions</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(
          settingsColumn = c("package_name", "package_version")
        ) |>
        dplyr::group_by(.data$package_name, .data$package_version) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select(c("package_name", "package_version")) |>
            dplyr::distinct(),
          by = c("package_name", "package_version")
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$package_name) |>
    dplyr::group_split() |>
    as.list()
  lab <- "<b>"
  names(x) <- x |>
    purrr::map_chr(\(x) {
      if (nrow(x) > 1) {
        lab <<- "<b style='color:red'>"
        paste0("<b style='color:red'>", unique(x$package_name), " (Multiple versions!) </b>")
      } else {
        paste0(
          x$package_name, " (version = ", x$package_version,
          "; number records = ", x$number_rows,")"
        )
      }
    })
  x <- x |>
    purrr::map(\(x) {
      if (nrow(x) > 1) {
        paste0(
          "version = ", x$package_version, "; number records = ",
          x$number_rows
        ) |>
          rlang::set_names() |>
          as.list()
      } else {
        x$package_name
      }
    })
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Packages versions</b>"))
}
summaryMinCellCount <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Min Cell Count Suppression</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(settingsColumn = "min_cell_count") |>
        dplyr::group_by(.data$min_cell_count) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select("min_cell_count") |>
            dplyr::distinct(),
          by = "min_cell_count"
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(min_cell_count = as.integer(.data$min_cell_count)) |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$min_cell_count == 0L,
        "<b style='color:red'>Not censored</b>",
        paste0("Min cell count = ", .data$min_cell_count)
      ),
      label = paste0(.data$label, " (", .data$number_rows, ")")
    ) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  lab <- ifelse(any(grepl("Not censored", unlist(x))), "<b style='color:red'>", "<b>")
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Min Cell Count Suppression</b>"))
}
summaryPanels <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Panels</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        res <- omopgenerics::settings(x) |>
          dplyr::select(!c(
            "result_id", "package_name", "package_version", "group", "strata",
            "additional", "min_cell_count"
          )) |>
          dplyr::relocate("result_type") |>
          as.list() |>
          purrr::map(\(x) sort(unique(x)))
      } else {
        sets <- c("result_type", omopgenerics::settingsColumns(x))
        res <- x |>
          omopgenerics::addSettings(settingsColumn = sets) |>
          dplyr::relocate(dplyr::all_of(sets)) |>
          omopgenerics::splitAll() |>
          dplyr::select(!c(
            "variable_name", "variable_level", "estimate_name",
            "estimate_type", "estimate_value", "result_id"
          )) |>
          as.list() |>
          purrr::map(\(values) {
            values <- as.list(table(values))
            paste0(names(values), " (number rows = ", values, ")") |>
              rlang::set_names() |>
              as.list()
          })
      }
      res
    })
  list(x) |>
    rlang::set_names(nm = "<b>Panels</b>")
}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()
  
  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }
  
  result <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    dplyr::select(-"result_id")
  
  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
tidyDT <- function(x,
                   columns,
                   pivotEstimates) {
  groupColumns <- omopgenerics::groupColumns(x)
  strataColumns <- omopgenerics::strataColumns(x)
  additionalColumns <- omopgenerics::additionalColumns(x)
  settingsColumns <- omopgenerics::settingsColumns(x)
  
  # split and add settings
  x <- x |>
    omopgenerics::splitAll() |>
    omopgenerics::addSettings()
  
  # remove density
  x <- x |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))
  
  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }
  
  # order columns
  cols <- list(
    "CDM name" = "cdm_name", "Group" = groupColumns, "Strata" = strataColumns,
    "Additional" = additionalColumns, "Settings" = settingsColumns,
    "Variable" = c("variable_name", "variable_level")
  ) |>
    purrr::map(\(x) x[x %in% columns]) |>
    purrr::compact()
  cols[["Estimates"]] <- estCols
  x <- x |>
    dplyr::select(dplyr::all_of(unname(unlist(cols))))
  
  # prepare the header
  container <- shiny::tags$table(
    class = "display",
    shiny::tags$thead(
      purrr::imap(cols, \(x, nm) shiny::tags$th(colspan = length(x), nm)) |>
        shiny::tags$tr(),
      shiny::tags$tr(purrr::map(unlist(cols), shiny::tags$th))
    )
  )
  
  # create DT table
  DT::datatable(
    data = x,
    filter = "top",
    container = container,
    rownames = FALSE,
    options = list(searching = FALSE)
  )
}
prepareResult <- function(result, resultList) {
  purrr::map(resultList, \(x) filterResult(result, x))
}
filterResult <- function(result, filt) {
  nms <- names(filt)
  for (nm in nms) {
    q <- paste0(".data$", nm, " %in% filt[[\"", nm, "\"]]") |>
      rlang::parse_exprs() |>
      rlang::eval_tidy()
    result <- omopgenerics::filterSettings(result, !!!q)
  }
  return(result)
}
getValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      res <- filterResult(result, x)
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id") |>
        as.list() |>
        purrr::map(\(x) sort(unique(x)))
      valuesSettings <- omopgenerics::settings(res) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) sort(unique(x[!is.na(x)]))) |>
        purrr::compact()
      values <- c(values, valuesSettings)
      names(values) <- paste0(nm, "_", names(values))
      values
    }) |>
    purrr::flatten()
}
getSelected <- function(choices) {
  purrr::imap(choices, \(vals, nm) {
    if (grepl("_denominator_sex$", nm)) {
      if ("Both" %in% vals) return("Both")
      return(vals[[1]])
    }
    
    if (grepl("_denominator_age_group$", nm)) {
      bounds <- regmatches(vals, regexec("^(\\d+) to (\\d+)$", vals))
      valid <- vapply(bounds, length, integer(1)) == 3
      if (any(valid)) {
        ranges <- vapply(bounds[valid], \(x) as.numeric(x[3]) - as.numeric(x[2]), numeric(1))
        return(vals[valid][[which.max(ranges)]])
      } else {
        return(vals[[1]])
      }
    }
    
    if (grepl("_outcome_cohort_name$", nm)) {
      return(vals[[1]])
    }
    
    vals
  })
}
renderInteractivePlot <- function(plt, interactive) {
  if (interactive) {
    plotly::renderPlotly(plt)
  } else {
    shiny::renderPlot(plt)
  }
}
reactiveSelectors <- function(data, prefix, columns, restrictions, input,
                              multiple = TRUE, default = list()) {
  if (length(restrictions) != length(columns)) {
    if (length(restrictions) == 1) {
      restrictions <- rep(restrictions, length(columns))
    } else {
      cli::cli_abort("Revise columns and restrictions arguments.")
    }
  }
  
  names(columns) <- restrictions
  
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col, dict = columns, input = input)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col, dict, input) {
    filterCol <- names(dict)[dict == col]
    return(sort(unique(data[[col]][data[[filterCol]] %in% input[[paste0(prefix, "_", filterCol)]]])))
  }
  renderUI({
    purrr::map(columns, ~ pickerInput(
      inputId = paste0(prefix, "_", .),
      label = stringr::str_to_sentence(gsub("_", " ", .)),
      choices = choic(., dict = columns, input = input),
      selected = def(.),
      options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
      multiple = multiple,
      inline = TRUE
    ))
  })
}

getColsForTbl <- function(tbl, sortNALast = TRUE, names = c("Standard concept ID")){
  
  cols <- list()
  for(i in seq_along(names(tbl))){
    working_col <- names(tbl)[i]
    
    if(working_col %in% c(names)){
      
      cols[[working_col]] <- colDef(name = working_col,
                                    sortNALast = sortNALast,
                                    cell = function(value){
                                      if(!is.na(value) && !grepl("^NA$", value)) {
                                        url <- sprintf("https://athena.ohdsi.org/search-terms/terms/%s", value)
                                        htmltools::tags$a(href = url, target = "_blank", as.character(value))
                                      }else{
                                        "-"
                                      }
                                    }
      )
      
    }else{
      cols[[working_col]] <- colDef(name = working_col,
                                    sortNALast = sortNALast,
                                    format = colFormat(separators = TRUE))
    }
  }
  
  return(cols)
}

metaIR <- function(df, rateMultiplier = 1e5, alpha = 0.05) {
  
  # need at least 2 databases
  if (nrow(df) < 2) {
    return(tibble(
      pooled_incidence_100000_pys = NA_real_,
      predicted_interval_95CI_lower = NA_real_,
      predicted_interval_95CI_upper = NA_real_,
      denominator_count = sum(df$denominator_count, na.rm = TRUE),
      person_years = sum(df$person_years, na.rm = TRUE),
      outcome_count = sum(df$outcome_count, na.rm = TRUE),
      outcome_percentage = outcome_count/denominator_count * 100,
      number_of_databases = nrow(df),
      tau2 = NA_real_
    ))
  }
  
  # Fit Poisson GLMM
  glmmModel <- tryCatch(
    glmer(
      outcome_count ~ 1 + (1 | cdm_name),
      offset = log(person_years),
      family = poisson(link = "log"),
      data = df,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(glmmModel)) {
    return(tibble(
      pooled_incidence_100000_pys = NA_real_,
      predicted_interval_95CI_lower = NA_real_,
      predicted_interval_95CI_upper = NA_real_,
      denominator_count = sum(df$denominator_count, na.rm = TRUE),
      person_years = sum(df$person_years, na.rm = TRUE),
      outcome_count = sum(df$outcome_count, na.rm = TRUE),
      outcome_percentage = outcome_count/denominator_count * 100,
      number_of_databases = nrow(df),
      tau2 = NA_real_
    ))
  }
  
  # Extract fixed effect
  pooledLogRate <- fixef(glmmModel)[["(Intercept)"]]
  pooledLogRateSe <- sqrt(vcov(glmmModel)[1, 1])
  
  # Between-database variance
  tau2 <- as.numeric(VarCorr(glmmModel)$cdm_name[1, 1])
  
  number_of_databases <- nrow(df)
  tCritical <- qt(1 - alpha / 2, df = max(1, number_of_databases - 1))
  
  # Prediction interval (log scale)
  piLogLower <- pooledLogRate - tCritical * sqrt(pooledLogRateSe^2 + tau2)
  piLogUpper <- pooledLogRate + tCritical * sqrt(pooledLogRateSe^2 + tau2)
  
  tibble(
    pooled_incidence_100000_pys = exp(pooledLogRate) * rateMultiplier,
    predicted_interval_95CI_lower = exp(piLogLower) * rateMultiplier,
    predicted_interval_95CI_upper = exp(piLogUpper) * rateMultiplier,
    denominator_count = sum(df$denominator_count, na.rm = TRUE),
    person_years = sum(df$person_years, na.rm = TRUE),
    outcome_count = sum(df$outcome_count, na.rm = TRUE),
    outcome_percentage = outcome_count/denominator_count * 100,
    number_of_databases = number_of_databases,
    tau2 = tau2
  )
}

pIncidence <- function(data, x, scales, ribbon = TRUE) {
  colors <- c(
    "#C94A5A",  # Muted red
    "#4A6FE3",  # Professional blue
    "#4FAE6A",  # Academic green
    "#E08A3C"   # Muted orange
  )
  p <- data |>
    ggplot(aes_string(x = x, y = "incidence_100000_pys", ymin = "incidence_100000_pys_95CI_lower", ymax = "incidence_100000_pys_95CI_upper", colour = "cdm_name", fill = "cdm_name", group = "cdm_name")) +
    geom_point(size = 2) +
    geom_line(linewidth = 0.3) +
    # geom_errorbar(width = 0.15,linewidth = 0.4) +
    facet_wrap(vars(outcome_cohort_name), scales = scales, nrow = 2) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    guides(
      color = guide_legend(title = "Database"),
      fill  = guide_legend(title = "Database")
    ) +
    ggplot2::theme_linedraw() +
    theme(
      strip.text = element_text(face = "bold", size = 10, colour = "white"),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    ) +
    xlab("") +
    ylab("Incidence (100,000 person-years)")
  if (ribbon) {
    p <- p +
      geom_ribbon(  alpha = 0.15, color = NA, show.legend = FALSE)
  }
  p
}

pSurvival <- function(data) {
  colors <- c("#C94A5A", "#4A6FE3", "#4FAE6A", "#E08A3C")
  data |>
    ggplot(aes(x = time, y = 1-estimate, ymin = 1-estimate_95CI_upper, ymax = 1-estimate_95CI_lower, colour = cdm_name, fill = cdm_name, group = cdm_name)) +
    geom_line() +
    geom_ribbon(color = NA, alpha = 0.3) +
    facet_wrap(vars(outcome), scales = "free_y") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    guides(
      color = guide_legend(title = "Database"),
      fill  = guide_legend(title = "Database")
    ) +
    ggplot2::theme_linedraw() +
    theme(
      strip.text = element_text(face = "bold", size = 10, colour = "white"),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    ) +
    xlab("Time (days)") +
    ylab("Cumulative Incidence")
}
