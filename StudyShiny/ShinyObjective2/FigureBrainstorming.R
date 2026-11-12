# Set-up ----
library(dplyr)
library(DT)
library(ggplot2)
library(gt)
library(omopgenerics)
library(purrr)
library(readr)
library(tidyr)
library(visOmopResults)
library(ggfortify)
library(scales)
library(meta)
library(EmpiricalCalibration)
library(here)
library(ggrepel)
library(forcats)
library(webshot2)
library(gt)
library(ggh4x)

fileData <- file.path(getwd(), "data", "shinyData.RData")
load(fileData)
source(file.path(getwd(), "functions.R"))

objective <- "objective_1"
cohortNameOrder <- paste0("population", c("_", "_miscarriage_", "_preterm_labour_"),  objective)
if (objective == "objective_1") {
  cohortNameClean <- c("First-dose vaccinated vs. Unvaccinated", "First-dose vaccinated vs. Unvaccinated (Miscarriage)", "First-dose vaccinated vs. Unvaccinated (Preterm delivery)")
  
} else if (objective == "objective_3") {
  cohortNameClean <- c("Boosted vs. Unboosted", "Boosted vs. Unboosted (Miscarriage)", "Boosted vs. Unboosted (Preterm delivery)")
  
}
dbOrder <- c("SCIFI-PEARL-v2", "NLHR@UiO-MBRN", "SIDIAP", "CPRD GOLD")
dbClean <- c("SCIFI-PEARL", "NLHR", "SIDIAP", "CPRD GOLD")
exposureOrder <- c("exposed", "comparator")
if (objective == "objective_1") {
  exposureClean <- c("Vaccinated (first-dose)", "Unvaccinated")
} else if (objective == "objective_3") {
  exposureClean <- c("Boosted", "Unboosted")
}
outcomeNameOrder <- c("miscarriage", "stillbirth", "eclampsia", "hellp", "antepartum_haemorrhage", "preterm_labour", "dysfunctional_labour", "postpartum_haemorrhage", "postpartum_endometritis", "maternal_death")
outcomeNameClean <- c("Miscarriage", "Stillbirth", "Eclampsia", "HELLP syndrome", "Antepartum haemorrhage", "Preterm delivery", "Dysfunctional labour", "Postpartum haemorrhage", "Postpartum endometritis", "Maternal death")

studyAnalysis <- "main"
if (studyAnalysis == "sensitivity") objective <- paste0(objective, "_", studyAnalysis)

figNum <- 1

plotOutcomes <- function(x, outcomeNameClean, dodge_offset, facet_var, xlim, colour) {
  
  # 1. Base data processing pipeline
  x_processed <- x |>
    select(!"variable_name") |>
    pivotEstimates(pivotEstimatesBy = c("estimate_name", "variable_level"), nameStyle = "{estimate_name}_{variable_level}") |>
    mutate(
      cdm_name = factor(cdm_name, dbOrder, dbClean),
      cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
      outcome_name = factor(outcome_name, rev(outcomeNameOrder), rev(outcomeNameClean))
    ) |>
    dplyr::filter(empirical_calibration == FALSE)
  
  # Clean up the weighting labels if that's the column we are plotting
  if ("weighting" %in% names(x_processed)) {
    x_processed <- x_processed |> 
      mutate(weighting = if_else(weighting == "TRUE", "PS Overlap Weights", "Unweighted"))
  }
  
  # 2. Build explicit outcome mapping coordinates
  outcome_mapping <- tibble(
    outcome_name = rev(outcomeNameClean),
    outcome_id = seq_along(outcomeNameClean)  
  )
  
  # 3. Dynamic Dodge Engine (Works for 2, 3, or any number of sub-groups)
  group_levels <- unique(na.omit(x_processed[[colour]]))
  num_groups <- length(group_levels)
  
  # Build a coordinate mapping grid for the categories
  group_mapping <- tibble(!!colour := group_levels) |>
    mutate(
      group_idx = seq_along(group_levels),
      # This scales and centers the offsets mathematically around the main row id
      group_shift = (group_idx - (num_groups + 1) / 2) * (dodge_offset * 2 / max(1, num_groups - 1))
    )
  
  # 4. Bind coordinates together and calculate capping boundaries
  mae_numeric <- x_processed |>
    inner_join(outcome_mapping, by = "outcome_name") |>
    inner_join(group_mapping, by = colour) |>
    mutate(
      y_coord = outcome_id + group_shift,
      lower_ci = if_else(!is.na(coef), lower_ci, NA),
      upper_ci = if_else(!is.na(coef), upper_ci, NA),
      # Bounds checks safely evaluating intervals if they exist
      cross_lower = if_else(!is.na(lower_ci), lower_ci < xlim[1], FALSE),
      cross_upper = if_else(!is.na(upper_ci), upper_ci > xlim[2], FALSE),
      lower_ci_capped = if_else(cross_lower, xlim[1], lower_ci),
      upper_ci_capped = if_else(cross_upper, xlim[2], upper_ci)
    )
  
  # 5. Define Zebra Stripes Coordinates
  rect_shading <- outcome_mapping |>
    filter(outcome_id %% 2 == 0) |>
    mutate(
      ymin = outcome_id - 0.5,
      ymax = outcome_id + 0.5,
      xmin = xlim[1],
      xmax = xlim[2]
    )
  
  # 6. Pair-checking bypass validation
  if (colour == "weighting") {
    mae_numeric <- mae_numeric |>
      group_by(cdm_name, cohort_name, outcome_name) |>
      filter(any(weighting == "PS Overlap Weights")) |>
      ungroup()
  }
  
  # 7. Generate Plot Layout
  mae_numeric |>
    ggplot(aes(y = y_coord, x = coef, color = .data[[colour]])) +
    
    # Background zebra-striping segments
    geom_rect(
      data = rect_shading,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
      fill = "grey93", alpha = 0.5, inherit.aes = FALSE
    ) +
    
    # Layered data elements
    ggplot2::geom_vline(xintercept = 1, color = "grey", linetype = "dashed", linewidth = 1) + 
    geom_linerange(aes(xmin = lower_ci_capped, xmax = upper_ci_capped), linewidth = 0.6) +
    
    geom_text(
      data = ~ subset(., cross_lower),
      aes(x = xlim[1], label = "◄"),
      size = 4, vjust = 0.35, show.legend = FALSE
    ) +
    geom_text(
      data = ~ subset(., cross_upper),
      aes(x = xlim[2], label = "►"),
      size = 4, vjust = 0.35, show.legend = FALSE
    ) +
    geom_point(size = 2.5) +
    
    facet_grid(as.formula(paste(facet_var, " ~ cdm_name")), labeller = label_wrap_gen(25)) +
    themeVisOmop() + 
    theme(
      strip.text.y.right = element_text(angle = 270, hjust = 0),
      panel.spacing.x = unit(1.5, "lines"),
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 8)
    ) +
    xlab("Incidence Rate Ratio") +
    ylab("Outcome") +
    scale_x_continuous(limits = xlim, oob = scales::rescale_none, expand = c(0, 0)) +
    scale_y_continuous(
      breaks = outcome_mapping$outcome_id,   
      labels = outcome_mapping$outcome_name  
    ) 
}

plotStratifiedNCOs <- function(data_input, outcome_order, strata_col, dodge_offset = 0.2, xlim_val = 4, facet_formula = "cohort_name ~ cdm_name") {
  
  # --- Step 1: Base Processing & Combined Mapping ---
  nco_processed <- data_input |>
    mutate(
      outcome_name = factor(outcome_name, levels = outcome_order)
    )
  
  # Map continuous vertical coordinates based on outcome order index
  outcome_mapping <- tibble(
    outcome_name = factor(outcome_order, levels = outcome_order),
    outcome_id = seq_along(outcome_order)
  )
  
  # --- DYNAMIC STRATA DODGE ENGINE ---
  # Pull unique values from the column specified by the user
  strata_groups <- unique(na.omit(nco_processed[[strata_col]]))
  num_strata <- length(strata_groups)
  
  # Dynamic offset allocation reference grid
  strata_mapping <- tibble(!!strata_col := strata_groups) |>
    mutate(
      strata_idx = seq_along(strata_groups),
      # Centers layout offsets mathematically around the core integer line item
      strata_shift = (strata_idx - (num_strata + 1) / 2) * (dodge_offset * 2 / max(1, num_strata - 1))
    )
  
  # Bind layouts together and configure color combination variables using dynamic string injection
  nco_numeric <- nco_processed |>
    inner_join(outcome_mapping, by = "outcome_name") |>
    inner_join(strata_mapping, by = strata_col) |>
    mutate(
      y_coord = outcome_id + strata_shift,
      # Combined interaction category for explicit matrix legends
      color_group = paste0(significance_95ci, " - ", .data[[strata_col]])
    )
  
  # --- Step 2: Dynamically Calculate Text Summaries per Facet Grid ---
  nco_counts <- nco_numeric |>
    group_by(cdm_name, cohort_name) |>
    summarize(
      total_obs = n(),
      sig_obs = sum(significance_95ci == "Statistically Significant", na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_sig = if_else(total_obs > 0, round((sig_obs / total_obs) * 100, 1), 0),
      label_text = paste0(
        "Significant NCOs:\n",
        "Weighted Strata: ", sig_obs, " (", pct_sig, "%)"
      ),
      y_coord = 1,
      coef = xlim_val
    )
  
  # --- Step 3: Zebra-Stripe Background Geometry ---
  rect_shading <- outcome_mapping |>
    filter(outcome_id %% 2 == 0) |>
    mutate(
      ymin = outcome_id - 0.5,
      ymax = outcome_id + 0.5,
      xmin = 0.01,
      xmax = xlim_val
    )
  
  # --- Step 4: Render Complete Object Graphics ---
  ggplot(nco_numeric, aes(y = y_coord, x = coef, color = color_group)) +
    
    # Background Canvas Striping
    geom_rect(
      data = rect_shading,
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
      fill = "grey93", alpha = 0.5, inherit.aes = FALSE
    ) +
    
    # Core Data Marks
    geom_vline(xintercept = 1, color = "grey", linetype = "dashed", linewidth = 1) + 
    geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), linewidth = 0.6) +
    geom_point(size = 2.2) +
    
    # Summary Label Blocks
    # geom_text(
    #   data = nco_counts,
    #   aes(x = coef, y = y_coord, label = label_text),
    #   size = 2.8, 
    #   hjust = 1, 
    #   vjust = 0,
    #   color = "black", 
    #   fontface = "italic", 
    #   lineheight = 0.95,
    #   inherit.aes = FALSE  # Keeps the global color mapping from breaking the text
    # )
    
    # Layout Configurations
    facet_grid(as.formula(facet_formula), labeller = label_wrap_gen(25)) +
    themeVisOmop() + 
    theme(
      strip.text.y.right = element_text(angle = 270, hjust = 0),
      panel.spacing = unit(0.8, "lines"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 8)
    ) +
    xlab("Outcome") +
    ylab("Incidence Rate Ratio") +
    scale_x_continuous(limits = c(0.01, xlim_val), oob = scales::rescale_none, expand = c(0, 0)) +
    scale_y_continuous(
      breaks = outcome_mapping$outcome_id,   
      labels = outcome_mapping$outcome_name  
    )
}

# Population counts ----
data$summarise_characteristics |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder,
    variable_name %in% paste0("Number ", c("records", "subjects"))
  ) |>
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall",
    .data$exposure != "overall"
  ) |>
  filterSettings(weighting == FALSE) |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    group_level = factor(group_level, cohortNameOrder, cohortNameClean),
    strata_level = factor(strata_level, exposureOrder, exposureClean)
  ) |>
  arrange(cdm_name, group_level) |>
  visOmopTable(
    estimateName = c("N" = "<count>"),
    groupColumn = "cohort_name",
    header = c("Data source", "cdm_name", "Exposure"),
    hide = c("vaccine_brand", "age_group", "gestational_trimester", "variable_level", "table_name", "weighting", "estimate_name"),
    .options = list(includeHeaderName = FALSE)
  )  |>
  gt::gtsave(filename = here("Figures", objective, paste0(figNum, "_counts.png")))
figNum <- figNum + 1

# Propensity Scores ----
ps <- data$propensity_scores |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |>
  omopgenerics::tidy() |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
    exposure = factor(exposure, exposureOrder, exposureClean)
  ) 
ps |>
  ggplot(aes(x = x, y = y, fill = exposure, color = exposure)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "transparent") +
  geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.2) +
  facet_grid(cohort_name ~ cdm_name, labeller = label_wrap_gen(25)) +
  themeVisOmop() + 
  theme(
    strip.text.y.right = element_text(angle = 270, hjust = 0),
    panel.spacing = unit(0.8, "lines"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    x = "Propensity Score",
    y = "Density",
    fill  = "Treatment Strategy",
    colour  = "Treatment Strategy"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) 

ggsave(
  filename = here("Figures", objective, paste0(figNum, "_ps.png")),
  dpi = 250, width = 10, height = 6
)
figNum <- figNum + 1


## Stratas ----
for (strata in c("gestational_trimester", "vaccine_brand", "age_group")) {
  filterExpr <- c(
    "vaccine_brand" = ".data$vaccine_brand == 'overall'",
    "gestational_trimester" = ".data$gestational_trimester == 'overall'",
    "age_group" = ".data$age_group == 'overall'"
  )
  filterExpr[strata] <- gsub("==", "!=", filterExpr[strata] )
  filterExpr <- filterExpr |> paste0(collapse = " & ") |> rlang::parse_expr()
  
  ps_subset <- data$propensity_scores |>
    dplyr::filter(
      .data$cdm_name %in% dbOrder,
      .data$group_level %in% cohortNameOrder
    ) |> 
    omopgenerics::filterStrata(!!filterExpr)
  
  ps_subset <- ps_subset |>
    omopgenerics::tidy() |>
    mutate(
      cdm_name = factor(cdm_name, dbOrder, dbClean),
      cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
      exposure = factor(exposure, exposureOrder, exposureClean), 
      age_group = paste0(age_group, " years"),
      vaccine_brand = stringr::str_to_sentence(vaccine_brand),
      gestational_trimester = gsub("T", "Trimester ", gestational_trimester)
    )
  
  name_title <- stringr::str_to_sentence(gsub("_", " ", strata))
  rows_vars <- rlang::syms(c("cohort_name", strata))
  cols_vars <- rlang::syms("cdm_name")
  
  ps_subset |>
    filter(age_group != "12 to 17 years") |>
    ggplot(aes(x = x, y = y, fill = exposure, color = exposure)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "transparent") +
    geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.2) +
    
    # Dynamic Stratified Faceting
    ggh4x::facet_nested(
      rows = vars(!!!rows_vars), 
      cols = vars(!!!cols_vars), 
      labeller = label_wrap_gen(25),
      nest_line = FALSE # Set to TRUE if you want a subtle hierarchy line inside the strip
    ) +
    
    themeVisOmop() + 
    theme(
      strip.text.y.right = element_text(angle = 270, hjust = 0),
      panel.spacing = unit(0.8, "lines"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    ) +
    labs(
      x = "Propensity Score",
      y = "Density",
      fill   = "Treatment Strategy",
      colour = "Treatment Strategy"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) 
  
  ggsave(
    filename = here("Figures", objective, paste0(figNum, "_ps_density_", strata, ".png")),
    dpi = 250, width = 11, height = 9
  )
  
  figNum <- figNum + 1
}

# ASMD ----
smd <- data$summarise_standardised_mean_differences |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |> 
  omopgenerics::tidy() |>
  dplyr::mutate(
    smd = abs(smd), 
    weighting = dplyr::if_else(weighting == "TRUE", "weighted_smd", "unweighted_smd")
  ) |>
  select(!c("weights")) |>
  tidyr::pivot_wider(names_from = "weighting", values_from = "smd") |>
  filter(!is.na(weighted_smd) & !is.na(unweighted_smd)) |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
    exposure = factor(exposure, exposureOrder, exposureClean),
    variable_name = case_match(
      variable_name,
      "birth_continent" ~ "Birth continent",
      "nationallity" ~ "Nationallity",
      "region" ~ "Region",
      "ethnicity" ~ "Ethnicity",
      "Influenza, split virus, trivalent, injectable, contains preservative" ~ "Influenza vaccine",
      "loratadine 10 MG Oral Tablet" ~ "Loratadine (10 MG Oral Tablet)",
      "influenza virus vaccine, split virus (incl. purified surface antigen)-retired CODE" ~ "Influenza vaccine",
      "pre_pregnancy_smoking" ~ "Pre-pregnancy smoking",
      .default = variable_name
    ),
    variable_level = if_else(grepl(" to ", variable_level), paste0(variable_level, " days"), variable_level),
    concept_name = paste0(variable_name, " [", variable_level, "]")
  ) 

smdLines <- smd |>
  distinct(cdm_name, cohort_name) |>
  filter(
    !(cdm_name %in% c("SCIFI-PEARL", "CPRD GOLD") &
        cohort_name == "Boosted vs. Unboosted (Miscarriage)")
  )

labelsData <- smd |> 
  dplyr::filter(unweighted_smd > 0.1)

smd |>
  ggplot(aes(x = weighted_smd, y = unweighted_smd, colour = concept_name)) +
  geom_point() +
  ggplot2::geom_vline(data = smdLines, aes(xintercept = 0.1), color = "#495057", linewidth = 0.4, linetype = "dashed") +
  ggplot2::geom_hline(data = smdLines,  aes(yintercept = 0.1), color = "black", linewidth = 0.4, linetype = "dashed") + 
  ggplot2::geom_abline(data = smdLines,  aes(intercept = 0, slope = 1), color = "black", linewidth = 0.4) +
  facet_grid(cohort_name ~ cdm_name, labeller = label_wrap_gen(25)) +
  geom_text_repel(
    data = labelsData,
    aes(label = concept_name),
    color = "black",          # Solid label color so it's readable
    size = 1.8,               # Adjust text font size here
    box.padding = 00.05,        # Padding around the text box
    point.padding = 0.1,      # Distance between text and the dot
    segment.color = "grey50", # Color of the tiny line connecting text to dot
    segment.linewidth = 0.3,
    show.legend = FALSE       # Prevents 'a' from showing up in your legend box
  ) +
  themeVisOmop() + 
  theme(
    strip.text.y.right = element_text(angle = 270, hjust = 0),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    x = "Absolute Standardised Mean Differences (PS Overlap Weighting)",
    y = "Absolute Standardised Mean Differences (Unweighted)"
  ) +
  scale_x_continuous(limits = c(0, 0.25)) 

ggsave(
  filename = here("Figures", objective, paste0(figNum, "_smd.png")),
  dpi = 250, width = 10, height = 6
)
figNum <- figNum + 1

## Stratas ----
for (strata in c("gestational_trimester", "vaccine_brand", "age_group")) {
  filterExpr <- c(
    "vaccine_brand" = ".data$vaccine_brand == 'overall'",
    "gestational_trimester" = ".data$gestational_trimester == 'overall'",
    "age_group" = ".data$age_group == 'overall'"
  )
  filterExpr[strata] <- gsub("==", "!=", filterExpr[strata] )
  filterExpr <- filterExpr |> paste0(collapse = " & ") |> rlang::parse_expr()
  
  smd <- data$summarise_standardised_mean_differences |>
    dplyr::filter(
      .data$cdm_name %in% dbOrder,
      .data$group_level %in% cohortNameOrder
    ) |> 
    omopgenerics::filterStrata(
      !!filterExpr
    ) |> 
    omopgenerics::tidy() |>
    dplyr::mutate(
      smd = abs(smd), 
      weighting = dplyr::if_else(weighting == "TRUE", "weighted_smd", "unweighted_smd")
    ) |>
    select(!c("weights")) |>
    tidyr::pivot_wider(names_from = "weighting", values_from = "smd") |>
    filter(!is.na(weighted_smd) & !is.na(unweighted_smd)) |>
    mutate(
      cdm_name = factor(cdm_name, dbOrder, dbClean),
      cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
      exposure = factor(exposure, exposureOrder, exposureClean),
      variable_name = case_match(
        variable_name,
        "birth_continent" ~ "Birth continent",
        "nationallity" ~ "Nationallity",
        "region" ~ "Region",
        "ethnicity" ~ "Ethnicity",
        "Influenza, split virus, trivalent, injectable, contains preservative" ~ "Influenza vaccine",
        "loratadine 10 MG Oral Tablet" ~ "Loratadine (10 MG Oral Tablet)",
        "influenza virus vaccine, split virus (incl. purified surface antigen)-retired CODE" ~ "Influenza vaccine",
        "pre_pregnancy_smoking" ~ "Pre-pregnancy smoking",
        .default = variable_name
      ),
      variable_level = if_else(grepl(" to ", variable_level), paste0(variable_level, " days"), variable_level),
      concept_name = paste0(variable_name, " [", variable_level, "]"),
      age_group = paste0(age_group, " years"),
      vaccine_brand = stringr::str_to_sentence(vaccine_brand),
      gestational_trimester = gsub("T", "Trimester ", gestational_trimester)
    ) 
  
  smdLines <- smd |>
    distinct(cdm_name, cohort_name) |>
    filter(
      !(cdm_name %in% c("SCIFI-PEARL", "CPRD GOLD") &
          cohort_name == "Boosted vs. Unboosted (Miscarriage)")
    )
  
  labelsData <- smd |> 
    dplyr::filter(weighted_smd > 0.1)
  
  name <- stringr::str_to_sentence(gsub("_", " ", strata)) 
  
  smd |>
    ggplot(aes_string(x = "weighted_smd", y = "unweighted_smd", colour = strata)) +
    geom_point() +
    ggplot2::geom_vline(data = smdLines, aes(xintercept = 0.1), color = "#495057", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_hline(data = smdLines,  aes(yintercept = 0.1), color = "black", linewidth = 0.4, linetype = "dashed") + 
    ggplot2::geom_abline(data = smdLines,  aes(intercept = 0, slope = 1), color = "black", linewidth = 0.4) +
    facet_grid(cohort_name ~ cdm_name, labeller = label_wrap_gen(25)) +
    geom_text_repel(
      data = labelsData,
      aes(label = concept_name),
      color = "black",          # Solid label color so it's readable
      size = 1.8,               # Adjust text font size here
      box.padding = 00.05,        # Padding around the text box
      point.padding = 0.1,      # Distance between text and the dot
      segment.color = "grey50", # Color of the tiny line connecting text to dot
      segment.linewidth = 0.3,
      show.legend = FALSE       # Prevents 'a' from showing up in your legend box
    ) +
    themeVisOmop() + 
    theme(
      strip.text.y.right = element_text(angle = 270, hjust = 0),
      panel.spacing = unit(0.8, "lines"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    ) +
    labs(
      x = "Absolute Standardised Mean Differences (PS Overlap Weighting)",
      y = "Absolute Standardised Mean Differences (Unweighted)"
    ) +
    scale_x_continuous(limits = c(0, 0.25)) +
    scale_color_manual(
      name = name,
      values = c(
        "Moderna" = "#3d5a80",
        "Pfizer" = "#e07a5f",
        "Trimester 1" = "#06a77d",
        "Trimester 2" = "#e9c46a",
        "Trimester 3" = "#264653",
        "12 to 17 years" = "#1e6091", # Soft Pastel Teal
        "18 to 34 years" = "#52b788", # Slate Lavender
        "35 to 55 years" = "#4a4e69"  # Deep Charcoal Purple
      )
    )
  
  ggsave(
    filename = here("Figures", objective, paste0(figNum, "_smd_", strata, ".png")),
    dpi = 250, width = 10, height = 6.5
  )
  figNum <- figNum + 1
}


# NCO ----
nco <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Negative Control Outcomes"
  ) |>
  filter(variable_name == "Relative Risk") |>
  omopgenerics::tidy() |>
  filter(!is.na(coef)) |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
    significance_95ci = if_else(
      lower_ci > 1 | upper_ci < 1, 
      "Statistically Significant", 
      "Not Statistically Significant"
    ),
    outcome_name = stringr::str_to_sentence(gsub("_", " ", outcome_name)),
    weighting = if_else(weighting == "TRUE", "PS Overlap Weights", "Unweighted")
  ) |>
  group_by(cdm_name, cohort_name, outcome_name) |>
  filter(any(weighting == "PS Overlap Weights")) |>
  ungroup() |>
  mutate(outcome_name = fct_reorder(outcome_name, coef))

xlim <- 4 

ncoCounts <- nco |>
  dplyr::group_by(cdm_name, cohort_name) |>
  dplyr::summarize(
    total_w = sum(weighting == "PS Overlap Weights", na.rm = TRUE),
    total_uw = sum(weighting == "Unweighted", na.rm = TRUE),
    w_sig = sum(weighting == "PS Overlap Weights" & significance_95ci == "Statistically Significant", na.rm = TRUE),
    uw_sig = sum(weighting == "Unweighted" & significance_95ci == "Statistically Significant", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    w_pct = if_else(total_w > 0, round((w_sig / total_w) * 100, 1), 0),
    uw_pct = if_else(total_uw > 0, round((uw_sig / total_uw) * 100, 1), 0),
    label_text = paste0(
      "Significant NCOs:\n",
      "Weighted: ", w_sig, " (", w_pct, "%)\n",
      "Unweighted: ", uw_sig, " (", uw_pct, "%)"
    ),
    outcome_name = factor(levels(nco$outcome_name)[1], levels = levels(nco$outcome_name)),
    coef = xlim,
    lower_ci = 1,
    upper_ci = 1,
    significance_95ci_weighting = "Statistically Significant - PS Overlap Weights"
  )

nco |>
  scatterPlot(
    x = "outcome_name",
    y = "coef",
    line = FALSE,
    point = TRUE,
    ribbon = FALSE,
    ymin = "lower_ci",
    ymax = "upper_ci",
    colour = c("significance_95ci", "weighting")
  ) +
  ggplot2::geom_text(
    data = ncoCounts,
    aes(label = label_text),
    size = 2.8,             
    hjust = 1,              
    vjust = 0,              
    color = "black",        
    fontface = "italic",
    lineheight = 0.95,
    show.legend = FALSE
  ) +
  facet_grid(cohort_name ~ cdm_name, labeller = label_wrap_gen(25)) +
  themeVisOmop() + 
  theme(
    strip.text.y.right = element_text(angle = 270, hjust = 0),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  ) +
  ggplot2::geom_hline(data = smdLines, aes(yintercept = 1), color = "grey", linetype = "dashed", linewidth = 1) + 
  ggplot2::coord_flip() +
  ggplot2::ylab("Incidence Rate Ratio") +
  scale_y_continuous(limits = c(0.01, xlim), oob=scales::rescale_none) +
  scale_color_manual(
    values = c(
      "Statistically Significant - PS Overlap Weights" = "#bc6c25",
      "Statistically Significant - Unweighted" = "#dda15e",
      "Not Statistically Significant - PS Overlap Weights" = "#264653",
      "Not Statistically Significant - Unweighted" = "#2a9d8f"
    )
  ) + 
  guides(
    color = guide_legend(nrow = 2, title = "95% CI significance - Weighting"),
    fill = guide_legend(nrow = 2, title = "95% CI significance - Weighting")
  ) 

ggsave(
  filename = here("Figures", objective, paste0(figNum, "_nco.png")),
  dpi = 250, width = 15, height = 9
)
figNum <- figNum + 1

## Stratas ----
ncoOrder <- nco$outcome_name |> levels()
for (strata in c("gestational_trimester", "vaccine_brand", "age_group")) {
  filterExpr <- c(
    "vaccine_brand" = ".data$vaccine_brand == 'overall'",
    "gestational_trimester" = ".data$gestational_trimester == 'overall'",
    "age_group" = ".data$age_group == 'overall'"
  )
  filterExpr[strata] <- gsub("==", "!=", filterExpr[strata] )
  filterExpr <- filterExpr |> paste0(collapse = " & ") |> rlang::parse_expr()
  
  nco <- data$incidence_rate_ratio |>
    dplyr::filter(
      .data$cdm_name %in% dbOrder,
      .data$group_level %in% cohortNameOrder
    ) |> 
    omopgenerics::filterStrata(
      !!filterExpr
    ) |> 
    filterSettings(
      # weighting == TRUE, 
      study_analysis == studyAnalysis,
      outcome_group == "Negative Control Outcomes"
    ) |>
    filter(variable_name == "Relative Risk") |>
    omopgenerics::tidy() |>
    filter(!is.na(coef)) |>
    mutate(
      cdm_name = factor(cdm_name, dbOrder, dbClean),
      cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
      significance_95ci = if_else(
        lower_ci > 1 | upper_ci < 1, 
        "Statistically Significant", 
        "Not Statistically Significant"
      ),
      outcome_name = stringr::str_to_sentence(gsub("_", " ", outcome_name)),
      weighting = if_else(weighting == "TRUE", "PS Overlap Weights", "Unweighted"),
      age_group = paste0(age_group, " years"),
      vaccine_brand = stringr::str_to_sentence(vaccine_brand),
      gestational_trimester = gsub("T", "Trimester ", gestational_trimester)
    ) |>
    filter(weighting == "PS Overlap Weights") 

  name <- stringr::str_to_sentence(gsub("_", " ", strata)) 
  
  plotStratifiedNCOs(
    data_input = nco,
    outcome_order = ncoOrder,
    strata_col = strata,
    dodge_offset = 0.2,
    xlim_val = xlim,
    facet_formula = "cohort_name ~ cdm_name"
  ) +
    scale_color_manual(
      values = c(
        # --- Statistically Significant Colors (Warm Sunset Matrix) ---
        "Statistically Significant - 12 to 17 years"     = "#e76f51", 
        "Statistically Significant - 18 to 34 years"     = "#f4a261", 
        "Statistically Significant - 35 to 55 years"     = "#e9c46a", 
        "Statistically Significant - Moderna"            = "#3d5a80", # Slate Blue
        "Statistically Significant - Pfizer"             = "#e07a5f", # Terracotta Coral
        "Statistically Significant - Trimester 1"        = "#06a77d", # Emerald Mint
        "Statistically Significant - Trimester 2"        = "#e9c46a", # Muted Gold
        "Statistically Significant - Trimester 3"        = "#264653", # Deep Sea Teal
        
        # --- Not Statistically Significant Colors (Cool Blue/Teal Matrix) ---
        "Not Statistically Significant - 12 to 17 years" = "#1d3557", 
        "Not Statistically Significant - 18 to 34 years" = "#457b9d", 
        "Not Statistically Significant - 35 to 55 years" = "#a8dadc",
        "Not Statistically Significant - Moderna"        = "#98a6b7", # Muted Gray-Blue
        "Not Statistically Significant - Pfizer"         = "#f2c6b9", # Soft Pale Coral
        "Not Statistically Significant - Trimester 1"    = "#a2dfce", # Light Sage/Mint
        "Not Statistically Significant - Trimester 2"    = "#f4e3b1", # Soft Pale Yellow
        "Not Statistically Significant - Trimester 3"    = "#7fa1ae"  # Slate Teal
      )
    ) +
    guides(
      color = guide_legend(nrow = 2, title = paste0("Significance status & ", name))
    )

  
  ggsave(
    filename = here("Figures", objective, paste0(figNum, "_nco_", strata, ".png")),
    dpi = 250, width = 15, height = 9
  )
  figNum <- figNum + 1
}

# PCO ----
pco <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Positive Control Outcomes"
  ) |>
  filter(variable_name == "Relative Risk")  |>
  addSettings(settingsColumn = c("weighting")) |>
  splitAll()|>
  filter(empirical_calibration == FALSE) |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
    weighting = if_else(weighting == "TRUE", "PS Overlap Weights", "Unweighted")
  ) |>
  select(!c("result_id", "vaccine_brand", "gestational_trimester", "age_group", "variable_name", "variable_level", "empirical_calibration"))

pco |> 
  arrange(cdm_name, cohort_name) |>
  visTable(
    estimateName = c("IRR (95% CI)" = "<coef> (<lower_ci> - <upper_ci>)"),
    header = c("estimate_name", "Data source", "cdm_name"),
    groupColumn = "cohort_name",
    hide = c("estimate_type", "outcome_name", "confidence_interval"),
    .options = list(includeHeaderName = FALSE)
  ) |>
  gt::gtsave(filename = here("Figures", objective, paste0(figNum, "_pco.png")))
figNum <- figNum + 1

## Strata ----
for (strata in c("gestational_trimester", "vaccine_brand", "age_group")) {
  filterExpr <- c(
    "vaccine_brand" = ".data$vaccine_brand == 'overall'",
    "gestational_trimester" = ".data$gestational_trimester == 'overall'",
    "age_group" = ".data$age_group == 'overall'"
  )
  filterExpr[strata] <- gsub("==", "!=", filterExpr[strata] )
  filterExpr <- filterExpr |> paste0(collapse = " & ") |> rlang::parse_expr()
  
  pco_subset <- data$incidence_rate_ratio |>
    dplyr::filter(
      .data$cdm_name %in% dbOrder,
      .data$group_level %in% cohortNameOrder
    ) |> 
    omopgenerics::filterStrata(!!filterExpr)|> 
    filterSettings(
      study_analysis == studyAnalysis,
      outcome_group == "Positive Control Outcomes"
    ) |>
    filter(variable_name == "Relative Risk")
  
  pco_subset <- pco_subset |>
    addSettings(settingsColumn = c("weighting")) |>
    splitAll() |>
    filter(empirical_calibration == FALSE, weighting == "TRUE") |>
    mutate(
      cdm_name = factor(cdm_name, dbOrder, dbClean),
      cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean),
      age_group = paste0(age_group, " years"),
      vaccine_brand = stringr::str_to_sentence(vaccine_brand),
      gestational_trimester = gsub("T", "Trimester ", gestational_trimester)
    )
  
  
  # Identify columns to drop, ensuring the active strata column is preserved
  all_drop_cols <- c("result_id", "vaccine_brand", "gestational_trimester", "weighting",
                     "age_group", "variable_name", "variable_level", "empirical_calibration")
  active_drop_cols <- setdiff(all_drop_cols, strata)
  
  pco_table_data <- pco_subset |> 
    select(!dplyr::all_of(active_drop_cols)) |>
    arrange(cdm_name, cohort_name, .data[[strata]])
  
  pco_table_data |>
    visTable(
      estimateName = c("IRR (95% CI)" = "<coef> (<lower_ci> - <upper_ci>)"),
      header = c("estimate_name", "Data source", "cdm_name"),
      groupColumn = "cohort_name", # Cleans and unions the outer layout rows
      hide = c("estimate_type", "outcome_name", "confidence_interval"),
      .options = list(includeHeaderName = FALSE)
    ) |>
    gt::gtsave(filename = here("Figures", objective, paste0(figNum, "_pco_", strata, ".png")))
  
  figNum <- figNum + 1
}

# MAE ----
## Overall ----
xlim <- c(0.1, 2.5)
mae <- data$incidence_rate_ratio |> 
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Maternal Adverse Events"
  ) |>
  addSettings(settingsColumn = "weighting") |>
  splitAll() 
plotOutcomes(mae, outcomeNameClean, 0.15, ".", xlim, "weighting") +
  scale_color_manual(
    name = "Weighting Strategy",
    values = c("PS Overlap Weights" = "#264653", "Unweighted" = "#2a9d8f")
  )
ggsave(
  filename = here("Figures", objective, paste0(figNum, "_irr.png")),
  dpi = 250, width = 10, height = 4
)
figNum <- figNum + 1

## Trimester ----
mae <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester != "overall",
    .data$age_group %in% "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Maternal Adverse Events"
  ) |>
  addSettings(settingsColumn = "weighting") |>
  filter(weighting == "TRUE") |>
  splitAll() |>
  mutate(gestational_trimester = gsub("T", "Trimester ", gestational_trimester))
plotOutcomes(mae, outcomeNameClean, dodge_offset = 0.15, facet_var = ".", xlim, "gestational_trimester") +
  scale_color_manual(
    name = "Gestational Trimester",
    values = c(
      "Trimester 1" = "#06a77d",
      "Trimester 2" = "#e9c46a",
      "Trimester 3" = "#264653"
    )
  )

ggsave(
  filename = here("Figures", objective, paste0(figNum, "_irr_trimester.png")),
  dpi = 250, width = 10, height = 5
)
figNum <- figNum + 1

## Vaccine ----
mae <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand != "overall",
    .data$gestational_trimester == "overall",
    .data$age_group %in% "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Maternal Adverse Events"
  ) |>
  addSettings(settingsColumn = "weighting") |>
  splitAll() |>
  filter(weighting == "TRUE") |>
  mutate(vaccine_brand = stringr::str_to_sentence(vaccine_brand))
plotOutcomes(mae, outcomeNameClean, dodge_offset = 0.15, facet_var = ".", xlim, "vaccine_brand") +
  scale_color_manual(
    name = "Vaccine Brand",
    values = c(
      "Moderna" = "#3d5a80",
      "Pfizer" = "#e07a5f"
    )
  )
ggsave(
  filename = here("Figures", objective, paste0(figNum, "_irr_brand.png")),
  dpi = 250, width = 10, height = 5
)
figNum <- figNum + 1

## Age ----
mae <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |> 
  omopgenerics::filterStrata(
    .data$vaccine_brand == "overall",
    .data$gestational_trimester == "overall",
    .data$age_group != "overall"
  ) |> 
  filterSettings(
    # weighting == TRUE, 
    study_analysis == studyAnalysis,
    outcome_group == "Maternal Adverse Events"
  ) |>
  addSettings(settingsColumn = "weighting") |>
  filter(weighting == "TRUE") |>
  splitAll() |>
  mutate(age_group = paste0(age_group, " years"))
plotOutcomes(mae, outcomeNameClean, 0.15, ".", xlim = c(0.25, 2.5), "age_group")  +
  scale_color_manual(
    name = "Age Categories",
    values = c(
      "12 to 17 years" = "#1e6091", # Soft Pastel Teal
      "18 to 34 years" = "#52b788", # Slate Lavender
      "35 to 55 years" = "#4a4e69"  # Deep Charcoal Purple
    )
  )
ggsave(
  filename = here("Figures", objective, paste0(figNum, "_irr_age.png")),
  dpi = 250, width = 10, height = 5
)
figNum <- figNum + 1


# AESI ----
aesi <- data$incidence_rate_ratio |>
  dplyr::filter(
    .data$cdm_name %in% dbOrder,
    .data$group_level %in% cohortNameOrder
  ) |>
  omopgenerics::filterStrata(
    .data$vaccine_brand %in% "overall",
    .data$gestational_trimester %in% "overall",
    .data$age_group %in% "overall"
  ) |>
  filterSettings(
    weighting == FALSE,
    study_analysis == studyAnalysis,
    outcome_group == "Adverse Events of Special Interest"
  ) |>
  filter(variable_name %in% c("Relative Risk")) |>
  splitAll() |>
  filter(empirical_calibration == FALSE) |>
  mutate(
    cdm_name = factor(cdm_name, dbOrder, dbClean),
    cohort_name = factor(cohort_name, cohortNameOrder, cohortNameClean)
  ) |>
  select(!c("result_id", "vaccine_brand", "gestational_trimester", "age_group",  "empirical_calibration"))
# 
aesi |>
  arrange(cdm_name, cohort_name) |>
  visTable(
    estimateName = c("IRR (95% CI)" = "<coef> (<lower_ci> - <upper_ci>)"),
    header = c("Data source", "cdm_name"),
    # groupColumn = "cohort_name",
    hide = c("estimate_type", "outcome_name", "confidence_interval", "variable_level", "variable_name"),
    .options = list(includeHeaderName = FALSE)
  ) |>
  gt::gtsave(filename = here("Figures", objective, paste0(figNum, "_aesi.png")))
# figNum <- figNum + 1
