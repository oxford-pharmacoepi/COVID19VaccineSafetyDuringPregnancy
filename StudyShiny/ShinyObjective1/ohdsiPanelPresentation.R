library(CohortCharacteristics)
library(dplyr)
library(ggplot2)
library(IncidencePrevalence)
library(markdown)
library(omopgenerics)
library(OmopSketch)
library(tidyr)
library(visOmopResults)
library(CohortSurvival)
library(flextable)
library(lme4)
library(broom)     
library(glmmTMB)
library(purrr)
library(patchwork)
library(ggh4x)

# preprocess data if it has not been done
fileData <- file.path(getwd(), "data", "shinyData.RData")
if (!file.exists(fileData)) {
  source(file.path(getwd(), "data", "preprocess.R"))
}

# load shiny data
load(fileData)

# source functions
source(file.path(getwd(), "functions.R"))

maeCode <- c("gestational_diabetes" )
maeClean <- c("Gestational diabetes mellitus")

dbOrder <- c("SCIFI-PEARL", "NLHR", "SIDIAP", "CPRD GOLD")

outcomeGroupExpr <- glue::glue('case_when(group_level %in% c("Miscarriage", "Ectopic pregnancy", "Stillbirth") ~ "Foetal Loss",
 group_level %in% c("Gestational diabetes mellitus", "Antepartum haemorrhage") ~ "Non-hypertensive Pregnancy Complications", group_level %in% c("Pre-eclampsia", "Eclampsia", "HELLP syndrome") ~ "Hypertensive Disorders of Pregnancy", group_level %in% c("Preterm delivery", "Dysfunctional labour") ~ "Perinatal Events", group_level %in% c("Postpartum haemorrhage", "Postpartum endometritis") ~ "Postpartum Events", group_level %in% "Maternal death" ~ "Maternal Death", group_level %in% c("Pulmonary embolism", "Deep vein thrombosis") ~ "Venous Thromboembolic Events", group_level %in% c("Immune thrombocytopenia") ~ "Immune-mediated Haematologic Disorder", group_level %in% c("Thrombosis with thrombocytopenia syndrome", "Disseminated intravascular coagulation") ~ "Thrombotic and Coagulation Disorders", group_level %in% c("Myocarditis or pericarditis", "Myocardial infarction") ~ "Cardiovascular Events", group_level %in% c("Ischaemic stroke", "Haemorrhagic stroke") ~ "Cerebrovascular Events", group_level %in% c("Bell’s palsy", "Guillain–Barré syndrome", "Transverse myelitis", "Encephalitis", "Narcolepsy") ~ "Neurological (Immune-mediated) Events", group_level == "Anaphylaxis" ~ "Hypersensitivity", .default = NA)') |>
  rlang::parse_exprs() |> rlang::set_names("additional_level")

setGlobalTableOptions(type = "flextable")

# SURVIVAL ----
survival <-data$survival |>
  filter(cdm_name %in% dbOrder) |>
  filter(strata_name %in% c("socioeconomic_status", "ethnicity", "birth_continent", "nationallity")) |>
  omopgenerics::filterSettings(result_type == "survival_probability") |>
  tidy() |>
  dplyr::mutate(
    outcome = factor(
      .data$outcome, 
      levels = c(maeCode), 
      labels = c(maeClean), 
    ),
    cdm_name = factor(cdm_name, levels = dbOrder),
    time = as.numeric(time),
    estimate = (1 - estimate)*100,
    estimate_95CI_lower = (1 - estimate_95CI_lower)*100,
    estimate_95CI_upper = (1 - estimate_95CI_upper)*100
  ) |>
  dplyr::filter(!is.na(outcome)) 

limit_grid <- bind_rows(
  survival |>
    mutate(week = time/7) |>
    filter((week <= 35 & outcome == "Eclampsia") | week <= 40) |>
    filter(target_cohort == "pregnancy_denominator") |>
    group_by(outcome) |>
    summarise(facet_max = max(estimate, na.rm = TRUE), .groups = "drop") |>
    mutate(upper_limit = facet_max),
  survival |>
    mutate(week = time/7) |>
    filter(week <= 40) |>
    filter(target_cohort %in% c("pregnancy_denominator")) |>
    group_by(outcome) |>
    summarise(facet_max = max(estimate, na.rm = TRUE), .groups = "drop") |>
    mutate(upper_limit = facet_max)
)

custom_scales_list <- pmap(limit_grid, function(outcome, maternal_age_group, upper_limit, ...) {
  as.formula(
    paste0(
      'outcome == "', outcome, '" ~ ',
      'scale_y_continuous(limits = c(0, ', upper_limit, '), 
                          expand = expansion(mult = c(0.05, 0.1)),
                          oob = scales::rescale_none)'
    )
  )
})

colors <- c("#C94A5A", "#4A6FE3", "#4FAE6A", "#E08A3C")
cif <- function(x, ylimits, color, colors = c("#edae49", "#d1495b", "#00798c")) {
  x |>
    ggplot(aes_string(x = "week", y = "estimate", color = color, fill = color, group = color)) +
    geom_ribbon(
      aes(
        ymin = estimate_95CI_lower, 
        ymax = estimate_95CI_upper 
      ),
      alpha = 0.15,
      color = NA
    ) +
    geom_line() +
    facet_grid(. ~ cdm_name, scales = "free") + #,  labeller = labeller(outcome = label_wrap_gen(width = 10))) +
    # facetted_pos_scales(y = ylimits) +
    scale_y_continuous(limits = c(0, 20)) +
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors) +
    theme_bw() +
    theme(
      strip.text = element_text(face = "bold", size = 10, colour = "black"),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      panel.spacing.y = unit(0.8, "lines")
    )
}

# PLOT ----
trimester_days <- c(91, 189)/7
ethnicity <- survival |>
  mutate(
    week = time/7,
    ethnicity = factor(ethnicity, levels = c("Black", "Asian", "White", "Missing"))
  ) |>
  filter(week <= 40) |>
  filter(ethnicity != "overall", target_cohort == "pregnancy_denominator") |>
  cif(custom_scales_list, color = "ethnicity") +
  geom_vline(xintercept = trimester_days, linetype = "dashed", color = "grey40", alpha = 0.7) +
  labs(x = "Gestational time (weeks)", y = "Cumulative Incidence (%)") +
  ggtitle("A) Ethncity") +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(title = "Ethnicity"),
    fill  = guide_legend(title = "Ethnicity")
  ) 

nationality <- survival |>
  mutate(
    week = time/7,
    nationallity = factor(nationallity, levels = c("Africa", "Asia", "Europe", "Central/South America", "North America", "America"))
  ) |>
  filter(week <= 40) |>
  filter(nationallity != "overall", target_cohort == "pregnancy_denominator") |>
  filter(!nationallity %in% c("Missing", "Oceania")) |>
  cif(custom_scales_list, color = "nationallity") +
  geom_vline(xintercept = trimester_days, linetype = "dashed", color = "grey40", alpha = 0.7) +
  labs(x = "Gestational time (weeks)", y = "Cumulative Incidence (%)") +
  ggtitle("B) Nationality") +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(title = "Nationality"),
    fill  = guide_legend(title = "Nationality")
  ) 

birth <- survival |>
  mutate(
    week = time/7,
    birth_continent = factor(birth_continent, levels = c("Africa", "Asia", "Europe", "Central/South America", "North America", "America"))
  ) |>
  filter(week <= 40) |>
  filter(birth_continent != "overall", target_cohort == "pregnancy_denominator") |>
  filter(!birth_continent %in% c("Missing", "Oceania")) |>
  cif(custom_scales_list, color = "birth_continent") +
  geom_vline(xintercept = trimester_days, linetype = "dashed", color = "grey40", alpha = 0.7) +
  labs(x = "Gestational time (weeks)", y = "Cumulative Incidence (%)") +
  ggtitle("C) Birth region") +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(title = "Birth region", nrow = 1),
    fill  = guide_legend(title = "Birth region", nrow = 1)
  )


survival |>
  mutate(
    socioeconomic_status = case_when(
      socioeconomic_status == "Q1" & cdm_name == "SCIFI-PEARL" ~ "Q5",
      socioeconomic_status == "Q2" & cdm_name == "SCIFI-PEARL" ~ "Q4",
      socioeconomic_status == "Q4" & cdm_name == "SCIFI-PEARL" ~ "Q2",
      socioeconomic_status == "Q5" & cdm_name == "SCIFI-PEARL" ~ "Q1",
      .default = socioeconomic_status
    ),
    week = time/7,
    socioeconomic_status = factor(
      socioeconomic_status, 
      levels = c(paste0("Q", 1:5), paste0("U", 1:5), "R", "0N", "Missing"),
      labels = c("Q1 (least disadvantaged)", paste0("Q", 2:4), "Q5 (most disadvantaged)", "Q1 (least disadvantaged)", paste0("Q", 2:4), "Q5 (most disadvantaged)", "Rural", "Missing", "Missing")
    )
  ) |>
  filter(week <= 40) |>
  filter(socioeconomic_status != "overall", target_cohort == "pregnancy_denominator") |>
  cif(custom_scales_list, color = "socioeconomic_status") +
  geom_vline(xintercept = trimester_days, linetype = "dashed", color = "grey40", alpha = 0.7) +
  labs(x = "Gestational time (weeks)", y = "Cumulative Incidence (%)") +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(0, 12)) +
  guides(
    color = guide_legend(title = "Socioeconomic Status"),
    fill  = guide_legend(title = "Socioeconomic Status")
  )
