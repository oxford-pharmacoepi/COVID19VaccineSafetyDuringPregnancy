# Figures comparison ----
# 'Age'
smd <- data$summarise_characteristics |>
  filter(strata_name == "overall")|>
  filter(
    variable_name %in% c(
      'Pre-Pregnancy Smoke',
      'Maternal Adverse Events during pregnancy', 
      'Maternal Adverse Events during previous pregnancy',
      'History of comorbidities', 'Covariates in the past year',
      'Medications in the past 6 months'
    ),
    grepl("sample|match", group_level),
    !grepl("_sens", group_level)
  ) |>
  splitAll() |>
  mutate(
    type = if_else(grepl("sampled", cohort_name), "sampled", "matched"),
    cohort_name = gsub("_sampled|_matched", "", cohort_name),
    estimate_value = as.numeric(estimate_value)
  ) |>
  select(!estimate_type) |>
  addSettings() |>
  pivot_wider(names_from = c("estimate_name", "type"), values_from = "estimate_value") |>
  filter(cohort_name %in% c(maeCode, "pulmonary_embolism", "deep_vein_thrombosis", "immune_thrombocytopenia", "bells_palsy")) |>
  filter(cohort_name != "maternal_death") |>
  filter(!(cohort_name == variable_level & variable_name == 'Maternal Adverse Events during pregnancy'), variable_level != "Diabetes") |>
  dplyr::mutate( 
    variable_name = factor(
      variable_name,
      levels = c(
        "Number records", 
        "Number subjects",
        "Maternal age", 
        "Maternal age group",
        "Trimester",
        "Days in cohort", 
        # "Pre-Pregnancy Smoke",
        "Covariates in the past year",
        "History of comorbidities", 
        "Medications in the past 6 months",
        "Maternal Adverse Events during pregnancy",
        "Maternal Adverse Events during previous pregnancy",
        "Season",
        "Socioeconomic status",
        "Ethnicity",
        "Nationallity",
        "Birth continent",
        "Pregnancy start period"
      ),
      labels = c(
        "Number\npregnancies", 
        "Number\nsubjects",
        "Maternal age", 
        "Maternal age\ngroup",
        "Trimester",
        "Pregnancy length\n(days)",
        # "Smoking status",
        "Comorbidities\n(prior year)",
        "Comorbidities\n(any-time before)",
        "Medications\n(prior 6 months)",
        "Maternal Adverse Events\n(during pregnancy)",
        "Maternal Adverse Events\n(prior pregnancy)",
        "Season",
        "Socioeconomic status",
        "Ethnicity",
        "Nationallity",
        "Birth continent",
        "Pregnancy start period"
      )
    ),
    variable_level = factor(
      variable_level,
      levels = c(
        "12 to 17", "18 to 34", "35 to 55", "-",
        
        paste0("Trimester ", 1:3), "Postpartum",
        
        "No", "Yes", "No smoker", "Smoker", "Never smoker", "Former smoker", "Current some day smoker", "Missing",          
        
        "Obesity", "Depression", "Anxiety", "Alcohol misuse dependence",
        
        "Asthma", "Diabetes", "Thyroid disorder",
        "Essential hypertension",
        "Systemic lupus erythematosus", "Hiv",
        "Uterus malformations", "Polycystic ovary syndrome",
        "Chronic viral hepatitis", "Inflammatory bowel disease",
        
        "Antidepressants", "Antiinflammatory antirheumatic", "Nsaids",
        "Opioids", "Treatment acid related disorder", "Antiepileptics",
        "Diabetes treatments", "Antithrombotics", "Immunosupressants",
        "Corticosteroids",
        
        maeCode,
        
        "Autumn", "Spring", "Summer", "Winter",
        
        "Q1", "Q2", "Q3", "Q4", "Q5", "U1", "U2", "U3", "U4", "U5", "R", "0n", 
        
        "White", "Asian", "Black", 
        
        "Asia", "Central/south america", "Europe", "Africa", "North america", "Oceania", "Missing",
        
        "Pre covid-19", "Covid-19 main outbreak", "Post covid-19 main outbreak"
      ),
      labels = c(
        "12 to 17", "18 to 34", "35 to 55", "-",
        
        paste0("Trimester ", 1:3), "Postpartum",
        
        "No", "Yes", "No", "Yes", "No", "Former smoker", "Yes", "Missing",   
        
        "Obesity", "Depression", "Anxiety", "Alcohol misuse\ndependence",
        
        "Asthma", "Diabetes", "Thyroid disorder",
        "Essential\nhypertension",
        "SLE", "HIV",
        "Uterus malformations", "PCOS",
        "Chronic viral hepatitis", "Inflammatory bowel disease",
        
        "Antidepressants", "Antiinflammatory/\nantirheumatic", "NSAIDs",
        "Opioids", "Acid-related disorder\ntreatments", "Antiepileptics",
        "Diabetes treatments", "Antithrombotics", "Immunosupressants",
        "Corticosteroids",
        
        maeClean,
        
        "Winter", "Autumn", "Spring", "Summer",
        
        "Q1", "Q2", "Q3", "Q4", "Q5", "U1", "U2", "U3", "U4", "U5", "R", "0n", 
        
        "White", "Asian", "Black", 
        "Asia", "Central/south america", "Europe", "Africa", "North america", "Oceania", "Missing",
        
        "Pre COVID-19", "COVID-19 main outbreak", "Post COVID-19 main outbreak"
      )
    )
  ) |>
  filter(!(cohort_name == "stillbirth" & variable_level == "Preterm delivery" & variable_name == 'Maternal Adverse Events\n(during pregnancy)')) |>
  mutate(
    smd = case_when(
      is.na(percentage_matched) | is.na(percentage_sampled) ~ NA,
      percentage_matched == percentage_sampled ~ 0,
      .default = (percentage_matched/100 - percentage_sampled/100) / sqrt((percentage_sampled/100*(1-percentage_sampled/100) + percentage_matched/100*(1-percentage_matched/100))/2)
    ),
    cohort_name = factor(cohort_name, levels = c(maeCode, aesiCode), labels = c(maeClean, aesiClean))
  )

colors <- c("#C94A5A", "#4A6FE3", "#4FAE6A", "#E08A3C")
smd |>
  # Focus on absolute SMD and sort by magnitude within each group
  # mutate(variable_level = reorder(variable_level, abs(smd))) |> 
  ggplot(aes(x = smd, y = variable_level, colour = cdm_name)) +
  geom_segment(aes(yend = variable_level, xend = 0), color = "grey70") + # Add lines for readability
  geom_point(size = 2.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # The balance threshold
  facet_grid(variable_name ~ cohort_name, scales = "free_y", space = "free", labeller = label_wrap_gen(width = 15)) + 
  labs(x = "Absolute Standardized Mean Difference", y = NULL) +
  # theme_minimal() +
  # theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  guides(
    color = guide_legend(title = "Data source"),
    fill  = guide_legend(title = "Data source")
  ) +
  # ggplot2::theme_linedraw() +
  theme(
    # strip.background = element_part_rect(colour = "grey70"),
    strip.text = element_text(face = "bold", size = 10, colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  ) 