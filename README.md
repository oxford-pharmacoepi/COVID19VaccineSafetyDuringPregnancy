# COVID19 Vaccine Safety During Pregnancy

Safety of COVID-19 mRNA vaccines administered during pregnancy: a target trial emulation across four European data sources (CPRD, SIDIAP, NLHR@UiO, SCIFI-PEARL), all mapped to the OMOP Common Data Model.

The study protocol is registered in the EMA Catalogue of Real-World Evidence: **EUPAS1000000712**.

## Repository contents

- **`Diagnostics/`** — phenotype diagnostics for the study cohorts and outcomes. Open `Diagnostics.Rproj`, restore packages with `renv::restore()`, open `CodeToRun.R` to enter database details and run the script.
- **`Study/`** — the main study code: cohort instantiation, risk-set sampling and matching, propensity-score weighting, and outcome models. Open the project, restore with `renv::restore()`, set your database details in `CodeToRun.R`, and run it.
- **`DiagnosticsShiny/`** — Shiny app to explore the diagnostics results. Open `shiny.Rproj` and run the app (data in `data/raw/`).
- **`StudyShiny/`** — Shiny app to explore the diagnostics results. Open `shiny.Rproj` and run the app (data in `data/`).

