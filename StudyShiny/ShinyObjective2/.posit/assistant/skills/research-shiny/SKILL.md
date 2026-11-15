---
name: research-shiny
description: Build or refine research-results Shiny applications in R using bslib, reactive modules, accessible controls, and downloadable results. Use when a user requests an interactive research dashboard, Shiny app, or results explorer.
---

# Research Shiny

## Workflow
1. Inspect existing result objects, data dictionaries, and scripts; identify stable inputs and output units. Do not re-estimate models unless requested.
2. Sketch the audience, questions, panels, filters, and privacy constraints before implementation.
3. Build with `shiny`, `bslib`, `ggplot2`, `gt`, and optional modules. Keep analytical transforms in standalone R functions and UI/reactivity in app code.
4. Prefer `bslib::page_sidebar()` and cards for modern dashboards; use accessible labels, keyboard-friendly controls, and responsive layouts.
5. Validate filters, empty selections, missing values, and all downloads. Never expose row-level confidential data or secrets by default.
6. Provide `app.R`, a README, and instructions to run with `shiny::runApp()`. Do not claim the app works without running it.

## Output contract
- Present results with units, population, period, N, uncertainty, and methods where available.
- Offer an accessible color-blind-friendly palette; do not encode differences only by color.
- Use a shared visual style with figures and Quarto; distinguish exploratory interactions from fixed reported estimates.
- Export figure PNG/SVG/PDF and table CSV when appropriate; avoid recalculating published estimates inside reactive expressions.
- Keep deployment optional and obtain approval before publishing data or apps.

## Templates
See `references/shiny-pattern.md` for a compact app scaffold.

## OMOPverse postprocessing
- Inspect available aggregate results and `visOmopResults` visualisation patterns before proposing bespoke plotting code.
- Never connect the app directly to patient-level OMOP CDM tables by default. Propose precomputed, disclosure-reviewed aggregates and explicit suppression handling.
- Preserve cohort names/IDs, strata, denominators and uncertainty in filters and download labels. Never allow filters to reveal suppressed small cells.
- Present proposed app architecture, dependencies, data contract and file diff; wait for approval before creating files, running Shiny, or publishing.

## Approval protocol
Before any mutation, execution, render, export, or Git action, present a specific plan and wait for explicit user approval. Read-only inspection and drafting proposals are allowed.

## Visualization choices (required)
- Offer both **ggplot2** (static, reproducible, publication-ready) and **plotly** (interactive hover, zoom, selection) for applicable charts. Explain trade-offs and ask the researcher which to use before implementing.
- Prefer a single ggplot2 specification and `plotly::ggplotly()` when it preserves semantics; if not, propose a separate native plotly implementation. Flag differences in legends, faceting, annotations, confidence intervals and accessibility.
- For tables, offer **gt** for polished compact summaries and **DT::datatable** for large, searchable, sortable, paginated tables. Prefer server-side processing for large data; do not expose row-level OMOP patient data.
- Do not imply `gt` provides the same large-data interaction as DT. Ask approval before adding packages or changing UI.
