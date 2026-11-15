---
name: publication-tables
description: Build polished descriptive, regression, and model-comparison research tables in R using gt, gtsummary, and modelsummary with reliable labels, rounding, notes, and Word/PDF-friendly output. Use for publication tables and research results exports.
---

# Publication Tables

## Workflow
1. Inspect actual result objects, variable labels, units, model specifications, and sample sizes.
2. Choose `gt` for bespoke summary tables, `gtsummary` for descriptive comparisons, and `modelsummary` for regression summaries.
3. Use informative column headers and footnotes; distinguish standard errors, confidence intervals, and p-values.
4. Apply consistent decimal rules, alignment, missing-data labels, and explicit denominators. Avoid false precision.
5. Export Word-friendly tables and PDF/HTML or image outputs according to installed package capabilities; test the rendered result.
6. Compare displayed values against source objects and document any formatting transformations.

## Guardrails
- Never fabricate cells or significance marks; never convert missing to zero.
- Keep estimates, units, N, model controls, and uncertainty definitions visible.
- Explain if a requested export requires additional tools or packages.
- Ask before changing analysis objects or rerunning estimation.

## Templates
See `references/table-style.R` for a gt formatting helper.

## OMOPverse postprocessing
- Check whether the source uses `omopgenerics` summarised-result conventions and whether `visOmopResults` supports the required table directly.
- Preserve result types, groups, strata, cohort identifiers, denominators, missingness and suppression rules. Never fill suppressed cells or convert missing to zero.
- Prefer `visOmopResults` for supported OMOP result presentations, with `gt`, `gtsummary`, or `modelsummary` when appropriate for the actual input class.
- Propose the exact table layout and code first; require approval before writing or exporting files.

## Approval protocol
Before any mutation, execution, render, export, or Git action, present a specific plan and wait for explicit user approval. Read-only inspection and drafting proposals are allowed.

## Table choices by context
- **flextable** is the first choice for Quarto Word reports; evaluate PDF compatibility before proposing it for PDF.
- **gt** is the default for polished, compact standalone or Shiny summary tables when appropriate.
- **DT::datatable** is the preferred interactive option for large Shiny tables needing search, sorting, pagination and column filters. For very large datasets, propose server-side filtering/pagination rather than shipping all records to the browser.
- Offer gt versus DT explicitly when a Shiny table is requested; describe the trade-off and await approval.
- Preserve OMOPverse result types, strata, counts, suppression rules, estimates and uncertainty. Never unsuppress small cells or expose row-level patient data.
