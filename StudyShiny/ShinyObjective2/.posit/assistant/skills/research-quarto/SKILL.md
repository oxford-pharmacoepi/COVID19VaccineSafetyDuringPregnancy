---
name: research-quarto
description: Turn finalized R research results into reproducible Quarto PDF and Word reports with cross-references, citations, and polished figures/tables. Use for research reports, manuscript drafts, and results postprocessing.
---

# Research Quarto

## Workflow
1. Inventory final analysis outputs, references, captions, and target journal or institutional template.
2. Create a `.qmd` that reads saved results or sources controlled by the user; do not silently change model definitions or analysis samples.
3. Use Quarto figure/table labels and cross-references, meaningful alt text, clear captions, and explicit N/units/uncertainty.
4. Target PDF and docx via YAML formats; preserve manuscript-friendly structure and reproducible relative paths.
5. Render each requested format where Quarto and required dependencies are available; otherwise report exact prerequisites and commands.
6. Inspect output for broken references, clipped figures, floating tables, font issues, and misleading rounding.

## Rules
- Never fabricate citations, statistics, p-values, confidence intervals, or findings.
- Separate results from interpretation, label exploratory analyses, and retain data provenance.
- Prefer `renv` for dependency reproducibility when already in project; do not install packages without approval.
- Keep intermediate output and generated files out of Git unless the user requests versioned artifacts.

## Templates
See `references/report.qmd` for a dual-format starting point.

## OMOPverse postprocessing
- Prefer finalized aggregate OMOPverse result objects and existing `visOmopResults` outputs; do not re-run cohort construction or statistical estimation by default.
- Preserve cohort definitions, source/result provenance, disclosure notes and denominators in report captions and appendices.
- Propose report outline and exact changes before writing `.qmd` or rendering PDF/Word. Treat render as a separately approved action.

## Approval protocol
Before any mutation, execution, render, export, or Git action, present a specific plan and wait for explicit user approval. Read-only inspection and drafting proposals are allowed.

## Preferred report outputs
- Prioritize **ggplot2** for all report figures; default to static figures that render consistently in PDF and Word. Do not convert to plotly unless explicitly requested and compatible with output format.
- Prioritize **flextable** for report tables, especially Word (`docx`) outputs. Check PDF compatibility and rendering prerequisites; propose `gt` or LaTeX-compatible alternatives only when flextable cannot satisfy the target output, and ask approval first.
- Produce PDF and Word from a shared `.qmd` when feasible, with captions, cross-references, accessibility text, and no recomputation of finalized OMOPverse estimates.
- Inspect installed Quarto and required PDF engine availability; never install or render without explicit approval.
