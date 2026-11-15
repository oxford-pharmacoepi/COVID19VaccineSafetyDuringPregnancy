---
name: publication-figures
description: Produce journal-ready and presentation-ready research figures in R using ggplot2, patchwork, accessible palettes, consistent styling, and reproducible PNG/SVG/PDF exports. Use when designing, revising, or exporting scientific figures.
---

# Publication Figures

## Workflow
1. Inspect source results and their units, uncertainty, groups, denominators, and scientific message.
2. Select the least misleading chart type; show uncertainty when supported and disclose transformations and scales.
3. Build using `ggplot2`, `patchwork` when needed, and a shared reusable theme. Prefer direct labels and readable legends.
4. Check accessibility: color-blind-safe colors, contrast, line types, text sizes, and alt text.
5. Export publication PDF/SVG (vector) and high-resolution PNG; use explicit physical dimensions and verify clipping and legibility.
6. Provide code and a descriptive caption including sample, estimates, and uncertainty definition.

## Two style modes
- Journal: restrained, minimal, grayscale-friendly, no decorative 3D effects.
- Presentation: larger labels, higher contrast, more annotation, same scientific values.

## Scientific integrity
- Never truncate axes in a misleading way; justify log scales and zero baselines when relevant.
- Do not add confidence intervals or significance symbols unless supported by source data.
- Never alter model results to improve appearance.

## Templates
See `references/figure-style.R` for theme and export helper.

## OMOPverse postprocessing
- Inspect OMOPverse result class and existing `visOmopResults` plotting functions first; only use `ggplot2` customization where compatible and justified.
- Preserve stratification, cohort ordering, denominators, confidence intervals and disclosure suppression; never reverse-engineer missing values or display suppressed cells.
- Show the figure specification, exact R code and proposed output filenames before editing or exporting. Require approval before execution.

## Approval protocol
Before any mutation, execution, render, export, or Git action, present a specific plan and wait for explicit user approval. Read-only inspection and drafting proposals are allowed.

## Figure defaults
- **ggplot2 is the default** for reproducible publication figures and Quarto reports. Offer plotly as an optional interactive counterpart for Shiny or exploratory presentations, not as a replacement for publication exports.
- Provide a choice of journal-minimal and presentation-modern themes; do not alter scales, limits, transformations, denominators or uncertainty intervals without approval.
- Propose PDF/SVG vector exports and PNG raster exports with dimensions, fonts and resolution stated before execution.
