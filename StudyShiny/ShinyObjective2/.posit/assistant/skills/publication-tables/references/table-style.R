# Apply to a verified gt table. Use explicit units, N, and footnotes upstream.
research_gt_style <- function(tbl, mode = c("journal", "presentation")) {
  mode <- match.arg(mode)
  font_size <- if (mode == "journal") 11 else 14
  tbl |>
    gt::tab_options(
      table.font.size = gt::px(font_size),
      data_row.padding = gt::px(if (mode == "journal") 4 else 8),
      table.border.top.width = gt::px(1),
      table.border.bottom.width = gt::px(1)
    )
}
# Example: gt::gt(data.frame(metric = "N", value = 100)) |> research_gt_style()
