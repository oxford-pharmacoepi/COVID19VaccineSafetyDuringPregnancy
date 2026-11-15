# Shared figure helpers; source this file from a project script.
research_theme <- function(mode = c("journal", "presentation")) {
  mode <- match.arg(mode)
  size <- if (mode == "journal") 11 else 16
  ggplot2::theme_minimal(base_size = size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "bottom",
      axis.title = ggplot2::element_text(face = "plain")
    )
}
export_figure <- function(plot, stem, width = 7, height = 5) {
  ggplot2::ggsave(paste0(stem, ".pdf"), plot, width = width, height = height, units = "in")
  ggplot2::ggsave(paste0(stem, ".svg"), plot, width = width, height = height, units = "in")
  ggplot2::ggsave(paste0(stem, ".png"), plot, width = width, height = height, units = "in", dpi = 300)
}
