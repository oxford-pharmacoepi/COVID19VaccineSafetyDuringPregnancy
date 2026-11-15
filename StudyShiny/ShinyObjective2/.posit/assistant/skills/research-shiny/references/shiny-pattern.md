# Minimal Shiny pattern

```r
library(shiny)
library(bslib)
library(ggplot2)

# Replace `mtcars` with a vetted, non-sensitive results table.
ui <- page_sidebar(
  title = "Research results explorer",
  sidebar = sidebar(selectInput("cyl", "Cylinders", choices = sort(unique(mtcars$cyl)))),
  card(plotOutput("figure"), downloadButton("download", "Download PNG"))
)
server <- function(input, output, session) {
  plot_data <- reactive(subset(mtcars, cyl == as.numeric(input$cyl)))
  make_plot <- function() ggplot(plot_data(), aes(wt, mpg)) + geom_point() + labs(x = "Weight (1000 lbs)", y = "Miles per gallon") + theme_minimal(base_size = 13)
  output$figure <- renderPlot(make_plot())
  output$download <- downloadHandler(filename = function() "figure.png", content = function(file) ggsave(file, plot = make_plot(), width = 7, height = 5, dpi = 300))
}
shinyApp(ui, server)
```

Replace demonstration data with saved aggregate results, validate controls and missingness, and test downloads before delivery.
