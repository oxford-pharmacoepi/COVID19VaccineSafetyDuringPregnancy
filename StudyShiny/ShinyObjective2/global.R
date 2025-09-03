library(bslib)
library(CohortCharacteristics)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(DT)
library(ggplot2)
library(gt)
library(jsonlite)
library(markdown)
library(omopgenerics)
library(OmopSketch)
library(plotly)
library(purrr)
library(reactable)
library(readr)
library(rlang)
library(rsvg)
library(shiny)
library(shiny.fluent)
library(shinycssloaders)
library(shinyWidgets)
library(sortable)
library(tidyr)
library(visOmopResults)
library(yaml)

# preprocess data if it has not been done
fileData <- file.path(getwd(), "data", "shinyData.RData")
if (!file.exists(fileData)) {
  source(file.path(getwd(), "data", "preprocess.R"))
}

# uncomment to load the raw data
# rawData <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

# load shiny data
load(fileData)

# source functions
source(file.path(getwd(), "functions.R"))
