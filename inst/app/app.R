# casimir Shiny app: entry point. Start it with casimir::run_app().
# Shiny loads every file in the R/ folder next to this file before it runs,
# so the functions defined there (one file per step) are available here.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(casimir)
options(casimir.cache = TRUE)  # reuse islands already built with the same settings

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "casimir", titleWidth = 280),
  app_sidebar(),
  app_body()
)

server <- function(input, output, session) {
  # rv holds the app's shared state: the fishery, the last sample and
  # estimate, the replicate runs and the trainee's betting slip
  rv <- reactiveValues(fishery = NULL, fish_snap = NULL, sample = NULL, sample_snap = NULL,
                       estimate = NULL, evals = list(), slip = NULL, saved_design = NULL,
                       rolls = 0, survey = NULL, spins = NULL)

  # r holds the reactive objects (settings, design, artefacts) used by
  # several steps
  r <- make_reactives(input, rv)

  run_server(input, output, session, rv, r)
  island_server(input, output, session, rv, r)
  frame_server(input, output, session, rv, r)
  design_server(input, output, session, rv, r)
  roll_server(input, output, session, rv, r)
  raise_server(input, output, session, rv, r)
  series_server(input, output, session, rv, r)
  spin_server(input, output, session, rv, r)
  export_server(input, output, session, rv, r)
  truth_server(input, output, session, rv, r)
}

shinyApp(ui, server)
