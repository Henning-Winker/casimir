# Step 6: Series (catch and effort over the survey years, against the truth)

series_ui <- function() {
  tagList(
    uiOutput("sr_empty"),
    box(width = 12, title = "A sampling programme over the final years",
      div(class = "infobox",
          "The design of step 3 is rolled and raised once in each survey year. The plots compare the estimated catch ",
          "by species and the estimated effort (landings, i.e. fishing trips) by segment with the truth. ",
          "To see the spread over many programmes, use ", tags$strong("Spin ×1000: Over the years"), "."),
      fluidRow(
        column(3, sliderInput("sr_years", "Survey years (the last ones)", 3, 10, 5, 1)),
        column(4, radioButtons("sr_route", lab("Route", "route"),
                               stats::setNames(c("psu", "effort"), c("Raise by site-days (PSU)", "CPUE × effort")))),
        column(3, conditionalPanel("input.sr_route == 'effort'",
          radioButtons("sr_frame", lab("Frame survey (boats per site)", "frame_year"),
                       c("New every year" = "annual", "First survey year only" = "first")))),
        column(2, br(), actionButton("sr_go", "Roll the series", class = "btn-primary",
                                     style = "height:40px; font-weight:600;"))),
      uiOutput("sr_frame_note")),
    tabsetPanel(
      id = "sr_tabs",
      tabPanel("Catch",
        box(width = 12, title = "Estimated against true catch by species",
          fluidRow(
            column(4, radioButtons("sr_by", "Show", inline = TRUE,
                                   c("Species totals" = "none", "By segment" = "segment", "By site type" = "site_type"))),
            column(8, uiOutput("sr_sp_ui"))),
          plotOutput("sr_catch", height = 560))),
      tabPanel("Effort",
        box(width = 12, title = "Estimated against true effort by segment",
          plotOutput("sr_effort", height = 380),
          p(class = "note", "Effort is the number of landings (fishing trips). Site-days route: M/m × the landings counted on Form A. ",
            "CPUE × effort route: frame boats × fishing days × landings per boat and day."))),
      tabPanel("Table",
        box(width = 12, title = "Catch and effort by year",
          tableOutput("sr_tab")))))
}

series_server <- function(input, output, session, rv, r) {
  output$sr_empty <- renderUI(if (!r$fish_current()) empty_msg("Build the island first (step 1)."))
  run <- function() {
    if (!r$fish_current()) {
      showNotification("Build the island first (step 1).", type = "warning")
      return()
    }
    yrs <- utils::tail(rv$fishery$years, input$sr_years %||% 5)
    frame <- if (identical(input$sr_route, "effort")) input$sr_frame %||% "annual" else "annual"
    tryCatch(
      withProgress(message = paste("Rolling", length(yrs), "years"), value = 0.5, {
        rv$survey <- run_survey(rv$fishery, r$design(), years = yrs, artefacts = r$artefacts(),
                                frame = frame, route = input$sr_route %||% "psu",
                                seed = seed_for(input$is_seed %||% 1, paste0("series", rv$rolls)))
      }),
      error = function(e) showNotification(paste("The series could not be rolled:", conditionMessage(e)),
                                           type = "error", duration = NULL))
  }
  observeEvent(input$sr_go, run())
  # once a series exists, changing its settings redraws it
  observeEvent(list(input$sr_years, input$sr_route, input$sr_frame), {
    if (!is.null(rv$survey) && r$fish_current()) run()
  }, ignoreInit = TRUE)

  output$sr_frame_note <- renderUI({
    if (!identical(input$sr_route, "effort")) {
      return(p(class = "note", "The frame survey (boats per site) only enters the CPUE × effort route; choose that route to see what an old frame does."))
    }
    if (identical(input$sr_frame, "first"))
      div(class = "warnbox", "The boats counted in the first survey year are used every year. Boats that joined since are missed, ",
          "so effort and catch are underestimated more each year as the fleet grows (Island tab: boats joining).")
    else p(class = "note", "A new frame survey every year counts the boats of that year.")
  })
  output$sr_sp_ui <- renderUI({
    req(rv$fishery)
    if (identical(input$sr_by %||% "none", "none")) return(NULL)
    selectizeInput("sr_sp", "Species", rv$fishery$species, multiple = TRUE,
                   selected = c("Scomber colias", "Thunnus alalunga", "Merluccius merluccius"))
  })
  output$sr_catch <- render_plot({
    req(rv$survey)
    by <- if (identical(input$sr_by %||% "none", "none")) NULL else input$sr_by
    plot_series(rv$survey, if (r$show_truth()) rv$fishery, by = by,
                species = if (is.null(by)) NULL else input$sr_sp)
  })
  output$sr_effort <- render_plot({
    req(rv$survey)
    plot_effort_series(rv$survey, if (r$show_truth()) rv$fishery)
  })
  output$sr_tab <- renderTable({
    req(rv$survey)
    tru <- if (r$show_truth()) rv$fishery
    cb <- catch_by(rv$survey, fishery = tru)
    eb <- effort_by(rv$survey, fishery = tru)
    out <- rbind(
      data.frame(year = as.integer(cb$year), what = "catch (t)", group = cb$species, estimate = cb$estimate / 1000,
                 `CV %` = 100 * cb$cv, true = if (!is.null(tru)) cb$true / 1000 else NA, check.names = FALSE),
      data.frame(year = as.integer(eb$year), what = "effort (trips)", group = eb$segment, estimate = eb$estimate,
                 `CV %` = 100 * eb$cv, true = if (!is.null(tru)) eb$true else NA, check.names = FALSE))
    if (is.null(tru)) out$true <- NULL
    out[order(out$what, out$group, out$year), ]
  }, digits = 1)
}
