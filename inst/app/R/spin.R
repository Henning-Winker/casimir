# Step 7: Spin x1000 (Spin, The spin, Results, Over the years, Betting slip)

spin_ui <- function() {
  tagList(
    uiOutput("sp_empty"),
    tabsetPanel(
      id = "sp_tabs",
      tabPanel("Spin",
        box(width = 12, title = "Repeat the survey many times on the same island",
          div(class = "infobox", "Each spin draws a new sample with the same design (and artefacts) and raises it. Comparing the estimates with the true catch shows the bias (the house edge), the precision (CV) and how often the 95% intervals contain the truth. ", tip("house_edge")),
          fluidRow(
            column(4, sliderInput("sp_R", "Spins", 50, 1000, 200, 50)),
            column(4, br(), actionButton("sp_go", "Spin", class = "btn-primary", style = "height:40px; width:160px; font-weight:600;")),
            column(4, br(), actionButton("sp_save", "Keep this design for comparison"))),
          uiOutput("sp_note"))),
      tabPanel("The spin",
        box(width = 12, title = "Every survey, around the truth",
          plotOutput("sp_violin", height = 620),
          p(class = "note", "Each spin is one survey with the same design. The line is the true catch; the spread is the precision, the shift of the box from the line the bias (house edge)."))),
      tabPanel("Results",
        box(width = 12, title = "Bias, CV and coverage by species",
          plotOutput("sp_plot", height = 520),
          tableOutput("sp_tab"),
          p(class = "note", "Coverage: share of 95% intervals that contain the true catch. ", tip("coverage")))),
      tabPanel("Over the years",
        box(width = 12, title = "Spin the whole programme: estimates against the truth, year by year",
          div(class = "infobox", "Each spin rolls and raises the design in every survey year (the years, route and frame of step 6). ",
              "The band holds the middle 95% of the estimates; with Hide truth unticked, the true catch or effort is drawn on top."),
          fluidRow(
            column(3, sliderInput("sy_R", "Spins", 20, 300, 100, 10)),
            column(3, radioButtons("sy_what", "Show", c("Catch by species" = "catch", "Effort by segment" = "effort"))),
            column(3, radioButtons("sy_type", "As", c("Band and line" = "band", "Paired bars" = "bars"))),
            column(3, br(), actionButton("sy_go", "Spin the series", class = "btn-primary",
                                         style = "height:40px; font-weight:600;"))),
          uiOutput("sy_note"),
          plotOutput("sy_plot", height = 560),
          tableOutput("sy_tab"))),
      tabPanel("Betting slip",
        box(width = 12, title = "Your bet against what happened",
          uiOutput("sp_slip")))))
}

spin_server <- function(input, output, session, rv, r) {
  output$sp_empty <- renderUI(if (!r$fish_current()) empty_msg("Build the island first (step 1)."))
  observeEvent(input$sp_go, {
    req(r$fish_current())
    R <- input$sp_R
    d <- r$design()
    withProgress(message = paste("Spinning", R, "times"), value = 0, {
      ev <- replicate_design(rv$fishery, d, R = R, artefacts = r$artefacts(),
                             route = input$rs_route %||% "psu", seed = input$is_seed %||% 1,
                             progress = function(i) if (i %% 10 == 0) incProgress(10 / R))
    })
    ev$label <- d$name
    rv$evals[[d$name]] <- ev
    updateTabsetPanel(session, "sp_tabs", "The spin")
  })
  observeEvent(input$sy_go, {
    req(r$fish_current())
    R <- input$sy_R
    yrs <- utils::tail(rv$fishery$years, input$sr_years %||% 5)
    route <- input$sr_route %||% "psu"
    frame <- if (identical(route, "effort")) input$sr_frame %||% "annual" else "annual"
    tryCatch(
      withProgress(message = paste("Spinning the", length(yrs), "year programme", R, "times"), value = 0, {
        rv$spins <- spin_series(rv$fishery, r$design(), years = yrs, R = R, artefacts = r$artefacts(),
                                frame = frame, route = route, seed = input$is_seed %||% 1,
                                progress = function(i) if (i %% 5 == 0) incProgress(5 / R))
      }),
      error = function(e) showNotification(paste("The spins failed:", conditionMessage(e)),
                                           type = "error", duration = NULL))
  })
  output$sy_note <- renderUI({
    s <- rv$spins
    if (is.null(s)) return(p(class = "note", "About 1 second per 7 spins of a five-year programme."))
    p(class = "note", sprintf("Design %s, %d spins, years %d-%d, route %s, frame %s.", s$design$name, s$R,
                              min(s$years), max(s$years), s$route, s$frame))
  })
  output$sy_plot <- render_plot({
    req(rv$spins)
    plot_spin_series(rv$spins, what = input$sy_what %||% "catch", type = input$sy_type %||% "band",
                     show_truth = r$show_truth())
  })
  output$sy_tab <- renderTable({
    req(rv$spins)
    w <- input$sy_what %||% "catch"
    s <- summary_spins(rv$spins, w, by = if (w == "effort") "segment")
    sc <- if (w == "catch") 1000 else 1
    out <- data.frame(s[intersect(c("species", "segment"), names(s))], year = as.integer(s$year),
                      mean = s$mean / sc, `2.5%` = s$q025 / sc, `97.5%` = s$q975 / sc, check.names = FALSE)
    if (r$show_truth()) {
      out$true <- s$true / sc
      out$`bias %` <- 100 * s$rel_bias
      out$coverage <- s$coverage
    }
    names(out)[names(out) == "mean"] <- if (w == "catch") "mean (t)" else "mean (trips)"
    out
  }, digits = 1)
  output$sp_note <- renderUI({
    if (!length(rv$evals)) return(p(class = "note", "About 1 second per 15 spins."))
    p(class = "note", "Designs spun so far: ", paste(names(rv$evals), collapse = ", "),
      ". Change the design name in step 3 to keep several designs.")
  })
  table_all <- reactive({
    req(length(rv$evals))
    do.call(rbind, lapply(rv$evals, function(e) data.frame(design = e$label, cost = e$cost, e$summary)))
  })
  output$sp_plot <- render_plot({
    s <- table_all()
    if (!r$show_truth()) {
      plot.new(); text(0.5, 0.5, "Hidden while Hide truth is ticked: bias and coverage need the true catch.")
      return()
    }
    plot_eval(s)
  })
  output$sp_violin <- render_plot({
    req(length(rv$evals))
    if (!r$show_truth()) {
      plot.new(); text(0.5, 0.5, "Hidden while Hide truth is ticked: the spin is shown against the true catch.")
      return()
    }
    plot_spin(rv$evals)
  })
  output$sp_tab <- renderTable({
    s <- table_all()
    if (!r$show_truth()) return(data.frame(design = s$design, species = s$species, `CV %` = 100 * s$mean_se / pmax(s$mean, 1e-9), check.names = FALSE))
    data.frame(design = s$design, cost = s$cost, species = s$species, `true (t)` = s$true / 1000,
               `bias %` = 100 * s$rel_bias, `CV %` = 100 * s$cv, coverage = s$coverage, check.names = FALSE)
  }, digits = 1)
  output$sp_slip <- renderUI({
    s <- rv$slip
    if (is.null(s)) return(empty_msg("No bet placed: fill in the betting slip in step 3 before you spin."))
    ev <- rv$evals[[s$design]]
    if (is.null(ev)) return(empty_msg(paste0("Spin design ", s$design, " to settle the bet.")))
    if (!r$show_truth()) return(tagList(
      div(class = "predictbox", tags$strong("Your bet: "), s$species, ", CV ", round(100 * s$cv), "%, bias: ", s$bias),
      hidden_msg()))
    x <- ev$summary[ev$summary$species == s$species, ]
    bias_real <- if (abs(x$rel_bias) < 2 * x$cv / sqrt(ev$R) + 0.02) "none" else if (x$rel_bias > 0) "high" else "low"
    tagList(
      div(class = "predictbox", tags$strong("Your bet: "), s$species, ", CV ", round(100 * s$cv), "%, bias: ", s$bias),
      div(class = if (abs(x$cv - s$cv) < 0.25 * x$cv && bias_real == s$bias) "infobox" else "flagbox",
          tags$strong("What happened: "), "CV ", round(100 * x$cv), "%, bias ", sprintf("%+.0f%%", 100 * x$rel_bias),
          " (", bias_real, "), over ", ev$R, " spins."),
      p(class = "note", casimir_tip("days_vs_boats")))
  })
}
