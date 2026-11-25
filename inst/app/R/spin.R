# Step 6: Spin x1000 (Run, Compare, Betting slip)

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
