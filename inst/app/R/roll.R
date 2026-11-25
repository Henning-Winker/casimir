# Step 4: Roll (Visits, Form A, Form B, Form C, Artefacts)

roll_ui <- function() {
  tagList(
    uiOutput("rl_empty"),
    tabsetPanel(
      id = "rl_tabs",
      tabPanel("Visits",
        box(width = 12, title = "Week × site grid: the site-days visited",
          fluidRow(
            column(4, selectInput("rl_type", "Site type", site_types)),
            column(4, selectInput("rl_q", "Quarter", 1:4))),
          plotOutput("rl_grid", height = 420),
          p(class = "note", "As in the training deck: the frame (all fishing weeks × sites of one type) and the sample (X)."))),
      tabPanel("How much we saw",
        box(width = 12, title = "The census and the sample",
          plotOutput("rl_funnel", height = 260),
          p(class = "note", "Each stage samples a small part of the one above; raising multiplies back up."))),
      tabPanel("Where they fished",
        box(width = 12, title = "Fishing grounds of the sampled landings",
          plotOutput("rl_map", height = 600))),
      tabPanel("Form A: visits",
        box(width = 12, title = "Form A: landings counted at each visit, by segment",
          p(class = "note", "One row per site-day and segment. N = landings counted; boats = boats of that segment based at the site, counted at the visit."),
          tableOutput("rl_A"))),
      tabPanel("Form B: landings",
        box(width = 12, title = "Form B: sampled landings",
          p(class = "note", "W = total landed weight (kg), w = subsample weight (kg), hours = hours fished."),
          tableOutput("rl_B"))),
      tabPanel("Form C: species",
        box(width = 12, title = "Form C: species weights in each subsample",
          tableOutput("rl_C"))),
      tabPanel("Artefacts (trainer)",
        conditionalPanel("input.hide_truth", hidden_msg()),
        conditionalPanel("!input.hide_truth",
          box(width = 12, title = "Plant field problems",
            div(class = "flagbox", "Switch on problems that real surveys have. They change the next Roll and are listed in the answer key. Trainees with Hide truth ticked do not see this page."),
            fluidRow(
              column(4, sliderInput("af_high", lab("High roller: picks big boats", "high_roller"), 0, 1, 0, 0.1)),
              column(4, checkboxInput("af_watch_on", lab("Short watch", "short_watch"), FALSE),
                     sliderInput("af_watch", "Enumerator leaves at (hour)", 8, 20, 13, 1)),
              column(4, sliderInput("af_top", lab("Top of the box", "top_of_box"), 0, 1, 0, 0.1))),
            fluidRow(
              column(4, checkboxInput("af_zeros", lab("Dropped zeros", "dropped_zeros"), FALSE)),
              column(4, checkboxInput("af_sites_all", "Visit all sites", TRUE),
                     checkboxGroupInput("af_sites", lab("Easy sites only", "easy_sites"),
                                        paste0("LS", 1:9), paste0("LS", 1:9), inline = TRUE)),
              column(4, sliderInput("af_frame", lab("Old frame (years)", "old_frame"), 0, 3, 0, 1)))))))
  )
}

roll_server <- function(input, output, session, rv, r) {
  output$rl_empty <- renderUI({
    if (is.null(rv$sample)) empty_msg("No sample yet: set the design (step 3) and press Roll in the left panel.")
    else if (!r$sample_current()) div(class = "warnbox", "Settings changed since this sample was drawn. Press Roll again.")
  })
  output$rl_grid <- render_plot({
    req(rv$sample)
    plot_design_grid(rv$sample, input$rl_type, as.integer(input$rl_q))
  })
  output$rl_funnel <- render_plot({ req(rv$sample); plot_funnel(rv$sample, rv$fishery) })
  output$rl_map <- render_plot({ req(rv$sample); plot_sample_map(rv$sample, rv$fishery) })
  fmt <- function(d) { d$date <- format(d$date); d }
  output$rl_A <- renderTable({ req(rv$sample); fmt(rv$sample$A) }, digits = 0)
  output$rl_B <- renderTable({ req(rv$sample); rv$sample$B }, digits = 1)
  output$rl_C <- renderTable({ req(rv$sample); rv$sample$C }, digits = 2)
}
