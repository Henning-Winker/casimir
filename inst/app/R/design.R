# Step 3: Design (Stages, Costs, Betting slip)

design_ui <- function() {
  tagList(uiOutput("ds_empty"), tabsetPanel(
    id = "ds_tabs",
    tabPanel("Stages",
      box(width = 12, title = span("Stage 1: site-days (PSUs)", tip("psu")),
        p("Strata are site type × quarter. In each stratum, choose how many site-days to visit (m). Every visit records all segments. ", tip("strata"), tip("m")),
        fluidRow(
          column(3, sliderInput("ds_m_Harbour", "Harbour: site-days per quarter", 1, 40, 6, 1)),
          column(3, sliderInput("ds_m_Beach", "Beach: site-days per quarter", 1, 40, 6, 1)),
          column(3, sliderInput("ds_m_Jetty", "Jetty: site-days per quarter", 1, 40, 6, 1)),
          column(3, radioButtons("ds_select", lab("Choose site-days", "psu_select"),
                                 c("At random" = "srs", "Spread over the quarter" = "systematic"))))),
      box(width = 12, title = span("Stage 2: landings at each visit", tip("n")),
        fluidRow(
          column(4, sliderInput("ds_n_canoe", "Canoe landings per visit", 1, 15, 3, 1)),
          column(4, sliderInput("ds_n_small_motor", "Small motor landings per visit", 1, 15, 3, 1)),
          column(4, sliderInput("ds_n_large_motor", "Large motor landings per visit", 1, 15, 3, 1))),
        p(class = "note", "The enumerator counts all landings of each segment (N) and samples n of them. If fewer than n land, all are sampled.")),
      box(width = 12, title = span("Stage 3: species subsample", tip("w_kg")),
        fluidRow(
          column(4, sliderInput("ds_w", "Subsample per landing (kg)", 1, 50, 10, 1)),
          column(4, textInput("ds_name", "Design name", "A")),
          column(4, uiOutput("ds_cost_box")))),
      box(width = 12, title = "Formulas used in step 5", class = "guide",
        div(class = "formula", "Step 1  landing:   W_ijs = w_ijs / w_ij × W_ij"),
        div(class = "formula", "Step 2  site-day:  W_is  = N_i / n_i × Σ_j W_ijs"),
        div(class = "formula", "Step 3  stratum:   W_hs  = M_h / m_h × Σ_i W_is"),
        div(class = "formula", "Step 4  total:     W_s   = Σ_h W_hs"),
        p(class = "note", casimir_tip("raise")))),
    tabPanel("Costs",
      box(width = 12, title = span("Cost model", tip("cost")),
        fluidRow(
          column(4, numericInput("ds_cost_psu", "Hours per site-day visit (travel, watch)", 4, 0, 24, 0.5)),
          column(4, numericInput("ds_cost_landing", "Hours per landing sampled", 0.5, 0, 5, 0.1)),
          column(4, numericInput("ds_cost_kg", "Hours per kg sorted", 0.05, 0, 1, 0.01))),
        tableOutput("ds_cost_tab"))),
    tabPanel("Betting slip",
      box(width = 12, title = span("Place your bet before you roll", tip("betting_slip")),
        div(class = "predictbox",
            "Predict how good this design will be. After Spin ×1000 (step 6) the app compares your bet with what happened."),
        fluidRow(
          column(4, uiOutput("bs_sp_ui")),
          column(4, sliderInput("bs_cv", "Predicted CV of its catch estimate (%)", 1, 100, 20, 1)),
          column(4, radioButtons("bs_bias", "Will the estimate be biased?",
                                 c("No" = "none", "Too high" = "high", "Too low" = "low")))),
        actionButton("bs_lock", "Lock in my bet", class = "btn-primary"),
        uiOutput("bs_locked")))))
}

design_server <- function(input, output, session, rv, r) {
  output$ds_empty <- renderUI(if (is.null(rv$fishery)) empty_msg("Build the island first (step 1): the design needs its landing sites and segments."))
  output$ds_cost_box <- renderUI({
    req(rv$fishery)
    stat_box("Planned cost (enumerator-hours per year)", round(design_cost(r$design(), fishery = rv$fishery)))
  })
  output$ds_cost_tab <- renderTable({
    req(rv$fishery)
    d <- r$design()
    data.frame(Item = c("Site-day visits", "Landings sampled (at most)", "Kg sorted (at most)", "Total hours"),
               Number = c(4 * sum(d$m), 4 * sum(d$m) * sum(d$n), 4 * sum(d$m) * sum(d$n) * d$w_kg, NA),
               Hours = c(4 * sum(d$m) * d$cost[["psu"]], 4 * sum(d$m) * sum(d$n) * d$cost[["landing"]],
                         4 * sum(d$m) * sum(d$n) * d$w_kg * d$cost[["kg"]],
                         design_cost(d, fishery = rv$fishery)))
  }, digits = 0, na = "")
  output$bs_sp_ui <- renderUI({
    sp <- rv$fishery$species %||% character(0)
    selectInput("bs_sp", "Species", choices = sp, selected = if (length(sp)) sp[length(sp)])
  })
  observeEvent(input$bs_lock, {
    rv$slip <- list(species = input$bs_sp, cv = input$bs_cv / 100, bias = input$bs_bias,
                    design = r$design()$name, time = Sys.time())
  })
  output$bs_locked <- renderUI({
    s <- rv$slip
    if (is.null(s)) return(NULL)
    div(class = "infobox", icon("lock"), " Bet locked for design ", tags$strong(s$design), ": ",
        s$species, ", CV ", round(100 * s$cv), "%, bias: ", s$bias, ".")
  })
}
