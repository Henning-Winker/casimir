# Step 1: Island (Fleet and ranges, Map, Landings)

island_ui <- function() {
  seg_row <- function(s, boats, range) {
    fluidRow(
      column(3, tags$div(style = "font-weight:700; padding-top:28px;", seg_label[[s]])),
      column(4, sliderInput(paste0("is_boats_", s), "Boats", min = 0, max = 300, value = boats, step = 5)),
      column(5, sliderInput(paste0("is_range_", s), lab("Range (cells of 2.5 km)", "range"),
                            min = 2, max = 35, value = range, step = 1)))
  }
  tabsetPanel(
    id = "is_tabs",
    tabPanel("Fleet and ranges",
      box(width = 12, title = "Island X",
        div(class = "infobox",
            "Island X has 9 landing sites of three types (Harbour, Beach, Jetty) along its coast. ",
            "Each boat belongs to a segment (fishing-unit type) and lands at a home site. ",
            "Where it fishes depends on its range from that site and on where its target species live. ",
            "Every trip, with its true catch by species, is the population the survey samples."),
        fluidRow(
          column(4, sliderInput("is_trips", "Trips per year (all boats)", min = 4000, max = 30000,
                                value = 16000, step = 1000)),
          column(4, sliderInput("is_years", "Years simulated (survey in the last 5)", min = 6, max = 15,
                                value = 10, step = 1)),
          column(4, numericInput("is_seed", lab("Seed", "seed"), value = 1, min = 1, step = 1)))),
      box(width = 12, title = span("Segments", tip("segments")),
        seg_row("canoe", 150, 5),
        seg_row("small_motor", 60, 12),
        seg_row("large_motor", 15, 25),
        p(class = "note", "Canoes are based at beaches and jetties, small motor boats everywhere, large motor boats at harbours and some jetties. Press Build island (left) after changing a setting."))),
    tabPanel("Map",
      box(width = 12, title = "Landing sites, ranges and where the trips fished",
        uiOutput("is_empty"),
        plotOutput("is_map", height = 560),
        p(class = "note", "Depth increases offshore (to the right). Circles show the range of each segment around the sites where it is based; white dots the number of trips per cell."))),
    tabPanel("Species, effort, catch",
      box(width = 12, title = "Where the fish are, where the boats fish, where the catch comes from",
        fluidRow(
          column(3, radioButtons("mp_what", "Map", c("Species distribution" = "species",
                                                     "Fishing effort" = "effort", "Catch" = "catch"))),
          column(3, radioButtons("mp_by", "Split by", c("None" = "none", "Segment" = "segment",
                                                        "Quarter" = "quarter", "Tactic" = "tactic"),
                                 selected = "segment")),
          column(6, uiOutput("mp_sp_ui"))),
        uiOutput("mp_note"),
        plotOutput("mp_plot", height = 640))),
    tabPanel("Landings",
      box(width = 6, title = "Landings in the survey year by site and segment",
        tableOutput("is_tab")),
      box(width = 6, title = "Landings per site-day",
        plotOutput("is_perday", height = 300),
        p(class = "note", "How many boats an enumerator would see in a day. Days with no landings are common at small sites."))))
}

island_server <- function(input, output, session, rv, r) {
  output$guide_tips <- renderTable(guide_tips_table())
  output$is_empty <- renderUI(if (is.null(rv$fishery)) empty_msg("Press Build island in the left panel."))
  output$is_map <- render_plot({
    req(rv$fishery)
    plot(rv$fishery)
  })
  output$mp_sp_ui <- renderUI({
    req(rv$fishery)
    if (identical(input$mp_what, "effort")) return(NULL)
    sp <- rv$fishery$species
    selectizeInput("mp_sp", "Species", sp, multiple = TRUE,
                   selected = if (identical(input$mp_what, "catch")) c("Scomber colias", "Thunnus alalunga", "Merluccius merluccius") else sp)
  })
  output$mp_note <- renderUI({
    if (identical(input$mp_what, "catch") && !r$show_truth()) return(hidden_msg())
    p(class = "note", switch(input$mp_what %||% "species",
      species = "Unfished density of each species, from depth, reef or sediment, and sea temperature. Pelagic species also move through the area with the seasons.",
      effort = "Trips per cell in the survey year. Each segment fishes within its range of its home site.",
      catch = "True catch per cell in the survey year (cells with at least 1 kg). Split by segment to see who catches what, and where."))
  })
  output$mp_plot <- render_plot({
    req(rv$fishery)
    by <- if (identical(input$mp_by, "none")) NULL else input$mp_by
    switch(input$mp_what %||% "species",
      species = plot_species_map(rv$fishery, species = input$mp_sp),
      effort = plot_effort_map(rv$fishery, by = by, ncol = if (identical(by, "quarter")) 2 else NULL),
      catch = {
        req(r$show_truth())
        sp <- input$mp_sp %||% rv$fishery$species[1:3]
        if (is.null(by) || identical(by, "tactic")) plot_catch_map(rv$fishery, species = sp, ncol = min(5, length(sp)))
        else plot_catch_map(rv$fishery, species = sp, by = by)
      })
  })

  survey_landings <- reactive({
    req(rv$fishery)
    l <- rv$fishery$landings
    l[l$year == max(rv$fishery$years), ]
  })
  output$is_tab <- renderTable({
    l <- survey_landings()
    t <- as.data.frame.matrix(table(l$site, factor(l$segment, rv$fishery$segments)))
    t <- cbind(site = rownames(t), type = rv$fishery$sites$type[match(rownames(t), rv$fishery$sites$site)], t)
    t
  }, digits = 0)
  output$is_perday <- render_plot({
    l <- survey_landings()
    fr <- frame_survey(rv$fishery)
    pf <- casimir:::psu_frame(fr)
    n <- as.vector(table(factor(l$key, paste(pf$site, pf$date))))
    df <- data.frame(n = n, type = pf$site_type)
    ggplot(df, aes(n, fill = type)) + geom_histogram(binwidth = 1, colour = "white") +
      labs(x = "Landings per site-day (all segments)", y = "Site-days", fill = "Site type") + theme_bw()
  })
}
