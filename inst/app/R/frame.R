# Step 2: Frame survey (Boats, Fishing days)

frame_ui <- function() {
  tabsetPanel(
    id = "fr_tabs",
    tabPanel("Boats by site",
      box(width = 12, title = span("Frame survey", tip("frame")),
        uiOutput("fr_empty"),
        p("The frame survey lists the boats at each landing site by segment, and the fishing days in each quarter. It is what a survey planner knows before going to the field."),
        tableOutput("fr_boats"),
        uiOutput("fr_old"))),
    tabPanel("Fishing days",
      box(width = 6, title = "Fishing days by quarter",
        tableOutput("fr_days"),
        p(class = "note", "No fishing on Sundays.")),
      box(width = 6, title = "Site-days (PSUs) by site type and quarter",
        tableOutput("fr_M"),
        p(class = "note", "M: the number of site-days a survey could visit. ", tip("psu")))))
}

frame_server <- function(input, output, session, rv, r) {
  output$fr_empty <- renderUI(if (is.null(rv$fishery)) empty_msg("Build the island first (step 1)."))
  frame <- reactive({
    req(rv$fishery)
    y <- max(rv$fishery$years)
    frame_survey(rv$fishery, y, ref_year = y - (r$artefacts()$old_frame %||% 0))
  })
  output$fr_boats <- renderTable({
    b <- stats::xtabs(boats ~ site + segment, frame()$boats)
    t <- as.data.frame.matrix(b)[rv$fishery$segments]
    cbind(site = rownames(t), type = rv$fishery$sites$type[match(rownames(t), rv$fishery$sites$site)], t,
          total = rowSums(t))
  }, digits = 0)
  output$fr_old <- renderUI({
    f <- frame()
    if (f$ref_year < f$year && r$show_truth())
      div(class = "flagbox", tags$strong("Planted: old frame. "),
          "This frame was surveyed in ", f$ref_year, ", the survey runs in ", f$year, ".")
  })
  output$fr_days <- renderTable(frame()$days, digits = 0)
  output$fr_M <- renderTable({
    pf <- casimir:::psu_frame(frame())
    as.data.frame.matrix(table(site_type = pf$site_type, quarter = pf$quarter))
  }, rownames = TRUE, digits = 0)
}
