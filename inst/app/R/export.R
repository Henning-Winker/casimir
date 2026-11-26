# Step 8: Cash out (download the survey data, with the answer key for trainers)

export_ui <- function() {
  tagList(
    uiOutput("ex_empty"),
    box(width = 12, title = "Cash out: download the survey",
      p("The frame survey, the design and the field forms of the last sample, as csv files in one zip: the data a survey team would hand in."),
      uiOutput("ex_key_ui"),
      downloadButton("ex_download", "Download survey (.zip)", class = "btn-primary"),
      div(class = "formatbox",
          tags$strong("Files: "), "frame_boats.csv, frame_days.csv, design.txt, form_A_visits.csv, form_B_landings.csv, form_C_species.csv, README.txt; ",
          "with the answer key: answer_key/true_catch_by_stratum.csv, answer_key/estimate_vs_truth.csv, answer_key/planted_artefacts.csv.")),
    box(width = 12, title = "Estimates against the truth: one long table",
      p("One row per spin (iter), year, quarter, site type (area), segment (fleet) and species, with the estimated and the true catch (kg). ",
        "Effort rows (variable = effort, in trips) have no species."),
      fluidRow(
        column(5, uiOutput("ex_long_src_ui")),
        column(3, radioButtons("ex_long_fmt", "Format", c("csv" = "csv", "rds (R)" = "rds"), inline = TRUE)),
        column(4, br(), downloadButton("ex_long", "Download table", class = "btn-primary"))),
      uiOutput("ex_long_note"),
      div(class = "formatbox", tags$strong("Columns: "),
          "variable (catch or effort), iter, year, quarter, site_type, segment, species, estimate, se, true. ",
          "In R: casimir::spins_long().")))
}

export_server <- function(input, output, session, rv, r) {
  output$ex_empty <- renderUI(if (is.null(rv$sample)) empty_msg("Roll a sample first (step 4)."))
  output$ex_key_ui <- renderUI({
    if (r$show_truth()) checkboxInput("ex_key", "Include the answer key (answer_key/ folder)", TRUE)
    else div(class = "note", icon("eye-slash"), " The answer key can be added when Hide truth is unticked.")
  })
  long_sources <- reactive({
    c(if (!is.null(rv$estimate)) c("Last sample (step 5)" = "sample"),
      if (!is.null(rv$survey)) c("Series (step 6)" = "series"),
      if (!is.null(rv$spins)) c("Spins over the years (step 7)" = "spins"))
  })
  output$ex_long_src_ui <- renderUI({
    ch <- long_sources()
    if (!length(ch)) return(div(class = "note", "Roll a sample, a series (step 6) or spin over the years (step 7) first."))
    radioButtons("ex_long_src", "Estimates from", ch, selected = ch[length(ch)])
  })
  output$ex_long_note <- renderUI({
    if (!r$show_truth()) div(class = "note", icon("eye-slash"), " The true values are left out while Hide truth is ticked.")
  })
  long_table <- function() {
    src <- input$ex_long_src %||% "sample"
    x <- switch(src, sample = rv$estimate, series = rv$survey, spins = rv$spins)
    d <- spins_long(x, fishery = rv$fishery)
    if (!r$show_truth()) d$true <- NULL
    d
  }
  output$ex_long <- downloadHandler(
    filename = function() paste0("casimir_estimates_", input$ex_long_src %||% "sample", ".", input$ex_long_fmt %||% "csv"),
    content = function(file) {
      req(length(long_sources()))
      d <- long_table()
      if (identical(input$ex_long_fmt, "rds")) saveRDS(d, file) else utils::write.csv(d, file, row.names = FALSE)
    })
  output$ex_download <- downloadHandler(
    filename = function() paste0("casimir_survey_", format(Sys.Date()), ".zip"),
    content = function(file) {
      req(rv$sample)
      key <- r$show_truth() && isTRUE(input$ex_key)
      paths <- export_survey(rv$sample, dir = tempfile("casimir"),
                             answer_key = if (key) rv$fishery else NULL)
      old <- setwd(attr(paths, "dir"))
      on.exit(setwd(old))
      utils::zip(file, files = list.files(".", recursive = TRUE), flags = "-q")
    })
}
