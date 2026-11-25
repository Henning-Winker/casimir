# Step 7: Cash out (download the survey data, with the answer key for trainers)

export_ui <- function() {
  tagList(
    uiOutput("ex_empty"),
    box(width = 12, title = "Cash out: download the survey",
      p("The frame survey, the design and the field forms of the last sample, as csv files in one zip: the data a survey team would hand in."),
      uiOutput("ex_key_ui"),
      downloadButton("ex_download", "Download survey (.zip)", class = "btn-primary"),
      div(class = "formatbox",
          tags$strong("Files: "), "frame_boats.csv, frame_days.csv, design.txt, form_A_visits.csv, form_B_landings.csv, form_C_species.csv, README.txt; ",
          "with the answer key: answer_key/true_catch_by_stratum.csv, answer_key/estimate_vs_truth.csv, answer_key/planted_artefacts.csv.")))
}

export_server <- function(input, output, session, rv, r) {
  output$ex_empty <- renderUI(if (is.null(rv$sample)) empty_msg("Roll a sample first (step 4)."))
  output$ex_key_ui <- renderUI({
    if (r$show_truth()) checkboxInput("ex_key", "Include the answer key (answer_key/ folder)", TRUE)
    else div(class = "note", icon("eye-slash"), " The answer key can be added when Hide truth is unticked.")
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
