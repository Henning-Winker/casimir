# True catch (trainer view): estimate vs truth, by stratum, planted artefacts.
# In the menu only when Hide truth is unticked.

truth_ui <- function() {
  tagList(
    uiOutput("tr_hidden"),
    tabsetPanel(
      id = "truth_tabs",
      tabPanel("Estimate vs truth",
        box(width = 12, title = "True catch in the survey year and the last estimate",
          tableOutput("tr_total"))),
      tabPanel("By stratum",
        box(width = 12, title = "True catch by site type, quarter and segment (t)",
          uiOutput("tr_sp_ui"),
          tableOutput("tr_strata"))),
      tabPanel("Planted artefacts",
        box(width = 12, title = "Field problems in the last sample",
          tableOutput("tr_art"),
          p(class = "note", "Set in step 4, tab Artefacts.")))))
}

truth_server <- function(input, output, session, rv, r) {
  output$tr_hidden <- renderUI({
    if (!r$show_truth()) return(hidden_msg())
    if (is.null(rv$fishery)) empty_msg("Build the island first.")
  })
  output$tr_total <- renderTable({
    req(r$show_truth(), rv$fishery)
    if (is.null(rv$estimate)) {
      t <- cas_truth(rv$fishery, years = max(rv$fishery$years), by = "year")
      return(data.frame(species = t$species, `true (t)` = t$kg / 1000, check.names = FALSE))
    }
    t <- check_estimate(rv$estimate, rv$fishery)
    data.frame(species = t$species, `true (t)` = t$true / 1000, `estimate (t)` = t$estimate / 1000,
               `error %` = 100 * t$rel_error, `in 95% interval` = ifelse(t$covered, "yes", "no"),
               check.names = FALSE)
  }, digits = 1)
  output$tr_sp_ui <- renderUI({
    req(rv$fishery)
    selectInput("tr_sp", "Species", rv$fishery$species, selected = rv$fishery$species[length(rv$fishery$species)])
  })
  output$tr_strata <- renderTable({
    req(r$show_truth(), rv$fishery, input$tr_sp)
    t <- cas_truth(rv$fishery, years = max(rv$fishery$years))
    t <- t[t$species == input$tr_sp, ]
    x <- stats::xtabs(kg / 1000 ~ paste(site_type, segment) + quarter, t)
    as.data.frame.matrix(x)
  }, rownames = TRUE, digits = 2)
  output$tr_art <- renderTable({
    req(r$show_truth(), rv$sample)
    a <- planted_artefacts(rv$sample)
    if (!nrow(a)) data.frame(artefact = "none", setting = "") else a
  })
}
