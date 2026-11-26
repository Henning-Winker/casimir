# Left panel (checklist, Build island, Roll), the status at the top of the
# page, and the True catch menu item.

run_server <- function(input, output, session, rv, r) {

  output$checklist <- renderUI({
    item <- function(state, text) {
      mark <- switch(state, ok = "✓", todo = "✗", "○")
      col <- switch(state, ok = "#6fdca8", todo = "#f5a623", "#8aa4af")
      tags$div(style = "font-size:13px; margin:3px 0;",
               tags$span(style = paste0("color:", col, "; font-weight:700; margin-right:8px;"), mark), text)
    }
    tagList(
      item(if (r$fish_current()) "ok" else "todo", "Island built *"),
      item(if (!is.null(rv$slip)) "ok" else "opt", "Betting slip filled in"),
      item(if (r$sample_current()) "ok" else "todo", "Sample rolled *"),
      item(if (length(rv$evals)) "ok" else "opt", "Spin ×1000 (optional)")
    )
  })

  output$run_help <- renderText({
    if (is.null(rv$fishery)) return("Island not built yet: set it up in step 1 and press Build island.")
    if (!r$fish_current()) return("Out of date: island settings changed since the last build.")
    if (is.null(rv$sample)) return("Island up to date. No sample yet: set the design in step 3 and Roll.")
    if (!r$sample_current()) return("Out of date: design or artefacts changed since the last roll.")
    "Island and sample up to date."
  })

  output$status_pill <- renderUI({
    col <- if (r$sample_current()) "#00a65a" else if (r$fish_current()) "#3c8dbc" else "#f39c12"
    txt <- if (r$sample_current()) "Sample ready" else if (r$fish_current()) "Island ready" else "Build the island"
    sty <- paste0("background:", col, "; color:#fff; padding:4px 12px; border-radius:12px; font-size:13px; border:0;")
    if (r$fish_current()) return(tags$span(style = sty, txt))
    tags$button(type = "button", style = paste0(sty, " cursor:pointer;"), title = "Build Island X with the current settings",
                onclick = "Shiny.setInputValue('build_req', Math.random(), {priority: 'event'})",
                icon("hammer"), " ", txt)
  })

  output$truth_item <- renderMenu({
    if (r$show_truth()) menuItem("True catch (trainer)", tabName = "truth", icon = icon("eye"))
  })

  # One build routine for every button that asks for it. Errors are shown
  # in the app instead of stopping it.
  build <- function() {
    if (!cpue_ok()) {
      showModal(modalDialog(title = "cpueTargeting needs updating",
        p("casimir needs cpueTargeting 0.4.1 or later (landing sites and fishing ranges). Close the app and run:"),
        tags$pre(cpue_fix), easyClose = TRUE))
      return(invisible(FALSE))
    }
    ok <- tryCatch({
      withProgress(message = "Building the island (about 5 seconds)", value = 0.3, {
        rv$fishery <- cas_fishery(r$settings(), seed = input$is_seed %||% 1)
        rv$fish_snap <- r$fish_key()
        rv$sample <- NULL
        rv$estimate <- NULL
        rv$evals <- list()
      })
      TRUE
    }, error = function(e) {
      showNotification(paste("The island could not be built:", conditionMessage(e)),
                       type = "error", duration = NULL)
      FALSE
    })
    if (ok) showNotification("Island built.", type = "message")
    invisible(ok)
  }

  observeEvent(input$build_island, build())
  observeEvent(input$build_req, build())

  observeEvent(input$roll, {
    if (!r$fish_current()) {
      showNotification("Build the island first (step 1).", type = "warning")
      return()
    }
    rv$rolls <- (rv$rolls %||% 0) + 1
    rv$sample <- draw_sample(rv$fishery, r$design(), artefacts = r$artefacts(),
                             seed = seed_for(input$is_seed %||% 1, paste0("roll", rv$rolls)))
    rv$sample_snap <- r$sample_key()
    rv$estimate <- estimate_catch(rv$sample, route = input$rs_route %||% "psu")
    updateTabItems(session, "step", "roll")
  })

  observeEvent(input$go_island, {
    updateTabItems(session, "step", "island")
    if (!r$fish_current()) build()
  })

  if (!cpue_ok()) showModal(modalDialog(title = "cpueTargeting needs updating",
    p("casimir needs cpueTargeting 0.4.1 or later (landing sites and fishing ranges). Close the app and run:"),
    tags$pre(cpue_fix), easyClose = TRUE))

  observeEvent(input$start_over, session$reload())
}
