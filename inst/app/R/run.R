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

  # Two pills at the top of every step: the island (build / new island) and
  # the sample (roll / redraw). Each is a button.
  pill <- function(input_id, col, ico, txt, title) {
    tags$button(type = "button", title = title,
      style = paste0("background:", col, "; color:#fff; padding:4px 14px; border-radius:12px;",
                     " font-size:13px; font-weight:600; border:0; cursor:pointer; margin-left:6px;"),
      onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", input_id),
      icon(ico), " ", txt)
  }
  output$status_pill <- renderUI({
    if (!r$fish_current())
      return(pill("build_req", "#f39c12", "hammer", "Build the island",
                  "Build Island X with the current settings"))
    island <- pill("rebuild_req", "#3c8dbc", "rotate", "Island ready \u00b7 new island",
                   "Build a new island with the same settings and the next seed (the sample is cleared)")
    sample <- if (r$sample_current())
      pill("roll_req", "#00a65a", "dice", "Sample ready \u00b7 redraw", "Draw a new sample with the current design")
    else pill("roll_req", "#f39c12", "dice", "Roll: draw a sample", "Draw a sample with the current design")
    tagList(island, sample)
  })

  output$truth_item <- renderMenu({
    if (r$show_truth()) menuItem("True catch (trainer)", tabName = "truth", icon = icon("eye"))
  })

  # One build routine for every button that asks for it. Errors are shown
  # in the app instead of stopping it.
  build <- function(seed = as.numeric(input$is_seed %||% 1)) {
    if (!cpue_ok()) {
      showModal(modalDialog(title = "cpueTargeting needs updating",
        p("casimir needs cpueTargeting 0.4.2 or later (landing sites, fishing ranges, fleet years). Close the app and run:"),
        tags$pre(cpue_fix), easyClose = TRUE))
      return(invisible(FALSE))
    }
    ok <- tryCatch({
      withProgress(message = "Building the island (about 5 seconds)", value = 0.3, {
        rv$fishery <- cas_fishery(r$settings(), seed = seed)
        rv$fish_snap <- list(r$settings(), seed)
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
  observeEvent(input$rebuild_req, {
    seed <- as.numeric(input$is_seed %||% 1) + 1
    updateNumericInput(session, "is_seed", value = seed)
    if (isTRUE(build(seed))) showNotification(paste("New island (seed", seed, ")."), type = "message")
  })

  observeEvent(input$roll_req, roll())

  observeEvent(input$roll, roll())
  roll <- function() {
    if (!r$fish_current()) {
      showNotification("Build the island first (step 1).", type = "warning")
      return()
    }
    rv$rolls <- (rv$rolls %||% 0) + 1
    ok <- tryCatch({
      rv$sample <- draw_sample(rv$fishery, r$design(), artefacts = r$artefacts(),
                               seed = seed_for(input$is_seed %||% 1, paste0("roll", rv$rolls)))
      rv$sample_snap <- r$sample_key()
      rv$estimate <- estimate_catch(rv$sample, route = input$rs_route %||% "psu")
      TRUE
    }, error = function(e) {
      showNotification(paste("The sample could not be drawn:", conditionMessage(e)),
                       type = "error", duration = NULL)
      FALSE
    })
    # stay on Roll or Raise to watch the new draw; otherwise go to Roll
    if (ok && !(input$step %in% c("roll", "raise"))) updateTabItems(session, "step", "roll")
  }

  observeEvent(input$go_island, {
    updateTabItems(session, "step", "island")
    if (!r$fish_current()) build()
  })

  if (!cpue_ok()) showModal(modalDialog(title = "cpueTargeting needs updating",
    p("casimir needs cpueTargeting 0.4.2 or later (landing sites, fishing ranges, fleet years). Close the app and run:"),
    tags$pre(cpue_fix), easyClose = TRUE))

  observeEvent(input$start_over, session$reload())
}
