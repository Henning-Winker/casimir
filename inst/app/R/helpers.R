# Small helper functions and the shared reactive objects used across the app.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- small UI pieces -----------------------------------------------------------

# An (i) symbol with the explanation from casimir_tip() shown on hover
tip <- function(id) tags$span(class = "tip", title = casimir_tip(id), icon("circle-info"))

# A label with a tip
lab <- function(text, id = NULL) if (is.null(id)) text else tags$span(text, tip(id))

# A simple number box
stat_box <- function(label, value) {
  div(class = "statbox", div(class = "lab", label), div(class = "val", value))
}

# Message shown on a page that needs something done first
# Buttons anywhere in the app can ask for a build: they set input$build_req
build_button <- function(label = "Build the island now") {
  tags$button(type = "button", class = "btn btn-primary btn-sm", style = "margin-left:10px;",
              onclick = "Shiny.setInputValue('build_req', Math.random(), {priority: 'event'})",
              icon("hammer"), label)
}

empty_msg <- function(text) {
  div(class = "infobox", text, if (grepl("Build the island", text)) build_button())
}

# casimir needs cpueTargeting with landing sites (>= 0.4.1)
cpue_ok <- function() {
  "landing_settings" %in% getNamespaceExports("cpueTargeting")
}
cpue_fix <- 'remotes::install_github("Henning-Winker/cpueTargeting@landing-sites")'


hidden_msg <- function() {
  div(class = "infobox", icon("eye-slash"), " Hidden while ", tags$strong("Hide truth"),
      " is ticked (left panel). Untick it to show the true catch.")
}

segments_all <- c("canoe", "small_motor", "large_motor")
seg_label <- c(canoe = "Canoe", small_motor = "Small motor", large_motor = "Large motor")
site_types <- c("Harbour", "Beach", "Jetty")

# Format kg as tonnes
tonnes <- function(x, d = 1) formatC(x / 1000, format = "f", digits = d, big.mark = ",")

# ---- shared reactives ----------------------------------------------------------

make_reactives <- function(input, rv) {
  r <- list()

  r$show_truth <- reactive(!isTRUE(input$hide_truth))

  # Island settings from the inputs
  r$settings <- reactive({
    d_boats <- eval(formals(island_x)$boats)
    d_range <- eval(formals(island_x)$range)
    boats <- vapply(segments_all, function(s) input[[paste0("is_boats_", s)]] %||% d_boats[[s]], 0)
    range <- vapply(segments_all, function(s) input[[paste0("is_range_", s)]] %||% d_range[[s]], 0)
    keep <- boats > 0
    yrs <- input$is_years %||% 10
    island_x(boats = boats[keep], range = range[keep],
             trips = input$is_trips %||% 16000, n_years = yrs, start_year = 2026 - yrs)
  })
  r$fish_key <- reactive(list(r$settings(), as.numeric(input$is_seed %||% 1)))
  r$fish_current <- reactive(!is.null(rv$fishery) && identical(rv$fish_snap, r$fish_key()))

  # Design from the inputs
  r$design <- reactive({
    req(rv$fishery)
    m <- vapply(site_types, function(t) as.numeric(input[[paste0("ds_m_", t)]] %||% 6), 0)
    segs <- rv$fishery$segments
    n <- vapply(segs, function(s) as.numeric(input[[paste0("ds_n_", s)]] %||% 3), 0)
    cas_design(m = m, n = n, psu_select = input$ds_select %||% "srs",
               w_kg = input$ds_w %||% 10, unit_kg = 0.5,
               cost = c(psu = input$ds_cost_psu %||% 4, landing = input$ds_cost_landing %||% 0.5,
                        kg = input$ds_cost_kg %||% 0.05),
               name = input$ds_name %||% "A")
  })

  # Planted artefacts (trainer)
  r$artefacts <- reactive({
    artefacts(high_roller = input$af_high %||% 0,
              short_watch = if (isTRUE(input$af_watch_on)) input$af_watch else NULL,
              easy_sites = if (length(input$af_sites) && !isTRUE(input$af_sites_all)) input$af_sites else NULL,
              dropped_zeros = isTRUE(input$af_zeros),
              top_of_box = input$af_top %||% 0,
              old_frame = input$af_frame %||% 0)
  })

  r$sample_key <- reactive(list(r$fish_key(), r$design(), r$artefacts()))
  r$sample_current <- reactive(!is.null(rv$sample) && identical(rv$sample_snap, r$sample_key()))
  r
}

# renderPlot at a resolution that keeps plot text readable in the boxes
render_plot <- function(expr, ..., env = parent.frame()) {
  shiny::renderPlot(substitute(expr), ..., res = 110, env = env, quoted = TRUE)
}
