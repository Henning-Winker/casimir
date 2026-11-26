# Step 5: Raise (Step by step, Totals)

raise_ui <- function() {
  tagList(
    uiOutput("rs_empty"),
    tabsetPanel(
      id = "rs_tabs",
      tabPanel("Step by step",
        box(width = 12, title = "Follow one species through the four steps",
          fluidRow(
            column(4, uiOutput("rs_sp_ui")),
            column(4, uiOutput("rs_psu_ui")),
            column(4, uiOutput("rs_seg_ui")))),
        box(width = 6, title = "Steps 1 and 2: landings at this visit",
          uiOutput("rs_step12"), tableOutput("rs_land")),
        box(width = 6, title = "Steps 3 and 4: stratum and total",
          uiOutput("rs_step34"))),
      tabPanel("Totals",
        box(width = 12, title = "Estimated catch by species",
          fluidRow(
            column(6, radioButtons("rs_route", lab("Route", "route"), inline = TRUE,
                         stats::setNames(c("psu", "effort"), c("Raise by site-days (PSU)", "CPUE \u00d7 effort")))),
            column(6, selectInput("rs_by", "Block by",
                         stats::setNames(c("none", "segment", "site_type", "quarter", "site_type,segment"),
                                         c("Species totals", "Fleet segment", "Site type", "Quarter",
                                           "Site type \u00d7 segment"))))),
          plotOutput("rs_plot", height = 520),
          tableOutput("rs_tot"),
          p(class = "note", "95% intervals from the variance between site-days (ultimate cluster). With Hide truth unticked, the true catch is shown as x.")))))

}

raise_server <- function(input, output, session, rv, r) {
  output$rs_empty <- renderUI(if (is.null(rv$sample)) empty_msg("Roll a sample first."))
  observeEvent(input$rs_route, {
    req(rv$sample)
    rv$estimate <- estimate_catch(rv$sample, route = input$rs_route)
  })
  output$rs_sp_ui <- renderUI({
    req(rv$sample)
    sp <- rv$sample$species
    tot <- rv$estimate$total
    selectInput("rs_sp", "Species", sp, selected = tot$species[which.max(tot$estimate)])
  })
  output$rs_psu_ui <- renderUI({
    req(rv$sample)
    v <- rv$sample$visits
    busy <- tapply(rv$sample$A$N, rv$sample$A$psu, sum)
    ch <- stats::setNames(v$psu, paste0(v$psu, ": ", v$site, " ", format(v$date), " (", busy[v$psu], " landings)"))
    selectInput("rs_psu", "Site-day (PSU)", ch, selected = names(which.max(busy)))
  })
  output$rs_seg_ui <- renderUI({
    req(rv$sample)
    selectInput("rs_seg", "Segment", rv$sample$segments)
  })
  output$rs_land <- renderTable({
    req(rv$estimate, input$rs_psu, input$rs_sp, input$rs_seg)
    e <- rv$estimate$landing
    e <- e[e$psu == input$rs_psu & e$segment == input$rs_seg, ]
    if (!nrow(e)) return(NULL)
    C <- rv$sample$C
    ws <- C$w_s[C$psu == input$rs_psu & C$species == input$rs_sp][match(e$trip, C$trip[C$psu == input$rs_psu & C$species == input$rs_sp])]
    data.frame(landing = e$trip, W = e$W, w = e$w, w_s = ifelse(is.na(ws), 0, ws),
               `W_ijs = w_s/w x W` = e[[input$rs_sp]], check.names = FALSE)
  }, digits = 2)
  output$rs_step12 <- renderUI({
    req(rv$estimate, input$rs_psu, input$rs_sp, input$rs_seg)
    p <- rv$estimate$psu
    p <- p[p$psu == input$rs_psu & p$segment == input$rs_seg, ]
    if (!nrow(p)) return(NULL)
    e <- rv$estimate$landing
    s <- sum(e[[input$rs_sp]][e$psu == input$rs_psu & e$segment == input$rs_seg])
    tagList(
      div(class = "formula", sprintf("Step 2:  W_is = N/n × Σ W_ijs = %d/%d × %.2f = %.2f kg",
                                     p$N, p$n, s, p[[input$rs_sp]])),
      if (p$N == 0) p(class = "note", casimir_tip("zeros")))
  })
  output$rs_step34 <- renderUI({
    req(rv$estimate, input$rs_psu, input$rs_sp, input$rs_seg)
    v <- rv$sample$visits[rv$sample$visits$psu == input$rs_psu, ]
    st <- rv$estimate$stratum
    h <- st[st$site_type == v$site_type & st$quarter == v$quarter & st$segment == input$rs_seg &
              st$species == input$rs_sp, ]
    p <- rv$estimate$psu
    sum_i <- sum(p[[input$rs_sp]][p$site_type == v$site_type & p$quarter == v$quarter &
                                     p$segment == input$rs_seg], na.rm = TRUE)
    tot <- rv$estimate$total[rv$estimate$total$species == input$rs_sp, ]
    tagList(
      p("Stratum: ", tags$strong(v$site_type, ", quarter ", v$quarter, ", ", input$rs_seg)),
      div(class = "formula", sprintf("Step 3:  W_hs = M/m × Σ W_is = %d/%d × %.1f = %.1f kg",
                                     h$M, h$m, sum_i, h$estimate)),
      div(class = "formula", sprintf("Step 4:  W_s = Σ_h W_hs = %s t  (CV %.0f%%)",
                                     tonnes(tot$estimate, 2), 100 * tot$cv)),
      p(class = "note", "The site-days of this stratum include the ones with no landings of this segment: their catch is zero."))
  })
  rs_by <- reactive({
    b <- input$rs_by %||% "none"
    if (b == "none") NULL else strsplit(b, ",")[[1]]
  })
  output$rs_plot <- render_plot({
    req(rv$estimate)
    tru <- if (r$show_truth()) rv$fishery
    if (is.null(rs_by())) plot_estimate(rv$estimate, tru) else plot_catch_by(rv$estimate, rs_by(), tru)
  })
  output$rs_tot <- renderTable({
    req(rv$estimate)
    x <- catch_by(rv$estimate, by = rs_by(), fishery = if (r$show_truth()) rv$fishery)
    out <- data.frame(x[c(rs_by(), "species")], `estimate (t)` = x$estimate / 1000,
                      `se (t)` = x$se / 1000, `CV %` = 100 * x$cv, check.names = FALSE)
    if (r$show_truth()) {
      out$`true (t)` <- x$true / 1000
      out$`in 95% interval` <- ifelse(x$covered, "yes", "no")
    }
    out
  }, digits = 2)
}
