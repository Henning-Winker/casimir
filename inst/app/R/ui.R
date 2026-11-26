# The layout: left panel (steps, Hide truth, checklist, Build / Roll / Spin
# buttons) and the page for each step. Same structure and style as the FLOSS
# app. Steps: 1 Island, 2 Frame, 3 Design, 4 Roll, 5 Raise, 6 Series,
# 7 Spin x1000, 8 Cash out, and True catch (trainer view).

app_sidebar <- function() {
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "step",
      menuItem("Start here", tabName = "guide", icon = icon("book-open"), selected = TRUE),
      menuItem("1  Island *", tabName = "island"),
      menuItem("2  Frame survey *", tabName = "frame"),
      menuItem("3  Design *", tabName = "design"),
      menuItem("4  Roll: field forms *", tabName = "roll"),
      menuItem("5  Raise: estimates *", tabName = "raise"),
      menuItem("6  Series: catch and effort", tabName = "series"),
      menuItem("7  Spin ×1000", tabName = "spin"),
      menuItem("8  Cash out", tabName = "export"),
      menuItemOutput("truth_item")
    ),
    div(style = "padding: 4px 16px;",
      tags$p(style = "font-size:12px; color:#8aa4af;", "* required. All other steps are optional."),
      tags$hr(style = "border-color:#4b646f; margin: 8px 0;"),
      div(class = "truth-switch",
          checkboxInput("hide_truth", "Hide truth", value = FALSE),
          tags$p(style = "font-size:12px; color:#8aa4af; margin-top:-8px;",
                 "When ticked, the true catch, the planted artefacts and the answer key are hidden (for trainees). Untick to show the True catch step.")),
      tags$hr(style = "border-color:#4b646f; margin: 8px 0;"),
      tags$div(style = "font-size:12px; font-weight:600; color:#8aa4af; text-transform:uppercase;", "Before you roll"),
      uiOutput("checklist"),
      actionButton("build_island", "Build island", class = "btn-primary", width = "100%",
                   style = "margin-top:10px; height:44px; font-weight:600;"),
      actionButton("roll", "Roll: draw a sample", class = "btn-primary", width = "100%",
                   style = "margin-top:8px; height:44px; font-weight:600;"),
      tags$p(style = "margin-top:8px; font-size:13px;", textOutput("run_help", inline = TRUE)),
      actionButton("start_over", "Start over", width = "100%",
                   style = "margin-top:10px; height:40px; background:transparent; color:#7fc0e8; border:1px solid #5d7480;")
    )
  )
}

app_css <- "
  .box { border-top-color: #3c8dbc; }
  .content-wrapper { background: #ecf0f5; }
  .skin-blue .main-header .logo { font-weight: 700; }
  .note { font-size: 13px; color: #555; margin-top: 6px; }
  .warnbox { border: 1px solid #f5a623; background: #fff4e0; color: #5c3b00; padding: 10px 12px; border-radius: 3px; margin: 8px 0; font-size: 14px; }
  .infobox { border: 1px solid #9cc3dc; background: #eef6fb; color: #22404f; padding: 10px 12px; border-radius: 3px; margin: 8px 0; font-size: 14px; }
  .predictbox { border: 1px solid #b7a6dd; background: #f4f0fb; color: #2f2350; padding: 10px 12px; border-radius: 3px; margin: 8px 0; font-size: 14px; }
  .flagbox { border: 1px solid #e3a0a0; background: #fcf0f0; color: #5a1f1f; padding: 10px 12px; border-radius: 3px; margin: 8px 0; font-size: 14px; }
  .statbox { background:#fff; border-radius:3px; box-shadow:0 1px 1px rgba(0,0,0,0.1); padding:10px 14px; margin-bottom:10px; }
  .statbox .lab { font-size:13px; color:#555; }
  .statbox .val { font-size:26px; font-weight:700; color:#333; }
  .formatbox { border: 1px dashed #b9c3c9; background: #fafbfc; padding: 10px 12px; font-size: 13px; margin-top: 10px; }
  .formula { font-family: 'Courier New', monospace; background:#f6f8fa; padding:8px 10px; border-radius:3px; margin:6px 0; font-size:14px; }
  .tip { color: #3c8dbc; cursor: help; margin-left: 4px; font-size: 12px; }
  .truth-switch .checkbox label { font-weight: 600; color: #fff; }
  .irs--shiny .irs-bar { background: #3c8dbc; border-color: #3c8dbc; }
  .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #3c8dbc; }
  .guide h2 { font-size: 20px; margin-top: 22px; }
  .guide table { border-collapse: collapse; margin: 8px 0; }
  .guide th, .guide td { border: 1px solid #d8dee2; padding: 4px 8px; font-size: 13px; }
  .guide pre { background: #f6f8fa; border: none; }
  .guide h3 { font-size: 17px; margin-top: 18px; }
  .workflow img { width: 100%; max-width: 1400px; height: auto; display: block; margin: 4px auto 0; }
"

app_body <- function() {
  dashboardBody(
    tags$head(tags$style(HTML(app_css))),
    conditionalPanel("input.step != 'guide'",
      div(style = "display:flex; justify-content:flex-end; align-items:center; gap:8px; margin-bottom:6px;",
          uiOutput("status_pill"))),
    tabItems(
      tabItem("guide", guide_ui()),
      tabItem("island", island_ui()),
      tabItem("frame", frame_ui()),
      tabItem("design", design_ui()),
      tabItem("roll", roll_ui()),
      tabItem("raise", raise_ui()),
      tabItem("series", series_ui()),
      tabItem("spin", spin_ui()),
      tabItem("export", export_ui()),
      tabItem("truth", truth_ui())
    )
  )
}
