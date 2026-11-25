# Guide: the app's own plain-language guide (inst/app/guide/app-guide.md),
# the workflow diagram and a teaching introduction.

guide_sections <- function() {
  f <- c(file.path("guide", "app-guide.md"),
         system.file("app", "guide", "app-guide.md", package = "casimir"))
  f <- f[nzchar(f) & file.exists(f)][1]
  if (is.na(f)) return(list())
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  starts <- grep("^## ", x)
  ends <- c(starts[-1] - 1, length(x))
  secs <- Map(function(a, b) paste(x[a:b], collapse = "\n"), starts, ends)
  names(secs) <- sub("^## +", "", x[starts])
  secs
}

md_html <- function(txt) HTML(commonmark::markdown_html(txt, extensions = TRUE))

guide_pick <- function(s, patterns) {
  hit <- unlist(lapply(patterns, function(p) grep(p, names(s), ignore.case = TRUE)))
  div(class = "guide", md_html(paste(unlist(s[unique(hit)]), collapse = "\n\n")))
}

guide_ui <- function() {
  s <- guide_sections()
  tabsetPanel(
    id = "guide_tabs",
    tabPanel("Start here",
      box(width = 12,
        div(class = "workflow", tags$img(src = "casimir-landing.svg",
          alt = "casimir: from boats on the beach to catch per species. 1 Island X, 2 Frame survey, 3 Design, 4 Roll, 5 Raise, 6 Spin x1000; below, one site-day up close.")),
        div(style = "text-align:center; margin-top:10px;",
            actionButton("go_island", "Start: build Island X", class = "btn-primary",
                         style = "height:44px; font-weight:600; padding:0 28px;"))),
      box(width = 12, title = "What casimir is for",
        p("casimir (Catch Sampling Inference Methods in R) is a catch estimator simulator for training in landing-site surveys. ",
          "It simulates a small-scale fishery whose true landings are known, lets trainees design a multi-stage survey, ",
          "draw samples from that fishery, raise them to catch per species with the estimators of the FAO training, ",
          "and see how close their estimates come to the truth."),
        p("No real data are used or needed. Island X, its fleet and its fish are simulated, so every estimate can be checked against the true catch, ",
          "which no real survey allows. Each roll draws a new sample from the same fishery, showing how much an estimate can vary through chance alone.")),
      box(width = 12, title = "How a casimir run is built",
        p("The steps in the left panel follow the stages of a catch survey (steps marked * are required):"),
        tags$ol(
          tags$li(tags$strong("Island: "), "landing sites, segments with their fishing ranges, and the simulated fishery (Build island)."),
          tags$li(tags$strong("Frame survey: "), "boats by site and segment, and fishing days: what the planner knows."),
          tags$li(tags$strong("Design: "), "site-days per stratum, landings per visit and the subsample, with the cost and a betting slip."),
          tags$li(tags$strong("Roll: "), "one sample, as field forms A to C (Roll button)."),
          tags$li(tags$strong("Raise: "), "the four estimation steps, from one landing to the total, with intervals."),
          tags$li(tags$strong("Spin ×1000 (optional): "), "the same design repeated many times: bias, CV and coverage."),
          tags$li(tags$strong("Cash out: "), "the survey as csv files, with an answer key for trainers."),
          tags$li(tags$strong("True catch (optional, for trainers): "), "shown only when Hide truth is unticked.")),
        div(class = "workflow",
            tags$img(src = "casimir-workflow.svg",
                     alt = "How the casimir steps connect: Island, Frame survey and Design lead to Roll, Raise and Spin; planted artefacts enter at Roll; the true catch is the answer key.")),
        p(class = "note", "Solid arrows show the path through the app; dashed arrows show what the trainer controls and what is compared with the truth.")),
      box(width = 12, title = "Bet first",
        div(class = "predictbox",
          p(style = "margin-bottom:0;",
            "Before each roll or spin, trainees should predict what will happen: will doubling the site-days halve the CV? Which species will be estimated worst? ",
            "Will the high roller bias the catch up or down? The betting slip in the Design step records the bet; Spin ×1000 settles it."))),
      box(width = 12, title = "Warning flags, Hide truth, and scope",
        div(class = "flagbox", tags$strong("Warning flags. "),
          "Some patterns in survey data come from the survey, not the fishery: landings sampled larger than those counted, a segment missing from the forms, no visit without landings. ",
          "The trainer can plant such problems in the Roll step, and the answer key lists them."),
        div(class = "formatbox", tags$strong("Hide truth. "),
          "Hide truth is unticked when the app opens, so the true catch is shown. Trainers should tick it before showing the app to trainees."),
        p(tags$strong("Scope. "),
          "casimir covers the estimation of total catch per species from landing-site surveys. Island X and the multi-stage design follow the FAO training concept by Pedro Barros (Data Collection and Estimation of Fisheries Quantities). The simulated fishery comes from the logbook simulator of cpueTargeting; the teaching style follows FLOSS."))),
    tabPanel("Island and frame", box(width = 12, guide_pick(s, c("^1\\.", "^2\\.")))),
    tabPanel("Design and field forms", box(width = 12, guide_pick(s, c("^3\\.", "^4\\.")))),
    tabPanel("Raising", box(width = 12, guide_pick(s, "^5\\."))),
    tabPanel("Spin ×1000", box(width = 12, guide_pick(s, "^6\\."))),
    tabPanel("Warning flags", box(width = 12, guide_pick(s, "^Warning flags"))),
    tabPanel("Cash out and truth", box(width = 12, guide_pick(s, "^7\\."))),
    tabPanel("Settings explained",
      box(width = 12, title = "What each setting means",
        tableOutput("guide_tips"),
        p(class = "note", "The same texts appear when you hover over the (i) symbols."))))
}

guide_tips_table <- function() {
  t <- casimir_tip()
  data.frame(Setting = names(t), Meaning = unlist(t), row.names = NULL)
}
