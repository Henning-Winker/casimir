#' Start the casimir app
#'
#' Opens the casimir Shiny app: build the island, look at the frame survey,
#' set the design, roll a sample, raise it to catch per species and spin the
#' design many times, step by step in the left panel. The app uses the same
#' functions as the package, so everything done in the app can be repeated
#' in a script.
#'
#' @param ... passed to [shiny::runApp()], e.g. `launch.browser = TRUE` or
#'   `port = 3838`.
#' @return called for its side effect (runs the app).
#' @export
#' @examples
#' if (interactive()) run_app()
run_app <- function(...) {
  need <- c("shiny", "shinydashboard", "commonmark")
  miss <- need[!vapply(need, requireNamespace, TRUE, quietly = TRUE)]
  if (length(miss))
    stop("The app needs: ", toString(miss), ". Install with install.packages(c(",
         paste0("\"", miss, "\"", collapse = ", "), "))")
  if (!"landing_settings" %in% getNamespaceExports("cpueTargeting"))
    stop("casimir needs cpueTargeting 0.4.1 or later (landing sites). Install it with\n",
         "  remotes::install_github(\"Henning-Winker/cpueTargeting@landing-sites\")", call. = FALSE)
  dir <- system.file("app", package = "casimir")
  if (!nzchar(dir)) stop("App folder not found: reinstall casimir")
  shiny::runApp(dir, ...)
}
