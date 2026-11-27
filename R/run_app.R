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
  if (utils::packageVersion("cpueTargeting") < "0.4.2")
    stop("casimir needs cpueTargeting 0.4.2 or later (landing sites, fleet years). Install it with\n",
         "  remotes::install_github(\"Henning-Winker/cpueTargeting\")", call. = FALSE)
  check_graphics()
  dir <- system.file("app", package = "casimir")
  if (!nzchar(dir)) stop("App folder not found: reinstall casimir")
  shiny::runApp(dir, ...)
}

# Shiny draws plots with ragg when it is installed. If ragg is installed but
# its compiled code does not load (common on Linux after an update of R or of
# system image libraries), every plot fails with "unable to load shared
# object". Fall back to the base png device and say how to repair ragg.
check_graphics <- function() {
  if (nzchar(system.file(package = "ragg")) && !requireNamespace("ragg", quietly = TRUE)) {
    options(shiny.useragg = FALSE)
    message("casimir: the 'ragg' package is installed but does not load, so plots use the ",
            "base png device. To repair it: install.packages(\"ragg\")")
  }
  if (!isTRUE(getOption("shiny.useragg", TRUE)) || !requireNamespace("ragg", quietly = TRUE)) {
    if (!any(grDevices::capabilities()[c("png", "cairo")]))
      warning("R on this computer cannot draw png files, so the app shows no plots. ",
              "On Linux install the cairo libraries (e.g. libcairo2-dev) and reinstall R ",
              "or install.packages(\"ragg\").", call. = FALSE)
  }
  invisible(TRUE)
}
