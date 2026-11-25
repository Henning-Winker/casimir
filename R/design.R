#' Frame survey
#'
#' A census of boats by landing site and segment in a reference year (boats
#' active in that year), and of fishing days by quarter in the survey year.
#' An old frame (`ref_year` before the survey year) misses boats that entered
#' since and keeps boats that left, which biases the CPUE x effort route.
#'
#' @param fishery A `cas_fishery`.
#' @param year Survey year (default: last year).
#' @param ref_year Year of the frame survey (default: `year`).
#' @return A `cas_frame`: `boats` (site, site_type, segment, boats), `days`
#'   (quarter, fishing days), `sites`, `year`, `ref_year`.
#' @export
frame_survey <- function(fishery, year = max(fishery$years), ref_year = year) {
  v <- fishery$vessels
  v <- v[v$entry <= ref_year & v$exit >= ref_year, ]
  st <- fishery$sites
  grid <- expand.grid(site = st$site, segment = fishery$segments, stringsAsFactors = FALSE)
  grid$site_type <- st$type[match(grid$site, st$site)]
  n <- table(factor(v$site, st$site), factor(v$vessel_type, fishery$segments))
  grid$boats <- as.integer(n[cbind(grid$site, grid$segment)])
  off <- fishery$settings$landing$calendar$days_off
  days <- data.frame(quarter = 1:4,
                     days = vapply(1:4, function(q) length(quarter_days(year, q, off)), 1L))
  out <- list(boats = grid[c("site", "site_type", "segment", "boats")], days = days,
              sites = st, year = year, ref_year = ref_year, days_off = off)
  class(out) <- "cas_frame"
  out
}

#' @export
print.cas_frame <- function(x, ...) {
  cat("<cas_frame> survey year", x$year, "| frame survey", x$ref_year, "\n")
  b <- stats::xtabs(boats ~ site + segment, x$boats)
  print(b)
  cat("Fishing days by quarter:", paste(x$days$days, collapse = ", "), "\n")
  invisible(x)
}

#' Survey design
#'
#' A stratified multi-stage design as in the FAO training: strata are site
#' type x quarter (x segment, as post-strata within each visit); the PSU is a
#' landing site on a fishing day; within a visit the enumerator counts the
#' landings of each segment (N) and samples `n` of them, weighs each sampled
#' landing (W), takes a species subsample (w) and splits it by species.
#'
#' @param m PSUs (site-days) per site type and quarter: a single number, or a
#'   named vector by site type.
#' @param n Landings sampled per segment and visit (a number, or named by
#'   segment).
#' @param psu_select `"srs"` (simple random) or `"systematic"` (spread over
#'   the quarter in date order, as the week x site grid of the deck).
#' @param w_kg Target subsample weight (kg); the whole landing if smaller.
#' @param unit_kg Basket size (kg) of the subsample model.
#' @param cost Costs per PSU visit, per landing sampled and per kg subsampled
#'   (in enumerator-hours).
#' @param name Label.
#' @return A `cas_design`.
#' @export
cas_design <- function(m = 6, n = 3, psu_select = c("srs", "systematic"),
                       w_kg = 10, unit_kg = 0.5,
                       cost = c(psu = 4, landing = 0.5, kg = 0.05), name = "design") {
  psu_select <- match.arg(psu_select)
  out <- list(m = m, n = n, psu_select = psu_select, w_kg = w_kg, unit_kg = unit_kg,
              cost = cost, name = name)
  class(out) <- "cas_design"
  out
}

#' @export
print.cas_design <- function(x, ...) {
  cat("<cas_design>", x$name, "\n")
  cat("  PSUs per site type x quarter:", paste(x$m, collapse = ", "), "(", x$psu_select, ")\n")
  cat("  landings per segment and visit:", paste(x$n, collapse = ", "), "\n")
  cat("  subsample:", x$w_kg, "kg in", x$unit_kg, "kg baskets\n")
  invisible(x)
}

#' Design of the training deck
#'
#' Roughly the slide-6 sampling intensity: 13 weeks x 3 sites per stratum
#' with about 20 site-days marked, i.e. `m = 20` per site type and quarter,
#' spread over the weeks.
#' @param ... Passed to [cas_design()].
#' @export
deck_design <- function(...) {
  cas_design(m = 20, n = 3, psu_select = "systematic", name = "deck", ...)
}

#' Frame of PSUs (site x fishing day) by site type and quarter
#' @noRd
psu_frame <- function(frame) {
  st <- frame$sites
  do.call(rbind, lapply(1:4, function(q) {
    d <- quarter_days(frame$year, q, frame$days_off)
    g <- expand.grid(date = d, site = st$site, stringsAsFactors = FALSE)
    g$site_type <- st$type[match(g$site, st$site)]
    g$quarter <- q
    g[order(g$site_type, g$date, g$site), c("site_type", "quarter", "site", "date")]
  }))
}

#' Design cost
#' @param design A `cas_design`.
#' @param sample Optional `cas_sample` (realised cost); otherwise planned.
#' @param fishery Needed for the planned cost (site types, segments).
#' @return Cost in enumerator-hours.
#' @export
design_cost <- function(design, sample = NULL, fishery = NULL) {
  cc <- design$cost
  if (!is.null(sample)) {
    return(unname(cc[["psu"]] * nrow(sample$visits) + cc[["landing"]] * nrow(sample$B) +
                    cc[["kg"]] * sum(sample$B$w)))
  }
  types <- unique(fishery$sites$type)
  m <- rep_len(design$m, length(types))
  n <- rep_len(design$n, length(fishery$segments))
  unname(4 * sum(m) * (cc[["psu"]] + cc[["landing"]] * sum(n) +
                         cc[["kg"]] * sum(n) * design$w_kg))
}
