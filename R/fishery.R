#' Island X: the default training fishery
#'
#' Settings for [cpueTargeting::simulate_logbook()] with landing sites, after
#' the island of the FAO training deck: 9 landing sites of 3 types (Harbour,
#' Beach, Jetty) and three segments (fishing-unit types) with different
#' ranges, home tactics and site preferences:
#'
#' | Segment | Boats | Range (cells, 2.5 km) | Home tactics | Based at |
#' |---|---|---|---|---|
#' | canoe | 150 | 5 | Reef | beaches, jetties |
#' | small_motor | 60 | 12 | Reef, Sediment | all site types |
#' | large_motor | 15 | 25 | Pelagic, Sediment | harbours, some jetties |
#'
#' The survey runs in the last year; earlier years are burn-in for depletion
#' and fleet change.
#'
#' @param boats Number of boats per segment.
#' @param range Fishing range per segment, in cells.
#' @param trips Trips per year (all segments).
#' @param n_years Number of years simulated.
#' @param start_year First calendar year.
#' @return A settings list for [cas_fishery()].
#' @export
island_x <- function(boats = c(canoe = 150, small_motor = 60, large_motor = 15),
                     range = c(canoe = 5, small_motor = 12, large_motor = 25),
                     trips = 16000, n_years = 5, start_year = 2021) {
  s <- cpueTargeting::logbook_settings()
  seg <- names(boats)
  s$n_years <- n_years
  s$start_year <- start_year
  s$vessel_types <- boats
  s$home <- list(canoe = "Reef", small_motor = c("Reef", "Sediment"),
                 large_motor = c("Pelagic", "Sediment"))[seg]
  s$loyalty <- c(canoe = 0.95, small_motor = 0.7, large_motor = 0.6)[seg]
  s$trips <- rep(trips, n_years)
  s$hours_mean <- 6
  s$closure <- NULL
  s$entry_share <- 0.15
  s$exit_share <- 0.1
  s$logbook$two_sets <- 0.15
  s$logbook$kg_trip <- 25
  l <- cpueTargeting::landing_settings(vessel_types = seg, range = range)
  l$site_mix <- matrix(c(0.1, 1, 0.6,
                         1, 0.6, 1,
                         1, 0, 0.3), 3, 3,
                       dimnames = list(c("Harbour", "Beach", "Jetty"),
                                       c("canoe", "small_motor", "large_motor")))[, seg, drop = FALSE]
  l$calendar$depart <- c(4, 7)
  s$landing <- l
  s
}

#' Simulate the fishery (the population of landings)
#'
#' Runs [cpueTargeting::simulate_logbook()] with landing sites and keeps what
#' the survey needs: every trip with its site, day, segment and true landed
#' weight by species (`landings`), the sites and the fleet (`vessels`).
#'
#' @param settings Settings from [island_x()] (must include `landing`).
#' @param seed Seed.
#' @return An object of class `cas_fishery`: `landings`, `sites`, `vessels`,
#'   `species`, `segments`, `years`, `env`, `plots`, `settings`, `seed`.
#' @export
#' @examples
#' \dontrun{
#' fish <- cas_fishery(island_x(), seed = 1)
#' fish
#' plot(fish)
#' }
cas_fishery <- function(settings = island_x(), seed = 1) {
  if (is.null(settings$landing)) stop("`settings$landing` is needed (see island_x()).",
                                      call. = FALSE)
  sim <- with_seed(seed_for(seed, "fishery"),
                   cpueTargeting::simulate_logbook(settings, plots = FALSE))
  sp <- settings$species$species
  l <- sim$landings
  l$segment <- l$vessel_type
  l$key <- paste(l$site, l$date)
  out <- list(landings = l, sites = sim$sites, vessels = sim$vessels,
              species = sp, segments = names(settings$vessel_types),
              years = sort(unique(l$year)), env = sim$env, trips = sim$trips,
              truth_biomass = sim$truth, settings = sim$settings, seed = seed)
  class(out) <- "cas_fishery"
  out
}

#' @export
print.cas_fishery <- function(x, ...) {
  l <- x$landings
  cat("<cas_fishery>", nrow(l), "landings,", length(unique(l$vessel)), "boats,",
      nrow(x$sites), "sites,", length(x$species), "species, years",
      paste(range(x$years), collapse = "-"), "\n")
  print(table(site = l$site, segment = l$segment)[, x$segments])
  invisible(x)
}

#' @export
plot.cas_fishery <- function(x, ...) {
  sim <- list(sites = x$sites, vessels = x$vessels, settings = x$settings,
              trips = x$trips, env = x$env)
  cpueTargeting::plot_landing_sites(sim)
}

#' True landings by species and stratum
#'
#' The answer key: true landed weight (kg) by year, quarter, site type,
#' segment and species, from the full census of landings.
#'
#' @param fishery A `cas_fishery`.
#' @param years Years (default: all).
#' @param by Grouping columns.
#' @return A long data frame with `kg` per group and species.
#' @export
cas_truth <- function(fishery, years = NULL,
                      by = c("year", "quarter", "site_type", "segment")) {
  l <- fishery$landings
  if (!is.null(years)) l <- l[l$year %in% years, ]
  sp <- fishery$species
  agg <- stats::aggregate(l[sp], l[by], sum)
  long <- do.call(rbind, lapply(sp, function(s) {
    data.frame(agg[by], species = s, kg = agg[[s]], stringsAsFactors = FALSE)
  }))
  rownames(long) <- NULL
  long
}
