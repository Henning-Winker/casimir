#' Maps of the island: species, effort and catch
#'
#' Spatial views of the simulated fishery on the island grid (cells of
#' 2.5 km; the coast and the landing sites are on the left, depth increases
#' offshore to the right):
#'
#' * `plot_species_map()`: where each species lives (unfished density,
#'   relative to its maximum), from depth, substrate and temperature;
#' * `plot_effort_map()`: where the fleet fished in a year (trips, or hours
#'   fished, per cell), by segment, quarter or tactic;
#' * `plot_catch_map()`: where the catch of each species was taken (kg per
#'   cell), optionally by segment or quarter.
#'
#' Landing sites are drawn on every map, coloured by site type.
#'
#' @param fishery A `cas_fishery`.
#' @param species Species to show (default: all).
#' @param year Year (default: the last).
#' @param by Facet by `"segment"`, `"quarter"` or `"tactic"`; `NULL` for none.
#' @param value For effort: `"trips"` or `"hours"`.
#' @param ncol Number of facet columns.
#' @return A ggplot.
#' @name island_maps
NULL

map_cells <- function(fishery, year) {
  tr <- fishery$trips
  tr <- tr[tr$year == year, ]
  l <- fishery$landings
  i <- match(tr$trip, l$trip)
  tr$segment <- l$segment[i]
  tr$quarter <- l$quarter[i]
  list(trips = tr, landings = l[i, ])
}

map_sites <- function(fishery) {
  st <- fishery$sites
  list(
    geom_point(data = st, aes(x = .data$x, y = .data$y, shape = .data$type), inherit.aes = FALSE,
               size = 2.6, fill = "#f4a259", colour = "black"),
    ggplot2::scale_shape_manual(values = c(Beach = 21, Harbour = 22, Jetty = 23), name = "Site type"))
}

map_theme <- function(fishery) {
  list(
    ggplot2::coord_equal(xlim = c(0.5, max(fishery$env$x) + 0.5), ylim = c(0.5, max(fishery$env$y) + 0.5),
                         expand = FALSE),
    labs(x = "coast (left) to offshore (right)", y = NULL),
    theme_bw(base_size = 11),
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          strip.text = element_text(size = 9), strip.background = ggplot2::element_rect(fill = "#eef3f7")))
}

#' @rdname island_maps
#' @export
plot_species_map <- function(fishery, species = NULL, ncol = 5) {
  d <- fishery$density
  if (is.null(d)) stop("This fishery has no density layer; rebuild it with cas_fishery().", call. = FALSE)
  if (!is.null(species)) d <- d[d$species %in% species, ]
  d$rel <- stats::ave(d$density, d$species, FUN = function(x) x / max(x))
  d$species <- factor(d$species, intersect(fishery$species, unique(d$species)))
  ggplot(d, aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(aes(fill = .data$rel)) +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1, name = "Relative\ndensity",
                                  limits = c(0, 1)) +
    map_sites(fishery) +
    facet_wrap(~species, ncol = ncol) +
    map_theme(fishery) +
    labs(subtitle = "Where each species lives (unfished density, relative to its maximum)")
}

#' @rdname island_maps
#' @export
plot_effort_map <- function(fishery, year = max(fishery$years), by = "segment",
                            value = c("trips", "hours"), ncol = NULL) {
  value <- match.arg(value)
  tr <- map_cells(fishery, year)$trips
  grp <- c("X", "Y", by)
  tr$v <- if (value == "trips") 1 else tr$hours
  agg <- stats::aggregate(list(v = tr$v), tr[grp], sum)
  if (!is.null(by)) agg[[by]] <- factor(agg[[by]], if (by == "segment") fishery$segments else sort(unique(agg[[by]])))
  p <- ggplot(agg, aes(x = .data$X, y = .data$Y)) +
    ggplot2::geom_tile(data = fishery$env, aes(x = .data$x, y = .data$y), fill = "#f2f5f7") +
    ggplot2::geom_tile(aes(fill = .data$v)) +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1, trans = "sqrt",
                                  name = if (value == "trips") "Trips" else "Hours") +
    map_sites(fishery) + map_theme(fishery) +
    labs(subtitle = paste0("Where the fleet fished in ", year, " (", value, " per cell",
                           if (!is.null(by)) paste0(", by ", sub("_", " ", by)), ")"))
  if (!is.null(by)) p <- p + facet_wrap(stats::as.formula(paste("~", by)), ncol = ncol)
  p
}

#' @rdname island_maps
#' @export
plot_catch_map <- function(fishery, species = NULL, year = max(fishery$years), by = NULL,
                           ncol = 5) {
  mc <- map_cells(fishery, year)
  sp <- if (is.null(species)) fishery$species else species
  tr <- mc$trips
  l <- mc$landings
  long <- do.call(rbind, lapply(sp, function(s) {
    data.frame(X = tr$X, Y = tr$Y, segment = tr$segment, quarter = tr$quarter,
               species = s, kg = l[[s]], stringsAsFactors = FALSE)
  }))
  grp <- c("X", "Y", "species", by)
  agg <- stats::aggregate(list(kg = long$kg), long[grp], sum)
  agg <- agg[agg$kg >= 1, ]   # traces below 1 kg per cell are not shown
  agg$species <- factor(agg$species, sp)
  if (identical(by, "segment")) agg$segment <- factor(agg$segment, fishery$segments)
  p <- ggplot(agg, aes(x = .data$X, y = .data$Y)) +
    ggplot2::geom_tile(data = fishery$env, aes(x = .data$x, y = .data$y), fill = "#f2f5f7") +
    ggplot2::geom_tile(aes(fill = .data$kg)) +
    ggplot2::scale_fill_viridis_c(option = "rocket", direction = -1, trans = "log10",
                                  name = "Catch (kg)",
                                  labels = function(x) formatC(x, format = "d", big.mark = ",")) +
    map_sites(fishery) + map_theme(fishery) +
    labs(subtitle = paste0("Where the catch was taken in ", year, " (kg per cell, log scale; cells with at least 1 kg)"))
  if (is.null(by)) p + facet_wrap(~species, ncol = ncol)
  else p + ggplot2::facet_grid(stats::as.formula(paste("species ~", by)))
}
