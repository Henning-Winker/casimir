#' Run a sampling programme over several years
#'
#' Rolls and raises the same design in each survey year (by default the last
#' five years of the fishery), giving a time series of catch estimates.
#'
#' @param fishery A `cas_fishery`.
#' @param design A `cas_design`.
#' @param years Survey years (default: the last 5).
#' @param artefacts A `cas_artefacts`, or `NULL`.
#' @param frame `"annual"` (a new frame survey every year) or `"first"` (the
#'   frame survey of the first survey year is used throughout, so it goes out
#'   of date as boats enter and leave).
#' @param route Estimation route (see [estimate_catch()]).
#' @param seed Seed.
#' @return A `cas_survey`: `samples` and `estimates` by year, `years`.
#' @export
#' @examples
#' \dontrun{
#' fish <- cas_fishery(island_x(), seed = 1)
#' sv <- run_survey(fish, cas_design())
#' plot_series(sv, fish)
#' plot_series(sv, fish, by = "segment")
#' }
run_survey <- function(fishery, design = cas_design(), years = utils::tail(fishery$years, 5),
                       artefacts = NULL, frame = c("annual", "first"), route = "psu", seed = 1) {
  frame <- match.arg(frame)
  smp <- lapply(years, function(y) {
    draw_sample(fishery, design, year = y, artefacts = artefacts,
                seed = seed_for(seed, paste0("year", y)),
                frame_year = if (frame == "first") min(years) else NULL)
  })
  est <- lapply(smp, estimate_catch, route = route)
  names(smp) <- names(est) <- years
  out <- list(samples = smp, estimates = est, years = years, design = design,
              artefacts = artefacts, frame = frame, route = route)
  class(out) <- "cas_survey"
  out
}

#' @export
print.cas_survey <- function(x, ...) {
  cat("<cas_survey>", x$design$name, "| years", paste(range(x$years), collapse = "-"),
      "| frame", x$frame, "| route", x$route, "\n")
  print(catch_by(x)[c("year", "species", "estimate", "cv")], row.names = FALSE)
  invisible(x)
}

#' Catch by species and segment (or other groups)
#'
#' Sums the stratum estimates (site type x quarter x segment) over the groups
#' not in `by`, with variances added over strata (strata are sampled
#' independently; segments within a site type and quarter share the visits,
#' so sums over segments are approximate).
#'
#' @param x A `cas_estimate` or a `cas_survey`.
#' @param by Grouping columns among `"segment"`, `"site_type"`, `"quarter"`;
#'   `NULL` for species totals.
#' @param fishery Optional `cas_fishery` to add the true catch (`true`,
#'   `rel_error`, `covered`).
#' @param level Confidence level.
#' @return A data frame with `year`, the groups, `species`, `estimate`, `se`,
#'   `cv`, `lower`, `upper` (kg), and the truth if `fishery` is given.
#' @export
catch_by <- function(x, by = NULL, fishery = NULL, level = 0.95) {
  if (inherits(x, "cas_survey")) {
    return(do.call(rbind, lapply(x$estimates, catch_by, by = by, fishery = fishery, level = level)))
  }
  st <- x$stratum
  st$var <- ifelse(is.na(st$var_between), 0, st$var_between)
  keys <- c(by, "species")
  agg <- stats::aggregate(st[c("estimate", "var")], st[keys], sum, na.rm = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  agg$se <- sqrt(agg$var)
  agg$cv <- agg$se / pmax(agg$estimate, 1e-12)
  agg$lower <- pmax(agg$estimate - z * agg$se, 0)
  agg$upper <- agg$estimate + z * agg$se
  agg$var <- NULL
  agg <- data.frame(year = x$year, agg, stringsAsFactors = FALSE)
  if (!is.null(fishery)) {
    tr <- cas_truth(fishery, years = x$year, by = c("year", by))
    agg$true <- tr$kg[match(do.call(paste, agg[c("year", keys)]), do.call(paste, tr[c("year", keys)]))]
    agg$true[is.na(agg$true)] <- 0
    agg$rel_error <- agg$estimate / agg$true - 1
    agg$covered <- agg$true >= agg$lower & agg$true <= agg$upper
  }
  agg <- agg[order(match(agg$species, x$species)), ]
  rownames(agg) <- NULL
  agg
}

#' Catch series against the truth
#'
#' Estimated catch by year with 95% intervals, one panel per species (or per
#' species and segment), and the true catch when `fishery` is given.
#'
#' @param x A `cas_survey`.
#' @param fishery Optional `cas_fishery` for the true catch.
#' @param by `NULL` (species totals) or `"segment"` / `"site_type"`.
#' @param species Species to show (default: all).
#' @return A ggplot.
#' @export
plot_series <- function(x, fishery = NULL, by = NULL, species = NULL) {
  d <- catch_by(x, by = by, fishery = fishery)
  if (!is.null(species)) d <- d[d$species %in% species, ]
  d$species <- factor(d$species, unique(d$species))
  p <- ggplot(d, aes(x = .data$year)) +
    geom_linerange(aes(ymin = .data$lower / 1000, ymax = .data$upper / 1000), colour = "#2c7fb8") +
    geom_point(aes(y = .data$estimate / 1000), colour = "#2c7fb8", size = 2)
  if (!is.null(fishery)) {
    p <- p + geom_line(aes(y = .data$true / 1000), colour = "black") +
      geom_point(aes(y = .data$true / 1000), shape = 4, size = 2.5)
  }
  p <- if (is.null(by)) p + facet_wrap(~species, scales = "free_y", ncol = 5)
       else p + ggplot2::facet_grid(stats::as.formula(paste("species ~", by[1])), scales = "free_y")
  p + labs(x = NULL, y = "Catch (t)",
           subtitle = if (is.null(fishery)) "Estimate with 95% interval"
                      else "Estimate with 95% interval (blue) and true catch (black)") +
    theme_bw(base_size = 11) +
    theme(strip.text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.5))
}
