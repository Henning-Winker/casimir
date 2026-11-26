#' True effort (number of landings) by stratum
#'
#' The answer key for effort: the number of landings (fishing trips) by
#' year, quarter, site type and segment, from the full census of landings.
#'
#' @param fishery A `cas_fishery`.
#' @param years Years (default: all).
#' @param by Grouping columns.
#' @return A data frame with `trips` per group.
#' @export
cas_effort_truth <- function(fishery, years = NULL,
                             by = c("year", "quarter", "site_type", "segment")) {
  l <- fishery$landings
  if (!is.null(years)) l <- l[l$year %in% years, ]
  l$trips <- 1
  out <- stats::aggregate(l["trips"], l[by], sum)
  rownames(out) <- NULL
  out
}

#' Effort by segment (or other groups)
#'
#' Sums the stratum estimates of effort (landings, i.e. fishing trips) over
#' the groups not in `by`, like [catch_by()] does for catch.
#'
#' @param x A `cas_estimate` or a `cas_survey`.
#' @param by Grouping columns among `"segment"`, `"site_type"`, `"quarter"`;
#'   `NULL` for the total.
#' @param fishery Optional `cas_fishery` to add the true effort.
#' @param level Confidence level.
#' @return A data frame with `year`, the groups, `estimate`, `se`, `cv`,
#'   `lower`, `upper` (trips), and `true`, `rel_error`, `covered` if
#'   `fishery` is given.
#' @export
effort_by <- function(x, by = "segment", fishery = NULL, level = 0.95) {
  if (inherits(x, "cas_survey")) {
    return(do.call(rbind, lapply(x$estimates, effort_by, by = by, fishery = fishery, level = level)))
  }
  ef <- x$effort
  ef$var[is.na(ef$var)] <- 0
  ef$one <- "all"
  keys <- if (length(by)) by else "one"
  agg <- stats::aggregate(ef[c("estimate", "var")], ef[keys], sum, na.rm = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  agg$se <- sqrt(agg$var)
  agg$cv <- agg$se / pmax(agg$estimate, 1e-12)
  agg$lower <- pmax(agg$estimate - z * agg$se, 0)
  agg$upper <- agg$estimate + z * agg$se
  agg$var <- NULL
  agg$one <- NULL
  agg <- data.frame(year = x$year, agg, stringsAsFactors = FALSE)
  if (!is.null(fishery)) {
    tr <- cas_effort_truth(fishery, years = x$year, by = c("year", by))
    k <- c("year", by)
    agg$true <- tr$trips[match(do.call(paste, agg[k]), do.call(paste, tr[k]))]
    agg$true[is.na(agg$true)] <- 0
    agg$rel_error <- agg$estimate / agg$true - 1
    agg$covered <- agg$true >= agg$lower & agg$true <= agg$upper
  }
  agg
}

#' Effort series against the truth
#'
#' Estimated effort (landings, i.e. fishing trips) by year with 95%
#' intervals, one panel per segment, and the true effort.
#'
#' @param x A `cas_survey`.
#' @param fishery Optional `cas_fishery` for the true effort.
#' @param by `"segment"` (default), `"site_type"` or `NULL` (total).
#' @return A ggplot.
#' @export
plot_effort_series <- function(x, fishery = NULL, by = "segment") {
  d <- effort_by(x, by = by, fishery = fishery)
  p <- ggplot(d, aes(x = .data$year)) +
    geom_linerange(aes(ymin = .data$lower / 1000, ymax = .data$upper / 1000), colour = "#d95f0e") +
    geom_point(aes(y = .data$estimate / 1000), colour = "#d95f0e", size = 2)
  if (!is.null(fishery)) {
    p <- p + geom_line(aes(y = .data$true / 1000), colour = "black") +
      geom_point(aes(y = .data$true / 1000), shape = 4, size = 2.5)
  }
  if (length(by)) p <- p + facet_wrap(stats::as.formula(paste("~", by[1])), scales = "free_y")
  p + labs(x = NULL, y = "Effort (thousand trips)",
           subtitle = if (is.null(fishery)) "Estimate with 95% interval"
                      else "Estimate with 95% interval (orange) and true effort (black)") +
    theme_bw(base_size = 11) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

#' Spin a sampling programme many times (Spin x1000 over the years)
#'
#' Runs the programme of [run_survey()] `R` times: in each replicate the same
#' design is rolled and raised in every survey year. The result holds the
#' estimates of every replicate by stratum, so the spread of the estimates
#' can be compared with the true catch and effort year by year.
#'
#' @inheritParams run_survey
#' @param R Number of replicates.
#' @param progress Optional function called with the replicate number.
#' @return A `cas_spins` with `catch` (one row per replicate, year, quarter,
#'   site type, segment and species: `estimate`, `se`, `true`, in kg),
#'   `effort` (the same without species: trips), and the settings.
#' @export
#' @examples
#' \dontrun{
#' fish <- cas_fishery(island_x(), seed = 1)
#' sp <- spin_series(fish, cas_design(), R = 50)
#' plot_spin_series(sp)
#' plot_spin_series(sp, what = "effort")
#' head(spins_long(sp))
#' }
spin_series <- function(fishery, design = cas_design(), years = utils::tail(fishery$years, 5),
                        R = 100, artefacts = NULL, frame = c("annual", "first"),
                        route = "psu", seed = 1, progress = NULL) {
  frame <- match.arg(frame)
  keys <- c("year", "quarter", "site_type", "segment")
  catch <- vector("list", R)
  effort <- vector("list", R)
  for (r in seq_len(R)) {
    if (!is.null(progress)) progress(r)
    sv <- run_survey(fishery, design, years = years, artefacts = artefacts, frame = frame,
                     route = route, seed = seed_for(seed, paste0("spin", r)))
    catch[[r]] <- do.call(rbind, lapply(sv$estimates, function(e) {
      st <- e$stratum
      data.frame(iter = r, year = e$year, st[c("quarter", "site_type", "segment", "species")],
                 estimate = st$estimate,
                 se = sqrt(ifelse(is.na(st$var_between), 0, st$var_between)),
                 stringsAsFactors = FALSE)
    }))
    effort[[r]] <- do.call(rbind, lapply(sv$estimates, function(e) {
      ef <- e$effort
      data.frame(iter = r, year = e$year, ef[c("quarter", "site_type", "segment")],
                 estimate = ef$estimate, se = sqrt(ifelse(is.na(ef$var), 0, ef$var)),
                 stringsAsFactors = FALSE)
    }))
  }
  catch <- do.call(rbind, catch)
  effort <- do.call(rbind, effort)
  tc <- cas_truth(fishery, years = years, by = keys)
  catch$true <- tc$kg[match(do.call(paste, catch[c(keys, "species")]),
                            do.call(paste, tc[c(keys, "species")]))]
  te <- cas_effort_truth(fishery, years = years, by = keys)
  effort$true <- te$trips[match(do.call(paste, effort[keys]), do.call(paste, te[keys]))]
  catch$true[is.na(catch$true)] <- 0
  effort$true[is.na(effort$true)] <- 0
  rownames(catch) <- rownames(effort) <- NULL
  out <- list(catch = catch, effort = effort, years = years, R = R, design = design,
              artefacts = artefacts, frame = frame, route = route, species = fishery$species)
  class(out) <- "cas_spins"
  out
}

#' @export
print.cas_spins <- function(x, ...) {
  cat("<cas_spins>", x$design$name, "|", x$R, "replicates | years",
      paste(range(x$years), collapse = "-"), "| frame", x$frame, "| route", x$route, "\n")
  print(summary_spins(x), row.names = FALSE)
  invisible(x)
}

# Totals per replicate and year (by species, or by the groups in `by`)
spin_totals <- function(x, what = c("catch", "effort"), by = NULL) {
  what <- match.arg(what)
  d <- x[[what]]
  keys <- c("iter", "year", by, if (what == "catch") "species")
  d$var <- d$se^2
  agg <- stats::aggregate(d[c("estimate", "var", "true")], d[keys], sum)
  agg$se <- sqrt(agg$var)
  agg$var <- NULL
  agg
}

#' Summary of a spun programme by year
#'
#' @param x A `cas_spins`.
#' @param what `"catch"` (by species) or `"effort"`.
#' @param by Optional grouping (`"segment"`, `"site_type"`, `"quarter"`).
#' @return A data frame by year (and group, species): `true`, `mean`
#'   estimate, the 2.5% and 97.5% quantiles of the estimates (`q025`,
#'   `q975`), relative bias, CV and coverage of the 95% intervals.
#' @export
summary_spins <- function(x, what = c("catch", "effort"), by = NULL) {
  what <- match.arg(what)
  tt <- spin_totals(x, what, by)
  keys <- c("year", by, if (what == "catch") "species")
  tt$covered <- abs(tt$estimate - tt$true) <= stats::qnorm(0.975) * tt$se
  g <- interaction(tt[keys], drop = TRUE, lex.order = TRUE)
  first <- !duplicated(g)
  s <- tt[first, keys, drop = FALSE]
  gi <- g[first]
  f <- function(v, fun) as.vector(tapply(v, g, fun))[match(gi, levels(g))]
  s$true <- f(tt$true, mean)
  s$mean <- f(tt$estimate, mean)
  s$q025 <- f(tt$estimate, function(v) stats::quantile(v, 0.025, names = FALSE))
  s$q975 <- f(tt$estimate, function(v) stats::quantile(v, 0.975, names = FALSE))
  s$rel_bias <- s$mean / s$true - 1
  s$cv <- f(tt$estimate, stats::sd) / s$true
  s$coverage <- f(tt$covered, mean)
  s <- s[do.call(order, s[rev(keys)]), ]
  if (what == "catch") s <- s[order(match(s$species, x$species), s$year), ]
  rownames(s) <- NULL
  s
}

#' Spun programme against the truth, year by year
#'
#' For each survey year: the middle 95% of the estimates over the
#' replicates (band or error bar), their mean, and the true catch or effort.
#'
#' @param x A `cas_spins`.
#' @param what `"catch"` (one panel per species) or `"effort"` (one panel
#'   per segment).
#' @param species Species to show (catch; default all).
#' @param type `"band"` (estimates as a band, truth as a line) or `"bars"`
#'   (paired bars of the mean estimate and the truth, with the 95% range of
#'   the estimates).
#' @param show_truth Draw the true catch or effort.
#' @return A ggplot.
#' @export
plot_spin_series <- function(x, what = c("catch", "effort"), species = NULL,
                             type = c("band", "bars"), show_truth = TRUE) {
  what <- match.arg(what)
  type <- match.arg(type)
  by <- if (what == "effort") "segment" else NULL
  s <- summary_spins(x, what, by)
  sc <- 1000
  if (what == "catch") {
    if (!is.null(species)) s <- s[s$species %in% species, ]
    s$panel <- factor(s$species, unique(s$species))
    ylab <- "Catch (t)"
    col <- "#2c7fb8"
  } else {
    s$panel <- s$segment
    ylab <- "Effort (thousand trips)"
    col <- "#d95f0e"
  }
  if (type == "band") {
    p <- ggplot(s, aes(x = .data$year)) +
      ggplot2::geom_ribbon(aes(ymin = .data$q025 / sc, ymax = .data$q975 / sc), fill = col, alpha = 0.25) +
      geom_line(aes(y = .data$mean / sc), colour = col, linewidth = 0.8) +
      geom_point(aes(y = .data$mean / sc), colour = col, size = 1.8)
    if (show_truth) p <- p + geom_line(aes(y = .data$true / sc), colour = "black", linetype = 2) +
      geom_point(aes(y = .data$true / sc), shape = 4, size = 2.5)
    sub <- paste0("Middle 95% of ", x$R, " replicates (band), their mean (line)",
                  if (show_truth) " and the truth (black, dashed)")
  } else {
    long <- rbind(data.frame(s[c("year", "panel")], what = "estimate", value = s$mean,
                             lo = s$q025, hi = s$q975),
                  if (show_truth) data.frame(s[c("year", "panel")], what = "true", value = s$true,
                                             lo = NA, hi = NA))
    long$what <- factor(long$what, c("estimate", "true"))
    dodge <- ggplot2::position_dodge(width = 0.8)
    p <- ggplot(long, aes(x = factor(.data$year), y = .data$value / sc, fill = .data$what)) +
      ggplot2::geom_col(position = dodge, width = 0.75) +
      ggplot2::geom_errorbar(aes(ymin = .data$lo / sc, ymax = .data$hi / sc), position = dodge,
                             width = 0.25, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = c(estimate = col, true = "grey35"),
                                 labels = c(estimate = "mean estimate", true = "true"), name = NULL)
    sub <- paste0("Mean of ", x$R, " replicates with the middle 95% of the estimates",
                  if (show_truth) ", and the truth")
  }
  p + facet_wrap(~panel, scales = "free_y", ncol = if (what == "catch") 5 else 3) +
    labs(x = NULL, y = ylab, subtitle = sub) +
    theme_bw(base_size = 11) +
    theme(strip.text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "top")
}

#' Estimates against the truth as one long table
#'
#' One row per replicate (`iter`), year, quarter, site type (area), segment
#' (fleet) and species, with the estimated and the true catch (kg); effort
#' rows (`variable = "effort"`, trips) have `species = NA`. Works for a single
#' sample's estimate, a survey over years, or a spun programme.
#'
#' @param x A `cas_estimate`, `cas_survey` or `cas_spins`.
#' @param fishery The `cas_fishery` (for the truth; not needed for
#'   `cas_spins`).
#' @param effort Include effort rows.
#' @return A data frame: `variable` (`"catch"` kg, `"effort"` trips), `iter`,
#'   `year`, `quarter`, `site_type`, `segment`, `species`, `estimate`, `se`,
#'   `true`.
#' @export
spins_long <- function(x, fishery = NULL, effort = TRUE) {
  if (inherits(x, "cas_estimate")) {
    sv <- list(estimates = list(x))
    x <- sv
    class(x) <- "cas_survey"
  }
  if (inherits(x, "cas_survey")) {
    if (is.null(fishery)) stop("`fishery` is needed for the truth.", call. = FALSE)
    keys <- c("year", "quarter", "site_type", "segment")
    catch <- do.call(rbind, lapply(x$estimates, function(e) {
      st <- e$stratum
      data.frame(iter = 1L, year = e$year, st[c("quarter", "site_type", "segment", "species")],
                 estimate = st$estimate, se = sqrt(ifelse(is.na(st$var_between), 0, st$var_between)),
                 stringsAsFactors = FALSE)
    }))
    eff <- do.call(rbind, lapply(x$estimates, function(e) {
      ef <- e$effort
      data.frame(iter = 1L, year = e$year, ef[c("quarter", "site_type", "segment")],
                 estimate = ef$estimate, se = sqrt(ifelse(is.na(ef$var), 0, ef$var)),
                 stringsAsFactors = FALSE)
    }))
    yrs <- unique(catch$year)
    tc <- cas_truth(fishery, years = yrs, by = keys)
    catch$true <- tc$kg[match(do.call(paste, catch[c(keys, "species")]), do.call(paste, tc[c(keys, "species")]))]
    te <- cas_effort_truth(fishery, years = yrs, by = keys)
    eff$true <- te$trips[match(do.call(paste, eff[keys]), do.call(paste, te[keys]))]
    catch$true[is.na(catch$true)] <- 0
    eff$true[is.na(eff$true)] <- 0
    x <- list(catch = catch, effort = eff)
  }
  cols <- c("variable", "iter", "year", "quarter", "site_type", "segment", "species",
            "estimate", "se", "true")
  out <- data.frame(variable = "catch", x$catch, stringsAsFactors = FALSE)[cols]
  if (effort) {
    e <- data.frame(variable = "effort", x$effort, species = NA_character_, stringsAsFactors = FALSE)
    out <- rbind(out, e[cols])
  }
  rownames(out) <- NULL
  out
}
