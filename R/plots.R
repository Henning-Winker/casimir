#' Week x site grid of a stratum (deck slide 6)
#'
#' Fishing weeks of one quarter (rows) by the landing sites of one site type
#' (columns): the sampling frame, with the visited site-days marked.
#' @param sample A `cas_sample`.
#' @param site_type Site type.
#' @param quarter Quarter.
#' @return A ggplot.
#' @export
plot_design_grid <- function(sample, site_type = NULL, quarter = 1) {
  st <- sample$frame$sites
  if (is.null(site_type)) site_type <- st$type[1]
  sites <- st$site[st$type == site_type]
  d <- quarter_days(sample$year, quarter, sample$frame$days_off)
  wk <- as.integer(format(d, "%U"))
  wk <- wk - min(wk) + 1
  grid <- expand.grid(week = sort(unique(wk)), site = sites, stringsAsFactors = FALSE)
  v <- sample$visits[sample$visits$site_type == site_type & sample$visits$quarter == quarter, ]
  vw <- as.integer(format(v$date, "%U")) - min(as.integer(format(d, "%U"))) + 1
  cnt <- table(factor(paste(vw, v$site), paste(grid$week, grid$site)))
  grid$visits <- as.integer(cnt)
  ggplot(grid, aes(x = .data$site, y = .data$week)) +
    geom_tile(aes(fill = .data$visits > 0), colour = "grey60") +
    geom_text(aes(label = ifelse(.data$visits > 0, ifelse(.data$visits > 1, paste0("X", .data$visits), "X"), "")),
              size = 3.5) +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#f4a259"), guide = "none") +
    scale_y_continuous(trans = "reverse", breaks = sort(unique(grid$week))) +
    labs(x = NULL, y = "Week of the quarter",
         title = paste0(site_type, ", quarter ", quarter),
         subtitle = paste(nrow(v), "of", length(d) * length(sites), "site-days visited")) +
    theme_bw() + theme(panel.grid = element_blank())
}

#' Estimates against the truth
#' @param estimate A `cas_estimate`.
#' @param fishery The `cas_fishery` (for the truth); `NULL` hides it.
#' @return A ggplot of the estimated catch by species with confidence
#'   intervals (and the true catch as a black dot).
#' @export
plot_estimate <- function(estimate, fishery = NULL) {
  t <- if (is.null(fishery)) estimate$total else check_estimate(estimate, fishery)
  t$species <- factor(t$species, rev(estimate$species))
  p <- ggplot(t, aes(y = .data$species)) +
    geom_linerange(aes(xmin = .data$lower / 1000, xmax = .data$upper / 1000), colour = "#2c7fb8",
                   linewidth = 1) +
    geom_point(aes(x = .data$estimate / 1000), colour = "#2c7fb8", size = 2.5)
  if (!is.null(fishery)) {
    p <- p + geom_point(aes(x = .data$true / 1000), shape = 4, size = 3, stroke = 1.2)
  }
  p + labs(x = "Catch (t)", y = NULL,
           subtitle = if (is.null(fishery)) "Estimate with 95% interval"
                      else "Estimate with 95% interval (blue) and true catch (x)") +
    theme_bw()
}

#' Bias, CV and coverage by species
#' @param x A `cas_eval`, or a data frame from [compare_designs()].
#' @return A ggplot with one panel per measure.
#' @export
plot_eval <- function(x) {
  s <- if (inherits(x, "cas_eval")) data.frame(design = x$design$name, x$summary) else x
  long <- rbind(
    data.frame(s[c("design", "species")], measure = "Relative bias (house edge)", value = s$rel_bias),
    data.frame(s[c("design", "species")], measure = "CV", value = s$cv),
    data.frame(s[c("design", "species")], measure = "Coverage of 95% intervals", value = s$coverage))
  ref <- data.frame(measure = c("Relative bias (house edge)", "CV", "Coverage of 95% intervals"),
                    y = c(0, 0, 0.95))
  long$species <- factor(long$species, rev(unique(s$species)))
  ggplot(long, aes(y = .data$species, x = .data$value, fill = .data$design)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_vline(data = ref, aes(xintercept = .data$y), linetype = 2) +
    facet_wrap(~measure, scales = "free_x") +
    labs(x = NULL, y = NULL, fill = "Design") + theme_bw() +
    theme(legend.position = "bottom")
}

#' Estimated against true catch, blocked by segment or stratum
#'
#' Estimate with 95% interval and the true catch for each species, in one
#' panel per block (segment, site type, quarter, or a combination).
#' @param x A `cas_estimate`.
#' @param by Grouping columns for [catch_by()], e.g. `"segment"` or
#'   `c("site_type", "segment")`; `NULL` for species totals.
#' @param fishery Optional `cas_fishery` for the truth.
#' @param species Species to show (default: all).
#' @return A ggplot.
#' @export
plot_catch_by <- function(x, by = "segment", fishery = NULL, species = NULL) {
  d <- catch_by(x, by = by, fishery = fishery)
  if (!is.null(species)) d <- d[d$species %in% species, ]
  d$species <- factor(d$species, rev(x$species))
  p <- ggplot(d, aes(y = .data$species)) +
    geom_linerange(aes(xmin = .data$lower / 1000, xmax = .data$upper / 1000), colour = "#2c7fb8",
                   linewidth = 1) +
    geom_point(aes(x = .data$estimate / 1000), colour = "#2c7fb8", size = 2.2)
  if (!is.null(fishery)) p <- p + geom_point(aes(x = .data$true / 1000), shape = 4, size = 2.8, stroke = 1.1)
  if (length(by)) p <- p + facet_wrap(stats::as.formula(paste("~", paste(by, collapse = " + "))),
                                      scales = "free_x")
  p + labs(x = "Catch (t)", y = NULL,
           subtitle = if (is.null(fishery)) "Estimate with 95% interval"
                      else "Estimate with 95% interval (blue) and true catch (x)") +
    theme_bw(base_size = 12)
}

#' How much of the fishery the sample saw
#'
#' The census and the sample at each stage: site-days, landings and kg, with
#' the raising factor between them.
#' @param sample A `cas_sample`.
#' @param fishery The `cas_fishery` (for the number of landings and kg in the
#'   census).
#' @return A ggplot.
#' @export
plot_funnel <- function(sample, fishery) {
  l <- fishery$landings[fishery$landings$year == sample$year, ]
  d <- data.frame(
    stage = factor(c("Site-days", "Landings", "Kg landed"),
                   c("Kg landed", "Landings", "Site-days")),
    census = c(sum(sample$M$M), nrow(l), sum(l$W)),
    sampled = c(nrow(sample$visits), nrow(sample$B), sum(sample$B$w)))
  f <- function(x) formatC(round(x), format = "d", big.mark = ",")
  d$label <- sprintf("%s of %s (1 in %s)", f(d$sampled), f(d$census), f(d$census / d$sampled))
  ggplot(d, aes(y = .data$stage)) +
    geom_col(aes(x = .data$census), fill = "#d9e6ef", width = 0.65) +
    geom_col(aes(x = .data$sampled), fill = "#f4a259", width = 0.65) +
    geom_text(aes(x = .data$census, label = .data$label), hjust = 1.05, size = 3.8) +
    ggplot2::scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(x = "Number (log scale)", y = NULL,
         subtitle = "Census (blue) and sample (orange): kg sampled is the subsample sorted by species") +
    theme_bw(base_size = 12)
}

#' Where the sampled boats fished
#'
#' Fishing grounds of all landings in the survey year (grey) and of the
#' landings sampled (coloured by segment), with the landing sites visited.
#' @param sample A `cas_sample`.
#' @param fishery The `cas_fishery`.
#' @return A ggplot.
#' @export
plot_sample_map <- function(sample, fishery) {
  tr <- fishery$trips
  all <- tr[tr$year == sample$year, ]
  cells <- stats::aggregate(list(n = all$trip), all[c("X", "Y")], length)
  sm <- tr[match(sample$B$trip, tr$trip), ]
  sm$segment <- sample$B$segment
  st <- fishery$sites
  st$visits <- as.integer(table(factor(sample$visits$site, st$site)))
  ggplot() +
    geom_tile(data = fishery$env, aes(x = .data$x, y = .data$y), fill = "#eef3f7") +
    geom_tile(data = cells, aes(x = .data$X, y = .data$Y, alpha = .data$n), fill = "grey40") +
    ggplot2::geom_jitter(data = sm, aes(x = .data$X, y = .data$Y, colour = .data$segment),
                         width = 0.3, height = 0.3, size = 1.6) +
    geom_point(data = st, aes(x = .data$x, y = .data$y, size = .data$visits), shape = 21,
               fill = "#f4a259", colour = "black") +
    geom_text(data = st, aes(x = .data$x + 1.2, y = .data$y, label = .data$site), size = 3, hjust = 0) +
    ggplot2::scale_alpha(range = c(0.05, 0.6), guide = "none") +
    ggplot2::scale_size_area(max_size = 6) +
    ggplot2::coord_equal(expand = FALSE) +
    labs(x = "x (offshore ->)", y = "y", colour = "Sampled landing", size = "Site-days visited",
         subtitle = "Grey: where all trips fished. Dots: where the sampled landings were caught") +
    theme_bw(base_size = 12)
}

#' The spin: distribution of the estimates around the truth
#'
#' Each replicate's estimate divided by the true catch, by species: the
#' spread is the precision, the offset of the median from 1 the bias (house
#' edge).
#' @param x A `cas_eval`, or a named list of them (one per design).
#' @return A ggplot.
#' @export
plot_spin <- function(x) {
  if (inherits(x, "cas_eval")) x <- stats::setNames(list(x), x$design$name)
  d <- do.call(rbind, lapply(names(x), function(nm) data.frame(design = nm, x[[nm]]$reps)))
  d$ratio <- d$estimate / d$true
  d$species <- factor(d$species, rev(unique(d$species)))
  dodge <- position_dodge(width = 0.75)
  ggplot(d, aes(x = .data$ratio, y = .data$species, fill = .data$design)) +
    ggplot2::geom_vline(xintercept = 1, colour = "black") +
    ggplot2::geom_violin(scale = "width", width = 0.8, alpha = 0.55, colour = NA, position = dodge) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, position = dodge, alpha = 0.9) +
    ggplot2::scale_x_continuous(limits = c(0, 3), oob = scales_squish,
                                breaks = c(0, 0.5, 1, 1.5, 2, 3),
                                labels = c("0", "0.5", "truth", "1.5", "2", "3+")) +
    labs(x = "Estimate / true catch", y = NULL, fill = "Design",
         subtitle = paste("Each spin is one survey;", max(d$rep), "spins per design")) +
    theme_bw(base_size = 12) + theme(legend.position = if (length(x) > 1) "bottom" else "none")
}

scales_squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  x[x < range[1]] <- range[1]
  x[x > range[2]] <- range[2]
  x
}
