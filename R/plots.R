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
