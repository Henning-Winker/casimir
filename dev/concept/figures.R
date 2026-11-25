# Figures and numbers for the concept page (pkgdown/assets/concept.html).
# Run from the package root: Rscript dev/concept/figures.R, then
# python3 dev/concept/build.py
library(casimir); library(ggplot2)
out <- "dev/concept/fig"; dir.create(out, showWarnings = FALSE)
fish <- cas_fishery(island_x(), seed = 1)
yr <- max(fish$years)
d <- cas_design(m = 6, n = 3, w_kg = 10, name = "A")
s <- draw_sample(fish, d, year = yr, seed = 2)
e <- estimate_catch(s)
sv <- run_survey(fish, d, frame = "first", seed = 4)
a <- replicate_design(fish, d, R = 200, seed = 3)
b <- replicate_design(fish, cas_design(m = 8, n = 1, w_kg = 10, name = "B"), R = 200, seed = 3)
sw <- replicate_design(fish, d, R = 200, seed = 3, artefacts = artefacts(short_watch = 13))
sv1 <- function(p, f, w, h) ggsave(file.path(out, f), p, width = w, height = h, dpi = 96, bg = "white")
sv1(plot_species_map(fish, ncol = 5), "species.png", 10, 4.6)
sv1(plot_effort_map(fish, by = "segment"), "effort.png", 10, 3.8)
sv1(plot_catch_map(fish, species = c("Scomber colias", "Thunnus alalunga", "Merluccius merluccius"), by = "segment"), "catch.png", 7.5, 7)
sv1(plot_design_grid(s, "Beach", quarter = 1), "grid.png", 5.5, 5.5)
sv1(plot_funnel(s, fish), "funnel.png", 8, 2.6)
sv1(plot_sample_map(s, fish), "samplemap.png", 7, 6.2)
sv1(plot_catch_by(e, by = "segment", fishery = fish), "bysegment.png", 10, 4.6)
sv1(plot_spin(list(A = a, B = b)), "spin.png", 8.5, 6.4)
sv1(plot_series(sv, fish), "series.png", 10, 4.4)
sv1(plot_spin(list(A = a, `A, short watch` = sw)), "shortwatch.png", 8.5, 6.4)
l <- fish$landings[fish$landings$year == yr, ]
fr <- frame_survey(fish, yr)
fb <- stats::xtabs(boats ~ site_type + segment, fr$boats)
nums <- list(
  year = yr, years = paste(range(fish$years), collapse = "-"),
  landings = nrow(l), boats = sum(fr$boats$boats), sites = nrow(fish$sites),
  species = length(fish$species), kg = round(sum(l$W) / 1000),
  M = sum(s$M$M), m = nrow(s$visits), n = nrow(s$B), w = round(sum(s$B$w)),
  frame = lapply(rownames(fb), function(t) c(list(type = t), as.list(fb[t, ]))),
  days = fr$days$days, cost = round(design_cost(d, s)),
  cv_main = round(100 * a$summary$cv[a$summary$species == "Scomber colias"]),
  cv_rare = round(100 * a$summary$cv[a$summary$species == "Dentex dentex"]),
  bias_max = round(100 * max(abs(a$summary$rel_bias[a$summary$true > stats::quantile(a$summary$true, .3)])), 1),
  cost_a = round(a$cost), cost_b = round(b$cost))
writeLines(jsonlite_like <- paste0("{", paste(sprintf('"%s": %s', names(nums), vapply(nums, function(x) {
  if (is.list(x)) paste0("[", paste(vapply(x, function(r) paste0("{", paste(sprintf('"%s": %s', names(r), vapply(r, function(v) if (is.character(v)) sprintf('"%s"', v) else as.character(v), "")), collapse = ", "), "}"), ""), collapse = ", "), "]")
  else if (is.character(x)) sprintf('"%s"', x) else if (length(x) > 1) paste0("[", paste(x, collapse = ", "), "]") else as.character(x)
}, "")), collapse = ",\n"), "}"), file.path(out, "numbers.json"))
cat(readLines(file.path(out, "numbers.json")), sep = "\n")
