#' Repeat a design many times (Spin x1000)
#'
#' Draws `R` samples from the same fishery with the same design (and
#' artefacts), raises each, and compares the totals by species with the true
#' landings.
#'
#' @param fishery A `cas_fishery`.
#' @param design A `cas_design`.
#' @param R Number of replicates.
#' @param artefacts A `cas_artefacts`, or `NULL`.
#' @param route Estimation route (see [estimate_catch()]).
#' @param year Survey year.
#' @param seed Seed; replicate r uses `seed_for(seed, paste0("rep", r))`.
#' @param progress Optional function called with the replicate number.
#' @return A `cas_eval`: `reps` (one row per replicate and species), `summary`
#'   by species (true, mean estimate, relative bias, CV, coverage, RMSE),
#'   `cost`, and the design.
#' @export
replicate_design <- function(fishery, design = cas_design(), R = 100, artefacts = NULL,
                             route = "psu", year = max(fishery$years), seed = 1,
                             progress = NULL) {
  tr <- cas_truth(fishery, years = year, by = "year")
  cost <- numeric(R)
  reps <- do.call(rbind, lapply(seq_len(R), function(r) {
    if (!is.null(progress)) progress(r)
    smp <- draw_sample(fishery, design, year, artefacts, seed = seed_for(seed, paste0("rep", r)))
    cost[r] <<- design_cost(design, smp)
    e <- estimate_catch(smp, route = route)$total
    data.frame(rep = r, e[c("species", "estimate", "se", "lower", "upper")])
  }))
  reps$true <- tr$kg[match(reps$species, tr$species)]
  reps$covered <- reps$true >= reps$lower & reps$true <= reps$upper
  sp <- fishery$species
  f <- factor(reps$species, sp)
  summ <- data.frame(
    species = sp,
    true = tr$kg[match(sp, tr$species)],
    mean = as.vector(tapply(reps$estimate, f, mean)),
    sd = as.vector(tapply(reps$estimate, f, stats::sd)),
    mean_se = as.vector(tapply(reps$se, f, mean)),
    coverage = as.vector(tapply(reps$covered, f, mean)),
    stringsAsFactors = FALSE)
  summ$rel_bias <- summ$mean / summ$true - 1
  summ$cv <- summ$sd / summ$true
  summ$rmse <- sqrt(as.vector(tapply((reps$estimate - reps$true)^2, f, mean))) / summ$true
  out <- list(reps = reps, summary = summ, cost = mean(cost), design = design,
              artefacts = artefacts, route = route, R = R, year = year)
  class(out) <- "cas_eval"
  out
}

#' @export
print.cas_eval <- function(x, ...) {
  cat("<cas_eval>", x$design$name, "|", x$R, "replicates | route", x$route,
      "| cost", round(x$cost), "enumerator-hours\n")
  s <- x$summary
  s$true <- round(s$true / 1000, 2)
  s[c("rel_bias", "cv", "coverage", "rmse")] <- round(s[c("rel_bias", "cv", "coverage", "rmse")], 3)
  print(s[c("species", "true", "rel_bias", "cv", "coverage", "rmse")], row.names = FALSE)
  invisible(x)
}

#' Compare designs on the same fishery
#'
#' @param fishery A `cas_fishery`.
#' @param designs A named list of `cas_design` objects.
#' @param ... Passed to [replicate_design()] (`R`, `artefacts`, `route`, `seed`).
#' @return A data frame of the summaries, with columns `design` and `cost`.
#' @export
compare_designs <- function(fishery, designs, ...) {
  do.call(rbind, lapply(names(designs), function(nm) {
    d <- designs[[nm]]
    d$name <- nm
    e <- replicate_design(fishery, d, ...)
    data.frame(design = nm, cost = e$cost, e$summary, stringsAsFactors = FALSE)
  }))
}

#' Days or boats? Designs of about equal cost
#'
#' Builds designs that spend a similar budget on more site-days with fewer
#' landings each, or fewer site-days with more landings each.
#' @param budget_m PSUs per site type and quarter for the "days" end.
#' @param n Landings per segment and visit at the "days" end.
#' @param steps Number of designs.
#' @return A named list of `cas_design` objects.
#' @export
days_vs_boats <- function(budget_m = 16, n = 1, steps = 4) {
  cc <- cas_design()$cost
  per_visit <- function(nn) cc[["psu"]] + 3 * nn * (cc[["landing"]] + cc[["kg"]] * 10)
  budget <- budget_m * per_visit(n)
  ns <- unique(round(seq(n, n * 2^(steps - 1), length.out = steps)))
  out <- lapply(ns, function(nn) {
    cas_design(m = max(2, round(budget / per_visit(nn))), n = nn,
               name = sprintf("m%d_n%d", max(2, round(budget / per_visit(nn))), nn))
  })
  stats::setNames(out, vapply(out, `[[`, "", "name"))
}
