# Short explanations of settings, shown as tooltips in the app and usable in
# documentation. One sentence or two each; keep them plain.

casimir_tips_list <- list(
  # island
  segments = "Fishing-unit types. Each has a home landing site, a fishing range and home tactics.",
  range = "How far a segment fishes from its home site (cells of 2.5 km). Canoes stay close; large motor boats go offshore.",
  site_mix = "Which segments are based at which type of landing site.",
  seed = "The same seed gives the same fishery and the same samples every time.",
  # frame
  frame = "The frame survey: boats by site and segment, and fishing days. It is the list from which the survey is planned.",
  old_frame = "A frame survey from earlier years misses new boats and keeps boats that left. Only the CPUE x effort route uses boat numbers.",
  # design
  psu = "A PSU (primary sampling unit) here is one landing site on one fishing day. M is the number of site-days in the stratum, m the number you visit.",
  strata = "Strata are site type x quarter, and segment within each visit. Each stratum is raised on its own.",
  m = "Site-days visited per site type and quarter.",
  n = "Landings sampled per segment at each visit. N, the landings counted, is recorded for every visit.",
  w_kg = "Weight of the species subsample taken from each sampled landing. The whole landing if it is smaller.",
  psu_select = "Simple random: any site-days. Systematic: spread evenly over the quarter, as in the week x site grid.",
  cost = "Enumerator-hours: per visit, per landing sampled and per kg sorted.",
  # raising
  raise = "Raising goes up one stage at a time: subsample -> landing (W/w), landing -> site-day (N/n), site-day -> stratum (M/m).",
  zeros = "A day with no landings is data: its catch is zero. Dropping it makes M/m raise only the busy days.",
  route = "PSU route: raise by site-days. Effort route: frame boats x days x activity x catch per landing.",
  # evaluation
  days_vs_boats = "Landings on the same day at the same site are alike. An extra day usually buys more precision than extra boats on the same day.",
  house_edge = "Bias (the house edge) does not shrink with more samples. Variance does.",
  coverage = "Share of 95% intervals that contain the true catch. Well below 0.95 means the intervals are too narrow or the estimate is biased.",
  betting_slip = "Write your prediction down before you roll. The point is to compare it with what happens.",
  # artefacts
  high_roller = "The enumerator picks the big boats. Sampled landings are larger than the site's typical landing.",
  short_watch = "The enumerator leaves early. Late landings (long-range boats) are neither counted nor sampled.",
  easy_sites = "Only some sites are visited, but M counts all of them.",
  dropped_zeros = "Visits with no landings are not written down.",
  top_of_box = "The subsample is taken from the top of the catch, where large and valuable fish lie.",
  hide_truth = "Hide the true catch and the answer key, e.g. while presenting to trainees."
)

#' Short explanations of settings
#'
#' The texts shown when hovering over the (i) symbols in the app.
#' @param id Tip id; `NULL` returns all tips as a named list.
#' @return A character string, or a named list.
#' @export
#' @examples
#' casimir_tip("psu")
casimir_tip <- function(id = NULL) {
  if (is.null(id)) return(casimir_tips_list)
  casimir_tips_list[[id]] %||% ""
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
