#' Export a survey (Cash out)
#'
#' Writes the frame survey, the design and the field forms of a sample as csv
#' files, with an optional answer key for trainers.
#'
#' @param sample A `cas_sample`.
#' @param dir Folder to write to (created).
#' @param answer_key The `cas_fishery` the sample was drawn from, to add an
#'   `answer_key/` folder (true catch by stratum, estimate vs truth, planted
#'   artefacts); `NULL` for none.
#' @return The paths written, invisibly, with attribute `dir`.
#' @export
export_survey <- function(sample, dir = "casimir_survey", answer_key = NULL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  w <- function(x, f) {
    p <- file.path(dir, f)
    utils::write.csv(x, p, row.names = FALSE)
    p
  }
  A <- sample$A
  A$date <- format(A$date)
  d <- sample$design
  paths <- c(
    w(sample$frame$boats, "frame_boats.csv"),
    w(sample$frame$days, "frame_days.csv"),
    w(A, "form_A_visits.csv"),
    w(sample$B, "form_B_landings.csv"),
    w(sample$C, "form_C_species.csv"))
  des <- file.path(dir, "design.txt")
  writeLines(c(
    paste("design:", d$name),
    paste("survey year:", sample$year),
    paste("site-days per site type and quarter:", paste(names(d$m), d$m, sep = "=", collapse = ", ")),
    paste("PSU selection:", d$psu_select),
    paste("landings per segment and visit:", paste(names(d$n), d$n, sep = "=", collapse = ", ")),
    paste("subsample (kg):", d$w_kg),
    "",
    "Site-days (M) in the frame by site type and quarter:",
    utils::capture.output(print(sample$M, row.names = FALSE))), des)
  readme <- file.path(dir, "README.txt")
  writeLines(c(
    "casimir survey data",
    "",
    "frame_boats.csv     boats by landing site and segment (frame survey)",
    "frame_days.csv      fishing days by quarter",
    "design.txt          the survey design and the number of site-days (M) per stratum",
    "form_A_visits.csv   one row per site-day visited and segment: N = landings counted",
    "form_B_landings.csv one row per sampled landing: W = landed kg, w = subsample kg, hours fished",
    "form_C_species.csv  species weights (kg) in each subsample",
    "",
    "Raise: W_ijs = w_ijs / w_ij * W_ij;  W_is = N_i / n_i * sum_j W_ijs;",
    "       W_hs = M_h / m_h * sum_i W_is;  W_s = sum_h W_hs"), readme)
  paths <- c(paths, des, readme)
  if (!is.null(answer_key)) {
    kd <- file.path(dir, "answer_key")
    dir.create(kd, showWarnings = FALSE)
    tr <- cas_truth(answer_key, years = sample$year)
    ce <- check_estimate(estimate_catch(sample), answer_key)
    pa <- planted_artefacts(sample)
    utils::write.csv(tr, file.path(kd, "true_catch_by_stratum.csv"), row.names = FALSE)
    utils::write.csv(ce, file.path(kd, "estimate_vs_truth.csv"), row.names = FALSE)
    utils::write.csv(pa, file.path(kd, "planted_artefacts.csv"), row.names = FALSE)
    paths <- c(paths, file.path(kd, c("true_catch_by_stratum.csv", "estimate_vs_truth.csv",
                                      "planted_artefacts.csv")))
  }
  attr(paths, "dir") <- dir
  invisible(paths)
}
