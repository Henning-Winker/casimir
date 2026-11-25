#' Raise a sample to catch per species (Raise)
#'
#' The estimators of the FAO training, one stage at a time:
#'
#' 1. species s in landing j of PSU i: `W_ijs = w_ijs / sum_s(w_ijs) * W_ij`;
#' 2. species s in PSU i (per segment): `W_is = N_i / n_i * sum_j W_ijs`;
#' 3. species s in stratum h (site type x quarter x segment):
#'    `W_hs = M_h / m_h * sum_i W_is`;
#' 4. total: sum over strata.
#'
#' The variance of step 3 is the ultimate-cluster estimator with the stage-1
#' finite population correction, `M^2 (1 - m/M) s^2 / m`, where `s^2` is the
#' variance of the PSU totals; `within` adds the second-stage term
#' `M/m sum_i N_i^2 (1 - n_i/N_i) s_i^2 / n_i`.
#'
#' `route = "effort"` estimates instead catch = effort x CPUE, with effort =
#' frame boats x fishing days x boat activity (landings counted per frame
#' boat and visit) and CPUE the mean step-1 catch per landing in the stratum.
#'
#' @param sample A `cas_sample`.
#' @param route `"psu"` (raising by PSUs) or `"effort"` (CPUE x effort).
#' @param level Confidence level.
#' @return A `cas_estimate` with `landing` (step 1, wide), `psu` (step 2,
#'   long), `stratum` (step 3, long) and `total` (by species: estimate, se,
#'   cv, lower, upper).
#' @export
estimate_catch <- function(sample, route = c("psu", "effort"), level = 0.95) {
  route <- match.arg(route)
  sp <- sample$species
  A <- sample$A
  B <- sample$B
  C <- sample$C
  z <- stats::qnorm(1 - (1 - level) / 2)
  # Step 1
  wmat <- matrix(0, nrow(B), length(sp), dimnames = list(NULL, sp))
  if (nrow(C)) {
    wmat[cbind(match(paste(C$psu, C$trip), paste(B$psu, B$trip)), match(C$species, sp))] <- C$w_s
  }
  tot <- rowSums(wmat)
  step1 <- wmat / ifelse(tot > 0, tot, 1) * B$W
  landing <- data.frame(B[c("psu", "trip", "segment", "W", "w")], step1, check.names = FALSE)
  # Step 2 (all visits x segments; N = 0 gives zero catch)
  key_b <- paste(B$psu, B$segment)
  key_a <- paste(A$psu, A$segment)
  n_i <- as.vector(table(factor(key_b, key_a)))
  sums <- rowsum(step1, factor(key_b, unique(key_a)), reorder = FALSE)
  sum_i <- matrix(0, nrow(A), length(sp), dimnames = list(NULL, sp))
  hit <- match(rownames(sums), key_a)
  sum_i[hit[!is.na(hit)], ] <- sums[!is.na(hit), , drop = FALSE]
  missed <- A$N > 0 & n_i == 0
  W_is <- ifelse(n_i > 0, A$N / pmax(n_i, 1), 0) * sum_i
  W_is[missed, ] <- NA
  # within-PSU variance of landing catches (for the two-stage term)
  sq <- rowsum(step1^2, factor(key_b, unique(key_a)), reorder = FALSE)
  sq_i <- matrix(0, nrow(A), length(sp))
  sq_i[hit[!is.na(hit)], ] <- sq[!is.na(hit), , drop = FALSE]
  s2_i <- pmax((sq_i - sum_i^2 / pmax(n_i, 1)) / pmax(n_i - 1, 1), 0)
  s2_i[n_i <= 1, ] <- 0
  within_i <- s2_i * ifelse(n_i > 0, A$N^2 * (1 - n_i / pmax(A$N, 1)) / pmax(n_i, 1), 0)
  psu <- data.frame(A[c("psu", "date", "site", "site_type", "quarter", "segment", "N")],
                    n = n_i, W_is, check.names = FALSE)
  # Step 3
  M <- sample$M
  strata <- unique(A[c("site_type", "quarter", "segment")])
  stratum <- do.call(rbind, lapply(seq_len(nrow(strata)), function(h) {
    s <- strata[h, ]
    rows <- which(A$site_type == s$site_type & A$quarter == s$quarter & A$segment == s$segment)
    Mh <- M$M[M$site_type == s$site_type & M$quarter == s$quarter]
    Wi <- W_is[rows, , drop = FALSE]
    ok <- stats::complete.cases(Wi)
    Wi <- Wi[ok, , drop = FALSE]
    mh <- nrow(Wi)
    if (route == "psu") {
      est <- if (mh > 0) Mh / mh * colSums(Wi) else rep(NA_real_, length(sp))
      v1 <- if (mh > 1) Mh^2 * (1 - mh / Mh) * apply(Wi, 2, stats::var) / mh else rep(NA_real_, length(sp))
      v2 <- if (mh > 0) Mh / mh * colSums(within_i[rows[ok], , drop = FALSE]) else rep(NA_real_, length(sp))
    } else {
      lb <- landing[landing$psu %in% A$psu[rows] & landing$segment == s$segment, sp, drop = FALSE]
      cpue <- if (nrow(lb)) colMeans(lb) else rep(0, length(sp))
      boats <- sum(A$boats[rows])
      bac <- if (boats > 0) sum(A$N[rows]) / boats else 0
      fb <- sample$frame$boats
      frame_boats <- sum(fb$boats[fb$site_type == s$site_type & fb$segment == s$segment])
      days <- sample$frame$days$days[s$quarter]
      effort <- frame_boats * days * bac
      est <- effort * cpue
      cv_c <- if (nrow(lb) > 1) apply(lb, 2, stats::sd) / sqrt(nrow(lb)) / pmax(cpue, 1e-12) else NA
      v1 <- (est * cv_c)^2
      v2 <- rep(0, length(sp))
    }
    data.frame(site_type = s$site_type, quarter = s$quarter, segment = s$segment,
               species = sp, M = Mh, m = mh, estimate = est, var_between = v1,
               var_within = v2, stringsAsFactors = FALSE)
  }))
  rownames(stratum) <- NULL
  # Step 4
  v <- ifelse(is.na(stratum$var_between), 0, stratum$var_between)
  total <- data.frame(species = sp,
                      estimate = as.vector(tapply(stratum$estimate, factor(stratum$species, sp), sum, na.rm = TRUE)),
                      var = as.vector(tapply(v, factor(stratum$species, sp), sum)),
                      stringsAsFactors = FALSE)
  total$se <- sqrt(total$var)
  total$cv <- total$se / pmax(total$estimate, 1e-12)
  total$lower <- pmax(total$estimate - z * total$se, 0)
  total$upper <- total$estimate + z * total$se
  out <- list(landing = landing, psu = psu, stratum = stratum, total = total,
              route = route, flags = list(missed_psu = sum(missed),
                                          strata_no_var = sum(stratum$m < 2) / length(sp)),
              year = sample$year, species = sp)
  class(out) <- "cas_estimate"
  out
}

#' @export
print.cas_estimate <- function(x, ...) {
  cat("<cas_estimate> route:", x$route, "| year", x$year, "\n")
  t <- x$total
  t$estimate <- round(t$estimate / 1000, 2)
  t$se <- round(t$se / 1000, 2)
  t$cv <- round(t$cv, 2)
  print(t[c("species", "estimate", "se", "cv")], row.names = FALSE)
  cat("(tonnes)\n")
  invisible(x)
}

#' Compare an estimate with the truth
#' @param estimate A `cas_estimate`.
#' @param fishery The `cas_fishery` it was sampled from.
#' @return The `total` table with `true` (kg), `rel_error` and `covered`.
#' @export
check_estimate <- function(estimate, fishery) {
  tr <- cas_truth(fishery, years = estimate$year, by = "year")
  t <- estimate$total
  t$true <- tr$kg[match(t$species, tr$species)]
  t$rel_error <- t$estimate / t$true - 1
  t$covered <- t$true >= t$lower & t$true <= t$upper
  t
}
