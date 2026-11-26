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
#' Effort, the number of landings (fishing trips), is estimated per stratum
#' as `M_h / m_h * sum_i N_i` from the landings counted on Form A (route
#' `"psu"`), or as frame boats x days x activity (route `"effort"`).
#'
#' @return A `cas_estimate` with `landing` (step 1, wide), `psu` (step 2,
#'   long), `stratum` (step 3, long), `total` (by species: estimate, se,
#'   cv, lower, upper) and `effort` (trips per stratum: estimate, var).
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
  # Step 3 (vectorised over strata h = site type x quarter x segment)
  M <- sample$M
  h_a <- paste(A$site_type, A$quarter, A$segment, sep = "|")
  hs <- unique(h_a)
  hk <- do.call(rbind, strsplit(hs, "|", fixed = TRUE))
  Mh <- M$M[match(paste(hk[, 1], hk[, 2]), paste(M$site_type, M$quarter))]
  grp_sums <- function(x, g) {
    out <- matrix(0, length(hs), ncol(x), dimnames = list(hs, colnames(x)))
    if (nrow(x)) {
      rs <- rowsum(x, g, reorder = FALSE)
      out[rownames(rs), ] <- rs
    }
    out
  }
  # Effort (landings = trips) per stratum: M_h / m_h * sum_i N_i, with the
  # ultimate-cluster variance
  mh_all <- as.vector(table(factor(h_a, hs)))
  NS <- as.vector(tapply(A$N, factor(h_a, hs), sum))
  NSS <- as.vector(tapply(A$N^2, factor(h_a, hs), sum))
  NS[is.na(NS)] <- 0
  NSS[is.na(NSS)] <- 0
  e_est <- ifelse(mh_all > 0, Mh / pmax(mh_all, 1), NA) * NS
  e_s2 <- pmax((NSS - NS^2 / pmax(mh_all, 1)) / pmax(mh_all - 1, 1), 0)
  e_var <- ifelse(mh_all > 1, Mh^2 * (1 - mh_all / Mh) / pmax(mh_all, 1), NA) * e_s2
  if (route == "psu") {
    ok <- stats::complete.cases(W_is)
    mh <- as.vector(table(factor(h_a[ok], hs)))
    S <- grp_sums(W_is[ok, , drop = FALSE], h_a[ok])
    SS <- grp_sums(W_is[ok, , drop = FALSE]^2, h_a[ok])
    est <- ifelse(mh > 0, Mh / pmax(mh, 1), NA) * S
    s2 <- (SS - S^2 / pmax(mh, 1)) / pmax(mh - 1, 1)
    v1 <- ifelse(mh > 1, Mh^2 * (1 - mh / Mh) / pmax(mh, 1), NA) * pmax(s2, 0)
    v2 <- ifelse(mh > 0, Mh / pmax(mh, 1), NA) * grp_sums(within_i[ok, , drop = FALSE], h_a[ok])
  } else {
    mh <- as.vector(table(factor(h_a, hs)))
    vis <- sample$visits
    vi <- match(landing$psu, vis$psu)
    h_l <- paste(vis$site_type[vi], vis$quarter[vi], landing$segment, sep = "|")
    nl <- as.vector(table(factor(h_l, hs)))
    L <- grp_sums(as.matrix(landing[sp]), h_l)
    LL <- grp_sums(as.matrix(landing[sp])^2, h_l)
    cpue <- L / pmax(nl, 1)
    sd_c <- sqrt(pmax((LL - L^2 / pmax(nl, 1)) / pmax(nl - 1, 1), 0))
    boats <- as.vector(tapply(A$boats, factor(h_a, hs), sum))
    N_h <- as.vector(tapply(A$N, factor(h_a, hs), sum))
    bac <- ifelse(boats > 0, N_h / pmax(boats, 1), 0)
    fb <- sample$frame$boats
    frame_boats <- as.vector(tapply(fb$boats, factor(paste(fb$site_type, fb$segment), unique(paste(hk[, 1], hk[, 3]))), sum)[paste(hk[, 1], hk[, 3])])
    frame_boats[is.na(frame_boats)] <- 0
    days <- sample$frame$days$days[as.integer(hk[, 2])]
    est <- (frame_boats * days * bac) * cpue
    # effort = frame boats x days x activity; variance scaled from the PSU form
    e_new <- frame_boats * days * bac
    e_var <- e_var * (e_new / pmax(e_est, 1e-12))^2
    e_est <- e_new
    v1 <- ifelse(nl > 1, 1, NA) * (est * sd_c / sqrt(pmax(nl, 1)) / pmax(cpue, 1e-12))^2
    v2 <- est * 0
  }
  nh <- length(hs)
  stratum <- data.frame(site_type = rep(hk[, 1], each = length(sp)),
                        quarter = rep(as.integer(hk[, 2]), each = length(sp)),
                        segment = rep(hk[, 3], each = length(sp)),
                        species = rep(sp, nh), M = rep(Mh, each = length(sp)),
                        m = rep(mh, each = length(sp)),
                        estimate = as.vector(t(est)), var_between = as.vector(t(v1)),
                        var_within = as.vector(t(v2)), stringsAsFactors = FALSE)
  rownames(stratum) <- NULL
  effort <- data.frame(site_type = hk[, 1], quarter = as.integer(hk[, 2]), segment = hk[, 3],
                       M = Mh, m = mh_all, estimate = e_est, var = e_var,
                       stringsAsFactors = FALSE)
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
  out <- list(landing = landing, psu = psu, stratum = stratum, total = total, effort = effort,
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
