#' Planted artefacts
#'
#' Field problems a trainer can switch on. Each is recorded in the answer key
#' ([planted_artefacts()]).
#'
#' @param high_roller 0-1: strength of size-biased selection of landings (the
#'   enumerator picks the big boats); selection probability is proportional to
#'   `W^(3 * high_roller)`.
#' @param short_watch Hour at which the enumerator leaves the site (landings
#'   after it are neither counted nor sampled); `NULL` = whole day.
#' @param easy_sites Sites the enumerator visits (others are never sampled but
#'   still counted in M); `NULL` = all.
#' @param dropped_zeros Logical: visits with no landings are not recorded.
#' @param top_of_box 0-1: the subsample over-represents valuable species
#'   (basket probabilities multiplied by `price^(2 * top_of_box)`).
#' @param old_frame Years between the frame survey and the survey (0 = current).
#' @return A `cas_artefacts` list.
#' @export
artefacts <- function(high_roller = 0, short_watch = NULL, easy_sites = NULL,
                      dropped_zeros = FALSE, top_of_box = 0, old_frame = 0) {
  out <- list(high_roller = high_roller, short_watch = short_watch,
              easy_sites = easy_sites, dropped_zeros = dropped_zeros,
              top_of_box = top_of_box, old_frame = old_frame)
  class(out) <- "cas_artefacts"
  out
}

#' Table of planted artefacts (answer key)
#' @param x A `cas_artefacts` (or a `cas_sample`).
#' @return A data frame of the artefacts that are switched on.
#' @export
planted_artefacts <- function(x) {
  if (inherits(x, "cas_sample")) x <- x$artefacts
  if (is.null(x)) x <- artefacts()
  rows <- list(
    high_roller = if (x$high_roller > 0) paste("strength", x$high_roller),
    short_watch = if (!is.null(x$short_watch)) paste("leaves at", x$short_watch, "h"),
    easy_sites = if (!is.null(x$easy_sites)) paste(x$easy_sites, collapse = ", "),
    dropped_zeros = if (isTRUE(x$dropped_zeros)) "zero visits not recorded",
    top_of_box = if (x$top_of_box > 0) paste("strength", x$top_of_box),
    old_frame = if (x$old_frame > 0) paste(x$old_frame, "years old")
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(data.frame(artefact = character(0), setting = character(0)))
  data.frame(artefact = names(rows), setting = unlist(rows), row.names = NULL)
}

#' Draw a survey sample (Roll)
#'
#' Selects site-days in each site type x quarter, then landings of each
#' segment at each visit, subsamples them and splits the subsample by
#' species. Returns the field forms:
#'
#' * `A` (visit and segment): `psu`, `date`, `site`, `site_type`, `quarter`,
#'   `segment`, `N` (landings counted), `boats` (boats of the segment based at
#'   the site, counted at the visit);
#' * `B` (landing): `psu`, `trip`, `segment`, `vessel`, `hours`, `W` (landed
#'   kg), `w` (subsample kg);
#' * `C` (species in the subsample, long): `psu`, `trip`, `species`, `w_s`.
#'
#' @param fishery A `cas_fishery`.
#' @param design A `cas_design`.
#' @param year Survey year.
#' @param artefacts A `cas_artefacts`, or `NULL`.
#' @param seed Seed.
#' @return A `cas_sample` with `visits`, `A`, `B`, `C`, the `frame`, the
#'   `psu_frame` sizes `M`, and the design and artefacts.
#' @export
draw_sample <- function(fishery, design = cas_design(), year = max(fishery$years),
                        artefacts = NULL, seed = 1) {
  if (is.null(artefacts)) artefacts <- artefacts()  # finds the function, not the argument
  frame <- frame_survey(fishery, year, ref_year = year - artefacts$old_frame)
  with_seed(seed_for(seed, c("sample", design$name)), {
    pf <- psu_frame(frame)
    types <- sort(unique(pf$site_type))
    m <- if (is.null(names(design$m))) stats::setNames(rep_len(design$m, length(types)), types)
         else design$m[types]
    M <- stats::aggregate(list(M = pf$site), pf[c("site_type", "quarter")], length)
    # Stage 1: site-days
    visits <- do.call(rbind, lapply(split(pf, list(pf$site_type, pf$quarter), drop = TRUE),
                                    function(g) {
      if (!is.null(artefacts$easy_sites)) {
        g <- g[g$site %in% artefacts$easy_sites, , drop = FALSE]
      }
      k <- min(m[[g$site_type[1]]], nrow(g))
      if (k == 0) return(NULL)
      idx <- if (design$psu_select == "systematic") {
        step <- nrow(g) / k
        floor(stats::runif(1, 0, step) + step * (seq_len(k) - 1)) + 1
      } else sort(pick(nrow(g), k))
      g[idx, ]
    }))
    visits$psu <- sprintf("P%03d", seq_len(nrow(visits)))
    rownames(visits) <- NULL
    l <- fishery$landings
    l <- l[l$year == year, ]
    by_key <- split(seq_len(nrow(l)), l$key)
    segs <- fishery$segments
    n_seg <- if (is.null(names(design$n))) stats::setNames(rep_len(design$n, length(segs)), segs)
             else design$n[segs]
    price <- stats::setNames(fishery$settings$species$price, fishery$species)
    # candidate landings at the visited site-days
    rows <- by_key[paste(visits$site, visits$date)]
    nr <- lengths(rows)
    li <- l[unlist(rows), c("trip", "segment", "vessel", "hours", "hour", "W", fishery$species)]
    li$psu <- rep(visits$psu, nr)
    if (!is.null(artefacts$short_watch)) li <- li[li$hour <= artefacts$short_watch, ]
    # Form A: landings counted per visit and segment
    A <- expand.grid(segment = segs, psu = visits$psu, stringsAsFactors = FALSE)[c("psu", "segment")]
    A <- cbind(visits[match(A$psu, visits$psu), c("psu", "date", "site", "site_type", "quarter")],
               segment = A$segment)
    A$N <- as.integer(table(factor(paste(li$psu, li$segment), paste(A$psu, A$segment))))
    # Stage 2: landings sampled per visit and segment
    grp <- split(seq_len(nrow(li)), paste(li$psu, li$segment))
    sel <- unlist(lapply(grp, function(ix) {
      k <- n_seg[[li$segment[ix[1]]]]
      p <- if (artefacts$high_roller > 0) pmax(li$W[ix], 1e-6)^(3 * artefacts$high_roller) else NULL
      ix[pick(length(ix), k, prob = p)]
    }), use.names = FALSE)
    sel <- sort(sel)
    ls <- li[sel, ]
    # Stage 3: subsample in baskets, split by species
    w <- pmin(ls$W, design$w_kg)
    nb <- pmax(1L, round(w / design$unit_kg))
    comp <- as.matrix(ls[fishery$species])
    pr <- comp / pmax(ls$W, 1e-12)
    if (artefacts$top_of_box > 0) pr <- sweep(pr, 2, price^(2 * artefacts$top_of_box), `*`)
    cnt <- t(vapply(seq_len(nrow(ls)), function(r) {
      if (sum(pr[r, ]) > 0) stats::rmultinom(1, nb[r], pr[r, ])[, 1] else rep(0, ncol(pr))
    }, numeric(ncol(pr))))
    if (nrow(ls) == 0) cnt <- matrix(0, 0, ncol(pr))
    ws <- cnt * w / nb
    B <- data.frame(psu = ls$psu, trip = ls$trip, segment = ls$segment, vessel = ls$vessel,
                    hours = round(ls$hours, 1), W = round(ls$W, 1), w = round(w, 2),
                    stringsAsFactors = FALSE)
    pos <- which(ws > 0, arr.ind = TRUE)
    pos <- pos[order(pos[, 1], pos[, 2]), , drop = FALSE]
    C <- data.frame(psu = ls$psu[pos[, 1]], trip = ls$trip[pos[, 1]],
                    species = fishery$species[pos[, 2]], w_s = round(ws[pos], 2),
                    stringsAsFactors = FALSE)
    # boats based at the site in the survey year, counted at the visit (the
    # frame may be older: the CPUE x effort route uses both)
    v <- fishery$vessels
    v <- v[v$entry <= year & v$exit >= year, ]
    nb <- table(paste(v$site, v$vessel_type))
    A$boats <- as.integer(nb[paste(A$site, A$segment)])
    A$boats[is.na(A$boats)] <- 0L
    rownames(A) <- rownames(B) <- rownames(C) <- NULL
    if (isTRUE(artefacts$dropped_zeros)) {
      tot <- tapply(A$N, A$psu, sum)
      keep <- names(tot)[tot > 0]
      A <- A[A$psu %in% keep, ]
      visits <- visits[visits$psu %in% keep, ]
    }
    out <- list(visits = visits, A = A, B = B, C = C, M = M, frame = frame,
                design = design, artefacts = artefacts, year = year,
                species = fishery$species, segments = segs, seed = seed)
    class(out) <- "cas_sample"
    out
  })
}

#' @export
print.cas_sample <- function(x, ...) {
  cat("<cas_sample>", x$design$name, "| year", x$year, "|", nrow(x$visits), "site-days,",
      nrow(x$B), "landings sampled,", round(sum(x$B$w), 1), "kg subsampled\n")
  pa <- planted_artefacts(x)
  if (nrow(pa)) cat("  artefacts:", paste(pa$artefact, collapse = ", "), "\n")
  invisible(x)
}
