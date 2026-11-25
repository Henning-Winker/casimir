#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_errorbar geom_hline
#'   geom_tile geom_text geom_line facet_wrap labs theme_bw theme element_text
#'   scale_fill_manual scale_colour_manual coord_flip position_dodge
#'   element_blank scale_x_continuous scale_y_continuous geom_linerange
#' @importFrom rlang .data
NULL

#' Seed for one stream of random numbers
#'
#' Derives a reproducible seed from a master seed and a tag (e.g. `"sample"`,
#' `"rep12"`), so that redrawing one stream (a new sample) does not change
#' another (the fishery), and designs compared on the same fishery share the
#' same draws where they overlap (common random numbers).
#' @param seed Master seed (integer).
#' @param tag Character tag of the stream.
#' @return An integer seed.
#' @export
seed_for <- function(seed, tag) {
  # polynomial rolling hash of the tag, then mixed with the seed
  p <- 2147483629  # prime below 2^31
  h <- as.numeric(seed) %% p
  for (cc in utf8ToInt(paste(tag, collapse = "_"))) h <- (h * 131 + cc) %% p
  h <- (h * 69069 + 12345) %% p
  as.integer(h)
}

#' Evaluate an expression with a seed, restoring the random number state
#' @param seed Seed, or `NULL` to leave the state alone.
#' @param expr Expression.
#' @return The value of `expr`.
#' @keywords internal
with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old, envir = globalenv())
  })
  set.seed(seed)
  expr
}

#' Sample `k` indices from `n` (safe for `n == 1`)
#' @noRd
pick <- function(n, k, prob = NULL) {
  if (k <= 0 || n <= 0) return(integer(0))
  k <- min(k, n)
  if (n == 1) return(1L)
  sample.int(n, k, prob = prob)
}

#' Fishing days of a quarter, given the calendar's days off
#' @noRd
quarter_days <- function(year, quarter, days_off = 7) {
  start <- as.Date(sprintf("%d-%02d-01", year, 3 * (quarter - 1) + 1))
  end <- seq(start, by = "3 months", length.out = 2)[2] - 1
  d <- seq(start, end, by = "day")
  d[!(as.integer(format(d, "%u")) %in% days_off)]
}

