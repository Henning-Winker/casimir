fish <- cas_fishery(island_x(n_years = 4, trips = 8000), seed = 1)

test_that("seeds are distinct and reproducible", {
  s <- vapply(1:2000, function(r) seed_for(3, paste0("rep", r)), 1L)
  expect_equal(length(unique(s)), 2000)
  expect_identical(seed_for(3, "a"), seed_for(3, "a"))
})

test_that("the frame covers every landing of the survey year", {
  y <- max(fish$years)
  pf <- casimir:::psu_frame(frame_survey(fish, y))
  l <- fish$landings[fish$landings$year == y, ]
  expect_true(all(l$key %in% paste(pf$site, pf$date)))
})

test_that("forms are consistent", {
  s <- draw_sample(fish, cas_design(m = 4, n = 2), seed = 1)
  expect_equal(nrow(s$A), nrow(s$visits) * length(fish$segments))
  nb <- table(factor(paste(s$B$psu, s$B$segment), paste(s$A$psu, s$A$segment)))
  expect_true(all(as.integer(nb) <= pmin(s$A$N, 2)))
  ws <- tapply(s$C$w_s, paste(s$C$psu, s$C$trip), sum)
  bw <- s$B$w[match(names(ws), paste(s$B$psu, s$B$trip))]
  expect_equal(unname(as.vector(ws)), bw, tolerance = 0.05)
})

test_that("a census design recovers the truth exactly", {
  d <- cas_design(m = 1000, n = 1000, w_kg = 1e6)
  s <- draw_sample(fish, d, seed = 1)
  e <- check_estimate(estimate_catch(s), fish)
  expect_equal(e$estimate, e$true, tolerance = 0.01)
})

test_that("the PSU route is unbiased over replicates", {
  skip_on_cran()
  ev <- replicate_design(fish, cas_design(m = 4, n = 2), R = 300, seed = 2)
  s <- ev$summary
  main <- s$true > stats::quantile(s$true, 0.3)
  z <- s$rel_bias / (s$sd / sqrt(300) / s$true)
  expect_true(all(abs(z[main]) < 3.5))
})

test_that("a high roller biases the catch upwards", {
  ev <- replicate_design(fish, cas_design(m = 4, n = 1), R = 60, seed = 3,
                         artefacts = artefacts(high_roller = 1))
  expect_gt(stats::weighted.mean(ev$summary$rel_bias, ev$summary$true), 0.05)
})
