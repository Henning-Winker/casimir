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

test_that("catch by segment sums to the species totals", {
  s <- draw_sample(fish, cas_design(m = 4, n = 2), seed = 7)
  e <- estimate_catch(s)
  bs <- catch_by(e, by = "segment", fishery = fish)
  tot <- tapply(bs$estimate, bs$species, sum)
  expect_equal(as.vector(tot[e$total$species]), e$total$estimate)
  tr <- tapply(bs$true, bs$species, sum)
  ck <- check_estimate(e, fish)
  expect_equal(as.vector(tr[ck$species]), ck$true)
})

test_that("a five-year programme gives one estimate per year", {
  sv <- run_survey(fish, cas_design(m = 3, n = 2), years = utils::tail(fish$years, 3), frame = "first")
  x <- catch_by(sv)
  expect_equal(sort(unique(x$year)), utils::tail(fish$years, 3))
  expect_true(all(vapply(sv$samples, function(s) s$frame$ref_year, 1) == min(sv$years)))
})

test_that("effort estimates recover the census and spins carry the truth", {
  fish <- cas_fishery(island_x(), seed = 1)
  big <- cas_design(m = 1000, n = 1)
  e <- estimate_catch(draw_sample(fish, big))
  eb <- effort_by(e, by = "segment", fishery = fish)
  expect_equal(eb$estimate, eb$true)
  sp <- spin_series(fish, cas_design(), years = utils::tail(fish$years, 2), R = 3)
  s <- summary_spins(sp)
  expect_equal(nrow(s), 2 * length(fish$species))
  tr <- cas_truth(fish, years = utils::tail(fish$years, 2), by = "year")
  expect_equal(sum(s$true), sum(tr$kg))
  l <- spins_long(sp)
  expect_setequal(unique(l$variable), c("catch", "effort"))
  expect_true(all(c("iter", "year", "quarter", "site_type", "segment", "species", "estimate", "se", "true") %in% names(l)))
  l1 <- spins_long(e, fishery = fish)
  expect_equal(sum(l1$true[l1$variable == "effort"]), nrow(fish$landings[fish$landings$year == e$year, ]))
})

test_that("Island X stocks decline without collapse", {
  fish <- cas_fishery(island_x(), seed = 1)
  b <- fish$truth_biomass
  s <- island_x()$species
  dep <- b$biomass[b$year == max(b$year)] / s$B0[match(b$species[b$year == max(b$year)], s$species)]
  expect_true(all(dep > 0.3))
})
