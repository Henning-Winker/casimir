## 1. What casimir does

casimir (*Catch Sampling Inference Methods in R*) simulates a small-scale
fishery whose true landings are known, and the landing-site surveys that try
to estimate them. It is built for training in catch assessment surveys:
trainees **design** a stratified multi-stage survey, **roll** a sample,
**raise** it to catch per species with the estimators of the FAO training,
and **spin** the design many times to see its bias, precision and cost.

A casimir run has six layers, each a plain R function:

| # | Layer | Function | Object |
|---|---|---|---|
| 1 | Island (fishery) | `cas_fishery(island_x())` | `cas_fishery` |
| 2 | Frame survey | `frame_survey()` | `cas_frame` |
| 3 | Design | `cas_design()` | `cas_design` |
| 4 | Roll (sample) | `draw_sample()` | `cas_sample` (forms A-C) |
| 5 | Raise (estimate) | `estimate_catch()` | `cas_estimate` |
| 6 | Spin (evaluate) | `replicate_design()`, `compare_designs()` | `cas_eval` |
| - | Truth | `cas_truth()`, `check_estimate()`, `planted_artefacts()` | answer key |

Seeds are derived per stream with `seed_for(seed, tag)`, so a new sample does
not change the fishery, and designs compared on the same fishery share the
same random draws where they overlap.

## 2. The island: landing sites, segments and ranges

The fishery comes from the logbook simulator of
[cpueTargeting](https://github.com/Henning-Winker/cpueTargeting)
(`simulate_logbook()`): ten demersal and pelagic species on a shelf-to-slope
grid with reef patches and a temperature gradient, Schaefer dynamics, and a
fleet that chooses a tactic (reef handline, sediment longline, pelagic troll)
and a cell by expected value. casimir switches on its optional landing layer
(`landing_settings()`):

* each boat has a **home landing site** on the coast, drawn by site type and
  segment (`site_mix`);
* the weight of each cell in the choice of where to fish is multiplied by the
  segment's **reach** from the home site, `exp(-(d / range)^2)`;
* each trip gets a **fishing day** (no fishing on Sundays, at most one trip
  per boat and day where possible) and a **landing hour** (departure + hours
  fished + travel).

`island_x()` sets up the island of the training deck: nine sites of three
types (Harbour, Beach, Jetty) and three segments.

| Segment | Boats | Range | Home tactics | Based at |
|---|---|---|---|---|
| canoe | 150 | 5 cells (12.5 km) | Reef | beaches, jetties |
| small_motor | 60 | 12 cells (30 km) | Reef, Sediment | all site types |
| large_motor | 15 | 25 cells (62.5 km) | Pelagic, Sediment | harbours, some jetties |

The simulation runs for several years and the survey takes place in the
last; the earlier years let stocks respond to fishing and the fleet change.
`fish$landings` holds every trip with its site, date, segment and true landed
weight by species: the census the survey estimates.

## 3. Frame survey and design

`frame_survey()` lists boats by site and segment (active in a reference year)
and fishing days by quarter. The **PSU** is a landing site on a fishing day;
strata are **site type x quarter**, and each visit records all segments, so
each segment is raised in its own stratum (site type x quarter x segment).

`cas_design()` sets the stages:

| Stage | Unit | Setting | Recorded on |
|---|---|---|---|
| 1 | site-day | `m` per site type and quarter; `psu_select` = `"srs"` or `"systematic"` | Form A: N, landings counted by segment |
| 2 | landing | `n` per segment and visit (all if fewer land) | Form B: W, landed kg; hours fished |
| 3 | subsample | `w_kg` per landing, in baskets of `unit_kg` | Form C: species weights w_s |

The subsample is drawn as `w_kg / unit_kg` baskets, each filled with the
species mix of the landing (multinomial), so small subsamples miss rare
species. `design_cost()` counts enumerator-hours per visit, landing and kg.

## 4. Estimators

`estimate_catch()` raises one stage at a time:

```
Step 1  landing   W_ijs = w_ijs / sum_s(w_ijs) * W_ij
Step 2  site-day  W_is  = N_i / n_i * sum_j W_ijs
Step 3  stratum   W_hs  = M_h / m_h * sum_i W_is
Step 4  total     W_s   = sum_h W_hs
```

Site-days with no landings of a segment count, with a catch of zero. The
variance of step 3 is the ultimate-cluster estimator with the stage-1 finite
population correction,

```
V(W_hs) = M_h^2 (1 - m_h / M_h) s^2_h / m_h
```

where `s^2_h` is the variance of the site-day totals; the second-stage term
`M_h / m_h sum_i N_i^2 (1 - n_i / N_i) s^2_i / n_i` is returned as
`var_within`. A census (all site-days, all landings, whole landings) recovers
the true catch exactly, and over replicates the estimator is unbiased (tests
in `tests/testthat`).

The **CPUE x effort** route (`route = "effort"`) multiplies the mean catch
per landing in the stratum by frame boats x fishing days x boat activity
(landings counted per frame boat on the visited days). It agrees with the PSU
route on average when the frame is up to date.

## 5. Planted artefacts

`artefacts()` switches on field problems; `planted_artefacts()` lists them for
the answer key.

| Artefact | Setting | Effect |
|---|---|---|
| High roller | `high_roller = 0-1` | landings chosen with probability proportional to `W^(3 * high_roller)`: catch biased up |
| Short watch | `short_watch = hour` | later landings neither counted nor sampled: long-range segments and their species biased down |
| Easy sites | `easy_sites = c(...)` | only these sites visited, M counts all: biased if their grounds differ |
| Dropped zeros | `dropped_zeros = TRUE` | visits with no landings not recorded: catch biased up |
| Top of the box | `top_of_box = 0-1` | subsample favours valuable species: species split biased |
| Old frame | `old_frame = years` | frame boats out of date: CPUE x effort route biased |

## 6. Spin: bias, CV and coverage

`replicate_design()` repeats Roll and Raise `R` times on the same island and
compares the totals with the truth: relative bias (the house edge), CV,
coverage of the 95% intervals and RMSE. `compare_designs()` does this for
several designs, and `days_vs_boats()` builds designs of about equal cost
that trade site-days for landings per visit.

Coverage below 0.95 is expected for species whose catches are skewed (a few
large landings) when strata have few site-days: the normal interval is too
narrow. It is a teaching point, not an error.
