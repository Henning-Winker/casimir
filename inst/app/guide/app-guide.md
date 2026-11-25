# casimir app guide

<!-- App-only copy in plain language. Sections are found by their heading, so headings must stay as they are. -->

## 1. Island and fleet
Island X has nine landing sites along its coast, of three types: harbours, beaches and jetties. Each boat belongs to a segment (a fishing-unit type: canoe, small motor boat or large motor boat) and lands at a home site. Canoes are based at beaches and jetties, small motor boats at all site types, and large motor boats at harbours and some jetties.

Where a boat fishes depends on two things: how far it can go from its home site (its range), and where the fish it targets live. Demersal species live at particular depths, on reef or on sediment; pelagic species follow sea temperature and migrate through the area in the warm season. Canoes fish reefs close to shore, small motor boats fish reef and sediment further out, and large motor boats go offshore for pelagic fish. Each trip lands its catch at the home site on a fishing day (no fishing on Sundays), and its landing time depends on how long and how far it fished.

Every trip of the survey year, with its true catch by species, is the **population**: the census the survey tries to estimate. Because casimir creates it, the true catch is always known.

The fishery is simulated over several years, and the survey takes place in the last one. The earlier years let the stocks respond to fishing and the fleet change (boats enter and leave), so the survey year is not a fresh start.

## 2. Frame survey
Before a catch survey can be planned, the planner needs a frame: a list of the landing sites, the boats at each site by segment, and the fishing days. In casimir, the frame survey is a census of boats in a reference year. If it is out of date (the **old frame** artefact), boats that entered since are missing and boats that left are still listed.

The frame gives the number of primary sampling units (PSUs) in each stratum. Here a PSU is one landing site on one fishing day, and the strata are site type × quarter, so a stratum of three beaches in a quarter with 78 fishing days has M = 234 site-days.

## 3. Design
The design follows the FAO training, stage by stage:

| Stage | Unit | What is chosen | What is recorded |
|---|---|---|---|
| Strata | site type × quarter (segments within each visit) | all strata are sampled | |
| 1 | site-day (PSU) | m site-days per stratum, at random or spread over the quarter | Form A: landings counted by segment (N) |
| 2 | landing | n landings per segment at each visit | Form B: landed weight (W), hours fished |
| 3 | subsample | w kg from each sampled landing | Form C: species weights (w_s) |

Every visit records all segments, so each segment is raised in its own stratum (site type × quarter × segment) from the same site-days.

The cost model counts enumerator-hours: a fixed cost for each visit (travel and watching the landings), and a cost for each landing sampled and each kg sorted. It lets designs be compared at the same cost.

**The betting slip.** Before rolling, trainees predict how precise the estimate of one species will be (its CV) and whether it will be biased. Spin ×1000 settles the bet.

## 4. Roll: field forms
Roll draws one sample with the design: it picks the site-days, counts the landings at each, samples some of them, takes a subsample from each and splits it by species. The subsample is drawn in baskets of 0.5 kg, each filled with the species mix of the landing, so a small subsample of a mixed landing can miss rare species entirely.

The result is three field forms, as a survey team would hand them in. The week × site grid shows which site-days were visited, as in the training deck.

**Planted artefacts (trainer).** Real surveys have field problems. The trainer can switch them on before a roll; trainees with Hide truth ticked do not see which.

| Artefact | What happens | What trainees may notice |
|---|---|---|
| High roller | the enumerator picks the big boats | sampled landings larger than the site's typical landing |
| Short watch | the enumerator leaves early; late landings are neither counted nor sampled | long-range boats rare in the forms but common in the frame |
| Easy sites | only some sites are visited, but M counts all of them | some sites never appear in Form A |
| Dropped zeros | visits with no landings are not written down | no visit in Form A has N = 0 for all segments |
| Top of the box | the subsample comes from the top of the catch, where large and valuable fish lie | valuable species over-represented in the subsamples |
| Old frame | the frame survey is years old | boats at sites that are not in the frame |

## 5. Raise: the estimators
Catch per species is raised one stage at a time:

1. **Landing.** The species share of the subsample times the landed weight: W_ijs = w_ijs / w_ij × W_ij.
2. **Site-day.** The mean of the sampled landings times the landings counted: W_is = N_i / n_i × Σ_j W_ijs.
3. **Stratum.** The mean of the visited site-days times the site-days in the stratum: W_hs = M_h / m_h × Σ_i W_is. Site-days with no landings count, with a catch of zero.
4. **Total.** The sum over strata.

The variance comes from how much the site-day totals differ within each stratum. The 95% interval is the estimate ± 1.96 standard errors.

The alternative route, **CPUE × effort**, multiplies the catch per landing by an estimate of the number of landings: boats in the frame × fishing days × boat activity (the share of boats landing on a visited day). With a correct frame, both routes agree on average; with an old frame, only the CPUE × effort route is biased.

## 6. Spin ×1000
One sample gives one estimate. To judge a design, Spin repeats the whole survey many times on the same island, each time with a new sample, and compares the estimates with the true catch:

- **bias** (the house edge): how far the average estimate is from the truth. It does not shrink with more samples;
- **CV**: how much the estimates vary from one survey to the next. It shrinks with more samples;
- **coverage**: how often the 95% interval contains the true catch. It should be close to 0.95.

Common species are estimated well, rare species poorly: a species landed by one segment at one type of site can have a CV several times that of the main species. Landings on the same day at the same site are alike, so an extra site-day usually buys more precision than extra landings on the same day.

## Warning flags
Some patterns in survey data point to the survey, not to the fishery:

- sampled landings consistently larger than the landings counted suggest (high roller);
- a segment that is common in the frame but rare in the forms (short watch);
- no visit without landings (dropped zeros);
- sites that are never visited (easy sites);
- landings from boats that are not in the frame (old frame);
- the two routes, PSU raising and CPUE × effort, disagree by more than their intervals.

## 7. Cash out and truth
Cash out writes the frame survey, the design and the field forms as csv files, as a survey team would hand them in. With Hide truth unticked, an answer key can be added: the true catch by stratum, the estimate against the truth, and the planted artefacts.

The True catch step (trainers) shows the true catch in the survey year, by species and by stratum, next to the last estimate.
