# casimir 0.0.2

* Island X retuned: moderate fishing pressure (`q = 0.004`, Dentex
  catchability by tactic x 0.3), so stocks decline without collapsing.
  Boats join and leave over the whole period (`island_x(entry, exit)`), and
  trips grow with the fleet, so an old frame survey goes out of date.
* Effort (landings, i.e. trips) estimated alongside catch: `effort_by()`,
  `plot_effort_series()`, `cas_effort_truth()`.
* Spin the whole programme over the years: `spin_series()`,
  `summary_spins()`, `plot_spin_series()` (band or paired bars), and
  `spins_long()`, all estimates with the truth as one long table.
* App: new step 6 Series (catch and effort over the survey years, route and
  frame), Spin x1000 > Over the years, long-table download (csv or rds) in
  Cash out, fleet sliders on the Island tab, the top pills as Build / new
  island and Roll / redraw buttons, Build from any empty tab.
* Requires cpueTargeting 0.4.2 (`entry_years`, `exit_years`).

# casimir 0.0.1

* First prototype: Island X on the landing-site layer of cpueTargeting
  (`landing-sites` branch), frame survey, stratified multi-stage design,
  field forms A-C with planted artefacts, four-step raising with variance and
  the CPUE x effort route, replicates, and a Shiny app in the FLOSS layout.
