# North America Trade Panel

Code to build a **monthly, product-level panel of US–Canada and US–Mexico trade,
2002–2026**, at the finest product detail each statistical agency publishes
(HS10 where it exists), together with the related-party share panel used to
approximate intra-firm trade.

The panel is built for a monetary-policy / dominant-currency exercise: it exists
to test whether policy shocks move real trade volumes and prices, which needs
quantities and unit values at monthly frequency, not annual value aggregates.

Finished panel: **19.3M rows, 16 columns**, covering 2002-01 to 2026-07.

---

## Sources and coverage

| | Census API | USA Trade Online | CIMT (Statistics Canada) |
|---|---|---|---|
| **Reporter** | US | US | Canada |
| **Period** | 2010-01 onward | 2002-01 – 2009-12 | 2002 onward (back to 1992) |
| **Detail** | HS10 | HS10 | HS10 imports, **HS8 exports** |
| **Currency** | USD | USD | CAD |
| **Access** | REST API, free key | manual report builder | direct zip download, no login |
| **Quantity** | reliable at HS10 (99.8%) | ~23% populated | 50–60%, stable |

Three things that are easy to get wrong:

- **The Census API really starts 2010-01.** Census's own documentation
  claims 2013; every year 2003–2009 returns HTTP 204. The
  2002–2009 gap is filled from USA Trade Online instead, which is why there are
  two US-side extractors. Verified by `R/06_diagnostics/census_historical_hs10_check.R`.
- **Census quantity is reliable at HS10 and zeroed at HS6.** That is a
  unit-mismatch artifact of aggregation, which is why the panel
  is kept at native HS10 and never pre-aggregated.
- **Canada publishes no HS10 on the export side.** HS8 is the ceiling there.
  Each direction is kept at its native granularity and aggregated to a common
  level only at analysis time.

CIMT archives are keyed by ODPFN table number, which anyone reading them must
match on by prefix — both the filename suffix letter and the folder nesting vary
by year:

| | HS10 | HS8 | HS6 | HS2 |
|---|---|---|---|---|
| Imports | ODPFN014 | — | ODPFN015 | ODPFN022 |
| Total exports | — | ODPFN017 | ODPFN019 | ODPFN021 |
| Domestic exports | — | ODPFN016 | ODPFN018 | ODPFN020 |

Total exports include re-exports; domestic exports do not. Both are kept, so
re-exports are recoverable as `Tot − sum_over_province(Dom)`. The domestic
archive carries an extra province-of-origin column, so it is at a finer grain
and the subtraction is not row-wise.

---

## Setup

Requires **R ≥ 4.3** (developed against 4.6). Python 3 is optional — only for the
no-dependency alternative to the NAICS concordance fetcher.

```bash
git clone <this repo>
cd north-america-trade-panel

Rscript setup/install_packages.R      # httr2, vroom, arrow, readxl, readr, dplyr, countrycode

cp .Renviron.example .Renviron        # then add your Census API key
```

A Census API key is free: <https://api.census.gov/data/key_signup.html>

Everything resolves paths through `R/config.R`, which finds the repo root by
looking for `north-america-panel.Rproj`. Scripts run from the repo root or from
their own folder, and nothing depends on your working directory.

---

## Getting the data

**No data is in this repo.** The full set is several GB. `R/config.R` reads three
optional environment variables so the bulk can live off your main drive:

| Variable | Default | Holds |
|---|---|---|
| `NA_PANEL_DATA_DIR` | `<repo>/data` | everything below |
| `NA_PANEL_CIMT_DIR` | `$DATA/cimt` | ~1–2 GB of CIMT zips |
| `NA_PANEL_UTO_IN_DIR` | `$DATA/uto_raw` | raw USA Trade Online exports |

Two of the three sources download themselves. Two inputs are manual:

- **USA Trade Online** (`$UTO_IN_DIR`) — no API. Register a free account at
  <https://usatrade.census.gov>, then export one calendar year per query at
  all-HS10 × both flows × one country × monthly, All Districts. Files run
  130–200 MB per year. `uto_reshape.R` expects them as downloaded.
- **Census related-party benchmark** (`$DATA/rp`) — four bulk NAICS6 CSVs
  covering 2005–2010, 2011–2015, 2016–2020 and 2021–2025, from
  <https://www.census.gov/foreign-trade/Press-Release/related_party/>. Save them
  as `rp_naics6_<year>.csv`; that is the pattern `rp_build_share_panel.R` globs
  for. Only needed for the related-party thread in `R/04_related_party/`. (The
  older `relatedparty.ftd.census.gov` interactive tool has been retired and no
  longer resolves — these bulk files replace it.)

---

## Run order

```
R/config.R                                  paths and keys (sourced by everything)

R/01_acquire/
  census_config.R                           sample boundaries, shared by the next two
  census_api_pull.R          ->  $DATA/census_api/        US HS10 monthly, 2010+
  census_api_validate.R                     verify before using
  cimt_bulk_download.R       ->  $CIMT_DIR/               Canada, 2002+
  cimt_validate.R                           verify before using
  uto_reshape.R              ->  $DATA/uto/               US 2002-2009, wide -> long
  uto_merge.R                ->  $DATA/uto/               merge the measure sets

R/02_panel/
  panel_schema.R                            the 16-column schema, sourced by all extractors
  extract_census.R           ->  $DATA/panel/census_core.csv.gz
  extract_uto.R              ->  $DATA/panel/uto_core.csv.gz
  extract_cimt.R             ->  $DATA/panel/cimt_core.csv.gz
  build_panel.R              ->  $DATA/panel/panel_core.csv.gz
  panel_sanity_checks.R      ->  $DATA/panel/panel_sanity_report.txt
  panel_open.R                              panel_slice() / panel_open() helpers
  panel_basic_tests.R                       quick reads against the built panel

R/03_concordance/
  naics_concordance_fetch.R  ->  $DATA/naics_concordance/ (or the .py equivalent)

R/04_related_party/
  related_party_trade_check.R               check the protocol's assumptions live
  rp_build_share_panel.R     ->  $DATA/rp_panel/          the share panel
  rp_reconcile_denominators.R               monthly vs annual reconciliation
  rp_apply_threshold.R                      vintage audit + threshold curve
  rp_step5_export_gate.R                    Canada export-side quality gate
  rp_step5_unflagged_structure.R            structure of unflagged trade
  rp_email_tables.R                         reproduces and pins the reported tables

R/05_analysis/
  unit_value_rigidity.R                     frequency of price change

R/06_diagnostics/                           source investigations; not pipeline steps
```

Each script states its inputs, outputs and run command in its own header. All
are re-runnable: the acquisition scripts skip work already on disk, and
everything from `02_panel` onward is offline.

---

## The panel schema

`panel_core.csv.gz`, 16 columns:

| Column | Type | Notes |
|---|---|---|
| `period` | character | `"200201"`. **Must stay character** |
| `reporter`, `partner` | character | `"US"`, `"CA"`, `"MX"` |
| `flow_reporter`, `direction` | character | flow as the reporter labels it, and normalized |
| `basis` | character | total vs domestic exports |
| `hs_code` | character | **Must stay character** |
| `hs_level`, `hs6` | character | native level, plus the HS6 truncation |
| `value` | double | in `currency` |
| `currency` | character | USD for US-reported, CAD for CIMT |
| `quantity_1`, `unit_1` | double, character | |
| `quantity_2`, `unit_2` | double, character | |
| `source` | character | `uto`, `census` or `cimt` |

**`period` and `hs_code` must be read as character.** Left to type inference,
`"200201"` becomes the number 200201 and `"0101100000"` becomes 101100000. The
leading zero is gone, every HS chapter 01–09 code is wrong, and nothing errors.
It just quietly stops joining. `PANEL_TYPES` in `panel_open.R` pins all sixteen
columns; use it, or use the Parquet conversion, which stores its own types.

Do not load the whole panel — 19.3M × 16 is roughly 2.9 GB in memory. Use
`panel_slice()` for a one-off look, or `panel_to_parquet()` once and then query
lazily.

---

## Scope and interpretation

- **The sample ends 2026-07 by decision.** Census keeps
  publishing past it. The boundary lives in `census_config.R` so the pull and
  the validator cannot disagree; change it there and re-run.
- **Stacking** US-reported and Canada-reported rows are
  both kept, in their own currencies, on their own classifications. Neither is
  corrected against the other.
- **Unit values are not prices.** A unit value is value ÷ quantity summed over
  every transaction in an HS10 code in a month, so a shift in product mix moves
  it although no seller changed a price. `unit_value_rigidity.R` therefore
  reports an **upper bound** on the frequency of price change, and a lower bound
  on duration, across a range of thresholds rather than a single number.
- **HS10 codes are only comparable within an inter-revision window.** The WCO
  revised in 2007, 2012, 2017 and 2022; across a revision, codes carrying real
  trade disappear at 10–20× the baseline rate and no official over-time HS10
  concordance exists to bridge them. Analysis defaults to 2017–2021, one clean
  regime. Do not set a window that spans a revision.
- **Re-exports are separated, not dropped.** A re-exported good's price was set
  by a foreign producer, which contaminates a question about US exporter
  pricing. Census carries this as a `DF` row flag; Canada splits it into
  separate archives. Both are preserved.

---

## Repository layout

```
north-america-panel.Rproj     project file; also the repo-root marker
.Renviron.example             copy to .Renviron and fill in
setup/install_packages.R
R/config.R                    the only place paths are resolved
R/01_acquire/ ... R/06_diagnostics/
```

.
