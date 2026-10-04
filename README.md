# Work limitation and employment one year later: replication code

R code and aggregate results for *Workers missed by the federal disability standard and their exit from employment: a linked Current Population Survey cohort, 2016–2025* by Afnan Hossain.

The study links Current Population Survey (CPS) Annual Social and Economic Supplement interviews one year apart. It examines subsequent nonemployment among employed adults who report a work limitation in the previous calendar year but answer no to all six federal disability questions.

## Repository contents

| Location | Contents |
|---|---|
| `scripts/` | Data retrieval, estimation and exhibit generation in R |
| `results/work-limitation-exit/` | Primary and linked work-history/health results |
| `results/work-limitation-exit/paper/` | Final aggregate tables, estimates and figures |
| `renv.lock`, `renv/` | Package versions and environment restoration |

No participant-level data are included. Obtain CPS data through your own IPUMS account and comply with its terms of use. Public SIPP data are downloaded separately.

## Environment and CPS data

The analysis used R 4.6.1. From the repository root, restore the packages:

```r
renv::restore()
```

Register at [IPUMS CPS](https://cps.ipums.org), accept the data-use terms and create an API key. Supply it through the environment variable `IPUMS_API_KEY`; keep its value outside the repository. The retrieval scripts use the variable without saving it in the project.

Request and download the main study extracts:

```sh
Rscript scripts/work_limitation_extracts.R submit wl_analysis wl_profile
Rscript scripts/work_limitation_extracts.R download wl_analysis wl_profile
```

IPUMS may need time to prepare the extracts. Repeat the download command if they are not ready. Extract numbers vary by account; the scripts locate files by their folders.

## Main analysis and exhibits

Run these commands from the repository root after downloading the extracts:

```sh
Rscript scripts/work_limitation_analysis.R > results/work-limitation-exit/2026-10-03-analysis.md
Rscript scripts/work_limitation_profile_timing.R > results/work-limitation-exit/2026-10-03-profile-timing.md
Rscript scripts/work_limitation_paper_exhibits.R
```

The exhibits script reads the two analysis reports, reproduces the estimates and checks them against the reported results. The final national shares and Supplementary Table S5 use the full ASEC sample and are saved as `population-shares.csv` and `table-s5-profile.csv`. The linked cohort excludes the ASEC oversample because it has no positive one-year longitudinal weight.

The profile/timing script retains the linked destination, work-history and health analyses. Its obsolete cross-sectional profile section has been withdrawn from this package; use the corrected exhibits outputs for population estimates. Analyses added after the original plan remain identified in the reports and paper.

## Supplementary linkage, precision and SIPP checks

These scripts reproduce checks reported in Supplementary Tables S3a–c and S7. They are separate from the main CPS estimation pipeline.

For S3a–c, obtain the identifier-based linkage extracts and the earlier-period precision extract:

```sh
Rscript scripts/linkage_check_extracts.R submit
Rscript scripts/linkage_check_extracts.R download
Rscript scripts/work_limitation_extracts.R submit wl_prevalence
Rscript scripts/work_limitation_extracts.R download wl_prevalence
Rscript scripts/work_limitation_gates.R > results/work-limitation-exit/2026-10-03-gates.md
```

For S7, obtain the SIPP public-use files for data years 2018–2025 from the [Census Bureau dataset directory](https://www2.census.gov/programs-surveys/sipp/data/datasets/), including the primary `pu{YEAR}_csv.zip` files and two-year longitudinal weight files. Place them in `data/sipp/raw/`, then run:

```sh
Rscript -e "dir.create('results/work-limitation-sipp', recursive = TRUE, showWarnings = FALSE)"
Rscript scripts/sipp_feasibility.R > results/work-limitation-sipp/2026-10-03-feasibility.md
```

The paper reports the failed criteria, the decision to proceed with the CPS analysis and the unsuccessful SIPP replication attempt. Supplementary Table S2 records the analysis chronology and deviations. This public code repository was created later and does not establish that chronology through its own commit history.

## Citation and license

Please cite the paper and the data when using this code:

- Hossain A. Workers missed by the federal disability standard and their exit from employment: a linked Current Population Survey cohort, 2016–2025. 2026.
- Flood S, King M, Rodgers R, Ruggles S, Warren JR, Backman D, Breton E, Cooper G, Rivera Drew JA, Richards S, Van Riper D, Williams KCW. IPUMS CPS: Version 13.0 [dataset]. Minneapolis, MN: IPUMS; 2025. https://doi.org/10.18128/D030.V13.0
- U.S. Census Bureau. Survey of Income and Program Participation, public-use files, 2018–2025.

The code is released under the MIT License. Data remain subject to their providers' terms of use.

Contact: Afnan Hossain, ORCID 0000-0001-6660-9917.
