# Work limitation outside the six-question screen: profile and timing checks

> **Correction recorded October 4, 2026.** The historical cross-sectional profile excluded the ASEC oversample while using the ASEC weight, which is built for the full supplement. That section is omitted here. The corrected national figures, including the oversample, are `results/work-limitation-exit/paper/population-shares.csv` and `table-s5-profile.csv` (Supplementary Table S5). Incomes use CPI-U (CPI99), and the "other" destination includes 25 people who had joined the Armed Forces. The linked-cohort results are unaffected.

## Integrity check

Linked positive-weight counts: Neither 121447; Work limitation only 3180; Standard screen only 4664; Both 891. Match the primary analysis: TRUE.

## Exit destinations (linked pairs, weighted %)

|group                |      n| unemployed| unable| retired| other_nilf|
|:--------------------|------:|----------:|------:|-------:|----------:|
|Neither              | 121447|      1.778|  0.506|   1.560|      2.745|
|Work limitation only |   3180|      2.487|  2.837|   3.077|      3.737|
|Standard screen only |   4664|      2.241|  3.088|   3.443|      3.771|
|Both                 |    891|      4.430| 10.754|   2.570|      5.203|

## Part 2: timing and work-history checks (risk difference, work limitation only vs neither)

|check                                                               |      n|  rd_pp| ci_low| ci_high|     p|
|:-------------------------------------------------------------------|------:|------:|------:|-------:|-----:|
|Primary specification (reproduced)                                  | 130182|  4.963|  3.580|   6.345| 0.000|
|T1 Add previous-year work history, wage quintile, class, occupation | 130182|  2.467|  1.096|   3.839| 0.000|
|T2 Full-year, full-time workers last year                           | 102659|  2.440|  0.821|   4.058| 0.003|
|T3 Self-rated health excellent, very good or good                   | 130182|  3.175|  1.764|   4.585| 0.000|
|T3 Self-rated health fair or poor                                   | 130182| 10.950|  6.905|  14.995| 0.000|

T3 interaction (fair/poor minus good+): 7.78 points, p = 0.000. Fair/poor share of the work-limitation-only linked group: 17.8%.
T4 (excluding part-time last year for health reasons) not run: the harmonized WHYPTLY has no health category.

**Prespecified rule:** T1 risk difference 2.47 points (95% CI 1.10 to 3.84). Robust to work history (≥ 2.5 and CI above zero): FALSE.

R version 4.6.1 (2026-06-24 ucrt); fixest 0.14.2; CPS extract 20 (ASEC 2016–2025).
