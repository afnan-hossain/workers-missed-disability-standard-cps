# Profile and timing checks, specified after the primary result was known and before they were run.
#   Rscript scripts/work_limitation_profile_timing.R > results/work-limitation-exit/2026-10-03-profile-timing.md
# Deviations (logged before the run): incomes are in 2024 dollars (latest income year in the data), and
# check T4 and the "part-time last year for health reasons" profile item are not run, because the
# harmonized WHYPTLY has no health category.
suppressPackageStartupMessages({library(dplyr); library(fixest)})
table_out <- function(x) cat(knitr::kable(x, format = "pipe", row.names = FALSE, digits = 3), sep = "\n")
cat("# Work limitation outside the six-question screen: profile and timing checks\n\n")
# The final national shares and profile are computed by work_limitation_paper_exhibits.R.
cat("> **Correction recorded October 4, 2026.** The historical cross-sectional profile excluded the ASEC oversample while using the ASEC weight, which is built for the full supplement. That section is omitted here. The corrected national figures, including the oversample, are `results/work-limitation-exit/paper/population-shares.csv` and `table-s5-profile.csv` (Supplementary Table S5). Incomes use CPI-U (CPI99), and the \"other\" destination includes 25 people who had joined the Armed Forces. The linked-cohort results are unaffected.\n\n")

ddi <- ipumsr::read_ipums_ddi(list.files("data/ipums-cps/work-limitation-exit/wl_profile", "\\.xml$", full.names = TRUE))
a <- as.data.frame(haven::zap_labels(ipumsr::read_ipums_micro(ddi, verbose = FALSE)))
stopifnot(all(a$YEAR %in% 2016:2025))
grp <- function(dw, df) factor(ifelse(dw == 2, ifelse(df == 2, "Both", "Work limitation only"),
  ifelse(df == 2, "Standard screen only", "Neither")), levels = c("Neither", "Work limitation only", "Standard screen only", "Both"))

# Derived variables (baseline).
a$age4 <- cut(a$AGE, c(24, 34, 44, 54, 64))
a$female <- as.integer(a$SEX == 2)
a$race4 <- factor(ifelse(a$HISPAN > 0 & a$HISPAN < 900, "Hispanic", ifelse(a$RACE == 100, "NH White", ifelse(a$RACE == 200, "NH Black", "Other"))))
a$educ4 <- factor(ifelse(a$EDUC <= 1 | a$EDUC > 125, "Missing", ifelse(a$EDUC <= 72, "<HS", ifelse(a$EDUC == 73, "HS", ifelse(a$EDUC < 111, "Some college", "BA+")))))
o <- a$OCC2010
a$occ6 <- factor(ifelse(o >= 10 & o <= 950, "Management, business, financial", ifelse(o >= 1000 & o <= 3540, "Professional and related",
  ifelse(o >= 3600 & o <= 4650, "Service", ifelse(o >= 4700 & o <= 5940, "Sales and office",
  ifelse(o >= 6005 & o <= 7630, "Natural resources, construction, maintenance", ifelse(o >= 7700 & o <= 9750, "Production and transportation", "Other or NIU")))))))
i <- a$IND1990
a$ind <- factor(ifelse(i >= 10 & i <= 50, "Agriculture, mining", ifelse(i == 60, "Construction", ifelse(i >= 100 & i <= 392, "Manufacturing",
  ifelse(i >= 400 & i <= 472, "Transport, utilities", ifelse(i >= 500 & i <= 691, "Trade", ifelse(i >= 700 & i <= 712, "Finance, real estate",
  ifelse(i >= 721 & i <= 810, "Business, personal, entertainment services", ifelse(i >= 812 & i <= 840, "Health care",
  ifelse(i >= 842 & i <= 860, "Education", ifelse(i >= 861 & i <= 893, "Other professional services",
  ifelse(i >= 900 & i <= 932, "Public administration", "Other or NIU"))))))))))))
a$cls3 <- factor(ifelse(a$CLASSWKR %in% c(10, 13, 14), "Self-employed", ifelse(a$CLASSWKR %in% c(24, 25, 27, 28), "Government",
  ifelse(a$CLASSWKR %in% 20:23, "Private", "Other"))))
a$hours_now <- ifelse(a$UHRSWORKT < 997, a$UHRSWORKT, NA)
a$hours_ly <- ifelse(a$UHRSWORKLY < 999, a$UHRSWORKLY, NA)
a$weeks_ly <- a$WKSWORK1
a$fullyear <- as.integer(a$WKSWORK1 >= 50)
a$fulltime_ly <- as.integer(a$FULLPART == 1)
cpi_2024 <- unique(a$CPI99[a$YEAR == 2025]); stopifnot(length(cpi_2024) == 1)
dollars <- function(x, niu) ifelse(x >= niu, NA, x * a$CPI99 / cpi_2024)
a$wage <- dollars(a$INCWAGE, 99999998)
a$faminc <- dollars(a$FTOTVAL, 9999999998)
a$poor <- ifelse(a$OFFPOV == 99, NA, as.integer(a$OFFPOV == 1))
a$fairpoor <- as.integer(a$HEALTH %in% 4:5)
a$dis_income <- as.integer(a$INCDISAB > 0 & a$INCDISAB < 9999999)
a$ss_income <- as.integer(a$INCSS > 0 & a$INCSS < 999999)
a$ssi_income <- as.integer(a$INCSSI > 0 & a$INCSSI < 999999)
a$insured <- ifelse(a$ANYCOVNW == 9, NA, as.integer(a$ANYCOVNW == 1))
a$medicaid <- as.integer(a$HIMCAIDNW == 2)
a$medicare <- as.integer(a$HIMCARENW == 2)

# ---------------------------------------------------------------- linked cohort (as in the primary analysis)
eligible <- subset(a, YEAR %in% 2016:2024 & AGE >= 25 & AGE <= 64 & EMPSTAT %in% c(10, 12) & ASECOVERP == 0 & MISH %in% 1:4 &
  DISABWRK %in% 1:2 & DIFFANY %in% 1:2)
f <- subset(a, CPSIDV > 0 & ASECOVERP == 0)[c("YEAR", "CPSIDV", "MISH", "SEX", "RACE", "AGE", "EMPSTAT")]
stopifnot(!anyDuplicated(f[c("YEAR", "CPSIDV")]))
f$YEAR <- f$YEAR - 1; names(f)[-(1:2)] <- paste0(names(f)[-(1:2)], "_next")
x <- left_join(eligible, f, by = c("YEAR", "CPSIDV"), relationship = "many-to-one")
x$linked <- x$CPSIDV > 0 & !is.na(x$MISH_next) & x$MISH_next == x$MISH + 4 & x$SEX_next == x$SEX & x$RACE_next == x$RACE & (x$AGE_next - x$AGE) %in% 0:2
x$linked[is.na(x$linked)] <- FALSE
d <- x[x$linked & x$LNKFW1YWT > 0, ]
d$group <- grp(d$DISABWRK, d$DIFFANY)
expected <- c(Neither = 121447, `Work limitation only` = 3180, `Standard screen only` = 4664, Both = 891)
got <- table(d$group)[names(expected)]
cat("\n## Integrity check\n\n")
cat(sprintf("Linked positive-weight counts: %s. Match the primary analysis: %s.\n\n", paste(sprintf("%s %d", names(got), as.vector(got)), collapse = "; "),
  all(as.vector(got) == expected)))
stopifnot(all(as.vector(got) == expected))
d$exit <- as.integer(!d$EMPSTAT_next %in% c(10, 12))
d$dest <- factor(ifelse(d$EMPSTAT_next %in% c(10, 12), "Employed", ifelse(d$EMPSTAT_next %in% 20:22, "Unemployed",
  ifelse(d$EMPSTAT_next == 32, "NILF: unable to work", ifelse(d$EMPSTAT_next == 36, "NILF: retired", "NILF: other")))))
d$EMPSTAT_next <- NULL

cat("## Exit destinations (linked pairs, weighted %)\n\n")
table_out(d |> group_by(group) |> summarise(n = n(),
  unemployed = 100 * weighted.mean(dest == "Unemployed", LNKFW1YWT), unable = 100 * weighted.mean(dest == "NILF: unable to work", LNKFW1YWT),
  retired = 100 * weighted.mean(dest == "NILF: retired", LNKFW1YWT), other_nilf = 100 * weighted.mean(dest == "NILF: other", LNKFW1YWT),
  .groups = "drop"))

# ---------------------------------------------------------------- Part 2: timing and work-history checks
d$wk4 <- cut(d$WKSWORK1, c(-1, 0, 26, 49, 52), labels = c("0", "1-26", "27-49", "50-52"))
d$hrs_ly <- factor(ifelse(is.na(d$hours_ly), "NIU", ifelse(d$hours_ly < 35, "<35", ifelse(d$hours_ly < 45, "35-44", "45+"))))
d <- d |> group_by(YEAR) |> mutate(wageq = factor(ifelse(is.na(wage) | wage <= 0, "none", paste0("q", ntile(ifelse(wage > 0, wage, NA), 5))))) |>
  ungroup() |> as.data.frame()
base <- "i(age4) + female + i(race4) + i(educ4)"
fit <- function(rhs, data = d) feols(as.formula(paste("exit ~", rhs, "+", base, "| YEAR")), data = data, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
tm <- "group::Work limitation only"
row <- function(m, label) { ct <- coeftable(m); ci <- confint(m)
  data.frame(check = label, n = nobs(m), rd_pp = 100 * ct[tm, 1], ci_low = 100 * ci[tm, 1], ci_high = 100 * ci[tm, 2], p = ct[tm, 4]) }
grpterm <- "i(group, ref = 'Neither')"
p0 <- row(fit(grpterm), "Primary specification (reproduced)")
t1 <- row(fit(paste(grpterm, "+ i(wk4) + i(hrs_ly) + i(wageq) + i(cls3) + i(occ6)")), "T1 Add previous-year work history, wage quintile, class, occupation")
stable <- d[d$WKSWORK1 >= 50 & d$FULLPART == 1, ]
t2 <- row(fit(grpterm, data = stable), "T2 Full-year, full-time workers last year")
m3 <- fit(paste(grpterm, "+ i(group, fairpoor, ref = 'Neither') + fairpoor"))
k <- c(tm, paste0(tm, ":fairpoor")); b <- coef(m3); v <- vcov(m3); tq <- qt(.975, fixest::degrees_freedom(m3, "t"))
fp <- sum(b[k]); fse <- sqrt(sum(v[k, k]))
t3 <- rbind(row(m3, "T3 Self-rated health excellent, very good or good"),
  data.frame(check = "T3 Self-rated health fair or poor", n = nobs(m3), rd_pp = 100 * fp, ci_low = 100 * (fp - tq * fse),
    ci_high = 100 * (fp + tq * fse), p = 2 * pt(-abs(fp / fse), fixest::degrees_freedom(m3, "t"))))
cat("\n## Part 2: timing and work-history checks (risk difference, work limitation only vs neither)\n\n")
table_out(rbind(p0, t1, t2, t3))
cat(sprintf("\nT3 interaction (fair/poor minus good+): %.2f points, p = %.3f. Fair/poor share of the work-limitation-only linked group: %.1f%%.\n",
  100 * b[k[2]], coeftable(m3)[k[2], 4], 100 * weighted.mean(d$fairpoor[d$group == "Work limitation only"], d$LNKFW1YWT[d$group == "Work limitation only"])))
cat("T4 (excluding part-time last year for health reasons) not run: the harmonized WHYPTLY has no health category.\n")
robust <- t1$rd_pp >= 2.5 && t1$ci_low > 0
cat(sprintf("\n**Prespecified rule:** T1 risk difference %.2f points (95%% CI %.2f to %.2f). Robust to work history (≥ 2.5 and CI above zero): %s.\n",
  t1$rd_pp, t1$ci_low, t1$ci_high, robust))
cat("\n", R.version.string, "; fixest ", as.character(packageVersion("fixest")), "; CPS extract 20 (ASEC 2016–2025).\n", sep = "")
