# Prespecified analysis, run under the deviation recorded before outcomes were requested
# (Methods, Prespecification and deviations).
#   Rscript scripts/work_limitation_analysis.R > results/work-limitation-exit/2026-10-03-analysis.md
suppressPackageStartupMessages({library(dplyr); library(fixest)})
table_out <- function(x) cat(knitr::kable(x, format = "pipe", row.names = FALSE, digits = 4), sep = "\n")
cat("# Work limitation outside the standard disability screen and next-year employment exit\n\n")

ddi <- ipumsr::read_ipums_ddi(list.files("data/ipums-cps/work-limitation-exit/wl_analysis", "\\.xml$", full.names = TRUE))
a <- as.data.frame(haven::zap_labels(ipumsr::read_ipums_micro(ddi, verbose = FALSE)))
stopifnot(all(a$YEAR %in% 2016:2025))

# Cohort and linkage exactly as in scripts/work_limitation_gates.R.
eligible <- subset(a, YEAR %in% 2016:2024 & AGE >= 25 & AGE <= 64 & EMPSTAT %in% c(10, 12) & ASECOVERP == 0 & MISH %in% 1:4 &
  DISABWRK %in% 1:2 & DIFFANY %in% 1:2)
f <- subset(a, CPSIDV > 0 & ASECOVERP == 0)[c("YEAR", "CPSIDV", "MISH", "SEX", "RACE", "AGE", "EMPSTAT")]
stopifnot(!anyDuplicated(f[c("YEAR", "CPSIDV")]))
f$YEAR <- f$YEAR - 1
names(f)[-(1:2)] <- paste0(names(f)[-(1:2)], "_next")
x <- left_join(eligible, f, by = c("YEAR", "CPSIDV"), relationship = "many-to-one")
x$linked <- x$CPSIDV > 0 & !is.na(x$MISH_next) & x$MISH_next == x$MISH + 4 & x$SEX_next == x$SEX &
  x$RACE_next == x$RACE & (x$AGE_next - x$AGE) %in% 0:2
x$linked[is.na(x$linked)] <- FALSE
x$group <- factor(ifelse(x$DISABWRK == 2, ifelse(x$DIFFANY == 2, "Both", "Work limitation only"),
  ifelse(x$DIFFANY == 2, "Standard screen only", "Neither")),
  levels = c("Neither", "Work limitation only", "Standard screen only", "Both"))

# Covariates (baseline).
x$age4 <- cut(x$AGE, c(24, 34, 44, 54, 64))
x$female <- as.integer(x$SEX == 2)
x$race4 <- factor(ifelse(x$HISPAN > 0 & x$HISPAN < 900, "Hispanic",
  ifelse(x$RACE == 100, "NH White", ifelse(x$RACE == 200, "NH Black", "Other"))))
x$educ4 <- factor(ifelse(x$EDUC <= 1 | x$EDUC > 125, "Missing", ifelse(x$EDUC <= 72, "<HS", ifelse(x$EDUC == 73, "HS",
  ifelse(x$EDUC < 111, "Some college", "BA+")))))
x$older <- as.integer(x$AGE >= 55)

# Inverse-probability-of-linkage weights (sensitivity): baseline ASECWT / P(linked | covariates, group).
lm_link <- glm(linked ~ age4 + female + race4 + factor(YEAR) + group, family = binomial(), data = x)
x$ipw <- x$ASECWT / fitted(lm_link)
d <- x[x$linked & x$LNKFW1YWT > 0, ]

# ---------------------------------------------------------------- integrity checks
cat("## Integrity checks (before outcomes are used)\n\n")
expected <- c(Neither = 121447, `Work limitation only` = 3180, `Standard screen only` = 4664, Both = 891)
got <- table(d$group)[names(expected)]
codes <- ipumsr::ipums_val_labels(ddi, EMPSTAT)$val
c1 <- all(as.vector(got) == expected); c2 <- all(d$EMPSTAT_next %in% codes)
table_out(data.frame(check = c("Linked positive-weight counts reproduce the gate screen", "Follow-up EMPSTAT codes valid"),
  value = c(paste(sprintf("%s %d", names(expected), as.vector(got)), collapse = "; "), sprintf("%d records", nrow(d))),
  met = c(c1, c2)))
if (!(c1 && c2)) { cat("\nA check failed. Stopping before any outcome analysis.\n"); quit(status = 1) }

d$exit <- as.integer(!d$EMPSTAT_next %in% c(10, 12))
d$unable <- as.integer(d$EMPSTAT_next == 32)
d$unemployed <- as.integer(d$EMPSTAT_next %in% 20:22)
d$EMPSTAT_next <- NULL

ctrl <- "i(age4) + female + i(race4) + i(educ4)"
fit <- function(y = "exit", data = d, w = ~LNKFW1YWT, cl = ~CPSID, fe = "YEAR", rhs = "i(group, ref = 'Neither')") {
  feols(as.formula(paste(y, "~", rhs, "+", ctrl, "|", fe)), data = data, weights = w, cluster = cl, notes = FALSE)
}
term <- "group::Work limitation only"
row <- function(m, label, data = d, y = "exit", w = data$LNKFW1YWT, tm = term) {
  ct <- coeftable(m); ci <- confint(m)
  ref <- weighted.mean(data[[y]][data$group == "Neither"], w[data$group == "Neither"])
  data.frame(analysis = label, n = nobs(m), rd_pp = 100 * ct[tm, 1], ci_low = 100 * ci[tm, 1], ci_high = 100 * ci[tm, 2],
    p = ct[tm, 4], reference_rate_pp = 100 * ref)
}

cat("\n## Weighted exit rates by group (unadjusted)\n\n")
table_out(d |> group_by(group) |> summarise(n = n(), exit_pct = 100 * weighted.mean(exit, LNKFW1YWT),
  unable_pct = 100 * weighted.mean(unable, LNKFW1YWT), unemployed_pct = 100 * weighted.mean(unemployed, LNKFW1YWT), .groups = "drop"))

cat("\n## Primary estimate\n\n")
m0 <- fit()
table_out(row(m0, "Not employed at follow-up: work limitation only vs neither"))
mp <- fepois(as.formula(paste("exit ~ i(group, ref = 'Neither') +", ctrl, "| YEAR")), data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
rr <- exp(c(coef(mp)[[term]], as.numeric(confint(mp)[term, ])))
cat(sprintf("\nAdjusted risk ratio (weighted Poisson, same covariates): %.2f (95%% CI %.2f to %.2f).\n\n", rr[1], rr[2], rr[3]))
cat("Other groups versus neither (descriptive):\n\n")
table_out(rbind(row(m0, "Standard screen only", tm = "group::Standard screen only"), row(m0, "Both", tm = "group::Both")))

cat("\n## Secondary\n\n")
mo <- fit(rhs = "i(group, ref = 'Neither') + i(group, older, ref = 'Neither') + older")
it <- "group::Work limitation only:older"
sec <- rbind(row(fit("unable"), "Not in labor force, unable to work", y = "unable"),
  row(fit("unemployed"), "Unemployed", y = "unemployed"),
  data.frame(analysis = "Age 55–64 minus 25–54 (interaction)", n = nobs(mo), rd_pp = 100 * coef(mo)[it],
    ci_low = 100 * confint(mo)[it, 1], ci_high = 100 * confint(mo)[it, 2], p = coeftable(mo)[it, 4], reference_rate_pp = NA))
table_out(sec)

cat("\n## Sensitivity\n\n")
nocovid <- d[!d$YEAR %in% 2019:2020, ]
table_out(rbind(
  row(fit(w = NULL), "Unweighted", w = rep(1, nrow(d))),
  row(fit(w = ~ipw), "Inverse-probability-of-linkage weights", w = d$ipw),
  row(fit(data = nocovid), "Excluding follow-up in 2020 or 2021", data = nocovid),
  row(fit(fe = "YEAR + STATEFIP"), "Adding state fixed effects"),
  row(fit(cl = ~STATEFIP), "Clustering by state")))

cat("\n## Provenance\n\n", R.version.string, "; fixest ", as.character(packageVersion("fixest")),
  "; CPS extract 19 (ASEC 2016–2025).\n", sep = "")
