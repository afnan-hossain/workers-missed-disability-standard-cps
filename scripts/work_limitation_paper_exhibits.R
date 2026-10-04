# Paper exhibits for the work-limitation exit study: Tables 1-2 and Figures 1-3.
#   Rscript scripts/work_limitation_paper_exhibits.R
# Writes results/work-limitation-exit/paper/. Tables, margins and the flow diagram are computed from CPS extract 20
# and checked against the committed results. Figure 3 plots the committed estimates in
# results/work-limitation-exit/2026-10-03-analysis.md and 2026-10-03-profile-timing.md, re-estimating the rows whose
# stored rounding is ambiguous at two decimals. Derivations and linkage follow scripts/work_limitation_profile_timing.R.
# Not prespecified (added for the paper, October 3, 2026): adjusted odds ratios from a weighted logistic model, adjusted
# risk ratios for the two comparison groups, and adjusted predicted percentages (marginal standardization).
suppressPackageStartupMessages({library(dplyr); library(fixest); library(ggplot2)})
out <- "results/work-limitation-exit/paper"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
f_analysis <- "results/work-limitation-exit/2026-10-03-analysis.md"
f_profile <- "results/work-limitation-exit/2026-10-03-profile-timing.md"

# Read the n-th pipe table under a "## " heading of a committed results file.
md_table <- function(path, heading, which = 1) {
  x <- readLines(path, encoding = "UTF-8")
  h <- match(heading, x); stopifnot(!is.na(h))
  rest <- x[(h + 1):length(x)]
  nxt <- grep("^## ", rest); if (length(nxt)) rest <- rest[seq_len(nxt[1] - 1)]
  runs <- rle(startsWith(rest, "|")); ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
  b <- which(runs$values)[which]; rows <- rest[starts[b]:ends[b]]
  cells <- lapply(strsplit(rows, "|", fixed = TRUE), function(r) trimws(r[-1]))
  df <- as.data.frame(do.call(rbind, cells[-(1:2)]), stringsAsFactors = FALSE); names(df) <- cells[[1]]
  num <- vapply(df, function(v) !anyNA(suppressWarnings(as.numeric(v))), logical(1))
  df[num] <- lapply(df[num], as.numeric); df
}
fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

ddi <- ipumsr::read_ipums_ddi(list.files("data/ipums-cps/work-limitation-exit/wl_profile", "\\.xml$", full.names = TRUE))
a <- as.data.frame(haven::zap_labels(ipumsr::read_ipums_micro(ddi, verbose = FALSE)))
stopifnot(all(a$YEAR %in% 2016:2025))
grp <- function(dw, df) factor(ifelse(dw == 2, ifelse(df == 2, "Both", "Work limitation only"),
  ifelse(df == 2, "Standard screen only", "Neither")), levels = c("Neither", "Work limitation only", "Standard screen only", "Both"))
a$age4 <- cut(a$AGE, c(24, 34, 44, 54, 64))
a$female <- as.integer(a$SEX == 2)
a$race4 <- factor(ifelse(a$HISPAN > 0 & a$HISPAN < 900, "Hispanic", ifelse(a$RACE == 100, "NH White", ifelse(a$RACE == 200, "NH Black", "Other"))))
a$educ4 <- factor(ifelse(a$EDUC <= 1 | a$EDUC > 125, "Missing", ifelse(a$EDUC <= 72, "<HS", ifelse(a$EDUC == 73, "HS", ifelse(a$EDUC < 111, "Some college", "BA+")))))
a$hours_now <- ifelse(a$UHRSWORKT < 997, a$UHRSWORKT, NA)
a$fullyear <- as.integer(a$WKSWORK1 >= 50)
a$fulltime_ly <- as.integer(a$FULLPART == 1)
cpi_2024 <- unique(a$CPI99[a$YEAR == 2025]); stopifnot(length(cpi_2024) == 1)
a$faminc <- ifelse(a$FTOTVAL >= 9999999998, NA, a$FTOTVAL * a$CPI99 / cpi_2024)
a$poor <- ifelse(a$OFFPOV == 99, NA, as.integer(a$OFFPOV == 1))
a$fairpoor <- as.integer(a$HEALTH %in% 4:5)
a$dis_income <- as.integer(a$INCDISAB > 0 & a$INCDISAB < 9999999)
a$medicaid <- as.integer(a$HIMCAIDNW == 2)
a$medicare <- as.integer(a$HIMCARENW == 2)

o6 <- a$OCC2010
a$occ6 <- factor(ifelse(o6 >= 10 & o6 <= 950, "Management, business, financial", ifelse(o6 >= 1000 & o6 <= 3540, "Professional and related",
  ifelse(o6 >= 3600 & o6 <= 4650, "Service", ifelse(o6 >= 4700 & o6 <= 5940, "Sales and office",
  ifelse(o6 >= 6005 & o6 <= 7630, "Natural resources, construction, maintenance", ifelse(o6 >= 7700 & o6 <= 9750, "Production and transportation", "Other or NIU")))))))
i6 <- a$IND1990
a$ind <- factor(ifelse(i6 >= 10 & i6 <= 50, "Agriculture, mining", ifelse(i6 == 60, "Construction", ifelse(i6 >= 100 & i6 <= 392, "Manufacturing",
  ifelse(i6 >= 400 & i6 <= 472, "Transport, utilities", ifelse(i6 >= 500 & i6 <= 691, "Trade", ifelse(i6 >= 700 & i6 <= 712, "Finance, real estate",
  ifelse(i6 >= 721 & i6 <= 810, "Business, personal, entertainment services", ifelse(i6 >= 812 & i6 <= 840, "Health care",
  ifelse(i6 >= 842 & i6 <= 860, "Education", ifelse(i6 >= 861 & i6 <= 893, "Other professional services",
  ifelse(i6 >= 900 & i6 <= 932, "Public administration", "Other or NIU"))))))))))))
a$cls3 <- factor(ifelse(a$CLASSWKR %in% c(10, 13, 14), "Self-employed", ifelse(a$CLASSWKR %in% c(24, 25, 27, 28), "Government",
  ifelse(a$CLASSWKR %in% 20:23, "Private", "Other"))))
a$hours_ly <- ifelse(a$UHRSWORKLY < 999, a$UHRSWORKLY, NA)
a$wage <- ifelse(a$INCWAGE >= 99999998, NA, a$INCWAGE * a$CPI99 / cpi_2024)
a$ss_income <- as.integer(a$INCSS > 0 & a$INCSS < 999999)
a$ssi_income <- as.integer(a$INCSSI > 0 & a$INCSSI < 999999)
a$insured <- ifelse(a$ANYCOVNW == 9, NA, as.integer(a$ANYCOVNW == 1))

# Cross-sectional population: the full ASEC sample, including the oversample, because ASECWT is built for the whole
# supplement (corrected October 4, 2026). The linked cohort below excludes the oversample, which has no longitudinal weight.
pop <- subset(a, YEAR %in% 2016:2024 & AGE >= 25 & AGE <= 64 & EMPSTAT %in% c(10, 12) &
  DISABWRK %in% 1:2 & DIFFANY %in% 1:2 & ASECWT > 0)
pop$group <- grp(pop$DISABWRK, pop$DIFFANY)
xs <- pop |> group_by(group) |> summarise(records = n(), wsum = sum(ASECWT), .groups = "drop") |>
  mutate(share_pct = 100 * wsum / sum(wsum), per_year_millions = wsum / length(2016:2024) / 1e6)

# Supplementary Table S5: weighted profile with 95% CIs clustered by household. CPSID identifies a household across
# survey years (oversample households have one too), so households observed in more than one year share a cluster.
stopifnot(all(pop$CPSID > 0))
pop$hh <- pop$CPSID
wmean_cl <- function(y, w, cl) {
  k <- !is.na(y); y <- y[k]; w <- w[k]; cl <- cl[k]
  m <- sum(w * y) / sum(w); z <- rowsum(w * (y - m) / sum(w), cl); nc <- nrow(z)
  se <- sqrt(nc / (nc - 1) * sum(z^2))
  c(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se, var = sum(w * (y - m)^2) / sum(w))
}
onehot <- function(f, label) { m <- sapply(levels(f), function(l) as.integer(f == l)); colnames(m) <- paste0(label, ": ", levels(f)); m }
pvars <- cbind(age = pop$AGE, female = pop$female, onehot(pop$race4, "race"), onehot(pop$educ4, "education"),
  onehot(pop$occ6, "occupation"), onehot(pop$ind, "industry"), onehot(pop$cls3, "class"),
  hours_now = pop$hours_now, hours_last_year = pop$hours_ly, weeks_last_year = pop$WKSWORK1, full_year_last_year = pop$fullyear,
  full_time_last_year = pop$fulltime_ly, wage_income_2024usd = pop$wage, family_income_2024usd = pop$faminc, poverty = pop$poor,
  fair_or_poor_health = pop$fairpoor, disability_income = pop$dis_income, social_security_income = pop$ss_income,
  ssi_income = pop$ssi_income, insured_now = pop$insured, medicaid_now = pop$medicaid, medicare_now = pop$medicare)
profile <- do.call(rbind, lapply(colnames(pvars), function(v) {
  r <- lapply(levels(pop$group), function(g) { k <- pop$group == g; wmean_cl(pvars[k, v], pop$ASECWT[k], pop$hh[k]) })
  names(r) <- levels(pop$group); wlr <- r[["Work limitation only"]]; ssr <- r[["Standard screen only"]]
  data.frame(characteristic = v, neither = r$Neither[["mean"]], wl = wlr[["mean"]], wl_lo = wlr[["lo"]], wl_hi = wlr[["hi"]],
    six = ssr[["mean"]], both = r$Both[["mean"]], smd = (wlr[["mean"]] - ssr[["mean"]]) / sqrt((wlr[["var"]] + ssr[["var"]]) / 2))
}))
write.csv(profile, file.path(out, "table-s5-profile.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(as.data.frame(xs), file.path(out, "population-shares.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Linked cohort, as in the primary analysis.
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
d$exit <- as.integer(!d$EMPSTAT_next %in% c(10, 12))
d$dest <- ifelse(d$EMPSTAT_next %in% c(10, 12), "Employed", ifelse(d$EMPSTAT_next %in% 20:22, "Unemployed",
  ifelse(d$EMPSTAT_next == 32, "Unable", ifelse(d$EMPSTAT_next == 36, "Retired", "Other NILF"))))
d$y_unable <- as.integer(d$dest == "Unable"); d$y_unemployed <- as.integer(d$dest == "Unemployed")
d$y_retired <- as.integer(d$dest == "Retired"); d$y_other <- as.integer(d$dest == "Other NILF")
stopifnot(!any(d$educ4 == "Missing"))

# Participant flow (sequential exclusions).
yrs <- a$YEAR %in% 2016:2024
k1 <- yrs & a$ASECOVERP == 0; k2 <- k1 & a$AGE >= 25 & a$AGE <= 64; k3 <- k2 & a$EMPSTAT %in% c(10, 12)
k4 <- k3 & a$DISABWRK %in% 1:2 & a$DIFFANY %in% 1:2; k5 <- k4 & a$MISH %in% 1:4
flow <- c(respondents = sum(yrs), ex_oversample = sum(yrs) - sum(k1), ex_age = sum(k1) - sum(k2), ex_employed = sum(k2) - sum(k3),
  ex_items = sum(k3) - sum(k4), employed_both = sum(k4), ex_rotation = sum(k4) - sum(k5), eligible = sum(k5),
  ex_unlinked = sum(k5) - sum(x$linked), ex_weight = sum(x$linked) - nrow(d), cohort = nrow(d))
stopifnot(flow[["eligible"]] == nrow(eligible))
print(flow)

# ---------------------------------------------------------------- models
committed_rates <- md_table(f_analysis, "## Weighted exit rates by group (unadjusted)")
committed_dest <- md_table(f_profile, "## Exit destinations (linked pairs, weighted %)")
committed_primary <- rbind(md_table(f_analysis, "## Primary estimate", 1)[c("rd_pp", "ci_low", "ci_high")],
  md_table(f_analysis, "## Primary estimate", 2)[c("rd_pp", "ci_low", "ci_high")])
rownames(committed_primary) <- c("Work limitation only", "Standard screen only", "Both")
committed_sens <- md_table(f_analysis, "## Sensitivity")
committed_sec <- md_table(f_analysis, "## Secondary")
committed_chk <- md_table(f_profile, "## Part 2: timing and work-history checks (risk difference, work limitation only vs neither)")
rr_line <- grep("^Adjusted risk ratio", readLines(f_analysis, encoding = "UTF-8"), value = TRUE)
committed_rr <- as.numeric(regmatches(rr_line, gregexpr("[0-9]+\\.[0-9]+", rr_line))[[1]])
stopifnot(length(committed_rr) == 3)

t2raw <- d |> group_by(group) |> summarise(n = n(), events = sum(exit),
  exit_pct = 100 * weighted.mean(exit, LNKFW1YWT), unemployed_pct = 100 * weighted.mean(y_unemployed, LNKFW1YWT),
  unable_pct = 100 * weighted.mean(y_unable, LNKFW1YWT), retired_pct = 100 * weighted.mean(y_retired, LNKFW1YWT),
  other_pct = 100 * weighted.mean(y_other, LNKFW1YWT), .groups = "drop") |> as.data.frame()
base <- "i(age4) + female + i(race4) + i(educ4)"
rhs <- paste("i(group, ref = 'Neither') +", base)
terms <- paste0("group::", rownames(committed_primary)); tm <- terms[1]
m <- feols(as.formula(paste("exit ~", rhs, "| YEAR")), data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
ci <- confint(m)
rd <- data.frame(group = rownames(committed_primary), rd_pp = 100 * coef(m)[terms], ci_low = 100 * ci[terms, 1],
  ci_high = 100 * ci[terms, 2], p = coeftable(m)[terms, 4])
mp <- fepois(as.formula(paste("exit ~", rhs, "| YEAR")), data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
rr <- data.frame(est = exp(coef(mp)[terms]), lo = exp(confint(mp)[terms, 1]), hi = exp(confint(mp)[terms, 2]))
mo <- suppressWarnings(feglm(as.formula(paste("exit ~", rhs, "| YEAR")), data = d, family = "logit", weights = ~LNKFW1YWT,
  cluster = ~CPSID, notes = FALSE))
or <- data.frame(est = exp(coef(mo)[terms]), lo = exp(confint(mo)[terms, 1]), hi = exp(confint(mo)[terms, 2]))

# Rows whose committed values sit on a rounding tie at two decimals, re-estimated at full precision for Figure 3.
m_unw <- feols(as.formula(paste("exit ~", rhs, "| YEAR")), data = d, cluster = ~CPSID, notes = FALSE)
unw <- 100 * c(coef(m_unw)[[tm]], confint(m_unw)[tm, 1], confint(m_unw)[tm, 2])
m3 <- feols(as.formula(paste("exit ~", rhs, "+ i(group, fairpoor, ref = 'Neither') + fairpoor | YEAR")),
  data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
k3 <- c(tm, paste0(tm, ":fairpoor")); b3 <- sum(coef(m3)[k3]); se3 <- sqrt(sum(vcov(m3)[k3, k3]))
tq3 <- qt(.975, fixest::degrees_freedom(m3, "t"))
t3 <- rbind(good = 100 * c(coef(m3)[[tm]], confint(m3)[tm, 1], confint(m3)[tm, 2]),
  fairpoor = 100 * c(b3, b3 - tq3 * se3, b3 + tq3 * se3))

# Adjusted predicted percentages by group (marginal standardization over the cohort, linear probability model with
# year dummies). Destination margins add up to the exit margin because the destination indicators sum to exit.
margins <- function(y) {
  mm <- feols(as.formula(paste(y, "~", rhs, "+ i(YEAR)")), data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
  X <- model.matrix(mm, type = "rhs"); stopifnot(nrow(X) == nrow(d), all(colnames(X) %in% names(coef(mm))))
  abar <- colSums(X * d$LNKFW1YWT) / sum(d$LNKFW1YWT); gcols <- grep("^group::", colnames(X), value = TRUE)
  b <- coef(mm)[colnames(X)]; V <- vcov(mm)[colnames(X), colnames(X)]; tq <- qt(.975, fixest::degrees_freedom(mm, "t"))
  do.call(rbind, lapply(levels(d$group), function(g) {
    av <- abar; av[gcols] <- 0; if (g != "Neither") av[paste0("group::", g)] <- 1
    est <- sum(av * b); se <- sqrt(drop(t(av) %*% V %*% av))
    data.frame(outcome = y, group = g, est = 100 * est, lo = 100 * (est - tq * se), hi = 100 * (est + tq * se))
  }))
}
mg <- do.call(rbind, lapply(c("exit", "y_unable", "y_unemployed", "y_retired", "y_other"), margins))
mg_exit <- mg[mg$outcome == "exit", ]
mg_sum <- tapply(mg$est[mg$outcome != "exit"], mg$group[mg$outcome != "exit"], sum)[levels(d$group)]

checks <- data.frame(
  check = c("Linked counts match the primary analysis (121,447; 3,180; 4,664; 891)",
    "Weighted exit rates match the committed analysis (to 4 decimals)",
    "Exit destinations match the committed profile-timing results (to 3 decimals)",
    "Adjusted risk differences reproduce the committed primary model (to 4 decimals)",
    "Adjusted risk ratio reproduces the committed Poisson model (1.70, 1.51 to 1.91)",
    "Unweighted model reproduces the committed sensitivity row (to 4 decimals)",
    "Self-rated health strata reproduce the committed T3 rows (to 3 decimals)",
    "Adjusted margins: group difference equals the primary risk difference; destinations sum to exit"),
  met = c(all(t2raw$n == c(121447, 3180, 4664, 891)),
    all(abs(t2raw$exit_pct - committed_rates$exit_pct) < 5e-5) && all(abs(t2raw$unable_pct - committed_rates$unable_pct) < 5e-5) &&
      all(abs(t2raw$unemployed_pct - committed_rates$unemployed_pct) < 5e-5),
    all(abs(t2raw$unemployed_pct - committed_dest$unemployed) < 5e-4) && all(abs(t2raw$unable_pct - committed_dest$unable) < 5e-4) &&
      all(abs(t2raw$retired_pct - committed_dest$retired) < 5e-4) && all(abs(t2raw$other_pct - committed_dest$other_nilf) < 5e-4),
    all(abs(as.matrix(rd[c("rd_pp", "ci_low", "ci_high")]) - as.matrix(committed_primary)) < 5e-5),
    all(abs(round(unlist(rr[1, ]), 2) - committed_rr) < 1e-9),
    all(abs(unw - unlist(committed_sens[committed_sens$analysis == "Unweighted", c("rd_pp", "ci_low", "ci_high")])) < 5e-5 + 1e-9),
    all(abs(t3 - as.matrix(committed_chk[4:5, c("rd_pp", "ci_low", "ci_high")])) < 5e-4 + 1e-9),
    abs(mg_exit$est[2] - mg_exit$est[1] - rd$rd_pp[1]) < 1e-6 && all(abs(mg_sum - mg_exit$est) < 1e-6)))
print(checks)
stopifnot(all(checks$met))

# Checks added after the October 3 review (not prespecified): the direct contrast between the two single-measure groups,
# and the primary model with the 25 Armed Forces follow-ups counted as employed.
k2 <- c("group::Work limitation only", "group::Standard screen only")
dd <- coef(m)[[k2[1]]] - coef(m)[[k2[2]]]; dse <- sqrt(vcov(m)[k2[1], k2[1]] + vcov(m)[k2[2], k2[2]] - 2 * vcov(m)[k2[1], k2[2]])
dq <- qt(.975, fixest::degrees_freedom(m, "t"))
d$exit_af_employed <- as.integer(!d$EMPSTAT_next %in% c(1, 10, 12))
m_af <- feols(as.formula(paste("exit_af_employed ~", rhs, "| YEAR")), data = d, weights = ~LNKFW1YWT, cluster = ~CPSID, notes = FALSE)
posthoc <- data.frame(
  analysis = c("Work limitation only minus six-question disability only", "Armed Forces at follow-up counted as employed"),
  n = c(nrow(d), nobs(m_af)),
  rd_pp = 100 * c(dd, coef(m_af)[[tm]]),
  ci_low = 100 * c(dd - dq * dse, confint(m_af)[tm, 1]), ci_high = 100 * c(dd + dq * dse, confint(m_af)[tm, 2]),
  p = c(2 * pt(-abs(dd / dse), fixest::degrees_freedom(m, "t")), coeftable(m_af)[tm, 4]))
armed_n <- tapply(d$EMPSTAT_next == 1, d$group, sum)
write.csv(posthoc, file.path(out, "posthoc-checks.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------- Table 1
wpct <- function(y, w) { k <- !is.na(y); sprintf("%.1f", 100 * sum(w[k] * y[k]) / sum(w[k])) }
wmsd <- function(y, w) { k <- !is.na(y); mu <- sum(w[k] * y[k]) / sum(w[k])
  sprintf("%.1f (%.1f)", mu, sqrt(sum(w[k] * (y[k] - mu)^2) / sum(w[k]))) }
wq <- function(y, w, p) { k <- !is.na(y); o <- order(y[k]); cw <- cumsum(w[k][o]) / sum(w[k]); y[k][o][which(cw >= p)[1]] }
dollars <- function(v) formatC(round(v, -2), format = "d", big.mark = ",")
wmed <- function(y, w) sprintf("%s (%s–%s)", dollars(wq(y, w, .5)), dollars(wq(y, w, .25)), dollars(wq(y, w, .75)))
cols <- c(levels(d$group), "All")
idx <- function(g) if (g == "All") rep(TRUE, nrow(d)) else d$group == g
share <- function(g) if (g == "All") "100.0" else sprintf("%.1f", xs$share_pct[xs$group == g])
spec <- list(
  list("Linked workers, n", "", 0, function(k, w) fmt_n(sum(k))),
  list("Share of employed adults aged 25–64, %", "a", 0, NULL),
  list("Age, mean (SD), years", "", 0, function(k, w) wmsd(d$AGE[k], w)),
  list("Aged 55–64, %", "", 0, function(k, w) wpct(as.integer(d$AGE[k] >= 55), w)),
  list("Female, %", "", 0, function(k, w) wpct(d$female[k], w)),
  list("Race and ethnicity, %", "", 0, "header"),
  list("Hispanic", "", 1, function(k, w) wpct(as.integer(d$race4[k] == "Hispanic"), w)),
  list("Non-Hispanic Black", "", 1, function(k, w) wpct(as.integer(d$race4[k] == "NH Black"), w)),
  list("Non-Hispanic White", "", 1, function(k, w) wpct(as.integer(d$race4[k] == "NH White"), w)),
  list("Non-Hispanic, other or multiple races", "", 1, function(k, w) wpct(as.integer(d$race4[k] == "Other"), w)),
  list("Education, %", "", 0, "header"),
  list("Less than high school", "", 1, function(k, w) wpct(as.integer(d$educ4[k] == "<HS"), w)),
  list("High school diploma or equivalent", "", 1, function(k, w) wpct(as.integer(d$educ4[k] == "HS"), w)),
  list("Some college", "", 1, function(k, w) wpct(as.integer(d$educ4[k] == "Some college"), w)),
  list("Bachelor's degree or higher", "", 1, function(k, w) wpct(as.integer(d$educ4[k] == "BA+"), w)),
  list("Work in the previous calendar year", "", 0, "header"),
  list("Weeks worked, mean (SD)", "", 1, function(k, w) wmsd(d$WKSWORK1[k], w)),
  list("Worked 50–52 weeks, %", "", 1, function(k, w) wpct(d$fullyear[k], w)),
  list("Usually worked full time, %", "", 1, function(k, w) wpct(d$fulltime_ly[k], w)),
  list("Usual weekly hours at interview, mean (SD)", "b", 0, function(k, w) wmsd(d$hours_now[k], w)),
  list("Family income, median (IQR), 2024 US$", "c", 0, function(k, w) wmed(d$faminc[k], w)),
  list("Family income below the poverty line, %", "c", 0, function(k, w) wpct(d$poor[k], w)),
  list("Self-rated health fair or poor, %", "", 0, function(k, w) wpct(d$fairpoor[k], w)),
  list("Medicaid coverage at interview, %", "", 0, function(k, w) wpct(d$medicaid[k], w)),
  list("Medicare coverage at interview, %", "", 0, function(k, w) wpct(d$medicare[k], w)),
  list("Disability income in the previous year, %", "d", 0, function(k, w) wpct(d$dis_income[k], w)))
t1 <- do.call(rbind, lapply(spec, function(s) {
  vals <- vapply(cols, function(g) {
    if (is.null(s[[4]])) return(share(g))
    if (identical(s[[4]], "header")) return("")
    k <- idx(g); s[[4]](k, d$LNKFW1YWT[k])
  }, character(1))
  data.frame(characteristic = s[[1]], note = s[[2]], indent = s[[3]], t(vals), check.names = FALSE)
}))
names(t1)[4:8] <- cols
write.csv(t1, file.path(out, "table1.csv"), row.names = FALSE, fileEncoding = "UTF-8")
n_hours_na <- sum(is.na(d$hours_now)); n_inc_na <- sum(is.na(d$faminc)); n_pov_na <- sum(is.na(d$poor))

# ---------------------------------------------------------------- Table 2
gl <- c(Neither = "Neither", `Work limitation only` = "Work limitation only", `Standard screen only` = "Six-question disability only",
  Both = "Both", All = "All")
ci2 <- function(e, l, h, dg = 2) sprintf(paste0("%.", dg, "f (%.", dg, "f to %.", dg, "f)"), e, l, h)
sec <- committed_sec[match(c("Not in labor force, unable to work", "Unemployed"), committed_sec$analysis), ]
t2 <- rbind(
  data.frame(row = "Not employed one year later", n = "", pct = "", rd = "", p = "", rr = "", or = ""),
  data.frame(row = unname(gl[levels(d$group)]), n = fmt_n(t2raw$n), pct = sprintf("%.1f", t2raw$exit_pct),
    rd = c("Reference", ci2(rd$rd_pp, rd$ci_low, rd$ci_high)), p = c("", fmt_p(rd$p)),
    rr = c("Reference", ci2(rr$est, rr$lo, rr$hi)), or = c("Reference", ci2(or$est, or$lo, or$hi))),
  data.frame(row = "By status one year later, work limitation only vs neither", n = "", pct = "", rd = "", p = "", rr = "", or = ""),
  data.frame(row = c("Out of the labor force, unable to work", "Unemployed"), n = "",
    pct = sprintf("%.1f vs %.1f", c(t2raw$unable_pct[2], t2raw$unemployed_pct[2]), c(t2raw$unable_pct[1], t2raw$unemployed_pct[1])),
    rd = ci2(sec$rd_pp, sec$ci_low, sec$ci_high), p = fmt_p(sec$p), rr = "", or = ""))
t2$indent <- as.integer(!t2$row %in% c("Not employed one year later", "By status one year later, work limitation only vs neither"))
write.csv(t2, file.path(out, "table2.csv"), row.names = FALSE, fileEncoding = "UTF-8")
st1 <- data.frame(group = unname(gl[levels(d$group)]), n = fmt_n(t2raw$n), events = fmt_n(t2raw$events), not_employed = sprintf("%.1f", t2raw$exit_pct),
  unable = sprintf("%.1f", t2raw$unable_pct), unemployed = sprintf("%.1f", t2raw$unemployed_pct),
  retired = sprintf("%.1f", t2raw$retired_pct), other = sprintf("%.1f", t2raw$other_pct))
write.csv(st1, file.path(out, "table-s1-destinations.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(mg, file.path(out, "figure2-adjusted-margins.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------- figure style
# Palette: blue, orange, green and red, deepened or lightened so every adjacent pair stays distinct with
# colour-vision deficiency (checked with a palette validator). In Figure 2 the
# order red-orange-blue-green keeps orange and green apart. Values are printed on the bars, so colour is never the only cue.
ink <- "#000000"; ink2 <- "#404040"; muted <- "#6b6b6b"; gridc <- "#ebebeb"; edge <- "#404040"
blue <- "#2f6fb0"; orange <- "#f39c4a"; green <- "#4caf6a"; red <- "#c0392b"
blue_band <- "#e2ebf4"; wash <- "#f0f5f9"          # blue at 14% and 7% on white
fam <- "Arial"; sz <- 8 / .pt; sz_s <- 7.5 / .pt
save_fig <- function(p, name, w_mm, h_mm) {
  w <- w_mm / 25.4; h <- h_mm / 25.4
  cairo_pdf(file.path(out, paste0(name, ".pdf")), width = w, height = h, family = fam); print(p); invisible(dev.off())
  png(file.path(out, paste0(name, ".png")), width = w, height = h, units = "in", res = 300, type = "cairo", family = fam); print(p); invisible(dev.off())
  tiff(file.path(out, paste0(name, ".tiff")), width = w, height = h, units = "in", res = 1000, type = "cairo", compression = "lzw", family = fam)
  print(p); invisible(dev.off())
}
canvas <- function(xlim, ylim) list(coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off"), theme_void(base_family = fam),
  theme(plot.margin = margin(2, 2, 2, 2, "mm"), plot.background = element_rect(fill = "white", colour = NA)))

# ---------------------------------------------------------------- Figure 1: participant flow (units = mm)
lh <- 3.7; pad <- 2.8
boxes <- list(); texts <- list()
add_box <- function(x0, x1, ytop, lines, face = "plain", col = ink, align = "centre", fill = "white", border = edge, lwd = 0.3, h = NULL) {
  nl <- length(lines); hh <- if (is.null(h)) nl * lh + 2 * pad else h
  off <- (hh - nl * lh) / 2
  boxes[[length(boxes) + 1]] <<- data.frame(x0 = x0, x1 = x1, y0 = ytop - hh, y1 = ytop, fill = fill, border = border, lwd = lwd)
  texts[[length(texts) + 1]] <<- data.frame(x = if (align == "centre") (x0 + x1) / 2 else x0 + pad, y = ytop - off - (seq_len(nl) - 0.5) * lh,
    label = lines, face = rep_len(face, nl), col = rep_len(col, nl), hjust = if (align == "centre") 0.5 else 0)
  ytop - hh
}
mx0 <- 16; mx1 <- 90; mxc <- (mx0 + mx1) / 2; sx0 <- 98; sx1 <- 160; gap <- 15
main <- list(
  c("ASEC respondents, 2016–2024", paste("n =", fmt_n(flow[["respondents"]]))),
  c("Employed adults aged 25–64 with", "answers to both disability measures", paste("n =", fmt_n(flow[["employed_both"]]))),
  c("Due for interview the next year", "(rotation months 1–4)", paste("n =", fmt_n(flow[["eligible"]]))),
  c("Linked cohort", paste("n =", fmt_n(flow[["cohort"]]))))
side <- list(
  c("Excluded", paste("ASEC oversample:", fmt_n(flow[["ex_oversample"]])), paste("Not aged 25–64:", fmt_n(flow[["ex_age"]])),
    paste("Not employed:", fmt_n(flow[["ex_employed"]])), if (flow[["ex_items"]] > 0) paste("Missing a disability item:", fmt_n(flow[["ex_items"]]))),
  c("Excluded", "Rotation months 5–8, not due for", paste("interview the next year:", fmt_n(flow[["ex_rotation"]]))),
  c("Excluded", "No validated match in the next", paste("year's ASEC:", fmt_n(flow[["ex_unlinked"]])),
    if (flow[["ex_weight"]] > 0) paste("Linked, zero longitudinal weight:", fmt_n(flow[["ex_weight"]]))))
arrows <- list(); lines_df <- list(); y <- 0
for (i in seq_along(main)) {
  ln <- main[[i]]
  bottom <- add_box(mx0, mx1, y, ln, face = c(rep("plain", length(ln) - 1), "bold"))
  if (i < length(main)) {
    mid <- bottom - gap / 2; sl <- side[[i]]; sh <- length(sl) * lh + 2 * pad
    add_box(sx0, sx1, mid + sh / 2, sl, face = c("bold", rep("plain", length(sl) - 1)), col = c(ink, rep(ink2, length(sl) - 1)),
      align = "left", fill = "#f7f7f7")
    arrows[[length(arrows) + 1]] <- data.frame(x = mxc, y = bottom, xend = mxc, yend = bottom - gap)
    arrows[[length(arrows) + 1]] <- data.frame(x = mxc, y = mid, xend = sx0, yend = mid)
    y <- bottom - gap
  } else y_last <- bottom
}
gw <- 38; gx0 <- seq(0, 160 - gw, length.out = 4); gc <- gx0 + gw / 2; y_j <- y_last - 7; gtop <- y_j - 7
lines_df[[1]] <- data.frame(x = mxc, y = y_last, xend = mxc, yend = y_j)
lines_df[[2]] <- data.frame(x = gc[1], y = y_j, xend = gc[4], yend = y_j)
for (k in 1:4) arrows[[length(arrows) + 1]] <- data.frame(x = gc[k], y = y_j, xend = gc[k], yend = gtop)
gname <- list("Neither", "Work limitation only", c("Six-question", "disability only"), "Both")
gdef <- list(c("No work limitation,", "no difficulty"), c("Work limitation,", "no difficulty"), c("Difficulty, no", "work limitation"),
  c("Work limitation", "and difficulty"))
gh <- 5 * lh + 2 * pad
for (k in 1:4) {
  nn <- length(gname[[k]])
  add_box(gx0[k], gx0[k] + gw, gtop, c(gname[[k]], paste("n =", fmt_n(t2raw$n[k])), gdef[[k]]),
    face = c(rep("bold", nn), "plain", "plain", "plain"), col = c(rep(if (k == 2) blue else ink, nn + 1), muted, muted),
    fill = if (k == 2) blue_band else "white", border = if (k == 2) blue else edge, lwd = if (k == 2) 0.75 else 0.3, h = gh)
}
bx <- do.call(rbind, boxes); tx <- do.call(rbind, texts); ar <- do.call(rbind, arrows); ld <- do.call(rbind, lines_df)
f1_ylim <- c(gtop - gh - 1, 1)
p1 <- ggplot() +
  geom_segment(data = ld, aes(x = x, y = y, xend = xend, yend = yend), colour = edge, linewidth = 0.4) +
  geom_segment(data = ar, aes(x = x, y = y, xend = xend, yend = yend), colour = edge, linewidth = 0.4,
    arrow = arrow(length = unit(1.6, "mm"), type = "closed")) +
  geom_rect(data = bx, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill, colour = border, linewidth = lwd)) +
  geom_text(data = tx, aes(x = x, y = y, label = label, fontface = face, colour = col, hjust = hjust), family = fam, size = sz) +
  scale_fill_identity() + scale_colour_identity() + scale_linewidth_identity() + canvas(c(0, 160), f1_ylim)
save_fig(p1, "figure1", 164, diff(f1_ylim) + 4)

# ---------------------------------------------------------------- Figure 2: adjusted predicted % not employed, by status
dests <- c(y_unable = "Unable to work", y_unemployed = "Unemployed", y_retired = "Retired", y_other = "Other, incl. Armed Forces")
dcol <- c(y_unable = red, y_unemployed = orange, y_retired = blue, y_other = green)
dtxt <- c(y_unable = "white", y_unemployed = ink, y_retired = "white", y_other = ink)
mm2 <- c(label = 36, gap1 = 3, plot = 108, gap2 = 4, txt = 35); xmax2 <- 25; k2 <- xmax2 / mm2[["plot"]]; mm_row <- 12.5
x_lab2 <- -(mm2[["label"]] + mm2[["gap1"]]) * k2; x_txt2 <- xmax2 + mm2[["gap2"]] * k2; x_end2 <- xmax2 + (mm2[["gap2"]] + mm2[["txt"]]) * k2
gy <- c(4, 3, 2, 1); names(gy) <- levels(d$group); bh <- 0.31
seg <- do.call(rbind, lapply(levels(d$group), function(g) {
  v <- sapply(names(dests), function(o) mg$est[mg$outcome == o & mg$group == g])
  data.frame(group = g, outcome = names(dests), xmin = cumsum(c(0, head(v, -1))), xmax = cumsum(v), y = gy[[g]])
}))
seg$fill <- dcol[seg$outcome]; seg$tcol <- dtxt[seg$outcome]; seg$label <- sprintf("%.1f", seg$xmax - seg$xmin)
seg$show <- (seg$xmax - seg$xmin) / k2 >= nchar(seg$label) * 1.45 + 2
tot <- mg_exit; tot$y <- gy[tot$group]; tot$txt <- sprintf("%.1f (%.1f to %.1f)", tot$est, tot$lo, tot$hi)
tot$name <- unname(gl[tot$group]); tot$n <- paste("n =", fmt_n(t2raw$n))
tot$face <- ifelse(tot$group == "Work limitation only", "bold", "plain")
key_mm <- 3.6; y_leg <- 5.0
leg <- data.frame(outcome = names(dests), label = unname(dests), fill = dcol[names(dests)])
leg$w_mm <- key_mm + 1.8 + nchar(leg$label) * 1.5 + 6; leg$x <- cumsum(c(0, head(leg$w_mm, -1))) * k2
y_ax2 <- 0.42; brk2 <- seq(0, 25, 5)
p2 <- ggplot() +
  annotate("rect", xmin = x_lab2 - 1.5 * k2, xmax = x_end2, ymin = 3 - 0.48, ymax = 3 + 0.48, fill = wash) +
  annotate("segment", x = brk2[-1], xend = brk2[-1], y = y_ax2, yend = 4.5, colour = gridc, linewidth = 0.35) +
  geom_rect(data = seg, aes(xmin = xmin, xmax = xmax, ymin = y - bh, ymax = y + bh, fill = fill), colour = "white", linewidth = 0.5) +
  geom_text(data = seg[seg$show, ], aes(x = (xmin + xmax) / 2, y = y, label = label, colour = tcol), family = fam, size = sz_s) +
  annotate("segment", x = 0, xend = 0, y = y_ax2, yend = 4.5, colour = ink, linewidth = 0.5) +
  annotate("segment", x = 0, xend = xmax2, y = y_ax2, yend = y_ax2, colour = ink, linewidth = 0.5) +
  annotate("segment", x = brk2, xend = brk2, y = y_ax2, yend = y_ax2 - 0.08, colour = ink, linewidth = 0.5) +
  annotate("text", x = brk2, y = y_ax2 - 0.3, label = brk2, family = fam, size = sz, colour = ink) +
  annotate("text", x = xmax2 / 2, y = y_ax2 - 0.7, label = "Adjusted predicted percentage not employed one year later",
    family = fam, size = sz, colour = ink) +
  geom_text(data = tot, aes(x = x_lab2, y = y + 0.13, label = name, fontface = face), hjust = 0, family = fam, size = sz, colour = ink) +
  geom_text(data = tot, aes(x = x_lab2, y = y - 0.2, label = n), hjust = 0, family = fam, size = sz_s, colour = muted) +
  geom_text(data = tot, aes(x = x_txt2, y = y, label = txt, fontface = face), hjust = 0, family = fam, size = sz, colour = ink) +
  annotate("text", x = x_txt2, y = 4.56, label = "Total, % (95% CI)", hjust = 0, family = fam, fontface = "bold", size = sz, colour = ink) +
  geom_rect(data = leg, aes(xmin = x, xmax = x + key_mm * k2, ymin = y_leg - key_mm / 2 / mm_row, ymax = y_leg + key_mm / 2 / mm_row, fill = fill)) +
  geom_text(data = leg, aes(x = x + (key_mm + 1.8) * k2, y = y_leg, label = label), hjust = 0, family = fam, size = sz, colour = ink) +
  scale_fill_identity() + scale_colour_identity() + canvas(c(x_lab2 - 1.5 * k2, x_end2), c(-0.45, 5.35))
save_fig(p2, "figure2", sum(mm2) + 1.5 + 4, 5.8 * mm_row + 4)

# ---------------------------------------------------------------- Figure 3: forest plot (committed estimates)
prim <- md_table(f_analysis, "## Primary estimate", 1)
sens <- committed_sens; chk <- committed_chk
stopifnot(nrow(sens) == 5, nrow(chk) == 5, abs(chk$rd_pp[1] - prim$rd_pp) < 1e-3, sens$analysis[1] == "Unweighted")
sens[1, c("rd_pp", "ci_low", "ci_high")] <- as.list(unw)
chk[4:5, c("rd_pp", "ci_low", "ci_high")] <- t3
fig <- data.frame(
  section = c("Primary analysis", rep("Prespecified sensitivity analyses", 5), rep("Work history and health checks", 4)),
  label = c("Primary specification", "Unweighted", "Inverse-probability-of-linkage weights",
    sprintf("Excluding 2020–2021 follow-up (n = %s)", fmt_n(sens$n[3])), "State fixed effects added", "Standard errors clustered by state",
    "Previous-year work history added", sprintf("Full-year, full-time last year (n = %s)", fmt_n(chk$n[3])),
    "Self-rated health good to excellent", "Self-rated health fair or poor"),
  source = c(prim$analysis, sens$analysis, chk$check[2:5]),
  n = c(prim$n, sens$n, chk$n[2:5]), rd = c(prim$rd_pp, sens$rd_pp, chk$rd_pp[2:5]),
  lo = c(prim$ci_low, sens$ci_low, chk$ci_low[2:5]), hi = c(prim$ci_high, sens$ci_high, chk$ci_high[2:5]))
stopifnot(grepl("^T1", fig$source[7]), grepl("^T2", fig$source[8]), grepl("good$", fig$source[9]), grepl("fair or poor$", fig$source[10]),
  grepl("2020 or 2021", fig$source[4]))
write.csv(fig, file.path(out, "figure3-estimates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
mm3 <- c(label = 65, gap1 = 2, forest = 82, gap2 = 4, rd = 33)
xr <- c(-1, 16); k3 <- diff(xr) / mm3[["forest"]]
x_lab <- xr[1] - (mm3[["label"]] + mm3[["gap1"]]) * k3; x_rd <- xr[2] + mm3[["gap2"]] * k3; x_end <- xr[2] + (mm3[["gap2"]] + mm3[["rd"]]) * k3
fig$y <- c(12, 9.5:5.5, 3:0)
heads <- data.frame(y = c(13, 10.5, 4), label = unique(fig$section))
y_col <- 14.2; y_rule <- 13.65; y_axis <- -0.7; ylim3 <- c(-2.5, 14.7); mm_per_y <- 5
fig$colour <- ifelse(fig$section == "Work history and health checks", green, blue)
fig$shape <- ifelse(seq_len(nrow(fig)) == 1, 23, 21); fig$size <- ifelse(seq_len(nrow(fig)) == 1, 3.6, 2.7)
fig$face <- ifelse(seq_len(nrow(fig)) == 1, "bold", "plain")
fig$ci_txt <- sprintf("%.2f (%.2f to %.2f)", fig$rd, fig$lo, fig$hi)
brk <- seq(0, 15, 5)
p3 <- ggplot() +
  annotate("rect", xmin = x_lab - 1 * k3, xmax = x_end, ymin = 12 - 0.45, ymax = 12 + 0.45, fill = wash) +
  annotate("rect", xmin = fig$lo[1], xmax = fig$hi[1], ymin = y_axis, ymax = y_rule - 0.25, fill = blue, alpha = 0.14) +
  annotate("segment", x = brk[-1], xend = brk[-1], y = y_axis, yend = y_rule - 0.25, colour = gridc, linewidth = 0.35) +
  annotate("segment", x = fig$rd[1], xend = fig$rd[1], y = y_axis, yend = y_rule - 0.25, colour = blue, linewidth = 0.4, alpha = 0.7) +
  annotate("segment", x = 0, xend = 0, y = y_axis, yend = y_rule - 0.25, colour = ink, linewidth = 0.5) +
  annotate("text", x = fig$hi[1] + 0.25, y = y_rule - 0.6, label = "Primary estimate and 95% CI", hjust = 0, family = fam, size = sz_s, colour = blue) +
  annotate("segment", x = x_lab, xend = x_end, y = y_rule, yend = y_rule, colour = ink, linewidth = 0.4) +
  annotate("segment", x = xr[1], xend = xr[2], y = y_axis, yend = y_axis, colour = ink, linewidth = 0.5) +
  annotate("segment", x = brk, xend = brk, y = y_axis, yend = y_axis - 0.2, colour = ink, linewidth = 0.5) +
  annotate("text", x = brk, y = y_axis - 0.6, label = brk, family = fam, size = sz, colour = ink) +
  annotate("text", x = mean(xr), y = y_axis - 1.45, label = "Adjusted risk difference vs neither, percentage points",
    family = fam, size = sz, colour = ink) +
  annotate("text", x = c(x_lab, x_rd), y = y_col, label = c("Analysis", "Risk difference (95% CI)"), hjust = 0,
    family = fam, fontface = "bold", size = sz, colour = ink) +
  geom_text(data = heads, aes(x = x_lab, y = y, label = label), hjust = 0, family = fam, fontface = "bold", size = sz, colour = ink) +
  geom_text(data = fig, aes(x = x_lab + 3 * k3, y = y, label = label, fontface = face), hjust = 0, family = fam, size = sz, colour = ink) +
  geom_text(data = fig, aes(x = x_rd, y = y, label = ci_txt, fontface = face), hjust = 0, family = fam, size = sz, colour = ink) +
  geom_segment(data = fig, aes(x = lo, xend = hi, y = y, yend = y, colour = colour), linewidth = 0.7, lineend = "round") +
  geom_point(data = fig, aes(x = rd, y = y, fill = colour, shape = shape, size = size), stroke = 0.7, colour = "white") +
  scale_colour_identity() + scale_fill_identity() + scale_shape_identity() + scale_size_identity() + canvas(c(x_lab - 1 * k3, x_end), ylim3)
save_fig(p3, "figure3", sum(mm3) + 1 + 4, diff(ylim3) * mm_per_y + 4)

# ---------------------------------------------------------------- exhibits.md
con <- file(file.path(out, "exhibits.md"), "w", encoding = "UTF-8")
say <- function(...) cat(..., file = con, sep = "")
kab <- function(x) cat(knitr::kable(x, format = "pipe", row.names = FALSE, align = c("l", rep("r", ncol(x) - 1))), file = con, sep = "\n")
say("# Paper exhibits: work limitation outside the six-question disability standard and next-year employment exit\n\n")
say("Generated by `scripts/work_limitation_paper_exhibits.R` from CPS extract 20 and the committed results in `",
  basename(f_analysis), "` and `", basename(f_profile), "`. Table values, margins and the flow counts come from the data and are ",
  "checked against those files. Figure 3 plots the committed estimates; the unweighted model and the two self-rated health ",
  "strata are re-estimated here because their stored values (for example 3.175 and 14.995) fall on a rounding tie at two decimals. ",
  "Not prespecified, added for the paper: adjusted odds ratios, adjusted risk ratios for the two comparison groups, and the ",
  "adjusted predicted percentages in Figure 2.\n\n")
say("## Integrity checks\n\n"); kab(checks); say("\n")
say("## Participant flow\n\n"); kab(data.frame(step = names(flow), n = fmt_n(unname(flow)))); say("\n")

say("## Table 1. Baseline characteristics of linked workers aged 25–64, by baseline group, CPS ASEC 2016–2024\n\n")
t1md <- t1
t1md$characteristic <- paste0(ifelse(t1$indent == 1, "&emsp;", ""), t1$characteristic, ifelse(nzchar(t1$note), paste0("<sup>", t1$note, "</sup>"), ""))
t1md <- t1md[c("characteristic", cols)]; names(t1md) <- c("Characteristic", unname(gl[cols])); kab(t1md)
wl <- xs[xs$group == "Work limitation only", ]
say("\nWeighted percentages, means and medians; unweighted counts. Work limitation: a health problem that prevented or limited work in the previous calendar year. ",
  "Six-question disability: a yes to any of the six questions on hearing, vision, cognition, walking, self-care and errands. ",
  "SD, standard deviation; IQR, interquartile range.\n\n",
  "<sup>a</sup> All employed adults aged 25–64 in ASEC 2016–2024, full sample with the ASEC weight (n = ", fmt_n(sum(xs$records)), "); the work-limitation-only group ",
  "averaged ", sprintf("%.1f", wl$per_year_millions), " million workers a year.\n\n",
  "<sup>b</sup> Excludes ", fmt_n(n_hours_na), " workers whose hours vary.\n\n",
  "<sup>c</sup> Previous calendar year; 2024 dollars.\n\n",
  "<sup>d</sup> Asked mainly of people who report a work limitation; not comparable across groups.\n\n")

say("## Table 2. Not employed one year later, by baseline group (n = ", fmt_n(nrow(d)), ")\n\n")
t2md <- t2
t2md$row <- ifelse(t2$indent == 1, paste0("&emsp;", t2$row), paste0("**", t2$row, "**"))
t2md$indent <- NULL
names(t2md) <- c("Baseline group", "n", "Not employed, %<sup>a</sup>", "Adjusted risk difference, percentage points (95% CI)<sup>b</sup>",
  "P<sup>b</sup>", "Adjusted risk ratio (95% CI)<sup>c</sup>", "Adjusted odds ratio (95% CI)<sup>d</sup>")
kab(t2md)
say("\nWeighted; n is unweighted. Models adjust for age group, sex, race and ethnicity, education and survey year, ",
  "with standard errors clustered by household.\n\n",
  "<sup>a</sup> Lower panel: work limitation only vs neither.\n\n",
  "<sup>b</sup> Linear probability model (primary).\n\n",
  "<sup>c</sup> Poisson model.\n\n",
  "<sup>d</sup> Logistic model; not prespecified.\n\n")

say("## Figure 1\n\n![Figure 1](figure1.png)\n\n")
say("**Figure 1.** Selection of the analytic cohort, CPS ASEC 2016–2024. Difficulty: a yes to at least one of the ",
  "six federal disability questions.\n\n")
say("## Figure 2\n\n![Figure 2](figure2.png)\n\n")
say("**Figure 2.** Adjusted predicted percentage not employed one year later, by baseline group and status at follow-up. ",
  "Adjusted for age group, sex, race and ethnicity, education and survey year; segments add up to the total. Other includes ",
  sum(armed_n), " people who had joined the Armed Forces.\n\n")
say("## Figure 3\n\n![Figure 3](figure3.png)\n\n")
say("**Figure 3.** Adjusted risk difference in not being employed one year later, work limitation only vs neither, across ",
  "specifications. Lines are 95% confidence intervals; the shaded band is the primary estimate's interval. Blue: primary and ",
  "prespecified sensitivity analyses; green: checks specified after the primary result. n = ", fmt_n(nrow(d)), " unless shown.\n\n")
say("## Supplementary Table S1. Unadjusted status one year later, by baseline group (weighted %)\n\n")
names(st1) <- c("Baseline group", "n", "Not employed, n (unweighted)", "Not employed", "Out of the labor force, unable to work", "Unemployed", "Retired",
  "Out of the labor force, other reason, or Armed Forces")
kab(st1)
say("\nThe four statuses add up to the percentage not employed, apart from rounding. Other includes people who had joined the Armed Forces (",
  paste(sprintf("%s %d", unname(gl[names(armed_n)]), as.vector(armed_n)), collapse = "; "), ").\n\n")
say("## Checks added after review (not prespecified)\n\n"); kab(posthoc); say("\n")
say("Files: `figure1`–`figure3` as `.pdf` (vector, fonts embedded), `.tiff` (1,000 dpi, LZW) and `.png` (300 dpi preview); ",
  "`table1.csv`, `table2.csv`, `table-s1-destinations.csv`, `figure2-adjusted-margins.csv`, `figure3-estimates.csv`.\n\n")
say(R.version.string, "; fixest ", as.character(packageVersion("fixest")), "; ggplot2 ", as.character(packageVersion("ggplot2")),
  "; CPS extract 20 (ASEC 2016–2025).\n")
close(con)
cat("wrote", out, "\n")
