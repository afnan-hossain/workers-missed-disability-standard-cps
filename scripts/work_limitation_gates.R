# Outcome-blind feasibility criteria A and B from the analysis plan (Supplementary Tables S3a-c).
#   Rscript scripts/work_limitation_gates.R > results/work-limitation-exit/2026-10-03-gates.md
# Gate A uses the linkage-check extracts (no follow-up employment). Gate B uses wl_prevalence,
# ASEC 2010-2014, which has no disability item and lies outside the 2016-2025 analysis window.
suppressPackageStartupMessages(library(dplyr))
table_out <- function(x) cat(knitr::kable(x, format = "pipe", row.names = FALSE, digits = 4), sep = "\n")
kish <- function(w) sum(w)^2 / sum(w^2)
read_x <- function(folder) {
  ddi <- ipumsr::read_ipums_ddi(list.files(folder, "\\.xml$", full.names = TRUE))
  as.data.frame(haven::zap_labels(ipumsr::read_ipums_micro(ddi, verbose = FALSE)))
}
# Linkage rules as specified in the analysis plan.
link <- function(base, follow, years, keep_follow = character()) {
  eligible <- subset(base, YEAR %in% years & AGE >= 25 & AGE <= 64 & EMPSTAT %in% c(10, 12) & ASECOVERP == 0 & MISH %in% 1:4)
  f <- subset(follow, CPSIDV > 0 & ASECOVERP == 0)[c("YEAR", "CPSIDV", "MISH", "SEX", "RACE", "AGE", keep_follow)]
  stopifnot(!anyDuplicated(f[c("YEAR", "CPSIDV")]))
  f$YEAR <- f$YEAR - 1
  names(f)[-(1:2)] <- paste0(names(f)[-(1:2)], "_next")
  j <- left_join(eligible, f, by = c("YEAR", "CPSIDV"), relationship = "many-to-one")
  j$linked <- eligible$CPSIDV > 0 & !is.na(j$MISH_next) & j$MISH_next == j$MISH + 4 &
    j$SEX_next == j$SEX & j$RACE_next == j$RACE & (j$AGE_next - j$AGE) %in% 0:2
  j$linked[is.na(j$linked)] <- FALSE
  j
}
smd <- function(a, b) {
  if (all(a %in% 0:1) && all(b %in% 0:1)) (mean(a) - mean(b)) / sqrt((mean(a) * (1 - mean(a)) + mean(b) * (1 - mean(b))) / 2)
  else (mean(a) - mean(b)) / sqrt((var(a) + var(b)) / 2)
}
cat("# Outcome-blind gates: work limitation and next-year employment exit\n\n")

# ---------------------------------------------------------------- Gate A
b <- read_x("data/ipums-cps/linkage-check/asec_baseline")
f <- read_x("data/ipums-cps/linkage-check/asec_followup")
stopifnot(!"EMPSTAT" %in% names(f))
j <- link(b, f, 2016:2024)
j <- j[j$DISABWRK %in% 1:2 & j$DIFFANY %in% 1:2, ]
j$group <- ifelse(j$DISABWRK == 2, ifelse(j$DIFFANY == 2, "Both", "Work limitation only"),
  ifelse(j$DIFFANY == 2, "Standard screen only", "Neither"))
cat("## Gate A: differential attrition (baselines 2016–2024)\n\n")
rates <- j |> group_by(group) |> summarise(eligible = n(), linkage_rate = mean(linked), n_linked = sum(linked), .groups = "drop")
table_out(rates)
gap <- abs(diff(rates$linkage_rate[match(c("Work limitation only", "Neither"), rates$group)]))
bal <- do.call(rbind, lapply(c("Work limitation only", "Neither"), function(g) {
  x <- j[j$group == g, ]
  vars <- list(age = x$AGE, female = as.integer(x$SEX == 2), white = as.integer(x$RACE == 100), black = as.integer(x$RACE == 200),
    other_race = as.integer(!x$RACE %in% c(100, 200)))
  data.frame(group = g, variable = names(vars), smd_linked_vs_unlinked = vapply(vars, function(v) smd(v[x$linked], v[!x$linked]), 0))
}))
cat("\nStandardized differences, linked minus unlinked:\n\n"); table_out(bal)
a_pass <- gap <= .05 && all(abs(bal$smd_linked_vs_unlinked) <= .10)
cat(sprintf("\nLinkage-rate gap: %.1f points (limit 5). Largest |SMD|: %.3f (limit 0.10).\n\n", 100 * gap, max(abs(bal$smd_linked_vs_unlinked))))

# ---------------------------------------------------------------- Gate B
p <- read_x("data/ipums-cps/work-limitation-exit/wl_prevalence")
stopifnot(!any(c("DISABWRK", "DIFFANY") %in% names(p)), all(p$YEAR %in% 2010:2014))
k <- link(p, p, 2010:2013, keep_follow = "EMPSTAT")
k <- k[k$linked & k$LNKFW1YWT > 0, ]
k$exit <- as.integer(!k$EMPSTAT_next %in% c(10, 12))
p0 <- weighted.mean(k$exit, k$LNKFW1YWT)
cat("## Gate B: precision\n\n")
cat(sprintf("Out-of-window exit rate (employed 25–64, ASEC 2010–2013 linked to 2011–2014, weighted): p0 = %.4f (n = %d linked, positive-weight pairs). No disability item was read.\n\n",
  p0, nrow(k)))
table_out(k |> group_by(baseline = YEAR) |> summarise(pairs = n(), exit_rate = weighted.mean(exit, LNKFW1YWT), .groups = "drop"))
w <- j[j$linked & j$LNKFW1YWT > 0, ]
ne <- kish(w$LNKFW1YWT[w$group == "Work limitation only"]); n0 <- kish(w$LNKFW1YWT[w$group == "Neither"])
proj <- do.call(rbind, lapply(c(1.5, 2, 2.5), function(r) {
  p1 <- min(.5, r * p0); v <- 2 * (p1 * (1 - p1) / ne + p0 * (1 - p0) / n0)
  data.frame(ratio = r, p1 = p1, neff_exposed = ne, neff_reference = n0,
    half_width_pp = 100 * qnorm(.975) * sqrt(v), mde80_pp = 100 * (qnorm(.975) + qnorm(.8)) * sqrt(v))
}))
cat("\nProjection with extra design effect 2.0 (gate uses ratio 2.5):\n\n"); table_out(proj)
hw <- proj$half_width_pp[proj$ratio == 2.5]

cat("\n## Gates\n\n")
table_out(data.frame(gate = c("A differential attrition", "B precision"),
  value = c(sprintf("gap %.1f points; max |SMD| %.3f", 100 * gap, max(abs(bal$smd_linked_vs_unlinked))),
    sprintf("half-width %.2f points (limit 2.0)", hw)),
  met = c(a_pass, hw <= 2)))
cat("\n", R.version.string, "; CPS extracts 3, 4 and 18.\n", sep = "")
