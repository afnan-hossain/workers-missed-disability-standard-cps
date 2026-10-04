# Outcome-blind feasibility criteria S1-S3 for a SIPP replication (Supplementary Table S7).
#   Rscript scripts/sipp_feasibility.R > results/work-limitation-sipp/2026-10-03-feasibility.md
# Reads baseline variables from data year Y and only identifiers from data year Y+1: no follow-up
# employment status (RMESR) is read. Raw files: data/sipp/raw/ (not in Git).
#
# Operational definitions fixed here, before any outcome is read (the plan did not specify them):
#   employed at baseline = RMESR 1-5 (with a job at least one week) in December of Y-1 (MONTHCODE 12);
#   not employed at outcome = RMESR 6-8 (no job all month) in December of Y;
#   stable prior year = RMESR 1-3 (with a job the entire month) in all 12 months of Y-1.
suppressPackageStartupMessages(library(dplyr))
options(warn = 1)  # print each warning as it occurs (to the log)
raw <- "data/sipp/raw"
table_out <- function(x) cat(knitr::kable(x, format = "pipe", row.names = FALSE, digits = 3), sep = "\n")
kish <- function(w) if (length(w)) sum(w)^2 / sum(w^2) else 0
six <- c("ESEEING", "EHEARING", "ECOGNIT", "EAMBULAT", "ESELFCARE", "EERRANDS")
base_cols <- c("SSUID", "PNUM", "MONTHCODE", "SPANEL", "SWAVE", "TAGE", "RMESR", "EDISABL", "EFINDJOB", six, "WPFINWGT")
# Each data-year file is read once into a slim cache (data/sipp/raw/cache, not in Git). December
# employment in data year Y+1 is the outcome month of pair Y->Y+1 and the baseline month of pair
# Y+1->Y+2; this script never links it to pair Y's exposure groups.
cache_dir <- file.path(raw, "cache"); dir.create(cache_dir, showWarnings = FALSE)
read_pu <- function(y, cols) {
  cf <- file.path(cache_dir, sprintf("pu%d_slim.rds", y))
  if (!file.exists(cf)) {
    f <- file.path(raw, sprintf("pu%d_csv.zip", y))
    x <- vroom::vroom(f, delim = "|", col_select = dplyr::any_of(base_cols), show_col_types = FALSE, progress = FALSE,
      col_types = vroom::cols(SSUID = "c", .default = "d"))
    names(x) <- toupper(names(x)); saveRDS(x, cf)
  }
  readRDS(cf)[cols]
}
read_wgt <- function(y) {
  f <- file.path(raw, if (y == 2019) "lgtwgt2019_csv.zip" else sprintf("lgtwgt%dyr2_csv.zip", y))
  x <- vroom::vroom(f, delim = "|", show_col_types = FALSE, progress = FALSE, col_types = vroom::cols(SSUID = "c", ssuid = "c", .default = "d"))
  names(x) <- toupper(names(x)); x[c("SSUID", "PNUM", "FINYR2")]
}
cat("# Outcome-blind feasibility: SIPP replication of work limitation and employment exit\n\n")
cat("No follow-up employment status is read.\n\n")

pairs <- list()
for (y in 2018:2024) {
  b <- read_pu(y, base_cols)
  stable <- b |> group_by(SSUID, PNUM) |> summarise(months = n(), stable = all(RMESR %in% 1:3) & n() == 12, .groups = "drop")
  p <- b |> filter(MONTHCODE == 12) |> left_join(stable, by = c("SSUID", "PNUM"))
  stopifnot(!anyDuplicated(p[c("SSUID", "PNUM")]))
  p <- p |> filter(TAGE >= 25, TAGE <= 64, RMESR %in% 1:5, EDISABL %in% 1:2, if_all(all_of(six), ~ .x %in% 1:2))
  anysix <- rowSums(p[six] == 1) > 0
  p$group <- factor(ifelse(p$EDISABL == 1, ifelse(anysix, "Both", "Work limitation only"), ifelse(anysix, "Six-item only", "Neither")),
    levels = c("Neither", "Work limitation only", "Six-item only", "Both"))
  nxt <- read_pu(y + 1, c("SSUID", "PNUM", "SPANEL", "SWAVE")) |> distinct()
  stopifnot(!"RMESR" %in% names(nxt))
  continuing <- distinct(nxt, SPANEL, SWAVE) |> mutate(SWAVE = SWAVE - 1, continues = TRUE)
  p <- p |> left_join(continuing, by = c("SPANEL", "SWAVE"))
  p$continues[is.na(p$continues)] <- FALSE
  w <- read_wgt(y + 1)
  p <- p |> left_join(w, by = c("SSUID", "PNUM"), relationship = "many-to-one")
  p$in_next <- paste(p$SSUID, p$PNUM) %in% paste(nxt$SSUID, nxt$PNUM)
  p$linked <- p$continues & p$in_next & !is.na(p$FINYR2) & p$FINYR2 > 0
  p$pair <- sprintf("%d->%d", y, y + 1)
  pairs[[length(pairs) + 1]] <- p
  rm(b, nxt, w); gc(verbose = FALSE)
}
all <- bind_rows(pairs)
fu <- all[all$continues, ]

cat("## Baseline and follow-up eligibility by pair\n\n")
table_out(all |> group_by(pair) |> summarise(employed_25_64_valid = n(), panel_continues = sum(continues),
  linkage_rate = mean(linked[continues]), work_limitation_only = sum(group == "Work limitation only"),
  wl_linked = sum(linked & group == "Work limitation only"), n_linked = sum(linked), .groups = "drop"))

cat("\n## S1 Linkage (people whose panel continues)\n\n")
s1 <- fu |> group_by(group) |> summarise(eligible = n(), linkage_rate = mean(linked), n_linked = sum(linked), .groups = "drop")
table_out(s1)
overall <- mean(fu$linked)
gap <- abs(diff(s1$linkage_rate[match(c("Work limitation only", "Neither"), s1$group)]))
cat(sprintf("\nOverall linkage %.1f%% (minimum 60%%); work-limitation-only versus neither gap %.1f points (maximum 5).\n", 100 * overall, 100 * gap))

cat("\n## S2 Counts (linked, positive two-year weight)\n\n")
lk <- all[all$linked, ]
by_pair <- lk |> filter(group == "Work limitation only") |> count(pair, name = "wl_pairs")
table_out(by_pair)
n_wl <- sum(lk$group == "Work limitation only")
cat(sprintf("\nPooled work-limitation-only pairs: %d (minimum 1,000). Pairs with at least 100: %d of 7 (minimum 5).\n", n_wl, sum(by_pair$wl_pairs >= 100)))
cat("\nAll groups, linked:\n\n"); table_out(lk |> count(group, name = "linked_pairs"))

cat("\n## S3 Precision\n\n")
proj <- function(x, label) {
  ne <- kish(x$FINYR2[x$group == "Work limitation only"]); n0 <- kish(x$FINYR2[x$group == "Neither"])
  p0 <- .0749; p1 <- 1.7 * p0; v <- 2 * (p1 * (1 - p1) / ne + p0 * (1 - p0) / n0)
  data.frame(analysis = label, n_wl = sum(x$group == "Work limitation only"), neff_wl = ne, neff_neither = n0,
    mde80_pp = 100 * (qnorm(.975) + qnorm(.8)) * sqrt(v), half_width_pp = 100 * qnorm(.975) * sqrt(v))
}
pr <- rbind(proj(lk, "Primary"), proj(lk[lk$stable %in% TRUE, ], "Stable prior year (employed every month of Y-1)"))
table_out(pr)
cat("\np0 = 7.49% (out-of-window CPS rate), p1 = 1.7 p0, extra design effect 2.0.\n")

cat("\n## Gates\n\n")
s1_ok <- overall >= .60 && gap <= .05
s2_ok <- n_wl >= 1000 && sum(by_pair$wl_pairs >= 100) >= 5
s3_ok <- pr$mde80_pp[1] <= 4
table_out(data.frame(gate = c("S1 linkage", "S2 counts", "S3 precision (primary)", "S3 precision (stable prior year)"),
  value = c(sprintf("%.1f%% linked; gap %.1f points", 100 * overall, 100 * gap), sprintf("%d WL-only pairs; %d pairs >= 100", n_wl, sum(by_pair$wl_pairs >= 100)),
    sprintf("MDE %.2f points", pr$mde80_pp[1]), sprintf("MDE %.2f points (informational)", pr$mde80_pp[2])),
  met = c(s1_ok, s2_ok, s3_ok, pr$mde80_pp[2] <= 4)))
cat("\n", R.version.string, "; SIPP public-use data years 2018-2025 (U.S. Census Bureau).\n", sep = "")
