# Retrieval utility for the study extracts.
# Usage: Rscript scripts/work_limitation_extracts.R submit|download <name> [...]
# Reads the IPUMS API key from the IPUMS_API_KEY environment variable, which ipumsr uses directly.
if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) stop("Set the IPUMS_API_KEY environment variable to your IPUMS API key.")

root <- "data/ipums-cps/work-limitation-exit"
definitions <- list(
  # Gate B: out-of-window exit rate. Employment status at both waves; no disability item.
  wl_prevalence = list(samples = paste0("cps", 2010:2014, "_03s"),
    variables = c("YEAR", "CPSIDV", "MISH", "AGE", "SEX", "RACE", "EMPSTAT", "ASECOVERP", "LNKFW1YWT")),
  # Analysis: baselines 2016-2024 and follow-ups 2017-2025 in one extract (requested after the
  # pre-outcome deviation was recorded).
  wl_analysis = list(samples = paste0("cps", 2016:2025, "_03s"),
    variables = c("YEAR", "SERIAL", "CPSID", "CPSIDV", "MISH", "AGE", "SEX", "RACE", "HISPAN", "EDUC", "STATEFIP",
      "EMPSTAT", "DISABWRK", "DIFFANY", "ASECOVERP", "LNKFW1YWT", "ASECWT")),
  # Profile and timing checks.
  wl_profile = list(samples = paste0("cps", 2016:2025, "_03s"),
    variables = c("YEAR", "SERIAL", "CPSID", "CPSIDV", "MISH", "AGE", "SEX", "RACE", "HISPAN", "EDUC", "STATEFIP",
      "EMPSTAT", "DISABWRK", "DIFFANY", "ASECOVERP", "LNKFW1YWT", "ASECWT",
      "OCC2010", "IND1990", "CLASSWKR", "UHRSWORKT", "WKSWORK1", "UHRSWORKLY", "FULLPART", "WHYPTLY", "INCWAGE",
      "FTOTVAL", "OFFPOV", "HEALTH", "INCDISAB", "INCSS", "INCSSI", "ANYCOVNW", "HIMCAIDNW", "HIMCARENW", "CPI99"))
)
args <- commandArgs(trailingOnly = TRUE)
for (name in args[-1]) {
  d <- definitions[[name]]
  folder <- file.path(root, name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  manifest <- file.path(folder, "request.json")
  if (identical(args[1], "submit")) {
    ex <- ipumsr::submit_extract(ipumsr::define_extract_micro("cps", paste("work limitation exit:", name),
      samples = d$samples, variables = d$variables))
    jsonlite::write_json(list(collection = "cps", number = ex$number, samples = d$samples, variables = d$variables,
      submitted = format(Sys.time(), tz = "UTC", usetz = TRUE)), manifest, auto_unbox = TRUE, pretty = TRUE)
    cat(name, "submitted as cps extract", ex$number, "\n")
  } else if (identical(args[1], "download")) {
    r <- jsonlite::read_json(manifest)
    ex <- ipumsr::wait_for_extract(paste0("cps:", r$number), verbose = FALSE)
    files <- ipumsr::download_extract(ex, download_dir = folder, overwrite = TRUE)
    cat(name, "downloaded:", paste(basename(files), collapse = ", "), "\n")
  } else stop("Use submit or download.")
}
