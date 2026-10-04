# Retrieval utility for the linkage check (feasibility criterion A, Supplementary Tables S3a-b).
# Usage: Rscript scripts/linkage_check_extracts.R submit|download
# Requests no follow-up outcomes.
# Reads the IPUMS API key from the IPUMS_API_KEY environment variable, which ipumsr uses directly.
if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) stop("Set the IPUMS_API_KEY environment variable to your IPUMS API key.")
root <- "data/ipums-cps/linkage-check"
dir.create(root, recursive = TRUE, showWarnings = FALSE)
definitions <- list(
  asec_baseline = list(samples = paste0("cps", 2016:2024, "_03s"),
    variables = c("AGE", "SEX", "RACE", "MISH", "CPSIDV", "ASECOVERP", "DISABWRK", "DIFFANY", "EMPSTAT", "LNKFW1YWT")),
  asec_followup = list(samples = paste0("cps", 2017:2025, "_03s"),
    variables = c("AGE", "SEX", "RACE", "MISH", "CPSIDV", "ASECOVERP"))
)
for (name in names(definitions)) {
  definition <- definitions[[name]]
  folder <- file.path(root, name)
  dir.create(folder, showWarnings = FALSE)
  manifest <- file.path(folder, "request.json")
  mode <- commandArgs(trailingOnly = TRUE)[1]
  if (mode == "submit") {
    if (file.exists(manifest)) {
      cat(name, ": request already recorded; not resubmitted.\n", sep = "")
      next
    }
    extract <- ipumsr::define_extract_micro("cps", paste("Outcome-blind linkage check", name),
      samples = definition$samples, variables = definition$variables)
    submitted <- ipumsr::submit_extract(extract)
    number <- submitted$number
    jsonlite::write_json(c(definition, list(number = number)), manifest, auto_unbox = TRUE, pretty = TRUE)
    cat(name, ": submitted extract ", number, ".\n", sep = "")
  } else if (mode == "download") {
    if (!file.exists(manifest)) stop("Submit requests first.")
    number <- jsonlite::read_json(manifest, simplifyVector = TRUE)$number
    request <- ipumsr::get_extract_info(paste0("cps:", number))
    if (request$status != "completed") {
      cat(name, ": ", request$status, ".\n", sep = "")
      next
    }
    invisible(ipumsr::download_extract(request, download_dir = folder, progress = FALSE))
    cat(name, ": downloaded completed extract ", number, ".\n", sep = "")
  } else stop("Use submit or download.")
}
