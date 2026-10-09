# =============================================================================
# FCECPR Training Registration Form: build all reports from exported files
#
#   1. One Word file per response (for reviewing the training application)
#   2. Survey functioning report + a methods-and-code companion
#   3. Open-ended feedback report (verbatim, anonymized)
#
# HOW TO RUN (each time you have new responses)
#   a. In Qualtrics: Data & Analysis > Export & Import > Export Data > CSV.
#      Export twice: once with "Use numeric values", once with "Use choice text".
#   b. Put both CSVs in the data/ folder (old exports can stay; the newest of
#      each kind is used). The survey Word file lives in survey/.
#   c. Optional: list test/fake ResponseIds, one per line, in
#      data/exclude_response_ids.txt
#   d. Set the date range below (or leave it NULL for everything).
#   e. Open this project in RStudio and run:  source("run_reports.R")
#
# Output goes to output/<today's date>/. The Shiny app (app.R) produces the
# same reports straight from the Qualtrics API.
# =============================================================================

# Run from the project folder (the folder that holds this file).
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  p <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(p)) setwd(dirname(p))
}
project_dir <- normalizePath(".")

needed <- c("rmarkdown", "knitr", "dplyr", "tidyr", "purrr", "stringr", "readr",
            "lubridate", "ggplot2", "flextable", "officer", "xml2")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs)

source(file.path(project_dir, "R", "functions.R"))

# ----------------------------------------------------------------------------
# SETTINGS: edit here
# ----------------------------------------------------------------------------
settings <- list(
  data_dir    = file.path(project_dir, "data"),
  survey_dir  = file.path(project_dir, "survey"),
  numeric_csv = NULL,      # NULL = newest "numeric values" CSV in data/
  labels_csv  = NULL,      # NULL = newest "choice text" CSV in data/
  survey_docx = NULL,      # NULL = newest .docx in survey/ (or data/)

  # DATE RANGE: "YYYY-MM-DD", both ends included; NULL = no limit
  date_from  = NULL,       # e.g. "2026-09-01"
  date_to    = NULL,       # e.g. "2026-09-30"
  date_field = "recorded", # "recorded" = date submitted/recorded, or "start"

  # Responses to leave out
  exclude_status       = c("Survey Preview", "Survey Test", "Spam",
                           "Survey Preview Spam", "Offline Survey Preview"),
  exclude_response_ids = character(),        # e.g. c("R_abc123", "R_def456")
  exclude_ids_file     = file.path(project_dir, "data", "exclude_response_ids.txt"),

  # Order of sections in the applicant files (blocks not listed go at the end;
  # the feedback block is never included there)
  applicant_sections = c("Section 1 Training Overview", "Training Topics",
                         "Section 2 Training Design", "Section 3 Supporting Materials",
                         "Section 4 Attestation"),
  feedback_block_pattern = "feedback",       # block name(s) holding survey feedback

  # Survey functioning
  quit_progress_cutoff   = 80,    # Finished = FALSE and Progress below this = "Quit"
  duration_trim_method   = "fixed",  # "fixed" (use hours below) or "iqr" (Q3 + 3 x IQR)
  duration_trim_hours    = 4,     # completion times longer than this are left out of averages
  low_response_threshold = 0.80,  # flag questions answered by fewer than 80% of those shown them

  display_tz = "America/New_York"
)

# ----------------------------------------------------------------------------
# BUILD
# ----------------------------------------------------------------------------
run_date <- format(Sys.Date(), "%Y-%m-%d")
out_dir  <- file.path(project_dir, "output", run_date)

message("Reading data ...")
dat <- prepare_survey_data(settings)
message(sprintf("  %s: %d responses kept (%d outside the date range, %d excluded).",
                dat$date_filter$text, nrow(dat$responses),
                dat$date_filter$n_outside, nrow(dat$excluded)))

render_all_reports(dat, out_dir, project_dir)
message("Done. Reports are in: ", out_dir)
