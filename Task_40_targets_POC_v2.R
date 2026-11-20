###############################################################################
# TASK 40: EVALUATE targets FOR DEPENDENCY MANAGEMENT  (version 2)
# Category: Research & Innovation | Level: Advanced | Priority: Low
#
# Goal: small proof of concept (POC) of the R package {targets}.
# Workflow: Raw DM -> simplified SDTM DM -> simplified ADaM ADSL -> summary
#
# What this script shows:
#   1. Build the pipeline from a clean start.
#   2. Rerun with no change      -> everything is skipped.
#   3. Change ONE function       -> only the affected steps rebuild.
#   4. Change the input data     -> everything downstream rebuilds.
#   5. Full rebuild from scratch -> outputs are identical (reproducible).
#   6. Compare with a plain script (same results?).
#   7. Save evidence + evaluation notes (benefits, limits, cost, rubric).
#
# All data are synthetic. This is NOT a production clinical pipeline.
#
# HOW TO RUN: save as Task_41_targets_POC_v2.R, open in RStudio, run all.
# Everything is created inside the folder Task_41_targets_POC_v2.
###############################################################################

# SECTION 1: Packages 

required_packages <- c("data.table", "targets")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(data.table)
  library(targets)
})

# SECTION 2: Project folder and clean start 

original_wd <- getwd()
project_dir <- file.path(original_wd, "Task_41_targets_POC_v2")
dir.create(project_dir, showWarnings = FALSE)

for (folder in c("R", "data", "results", "logs", "reports")) {
  dir.create(file.path(project_dir, folder), recursive = TRUE, showWarnings = FALSE)
}
setwd(project_dir)

cat("\nTASK 40: targets DEPENDENCY MANAGEMENT POC (v2)\n")
cat("===============================================\n")
cat("Project folder  :", normalizePath(getwd()), "\n")
cat("R version       :", R.version.string, "\n")
cat("targets version :", as.character(packageVersion("targets")), "\n")
cat("data.table      :", as.character(packageVersion("data.table")), "\n")

# Delete any old targets store so the first run is a true first run.
tar_destroy(ask = FALSE)

# SECTION 3: Synthetic raw data 

set.seed(4101)
n_subjects <- 30L

raw_dm <- data.table(
  STUDYID = "STUDY-POC-001",
  USUBJID = sprintf("POC-%03d", seq_len(n_subjects)),
  SUBJID  = sprintf("%03d", seq_len(n_subjects)),
  AGE     = sample(18:80, n_subjects, replace = TRUE),
  SEX     = sample(c("M", "F"), n_subjects, replace = TRUE),
  ARM     = sample(c("Placebo", "Drug A", "Drug B"), n_subjects, replace = TRUE),
  COUNTRY = sample(c("IND", "USA", "GBR"), n_subjects, replace = TRUE)
)

raw_file_path <- file.path("data", "raw_dm.csv")
fwrite(raw_dm, raw_file_path)
cat("\nSynthetic raw data created:", raw_file_path, "-", nrow(raw_dm), "subjects\n")

# SECTION 4: Functions (saved to R/functions.R)

functions_code <- '
library(data.table)

# Simplified SDTM-like DM. Teaching example only.
create_sdtm_dm <- function(raw_data) {
  x <- copy(as.data.table(raw_data))

  required <- c("STUDYID", "USUBJID", "SUBJID", "AGE", "SEX", "ARM", "COUNTRY")
  missing_columns <- setdiff(required, names(x))
  if (length(missing_columns) > 0L) {
    stop("Missing raw columns: ", paste(missing_columns, collapse = ", "))
  }

  result <- x[, .(
    STUDYID = STUDYID,
    DOMAIN  = "DM",
    USUBJID = USUBJID,
    SUBJID  = SUBJID,
    AGE     = as.numeric(AGE),
    SEX     = SEX,
    ARM     = ARM,
    COUNTRY = COUNTRY
  )]

  if (anyDuplicated(result$USUBJID) > 0L) stop("Duplicate USUBJID values found.")
  result[]
}

# Simplified ADaM ADSL-like dataset. Teaching example only.
create_adam_adsl <- function(sdtm_dm) {
  x <- copy(as.data.table(sdtm_dm))

  # Stop early if SEX has a value we do not expect.
  if (any(!x$SEX %in% c("M", "F"))) stop("SEX must be M or F.")

  x[, .(
    STUDYID = STUDYID,
    USUBJID = USUBJID,
    AGE     = AGE,
    SEX     = fifelse(SEX == "M", "Male", "Female"),
    TRT01A  = ARM,
    COUNTRY = COUNTRY,
    SAFFL   = "Y"
  )]
}

# Subjects and age summary by treatment group.
create_subject_summary <- function(adsl) {
  x <- copy(as.data.table(adsl))

  x[, .(
    N_SUBJECTS = uniqueN(USUBJID),
    MEAN_AGE   = round(mean(AGE, na.rm = TRUE), 1),
    MIN_AGE    = min(AGE, na.rm = TRUE),
    MAX_AGE    = max(AGE, na.rm = TRUE)
  ), by = .(TRT01A)][order(TRT01A)]
}

# Write a short text report. Returns the (relative) file path.
write_summary_report <- function(summary_table, output_path) {
  lines <- c(
    "TASK 40 - targets PROOF OF CONCEPT",
    "=================================",
    "",
    "Subjects by treatment group:",
    capture.output(print(summary_table)),
    "",
    "All data are synthetic. This is not a production pipeline."
  )
  writeLines(lines, output_path)
  output_path
}
'
writeLines(functions_code, file.path("R", "functions.R"))

# SECTION 5: The targets pipeline (saved to _targets.R) 

targets_code <- '
library(targets)
library(data.table)

source("R/functions.R")
tar_option_set(packages = "data.table")

list(
  # File target: targets watches the raw CSV for changes.
  tar_target(raw_file, "data/raw_dm.csv", format = "file"),

  # Each step below uses the result of the step before it.
  tar_target(raw_dm, fread(raw_file)),
  tar_target(sdtm_dm, create_sdtm_dm(raw_dm)),
  tar_target(adam_adsl, create_adam_adsl(sdtm_dm)),
  tar_target(subject_summary, create_subject_summary(adam_adsl)),

  # Output files are file targets too.
  tar_target(
    adsl_csv,
    {
      path <- "results/adam_adsl.csv"
      fwrite(adam_adsl, path)
      path
    },
    format = "file"
  ),
  tar_target(
    summary_csv,
    {
      path <- "results/subject_summary.csv"
      fwrite(subject_summary, path)
      path
    },
    format = "file"
  ),
  tar_target(
    summary_report,
    write_summary_report(subject_summary, "reports/subject_summary.txt"),
    format = "file"
  )
)
'
writeLines(targets_code, "_targets.R")

# SECTION 6: Small helper functions 

# Run the pipeline and report which targets were rebuilt or skipped.
run_step <- function(label) {
  cat("\n", label, "\n", strrep("-", nchar(label)), "\n", sep = "")
  tar_make(reporter = "silent")

  progress <- as.data.table(tar_progress())
  rebuilt <- progress[progress == "completed", name]
  skipped <- progress[progress == "skipped", name]

  cat("Rebuilt (", length(rebuilt), "): ",
      if (length(rebuilt)) paste(rebuilt, collapse = ", ") else "none", "\n", sep = "")
  cat("Skipped (", length(skipped), "): ",
      if (length(skipped)) paste(skipped, collapse = ", ") else "none", "\n", sep = "")

  data.table(
    STEP = label,
    N_REBUILT = length(rebuilt),
    N_SKIPPED = length(skipped),
    REBUILT_TARGETS = paste(rebuilt, collapse = ", ")
  )
}

# Fingerprint (md5) of the output files. Same file content = same fingerprint.
output_files <- c("results/adam_adsl.csv",
                  "results/subject_summary.csv",
                  "reports/subject_summary.txt")
hash_outputs <- function() unname(tools::md5sum(output_files))

# Record a PASS/FAIL check so it can be printed and saved.
checks <- data.table(CHECK = character(), RESULT = character())
check <- function(ok, message) {
  checks <<- rbind(checks, data.table(CHECK = message,
                                      RESULT = if (isTRUE(ok)) "PASS" else "FAIL"))
  cat(if (isTRUE(ok)) "[PASS] " else "[FAIL] ", message, "\n", sep = "")
}

run_log <- data.table()

# SECTION 7: Step 1 - first run (clean start) 

run_log <- rbind(run_log, run_step("STEP 1: First run (clean start)"))
check(run_log[1, N_REBUILT] == 8L, "Step 1: all 8 targets were built")

# SECTION 8: Step 2 - rerun with no change 

hash_step1 <- hash_outputs()
run_log <- rbind(run_log, run_step("STEP 2: Rerun, nothing changed"))
check(run_log[2, N_REBUILT] == 0L, "Step 2: nothing rebuilt, all 8 skipped")
check(identical(hash_step1, hash_outputs()), "Step 2: output files unchanged")

# SECTION 9: Step 3 - change ONE function 
# Add MEDIAN_AGE to the summary function. Only the summary steps should rebuild.

old_text <- readLines("R/functions.R")
new_text <- sub("MAX_AGE    = max(AGE, na.rm = TRUE)",
                "MAX_AGE    = max(AGE, na.rm = TRUE),\n    MEDIAN_AGE = median(AGE, na.rm = TRUE)",
                old_text, fixed = TRUE)
stopifnot(!identical(old_text, new_text))
writeLines(new_text, "R/functions.R")

run_log <- rbind(run_log, run_step("STEP 3: Changed one function (added MEDIAN_AGE)"))
rebuilt_3 <- strsplit(run_log[3, REBUILT_TARGETS], ", ", fixed = TRUE)[[1]]
check(setequal(rebuilt_3, c("subject_summary", "summary_csv", "summary_report")),
      "Step 3: only the summary targets rebuilt")
check(!"adam_adsl" %in% rebuilt_3, "Step 3: adam_adsl was skipped")

# SECTION 10: Step 4 - change the input data 

new_subject <- data.table(
  STUDYID = "STUDY-POC-001", USUBJID = "POC-031", SUBJID = "031",
  AGE = 45, SEX = "F", ARM = "Drug A", COUNTRY = "IND"
)
fwrite(rbindlist(list(fread(raw_file_path), new_subject), use.names = TRUE),
       raw_file_path)
cat("\nAdded 1 subject to the raw input (now 31 subjects).\n")
cat("Targets reported as outdated: ",
    paste(sort(tar_outdated()), collapse = ", "), "\n", sep = "")

run_log <- rbind(run_log, run_step("STEP 4: Changed the raw input data"))
check(run_log[4, N_REBUILT] == 8L, "Step 4: all targets downstream of the data rebuilt")
check(uniqueN(fread("results/adam_adsl.csv")$USUBJID) == 31L,
      "Step 4: ADSL now has 31 subjects")

# SECTION 11: Step 5 - reproducibility check 
# Delete the store, rebuild everything, and compare output fingerprints.

hash_step4 <- hash_outputs()
tar_destroy(ask = FALSE)
run_log <- rbind(run_log, run_step("STEP 5: Full rebuild from scratch"))
check(identical(hash_step4, hash_outputs()),
      "Step 5: rebuilt outputs are identical (same md5)")

# SECTION 12: Step 6 - compare with a plain script 
# Run the same functions in order, without targets, and compare the summary.

plain_env <- new.env()
sys.source("R/functions.R", envir = plain_env)

plain_raw     <- fread(raw_file_path)
plain_sdtm    <- plain_env$create_sdtm_dm(plain_raw)
plain_adsl    <- plain_env$create_adam_adsl(plain_sdtm)
plain_summary <- plain_env$create_subject_summary(plain_adsl)

plain_csv <- tempfile(fileext = ".csv")
fwrite(plain_summary, plain_csv)
check(isTRUE(all.equal(fread(plain_csv), fread("results/subject_summary.csv"))),
      "Step 6: plain script and targets give the same summary")

# SECTION 13: Evidence files 

# Dependency graph as Mermaid text (open it in https://mermaid.live).
writeLines(tar_mermaid(), file.path("reports", "pipeline_graph.mmd"))

# Session details for reproducibility.
writeLines(capture.output(sessionInfo()), file.path("logs", "session_info.txt"))

# Run log and checks.
fwrite(run_log, file.path("reports", "rerun_comparison.csv"))
fwrite(checks, file.path("reports", "checks.csv"))

# Team-fit rubric: the team fills the ratings after trying the POC.
rubric <- data.table(
  CRITERION = c("Time to understand _targets.R",
                "Ease of adding a new target",
                "Reading error messages",
                "Debugging a failed target",
                "Fit with our folder and QC process",
                "Training needed"),
  WHAT_TO_CHECK = c("Can a beginner explain what each target does?",
                    "Can they add one step without help?",
                    "Is it clear which target failed and why?",
                    "Can they rerun just the failed part?",
                    "Does it work with our logs, specs and reviews?",
                    "Hours needed before a first real use"),
  RATING_1_TO_5 = NA_integer_,
  NOTES = ""
)
fwrite(rubric, file.path("reports", "team_fit_rubric.csv"))

# SECTION 14: Evaluation notes (numbers come from the runs above) 

evaluation <- c(
  "TASK 40 - EVALUATION NOTES (targets)",
  "====================================",
  "",
  "Tool evaluated: targets (compared with a plain R script).",
  "",
  "WHAT WE MEASURED IN THIS POC",
  sprintf("- First run built %d targets.", run_log[1, N_REBUILT]),
  sprintf("- Rerun with no change: %d rebuilt, %d skipped.",
          run_log[2, N_REBUILT], run_log[2, N_SKIPPED]),
  sprintf("- After changing ONE function: %d rebuilt (%s), %d skipped.",
          run_log[3, N_REBUILT], run_log[3, REBUILT_TARGETS], run_log[3, N_SKIPPED]),
  sprintf("- After changing the input data: %d of 8 targets rebuilt.",
          run_log[4, N_REBUILT]),
  "- Full rebuild from scratch: output files had the same md5 as before.",
  "- A plain script gave the same summary table as targets.",
  sprintf("- Checks passed: %d of %d (see reports/checks.csv).",
          sum(checks$RESULT == "PASS"), nrow(checks)),
  "",
  "REPRODUCIBILITY: plain script vs targets",
  "- Plain script: runs every step each time. Skipping steps is manual.",
  "- targets: tracks code and data, rebuilds only outdated steps.",
  "- Both gave the same results here. targets adds the proof of what is up to date.",
  "",
  "TEAM FIT",
  sprintf("- _targets.R is %d lines and R/functions.R is %d lines for this tiny POC.",
          length(readLines("_targets.R")), length(readLines("R/functions.R"))),
  "- Ratings are NOT filled yet. A beginner programmer must try it and fill",
  "  reports/team_fit_rubric.csv before we decide.",
  "",
  "BENEFITS",
  "- Dependencies between steps are explicit and can be drawn as a graph.",
  "- Up-to-date steps are skipped, so reruns are faster and safer.",
  "- Easy to show which outputs are current.",
  "",
  "LIMITATIONS",
  "- Programmers must learn the targets way of working (_targets.R, file targets).",
  "- Input and output files must be declared carefully.",
  "- This POC is tiny. Speed gains on a real study are not measured here.",
  "- Production use would need validation, logging, access control and support.",
  "- This POC does not prove regulatory compliance.",
  "",
  "ADOPTION COST",
  "- The package is free, but team time is needed for design, training,",
  "  converting existing scripts, code review and validation.",
  "",
  "SIMILAR TOOLS (for context)",
  "- drake: earlier package by the same author, now replaced by targets.",
  "- make / Makefile: works with any language, but is not R-aware.",
  "- renv: locks package versions. It complements targets and does not replace it.",
  "- Snakemake / Nextflow: larger workflow tools, usually for bigger pipelines.",
  "",
  "RECOMMENDATION",
  "- targets looks promising for a controlled pilot on ONE real, non-production",
  "  workflow. Decide on wider use after the team-fit rubric is filled.",
  "",
  "EVIDENCE FILES",
  "- _targets.R, R/functions.R",
  "- reports/rerun_comparison.csv, reports/checks.csv",
  "- reports/pipeline_graph.mmd, reports/team_fit_rubric.csv",
  "- logs/session_info.txt",
  "- results/adam_adsl.csv, results/subject_summary.csv, reports/subject_summary.txt",
  "",
  "All data are synthetic training data."
)
writeLines(evaluation, file.path("reports", "task_40_evaluation.txt"))

# SECTION 15: Final summary 

cat("\n\nRUN SUMMARY\n===========\n")
print(run_log[, .(STEP, N_REBUILT, N_SKIPPED)])

cat("\nCHECKS\n======\n")
print(checks)

cat("\nFINAL TREATMENT SUMMARY\n=======================\n")
print(fread("results/subject_summary.csv"))

cat("\nGENERATED FILES\n===============\n")
print(setdiff(list.files(".", recursive = TRUE), list.files("_targets", recursive = TRUE, full.names = TRUE)))

cat("\nTASK 40 POC (v2) COMPLETED. Read reports/task_40_evaluation.txt\n")

setwd(original_wd)

