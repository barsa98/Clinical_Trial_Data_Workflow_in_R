# ============================================================
# Project: Clinical Trial Data Workflow in R
# Script: 07_create_adam.R
# Purpose: Create provisional ADaM analysis datasets from SDTM
# Standard reference: ADaMIG conventions (confirm version for project)
#
# Input : data/sdtm/*.csv
# Output: data/adam/*.csv
# Reports: reports/adam/*.csv
#
# Datasets created:
#   ADSL - Subject-Level Analysis Dataset
#   ADAE - Adverse Event Analysis Dataset
#   ADCM - Concomitant Medication Analysis Dataset
#   ADLB - Laboratory Analysis Dataset
#   ADVS - Vital Signs Analysis Dataset
#
# Important limitations:
#   - TRT01P represents planned randomized treatment only.
#   - Actual treatment received/exposure is not established by the
#     current SDTM datasets; TRT01A, TRT01AN and SAFFL are not derived.
#   - ITTFL is not assigned until the protocol's analysis-population
#     definition is confirmed.
#   - BASE/CHG/PCHG are not derived until the baseline visit/record
#     is confirmed for LB and VS.
#   - Dates are exported as ISO dates (YYYY-MM-DD) in CSV files.
# ============================================================

# 1. SETUP ----------------------------------------------------

project_dir <- "C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R"
setwd(project_dir)

required_packages <- c("dplyr", "readr", "stringr", "purrr")
new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# 2. DIRECTORIES ----------------------------------------------

sdtm_dir <- file.path("data", "sdtm")
adam_dir <- file.path("data", "adam")
report_dir <- file.path("reports", "adam")

dir.create(adam_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(sdtm_dir)) {
  stop("SDTM directory not found: data/sdtm. Run Script 06 first.")
}

# 3. IMPORT SDTM DATASETS -------------------------------------

read_sdtm <- function(domain) {
  file_path <- file.path(sdtm_dir, paste0(domain, ".csv"))
  if (!file.exists(file_path)) {
    stop("Required SDTM file not found: ", file_path)
  }
  readr::read_csv(
    file_path,
    show_col_types = FALSE,
    na = c("", "NA")
  )
}

DM <- read_sdtm("DM")
AE <- read_sdtm("AE")
CM <- read_sdtm("CM")
LB <- read_sdtm("LB")
VS <- read_sdtm("VS")
DS <- read_sdtm("DS")

# 4. GENERAL HELPERS ------------------------------------------

required_vars <- function(data, vars, dataset_name) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      dataset_name, " is missing required variable(s): ",
      paste(missing_vars, collapse = ", ")
    )
  }
}

parse_iso_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  # SDTM date/time variables in this project are expected as YYYY-MM-DD.
  as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
}

numeric_value <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

check_unique_key <- function(data, key, dataset_name) {
  if (any(is.na(data[[key]]) | trimws(as.character(data[[key]])) == "")) {
    stop(dataset_name, " contains missing key values in ", key, ".")
  }
  if (anyDuplicated(data[[key]]) > 0) {
    stop(dataset_name, " contains duplicate key values in ", key, ".")
  }
}

# 5. ADSL - SUBJECT-LEVEL ANALYSIS DATASET ---------------------

create_adsl <- function(dm, ds) {
  required_vars(
    dm,
    c("STUDYID", "USUBJID", "SUBJID", "AGE", "AGEU", "SEX", "ARMCD", "ARM"),
    "SDTM DM"
  )
  required_vars(
    ds,
    c("USUBJID", "DSTERM", "DSDECOD", "DSSTDTC"),
    "SDTM DS"
  )

  adsl <- dm %>%
    transmute(
      STUDYID,
      USUBJID,
      SUBJID,
      AGE = numeric_value(AGE),
      AGEU,
      SEX,
      ARMCD,
      ARM,
      TRT01P = ARM
    ) %>%
    left_join(
      ds %>%
        mutate(DSDT = parse_iso_date(DSSTDTC)) %>%
        arrange(USUBJID, DSDT) %>%
        group_by(USUBJID) %>%
        summarise(
          DCSREAS = dplyr::first(na.omit(as.character(DSTERM)), default = NA_character_),
          DCSDECOD = dplyr::first(na.omit(as.character(DSDECOD)), default = NA_character_),
          DSDT = dplyr::first(na.omit(DSDT), default = as.Date(NA)),
          .groups = "drop"
        ),
      by = "USUBJID"
    ) %>%
    mutate(
      DCSFL = ifelse(!is.na(DCSREAS), "Y", "N")
    ) %>%
    arrange(SUBJID)

  check_unique_key(adsl, "USUBJID", "ADSL")
  if (nrow(adsl) != nrow(dm)) stop("ADSL row count does not match SDTM DM.")
  adsl
}

# 6. ADAE - ADVERSE EVENT ANALYSIS DATASET --------------------

create_adae <- function(ae, adsl) {
  required_vars(
    ae,
    c("STUDYID", "USUBJID", "AESEQ", "AETERM", "AESTDTC", "AEENDTC",
      "AESEV", "AESER", "AEREL", "AEACN", "AEOUT"),
    "SDTM AE"
  )

  adae <- ae %>%
    left_join(
      adsl %>% select(USUBJID, SUBJID, AGE, AGEU, SEX, ARMCD, ARM, TRT01P),
      by = "USUBJID"
    ) %>%
    mutate(
      ASEQ = as.integer(AESEQ),
      ASTDT = parse_iso_date(AESTDTC),
      AENDT = parse_iso_date(AEENDTC)
    ) %>%
    transmute(
      STUDYID, USUBJID, SUBJID,
      ASEQ, AETERM, ASTDT, AENDT, AESEV, AESER, AEREL, AEACN, AEOUT,
      AGE, AGEU, SEX, ARMCD, ARM, TRT01P
    ) %>%
    arrange(USUBJID, ASEQ)

  if (any(is.na(adae$ARMCD))) stop("ADAE contains subjects not matched to ADSL.")
  adae
}

# 7. ADCM - CONCOMITANT MEDICATION ANALYSIS DATASET -----------

create_adcm <- function(cm, adsl) {
  required_vars(
    cm,
    c("STUDYID", "USUBJID", "CMSEQ", "CMTRT", "CMINDC", "CMDOSTXT", "CMSTDTC", "CMENDTC"),
    "SDTM CM"
  )

  adcm <- cm %>%
    left_join(
      adsl %>% select(USUBJID, SUBJID, AGE, AGEU, SEX, ARMCD, ARM, TRT01P),
      by = "USUBJID"
    ) %>%
    mutate(
      ASEQ = as.integer(CMSEQ),
      ASTDT = parse_iso_date(CMSTDTC),
      AENDT = parse_iso_date(CMENDTC)
    ) %>%
    transmute(
      STUDYID, USUBJID, SUBJID, ASEQ, CMTRT, CMINDC, CMDOSTXT,
      ASTDT, AENDT, AGE, AGEU, SEX, ARMCD, ARM, TRT01P
    ) %>%
    arrange(USUBJID, ASEQ)

  if (any(is.na(adcm$ARMCD))) stop("ADCM contains subjects not matched to ADSL.")
  adcm
}

# 8. ADLB - LABORATORY ANALYSIS DATASET -----------------------

create_adlb <- function(lb, adsl) {
  required_vars(
    lb,
    c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST", "LBORRES", "LBDTC", "VISIT"),
    "SDTM LB"
  )

  adlb <- lb %>%
    left_join(
      adsl %>% select(USUBJID, SUBJID, AGE, AGEU, SEX, ARMCD, ARM, TRT01P),
      by = "USUBJID"
    ) %>%
    mutate(
      ASEQ = as.integer(LBSEQ),
      PARAMCD = as.character(LBTESTCD),
      PARAM = as.character(LBTEST),
      ADT = parse_iso_date(LBDTC),
      AVISIT = as.character(VISIT),
      AVISITN = NA_real_,
      AVAL = numeric_value(LBORRES),
      DTYPE = NA_character_,
      BASE = NA_real_,
      CHG = NA_real_,
      PCHG = NA_real_
    ) %>%
    transmute(
      STUDYID, USUBJID, SUBJID, ASEQ, PARAMCD, PARAM, ADT, AVISIT, AVISITN,
      AVAL, DTYPE, BASE, CHG, PCHG,
      LBORRES, AGE, AGEU, SEX, ARMCD, ARM, TRT01P
    ) %>%
    arrange(USUBJID, PARAMCD, ADT, ASEQ)

  if (any(is.na(adlb$ARMCD))) stop("ADLB contains subjects not matched to ADSL.")
  adlb
}

# 9. ADVS - VITAL SIGNS ANALYSIS DATASET ----------------------

create_advs <- function(vs, adsl) {
  required_vars(
    vs,
    c("STUDYID", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST", "VSORRES", "VSORRESU", "VSSTRESN", "VSSTRESU"),
    "SDTM VS"
  )

  advs <- vs %>%
    left_join(
      adsl %>% select(USUBJID, SUBJID, AGE, AGEU, SEX, ARMCD, ARM, TRT01P),
      by = "USUBJID"
    ) %>%
    mutate(
      ASEQ = as.integer(VSSEQ),
      PARAMCD = as.character(VSTESTCD),
      PARAM = as.character(VSTEST),
      AVAL = numeric_value(VSSTRESN),
      AVALU = as.character(VSSTRESU),
      ADT = as.Date(NA),
      AVISIT = NA_character_,
      AVISITN = NA_real_,
      BASE = NA_real_,
      CHG = NA_real_,
      PCHG = NA_real_
    ) %>%
    transmute(
      STUDYID, USUBJID, SUBJID, ASEQ, PARAMCD, PARAM, ADT, AVISIT, AVISITN,
      AVAL, AVALU, BASE, CHG, PCHG, VSORRES, VSORRESU,
      AGE, AGEU, SEX, ARMCD, ARM, TRT01P
    ) %>%
    arrange(USUBJID, PARAMCD, ASEQ)

  if (any(is.na(advs$ARMCD))) stop("ADVS contains subjects not matched to ADSL.")
  advs
}

# 10. CREATE DATASETS -----------------------------------------

cat("Creating ADSL...\n")
ADSL <- create_adsl(DM, DS)

cat("Creating ADAE...\n")
ADAE <- create_adae(AE, ADSL)

cat("Creating ADCM...\n")
ADCM <- create_adcm(CM, ADSL)

cat("Creating ADLB...\n")
ADLB <- create_adlb(LB, ADSL)

cat("Creating ADVS...\n")
ADVS <- create_advs(VS, ADSL)

adam_datasets <- list(
  ADSL = ADSL,
  ADAE = ADAE,
  ADCM = ADCM,
  ADLB = ADLB,
  ADVS = ADVS
)

# 11. EXPORT ADAM DATASETS ------------------------------------

purrr::iwalk(
  adam_datasets,
  function(dataset, dataset_name) {
    readr::write_csv(
      dataset,
      file.path(adam_dir, paste0(dataset_name, ".csv")),
      na = ""
    )
    cat(dataset_name, "exported:", nrow(dataset), "rows x", ncol(dataset), "columns\n")
  }
)

# 12. DATASET OVERVIEW AND BASIC QC ---------------------------

cat("\nGenerating ADaM dataset overview and QC reports...\n")

# Check that all datasets are data frames
for (dataset_name in names(adam_datasets)) {

  dataset <- adam_datasets[[dataset_name]]

  if (!is.data.frame(dataset)) {
    stop(
      paste(dataset_name, "is not a data frame.")
    )
  }

  if (!"USUBJID" %in% names(dataset)) {
    stop(
      paste(dataset_name, "is missing USUBJID.")
    )
  }
}


# 12.1 DATASET OVERVIEW ---------------------------------------

overview_list <- lapply(
  names(adam_datasets),

  function(dataset_name) {

    dataset <- adam_datasets[[dataset_name]]

    data.frame(
      dataset = dataset_name,
      rows = nrow(dataset),
      columns = ncol(dataset),
      unique_subjects = dplyr::n_distinct(dataset$USUBJID),
      missing_usubjid = sum(
        is.na(dataset$USUBJID) |
          trimws(dataset$USUBJID) == ""
      )
    )
  }
)

overview <- dplyr::bind_rows(overview_list)


# Export overview
readr::write_csv(
  overview,
  file.path(report_dir, "adam_dataset_overview.csv")
)


# 12.2 BASIC QC SUMMARY ---------------------------------------

qc_list <- lapply(
  names(adam_datasets),

  function(dataset_name) {

    dataset <- adam_datasets[[dataset_name]]

    unmatched_treatment <- if ("ARMCD" %in% names(dataset)) {
      sum(is.na(dataset$ARMCD))
    } else {
      NA_integer_
    }

    data.frame(
      dataset = dataset_name,
      rows = nrow(dataset),
      missing_usubjid = sum(
        is.na(dataset$USUBJID) |
          trimws(dataset$USUBJID) == ""
      ),
      unmatched_treatment = unmatched_treatment
    )
  }
)

qc_summary <- dplyr::bind_rows(qc_list)


# Export QC summary
readr::write_csv(
  qc_summary,
  file.path(report_dir, "adam_basic_qc.csv")
)


# 12.3 DISPLAY RESULTS ----------------------------------------

cat("\nADaM Dataset Overview:\n")
print(overview)

cat("\nADaM Basic QC Summary:\n")
print(qc_summary)

cat("\nQC reports saved in:", report_dir, "\n")