
# ============================================================
# Project: Clinical Trial Data Workflow in R
# Script: 06_create_sdtm.R
#
# Purpose:
# Convert cleaned EDC datasets into SDTM domains.
#
# SDTM Standard:
# SDTMIG v3.4 Final (2022-07-21)
#
# Domains:
# DM - Demographics (includes planned treatment assignment from kit list)
# AE - Adverse Events
# CM - Concomitant Medications
# LB - Laboratory Tests
# VS - Vital Signs (provisional)
# DS - Disposition
#
# ============================================================


# 1. SETUP ----------------------------------------------------

# Set project directory
project_dir <- "C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R"

setwd(project_dir)

# Install required packages if not already installed
required_packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr"
)

new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load packages
invisible(
  lapply(required_packages, library, character.only = TRUE)
)


# 2. PROJECT DIRECTORIES --------------------------------------

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
sdtm_dir <- file.path("data", "sdtm")
report_dir <- file.path("reports", "sdtm")

dir.create(sdtm_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found. Run Script 03 first.")
}


# 3. IMPORT PROCESSED DATA ------------------------------------

processed_files <- list.files(
  processed_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(processed_files) == 0) {
  stop("No processed CSV datasets found.")
}

processed_data <- lapply(
  processed_files,
  function(file) {
    read.csv(
      file,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA")
    )
  }
)

names(processed_data) <- tools::file_path_sans_ext(
  basename(processed_files)
)

cat("Datasets imported:", length(processed_data), "\n")


# 4. GENERAL CONFIGURATION ------------------------------------

# Provisional study identifier.
# Confirm this against the study protocol before finalization.

STUDYID_VALUE <- "DTM201"

# Subject identifier convention:
# Example: DTM201-SUB001

create_usubjid <- function(subject_id) {

  subject_id <- as.character(subject_id)

  ifelse(
    is.na(subject_id) | subject_id == "",
    NA_character_,
    paste(STUDYID_VALUE, subject_id, sep = "-")
  )
}


# Convert source dates from DD-MM-YYYY to ISO 8601

convert_date <- function(x) {

  x <- trimws(as.character(x))

  parsed_date <- as.Date(
    x,
    format = "%d-%m-%Y"
  )

  ifelse(
    is.na(parsed_date),
    NA_character_,
    format(parsed_date, "%Y-%m-%d")
  )
}


# Convert Yes/No values to Y/N

convert_yes_no <- function(x) {

  x <- tolower(trimws(as.character(x)))

  dplyr::case_when(
    x == "yes" ~ "Y",
    x == "no" ~ "N",
    TRUE ~ NA_character_
  )
}


# Return a dataset safely

get_dataset <- function(dataset_name) {

  if (!dataset_name %in% names(processed_data)) {
    warning(
      paste("Dataset not found:", dataset_name)
    )

    return(NULL)
  }

  processed_data[[dataset_name]]
}


# Return a source column safely

get_column <- function(data, column_name) {

  if (is.null(data) || !column_name %in% names(data)) {
    return(rep(NA, if (is.null(data)) 0 else nrow(data)))
  }

  data[[column_name]]
}


# Create an empty SDTM dataset

empty_sdtm <- function(columns) {

  as.data.frame(
    setNames(
      replicate(
        length(columns),
        character(0),
        simplify = FALSE
      ),
      columns
    ),
    stringsAsFactors = FALSE
  )
}


# 5. SUBJECT IDENTIFIER LOOKUP -------------------------------

baseline <- get_dataset("Baseline_and_Demographics")

if (is.null(baseline)) {
  stop("Baseline_and_Demographics is required to construct subject identifiers.")
}

if (!all(c("record_id", "subject_id") %in% names(baseline))) {
  stop("Baseline dataset must contain record_id and subject_id.")
}

subject_lookup <- baseline %>%
  select(record_id, subject_id) %>%
  distinct() %>%
  mutate(
    STUDYID = STUDYID_VALUE,
    USUBJID = create_usubjid(subject_id)
  )

# This lookup is used to link source forms that do not
# contain a subject_id variable.

get_subject_id <- function(data) {

  if ("subject_id" %in% names(data)) {
    return(as.character(data$subject_id))
  }

  if ("sub_id_random" %in% names(data)) {
    return(as.character(data$sub_id_random))
  }

  data <- data %>%
    left_join(
      subject_lookup %>%
        select(record_id, subject_id),
      by = "record_id"
    )

  as.character(data$subject_id)
}

# Master randomization list is maintained separately from the EDC exports.
# Expected columns: study_kit_number, treatment (Drug / Placebo).
master_randomization_file <- file.path(
  raw_dir,
  "Master_Randomization_List.csv"
)

if (!file.exists(master_randomization_file)) {
  stop(
    "Master randomization list not found: ",
    master_randomization_file,
    "\nSave the supplied kit-to-treatment table as ",
    "data/raw/Master_Randomization_List.csv."
  )
}

Master_Randomization_List <- read.csv(
  master_randomization_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

create_treatment_mapping <- function(
    randomization_data,
    master_randomization_data
) {

  required_randomization_columns <- c(
    "sub_id_random",
    "study_kit_number",
    "randomization_number",
    "randomization_date",
    "drug_dispensed"
  )

  required_master_columns <- c("study_kit_number", "treatment")

  missing_randomization_columns <- setdiff(
    required_randomization_columns,
    names(randomization_data)
  )

  missing_master_columns <- setdiff(
    required_master_columns,
    names(master_randomization_data)
  )

  if (length(missing_randomization_columns) > 0) {
    stop(
      "Randomization dataset is missing: ",
      paste(missing_randomization_columns, collapse = ", ")
    )
  }

  if (length(missing_master_columns) > 0) {
    stop(
      "Master randomization list is missing: ",
      paste(missing_master_columns, collapse = ", ")
    )
  }

  # Standardize key fields before joining.
  randomization_data <- randomization_data %>%
    mutate(
      sub_id_random = trimws(as.character(sub_id_random)),
      study_kit_number = trimws(as.character(study_kit_number))
    )

  master_randomization_data <- master_randomization_data %>%
    transmute(
      study_kit_number = trimws(as.character(study_kit_number)),
      # Normalize treatment labels using base R; no stringr/stringi needed.
      treatment_raw = trimws(as.character(treatment)),
      treatment = ifelse(
        tolower(treatment_raw) == "drug", "Drug",
        ifelse(tolower(treatment_raw) == "placebo", "Placebo", NA_character_)
      )
    ) %>%
    select(study_kit_number, treatment)

  if (any(is.na(master_randomization_data$study_kit_number)) ||
      any(master_randomization_data$study_kit_number == "")) {
    stop("Master randomization list contains missing kit numbers.")
  }

  if (anyDuplicated(master_randomization_data$study_kit_number)) {
    stop("Duplicate kit numbers found in master randomization list.")
  }

  if (any(is.na(master_randomization_data$treatment)) ||
      any(!master_randomization_data$treatment %in% c("Drug", "Placebo"))) {
    invalid_treatment_rows <- which(
      is.na(master_randomization_data$treatment) |
        !master_randomization_data$treatment %in% c("Drug", "Placebo")
    )
    stop(
      "Treatment values must be 'Drug' or 'Placebo'. Invalid/missing value(s) at master-list row(s): ",
      paste(invalid_treatment_rows, collapse = ", ")
    )
  }

  if (anyDuplicated(randomization_data$sub_id_random)) {
    stop("Duplicate subject IDs found in the Randomization dataset.")
  }

  if (anyDuplicated(randomization_data$study_kit_number)) {
    stop("A study kit is assigned to more than one subject in Randomization.")
  }

  if (nrow(master_randomization_data) != 120) {
    stop(
      "Expected 120 kit records in the master randomization list; found ",
      nrow(master_randomization_data), "."
    )
  }

  allocation_counts <- master_randomization_data %>%
    count(treatment, name = "Subjects")

  drug_count <- sum(
    master_randomization_data$treatment == "Drug",
    na.rm = TRUE
  )
  placebo_count <- sum(
    master_randomization_data$treatment == "Placebo",
    na.rm = TRUE
  )

  if (drug_count != 60 || placebo_count != 60) {
    stop(
      "Expected 1:1 allocation (60 Drug / 60 Placebo). Found ",
      drug_count, " Drug and ", placebo_count, " Placebo."
    )
  }

  treatment_mapping <- randomization_data %>%
    left_join(master_randomization_data, by = "study_kit_number")

  if (any(is.na(treatment_mapping$treatment))) {
    unmatched <- treatment_mapping %>%
      filter(is.na(treatment)) %>%
      pull(study_kit_number)

    stop(
      "No treatment assignment found for kit(s): ",
      paste(unmatched, collapse = ", ")
    )
  }

  # Confirm that the randomization records and master list cover the same kits.
  extra_master_kits <- setdiff(
    master_randomization_data$study_kit_number,
    randomization_data$study_kit_number
  )

  if (length(extra_master_kits) > 0) {
    stop(
      "Master list contains kit(s) absent from Randomization: ",
      paste(extra_master_kits, collapse = ", ")
    )
  }

  if (nrow(treatment_mapping) != 120 ||
      n_distinct(treatment_mapping$sub_id_random) != 120) {
    stop("Treatment mapping did not produce exactly 120 unique subjects.")
  }

  treatment_mapping %>%
    mutate(
      ARMCD = case_when(
        treatment == "Drug" ~ "DRUG",
        treatment == "Placebo" ~ "PBO",
        TRUE ~ NA_character_
      ),
      ARM = treatment
    )
}

# Build and validate the subject-to-planned-treatment mapping once.
randomization <- get_dataset("Randomization")

if (is.null(randomization)) {
  stop("Randomization dataset is required to assign planned treatment arms.")
}

treatment_mapping <- create_treatment_mapping(
  randomization,
  Master_Randomization_List
)

treatment_allocation_qc <- treatment_mapping %>%
  count(ARM, ARMCD, name = "Subjects")

write.csv(
  treatment_allocation_qc,
  file.path(report_dir, "sdtm_treatment_allocation_qc.csv"),
  row.names = FALSE
)

cat("\nValidated planned treatment allocation:\n")
print(treatment_allocation_qc, row.names = FALSE)


# 6. DM - DEMOGRAPHICS ----------------------------------------

create_dm <- function() {

  data <- get_dataset("Baseline_and_Demographics")

  if (is.null(data)) {
    return(NULL)
  }

  required_columns <- c("subject_id", "dob", "age", "gender")
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "DM source is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  dm <- data %>%
    mutate(
      STUDYID = STUDYID_VALUE,
      DOMAIN = "DM",
      USUBJID = create_usubjid(subject_id),
      SUBJID = as.character(subject_id),
      BRTHDTC = convert_date(dob),
      AGE = suppressWarnings(as.numeric(age)),
      AGEU = ifelse(!is.na(age), "YEARS", NA_character_),
      SEX = case_when(
        tolower(trimws(as.character(gender))) == "male" ~ "M",
        tolower(trimws(as.character(gender))) == "female" ~ "F",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(
      treatment_mapping %>%
        select(
          sub_id_random,
          treatment,
          ARMCD,
          ARM
        ),
      by = c("SUBJID" = "sub_id_random")
    ) %>%
    # ARM / ARMCD represent the planned randomized treatment assignment.
    # ACTARM / ACTARMCD are not populated because dispensing alone does not
    # establish actual treatment exposure or treatment taken.
    transmute(
      STUDYID,
      DOMAIN,
      USUBJID,
      SUBJID,
      BRTHDTC,
      AGE,
      AGEU,
      SEX,
      ARMCD,
      ARM
    ) %>%
    distinct()

  if (any(is.na(dm$ARM) | is.na(dm$ARMCD))) {
    stop("One or more DM subjects have no planned treatment assignment.")
  }

  if (nrow(dm) != 120 || n_distinct(dm$USUBJID) != 120) {
    stop("DM must contain exactly 120 unique subjects after treatment mapping.")
  }

  dm
}


# 7. AE - ADVERSE EVENTS --------------------------------------

create_ae <- function() {

  data <- get_dataset("Adverse_Events")

  if (is.null(data)) {
    return(NULL)
  }

  # Three repeated AE entry blocks in the source form

  suffixes <- c("", "_2", "_3")

  ae_list <- list()

  for (suffix in suffixes) {

    flag_var <- paste0("any_adverse_event", suffix)
    term_var <- paste0("adverse_event_term", suffix)
    start_var <- paste0("start_date_ad", suffix)
    end_var <- paste0("end_date_ad", suffix)
    severity_var <- paste0("severity", suffix)
    serious_var <- paste0("serious_ad", suffix)
    related_var <- paste0("related_to_study_drug", suffix)
    action_var <- paste0("actions_taken", suffix)
    outcome_var <- paste0("outcome", suffix)

    required_columns <- c(
      flag_var,
      term_var,
      start_var,
      end_var
    )

    missing_columns <- setdiff(
      required_columns,
      names(data)
    )

    if (length(missing_columns) > 0) {
      warning(
        paste(
          "AE block skipped. Missing:",
          paste(missing_columns, collapse = ", ")
        )
      )

      next
    }

    temp <- data %>%
      mutate(
        SUBJECT_ID = get_subject_id(data),
        AE_FLAG = .data[[flag_var]]
      ) %>%
      filter(
        tolower(trimws(as.character(AE_FLAG))) == "yes"
      ) %>%
      transmute(
        STUDYID = STUDYID_VALUE,
        DOMAIN = "AE",
        USUBJID = create_usubjid(SUBJECT_ID),
        AETERM = as.character(.data[[term_var]]),
        AESTDTC = convert_date(.data[[start_var]]),
        AEENDTC = convert_date(.data[[end_var]]),

        AESEV = toupper(
          trimws(as.character(.data[[severity_var]]))
        ),

        AESER = convert_yes_no(.data[[serious_var]]),

        AEREL = as.character(.data[[related_var]]),

        AEACN = as.character(.data[[action_var]]),

        AEOUT = as.character(.data[[outcome_var]])
      ) %>%
      filter(
        !is.na(AETERM),
        trimws(AETERM) != ""
      )

    ae_list[[suffix]] <- temp
  }

  if (length(ae_list) == 0) {
    return(empty_sdtm(c(
      "STUDYID", "DOMAIN", "USUBJID", "AESEQ",
      "AETERM", "AESTDTC", "AEENDTC",
      "AESEV", "AESER", "AEREL", "AEACN", "AEOUT"
    )))
  }

  ae <- bind_rows(ae_list) %>%
    group_by(USUBJID) %>%
    arrange(AESTDTC, .by_group = TRUE) %>%
    mutate(
      AESEQ = row_number()
    ) %>%
    ungroup() %>%
    select(
      STUDYID,
      DOMAIN,
      USUBJID,
      AESEQ,
      AETERM,
      AESTDTC,
      AEENDTC,
      AESEV,
      AESER,
      AEREL,
      AEACN,
      AEOUT
    )

  ae
}


# 8. CM - CONCOMITANT MEDICATIONS -----------------------------

create_cm <- function() {

  data <- get_dataset("Concomitant_Medication")

  if (is.null(data)) {
    return(NULL)
  }

  # Four repeated medication entry blocks

  suffixes <- c("", "_2", "_3", "_4")

  cm_list <- list()

  for (suffix in suffixes) {

    medication_var <- paste0("medication_name", suffix)
    indication_var <- paste0("indication", suffix)
    dose_var <- paste0("dose", suffix)
    start_var <- paste0("start_date", suffix)
    stop_var <- paste0("stop_date", suffix)
    ongoing_var <- paste0("ongoing", suffix)

    if (!medication_var %in% names(data)) {
      next
    }

    temp <- data %>%
      mutate(
        SUBJECT_ID = get_subject_id(data)
      ) %>%
      transmute(
        STUDYID = STUDYID_VALUE,
        DOMAIN = "CM",
        USUBJID = create_usubjid(SUBJECT_ID),

        CMTRT = as.character(.data[[medication_var]]),

        CMINDC = as.character(
          get_column(data, indication_var)
        ),

        CMDOSTXT = as.character(
          get_column(data, dose_var)
        ),

        CMSTDTC = convert_date(
          get_column(data, start_var)
        ),

        CMENDTC = convert_date(
          get_column(data, stop_var)
        ),

        ONGOING = convert_yes_no(
          get_column(data, ongoing_var)
        )
      ) %>%
      filter(
        !is.na(CMTRT),
        trimws(CMTRT) != ""
      )

    cm_list[[suffix]] <- temp
  }

  if (length(cm_list) == 0) {
    return(empty_sdtm(c(
      "STUDYID", "DOMAIN", "USUBJID", "CMSEQ",
      "CMTRT", "CMINDC", "CMDOSTXT",
      "CMSTDTC", "CMENDTC"
    )))
  }

  cm <- bind_rows(cm_list) %>%
    group_by(USUBJID) %>%
    mutate(
      CMSEQ = row_number()
    ) %>%
    ungroup() %>%
    select(
      STUDYID,
      DOMAIN,
      USUBJID,
      CMSEQ,
      CMTRT,
      CMINDC,
      CMDOSTXT,
      CMSTDTC,
      CMENDTC
    )

  cm
}


# 9. LB - LABORATORY TESTS ------------------------------------

create_lb <- function() {

  data <- get_dataset("Laboratory_Results")

  if (is.null(data)) {
    return(NULL)
  }

  # Laboratory analytes available in the source form

  analytes <- c(
    "hba1c",
    "fasting_plasma_glucose",
    "alt",
    "ast",
    "creatinine",
    "total_cholesterol",
    "ldl_cholesterol",
    "hdl_cholesterol",
    "triglycerides"
  )

  # Five repeated laboratory collection blocks

  suffixes <- c("", "_2", "_3", "_4", "_5")

  lb_list <- list()

  for (suffix in suffixes) {

    date_var <- paste0(
      "laboratory_collection_date",
      suffix
    )

    # Source visit labels are stored as visit_2 through visit_5.
    # The fifth repeated lab block has no confirmed visit-label field.
    visit_variables <- c(
      "visit_2",
      "visit_3",
      "visit_4",
      "visit_5",
      NA_character_
    )

    block_index <- match(suffix, suffixes)
    visit_var <- visit_variables[block_index]

    if (!date_var %in% names(data)) {
      next
    }

    for (analyte in analytes) {

      analyte_var <- paste0(analyte, suffix)

      if (!analyte_var %in% names(data)) {
        next
      }

      temp <- data %>%
        mutate(
          SUBJECT_ID = get_subject_id(data)
        ) %>%
        transmute(
          STUDYID = STUDYID_VALUE,
          DOMAIN = "LB",
          USUBJID = create_usubjid(SUBJECT_ID),

          # Use concise test codes (maximum 8 characters).
          # Confirm these against the study's controlled terminology
          # package before submission.
          LBTESTCD = case_when(
            analyte == "hba1c" ~ "HBA1C",
            analyte == "fasting_plasma_glucose" ~ "GLUC",
            analyte == "alt" ~ "ALT",
            analyte == "ast" ~ "AST",
            analyte == "creatinine" ~ "CREAT",
            analyte == "total_cholesterol" ~ "CHOL",
            analyte == "ldl_cholesterol" ~ "LDL",
            analyte == "hdl_cholesterol" ~ "HDL",
            analyte == "triglycerides" ~ "TRIG",
            TRUE ~ NA_character_
          ),

          LBTEST = case_when(
            analyte == "hba1c" ~ "Hemoglobin A1c",
            analyte == "fasting_plasma_glucose" ~ "Fasting Plasma Glucose",
            analyte == "alt" ~ "Alanine Aminotransferase",
            analyte == "ast" ~ "Aspartate Aminotransferase",
            analyte == "creatinine" ~ "Creatinine",
            analyte == "total_cholesterol" ~ "Total Cholesterol",
            analyte == "ldl_cholesterol" ~ "LDL Cholesterol",
            analyte == "hdl_cholesterol" ~ "HDL Cholesterol",
            analyte == "triglycerides" ~ "Triglycerides",
            TRUE ~ NA_character_
          ),

          LBORRES = as.character(.data[[analyte_var]]),

          LBDTC = convert_date(
            get_column(data, date_var)
          ),

          VISIT = if (
            !is.na(visit_var) && visit_var %in% names(data)
          ) {
            as.character(.data[[visit_var]])
          } else {
            NA_character_
          }
        ) %>%
        filter(
          !is.na(LBORRES),
          trimws(LBORRES) != ""
        )

      lb_list[[paste0(analyte, suffix)]] <- temp
    }
  }

  if (length(lb_list) == 0) {
    return(empty_sdtm(c(
      "STUDYID", "DOMAIN", "USUBJID", "LBSEQ",
      "LBTESTCD", "LBTEST", "LBORRES", "LBDTC", "VISIT"
    )))
  }

  lb <- bind_rows(lb_list) %>%
    mutate(
      LBORRES = as.character(LBORRES)
    ) %>%
    group_by(USUBJID) %>%
    mutate(
      LBSEQ = row_number()
    ) %>%
    ungroup() %>%
    select(
      STUDYID,
      DOMAIN,
      USUBJID,
      LBSEQ,
      LBTESTCD,
      LBTEST,
      LBORRES,
      LBDTC,
      VISIT
    )

  lb
}


# 10. VS - VITAL SIGNS ----------------------------------------

create_vs <- function() {

  data <- get_dataset("Baseline_and_Demographics")

  if (is.null(data)) {
    return(NULL)
  }

  # Weight and height are included as provisional VS records.
  # Confirm source units before final SDTM submission.

  vs <- data %>%
    select(
      record_id,
      subject_id,
      weight,
      height
    ) %>%
    pivot_longer(
      cols = c(weight, height),
      names_to = "SOURCE_TEST",
      values_to = "VSORRES"
    ) %>%
    mutate(
      STUDYID = STUDYID_VALUE,
      DOMAIN = "VS",
      USUBJID = create_usubjid(subject_id),

      VSTESTCD = case_when(
        SOURCE_TEST == "weight" ~ "WEIGHT",
        SOURCE_TEST == "height" ~ "HEIGHT"
      ),

      VSTEST = case_when(
        SOURCE_TEST == "weight" ~ "Weight",
        SOURCE_TEST == "height" ~ "Height"
      ),

      # Units intentionally left missing until verified
      VSORRESU = NA_character_,

      VSSTRESN = suppressWarnings(
        as.numeric(VSORRES)
      ),

      VSSTRESU = NA_character_
    ) %>%
    filter(!is.na(VSSTRESN)) %>%
    group_by(USUBJID) %>%
    mutate(
      VSSEQ = row_number()
    ) %>%
    ungroup() %>%
    select(
      STUDYID,
      DOMAIN,
      USUBJID,
      VSSEQ,
      VSTESTCD,
      VSTEST,
      VSORRES,
      VSORRESU,
      VSSTRESN,
      VSSTRESU
    )

  vs
}


# 11. DS - DISPOSITION ----------------------------------------

create_ds <- function() {

  data <- get_dataset("End_of_Study")

  if (is.null(data)) {
    return(NULL)
  }

  required_columns <- c(
    "reason_for_discontinuation",
    "study_completion_date"
  )

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    warning(
      "DS not created. Missing source columns: ",
      paste(missing_columns, collapse = ", ")
    )
    return(NULL)
  }

  # Identify participants with a documented discontinuation reason.
  # Filter first so every subsequent variable has the same row count.

  ds_source <- data %>%
    mutate(
      SUBJECT_ID = get_subject_id(data),
      DSTERM_SOURCE = as.character(reason_for_discontinuation)
    ) %>%
    filter(
      !is.na(DSTERM_SOURCE),
      trimws(DSTERM_SOURCE) != ""
    )

  if (nrow(ds_source) == 0) {
    warning("No documented discontinuation records found.")
    return(NULL)
  }

  ds <- ds_source %>%
    transmute(
      STUDYID = STUDYID_VALUE,
      DOMAIN = "DS",
      USUBJID = create_usubjid(SUBJECT_ID),
      DSTERM = DSTERM_SOURCE,


      # Controlled terminology mapping is intentionally left pending
      # until the study's terminology and disposition definitions
      # have been confirmed.
      DSDECOD = dplyr::case_when(
        DSTERM == "Withdrawal of Consent" ~ "WITHDRAWAL BY SUBJECT",
        DSTERM == "Lost to Follow-up" ~ "LOST TO FOLLOW-UP",
        DSTERM == "Protocol Deviation" ~ "PROTOCOL DEVIATION",
        DSTERM == "Adverse Event" ~ "ADVERSE EVENT",
        DSTERM == "Investigator Decision" ~ "INVESTIGATOR DECISION",
        TRUE ~ NA_character_
      ),

      # Source field is study_completion_date. Confirm that this field
      # represents the disposition date for discontinued subjects.
      DSSTDTC = convert_date(study_completion_date),

      DSSCAT = "STUDY DISPOSITION"
    ) %>%
    group_by(USUBJID) %>%
    mutate(DSSEQ = row_number()) %>%
    ungroup() %>%
    select(
      STUDYID,
      DOMAIN,
      USUBJID,
      DSSEQ,
      DSTERM,
      DSDECOD,
      DSSTDTC,
      DSSCAT
    )

  ds
}


# 12. GENERATE SDTM DOMAINS -----------------------------------

cat("\nGenerating SDTM domains...\n")

# Run each domain function independently. A failure in one domain
# will be recorded without preventing the other domains from running.

domain_functions <- list(
  DM = create_dm,
  AE = create_ae,
  CM = create_cm,
  LB = create_lb,
  VS = create_vs,
  DS = create_ds
)

sdtm_data <- list()

domain_errors <- data.frame(
  Domain = character(),
  Error = character(),
  stringsAsFactors = FALSE
)

for (domain_name in names(domain_functions)) {

  cat("\nGenerating", domain_name, "...\n")

  result <- tryCatch(
    domain_functions[[domain_name]](),
    error = function(e) {

      domain_errors <<- rbind(
        domain_errors,
        data.frame(
          Domain = domain_name,
          Error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      )

      cat(
        "ERROR in",
        domain_name,
        ":",
        conditionMessage(e),
        "\n"
      )

      NULL
    }
  )

  if (!is.null(result)) {

    sdtm_data[[domain_name]] <- result

    cat(
      domain_name,
      "generated:",
      nrow(result),
      "records and",
      ncol(result),
      "variables\n"
    )

  } else {

    cat(domain_name, "was not generated.\n")
  }
}

if (length(sdtm_data) == 0) {
  stop("No SDTM domains were generated. Review the errors above.")
}


# 13. SAVE SDTM DATASETS --------------------------------------

cat("\nExporting SDTM datasets...\n")

dir.create(
  sdtm_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

export_status <- data.frame(
  Domain = character(),
  Records = integer(),
  Variables = integer(),
  File = character(),
  Exported = logical(),
  Error = character(),
  stringsAsFactors = FALSE
)

for (domain_name in names(sdtm_data)) {

  output_file <- file.path(
    sdtm_dir,
    paste0(domain_name, ".csv")
  )

  export_error <- NA_character_

  exported <- tryCatch({

    write.csv(
      sdtm_data[[domain_name]],
      file = output_file,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    file.exists(output_file)

  }, error = function(e) {

    export_error <<- conditionMessage(e)
    FALSE
  })

  export_status <- bind_rows(
    export_status,
    data.frame(
      Domain = domain_name,
      Records = nrow(sdtm_data[[domain_name]]),
      Variables = ncol(sdtm_data[[domain_name]]),
      File = output_file,
      Exported = exported,
      Error = export_error,
      stringsAsFactors = FALSE
    )
  )

  if (exported) {
    cat("Saved:", output_file, "\n")
  } else {
    cat("Export failed:", domain_name, "-", export_error, "\n")
  }
}

write.csv(
  export_status,
  file.path(report_dir, "sdtm_export_status.csv"),
  row.names = FALSE
)


# 14. CREATE SDTM DOMAIN OVERVIEW -----------------------------

domain_descriptions <- c(
  DM = "Demographics",
  AE = "Adverse Events",
  CM = "Concomitant Medications",
  LB = "Laboratory Tests",
  VS = "Vital Signs",
  DS = "Disposition"
)

domain_overview <- data.frame(
  Domain = names(sdtm_data),
  Description = unname(domain_descriptions[names(sdtm_data)]),
  Records = vapply(sdtm_data, nrow, integer(1)),
  Variables = vapply(sdtm_data, ncol, integer(1)),
  stringsAsFactors = FALSE
)

write.csv(
  domain_overview,
  file.path(report_dir, "sdtm_domain_overview.csv"),
  row.names = FALSE
)


# 15. BASIC SDTM STRUCTURAL QC -------------------------------

qc_report <- data.frame()

for (domain_name in names(sdtm_data)) {

  df <- sdtm_data[[domain_name]]

  required_vars <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID"
  )

  missing_required <- setdiff(required_vars, names(df))

  missing_subject_ids <- if ("USUBJID" %in% names(df)) {
    sum(
      is.na(df$USUBJID) |
        trimws(as.character(df$USUBJID)) == ""
    )
  } else {
    nrow(df)
  }

  incorrect_domain <- if ("DOMAIN" %in% names(df)) {
    sum(
      is.na(df$DOMAIN) |
        as.character(df$DOMAIN) != domain_name
    )
  } else {
    nrow(df)
  }

  sequence_variable <- paste0(domain_name, "SEQ")
  duplicate_subject_sequence <- 0L

  if (all(c("USUBJID", sequence_variable) %in% names(df))) {
    duplicate_subject_sequence <- sum(
      duplicated(df[c("USUBJID", sequence_variable)])
    )
  }

  qc_report <- bind_rows(
    qc_report,
    data.frame(
      Domain = domain_name,
      Record_Count = nrow(df),
      Variable_Count = ncol(df),
      Missing_Required_Variables = paste(
        missing_required,
        collapse = ", "
      ),
      Missing_USUBJID = missing_subject_ids,
      Incorrect_DOMAIN = incorrect_domain,
      Duplicate_Subject_Sequence = duplicate_subject_sequence,
      stringsAsFactors = FALSE
    )
  )
}

write.csv(
  qc_report,
  file.path(report_dir, "sdtm_structural_qc.csv"),
  row.names = FALSE
)


# 16. SAVE UNMAPPED / PENDING ITEMS ---------------------------

pending_items <- data.frame(
  Source_Form = c(
    "Baseline_and_Demographics",
    "Baseline_and_Demographics",
    "Randomization / Drug_Accountability",
    "Drug_Accountability",
    "Eligibility_Assessment",
    "Laboratory_Results",
    "Clinical_Assessment",
    "Screening_and_InformedConsent",
    "End_of_Study"
  ),

  Source_Variables_or_Content = c(
    "rece",
    "weight and height units",
    "Actual treatment exposure and treatment start/end dates",
    "Drug dispensing and accountability details",
    "Inclusion and exclusion criteria",
    "Laboratory units and reference ranges",
    "Clinical assessment variables",
    "Consent and screening information",
    "Study completion status"
  ),

  Reason = c(
    "Race and ethnicity meaning must be clarified from the CRF.",
    "Confirm units before finalizing VS records.",
    "Planned ARM/ARMCD are now mapped from the validated master kit list. Actual treatment exposure and dates still require confirmation.",
    "Dispensing and accountability records do not automatically establish the complete exposure period.",
    "Eligibility criteria need review before domain assignment.",
    "Source units and reference ranges are not yet confirmed.",
    "Variable definitions and assessment context require review.",
    "Consent information requires review for appropriate SDTM representation.",
    "Completion and discontinuation status need separate mapping."
  ),

  Status = "Pending review",

  stringsAsFactors = FALSE
)

write.csv(
  pending_items,
  file.path(report_dir, "sdtm_pending_mapping_items.csv"),
  row.names = FALSE
)


# 17. SAVE DOMAIN GENERATION ERRORS ---------------------------

if (nrow(domain_errors) > 0) {

  write.csv(
    domain_errors,
    file.path(report_dir, "sdtm_domain_generation_errors.csv"),
    row.names = FALSE
  )

}


# 18. FINAL SUMMARY -------------------------------------------

cat("\n========================================\n")
cat("SDTM GENERATION SUMMARY\n")
cat("========================================\n")

cat("\nStudy ID:", STUDYID_VALUE, "\n")

cat("\nGenerated domains:\n")
print(domain_overview, row.names = FALSE)

cat("\nExport status:\n")
print(export_status, row.names = FALSE)

cat("\nTreatment allocation QC:\n")
print(treatment_allocation_qc, row.names = FALSE)

cat("\nStructural QC:\n")
print(qc_report, row.names = FALSE)

cat("\nFiles in SDTM output directory:\n")
print(list.files(sdtm_dir, full.names = TRUE))

cat("\nReports directory:\n")
print(list.files(report_dir, full.names = TRUE))

cat("\nImportant:\n")
cat(
  "These are provisional SDTM datasets. Complete source mapping, ",
  "controlled terminology, units, metadata, and validation before ",
  "treating them as submission-ready.\n",
  sep = ""
)

expected_domains <- c("DM", "AE", "CM", "LB", "VS", "DS")
missing_domains <- setdiff(expected_domains, names(sdtm_data))

if (all(export_status$Exported) && length(missing_domains) == 0) {
  cat("\nAll six expected SDTM domains were generated and exported.\n")
} else {
  cat("\nWARNING: SDTM generation is incomplete.\n")
  if (length(missing_domains) > 0) {
    cat("Missing domain(s): ", paste(missing_domains, collapse = ", "), "\n", sep = "")
  }
  cat("Review sdtm_domain_generation_errors.csv and sdtm_export_status.csv.\n")
}

if (nrow(domain_errors) > 0) {
  cat(
    "\nWARNING:",
    nrow(domain_errors),
    "domain(s) had generation errors. Check sdtm_domain_generation_errors.csv.\n"
  )
}

cat("\n========================================\n")
cat("END OF SCRIPT\n")
cat("========================================\n")
