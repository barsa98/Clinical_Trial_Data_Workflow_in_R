
# ============================================================
# Project: Clinical Trial Data Workflow in R
# Script: 04_data_quality_checks.R
# Purpose: Identify potential clinical data quality issues
# ============================================================


# 1. Set up project directories -------------------------------

setwd("C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R")

processed_dir <- file.path("data", "processed")
report_dir <- "reports"

if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found. Run Script 03 first.")
}

dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)


# 2. Import processed datasets --------------------------------

processed_files <- list.files(
  processed_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(processed_files) == 0) {
  stop("No CSV files found in data/processed/.")
}

processed_data <- lapply(
  processed_files,
  read.csv,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)

names(processed_data) <- tools::file_path_sans_ext(
  basename(processed_files)
)

cat("Datasets loaded:", length(processed_data), "\n")


# 3. Initialize report objects -------------------------------

quality_overview <- data.frame(
  Dataset = character(),
  Rows = integer(),
  Columns = integer(),
  Duplicate_Rows = integer(),
  Missing_Cells = integer(),
  Duplicate_Record_IDs = integer(),
  stringsAsFactors = FALSE
)

missingness_report <- data.frame(
  Dataset = character(),
  Variable = character(),
  Missing_Count = integer(),
  Missing_Percent = numeric(),
  stringsAsFactors = FALSE
)

record_id_report <- data.frame(
  Dataset = character(),
  Duplicate_Record_ID_Count = integer(),
  stringsAsFactors = FALSE
)

categorical_report <- data.frame(
  Dataset = character(),
  Variable = character(),
  Value = character(),
  Count = integer(),
  stringsAsFactors = FALSE
)


# 4. Run quality checks for each dataset ----------------------

for (dataset_name in names(processed_data)) {

  df <- processed_data[[dataset_name]]

  # 4a. Check duplicate complete rows
  duplicate_rows <- sum(duplicated(df))

  # 4b. Count all missing cells
  missing_cells <- sum(is.na(df))

  # 4c. Check record_id duplicates, if the column exists
  duplicate_record_ids <- 0L

  if ("record_id" %in% names(df)) {

    record_id <- trimws(as.character(df$record_id))

    # Exclude missing IDs from the duplicate count
    valid_ids <- record_id[!is.na(record_id) & record_id != ""]

    duplicate_record_ids <- sum(duplicated(valid_ids))

    record_id_report <- rbind(
      record_id_report,
      data.frame(
        Dataset = dataset_name,
        Duplicate_Record_ID_Count = as.integer(duplicate_record_ids),
        stringsAsFactors = FALSE
      )
    )
  }

  # 4d. Record dataset-level summary
  quality_overview <- rbind(
    quality_overview,
    data.frame(
      Dataset = dataset_name,
      Rows = nrow(df),
      Columns = ncol(df),
      Duplicate_Rows = as.integer(duplicate_rows),
      Missing_Cells = as.integer(missing_cells),
      Duplicate_Record_IDs = as.integer(duplicate_record_ids),
      stringsAsFactors = FALSE
    )
  )

  # 4e. Summarize missing values by variable
  for (col in names(df)) {

    missing_count <- sum(is.na(df[[col]]))

    if (missing_count > 0) {

      missingness_report <- rbind(
        missingness_report,
        data.frame(
          Dataset = dataset_name,
          Variable = col,
          Missing_Count = as.integer(missing_count),
          Missing_Percent = round(
            100 * missing_count / nrow(df), 2
          ),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # 4f. Summarize categorical values for review
  for (col in names(df)) {

    if (is.character(df[[col]])) {

      values <- df[[col]][!is.na(df[[col]])]
      unique_values <- unique(values)

      # Limit this report to fields with manageable
      # numbers of distinct values
      if (length(unique_values) > 0 &&
          length(unique_values) <= 30) {

        value_counts <- as.data.frame(
          table(values),
          stringsAsFactors = FALSE
        )

        names(value_counts) <- c("Value", "Count")

        categorical_report <- rbind(
          categorical_report,
          data.frame(
            Dataset = dataset_name,
            Variable = col,
            Value = as.character(value_counts$Value),
            Count = as.integer(value_counts$Count),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
}


# 5. Save quality control reports -----------------------------

write.csv(
  quality_overview,
  file.path(report_dir, "data_quality_overview.csv"),
  row.names = FALSE
)

write.csv(
  missingness_report,
  file.path(report_dir, "missing_values_by_variable.csv"),
  row.names = FALSE
)

write.csv(
  record_id_report,
  file.path(report_dir, "duplicate_record_id_checks.csv"),
  row.names = FALSE
)

write.csv(
  categorical_report,
  file.path(report_dir, "categorical_value_summary.csv"),
  row.names = FALSE
)


# 6. Display summary ------------------------------------------

cat("\nData quality checks completed.\n\n")

cat("Dataset overview:\n")
print(quality_overview, row.names = FALSE)

cat("\nPotential duplicate record IDs:\n")
print(
  subset(record_id_report, Duplicate_Record_ID_Count > 0),
  row.names = FALSE
)

cat("\nReports saved in:", report_dir, "\n")
cat("- data_quality_overview.csv\n")
cat("- missing_values_by_variable.csv\n")
cat("- duplicate_record_id_checks.csv\n")
cat("- categorical_value_summary.csv\n")




# Inspect identifier columns in all datasets

for (dataset_name in names(processed_data)) {

  df <- processed_data[[dataset_name]]

  cat("\n------------------------------------\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Possible identifier columns:\n")

  print(
    grep(
      "record_id|subject|participant|patient|screening|study_id",
      names(df),
      value = TRUE,
      ignore.case = TRUE
    )
  )
}



for (dataset_name in names(processed_data)) {

  df <- processed_data[[dataset_name]]

  if ("record_id" %in% names(df)) {
    cat(
      "\n", dataset_name,
      "| Unique record IDs:", length(unique(na.omit(df$record_id))),
      "| Rows:", nrow(df),
      "\n"
    )
  }
}




# ============================================================
# Step 4.1: Cross-dataset identifier consistency
# ============================================================

# 1. Compare record_id values across datasets

record_id_sets <- lapply(
  processed_data,
  function(df) {
    if ("record_id" %in% names(df)) {
      as.character(df$record_id)
    } else {
      character(0)
    }
  }
)

reference_ids <- record_id_sets[[1]]

for (dataset_name in names(record_id_sets)) {

  current_ids <- record_id_sets[[dataset_name]]

  cat("\nDataset:", dataset_name, "\n")
  cat("IDs missing compared with first dataset:",
      length(setdiff(reference_ids, current_ids)), "\n")
  cat("Additional IDs compared with first dataset:",
      length(setdiff(current_ids, reference_ids)), "\n")
}


# 2. Inspect subject_id and record_id relationships

datasets_with_subject_id <- names(processed_data)[
  vapply(
    processed_data,
    function(df) "subject_id" %in% names(df),
    logical(1)
  )
]

for (dataset_name in datasets_with_subject_id) {

  df <- processed_data[[dataset_name]]

  cat("\n------------------------------------\n")
  cat("Dataset:", dataset_name, "\n")

  print(
    head(
      df[, c("record_id", "subject_id")],
      10
    ),
    row.names = FALSE
  )

  cat("Missing subject IDs:",
      sum(is.na(df$subject_id) |
            trimws(as.character(df$subject_id)) == ""),
      "\n")

  cat("Unique subject IDs:",
      length(unique(
        df$subject_id[!is.na(df$subject_id) &
                        trimws(as.character(df$subject_id)) != ""]
      )),
      "\n")
}





# ============================================================
# Step 4.2: Cross-dataset subject ID consistency
# ============================================================

datasets_with_subject_id <- names(processed_data)[
  vapply(
    processed_data,
    function(df) "subject_id" %in% names(df),
    logical(1)
  )
]

# Use the first dataset as the reference
reference_dataset <- datasets_with_subject_id[1]

reference_df <- processed_data[[reference_dataset]]

reference_mapping <- reference_df[
  , c("record_id", "subject_id")
]

subject_id_check <- data.frame(
  Dataset = character(),
  Missing_Subject_IDs = integer(),
  Duplicate_Subject_IDs = integer(),
  Record_ID_Mismatches = integer(),
  Subject_ID_Mismatches = integer(),
  stringsAsFactors = FALSE
)

for (dataset_name in datasets_with_subject_id) {

  df <- processed_data[[dataset_name]]

  # Count missing subject IDs
  missing_subject_ids <- sum(
    is.na(df$subject_id) |
      trimws(as.character(df$subject_id)) == ""
  )

  # Count duplicate subject IDs
  valid_subject_ids <- as.character(
    df$subject_id[
      !is.na(df$subject_id) &
        trimws(as.character(df$subject_id)) != ""
    ]
  )

  duplicate_subject_ids <- sum(
    duplicated(valid_subject_ids)
  )

  # Compare record_id and subject_id mappings
  comparison <- merge(
    reference_mapping,
    df[, c("record_id", "subject_id")],
    by = "record_id",
    all = TRUE,
    suffixes = c("_reference", "_current")
  )

  record_id_mismatches <- sum(
    is.na(comparison$subject_id_reference) |
      is.na(comparison$subject_id_current)
  )

  subject_id_mismatches <- sum(
    !is.na(comparison$subject_id_reference) &
      !is.na(comparison$subject_id_current) &
      comparison$subject_id_reference !=
        comparison$subject_id_current
  )

  subject_id_check <- rbind(
    subject_id_check,
    data.frame(
      Dataset = dataset_name,
      Missing_Subject_IDs = as.integer(missing_subject_ids),
      Duplicate_Subject_IDs = as.integer(duplicate_subject_ids),
      Record_ID_Mismatches = as.integer(record_id_mismatches),
      Subject_ID_Mismatches = as.integer(subject_id_mismatches),
      stringsAsFactors = FALSE
    )
  )
}

print(subject_id_check, row.names = FALSE)

# Save report
write.csv(
  subject_id_check,
  file.path(report_dir, "subject_id_consistency_check.csv"),
  row.names = FALSE
)


 # Save report
 write.csv(
   subject_id_check,
   file.path(report_dir, "subject_id_consistency_check.csv"),
   row.names = FALSE
 )
 # Identify date-related columns
 
 for (dataset_name in names(processed_data)) {
 
   df <- processed_data[[dataset_name]]
 
   date_columns <- grep(
     "date|time",
     names(df),
     value = TRUE,
     ignore.case = TRUE
   )
 
   cat("\nDataset:", dataset_name, "\n")
   print(date_columns)
 }



 # ============================================================
 # Step 4.3: Inspect date formats and values
 # ============================================================
 
 date_inspection <- data.frame(
   Dataset = character(),
   Variable = character(),
   Data_Type = character(),
   Non_Missing_Values = integer(),
   Missing_Values = integer(),
   Example_Values = character(),
   stringsAsFactors = FALSE
 )
 
 for (dataset_name in names(processed_data)) {
 
   df <- processed_data[[dataset_name]]
 
   date_columns <- grep(
     "date",
     names(df),
     value = TRUE,
     ignore.case = TRUE
   )
 
   for (col in date_columns) {
 
     values <- df[[col]]
 
     non_missing <- values[!is.na(values)]
     examples <- unique(as.character(non_missing))
     date_inspection <- rbind(
       date_inspection,
       data.frame(
         Dataset = dataset_name,
         Variable = col,
         Data_Type = class(values)[1],
         Non_Missing_Values = length(non_missing),
         Missing_Values = sum(is.na(values)),
         Example_Values = paste(
           head(examples, 5),
           collapse = " | "
         ),
         stringsAsFactors = FALSE
       )
     )
   }
 }
 
 print(date_inspection, row.names = FALSE)


# Save the inspection report
 write.csv(
   date_inspection,
   file.path(report_dir, "date_format_inspection.csv"),
   row.names = FALSE
 )
 # Inspect time fields separately
 for (dataset_name in names(processed_data)) {
 
   df <- processed_data[[dataset_name]]
   time_columns <- grep(
     "time",
     names(df),
     value = TRUE,
     ignore.case = TRUE
   )
 
   if (length(time_columns) > 0) {
     cat("\nDataset:", dataset_name, "\n")
 
     for (col in time_columns) {
       cat("\nVariable:", col, "\n")
       print(head(unique(df[[col]]), 10))
     }
   }
 }



# ============================================================
# Step 4.4: Date format and chronology checks
# ============================================================

# 1. Define date parsing function

parse_study_date <- function(x) {
  as.Date(x, format = "%d-%m-%Y")
}


# 2. Initialize date quality reports

invalid_date_report <- data.frame(
  Dataset = character(),
  Variable = character(),
  Record_ID = character(),
  Original_Value = character(),
  stringsAsFactors = FALSE
)

date_chronology_report <- data.frame(
  Dataset = character(),
  Record_ID = character(),
  Start_Date_Variable = character(),
  End_Date_Variable = character(),
  Start_Date = character(),
  End_Date = character(),
  Issue = character(),
  stringsAsFactors = FALSE
)


# 3. Check date formats across all datasets

for (dataset_name in names(processed_data)) {

  df <- processed_data[[dataset_name]]

  date_columns <- grep(
    "date",
    names(df),
    value = TRUE,
    ignore.case = TRUE
  )

  for (col in date_columns) {

    original_values <- as.character(df[[col]])

    parsed_dates <- parse_study_date(original_values)

    # Identify non-missing values that failed parsing
    invalid_rows <- which(
      !is.na(original_values) &
        trimws(original_values) != "" &
        is.na(parsed_dates)
    )

    if (length(invalid_rows) > 0) {

      invalid_date_report <- rbind(
        invalid_date_report,
        data.frame(
          Dataset = dataset_name,
          Variable = col,
          Record_ID = as.character(
            df$record_id[invalid_rows]
          ),
          Original_Value = original_values[invalid_rows],
          stringsAsFactors = FALSE
        )
      )
    }
  }
}


# 4. Define date pairs for chronology checks

date_pairs <- list(

  Adverse_Events = list(
    c("start_date_ad", "end_date_ad"),
    c("start_date_ad_2", "end_date_ad_2"),
    c("start_date_ad_3", "end_date_ad_3")
  ),

  Concomitant_Medication = list(
    c("start_date", "stop_date"),
    c("start_date_2", "stop_date_2"),
    c("start_date_3", "stop_date_3"),
    c("start_date_4", "stop_date_4")
  ),

  Laboratory_Results = list(
    c("laboratory_collection_date", "review_date"),
    c("laboratory_collection_date_2", "review_date_2"),
    c("laboratory_collection_date_3", "review_date_3"),
    c("laboratory_collection_date_4", "review_date_4"),
    c("laboratory_collection_date_5", "review_date_5")
  )
)


# 5. Check chronological relationships

for (dataset_name in names(date_pairs)) {

  df <- processed_data[[dataset_name]]

  for (pair in date_pairs[[dataset_name]]) {

    start_col <- pair[1]
    end_col <- pair[2]

    if (all(c(start_col, end_col) %in% names(df))) {

      start_date <- parse_study_date(df[[start_col]])
      end_date <- parse_study_date(df[[end_col]])

      # Flag cases where the end date is earlier
      # than the corresponding start date
      issue_rows <- which(
        !is.na(start_date) &
          !is.na(end_date) &
          end_date < start_date
      )

      if (length(issue_rows) > 0) {

        date_chronology_report <- rbind(
          date_chronology_report,
          data.frame(
            Dataset = dataset_name,
            Record_ID = as.character(
              df$record_id[issue_rows]
            ),
            Start_Date_Variable = start_col,
            End_Date_Variable = end_col,
            Start_Date = as.character(
              start_date[issue_rows]
            ),
            End_Date = as.character(
              end_date[issue_rows]
            ),
            Issue = "End date precedes start date",
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
}


# 6. Save date QC reports

write.csv(
  invalid_date_report,
  file.path(report_dir, "invalid_date_values.csv"),
  row.names = FALSE
)

write.csv(
  date_chronology_report,
  file.path(report_dir, "date_chronology_issues.csv"),
  row.names = FALSE
)


# 7. Display results

cat("\nDate format validation:\n")
cat("Invalid date values:", nrow(invalid_date_report), "\n")

cat("\nDate chronology validation:\n")
cat("Chronology issues:", nrow(date_chronology_report), "\n")

if (nrow(invalid_date_report) > 0) {
  print(invalid_date_report, row.names = FALSE)
}

if (nrow(date_chronology_report) > 0) {
  print(date_chronology_report, row.names = FALSE)
}


nrow(invalid_date_report)
nrow(date_chronology_report)


# Validate Time formats

time_columns <- list(
  Laboratory_Results = c(
    "collection_time",
    "collection_time_2",
    "collection_time_3",
    "collection_time_4",
    "collection_time_5"
  ),
  Screening_and_InformedConsent = c(
    "time_of_consent_received"
  )
)

invalid_time_report <- data.frame()

for (dataset_name in names(time_columns)) {

  df <- processed_data[[dataset_name]]

  for (variable in time_columns[[dataset_name]]) {

    values <- df[[variable]]

    invalid_rows <- which(
      !is.na(values) &
      !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", values)
    )

    if (length(invalid_rows) > 0) {

      temp <- data.frame(
        Dataset = dataset_name,
        Variable = variable,
        Record_ID = df$record_id[invalid_rows],
        Original_Value = values[invalid_rows]
      )

      invalid_time_report <- rbind(
        invalid_time_report,
        temp
      )
    }
  }
}

write.csv(
  invalid_time_report,
  "reports/invalid_time_values.csv",
  row.names = FALSE
)

nrow(invalid_time_report)


# Screening and consent
head(
  processed_data$Screening_and_InformedConsent[
    c("record_id", "screening_date", "consent_date",
      "time_of_consent_received", "reconsent_date",
      "screening_status")
  ],
  10
)

# Randomization
head(
  processed_data$Randomization[
    c("record_id", "randomization_date", "dispensed_date")
  ],
  10
)

# End of study
head(
  processed_data$End_of_Study[
    c("record_id", "study_completion_date",
      "reason_for_discontinuation")
  ],
  10
)



# Cross form  chronology checks


# Convert dates temporarily for comparison
parse_study_date <- function(x) {
  as.Date(x, format = "%d-%m-%Y")
}

# Prepare datasets
screening <- processed_data$Screening_and_InformedConsent[
  c("record_id", "screening_date", "consent_date",
    "reconsent_date", "screening_status")
]

randomization <- processed_data$Randomization[
  c("record_id", "randomization_date", "dispensed_date")
]

end_study <- processed_data$End_of_Study[
  c("record_id", "study_completion_date")
]

# Combine datasets using record_id
cross_form_data <- merge(screening, randomization, by = "record_id")
cross_form_data <- merge(cross_form_data, end_study, by = "record_id")

# Convert date columns temporarily
date_variables <- c(
  "screening_date",
  "consent_date",
  "reconsent_date",
  "randomization_date",
  "dispensed_date",
  "study_completion_date"
)

for (variable in date_variables) {
  cross_form_data[[variable]] <-
    parse_study_date(cross_form_data[[variable]])
}

# Initialize chronology report
cross_form_chronology_report <- data.frame()

# Function to record chronology issues
add_chronology_issue <- function(data, condition, issue_name,
                                 earlier_variable, later_variable) {

  rows <- which(!is.na(condition) & condition)

  if (length(rows) > 0) {

    result <- data.frame(
      Record_ID = data$record_id[rows],
      Earlier_Event = earlier_variable,
      Earlier_Date = as.character(
        data[[earlier_variable]][rows]
      ),
      Later_Event = later_variable,
      Later_Date = as.character(
        data[[later_variable]][rows]
      ),
      Issue = issue_name
    )

    return(result)
  }

  return(data.frame())
}

# 1. Consent should not occur after randomization
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$consent_date >
      cross_form_data$randomization_date,
    "Consent after randomization",
    "consent_date",
    "randomization_date"
  )
)

# 2. Screening should not occur after randomization
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$screening_date >
      cross_form_data$randomization_date,
    "Screening after randomization",
    "screening_date",
    "randomization_date"
  )
)

# 3. Dispensing should not occur before randomization
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$dispensed_date <
      cross_form_data$randomization_date,
    "Dispensing before randomization",
    "randomization_date",
    "dispensed_date"
  )
)

# 4. Study completion should not occur before randomization
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$study_completion_date <
      cross_form_data$randomization_date,
    "Study completion before randomization",
    "randomization_date",
    "study_completion_date"
  )
)

# 5. Study completion should not occur before dispensing
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$study_completion_date <
      cross_form_data$dispensed_date,
    "Study completion before dispensing",
    "dispensed_date",
    "study_completion_date"
  )
)

# 6. Re-consent should not precede initial consent
cross_form_chronology_report <- rbind(
  cross_form_chronology_report,
  add_chronology_issue(
    cross_form_data,
    cross_form_data$reconsent_date <
      cross_form_data$consent_date,
    "Re-consent before initial consent",
    "consent_date",
    "reconsent_date"
  )
)

# Save report
write.csv(
  cross_form_chronology_report,
  "reports/cross_form_chronology_issues.csv",
  row.names = FALSE
)

# Display results
nrow(cross_form_chronology_report)

cross_form_chronology_report




# Check screen failures against randomization


screen_failure_check <- subset(
  cross_form_data,
  screening_status == "Screen Failure"
)

screen_failure_randomization_report <- screen_failure_check[
  !is.na(screen_failure_check$randomization_date),
  c("record_id", "screening_status", "randomization_date")
]

write.csv(
  screen_failure_randomization_report,
  "reports/screen_failure_randomization_check.csv",
  row.names = FALSE
)

nrow(screen_failure_randomization_report)

screen_failure_randomization_report



cross_form_data[
  cross_form_data$record_id %in%
    screen_failure_randomization_report$record_id,
  c(
    "record_id",
    "screening_date",
    "consent_date",
    "screening_status",
    "randomization_date",
    "dispensed_date"
  )
]


# Create a dedicated screen-failure QC report

# Identify screen failures with randomization dates
screen_failure_qc <- subset(
  cross_form_data,
  screening_status == "Screen Failure" &
    !is.na(randomization_date)
)

# Select relevant variables
screen_failure_qc <- screen_failure_qc[
  c(
    "record_id",
    "screening_status",
    "screening_date",
    "consent_date",
    "randomization_date",
    "dispensed_date"
  )
]

# Add QC finding
screen_failure_qc$QC_Issue <-
  "Screen failure record has a randomization date"

# Save report
write.csv(
  screen_failure_qc,
  "reports/screen_failure_randomization_issues.csv",
  row.names = FALSE
)

# Display results
nrow(screen_failure_qc)
screen_failure_qc




