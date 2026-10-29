
# ============================================================
# Project: Clinical Trial Data Workflow in R
# Script: 03_clean_and_standardize.R
# Purpose: Standardize blank fields and whitespace
# ============================================================

# 1. Set up project directories -------------------------------

setwd("C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R")

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
report_dir <- "reports"

if (!dir.exists(raw_dir)) {
  stop("Run this script from the repository root.")
}

dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)


# 2. Import raw datasets --------------------------------------

raw_files <- list.files(
  raw_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(raw_files) == 0) {
  stop("No CSV files found in data/raw/.")
}

raw_data <- lapply(
  raw_files,
  read.csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(raw_data) <- tools::file_path_sans_ext(
  basename(raw_files)
)



# 3. Clean and standardize character fields

clean_dataset <- function(df, dataset_name) {

  dataset_log <- data.frame(
    Dataset = character(),
    Variable = character(),
    Whitespace_Trimmed = integer(),
    Blank_Values_Converted_To_NA = integer(),
    stringsAsFactors = FALSE
  )

  for (col in names(df)) {

    if (is.character(df[[col]])) {

      original_values <- df[[col]]
      trimmed_values <- trimws(original_values)

      whitespace_count <- sum(
        !is.na(original_values) &
          original_values != trimmed_values
      )

      blank_count <- sum(
        !is.na(trimmed_values) &
          trimmed_values == ""
      )

      # Remove leading and trailing whitespace
      df[[col]] <- trimmed_values

      # Convert blank character values to NA
      df[[col]][
        !is.na(df[[col]]) & df[[col]] == ""
      ] <- NA

      # Record changes for this variable
      if (whitespace_count > 0 || blank_count > 0) {

        dataset_log <- rbind(
          dataset_log,
          data.frame(
            Dataset = dataset_name,
            Variable = col,
            Whitespace_Trimmed = as.integer(whitespace_count),
            Blank_Values_Converted_To_NA = as.integer(blank_count),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  # Return both the cleaned dataset and its log
  list(
    data = df,
    log = dataset_log
  )
}

# Apply the function to every raw dataset
cleaning_results <- lapply(
  names(raw_data),
  function(dataset_name) {
    clean_dataset(raw_data[[dataset_name]], dataset_name)
  }
)

names(cleaning_results) <- names(raw_data)

# Extract the cleaned datasets
processed_data <- lapply(
  cleaning_results,
  function(result) result$data
)

# Combine cleaning logs from all datasets
logs <- lapply(
  cleaning_results,
  function(result) result$log
)

logs <- logs[vapply(logs, nrow, integer(1)) > 0]

if (length(logs) > 0) {
  cleaning_log <- do.call(rbind, logs)
} else {
  cleaning_log <- data.frame(
    Dataset = character(),
    Variable = character(),
    Whitespace_Trimmed = integer(),
    Blank_Values_Converted_To_NA = integer(),
    stringsAsFactors = FALSE
  )
}

row.names(cleaning_log) <- NULL



# 4. Save processed datasets ----------------------------------

for (dataset_name in names(processed_data)) {

  output_file <- file.path(
    processed_dir,
    paste0(dataset_name, ".csv")
  )

  write.csv(
    processed_data[[dataset_name]],
    output_file,
    row.names = FALSE,
    na = ""
  )
}


# 5. Save cleaning log ----------------------------------------

write.csv(
  cleaning_log,
  file.path(report_dir, "cleaning_log.csv"),
  row.names = FALSE
)


# 6. Confirm outputs ------------------------------------------

cat("Cleaning and standardization complete.\n")
cat("Datasets processed:", length(processed_data), "\n")
cat("Processed files saved to:", processed_dir, "\n")
cat("Cleaning log saved to:", file.path(report_dir, "cleaning_log.csv"), "\n")

print(cleaning_log, row.names = FALSE)
