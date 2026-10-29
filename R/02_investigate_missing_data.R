
# ============================================================
# Project: Clinical Trial Data Workflow in R
# Script: 02_investigate_missing_data.R
# Purpose: Investigate missing values and blank fields
# ============================================================
setwd("C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R")

 list.files("data/raw", pattern = "\\.csv$", full.names = TRUE)

# 1. Set up directories ---------------------------------------

raw_dir <- file.path("data", "raw")
report_dir <- "reports"

if (!dir.exists(raw_dir)) {
  stop("Run this script from the repository root.")
}

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


# 3. Summarize missing values and blanks ----------------------

missing_summary <- do.call(
  rbind,
  lapply(names(raw_data), function(dataset_name) {

    df <- raw_data[[dataset_name]]

    data.frame(
      Dataset = dataset_name,
      Variable = names(df),

      Missing_Count = vapply(
        df, function(x) sum(is.na(x)), integer(1)
      ),

      Blank_Count = vapply(
        df,
        function(x) {
          if (is.character(x)) {
            sum(trimws(x) == "", na.rm = TRUE)
          } else {
            0L
          }
        },
        integer(1)
      ),

      Total_Rows = nrow(df),
      stringsAsFactors = FALSE
    )
  })
)

missing_summary$Missing_Percent <- round(
  100 * missing_summary$Missing_Count /
    missing_summary$Total_Rows,
  2
)

write.csv(
  missing_summary,
  file.path(report_dir, "missing_data_summary.csv"),
  row.names = FALSE
)


# 4. Review variables with missing values or blanks -----------

issues_found <- subset(
  missing_summary,
  Missing_Count > 0 | Blank_Count > 0
)

issues_found <- issues_found[
  order(
    issues_found$Dataset,
    -issues_found$Missing_Count,
    -issues_found$Blank_Count
  ),
]

print(issues_found, row.names = FALSE)


# 5. Investigate Drug Accountability --------------------------

if ("Drug_Accountability" %in% names(raw_data)) {

  da <- raw_data[["Drug_Accountability"]]

  missed_cols <- grep(
    "^number_of_missed_doses",
    names(da),
    value = TRUE
  )

  for (col in missed_cols) {

    # Identify the corresponding missed-dose indicator.
    suffix <- sub(
      "^number_of_missed_doses",
      "",
      col
    )

    indicator_col <- paste0("missed_any_doses", suffix)

    if (indicator_col %in% names(da)) {

      cat("\n", col, "versus", indicator_col, "\n")

      print(
        table(
          da[[indicator_col]],
          is.na(da[[col]]),
          useNA = "ifany"
        )
      )
    }
  }
}


# 6. Investigate laboratory collection fields -----------------

if ("Laboratory_Results" %in% names(raw_data)) {

  lab <- raw_data[["Laboratory_Results"]]

  collection_cols <- grep(
    "^was_sample_collected",
    names(lab),
    value = TRUE
  )

  reason_cols <- grep(
    "^reason_for_not_collecting",
    names(lab),
    value = TRUE
  )

  cat("\nLaboratory collection indicators:\n")

  for (col in collection_cols) {
    cat("\n", col, "\n")
    print(table(lab[[col]], useNA = "ifany"))
  }

  cat("\nMissing reasons for non-collection:\n")

  for (col in reason_cols) {
    cat(
      col, ":",
      sum(is.na(lab[[col]])),
      "NA values\n"
    )
  }
}


# 7. Save findings --------------------------------------------

cat("\nMissing data investigation complete.\n")
cat("Review reports/missing_data_summary.csv\n")
cat("Conditional checks above require interpretation.\n")
