# Check the version
R.version.string

# Check the directory
getcwd()


# Check the instrallled packages
 rownames(installed.packages())


# Load all the files from a  folder 

 raw_files <- list.files( path="C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_Trial_Workflow_in_R/data/raw",  pattern = "\\csv$", full.names = TRUE)

#check 
raw_files
 length(raw_files)


# Read all the files 
library(readr)

 raw_data <- lapply(raw_files, read.csv, stringsAsFactors = FALSE, check.names = FALSE)

 names(raw_data) <- tools::file_path_sans_ext(basename(raw_files))
 raw_data$Adverse_Events


 df = raw_data[["Adverse_Events"]]



cat("\n====================================\n")
cat("Dataset:", "Adverse_Events", "\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("====================================\n")

  # Column names
  print(names(df))

  # Data types
  
print(sapply(df, class))

  # Missing values per column
  print(colSums(is.na(df)))

  # First five records
  print(head(df, 5))


# length and names of the files

length(raw_data)
names(raw_data)

# Create a compact data inventory to produce a summary of the size and missingness of each dataset.


data_inventory <- data.frame(Dataset = names(raw_data),Rows = sapply(raw_data, nrow),Columns = sapply(raw_data, ncol),Missing_Values = sapply(raw_data,function(df) sum(is.na(df))))

print(data_inventory)



# Inspect the columns in every dataset

# This will help us identify possible participant identifiers, dates, visit variables, treatment assignments, laboratory measurements and other fields.

# IMPORTANT: NA counts alone do not tell us whether data are missing incorrectly. For example, an end date may legitimately be blank for an ongoing adverse event. We will interpret missing values according to the meaning of each variable.


for (dataset_name in names(raw_data)) { cat("\n---", dataset_name, "---\n"); df <- raw_data[[dataset_name]]; print(data.frame(Variable = names(df), Data_Type = sapply(df, function(x) class(x)[1]), Missing = colSums(is.na(df)), Unique_Values = sapply(df, function(x) length(unique(x))))) }



# Inspect the actual variables without printing everything
# names() shows the variable names, while str() shows their data types and a sample of their values.

names(raw_data$Baseline_and_Demographics)
str(raw_data$Baseline_and_Demographics)

names(raw_data$Randomization)
str(raw_data$Randomization)


names(raw_data$Laboratory_Results)
str(raw_data$Laboratory_Results)



# Check the missing values in the two affected datasets

colSums(is.na(raw_data$Drug_Accountability))
colSums(is.na(raw_data$Laboratory_Results))


# Check whether blank strings are also present:


count_blanks<- function(df) {sapply(df, function(x) {if (is.character(x)) {sum(trimws(x) == "", na.rm = TRUE)} else { 0}})}

count_blanks(raw_data$Drug_Accountability)

count_blanks(raw_data$Laboratory_Results)




# To check which datasets contain participant identifiers and whether record_id is unique within each dataset.

id_inventory <- do.call(rbind, lapply(names(raw_data), function(n) { df <- raw_data[[n]]; ids <- grep("subject|sub_id|participant|record_id", names(df), ignore.case = TRUE, value = TRUE); data.frame(Dataset = n, ID_Columns = paste(ids, collapse = ", "), Rows = nrow(df), Record_ID_Unique = if ("record_id" %in% names(df)) !anyDuplicated(df$record_id) else NA) })); print(id_inventory, row.names = FALSE)



# To check whether the record_id values match across the nine files

reference_id <- raw_data$Baseline_and_Demographics$record_id; record_id_check <- sapply(names(raw_data), function(nm) { df <- raw_data[[nm]]; if ("record_id" %in% names(df) && length(df$record_id) == length(reference_id)) { all(df$record_id == reference_id) } else { NA } }); print(record_id_check)


# Investigate the missing values

lapply(c("was_sample_collected", "was_sample_collected_2", "was_sample_collected_3", "was_sample_collected_4", "was_sample_collected_5"), function(v) table(raw_data$Laboratory_Results[[v]], useNA = "ifany"))

# Inspect the relationship between missed-dose responses and missing counts

da <- raw_data$Drug_Accountability; table(da$missed_any_doses, is.na(da$number_of_missed_doses), useNA = "ifany"); table(da$missed_any_doses_2, is.na(da$number_of_missed_doses_2), useNA = "ifany"); table(da$missed_any_doses_3, is.na(da$number_of_missed_doses_3), useNA = "ifany"); table(da$missed_any_doses_4, is.na(da$number_of_missed_doses_4), useNA = "ifany")

