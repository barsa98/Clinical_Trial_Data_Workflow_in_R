# Clinical_Trial_Data_Workflow_in_R
An end-to-end clinical trial data programming workflow in R, covering raw EDC data exploration, data cleaning, quality control, CDISC SDTM and ADaM dataset preparation, and analysis-ready data development using simulated Phase II Type 2 Diabetes clinical trial data.


## Project Overview

This project demonstrates an end-to-end clinical trial data programming workflow using R, starting from raw electronic data capture (EDC) exports and progressing towards clean clinical datasets, data quality checks, CDISC SDTM domains, ADaM analysis datasets, and analysis-ready data.

The project uses simulated clinical trial data from a fictional Phase II, randomized, double-blind, placebo-controlled study of DTM-201 in participants with Type 2 Diabetes Mellitus (T2DM), with a planned study duration of 24 weeks.

The aim is to build a reproducible workflow that demonstrates clinical data handling, data cleaning, validation, programming, and preparation of datasets for statistical analysis.

## Project Objectives

- Import and understand raw clinical trial datasets.
- Review dataset structures, variable types, missing values, and unique identifiers.
- Develop a data dictionary and document data structures.
- Clean and standardize variables, dates, and categorical values.
- Perform clinical data quality checks and document findings.
- Reshape and prepare data for downstream processing.
- Create relevant CDISC SDTM domains.
- Develop ADaM datasets, including subject-level and analysis datasets.
- Prepare analysis-ready data for descriptive and exploratory analysis.
- Document programming decisions, data issues, and quality control results.

## Study Overview

| Attribute | Description |
|---|---|
| Study phase | Phase II |
| Study design | Randomized, double-blind, placebo-controlled |
| Therapeutic area | Type 2 Diabetes Mellitus |
| Study treatment | DTM-201 |
| Planned duration | 24 weeks |
| Data source | Simulated EDC-style CSV exports |
| Programming language | R |

## Data Sources

The `data/raw/` directory contains nine CSV datasets representing different clinical trial forms:

- Adverse Events
- Baseline and Demographics
- Clinical Assessment
- Concomitant Medication
- Drug Accountability
- Eligibility Assessment
- End of Study
- Laboratory Results
- Randomization
- Screening and Informed Consent

These files represent raw source data for the workflow. They will be retained unchanged, while processed datasets and derived outputs will be stored separately.

## Planned Workflow

1. **Study and data familiarization** — understand the study structure, source forms, variables, and identifiers.
2. **Data import and exploration** — import CSV files into R and examine dimensions, data types, missing values, and variable distributions.
3. **Data cleaning and standardization** — address inconsistent formats, date variables, categorical values, and documented data issues.
4. **Clinical data quality control** — check uniqueness, consistency, completeness, cross-form relationships, and study-specific rules.
5. **SDTM preparation** — map appropriate source variables into relevant SDTM domains and document mapping decisions.
6. **ADaM preparation** — create analysis datasets, including subject-level data and relevant laboratory or adverse-event analysis data.
7. **Analysis and reporting** — perform exploratory analysis and generate reproducible outputs and QC reports.

The workflow will be developed incrementally. Transformations and validation rules will be documented as they are implemented.

## Current Progress

### Completed

- Set up the initial R project structure.
- Imported the raw CSV datasets into R.
- Reviewed dataset dimensions and missing-value counts.
- Confirmed that the source files contain 120 rows each.
- Started reviewing identifier fields and dataset structures.
- Began preparing a draft data dictionary.

### In Progress

- Confirming subject identifiers and relationships across source forms.
- Reviewing missing values, blank fields, repeated visit structures, and potential data quality issues.
- Defining documented cleaning and validation rules.

### Planned

- Complete data cleaning and standardization.
- Implement reusable clinical data quality checks.
- Reshape longitudinal laboratory and other repeated-visit data where appropriate.
- Develop SDTM and ADaM datasets.
- Add validation outputs, analysis examples, and documentation.

## Repository Structure

```text
clinical-trial-data-workflow-in-R/
├── README.md
├── data/
│   ├── raw/
│   ├── processed/
│   ├── sdtm/
│   └── adam/
├── R/
│   ├── 01_import_and_initial_data_exploration.R
│   ├── 02_clean_and_standardize.R
│   ├── 03_data_quality_checks.R
│   ├── 04_create_sdtm.R
│   └── 05_create_adam.R
└── reports/
```

Some scripts and folders are planned and will be added as the project progresses.

## Tools and Technologies

- R
- Base R data import and manipulation
- Clinical data review and quality control
- CDISC SDTM and ADaM concepts
- Reproducible data processing and reporting

Additional packages and tools will be documented as they are introduced.

## Reproducibility

The R scripts are intended to run from the repository root directory. Raw data will be kept separate from processed and derived datasets so that transformations can be traced and repeated.

## Important Notes

This is a learning and portfolio project based on simulated clinical trial data. It is intended to demonstrate programming methods and an understanding of clinical data workflows; it is not a validated clinical trial system and is not intended for clinical decision-making.

Any data published in this repository should be confirmed to be synthetic and free of real participant information.

## Project Status

**Work in progress.** The current focus is raw data exploration, understanding the source datasets, and establishing the foundations for data cleaning and quality control.
