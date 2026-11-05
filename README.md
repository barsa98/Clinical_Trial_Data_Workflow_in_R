# Clinical_Trial_Data_Workflow_in_R

An end-to-end clinical trial data programming workflow in R, covering raw EDC data exploration, data cleaning, clinical data quality control, CDISC SDTM domain preparation, and initial ADaM analysis dataset development using simulated Phase II Type 2 Diabetes clinical trial data.

## Project Overview

This project demonstrates a clinical trial data programming workflow using R, starting with raw electronic data capture (EDC) exports and progressing through data exploration, cleaning, quality control, CDISC SDTM domain creation, and ADaM dataset development.

The project uses simulated clinical trial data from a fictional Phase II, randomized, double-blind, placebo-controlled study of DTM-201 in participants with Type 2 Diabetes Mellitus (T2DM), with a planned study duration of 24 weeks.

The workflow focuses on understanding clinical source data, identifying and documenting data quality issues, applying data cleaning rules, mapping source data to CDISC structures, and preparing analysis datasets for downstream statistical analysis.

## Project Objectives

- Import and understand raw clinical trial datasets.
- Review dataset structures, variable types, missing values, and unique identifiers.
- Investigate relationships and consistency across source forms.
- Develop data cleaning and standardization procedures.
- Perform clinical data quality checks and document findings.
- Prepare source data for downstream processing.
- Map source data to relevant CDISC SDTM domains.
- Develop subject-level and other analysis datasets using ADaM concepts.
- Perform structural and consistency checks on derived datasets.
- Maintain reproducible scripts and document programming decisions.

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

The `data/raw/` directory contains ten CSV datasets representing different clinical trial forms:

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

Each source file initially contains 120 records. The datasets differ in their structures, variables, and completeness.

The raw files are retained separately from processed and derived datasets to preserve source data and support reproducibility.

## Implemented Workflow

### 1. Data Import and Exploration

- Imported the raw EDC-style CSV files into R.
- Reviewed dataset dimensions, variable names, and data types.
- Examined missing values and blank fields.
- Reviewed identifier variables and relationships across source datasets.
- Investigated repeated records and longitudinal data structures.

### 2. Data Cleaning and Standardization

- Created separate directories for raw and processed datasets.
- Identified blank strings and whitespace-related inconsistencies.
- Applied cleaning rules to standardize selected categorical variables and missing-value representations.
- Generated a cleaning log to document cleaning operations.
- Saved processed datasets separately from the original source files.

### 3. Clinical Data Quality Control

Implemented checks covering:

- Duplicate records and duplicate record identifiers.
- Record identifier uniqueness.
- Subject identifier completeness and consistency.
- Record-to-subject identifier mismatches.
- Missing values and completeness across source datasets.
- Cross-form identifier relationships.

Initial QC results showed no duplicate record identifiers, missing or duplicate identifiers in the reviewed ID fields, or record-to-subject mismatches.

Missing-value patterns were also reviewed across datasets. These were treated as findings for investigation rather than automatically assuming that every missing value represented an error.

### 4. SDTM Domain Preparation

Created six SDTM-style domains from the processed source data:

| Domain | Description | Records |
|---|---|---:|
| DM | Demographics | 120 |
| AE | Adverse Events | 67 |
| CM | Concomitant Medications | 326 |
| LB | Laboratory Results | 5,400 |
| VS | Vital Signs | 240 |
| DS | Disposition | 11 |

Additional implementation work included:

- Mapping subject-level demographic information into DM.
- Mapping adverse event and concomitant medication records.
- Preparing laboratory and vital signs datasets.
- Creating disposition records from study discontinuation information.
- Mapping disposition terms to proposed standardized decoded values.
- Linking the randomization kit information to a separate master randomization list to derive planned treatment assignments.
- Populating `ARMCD` and `ARM` in DM with the planned treatment groups.

The planned treatment allocation was mapped as 60 participants to Drug and 60 participants to Placebo.

The treatment mapping represents planned study arm assignment; it should not be interpreted as confirmation of actual treatment received.

### 5. ADaM Dataset Development

Created five initial ADaM-style analysis datasets:

| Dataset | Description | Records |
|---|---|---:|
| ADSL | Subject-Level Analysis Dataset | 120 |
| ADAE | Adverse Events Analysis Dataset | 67 |
| ADCM | Concomitant Medications Analysis Dataset | 326 |
| ADLB | Laboratory Results Analysis Dataset | 5,400 |
| ADVS | Vital Signs Analysis Dataset | 240 |

Implementation work included:

- Creating a subject-level ADSL dataset.
- Carrying subject identifiers and demographic information into analysis datasets.
- Incorporating planned treatment group information.
- Deriving disposition-related subject-level variables, including `DCSFL`, `DCSREAS`, and `DCSDECOD`.
- Preparing analysis datasets for adverse events, concomitant medications, laboratory results, and vital signs.
- Reviewing subject identifiers, treatment assignments, and dataset-level structure.

Initial ADaM QC confirmed that all five datasets contained subject identifiers and that the treatment assignments in ADSL were consistent with the planned 60:60 allocation.

Further derivation and validation work remains necessary before considering these datasets fully analysis-ready.


## Project Status

**Ongoing — Core data processing, SDTM domain creation, and initial ADaM development completed.**

The project has progressed from raw EDC data exploration through cleaning, clinical data QC, SDTM domain preparation, and ADaM dataset development. The remaining work focuses on refining derivations, strengthening validation, and documenting traceability.

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
│   ├── 02_investigate_missing_data.R
│   ├── 03_clean_and_standardize.R
│   ├── 04_data_quality_checks.R
│   ├── 05_create_sdtm.R
│   └── 06_create_adam.R
└── reports/
```

The repository structure may be extended as additional scripts, specifications, QC reports, and documentation are added.

## Tools and Technologies

- **R:** Data import, transformation, cleaning, and programming.
- **Base R:** Data handling, validation, and file operations.
- **Clinical data quality control:** Identifier checks, completeness checks, duplicate checks, and cross-form consistency.
- **CDISC SDTM:** Source data mapping and domain preparation.
- **CDISC ADaM:** Analysis dataset development and subject-level derivations.
- **Reproducible programming:** Modular scripts, separate data layers, and documented transformations.

## Project Achievements

- Imported and explored all ten raw EDC datasets.
- Reviewed dataset structures, variable types, missing-value patterns, and identifier relationships.
- Investigated missing data, blank fields, duplicate records, and cross-form consistency.
- Implemented data cleaning and standardization procedures.
- Generated cleaning logs and maintained separate raw and processed datasets.
- Implemented clinical data quality checks covering completeness, uniqueness, identifier consistency, and duplicate records.
- Created six CDISC SDTM-style domains: DM, AE, CM, LB, VS, and DS.
- Mapped planned treatment assignments into DM using a separate master randomization list.
- Developed five ADaM-style analysis datasets: ADSL, ADAE, ADCM, ADLB, and ADVS.
- Derived subject-level disposition variables in ADSL.
- Performed structural, completeness, and consistency QC on the generated ADaM datasets.
- Reviewed dataset-level record counts, subject identifiers, treatment allocation, and key derivations.
- Organized the workflow into reproducible R scripts with separate storage for raw, processed, SDTM, and ADaM datasets.

## Future Enhancements

Potential extensions to the project include:

- Additional validation against applicable CDISC conformance rules and controlled terminology.
- Further refinement of analysis derivations and source-to-analysis traceability.
- Expanded independent QC and validation reporting.
- Descriptive and exploratory analysis using the generated ADaM datasets.


## Reproducibility

The R scripts are designed to run from the repository root directory.

Raw data, processed data, SDTM domains, and ADaM datasets are stored separately. This separation helps preserve source data, maintain traceability, and make transformations reproducible.

Cleaning operations and selected programming decisions are documented through logs and project documentation.

## Important Notes

This is a learning and portfolio project based on simulated clinical trial data. It demonstrates clinical data programming concepts and the application of R to clinical trial data workflows.

The datasets and outputs are not part of a validated clinical trial submission system and are not intended for clinical decision-making.

Any data published in this repository should be confirmed to be synthetic and free of real participant information.

### Project Status

**Core workflow completed.**

The project demonstrates an end-to-end clinical trial data programming workflow in R, from raw EDC data exploration and cleaning through clinical data quality checks, SDTM domain preparation, and ADaM dataset development. It provides a reproducible foundation for clinical data processing and analysis using simulated Phase II Type 2 Diabetes trial data.
