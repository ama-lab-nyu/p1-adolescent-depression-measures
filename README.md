# Comparing Different Ways to Measure Adolescent Depression

### Project Summary

Researchers often treat different measures of the same psychological construct as interchangeable. 
In practice, measurement choices can shape the patterns, estimates, and conclusions that emerge from the same data. 

This project uses data from the National Longitudinal Study of Adolescent to Adult Health (Add Health) to examine how different operationalizations of adolescent depressive symptoms shape empirical findings. 
We compare three ways of measuring depressive symptoms: a full modified CES-D scale, a four-item brief screener, and a single-item indicator. 

The goal is not to identify the “best” depression measure, but to demonstrate how measurement choices can affect estimated prevalence, subgroup patterns, classification of elevated distress, and predictor-outcome associations.

## Data Source

Add Health data are available through the [Carolina Population Center at the University of North Carolina](https://addhealth.cpc.unc.edu/), and Public-use data are available from three sources:

- [Add Health Dataverse](https://dataverse.unc.edu/dataverse/addhealth) hosted by [UNC’s Research Data Management Core](https://researchdata.unc.edu/)
- [Inter-university Consortium for Political and Social Research (ICPSR)[(https://www.icpsr.umich.edu/sites/icpsr/home)
- [Association of Religion Data Archives (ARDA)](https://www.thearda.com/data-archive/browse-category?cid=C-A-A-A) Users may obtain the data from any of these sources, depending on their needs.

This project uses the public-use version of the Wave I In-Home Interview dataset. No restricted-use contract is required. 
Raw Add Health data are not stored in this repository. To reproduce the analysis, users should obtain the public-use Add Health data directly from Add Health and place the required files in the `/data/` folder.

## Repository Structure

This repository is organized to support a reproducible analysis workflow.

| Folder | Description |
|---|---|
| `/data/` | Raw and processed data files, or instructions for obtaining Add Health data |
| `/code/` | R scripts for data preparation, measure construction, and analysis |
| `/outputs/` | Tables, figures, and summary statistics produced by the analysis |
| `/docs/` | Project protocol, variable specifications, and manuscript drafts |

The `/data/` folder should not contain restricted or private files. If raw Add Health data cannot be shared, the folder should include instructions for obtaining the data and placing files locally.

## Measurement Approaches

This project compares three operationalizations of adolescent depressive symptoms.

| Measure | Description |
|---|---|
| Full modified CES-D scale | Uses all modified CES-D depressive symptom items available in Add Health Wave I, with positive affect items reverse-coded before summing. |
| Four-item brief screener | Uses a shorter set of depressive symptom items focused on core affective symptoms. |
| Single-item indicator | Uses one depressive symptom item as the simplest operationalization of adolescent distress. |

## Analysis Workflow

The analysis will be organized into numbered R scripts. Scripts should be run in order.

| Script | Purpose |
|---|---|
| `01_construct_measures.R` | Constructs the full modified CES-D scale, four-item brief screener, and single-item depressive symptom measure. |
| `02_build_sample.R` | Builds the complete-case analytic sample and documents the sample size at each restriction step. |
| `03_descriptives.R` | Produces descriptive statistics, distribution plots, and reliability estimates for the depressive symptom measures. |
| `04_subgroups.R` | Compares depressive symptom measures across key demographic subgroups, including gender and race/ethnicity. |
| `05_regressions.R` | Fits linear and logistic regression models using the same predictor set across all measurement approaches. |
| `06_agreement.R` | Calculates classification agreement across binary measures, including Cohen’s kappa and reclassification tables. |

Additional scripts may be added if needed, but the main analysis should follow this numbered workflow.

## R Version and Package Dependencies

This project will be run in R. The exact R version used for the final analysis will be recorded after the analysis environment is finalized.

Required R packages include:

| Package | Purpose |
|---|---|
| `tidyverse` | Data cleaning, data manipulation, reshaping, and plotting |
| `haven` | Importing Stata, SAS, or SPSS data files if needed |
| `janitor` | Cleaning variable names and producing simple tabulations |
| `psych` | Reliability analysis, including Cronbach’s alpha |
| `broom` | Tidying regression model output |
| `car` | Checking variance inflation factors for multicollinearity |
| `survey` | Survey-weighted sensitivity analyses |

To install the required packages, run:

```text
install.packages(c("tidyverse", "haven", "janitor", "psych", "broom", "car", "survey"))
```

## Planned Outputs

Project outputs will be saved in the `/outputs/` folder.

```text
/outputs/tables/      Descriptive tables, regression tables, and agreement tables
/outputs/figures/     Distribution plots, prevalence plots, subgroup figures, and coefficient plots
```

The main planned figures are:

| Figure | Description |
|---|---|
| Figure 1 | Distribution of depressive symptom measures |
| Figure 2 | Prevalence and classification differences across measures |
| Figure 3 | Demographic subgroup differences across measurement approaches |
| Figure 4 | Regression coefficient comparison across measures |

Final figures should be exported at 300 DPI or higher and saved in `/outputs/figures/`.

## Reproducibility

The goal of this repository is to make the analysis reproducible from the public-use Add Health data. A user should be able to clone the repository, obtain the required data files, run the numbered scripts in order, and reproduce the project tables and figures.

All analysis scripts should:

- be written in R;
- run from beginning to end without manual editing;
- include a short header describing the script purpose, inputs, and outputs;
- use clear variable names, such as `cesd_full_score`, `brief_screener_score`, or `single_item_depression`;
- include comments explaining important coding decisions;
- save tables, figures, and processed files to the appropriate output folders.

Before any output is treated as final, at least one team member who did not write the script should review it to confirm that the code runs correctly, follows the project protocol, and produces the expected outputs.

Any manual decisions, deviations from the planned workflow, or unresolved issues should be documented in the `/docs/` folder.

## Project Status

Current status: Repository setup and skeleton documentation.

The README will be updated as scripts, outputs, and manuscript materials are added.

## Collaborators

- **Principle Investigator:** [Brian Spitzer, PhD](mailto:brian.spitzer@nyu.edu)
- **Project Lead:** [Rophence Ojiambo, ScM](mailto:rophence.ojiambo@nyu.edu)
- [Isaiah Omari, MPH](mailto:io2087@nyu.edu)
- [Alice (Peiran) Wang](mailto:pw1279@nyu.edu)
- [Helen Liang, MS](mailto:hjl9111@nyu.edu)
- [Susan Chandraganti](mailto:svc2047@nyu.edu)


