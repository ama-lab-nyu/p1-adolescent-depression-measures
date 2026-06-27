################################################################################
# Script: 01_construct_measures_Rophence.R
# Purpose: Construct depressive symptom measures for the Add Health P1 project
################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)

# Extract the actual Add Health numeric code from factor-labelled variables.
# Example: "(95) (95) 1995" becomes 95.
get_code <- function(x) {
  readr::parse_number(as.character(x))
}

# Load the Add Health Wave I data
loaded_name <- load("data/21600-0001-Data.rda")

# Save the loaded dataset with a clear working name
addhealth_raw <- get(loaded_name[1])


################################################################################
# 1. Define depressive symptom items
################################################################################

# Full modified CES-D items
cesd_items <- paste0("H1FS", 1:19)

# Positive affect items that must be reverse-coded
positive_affect_items <- c("H1FS4", "H1FS8", "H1FS11", "H1FS15")

# Four items used for the brief screener
brief_items <- c("H1FS6", "H1FS16", "H1FS3", "H1FS13")


################################################################################
# 2. Clean depressive symptom items
################################################################################

# Valid H1FS responses are 0, 1, 2, and 3.
# Special codes 6 = refused and 8 = don't know are set to missing.
clean_h1fs <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% 0:3     ~ x_num,
    x_num %in% c(6, 8) ~ NA_real_,
    TRUE               ~ NA_real_
  )
}

# Clean all 19 depressive symptom items
addhealth_measures <- addhealth_raw %>%
  mutate(
    across(
      all_of(cesd_items),
      clean_h1fs,
      .names = "{.col}_clean"
    )
  )


################################################################################
# 3. Reverse-code positive affect items
################################################################################

# Original scale: 0 to 3
# Reverse-coded scale: 3 - original value
# Higher values should always mean more depressive symptoms.
addhealth_measures <- addhealth_measures %>%
  mutate(
    H1FS4_rev  = 3 - H1FS4_clean,
    H1FS8_rev  = 3 - H1FS8_clean,
    H1FS11_rev = 3 - H1FS11_clean,
    H1FS15_rev = 3 - H1FS15_clean
  )


################################################################################
# 4. Construct the three depressive symptom measures
################################################################################

# Items used in the full 19-item modified CES-D score
# The positive affect items use their reverse-coded versions.
full_score_items <- c(
  "H1FS1_clean",
  "H1FS2_clean",
  "H1FS3_clean",
  "H1FS4_rev",
  "H1FS5_clean",
  "H1FS6_clean",
  "H1FS7_clean",
  "H1FS8_rev",
  "H1FS9_clean",
  "H1FS10_clean",
  "H1FS11_rev",
  "H1FS12_clean",
  "H1FS13_clean",
  "H1FS14_clean",
  "H1FS15_rev",
  "H1FS16_clean",
  "H1FS17_clean",
  "H1FS18_clean",
  "H1FS19_clean"
)

# Items used in the four-item brief screener
brief_score_items <- paste0(brief_items, "_clean")

# Pull item data for scoring
full_item_data <- addhealth_measures %>%
  select(all_of(full_score_items))

brief_item_data <- addhealth_measures %>%
  select(all_of(brief_score_items))

# Construct the three outcomes
addhealth_measures <- addhealth_measures %>%
  mutate(
    # Full scale is calculated only when all 19 items are non-missing.
    cesd_full_score = if_else(
      complete.cases(full_item_data),
      rowSums(full_item_data),
      NA_real_
    ),
    
    # Brief screener is calculated only when all 4 items are non-missing.
    cesd_brief_score = if_else(
      complete.cases(brief_item_data),
      rowSums(brief_item_data),
      NA_real_
    ),
    
    # Single-item depressed mood outcome.
    cesd_single_depressed_ord = H1FS6_clean
  )


################################################################################
# 5. Documentation and quality checks
################################################################################

# Documentation table
measure_documentation <- tibble(
  variable_name = c(
    "cesd_full_score",
    "cesd_brief_score",
    "cesd_single_depressed_ord"
  ),
  source_items = c(
    "H1FS1-H1FS19, with H1FS4, H1FS8, H1FS11, and H1FS15 reverse-coded",
    "H1FS6, H1FS16, H1FS3, H1FS13",
    "H1FS6"
  ),
  scoring_rule = c(
    "Clean all items; reverse-code positive affect items as 3 - item; sum all 19 items if complete",
    "Clean all items; sum the 4 screener items if complete",
    "Clean H1FS6 and keep as ordinal 0 to 3"
  ),
  valid_range = c(
    "0 to 57",
    "0 to 12",
    "0 to 3"
  ),
  note = c(
    "Binary version should be created later after analytic sample is locked",
    "Binary version should be created later after analytic sample is locked",
    "Binary version should be created later after analytic sample is locked"
  )
)

# Quality-check table
quality_checks <- tibble(
  variable_name = c(
    "cesd_full_score",
    "cesd_brief_score",
    "cesd_single_depressed_ord"
  ),
  n_total = nrow(addhealth_measures),
  n_nonmissing = c(
    sum(!is.na(addhealth_measures$cesd_full_score)),
    sum(!is.na(addhealth_measures$cesd_brief_score)),
    sum(!is.na(addhealth_measures$cesd_single_depressed_ord))
  ),
  n_missing = c(
    sum(is.na(addhealth_measures$cesd_full_score)),
    sum(is.na(addhealth_measures$cesd_brief_score)),
    sum(is.na(addhealth_measures$cesd_single_depressed_ord))
  ),
  min = c(
    min(addhealth_measures$cesd_full_score, na.rm = TRUE),
    min(addhealth_measures$cesd_brief_score, na.rm = TRUE),
    min(addhealth_measures$cesd_single_depressed_ord, na.rm = TRUE)
  ),
  max = c(
    max(addhealth_measures$cesd_full_score, na.rm = TRUE),
    max(addhealth_measures$cesd_brief_score, na.rm = TRUE),
    max(addhealth_measures$cesd_single_depressed_ord, na.rm = TRUE)
  ),
  mean = c(
    mean(addhealth_measures$cesd_full_score, na.rm = TRUE),
    mean(addhealth_measures$cesd_brief_score, na.rm = TRUE),
    mean(addhealth_measures$cesd_single_depressed_ord, na.rm = TRUE)
  ),
  sd = c(
    sd(addhealth_measures$cesd_full_score, na.rm = TRUE),
    sd(addhealth_measures$cesd_brief_score, na.rm = TRUE),
    sd(addhealth_measures$cesd_single_depressed_ord, na.rm = TRUE)
  )
)

# Reverse-code spot-check
reverse_code_spotcheck <- addhealth_measures %>%
  select(
    H1FS4, H1FS4_clean, H1FS4_rev,
    H1FS8, H1FS8_clean, H1FS8_rev,
    H1FS11, H1FS11_clean, H1FS11_rev,
    H1FS15, H1FS15_clean, H1FS15_rev
  ) %>%
  slice_head(n = 10)


################################################################################
# 6. Save outputs
################################################################################

# Save constructed dataset
saveRDS(
  addhealth_measures,
  "data/addhealth_depression_measures.rds"
)

# Save documentation and checks as one Excel workbook
write_xlsx(
  list(
    measure_documentation = measure_documentation,
    quality_checks = quality_checks,
    reverse_code_spotcheck = reverse_code_spotcheck
  ),
  "outputs/01_construct_measures_outputs.xlsx"
)


################################################################################
# 7. Print checks to console
################################################################################

quality_checks
reverse_code_spotcheck