################################################################################
# Script: 02_build_sample.R
# Purpose: Prepare final analytic sample for the Add Health P1 project
################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)

# Read the dataset created in 01_construct_measures_Rophence.R
addhealth_measures <- readRDS("data/addhealth_depression_measures.rds")

# Add a row ID so every respondent can be tracked
addhealth_measures <- addhealth_measures %>%
  mutate(respondent_row_id = row_number())


################################################################################
# 1. Define item lists
################################################################################

# Full modified CES-D items after cleaning/reverse-coding
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

# Brief screener items
brief_score_items <- c(
  "H1FS6_clean",
  "H1FS16_clean",
  "H1FS3_clean",
  "H1FS13_clean"
)

# Household roster relationship variables
roster_relation_items <- paste0("H1HR3", LETTERS[1:20])
roster_relation_code_items <- paste0(roster_relation_items, "_code")

# School connectedness items
school_connectedness_items <- c(
  "H1ED19",
  "H1ED20",
  "H1ED22",
  "H1ED24"
)


################################################################################
# 2. Helper cleaning functions
################################################################################

# Extract the actual Add Health numeric code from factor-labelled variables.
# Example: "(95) (95) 1995" becomes 95.
get_code <- function(x) {
  readr::parse_number(as.character(x))
}

clean_0_1 <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% c(0, 1) ~ x_num,
    TRUE              ~ NA_real_
  )
}

clean_parent_education <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% 1:10 ~ x_num,
    TRUE           ~ NA_real_
  )
}

clean_month <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% 1:12 ~ x_num,
    TRUE           ~ NA_real_
  )
}

clean_birth_year <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% c(96, 97, 98, 99, 996, 997, 998, 999) ~ NA_real_,
    x_num >= 1900 & x_num <= 2100                    ~ x_num,
    x_num >= 0 & x_num <= 95                         ~ 1900 + x_num,
    TRUE                                             ~ NA_real_
  )
}

clean_interview_year <- function(x) {
  x_num <- get_code(x)
  
  case_when(
    x_num %in% c(996, 997, 998, 999) ~ NA_real_,
    x_num >= 1900 & x_num <= 2100    ~ x_num,
    x_num >= 0 & x_num <= 99         ~ 1900 + x_num,
    TRUE                             ~ NA_real_
  )
}


################################################################################
# 3. Construct predictors and covariate
################################################################################

addhealth_vars <- addhealth_measures %>%
  
  ###########################################################################
# Gender, race source variables, parent education source variables, age parts
###########################################################################

mutate(
  gender = case_when(
    get_code(BIO_SEX) == 1 ~ "Male",
    get_code(BIO_SEX) == 2 ~ "Female",
    TRUE                  ~ NA_character_
  ),
  
  gender = factor(
    gender,
    levels = c("Male", "Female")
  ),
  
  hispanic_origin = clean_0_1(H1GI4),
  
  race_white = clean_0_1(H1GI6A),
  race_black = clean_0_1(H1GI6B),
  race_aian  = clean_0_1(H1GI6C),
  race_asian = clean_0_1(H1GI6D),
  race_other = clean_0_1(H1GI6E),
  
  H1RM1_clean = clean_parent_education(H1RM1),
  H1RF1_clean = clean_parent_education(H1RF1),
  
  H1PR4_code = get_code(H1PR4),
  
  interview_month = clean_month(IMONTH),
  interview_year  = clean_interview_year(IYEAR),
  
  birth_month = clean_month(H1GI1M),
  birth_year  = clean_birth_year(H1GI1Y)
) %>%
  
  ###########################################################################
# Race/ethnicity
###########################################################################

mutate(
  race_count_all = rowSums(
    select(
      .,
      race_white,
      race_black,
      race_aian,
      race_asian,
      race_other
    ) == 1,
    na.rm = TRUE
  ),
  
  race_count_no_aian = rowSums(
    select(
      .,
      race_white,
      race_black,
      race_asian,
      race_other
    ) == 1,
    na.rm = TRUE
  ),
  
  non_hispanic_or_missing = is.na(hispanic_origin) | hispanic_origin == 0,
  
  race_eth = case_when(
    hispanic_origin == 1 ~ "Hispanic",
    
    non_hispanic_or_missing & race_aian == 1 ~ "NH-AI/AN",
    
    non_hispanic_or_missing & race_count_all == 0 ~ NA_character_,
    
    non_hispanic_or_missing & race_count_no_aian == 1 & race_white == 1 ~
      "NH-White",
    
    non_hispanic_or_missing & race_count_no_aian == 1 & race_black == 1 ~
      "NH-Black",
    
    non_hispanic_or_missing & race_count_no_aian == 1 & race_asian == 1 ~
      "NH-Asian",
    
    non_hispanic_or_missing & race_count_no_aian == 1 & race_other == 1 ~
      "NH-Multiracial",
    
    non_hispanic_or_missing & race_count_no_aian >= 2 ~
      "NH-Multiracial",
    
    TRUE ~ NA_character_
  ),
  
  race_eth = factor(
    race_eth,
    levels = c(
      "NH-White",
      "NH-Black",
      "Hispanic",
      "NH-AI/AN",
      "NH-Asian",
      "NH-Multiracial"
    )
  )
) %>%
  
###########################################################################
# Highest resident parent education
###########################################################################

mutate(
  # Recode each parent's education into an ordered attainment category
  # before selecting the higher level. The original Add Health codes are
  # not numerically ordered by educational attainment.
  mother_education_n = case_when(
    H1RM1_clean %in% c(1, 2, 10) ~ 1,
    H1RM1_clean %in% c(3, 4, 5)  ~ 2,
    H1RM1_clean %in% c(6, 7)     ~ 3,
    H1RM1_clean %in% c(8, 9)     ~ 4,
    TRUE                         ~ NA_real_
  ),
  
  father_education_n = case_when(
    H1RF1_clean %in% c(1, 2, 10) ~ 1,
    H1RF1_clean %in% c(3, 4, 5)  ~ 2,
    H1RF1_clean %in% c(6, 7)     ~ 3,
    H1RF1_clean %in% c(8, 9)     ~ 4,
    TRUE                         ~ NA_real_
  ),
  
  # Use the higher educational-attainment category when both are available.
  # When only one parent has valid education data, use that parent's value.
  resident_parent_education_n = case_when(
    !is.na(mother_education_n) & !is.na(father_education_n) ~
      pmax(mother_education_n, father_education_n),
    
    !is.na(mother_education_n) ~ mother_education_n,
    
    !is.na(father_education_n) ~ father_education_n,
    
    TRUE ~ NA_real_
  ),
  
  resident_parent_education = factor(
    resident_parent_education_n,
    levels = 1:4,
    labels = c(
      "Less than high school",
      "HS/GED/vocational instead",
      "Some college/vocational after",
      "College or more"
    )
  )
) %>%
  
  ###########################################################################
# Family structure from household roster
###########################################################################

mutate(
  across(
    all_of(roster_relation_items),
    get_code,
    .names = "{.col}_code"
  )
) %>%
  mutate(
    has_resident_father = rowSums(
      select(., all_of(roster_relation_code_items)) == 11,
      na.rm = TRUE
    ) > 0,
    
    has_resident_mother = rowSums(
      select(., all_of(roster_relation_code_items)) == 14,
      na.rm = TRUE
    ) > 0,
    
    family_structure = case_when(
      has_resident_mother & has_resident_father   ~ "Two parents",
      has_resident_mother & !has_resident_father  ~ "Mother only",
      !has_resident_mother & has_resident_father  ~ "Father only",
      !has_resident_mother & !has_resident_father ~ "Neither parent",
      TRUE ~ NA_character_
    ),
    
    family_structure = factor(
      family_structure,
      levels = c(
        "Two parents",
        "Mother only",
        "Father only",
        "Neither parent"
      )
    )
  ) %>%
  
  ###########################################################################
# School connectedness
###########################################################################

mutate(
  across(
    all_of(school_connectedness_items),
    ~ {
      x_num <- get_code(.x)
      
      case_when(
        x_num %in% 1:5        ~ x_num,
        x_num %in% c(6, 7, 8) ~ NA_real_,
        TRUE                  ~ NA_real_
      )
    },
    .names = "{.col}_clean"
  )
) %>%
  mutate(
    school_connectedness = rowMeans(
      select(
        .,
        H1ED19_clean,
        H1ED20_clean,
        H1ED22_clean,
        H1ED24_clean
      ),
      na.rm = TRUE
    ),
    
    school_connectedness = if_else(
      is.nan(school_connectedness),
      NA_real_,
      school_connectedness
    )
  ) %>%
  
  ###########################################################################
# Peer social support and age
###########################################################################

mutate(
  peer_support = case_when(
    H1PR4_code %in% 1:5          ~ H1PR4_code,
    H1PR4_code %in% c(6, 96, 98) ~ NA_real_,
    TRUE                         ~ NA_real_
  ),
  
  age = interview_year - birth_year +
    ((interview_month - birth_month) / 12),
  
  age = if_else(
    age >= 10 & age <= 25,
    age,
    NA_real_
  )
)


################################################################################
# 4. Age check before filtering
################################################################################

age_check <- addhealth_vars %>%
  summarise(
    n_total = n(),
    n_missing_interview_month = sum(is.na(interview_month)),
    n_missing_interview_year  = sum(is.na(interview_year)),
    n_missing_birth_month     = sum(is.na(birth_month)),
    n_missing_birth_year      = sum(is.na(birth_year)),
    n_missing_age             = sum(is.na(age)),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE)
  )


################################################################################
# 5. Define variables required for the analytic sample
################################################################################

outcome_vars <- c(
  "cesd_full_score",
  "cesd_brief_score",
  "cesd_single_depressed_ord"
)

predictor_covariate_vars <- c(
  "gender",
  "race_eth",
  "resident_parent_education",
  "family_structure",
  "school_connectedness",
  "peer_support",
  "age"
)


################################################################################
# 6. Build sample flow documentation
################################################################################

add_sample_flow_row <- function(sample_flow, step_number, restriction,
                                before_n, after_n, original_n) {
  
  bind_rows(
    sample_flow,
    tibble(
      step_number = step_number,
      restriction = restriction,
      n_before = before_n,
      n_after = after_n,
      n_removed_at_step = before_n - after_n,
      percent_removed_at_step = if_else(
        before_n > 0,
        round(100 * (before_n - after_n) / before_n, 2),
        NA_real_
      ),
      percent_original_remaining = round(100 * after_n / original_n, 2),
      percent_original_removed_total = round(
        100 * (original_n - after_n) / original_n,
        2
      )
    )
  )
}

original_n <- nrow(addhealth_vars)

sample_flow <- tibble(
  step_number = 0,
  restriction = "Original dataset after 01_construct_measures_Rophence.R",
  n_before = original_n,
  n_after = original_n,
  n_removed_at_step = 0,
  percent_removed_at_step = 0,
  percent_original_remaining = 100,
  percent_original_removed_total = 0
)

current_data <- addhealth_vars


# Step 1: complete gender
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(gender))

sample_flow <- add_sample_flow_row(
  sample_flow,
  1,
  "Complete gender: BIO_SEX coded male or female",
  before_n,
  nrow(current_data),
  original_n
)


# Step 2: complete race/ethnicity
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(race_eth))

sample_flow <- add_sample_flow_row(
  sample_flow,
  2,
  "Complete constructed race/ethnicity",
  before_n,
  nrow(current_data),
  original_n
)


# Step 3: complete family structure
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(family_structure))

sample_flow <- add_sample_flow_row(
  sample_flow,
  3,
  "Complete household-roster family structure",
  before_n,
  nrow(current_data),
  original_n
)


# Step 4: complete resident parent education
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(resident_parent_education))

sample_flow <- add_sample_flow_row(
  sample_flow,
  4,
  "Complete highest resident parent education",
  before_n,
  nrow(current_data),
  original_n
)


# Step 5: complete school connectedness
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(school_connectedness))

sample_flow <- add_sample_flow_row(
  sample_flow,
  5,
  "Complete school connectedness score",
  before_n,
  nrow(current_data),
  original_n
)


# Step 6: complete peer social support
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(peer_support))

sample_flow <- add_sample_flow_row(
  sample_flow,
  6,
  "Complete peer social support: H1PR4",
  before_n,
  nrow(current_data),
  original_n
)


# Step 7: complete age
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(age))

sample_flow <- add_sample_flow_row(
  sample_flow,
  7,
  "Complete computed age",
  before_n,
  nrow(current_data),
  original_n
)


# Step 8: complete full modified CES-D score
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(cesd_full_score))

sample_flow <- add_sample_flow_row(
  sample_flow,
  8,
  "Complete full 19-item modified CES-D score",
  before_n,
  nrow(current_data),
  original_n
)


# Step 9: complete brief screener score
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(cesd_brief_score))

sample_flow <- add_sample_flow_row(
  sample_flow,
  9,
  "Complete four-item brief screener score",
  before_n,
  nrow(current_data),
  original_n
)


# Step 10: complete single depressed mood item
before_n <- nrow(current_data)

current_data <- current_data %>%
  filter(!is.na(cesd_single_depressed_ord))

sample_flow <- add_sample_flow_row(
  sample_flow,
  10,
  "Complete single depressed mood item: H1FS6",
  before_n,
  nrow(current_data),
  original_n
)


################################################################################
# 7. Final analytic dataset
################################################################################

analytic_sample <- current_data %>%
  select(
    respondent_row_id,
    any_of(c("AID")),
    all_of(outcome_vars),
    all_of(predictor_covariate_vars),
    all_of(full_score_items)
  )


################################################################################
# 8. Quality checks
################################################################################

# Check 1: each outcome has the same final sample size
outcome_sample_check <- tibble(
  outcome_variable = outcome_vars,
  final_sample_size = nrow(analytic_sample),
  n_nonmissing = sapply(
    analytic_sample[outcome_vars],
    function(x) sum(!is.na(x))
  ),
  n_missing = sapply(
    analytic_sample[outcome_vars],
    function(x) sum(is.na(x))
  )
) %>%
  mutate(
    sample_identical_to_final_n = n_nonmissing == final_sample_size
  )


# Check 2: no missing values remain in predictors or covariates
predictor_missing_check <- analytic_sample %>%
  summarise(
    across(
      all_of(predictor_covariate_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable_name",
    values_to = "n_missing"
  ) %>%
  mutate(
    no_missing_values = n_missing == 0
  )


# Check 3: no missing values remain in outcomes
outcome_missing_check <- analytic_sample %>%
  summarise(
    across(
      all_of(outcome_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable_name",
    values_to = "n_missing"
  ) %>%
  mutate(
    no_missing_values = n_missing == 0
  )


# Check 4: 25% sample-loss decision rule
sample_loss_check <- tibble(
  original_n = original_n,
  final_n = nrow(analytic_sample),
  n_removed_total = original_n - nrow(analytic_sample),
  percent_removed_total = round(
    100 * (original_n - nrow(analytic_sample)) / original_n,
    2
  ),
  exceeds_25_percent_loss = percent_removed_total > 25,
  decision_rule_message = if_else(
    exceeds_25_percent_loss,
    "Flag for team: complete-case restriction removed more than 25% of original sample.",
    "Complete-case restriction did not exceed 25% sample-loss rule."
  )
)


# Check 5: final sample size to use in every later analysis
final_sample_size_check <- tibble(
  analytic_dataset = "data/addhealth_analytic_sample.rds",
  final_n = nrow(analytic_sample),
  note = "Use this same analytic dataset for all later descriptive, subgroup, regression, and classification analyses."
)


################################################################################
# 9. Save final dataset and documentation
################################################################################

# Save the final analytic dataset
saveRDS(
  analytic_sample,
  "data/addhealth_analytic_sample.rds"
)


# Save sample flow and quality checks in one Excel workbook
write_xlsx(
  list(
    age_check = age_check,
    sample_flow = sample_flow,
    sample_loss_check = sample_loss_check,
    outcome_sample_check = outcome_sample_check,
    predictor_missing_check = predictor_missing_check,
    outcome_missing_check = outcome_missing_check,
    final_sample_size_check = final_sample_size_check
  ),
  "outputs/02_build_sample_outputs_RO.xlsx"
)


################################################################################
# 10. Print key checks to console
################################################################################

age_check
sample_flow
sample_loss_check
outcome_sample_check
predictor_missing_check
outcome_missing_check
final_sample_size_check