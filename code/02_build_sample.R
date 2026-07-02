# 02_build_sample.R
# Date: Sys.Date()

############################################################
# Install packages (run once)
############################################################
# install.packages(c("tidyverse", "haven", "janitor", "psych", "broom", "car", "survey"))
# install.packages("readxl")
# install.packages("openxlsx")

############################################################
# Load packages
############################################################
library(tidyverse)
library(haven)
library(janitor)
library(psych)
library(broom)
library(car)
library(survey)
library(readxl)
library(openxlsx)

############################################################
# Load and inspect dataset
############################################################
datapre <- read_excel("outputs/01_construct_measures_outputs.xlsx")
dim(datapre)
names(datapre)

############################################################
# Data cleaning
############################################################
datapre <- datapre %>%
  
  # Gender: keep 1-2
  mutate(
    BIO_SEX = ifelse(BIO_SEX %in% c(1, 2),
                     BIO_SEX,
                     NA)
  ) %>%
  
  # Ethnicity
  mutate(
    H1GI4 = ifelse(H1GI4 %in% c(0, 1),
                   H1GI4,
                   NA)
  ) %>%
  
  # Race
  mutate(
    across(
      H1GI6A:H1GI6E,
      ~ ifelse(.x %in% c(0, 1), .x, NA)
    )
  ) %>%
  
  # Parental education
  mutate(
    H1RM1 = ifelse(H1RM1 %in% c(1:12),
                   H1RM1,
                   NA),
    
    H1RF1 = ifelse(H1RF1 %in% c(1:10),
                   H1RF1,
                   NA)
  ) %>%
  
  # Family structure
  mutate(
    across(
      H1HR3A:H1HR3T,
      ~ ifelse(.x %in% 1:95, .x, NA)
    )
  ) %>%
  
  # School connectedness
  mutate(
    across(
      c(H1ED19, H1ED20, H1ED22, H1ED24),
      ~ ifelse(.x %in% 1:5, .x, NA)
    )
  ) %>%
  
  # Peer social support
  mutate(
    H1PR4 = ifelse(H1PR4 %in% 1:5,
                   H1PR4,
                   NA)
  ) %>% 
  
  # Age
  mutate(
    H1GI1M = ifelse(H1GI1M %in% 1:12, H1GI1M, NA),
    H1GI1Y = ifelse(H1GI1Y >= 70 & H1GI1Y <= 90, H1GI1Y, NA),
    IMONTH = ifelse(IMONTH %in% 1:12, IMONTH, NA),
    IYEAR  = ifelse(IYEAR %in% c(94, 95), IYEAR, NA)
  )

# QC after data cleaning
# table(datapre$BIO_SEX, useNA = "ifany")
# table(datapre$H1GI4, useNA = "ifany")
# table(datapre$H1PR4, useNA = "ifany")
# table(datapre$H1GI1M, useNA = "ifany")
# table(datapre$H1GI1Y, useNA = "ifany")
# table(datapre$IMONTH, useNA = "ifany")
# table(datapre$IYEAR, useNA = "ifany")

############################################################
# Gender
############################################################
# Convert gender to factor
datapre <- datapre %>%
  mutate(
    bio_sex = factor(
      BIO_SEX,
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

# QC
table(datapre$bio_sex, useNA = "ifany")

############################################################
# Race/Ethnicity
############################################################
# Recode special codes
datapre <- datapre %>%
  mutate(
    H1GI4  = ifelse(H1GI4  %in% c(0,1), H1GI4,  NA),
    H1GI6A = ifelse(H1GI6A %in% c(0,1), H1GI6A, NA),
    H1GI6B = ifelse(H1GI6B %in% c(0,1), H1GI6B, NA),
    H1GI6C = ifelse(H1GI6C %in% c(0,1), H1GI6C, NA),
    H1GI6D = ifelse(H1GI6D %in% c(0,1), H1GI6D, NA),
    H1GI6E = ifelse(H1GI6E %in% c(0,1), H1GI6E, NA)
  )

# Create race/ethnicity variable
datapre <- datapre %>%
  mutate(
    race_eth = case_when(
      H1GI4 == 1 ~ 1,
      H1GI4 == 0 & H1GI6A == 1 ~ 2,
      H1GI4 == 0 & H1GI6B == 1 ~ 3,
      H1GI4 == 0 & H1GI6D == 1 ~ 4,
      H1GI4 == 0 & (H1GI6C == 1 | H1GI6E == 1) ~ 5,
      TRUE ~ NA_real_
    ),
    
    race_eth = factor(
      race_eth,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic Asian",
        "Non-Hispanic Other/Multiracial"
      )
    )
  )

# QC
table(datapre$race_eth, useNA = "ifany")

############################################################
# Parental Education
############################################################
# Create parental education variable
datapre <- datapre %>%
  mutate(
    edu = case_when(
      H1RM1 %in% c(1, 2, 10) ~ 1,
      H1RM1 %in% c(3, 4, 5)  ~ 2,
      H1RM1 %in% c(6, 7)     ~ 3,
      H1RM1 %in% c(8, 9)     ~ 4,
      TRUE ~ NA_real_
    ),
    
    edu = factor(
      edu,
      levels = 1:4,
      labels = c(
        "Less than high school",
        "High school or GED",
        "Some college",
        "College or higher"
      ),
      ordered = TRUE
    )
  )

# QC
table(datapre$edu, useNA = "ifany")

############################################################
# Family Structure
############################################################
# Recode special codes
# datapre <- datapre %>%
#   mutate(
#     across(
#       H1HR3A:H1HR3T,
#       ~ ifelse(.x %in% c(96, 97, 98), NA, .x)
#     )
#   )

# Identify mother and father in household roster
datapre <- datapre %>%
  mutate(
    mother_present = rowSums(
      across(H1HR3A:H1HR3T, ~ .x == 14),
      na.rm = TRUE
    ) > 0,
    
    father_present = rowSums(
      across(H1HR3A:H1HR3T, ~ .x == 11),
      na.rm = TRUE
    ) > 0
  )

# Create family structure variable
datapre <- datapre %>%
  mutate(
    fam_struct = case_when(
      mother_present & father_present ~ 1,
      mother_present & !father_present ~ 2,
      !mother_present & father_present ~ 3,
      !mother_present & !father_present ~ 4,
      TRUE ~ NA_real_
    ),
    
    fam_struct = factor(
      fam_struct,
      levels = 1:4,
      labels = c(
        "Two parents",
        "Mother only",
        "Father only",
        "Neither"
      )
    )
  )

# QC
table(datapre$fam_struct, useNA = "ifany")

# Verify parent presence flags
table(datapre$mother_present, useNA = "ifany")
table(datapre$father_present, useNA = "ifany")

############################################################
# School Connectedness
############################################################
# Compute 4-item mean
datapre <- datapre %>%
  mutate(
    school_connect = rowMeans(
      select(., H1ED19, H1ED20, H1ED22, H1ED24),
      na.rm = FALSE
    )
  )

############################################################
# Peer Social Support
############################################################
# Convert H1PR4 to factor
datapre <- datapre %>%
  mutate(
    H1PR4 = factor(
      H1PR4,
      levels = 1:5,
      labels = c(
        "Not at all",
        "Very little",
        "Somewhat",
        "Quite a bit",
        "Very much"
      ),
      ordered = TRUE
    )
  )

# QC
table(datapre$H1PR4, useNA = "ifany")

levels(datapre$H1PR4)

############################################################
# Age
############################################################
datapre <- datapre %>%
  mutate(
    Age = (IYEAR - H1GI1Y) +
      ((IMONTH - H1GI1M) / 12)
  )

# QC
summary(datapre$Age)
range(datapre$Age, na.rm = TRUE)

############################################################
# Final Cleaned Dataset
############################################################
# Final dataset (NAs incl.)
final0 <- datapre %>% 
  transmute(
    cesd_full_score,
    brief_screener_score,
    single_item_depression,
    gender = as.numeric(bio_sex),
    race_eth = as.numeric(race_eth),
    fam_struct = as.numeric(fam_struct),
    school_connect,
    peer_support = as.numeric(H1PR4),
    age = Age
  )

dim(final0)
names(final0)
head(final0)

# Final dataset (no NAs)
final <- final0 %>% 
  drop_na(
    cesd_full_score,
    brief_screener_score,
    single_item_depression,
    gender,
    race_eth,
    fam_struct,
    school_connect,
    peer_support,
    age 
  )

dim(final)
names(final)
head(final)
# view(final)

# Output final datasets (NAs incl. and no NAs)
# write.xlsx(
#   final0,
#   file = "data/p2_final_dataset_NAs.xlsx",
#   rowNames = FALSE
# )

write.xlsx(
  final,
  file = "outputs/02_build_sample_outputs.xlsx",
  rowNames = FALSE
)

############################################################
# Final Data Check
############################################################
# Sample size before complete-case restriction
nrow(final0)

# Sample size after restriction
nrow(final)

# Number excluded
nrow(final0) - nrow(final)

# Percent excluded
100 * (nrow(final0) - nrow(final)) / nrow(final0)

# Number of NAs in each variable
colSums(is.na(final0))
colSums(is.na(final))

# Missing data rate is low (<5%), therefore complete-case analysis is unlikely to substantially affect study results.