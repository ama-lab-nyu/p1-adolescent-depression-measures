# 01_construct_measures.R
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
rawdata <- readr::read_tsv("data/21600-0001-Data.tsv")
# view(rawdata)
dim(rawdata) #6504 rows x 2794 columns
names(rawdata)[1:20]

audit <- read_excel("docs/p1_audit_spreadsheet_v1.xlsx")
vars_keep <- audit$variable_name
vars_keep <- intersect(vars_keep, names(rawdata))

datapre <- rawdata %>% select(all_of(vars_keep))
dim(datapre) #6504 rows x 57 columns
names(datapre)

############################################################
# Data cleaning
############################################################
datapre <- datapre %>%
  
  # CES-D items: keep 0-3
  mutate(
    across(
      H1FS1:H1FS19,
      ~ ifelse(.x %in% 0:3, .x, NA)
    )
  )

# QC after data cleaning
# lapply(
#   datapre %>% select(H1FS1:H1FS19),
#   table,
#   useNA = "ifany"
# )

############################################################
# CES-D Measures
############################################################
# CES-D item dataset
cesd <- datapre %>% 
  select(H1FS1:H1FS19)

# Reverse-code positive affect items
cesd <- cesd %>%
  mutate(
    H1FS4_rev  = 3 - H1FS4,
    H1FS8_rev  = 3 - H1FS8,
    H1FS11_rev = 3 - H1FS11,
    H1FS15_rev = 3 - H1FS15
  )

# QC: spot-check original and reverse-coded values for first 10 obs
cesd %>%
  select(
    H1FS4, H1FS4_rev,
    H1FS8, H1FS8_rev,
    H1FS11, H1FS11_rev,
    H1FS15, H1FS15_rev
  ) %>%
  head(10)

# 19-item full modified CES-D scale
cesd_full_score <- cesd %>%
  mutate(
    cesd_full_score = rowSums(across(H1FS1:H1FS19), na.rm = FALSE)
  ) %>%
  select(cesd_full_score)

# 4-item Brief Screener
brief_screener_score <- cesd %>%
  mutate(
    brief_screener_score = H1FS6 + H1FS16 + H1FS3 + H1FS13
  ) %>%
  select(brief_screener_score)

# single-item Indicator
single_item_depression <- cesd %>%
  mutate(
    single_item_depression = H1FS6
  ) %>%
  select(single_item_depression)

# Add to datapre
datapre <- datapre %>% 
  bind_cols(
    H1FS4_rev  = cesd$H1FS4_rev,
    H1FS8_rev  = cesd$H1FS8_rev,
    H1FS11_rev = cesd$H1FS11_rev,
    H1FS15_rev = cesd$H1FS15_rev,
    cesd_full_score,
    brief_screener_score,
    single_item_depression
  )

# QC
summary(cesd_full_score$cesd_full_score)
summary(brief_screener_score$brief_screener_score)
table(single_item_depression$single_item_depression, useNA = "ifany")

range(cesd_full_score$cesd_full_score, na.rm = TRUE)               # Expected: 0-57
range(brief_screener_score$brief_screener_score, na.rm = TRUE)     # Expected: 0-12
range(single_item_depression$single_item_depression, na.rm = TRUE) # Expected: 0-3

############################################################
# Output P1 Dataset
############################################################
write.xlsx(
  datapre,
  file = "outputs/01_construct_measures_outputs.xlsx",
  rowNames = FALSE
)