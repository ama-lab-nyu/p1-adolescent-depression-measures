################################################################################
# Script: 03_descriptives.R
# Purpose: Produce descriptive statistics, distribution tables, reliability
#          estimates, and Figure 1 for the Add Health P1 project
################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)
library(psych)
library(ggplot2)


################################################################################
# 1. Read analytic dataset
################################################################################

# This dataset was created in 02_build_sample.R.
# It should be the one used for every later analysis.
analytic_sample <- readRDS("data/addhealth_analytic_sample.rds")


################################################################################
# 2. Define outcome variables and item lists
################################################################################

outcome_vars <- c(
  "cesd_full_score",
  "cesd_brief_score",
  "cesd_single_depressed_ord"
)

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

brief_score_items <- c(
  "H1FS6_clean",
  "H1FS16_clean",
  "H1FS3_clean",
  "H1FS13_clean"
)


################################################################################
# 3. Basic sample-size check
################################################################################

sample_size_check <- tibble(
  analytic_n = nrow(analytic_sample),
  n_missing_full_score = sum(is.na(analytic_sample$cesd_full_score)),
  n_missing_brief_score = sum(is.na(analytic_sample$cesd_brief_score)),
  n_missing_single_item = sum(is.na(analytic_sample$cesd_single_depressed_ord))
)


################################################################################
# 4. Summary statistics for the three outcome measures
################################################################################

outcome_summary <- analytic_sample %>%
  summarise(
    full_n = sum(!is.na(cesd_full_score)),
    full_mean = mean(cesd_full_score, na.rm = TRUE),
    full_sd = sd(cesd_full_score, na.rm = TRUE),
    full_min = min(cesd_full_score, na.rm = TRUE),
    full_max = max(cesd_full_score, na.rm = TRUE),
    
    brief_n = sum(!is.na(cesd_brief_score)),
    brief_mean = mean(cesd_brief_score, na.rm = TRUE),
    brief_sd = sd(cesd_brief_score, na.rm = TRUE),
    brief_min = min(cesd_brief_score, na.rm = TRUE),
    brief_max = max(cesd_brief_score, na.rm = TRUE),
    
    single_n = sum(!is.na(cesd_single_depressed_ord)),
    single_mean = mean(cesd_single_depressed_ord, na.rm = TRUE),
    single_sd = sd(cesd_single_depressed_ord, na.rm = TRUE),
    single_min = min(cesd_single_depressed_ord, na.rm = TRUE),
    single_max = max(cesd_single_depressed_ord, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  separate(
    statistic,
    into = c("measure", "statistic"),
    sep = "_",
    extra = "merge"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(
    measure = case_when(
      measure == "full" ~ "Full 19-item modified CES-D",
      measure == "brief" ~ "Four-item brief screener",
      measure == "single" ~ "Single depressed-mood item",
      TRUE ~ measure
    ),
    possible_range = case_when(
      measure == "Full 19-item modified CES-D" ~ "0 to 57",
      measure == "Four-item brief screener" ~ "0 to 12",
      measure == "Single depressed-mood item" ~ "0 to 3",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    measure,
    n,
    mean,
    sd,
    min,
    max,
    possible_range
  )


################################################################################
# 5. Score distribution tables
################################################################################

# These tables show how many respondents fall at each observed score.
# This is useful for seeing floor effects, especially for the single item.

full_score_distribution <- analytic_sample %>%
  count(cesd_full_score, name = "n") %>%
  mutate(
    percent = round(100 * n / sum(n), 2),
    measure = "Full 19-item modified CES-D"
  ) %>%
  rename(score = cesd_full_score) %>%
  select(measure, score, n, percent)

brief_score_distribution <- analytic_sample %>%
  count(cesd_brief_score, name = "n") %>%
  mutate(
    percent = round(100 * n / sum(n), 2),
    measure = "Four-item brief screener"
  ) %>%
  rename(score = cesd_brief_score) %>%
  select(measure, score, n, percent)

single_item_distribution <- analytic_sample %>%
  count(cesd_single_depressed_ord, name = "n") %>%
  mutate(
    percent = round(100 * n / sum(n), 2),
    measure = "Single depressed-mood item"
  ) %>%
  rename(score = cesd_single_depressed_ord) %>%
  select(measure, score, n, percent)

score_distribution_all <- bind_rows(
  full_score_distribution,
  brief_score_distribution,
  single_item_distribution
)


################################################################################
# 6. Cronbach's alpha for full scale and brief screener
################################################################################

# psych::alpha() expects numeric item columns.
full_alpha_object <- psych::alpha(
  analytic_sample %>%
    select(all_of(full_score_items)),
  check.keys = FALSE
)

brief_alpha_object <- psych::alpha(
  analytic_sample %>%
    select(all_of(brief_score_items)),
  check.keys = FALSE
)

# Overall alpha values
reliability_summary <- tibble(
  measure = c(
    "Full 19-item modified CES-D",
    "Four-item brief screener"
  ),
  number_of_items = c(
    length(full_score_items),
    length(brief_score_items)
  ),
  cronbach_alpha = c(
    full_alpha_object$total$raw_alpha,
    brief_alpha_object$total$raw_alpha
  ),
  decision_rule = c(
    "If alpha < 0.70, investigate before proceeding",
    "If alpha < 0.60, document as limitation but proceed"
  ),
  decision_flag = c(
    if_else(
      full_alpha_object$total$raw_alpha < 0.70,
      "FLAG: full-scale alpha below 0.70",
      "OK: full-scale alpha is at least 0.70"
    ),
    if_else(
      brief_alpha_object$total$raw_alpha < 0.60,
      "NOTE: screener alpha below 0.60; document as limitation",
      "OK: screener alpha is at least 0.60"
    )
  )
)

# Alpha if each item is removed
full_alpha_drop <- full_alpha_object$alpha.drop %>%
  as.data.frame() %>%
  rownames_to_column("item") %>%
  as_tibble() %>%
  transmute(
    measure = "Full 19-item modified CES-D",
    item = item,
    overall_alpha = full_alpha_object$total$raw_alpha,
    alpha_if_item_deleted = raw_alpha,
    alpha_change_if_deleted = alpha_if_item_deleted - overall_alpha,
    item_flag = if_else(
      alpha_change_if_deleted > 0.02,
      "Alpha increases by more than 0.02 if removed; review item",
      "No large alpha increase if removed"
    )
  )

brief_alpha_drop <- brief_alpha_object$alpha.drop %>%
  as.data.frame() %>%
  rownames_to_column("item") %>%
  as_tibble() %>%
  transmute(
    measure = "Four-item brief screener",
    item = item,
    overall_alpha = brief_alpha_object$total$raw_alpha,
    alpha_if_item_deleted = raw_alpha,
    alpha_change_if_deleted = alpha_if_item_deleted - overall_alpha,
    item_flag = if_else(
      alpha_change_if_deleted > 0.02,
      "Alpha increases by more than 0.02 if removed; review item",
      "No large alpha increase if removed"
    )
  )

alpha_if_item_deleted <- bind_rows(
  full_alpha_drop,
  brief_alpha_drop
)


################################################################################
# 7. Figure 1: Distribution of depressive symptom measures
################################################################################

# Put the three outcomes into long format for plotting.
plot_data <- analytic_sample %>%
  select(
    cesd_full_score,
    cesd_brief_score,
    cesd_single_depressed_ord
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "measure",
    values_to = "score"
  ) %>%
  mutate(
    measure = case_when(
      measure == "cesd_full_score" ~ "Full 19-item modified CES-D",
      measure == "cesd_brief_score" ~ "Four-item brief screener",
      measure == "cesd_single_depressed_ord" ~ "Single depressed-mood item",
      TRUE ~ measure
    ),
    measure = factor(
      measure,
      levels = c(
        "Full 19-item modified CES-D",
        "Four-item brief screener",
        "Single depressed-mood item"
      )
    )
  )

# We use separate panels because the score ranges differ:
# full scale = 0 to 57, brief screener = 0 to 12, single item = 0 to 3.
figure1_distribution <- ggplot(
  plot_data,
  aes(x = score)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    closed = "right"
  ) +
  facet_wrap(
    ~ measure,
    scales = "free_x",
    nrow = 1
  ) +
  labs(
    title = "Distribution of Depressive Symptom Measures",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; each panel uses the same respondents"
    ),
    x = "Depressive symptom score",
    y = "Number of respondents",
    caption = "Full scale range: 0-57; brief screener range: 0-12; single item range: 0-3."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Save Figure 1 as 300 DPI PNG
ggsave(
  filename = "outputs/figures/figure1_distribution_depressive_symptom_measures.png",
  plot = figure1_distribution,
  width = 12,
  height = 5,
  dpi = 300
)


################################################################################
# 8. Save tables to Excel workbook
################################################################################

write_xlsx(
  list(
    sample_size_check = sample_size_check,
    outcome_summary = outcome_summary,
    score_distribution_all = score_distribution_all,
    full_score_distribution = full_score_distribution,
    brief_score_distribution = brief_score_distribution,
    single_item_distribution = single_item_distribution,
    reliability_summary = reliability_summary,
    alpha_if_item_deleted = alpha_if_item_deleted
  ),
  "outputs/03_descriptives_outputs.xlsx"
)


################################################################################
# 9. Print key outputs to console
################################################################################

sample_size_check
outcome_summary
reliability_summary
alpha_if_item_deleted
figure1_distribution


################################################################################
# 10. Step 4: Apply 80th-percentile classification thresholds
################################################################################

# The binary elevated-distress variables are created only after the analytic
# sample is locked. This ensures that all three measures use the same respondents.
#
# Decision rule:
# - Use the 80th percentile of each measure's distribution in the locked analytic
#   sample.
# - Use an empirical percentile so the cutoff is an observed score.
# - Include all respondents tied at the cutoff in the elevated-distress group.

get_empirical_80th_percentile <- function(x) {
  as.numeric(
    quantile(
      x,
      probs = 0.80,
      na.rm = TRUE,
      type = 1,
      names = FALSE
    )
  )
}

# Calculate the locked 80th-percentile cutoffs
full_cutoff <- get_empirical_80th_percentile(
  analytic_sample$cesd_full_score
)

brief_cutoff <- get_empirical_80th_percentile(
  analytic_sample$cesd_brief_score
)

single_cutoff <- get_empirical_80th_percentile(
  analytic_sample$cesd_single_depressed_ord
)


# Create binary elevated-distress variables
# 1 = elevated distress
# 0 = not elevated distress
analytic_sample_thresholded <- analytic_sample %>%
  mutate(
    cesd_full_elevated = if_else(
      cesd_full_score >= full_cutoff,
      1L,
      0L
    ),
    
    cesd_brief_elevated = if_else(
      cesd_brief_score >= brief_cutoff,
      1L,
      0L
    ),
    
    cesd_single_depressed_elevated = if_else(
      cesd_single_depressed_ord >= single_cutoff,
      1L,
      0L
    ),
    
    n_elevated_measures =
      cesd_full_elevated +
      cesd_brief_elevated +
      cesd_single_depressed_elevated,
    
    classification_overlap = case_when(
      n_elevated_measures == 0 ~ "Not elevated on any measure",
      n_elevated_measures == 1 ~ "Elevated on one measure",
      n_elevated_measures == 2 ~ "Elevated on two measures",
      n_elevated_measures == 3 ~ "Elevated on all three measures",
      TRUE ~ NA_character_
    ),
    
    classification_overlap = factor(
      classification_overlap,
      levels = c(
        "Not elevated on any measure",
        "Elevated on one measure",
        "Elevated on two measures",
        "Elevated on all three measures"
      )
    )
  )


################################################################################
# 11. Threshold documentation
################################################################################

make_threshold_row <- function(data, measure_label, score_variable,
                               binary_variable, cutoff_score) {
  
  tibble(
    measure = measure_label,
    score_variable = score_variable,
    binary_variable = binary_variable,
    percentile_rule = "80th percentile within locked analytic sample",
    quantile_rule = "Empirical quantile using type = 1",
    cutoff_score = cutoff_score,
    elevated_definition = paste0(score_variable, " >= ", cutoff_score),
    n_total = nrow(data),
    n_at_cutoff = sum(data[[score_variable]] == cutoff_score),
    n_elevated = sum(data[[binary_variable]] == 1),
    percent_elevated = round(
      100 * sum(data[[binary_variable]] == 1) / nrow(data),
      2
    ),
    tie_rule = "All respondents with scores equal to the cutoff are included as elevated."
  )
}

threshold_summary <- bind_rows(
  make_threshold_row(
    analytic_sample_thresholded,
    "Full 19-item modified CES-D",
    "cesd_full_score",
    "cesd_full_elevated",
    full_cutoff
  ),
  
  make_threshold_row(
    analytic_sample_thresholded,
    "Four-item brief screener",
    "cesd_brief_score",
    "cesd_brief_elevated",
    brief_cutoff
  ),
  
  make_threshold_row(
    analytic_sample_thresholded,
    "Single depressed-mood item",
    "cesd_single_depressed_ord",
    "cesd_single_depressed_elevated",
    single_cutoff
  )
)


################################################################################
# 12. Classification prevalence and overlap tables
################################################################################

classification_prevalence <- analytic_sample_thresholded %>%
  summarise(
    full_n_elevated = sum(cesd_full_elevated == 1),
    full_percent_elevated = round(100 * mean(cesd_full_elevated == 1), 2),
    
    brief_n_elevated = sum(cesd_brief_elevated == 1),
    brief_percent_elevated = round(100 * mean(cesd_brief_elevated == 1), 2),
    
    single_n_elevated = sum(cesd_single_depressed_elevated == 1),
    single_percent_elevated = round(
      100 * mean(cesd_single_depressed_elevated == 1),
      2
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  separate(
    statistic,
    into = c("measure", "statistic"),
    sep = "_",
    extra = "merge"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(
    measure = case_when(
      measure == "full" ~ "Full 19-item modified CES-D",
      measure == "brief" ~ "Four-item brief screener",
      measure == "single" ~ "Single depressed-mood item",
      TRUE ~ measure
    )
  ) %>%
  rename(
    n_elevated = n_elevated,
    percent_elevated = percent_elevated
  )

classification_overlap_table <- analytic_sample_thresholded %>%
  count(classification_overlap, name = "n") %>%
  mutate(
    percent = round(100 * n / sum(n), 2)
  )


################################################################################
# 13. Quality checks for binary classification variables
################################################################################

binary_vars <- c(
  "cesd_full_elevated",
  "cesd_brief_elevated",
  "cesd_single_depressed_elevated"
)

binary_missing_check <- analytic_sample_thresholded %>%
  summarise(
    across(
      all_of(binary_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "binary_variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    no_missing_values = n_missing == 0
  )

binary_value_check <- analytic_sample_thresholded %>%
  summarise(
    across(
      all_of(binary_vars),
      ~ paste(sort(unique(.)), collapse = ", ")
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "binary_variable",
    values_to = "observed_values"
  ) %>%
  mutate(
    expected_values = "0, 1",
    values_correct = observed_values == expected_values
  )

threshold_sample_size_check <- tibble(
  analytic_sample_n = nrow(analytic_sample),
  thresholded_sample_n = nrow(analytic_sample_thresholded),
  same_n = analytic_sample_n == thresholded_sample_n,
  note = "Thresholding should not add or remove respondents."
)


################################################################################
# 14. Save thresholded analytic dataset and threshold outputs
################################################################################

# Save analytic dataset with binary elevated-distress variables.
# Later scripts should read this file.
saveRDS(
  analytic_sample_thresholded,
  "data/addhealth_analytic_sample_thresholded.rds"
)

# Save threshold documentation and checks.
write_xlsx(
  list(
    threshold_summary = threshold_summary,
    classification_prevalence = classification_prevalence,
    classification_overlap_table = classification_overlap_table,
    binary_missing_check = binary_missing_check,
    binary_value_check = binary_value_check,
    threshold_sample_size_check = threshold_sample_size_check
  ),
  "outputs/03_classification_threshold_outputs.xlsx"
)


################################################################################
# 15. Print Step 4 outputs to console
################################################################################

threshold_summary
classification_prevalence
classification_overlap_table
binary_missing_check
binary_value_check
threshold_sample_size_check