################################################################################
# Script: 04_subgroups.R
# Purpose: Produce subgroup comparisons by gender and race/ethnicity for the
#          Add Health P1 depression measurement comparison project
################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)
library(ggplot2)


################################################################################
# 1. Read thresholded analytic dataset
################################################################################

# This dataset should come from 03_descriptives.R after Step 4 thresholds
# have been applied.
analytic_sample <- readRDS("data/addhealth_analytic_sample_thresholded.rds")


################################################################################
# 2. Define continuous and binary outcome variables
################################################################################

continuous_outcomes <- c(
  "cesd_full_score",
  "cesd_brief_score",
  "cesd_single_depressed_ord"
)

binary_outcomes <- c(
  "cesd_full_elevated",
  "cesd_brief_elevated",
  "cesd_single_depressed_elevated"
)


################################################################################
# 3. Basic quality checks
################################################################################

required_vars <- c(
  "gender",
  "race_eth",
  continuous_outcomes,
  binary_outcomes
)

missing_required_vars <- setdiff(required_vars, names(analytic_sample))

if (length(missing_required_vars) > 0) {
  stop(
    paste(
      "These required variables are missing from the dataset:",
      paste(missing_required_vars, collapse = ", ")
    )
  )
}

sample_size_check <- tibble(
  analytic_n = nrow(analytic_sample),
  n_missing_gender = sum(is.na(analytic_sample$gender)),
  n_missing_race_eth = sum(is.na(analytic_sample$race_eth)),
  n_missing_full_score = sum(is.na(analytic_sample$cesd_full_score)),
  n_missing_brief_score = sum(is.na(analytic_sample$cesd_brief_score)),
  n_missing_single_item = sum(is.na(analytic_sample$cesd_single_depressed_ord)),
  n_missing_full_binary = sum(is.na(analytic_sample$cesd_full_elevated)),
  n_missing_brief_binary = sum(is.na(analytic_sample$cesd_brief_elevated)),
  n_missing_single_binary = sum(is.na(analytic_sample$cesd_single_depressed_elevated))
)


################################################################################
# 4. Helper functions for subgroup summaries
################################################################################

label_measure <- function(x) {
  case_when(
    x == "cesd_full_score" ~ "Full 19-item modified CES-D",
    x == "cesd_brief_score" ~ "Four-item brief screener",
    x == "cesd_single_depressed_ord" ~ "Single depressed-mood item",
    x == "cesd_full_elevated" ~ "Full 19-item modified CES-D",
    x == "cesd_brief_elevated" ~ "Four-item brief screener",
    x == "cesd_single_depressed_elevated" ~ "Single depressed-mood item",
    TRUE ~ x
  )
}


make_continuous_subgroup_summary <- function(data, subgroup_var, subgroup_label) {
  
  data %>%
    select(
      all_of(subgroup_var),
      all_of(continuous_outcomes)
    ) %>%
    pivot_longer(
      cols = all_of(continuous_outcomes),
      names_to = "outcome_variable",
      values_to = "score"
    ) %>%
    mutate(
      subgroup_variable = subgroup_label,
      subgroup_level = as.character(.data[[subgroup_var]]),
      measure = label_measure(outcome_variable)
    ) %>%
    group_by(
      subgroup_variable,
      subgroup_level,
      outcome_variable,
      measure
    ) %>%
    summarise(
      n = n(),
      mean_score = mean(score),
      sd_score = sd(score),
      min_score = min(score),
      max_score = max(score),
      .groups = "drop"
    ) %>%
    mutate(
      mean_score = round(mean_score, 3),
      sd_score = round(sd_score, 3)
    )
}


make_binary_subgroup_summary <- function(data, subgroup_var, subgroup_label) {
  
  data %>%
    select(
      all_of(subgroup_var),
      all_of(binary_outcomes)
    ) %>%
    pivot_longer(
      cols = all_of(binary_outcomes),
      names_to = "binary_variable",
      values_to = "elevated"
    ) %>%
    mutate(
      subgroup_variable = subgroup_label,
      subgroup_level = as.character(.data[[subgroup_var]]),
      measure = label_measure(binary_variable)
    ) %>%
    group_by(
      subgroup_variable,
      subgroup_level,
      binary_variable,
      measure
    ) %>%
    summarise(
      n = n(),
      n_elevated = sum(elevated == 1),
      prevalence_percent = 100 * mean(elevated == 1),
      .groups = "drop"
    ) %>%
    mutate(
      prevalence_percent = round(prevalence_percent, 2)
    )
}


################################################################################
# 5. Mean continuous scores by gender and race/ethnicity
################################################################################

gender_continuous_summary <- make_continuous_subgroup_summary(
  data = analytic_sample,
  subgroup_var = "gender",
  subgroup_label = "Gender"
)

race_continuous_summary <- make_continuous_subgroup_summary(
  data = analytic_sample,
  subgroup_var = "race_eth",
  subgroup_label = "Race/ethnicity"
)

continuous_subgroup_summary <- bind_rows(
  gender_continuous_summary,
  race_continuous_summary
)


################################################################################
# 6. Elevated-distress prevalence by gender and race/ethnicity
################################################################################

gender_prevalence_summary <- make_binary_subgroup_summary(
  data = analytic_sample,
  subgroup_var = "gender",
  subgroup_label = "Gender"
)

race_prevalence_summary <- make_binary_subgroup_summary(
  data = analytic_sample,
  subgroup_var = "race_eth",
  subgroup_label = "Race/ethnicity"
)

prevalence_subgroup_summary <- bind_rows(
  gender_prevalence_summary,
  race_prevalence_summary
)


################################################################################
# 7. Optional disparity summary tables
################################################################################

# These tables summarize the spread across subgroup levels.
# They are useful for quickly seeing whether subgroup gaps look larger
# under one measurement approach than another.

continuous_disparity_summary <- continuous_subgroup_summary %>%
  group_by(
    subgroup_variable,
    outcome_variable,
    measure
  ) %>%
  summarise(
    lowest_mean_score = min(mean_score),
    highest_mean_score = max(mean_score),
    mean_score_range = highest_mean_score - lowest_mean_score,
    subgroup_with_lowest_mean = subgroup_level[which.min(mean_score)],
    subgroup_with_highest_mean = subgroup_level[which.max(mean_score)],
    .groups = "drop"
  ) %>%
  mutate(
    mean_score_range = round(mean_score_range, 3)
  )

prevalence_disparity_summary <- prevalence_subgroup_summary %>%
  group_by(
    subgroup_variable,
    binary_variable,
    measure
  ) %>%
  summarise(
    lowest_prevalence_percent = min(prevalence_percent),
    highest_prevalence_percent = max(prevalence_percent),
    prevalence_range_percentage_points =
      highest_prevalence_percent - lowest_prevalence_percent,
    subgroup_with_lowest_prevalence = subgroup_level[which.min(prevalence_percent)],
    subgroup_with_highest_prevalence = subgroup_level[which.max(prevalence_percent)],
    .groups = "drop"
  ) %>%
  mutate(
    prevalence_range_percentage_points =
      round(prevalence_range_percentage_points, 2)
  )


################################################################################
# 8. Figure 3A: Mean continuous scores by subgroup
################################################################################

# Note:
# The three continuous measures have different score ranges:
# full scale = 0 to 57
# brief screener = 0 to 12
# single item = 0 to 3
# For this reason, the figure uses separate rows for each measure.

# Create cleaner labels for plotting
continuous_subgroup_summary_plot <- continuous_subgroup_summary %>%
  mutate(
    mean_label = round(mean_score, 2)
  )

figure3a_mean_scores <- ggplot(
  continuous_subgroup_summary_plot,
  aes(
    x = subgroup_level,
    y = mean_score,
    fill = measure
  )
) +
  geom_col(
    show.legend = FALSE,
    width = 0.55
  ) +
  geom_text(
    aes(label = mean_label),
    vjust = -0.15,
    size = 3.5
  ) +
  facet_grid(
    rows = vars(measure),
    cols = vars(subgroup_variable),
    scales = "free",
    space = "free_x"
  ) +
  scale_x_discrete(drop = TRUE) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Figure 3A. Mean Depressive Symptom Scores by Demographic Subgroup",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; same respondents used for all three measurement approaches"
    ),
    x = NULL,
    y = "Mean score",
    caption = "Score ranges differ across measures: full scale 0-57, brief screener 0-12, single item 0-3."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/figures/figure3a_mean_scores_by_subgroup.png",
  plot = figure3a_mean_scores,
  width = 12,
  height = 7,
  dpi = 300
)


################################################################################
# 9. Figure 3B: Elevated-distress prevalence by subgroup
################################################################################

# Create a shorter label just for plotting
prevalence_subgroup_summary_plot <- prevalence_subgroup_summary %>%
  mutate(
    prevalence_label = paste0(round(prevalence_percent, 1), "%")
  )

figure3b_prevalence <- ggplot(
  prevalence_subgroup_summary_plot,
  aes(
    x = subgroup_level,
    y = prevalence_percent,
    fill = measure
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = prevalence_label),
    position = position_dodge(width = 0.8),
    vjust = -0.25,
    size = 3
  ) +
  facet_grid(
    . ~ subgroup_variable,
    scales = "free_x",
    space = "free_x"
  ) +
  scale_x_discrete(drop = TRUE) +
  labs(
    title = "Figure 3B. Elevated-Distress Prevalence by Demographic Subgroup",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; elevated distress defined using locked 80th-percentile thresholds"
    ),
    x = NULL,
    y = "Percent classified as elevated distress",
    fill = "Measurement approach",
    caption = "Prevalence uses binary elevated-distress variables created in Step 4."
  ) +
  coord_cartesian(
    ylim = c(0, max(prevalence_subgroup_summary_plot$prevalence_percent) + 10)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/figures/figure3b_prevalence_by_subgroup.png",
  plot = figure3b_prevalence,
  width = 12,
  height = 6,
  dpi = 300
)


################################################################################
# 10. Save subgroup tables
################################################################################

write_xlsx(
  list(
    sample_size_check = sample_size_check,
    continuous_subgroup_summary = continuous_subgroup_summary,
    prevalence_subgroup_summary = prevalence_subgroup_summary,
    continuous_disparity_summary = continuous_disparity_summary,
    prevalence_disparity_summary = prevalence_disparity_summary
  ),
  "outputs/04_subgroup_comparisons_outputs.xlsx"
)


################################################################################
# 11. Print key outputs to console
################################################################################

sample_size_check
continuous_subgroup_summary
prevalence_subgroup_summary
continuous_disparity_summary
prevalence_disparity_summary
figure3a_mean_scores
figure3b_prevalence