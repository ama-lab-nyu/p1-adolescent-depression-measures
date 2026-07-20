################################################################################
# Script: 06_agreement.R
# Purpose: Compute classification agreement across depressive symptom measures
#          and create Figure 2 for the Add Health P1 project
################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)
library(ggplot2)


################################################################################
# 1. Read thresholded analytic dataset
################################################################################

analytic_sample <- readRDS("data/addhealth_analytic_sample_thresholded.rds")


################################################################################
# 2. Define binary classification variables
################################################################################

binary_vars <- c(
  "cesd_full_elevated",
  "cesd_brief_elevated",
  "cesd_single_depressed_elevated"
)

score_vars <- c(
  "cesd_full_score",
  "cesd_brief_score",
  "cesd_single_depressed_ord"
)


################################################################################
# 3. Basic quality checks
################################################################################

required_vars <- c(binary_vars, score_vars)

missing_required_vars <- setdiff(required_vars, names(analytic_sample))

if (length(missing_required_vars) > 0) {
  stop(
    paste(
      "These required variables are missing:",
      paste(missing_required_vars, collapse = ", ")
    )
  )
}

agreement_sample_check <- tibble(
  analytic_n = nrow(analytic_sample),
  n_missing_full_binary = sum(is.na(analytic_sample$cesd_full_elevated)),
  n_missing_brief_binary = sum(is.na(analytic_sample$cesd_brief_elevated)),
  n_missing_single_binary = sum(is.na(analytic_sample$cesd_single_depressed_elevated))
)

binary_value_check <- analytic_sample %>%
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


################################################################################
# 4. Define measure pairs
################################################################################

measure_pairs <- tribble(
  ~comparison, ~measure1_label, ~measure1_binary, ~measure2_label, ~measure2_binary,
  "Full vs brief",
  "Full 19-item modified CES-D",
  "cesd_full_elevated",
  "Four-item brief screener",
  "cesd_brief_elevated",
  
  "Full vs single",
  "Full 19-item modified CES-D",
  "cesd_full_elevated",
  "Single depressed-mood item",
  "cesd_single_depressed_elevated",
  
  "Brief vs single",
  "Four-item brief screener",
  "cesd_brief_elevated",
  "Single depressed-mood item",
  "cesd_single_depressed_elevated"
)


################################################################################
# 5. Helper functions
################################################################################

class_label <- function(x) {
  if_else(
    x == 1,
    "Elevated",
    "Not elevated"
  )
}

landis_koch_label <- function(kappa_value) {
  case_when(
    kappa_value <= 0.20 ~ "Poor agreement",
    kappa_value > 0.20 & kappa_value <= 0.40 ~ "Fair agreement",
    kappa_value > 0.40 & kappa_value <= 0.60 ~ "Moderate agreement",
    kappa_value > 0.60 & kappa_value <= 0.80 ~ "Substantial agreement",
    kappa_value > 0.80 ~ "Near-perfect agreement",
    TRUE ~ NA_character_
  )
}

compute_kappa <- function(data, comparison, measure1_label, measure1_binary,
                          measure2_label, measure2_binary) {
  
  x <- data[[measure1_binary]]
  y <- data[[measure2_binary]]
  
  complete_index <- !is.na(x) & !is.na(y)
  
  x <- x[complete_index]
  y <- y[complete_index]
  
  # Create a 2x2 table with fixed category order
  kappa_table <- table(
    factor(x, levels = c(0, 1), labels = c("Not elevated", "Elevated")),
    factor(y, levels = c(0, 1), labels = c("Not elevated", "Elevated"))
  )
  
  # Use psych::cohen.kappa() as specified in the analysis plan
  kappa_object <- psych::cohen.kappa(kappa_table)
  
  # Extract kappa from the psych output
  kappa_value <- as.numeric(kappa_object$kappa)
  
  # Pull the 2x2 counts
  n00 <- as.numeric(kappa_table["Not elevated", "Not elevated"])
  n01 <- as.numeric(kappa_table["Not elevated", "Elevated"])
  n10 <- as.numeric(kappa_table["Elevated", "Not elevated"])
  n11 <- as.numeric(kappa_table["Elevated", "Elevated"])
  
  n_total <- n00 + n01 + n10 + n11
  
  observed_agreement <- (n00 + n11) / n_total
  
  tibble(
    comparison = comparison,
    measure1 = measure1_label,
    measure2 = measure2_label,
    n_total = n_total,
    
    n_agree_not_elevated = n00,
    n_measure2_only = n01,
    n_measure1_only = n10,
    n_agree_elevated = n11,
    
    n_concordant = n00 + n11,
    n_discordant = n01 + n10,
    
    observed_agreement = round(observed_agreement, 4),
    cohen_kappa = round(kappa_value, 4),
    
    landis_koch_interpretation = landis_koch_label(kappa_value),
    
    decision_note = case_when(
      kappa_value > 0.80 ~
        "Measures are largely interchangeable for this pair.",
      kappa_value < 0.40 ~
        "Potential headline finding: low classification agreement.",
      TRUE ~
        "Agreement is not near-perfect; inspect reclassification table."
    )
  )
}

make_reclassification_long <- function(data, comparison, measure1_label,
                                       measure1_binary, measure2_label,
                                       measure2_binary) {
  
  data %>%
    transmute(
      measure1_status = factor(
        class_label(.data[[measure1_binary]]),
        levels = c("Not elevated", "Elevated")
      ),
      measure2_status = factor(
        class_label(.data[[measure2_binary]]),
        levels = c("Not elevated", "Elevated")
      )
    ) %>%
    count(
      measure1_status,
      measure2_status,
      name = "n"
    ) %>%
    complete(
      measure1_status,
      measure2_status,
      fill = list(n = 0)
    ) %>%
    mutate(
      comparison = comparison,
      measure1 = measure1_label,
      measure2 = measure2_label,
      percent_total = round(100 * n / sum(n), 2),
      classification_pattern = case_when(
        measure1_status == "Not elevated" & measure2_status == "Not elevated" ~
          "Concordant: neither measure classifies elevated",
        measure1_status == "Elevated" & measure2_status == "Elevated" ~
          "Concordant: both measures classify elevated",
        measure1_status == "Elevated" & measure2_status == "Not elevated" ~
          paste0("Discordant: ", measure1_label, " only"),
        measure1_status == "Not elevated" & measure2_status == "Elevated" ~
          paste0("Discordant: ", measure2_label, " only"),
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      comparison,
      measure1,
      measure2,
      measure1_status,
      measure2_status,
      classification_pattern,
      n,
      percent_total
    )
}

make_reclassification_matrix <- function(data, measure1_binary,
                                         measure2_binary) {
  
  x <- factor(
    class_label(data[[measure1_binary]]),
    levels = c("Not elevated", "Elevated")
  )
  
  y <- factor(
    class_label(data[[measure2_binary]]),
    levels = c("Not elevated", "Elevated")
  )
  
  tab <- table(x, y)
  
  as.data.frame.matrix(tab) %>%
    rownames_to_column("measure1_status") %>%
    as_tibble()
}


################################################################################
# 6. Cohen's kappa for all pairwise comparisons
################################################################################

kappa_summary <- pmap_dfr(
  measure_pairs,
  function(comparison, measure1_label, measure1_binary,
           measure2_label, measure2_binary) {
    
    compute_kappa(
      data = analytic_sample,
      comparison = comparison,
      measure1_label = measure1_label,
      measure1_binary = measure1_binary,
      measure2_label = measure2_label,
      measure2_binary = measure2_binary
    )
  }
)

kappa_decision_summary <- tibble(
  all_pairs_above_0_80 = all(kappa_summary$cohen_kappa > 0.80),
  any_pair_below_0_40 = any(kappa_summary$cohen_kappa < 0.40),
  overall_decision_note = case_when(
    all_pairs_above_0_80 ~
      "All kappas are above 0.80. Classification results suggest measures are largely interchangeable; emphasize regression and subgroup differences.",
    any_pair_below_0_40 ~
      "At least one kappa is below 0.40. Low classification agreement should be treated as a headline finding.",
    TRUE ~
      "Classification agreement is mixed. Emphasize both prevalence shifts and reclassification patterns."
  )
)


################################################################################
# 7. 2x2 reclassification tables
################################################################################

reclassification_long <- pmap_dfr(
  measure_pairs,
  function(comparison, measure1_label, measure1_binary,
           measure2_label, measure2_binary) {
    
    make_reclassification_long(
      data = analytic_sample,
      comparison = comparison,
      measure1_label = measure1_label,
      measure1_binary = measure1_binary,
      measure2_label = measure2_label,
      measure2_binary = measure2_binary
    )
  }
)

full_vs_brief_2x2 <- make_reclassification_matrix(
  data = analytic_sample,
  measure1_binary = "cesd_full_elevated",
  measure2_binary = "cesd_brief_elevated"
) %>%
  rename(
    `Full scale status` = measure1_status
  )

full_vs_single_2x2 <- make_reclassification_matrix(
  data = analytic_sample,
  measure1_binary = "cesd_full_elevated",
  measure2_binary = "cesd_single_depressed_elevated"
) %>%
  rename(
    `Full scale status` = measure1_status
  )

brief_vs_single_2x2 <- make_reclassification_matrix(
  data = analytic_sample,
  measure1_binary = "cesd_brief_elevated",
  measure2_binary = "cesd_single_depressed_elevated"
) %>%
  rename(
    `Brief screener status` = measure1_status
  )


################################################################################
# 8. Prevalence table for Figure 2 Panel 1
################################################################################

prevalence_summary <- tibble(
  measure = c(
    "Full 19-item modified CES-D",
    "Four-item brief screener",
    "Single depressed-mood item"
  ),
  binary_variable = c(
    "cesd_full_elevated",
    "cesd_brief_elevated",
    "cesd_single_depressed_elevated"
  ),
  n_total = nrow(analytic_sample),
  n_elevated = c(
    sum(analytic_sample$cesd_full_elevated == 1),
    sum(analytic_sample$cesd_brief_elevated == 1),
    sum(analytic_sample$cesd_single_depressed_elevated == 1)
  )
) %>%
  mutate(
    percent_elevated = round(100 * n_elevated / n_total, 2)
  )


################################################################################
# 9. Classification overlap table for Figure 2 Panel 2
################################################################################

classification_overlap_summary <- analytic_sample %>%
  mutate(
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
  ) %>%
  count(
    classification_overlap,
    name = "n"
  ) %>%
  mutate(
    n_total = sum(n),
    percent = round(100 * n / n_total, 2)
  )


################################################################################
# 10. Figure 2: Prevalence and classification overlap
################################################################################

figure2_panel1_data <- prevalence_summary %>%
  transmute(
    panel = "Panel A. Prevalence by measure",
    category = measure,
    n = n_elevated,
    percent = percent_elevated,
    label = paste0(round(percent, 1), "%")
  )

figure2_panel2_data <- classification_overlap_summary %>%
  transmute(
    panel = "Panel B. Classification overlap",
    category = as.character(classification_overlap),
    n = n,
    percent = percent,
    label = paste0(round(percent, 1), "%")
  )

figure2_data <- bind_rows(
  figure2_panel1_data,
  figure2_panel2_data
) %>%
  mutate(
    panel = factor(
      panel,
      levels = c(
        "Panel A. Prevalence by measure",
        "Panel B. Classification overlap"
      )
    )
  )

figure2_prevalence_overlap <- ggplot(
  figure2_data,
  aes(
    x = category,
    y = percent,
    fill = category
  )
) +
  geom_col(
    width = 0.6,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.25,
    size = 3.5
  ) +
  facet_grid(
    . ~ panel,
    scales = "free_x",
    space = "free_x"
  ) +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 16)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Figure 2. Prevalence and Classification Differences Across Measures",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; binary classifications use locked 80th-percentile thresholds"
    ),
    x = NULL,
    y = "Percent of respondents",
    caption = "Panel A shows prevalence under each measurement approach. Panel B shows overlap across the three binary classifications."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/figures/figure2_prevalence_classification_overlap.png",
  plot = figure2_prevalence_overlap,
  width = 13,
  height = 6,
  dpi = 300
)


################################################################################
# 11. Save agreement outputs
################################################################################

write_xlsx(
  list(
    agreement_sample_check = agreement_sample_check,
    binary_value_check = binary_value_check,
    kappa_summary = kappa_summary,
    kappa_decision_summary = kappa_decision_summary,
    reclassification_long = reclassification_long,
    full_vs_brief_2x2 = full_vs_brief_2x2,
    full_vs_single_2x2 = full_vs_single_2x2,
    brief_vs_single_2x2 = brief_vs_single_2x2,
    prevalence_summary = prevalence_summary,
    classification_overlap_summary = classification_overlap_summary
  ),
  "outputs/06_agreement_outputs_RO.xlsx"
)


################################################################################
# 12. Print key outputs to console
################################################################################

agreement_sample_check
binary_value_check
kappa_summary
kappa_decision_summary
reclassification_long
prevalence_summary
classification_overlap_summary
figure2_prevalence_overlap