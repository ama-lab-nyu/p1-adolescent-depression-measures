################################################################################
# Script: 05_regressions.R
# Purpose: Run linear and logistic regression models for the Add Health P1
#          depression measurement comparison project
################################################################################

rm(list = ls())

library(tidyverse)
library(broom)
library(writexl)
library(ggplot2)


################################################################################
# 1. Read thresholded analytic dataset
################################################################################

analytic_sample <- readRDS("data/addhealth_analytic_sample_thresholded.rds")


################################################################################
# 2. Define outcomes and predictors
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

predictor_vars <- c(
  "gender",
  "race_eth",
  "resident_parent_education",
  "family_structure",
  "school_connectedness",
  "peer_support",
  "age"
)

categorical_predictors <- c(
  "gender",
  "race_eth",
  "resident_parent_education",
  "family_structure"
)

continuous_predictors <- c(
  "school_connectedness",
  "peer_support",
  "age"
)


################################################################################
# 3. Basic checks before modeling
################################################################################

required_vars <- c(
  continuous_outcomes,
  binary_outcomes,
  predictor_vars
)

missing_required_vars <- setdiff(required_vars, names(analytic_sample))

if (length(missing_required_vars) > 0) {
  stop(
    paste(
      "These required variables are missing:",
      paste(missing_required_vars, collapse = ", ")
    )
  )
}

regression_sample_check <- tibble(
  analytic_n = nrow(analytic_sample),
  n_missing_any_model_variable = analytic_sample %>%
    select(all_of(required_vars)) %>%
    filter(if_any(everything(), is.na)) %>%
    nrow(),
  no_missing_model_variables = n_missing_any_model_variable == 0
)

binary_outcome_check <- analytic_sample %>%
  summarise(
    across(
      all_of(binary_outcomes),
      ~ paste(sort(unique(.)), collapse = ", ")
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "binary_outcome",
    values_to = "observed_values"
  ) %>%
  mutate(
    expected_values = "0, 1",
    values_correct = observed_values == expected_values
  )


################################################################################
# 4. Set reference categories
################################################################################

analytic_sample <- analytic_sample %>%
  mutate(
    gender = factor(
      gender,
      levels = c("Male", "Female")
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
    ),
    
    resident_parent_education = factor(
      resident_parent_education,
      levels = c(
        "Less than high school",
        "HS/GED/vocational instead",
        "Some college/vocational after",
        "College or more"
      )
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
  )

reference_level_table <- tibble(
  variable = c(
    "gender",
    "race_eth",
    "resident_parent_education",
    "family_structure"
  ),
  reference_level = c(
    levels(analytic_sample$gender)[1],
    levels(analytic_sample$race_eth)[1],
    levels(analytic_sample$resident_parent_education)[1],
    levels(analytic_sample$family_structure)[1]
  )
)


################################################################################
# 5. Create standardized variables for continuous linear models
################################################################################

# For standardized continuous models:
# - continuous outcomes are standardized
# - continuous predictors are standardized
# - categorical predictors stay as factors
#
# This lets coefficients be compared across the three outcome scales.
# For categorical predictors, coefficients are interpreted as SD-unit outcome
# differences relative to the reference group.

analytic_sample <- analytic_sample %>%
  mutate(
    cesd_full_score_z = as.numeric(scale(cesd_full_score)),
    cesd_brief_score_z = as.numeric(scale(cesd_brief_score)),
    cesd_single_depressed_ord_z = as.numeric(scale(cesd_single_depressed_ord)),
    
    school_connectedness_z = as.numeric(scale(school_connectedness)),
    peer_support_z = as.numeric(scale(peer_support)),
    age_z = as.numeric(scale(age))
  )

standardized_outcomes <- c(
  "cesd_full_score_z",
  "cesd_brief_score_z",
  "cesd_single_depressed_ord_z"
)

standardized_predictor_vars <- c(
  "gender",
  "race_eth",
  "resident_parent_education",
  "family_structure",
  "school_connectedness_z",
  "peer_support_z",
  "age_z"
)


################################################################################
# 6. Helper functions
################################################################################

measure_label <- function(x) {
  case_when(
    x %in% c("cesd_full_score", "cesd_full_score_z", "cesd_full_elevated") ~
      "Full 19-item modified CES-D",
    
    x %in% c("cesd_brief_score", "cesd_brief_score_z", "cesd_brief_elevated") ~
      "Four-item brief screener",
    
    x %in% c(
      "cesd_single_depressed_ord",
      "cesd_single_depressed_ord_z",
      "cesd_single_depressed_elevated"
    ) ~
      "Single depressed-mood item",
    
    TRUE ~ x
  )
}


clean_term_label <- function(term) {
  case_when(
    term == "(Intercept)" ~ "Intercept",
    
    term == "genderFemale" ~
      "Gender: Female vs Male",
    
    str_starts(term, "race_eth") ~
      paste0(
        "Race/ethnicity: ",
        str_remove(term, "^race_eth"),
        " vs NH-White"
      ),
    
    str_starts(term, "resident_parent_education") ~
      paste0(
        "Resident parent education: ",
        str_remove(term, "^resident_parent_education"),
        " vs less than high school"
      ),
    
    str_starts(term, "family_structure") ~
      paste0(
        "Family structure: ",
        str_remove(term, "^family_structure"),
        " vs two parents"
      ),
    
    term == "school_connectedness" ~
      "School connectedness",
    
    term == "peer_support" ~
      "Peer social support",
    
    term == "age" ~
      "Age",
    
    term == "school_connectedness_z" ~
      "School connectedness, per SD",
    
    term == "peer_support_z" ~
      "Peer social support, per SD",
    
    term == "age_z" ~
      "Age, per SD",
    
    TRUE ~ term
  )
}


make_formula <- function(outcome, predictors) {
  as.formula(
    paste(
      outcome,
      "~",
      paste(predictors, collapse = " + ")
    )
  )
}


tidy_lm_wald <- function(model, outcome_name, model_type, coefficient_scale) {
  broom::tidy(model) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      outcome = outcome_name,
      measure = measure_label(outcome_name),
      model_type = model_type,
      coefficient_scale = coefficient_scale,
      term_label = clean_term_label(term)
    ) %>%
    select(
      outcome,
      measure,
      model_type,
      coefficient_scale,
      term,
      term_label,
      estimate,
      std.error,
      conf.low,
      conf.high,
      statistic,
      p.value
    )
}


tidy_glm_or_wald <- function(model, outcome_name) {
  broom::tidy(model) %>%
    mutate(
      log_odds_estimate = estimate,
      log_odds_std_error = std.error,
      log_odds_conf_low = estimate - 1.96 * std.error,
      log_odds_conf_high = estimate + 1.96 * std.error,
      
      odds_ratio = exp(log_odds_estimate),
      conf.low = exp(log_odds_conf_low),
      conf.high = exp(log_odds_conf_high),
      
      outcome = outcome_name,
      measure = measure_label(outcome_name),
      model_type = "Logistic regression",
      coefficient_scale = "Odds ratio",
      term_label = clean_term_label(term)
    ) %>%
    select(
      outcome,
      measure,
      model_type,
      coefficient_scale,
      term,
      term_label,
      log_odds_estimate,
      log_odds_std_error,
      odds_ratio,
      conf.low,
      conf.high,
      statistic,
      p.value
    )
}


fit_glm_with_warnings <- function(formula, data) {
  
  warning_messages <- character(0)
  
  model <- withCallingHandlers(
    glm(
      formula = formula,
      data = data,
      family = binomial()
    ),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  list(
    model = model,
    warnings = unique(warning_messages)
  )
}


calculate_vif_from_model <- function(model) {
  
  x_matrix <- model.matrix(model)
  
  x_matrix <- x_matrix[
    ,
    colnames(x_matrix) != "(Intercept)",
    drop = FALSE
  ]
  
  if (ncol(x_matrix) < 2) {
    return(
      tibble(
        model_term = colnames(x_matrix),
        vif = NA_real_,
        vif_flag = "Not enough predictors to calculate VIF"
      )
    )
  }
  
  vif_values <- map_dbl(
    seq_len(ncol(x_matrix)),
    function(j) {
      
      x_j <- x_matrix[, j]
      x_others <- x_matrix[, -j, drop = FALSE]
      
      r_squared <- summary(
        lm(x_j ~ x_others)
      )$r.squared
      
      1 / (1 - r_squared)
    }
  )
  
  tibble(
    model_term = colnames(x_matrix),
    vif = vif_values,
    vif_flag = case_when(
      is.infinite(vif) ~ "FLAG: infinite VIF",
      vif > 5 ~ "FLAG: VIF above 5",
      TRUE ~ "OK"
    )
  )
}


check_categorical_separation <- function(data, binary_outcome, categorical_vars) {
  
  map_dfr(
    categorical_vars,
    function(var_name) {
      
      data %>%
        group_by(
          predictor = .data[[var_name]],
          .drop = FALSE
        ) %>%
        summarise(
          n = n(),
          n_events = sum(.data[[binary_outcome]] == 1),
          n_nonevents = sum(.data[[binary_outcome]] == 0),
          event_rate = mean(.data[[binary_outcome]] == 1),
          .groups = "drop"
        ) %>%
        mutate(
          outcome = binary_outcome,
          measure = measure_label(binary_outcome),
          predictor_variable = var_name,
          separation_flag = case_when(
            n_events == 0 ~ "FLAG: zero events in this category",
            n_nonevents == 0 ~ "FLAG: zero non-events in this category",
            TRUE ~ "OK"
          )
        ) %>%
        select(
          outcome,
          measure,
          predictor_variable,
          predictor,
          n,
          n_events,
          n_nonevents,
          event_rate,
          separation_flag
        )
    }
  )
}


################################################################################
# 7. Fit unstandardized linear regression models
################################################################################

linear_unstandardized_models <- map(
  continuous_outcomes,
  function(y) {
    lm(
      formula = make_formula(y, predictor_vars),
      data = analytic_sample
    )
  }
)

names(linear_unstandardized_models) <- continuous_outcomes

linear_unstandardized_results <- map_dfr(
  continuous_outcomes,
  function(y) {
    tidy_lm_wald(
      model = linear_unstandardized_models[[y]],
      outcome_name = y,
      model_type = "Linear regression",
      coefficient_scale = "Unstandardized coefficient"
    )
  }
)


################################################################################
# 8. Fit standardized linear regression models
################################################################################

linear_standardized_models <- map(
  standardized_outcomes,
  function(y) {
    lm(
      formula = make_formula(y, standardized_predictor_vars),
      data = analytic_sample
    )
  }
)

names(linear_standardized_models) <- standardized_outcomes

linear_standardized_results <- map_dfr(
  standardized_outcomes,
  function(y) {
    tidy_lm_wald(
      model = linear_standardized_models[[y]],
      outcome_name = y,
      model_type = "Linear regression",
      coefficient_scale = "Standardized coefficient"
    )
  }
)


################################################################################
# 9. Fit logistic regression models
################################################################################

logistic_fit_objects <- map(
  binary_outcomes,
  function(y) {
    fit_glm_with_warnings(
      formula = make_formula(y, predictor_vars),
      data = analytic_sample
    )
  }
)

names(logistic_fit_objects) <- binary_outcomes

logistic_models <- map(
  logistic_fit_objects,
  "model"
)

logistic_results <- map_dfr(
  binary_outcomes,
  function(y) {
    tidy_glm_or_wald(
      model = logistic_models[[y]],
      outcome_name = y
    )
  }
)


################################################################################
# 10. Model fit summaries
################################################################################

linear_unstandardized_fit_summary <- map_dfr(
  continuous_outcomes,
  function(y) {
    broom::glance(linear_unstandardized_models[[y]]) %>%
      mutate(
        outcome = y,
        measure = measure_label(y),
        model_type = "Linear regression",
        coefficient_scale = "Unstandardized"
      )
  }
)

linear_standardized_fit_summary <- map_dfr(
  standardized_outcomes,
  function(y) {
    broom::glance(linear_standardized_models[[y]]) %>%
      mutate(
        outcome = y,
        measure = measure_label(y),
        model_type = "Linear regression",
        coefficient_scale = "Standardized"
      )
  }
)

logistic_fit_summary <- map_dfr(
  binary_outcomes,
  function(y) {
    broom::glance(logistic_models[[y]]) %>%
      mutate(
        outcome = y,
        measure = measure_label(y),
        model_type = "Logistic regression",
        coefficient_scale = "Odds ratio",
        converged = logistic_models[[y]]$converged
      )
  }
)

model_fit_summary <- bind_rows(
  linear_unstandardized_fit_summary,
  linear_standardized_fit_summary,
  logistic_fit_summary
)


################################################################################
# 11. Multicollinearity diagnostics
################################################################################

# VIF does not depend on the outcome when the predictor set is the same.
# We calculate it using the full-scale unstandardized linear model.
# This is a column-level VIF based on the model matrix, so categorical
# predictors appear as dummy-variable contrasts.

vif_diagnostics <- calculate_vif_from_model(
  linear_unstandardized_models[["cesd_full_score"]]
)


################################################################################
# 12. Logistic separation diagnostics
################################################################################

logistic_warning_diagnostics <- map_dfr(
  binary_outcomes,
  function(y) {
    
    warnings_y <- logistic_fit_objects[[y]]$warnings
    
    if (length(warnings_y) == 0) {
      tibble(
        outcome = y,
        measure = measure_label(y),
        warning_message = "No glm warnings"
      )
    } else {
      tibble(
        outcome = y,
        measure = measure_label(y),
        warning_message = warnings_y
      )
    }
  }
)

separation_diagnostics <- map_dfr(
  binary_outcomes,
  function(y) {
    check_categorical_separation(
      data = analytic_sample,
      binary_outcome = y,
      categorical_vars = categorical_predictors
    )
  }
)


################################################################################
# 13. Prepare data for Figure 4A: standardized linear coefficients
################################################################################

figure4a_data <- linear_standardized_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    measure = factor(
      measure,
      levels = c(
        "Full 19-item modified CES-D",
        "Four-item brief screener",
        "Single depressed-mood item"
      )
    )
  )

term_order_linear <- figure4a_data %>%
  distinct(term, term_label) %>%
  pull(term_label)

figure4a_data <- figure4a_data %>%
  mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_linear)
    )
  )


################################################################################
# 14. Figure 4A: standardized linear regression coefficients
################################################################################

figure4a_linear <- ggplot(
  figure4a_data,
  aes(
    x = estimate,
    y = term_label,
    xmin = conf.low,
    xmax = conf.high,
    color = measure
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_pointrange(
    position = position_dodge(width = 0.6)
  ) +
  labs(
    title = "Figure 4A. Standardized Linear Regression Coefficients Across Measures",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; coefficients use standardized continuous outcomes"
    ),
    x = "Standardized coefficient",
    y = NULL,
    color = "Measurement approach",
    caption = "Points are estimates; horizontal lines are 95% Wald confidence intervals."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/figures/figure4a_linear_standardized_coefficients.png",
  plot = figure4a_linear,
  width = 12,
  height = 8,
  dpi = 300
)


################################################################################
# 15. Prepare data for Figure 4B: logistic odds ratios
################################################################################

figure4b_data <- logistic_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    measure = factor(
      measure,
      levels = c(
        "Full 19-item modified CES-D",
        "Four-item brief screener",
        "Single depressed-mood item"
      )
    )
  )

term_order_logistic <- figure4b_data %>%
  distinct(term, term_label) %>%
  pull(term_label)

figure4b_data <- figure4b_data %>%
  mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_logistic)
    )
  )


################################################################################
# 16. Figure 4B: logistic regression odds ratios
################################################################################

figure4b_logistic <- ggplot(
  figure4b_data,
  aes(
    x = odds_ratio,
    y = term_label,
    xmin = conf.low,
    xmax = conf.high,
    color = measure
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  geom_pointrange(
    position = position_dodge(width = 0.6)
  ) +
  scale_x_log10() +
  labs(
    title = "Figure 4B. Logistic Regression Odds Ratios Across Measures",
    subtitle = paste0(
      "Analytic sample N = ",
      nrow(analytic_sample),
      "; binary outcomes use locked 80th-percentile thresholds"
    ),
    x = "Odds ratio, log scale",
    y = NULL,
    color = "Measurement approach",
    caption = "Points are odds ratios; horizontal lines are 95% Wald confidence intervals."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/figures/figure4b_logistic_odds_ratios.png",
  plot = figure4b_logistic,
  width = 12,
  height = 8,
  dpi = 300
)


################################################################################
# 17. Save model objects
################################################################################

regression_model_objects <- list(
  linear_unstandardized_models = linear_unstandardized_models,
  linear_standardized_models = linear_standardized_models,
  logistic_models = logistic_models
)

saveRDS(
  regression_model_objects,
  "data/regression_model_objects.rds"
)


################################################################################
# 18. Save regression outputs
################################################################################

write_xlsx(
  list(
    regression_sample_check = regression_sample_check,
    binary_outcome_check = binary_outcome_check,
    reference_level_table = reference_level_table,
    linear_unstandardized_results = linear_unstandardized_results,
    linear_standardized_results = linear_standardized_results,
    logistic_odds_ratio_results = logistic_results,
    model_fit_summary = model_fit_summary,
    vif_diagnostics = vif_diagnostics,
    logistic_warning_diagnostics = logistic_warning_diagnostics,
    separation_diagnostics = separation_diagnostics
  ),
  "outputs/05_regression_outputs_RO.xlsx"
)


################################################################################
# 19. Print key outputs to console
################################################################################

regression_sample_check
binary_outcome_check
reference_level_table
vif_diagnostics
logistic_warning_diagnostics
separation_diagnostics
model_fit_summary
figure4a_linear
figure4b_logistic