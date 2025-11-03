library(tidyverse)
library(broom)

# Load data (train.csv from Kaggle)
house <- read_csv("train.csv")

# Keep only numeric predictors
num_vars <- house %>%
  dplyr::select(where(is.numeric)) %>%
  drop_na()

names(num_vars) <- make.names(names(num_vars)) 

# Outcome
outcome <- "SalePrice"
predictors <- setdiff(names(num_vars), outcome)

# Univariate regressions
results_list <- list()

for (var in predictors) {
  # Build formula
  form <- as.formula(paste(outcome, "~", var))
  
  # Fit linear model
  m <- lm(form, data = num_vars)
  
  # Tidy output
  tmp <- tidy(m)
  
  # Drop intercept row
  tmp <- tmp[tmp$term != "(Intercept)", ]
  
  # Add predictor name
  tmp$predictor <- var
  
  # Store in list
  results_list[[var]] <- tmp
}

# Combine all results
results <- bind_rows(results_list)

# FDR correction
results <- results %>%
  dplyr::mutate(p_adj_fdr = p.adjust(p.value, method="fdr"), 
                neglog10p = -log10(p.value),
                significant = p_adj_fdr < 0.05) %>%
  dplyr::arrange(p_adj_fdr)

# plot
ggplot(results, aes(x = reorder(predictor, neglog10p), y = neglog10p, fill = significant)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = -log10(0.05), color = "red", linetype = "dashed") +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  labs(
    title = "Univariate regressions: SalePrice vs predictors",
    subtitle = "Dashed red line = FDR threshold",
    x = "Predictor",
    y = "-log10(p-value)",
    fill = "FDR < 0.05"
  ) +
  theme_minimal()
