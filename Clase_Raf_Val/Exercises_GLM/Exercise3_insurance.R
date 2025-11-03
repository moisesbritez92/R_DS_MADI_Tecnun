library(tidyverse)
library(broom)
library(ggpubr)
library(lmtest)
library(car)

# Load and clean ---------------------------------------------------------
insurance <- read_csv("insurance.csv", show_col_types = FALSE)

df <- subset(insurance)  %>%
  dplyr::select(charges, bmi) %>%
  dplyr::filter(is.finite(charges), is.finite(bmi))

ggplot(df, aes(bmi, charges)) +
  geom_point(alpha = 0.35) +
  labs(title = "Charges vs BMI", x = "BMI", y = "Medical Charges") +
  theme_minimal()

## any problem?
# check other colums!

# 1a) Scatter (shape of relationship)
df <- subset(insurance, smoker=="yes")  %>%
  dplyr::select(charges, bmi) %>%
  dplyr::filter(is.finite(charges), is.finite(bmi))

# 4) Visualize explained vs unexplained -------------------------------------

ggplot(df, aes(bmi, charges)) +
  geom_point(alpha = 0.35) +
  labs(title = "Charges vs BMI", x = "BMI", y = "Medical Charges") +
  theme_minimal()

# 1b) Distributions
p_hist_bmi <- ggplot(df, aes(bmi)) +
  geom_histogram(bins = 30) +
  labs(title = "BMI distribution") +
  theme_minimal()

p_hist_charges <- ggplot(df, aes(charges)) +
  geom_histogram(bins = 30) +
  labs(title = "Charges distribution") +
  theme_minimal()

# 1c) Correlation
pearson  <- cor(df$bmi, df$charges, method = "pearson")
spearman <- cor(df$bmi, df$charges, method = "spearman")

p_scatter_lm <- ggplot(df, aes(bmi, charges)) +
  geom_point(alpha = 0.35) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", color="red") +
  annotate("text", x = Inf, y = Inf, hjust = 1.02, vjust = 1.5,
           label = sprintf("Pearson r = %.2f\nSpearman ρ = %.2f", pearson, spearman)) +
  labs(title = "Correlation overlay (dashed = linear fit)", x = "BMI", y = "Charges") +
  theme_minimal()

ggpubr::ggarrange(p_scatter_loess, p_scatter_lm, p_hist_bmi, p_hist_charges,
                  ncol = 2, nrow = 2)

# 2) Linear model -----------------------------------------------------------
m <- lm(charges ~ bmi, data = df)
summary(m)

df_mod <- augment(m)

set.seed(1)
samp <- df_mod %>% slice_sample(n = 100)

ggplot(samp, aes(bmi, charges)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_segment(aes(xend = bmi, y = .fitted, yend = charges),
               color = "gray40", alpha = 0.6) +
  labs(title = "Explained (ŷ = Xβ) + Unexplained (ε)",
       subtitle = "Red = model fit; Gray = residuals",
       x = "BMI", y = "Charges") +
  theme_minimal()

# 3) Assumption checks ------------------------------------------------------

# Homoscedasticity
bptest(m)

set.seed(1)
shapiro.test(sample(residuals(m), size = min(5000, length(residuals(m)))))




