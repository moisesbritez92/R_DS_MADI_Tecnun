library(tidyverse)
library(broom)

# Load data
ins <- read_csv("insurance.csv")

# Convert categorical variables
ins <- ins %>%
  mutate(
    smoker = factor(smoker),
    sex    = factor(sex)
  )

# 1) Single predictors
m1 <- lm(charges ~ bmi, data=ins)
m2 <- lm(charges ~ age, data=ins)
m3 <- lm(charges ~ smoker, data=ins)

# 2) Multiple predictors
m4 <- lm(charges ~ age + bmi + smoker, data=ins)

# Compare adjusted R²
glance(m1)$adj.r.squared
glance(m2)$adj.r.squared
glance(m3)$adj.r.squared
glance(m4)$adj.r.squared

# Summaries
summary(m4)
