library(tidyverse)

# BMI vs Charges
df <- data.frame(
  BMI = c(18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40),
  Charges = c(2100, 2300, 2600, 3100, 3400,   
              4200, 4800, 6700, 7600, 10300,  
              12800, 15000)                   
)

# Scatter plot
ggplot(df, aes(BMI, Charges)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "BMI vs Medical Charges",
       subtitle = "Positive, right-skewed with noise",
       x = "BMI", y = "Charges (USD)") +
  theme_minimal()


