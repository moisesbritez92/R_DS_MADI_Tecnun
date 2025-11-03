library(ggplot2)

# Artificial dataset: BMI vs Heart Event (0/1)
df <- data.frame(
  BMI = c(
    18.5, 19.2, 19.8, 20.1, 20.5,
    21.0, 21.5, 22.0, 22.5, 23.0,
    23.5, 24.0, 24.5, 25.0, 25.5,
    26.0, 26.5, 27.0, 27.5, 28.0,
    28.5, 29.0, 29.5, 30.0, 30.5,
    31.0, 31.5, 32.0, 32.5, 33.0,
    33.5, 34.0, 34.5, 35.0, 35.5,
    36.0, 36.5, 37.0, 37.5, 38.0,
    38.5, 39.0, 39.5, 40.0, 40.5,
    41.0, 41.5, 42.0, 42.5, 43.0
  ),
  Event = c(
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,1,
    0,1,0,1,0,
    1,1,1,1,1,
    1,1,1,1,1,
    1,1,1,1,1,
    1,1,1,1,1
  )
)

# Fit linear regression (bad for binary) and logistic regression (correct)
lm_fit  <- lm(Event ~ BMI, data = df)
glm_fit <- glm(Event ~ BMI, data = df, family = binomial(link = "logit"))
# family binomial - binary events / logistic as the link function 

df$lm_pred  <- predict(lm_fit, type = "response")
df$glm_pred <- predict(glm_fit, type = "response")

# Plot
ggplot(df, aes(BMI, Event)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_line(aes(y = lm_pred), color = "blue", size = 1, linetype = "dashed") +
  geom_line(aes(y = glm_pred), color = "red", size = 1) +
  labs(title = "Heart Event vs BMI",
       subtitle = "Points = observed heart events (0/1)\nBlue dashed = LM; Red = GLM (predicted probability)",
       y = "Heart Event (0 = No, 1 = Yes)", 
       x = "BMI") +
  theme_minimal()


# probabilities vs actual events
sum(abs(df$lm_pred  - df$Event))
sum(abs(df$glm_pred - df$Event))

