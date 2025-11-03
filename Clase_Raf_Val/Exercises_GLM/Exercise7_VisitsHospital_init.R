
df <- data.frame(
  BMI = c(
    18, 19, 20, 21, 22,
    23, 24, 25, 26, 27,
    28, 29, 30, 31, 32,
    33, 34, 35, 36, 37,
    38, 39, 40
  ),
  Visits = c(
    0, 0, 0, 0, 0,   # 18–22
    0, 1, 0, 1, 1,   # 23–27
    1, 1, 2, 2, 2,   # 28–32
    3, 3, 4, 4, 5,   # 33–37
    6, 7, 8          # 38–40
  )
)

ggplot(df, aes(BMI, Visits)) +
  geom_point(alpha = 0.7) +
  labs(title = "BMI vs Number of Hospital Visits",
       subtitle = "Points = observed counts\n",
       y = "Number of Visits", 
       x = "BMI") +
  theme_minimal()

