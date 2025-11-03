u1 <- 3; u0 <- 0; alpha <- 0.05; beta <- 0.2 # nolint
sigma <- seq(10,40,length.out = 40) # nolint
#Effect size
d <- abs(u1 - u0)/sigma

# Power
power <- (1 - beta)
# Calcularomos N usando la formula
z_alpha <- abs(qnorm(alpha))
z_beta <- abs(qnorm(beta))
N <- 4 * ((z_alpha + z_beta)^2) / d^2
df <- data.frame(sigma = sigma, N = N)

print(N)
# Cargar la librería ggplot2
library(ggplot2)
ggplot(df, aes(x = sigma, y = N)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  scale_y_continuous(breaks = seq(0, max(N), by = 50)) +
  labs(
    title = "Tamaño de muestra vs. Desviación estándar",
    x = "Desviación estándar (σ)",
    y = "Tamaño de muestra (N)"
  ) +
  theme_minimal()
