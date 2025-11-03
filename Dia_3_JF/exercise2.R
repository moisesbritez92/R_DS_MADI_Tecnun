u1 <- 3; u0 <- 0; sigma <- 10; alpha <- 0.05; beta <- 0.2 # nolint

#Effect size
d <- abs(u1 - u0)/sigma

# Power
power <- (1 - beta)
# Calcularomos N usando la formula
z_alpha <- abs(qnorm(alpha))
z_beta <- abs(qnorm(beta))
N <- 4 * ((z_alpha + z_beta)^2) / d^2
print(N)


