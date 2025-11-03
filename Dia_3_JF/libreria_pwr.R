library(pwr)
u1 <- 3; u0 <- 0; sigma <- 10; alpha <- 0.05; beta <- 0.2 # nolint

#Effect size
d <- abs(u1 - u0)/sigma

# Power
power <- 1 - beta

# Calcularomos N usando la libreruaa pwr
N <- pwr.t.test(d = d, sig.level = alpha, power = power, type = "two.sample", alternative = "greater") # nolint
print(2 * N$n)