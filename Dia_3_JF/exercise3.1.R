library(pwr)
u1 <- 0.8
u2 <- 1
u3 <- 1.2
n <- 10
sigma <- 1
l <- 3
## effect size
grand_media <- mean(c(u1, u2, u3))
sigma_m2 <- mean( (c(u1, u2, u3) - grand_media)^2 )
sigma <- sqrt(sigma_m2)
effect <- sigma_m2 / sigma

#Un valor debe ser nulo para calcular ese parámetro
pwrr <- pwr.anova.test(k = l, n = n, f = effect, sig.level = 0.05, power = NULL)
print(pwrr)