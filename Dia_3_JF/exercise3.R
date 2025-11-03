library(pwr)
u1 <- 0.8
u2 <- 1
u3 <- 1.2
n <- 10
sigma <- 1
l <- 3
f <- c("T1", "T2", "T3")
tm <- gl(l, 1, n*l, factor (f))

#Generar output valores
t1 <- rnorm(n, mean = u1, sd = sigma)
t2 <- rnorm(n, mean = u2, sd = sigma)
t3 <- rnorm(n, mean = u3, sd = sigma)
y <- matrix(c(t1, t2, t3), nrow = n, ncol = l)
y <- c(t(y))
#print(y)

aov <- aov(y ~ tm, projections = TRUE)
summary(aov)


