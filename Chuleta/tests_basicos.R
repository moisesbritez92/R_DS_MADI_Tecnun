# ================================================
# Pruebas básicas en R: Shapiro -> t.test / Wilcoxon / Kruskal
# Uso rápido, como en el flujo del chat anterior
# ================================================

set.seed(123)

# -----------------------------
# 0) Mini "chuleta" (cheat sheet)
# -----------------------------
# 1 muestra (comparar con mu):
#   - Si Shapiro p>0.05  -> t.test(x, mu=...)
#   - Si Shapiro p<=0.05 -> wilcox.test(x, mu=...)
#
# 2 muestras independientes (x vs y):
#   - Si ambos Shapiro p>0.05 -> t.test(x, y)  # Welch por defecto (no exige varianzas iguales)
#   - Si alguno p<=0.05       -> wilcox.test(x, y)  # Mann-Whitney
#
# Pareado (pre vs post):
#   - Mirar normalidad de las diferencias (post-pre)
#   - Si Shapiro p>0.05  -> t.test(pre, post, paired=TRUE)
#   - Si Shapiro p<=0.05 -> wilcox.test(pre, post, paired=TRUE)
#
# >2 grupos:
#   - Si normalidad razonable por grupo -> aov(y ~ g)  (ANOVA)
#   - Si no -> kruskal.test(y ~ g)

# ------------------------------------
# 1) 1 muestra: t.test vs. Wilcoxon
# ------------------------------------
x <- rnorm(20, mean = 2, sd = 1)  # ejemplo
mu0 <- 2

cat("\n--- 1 MUESTRA ---\n")
sh <- shapiro.test(x)
print(sh)

if (sh$p.value > 0.05) {
  cat("Usar t.test (1 muestra)\n")
  print(t.test(x, mu = mu0))
} else {
  cat("Usar Wilcoxon (1 muestra)\n")
  print(wilcox.test(x, mu = mu0, conf.int = TRUE, exact = FALSE))
}

# ----------------------------------------------
# 2) 2 muestras independientes: Welch vs. MW
# ----------------------------------------------
x2 <- rnorm(20, mean = 2, sd = 1)
y2 <- rnorm(24, mean = 4, sd = 2)

cat("\n--- 2 MUESTRAS INDEPENDIENTES ---\n")
shx <- shapiro.test(x2); shy <- shapiro.test(y2)
print(list(p_shapiro_x = shx$p.value, p_shapiro_y = shy$p.value))

if (shx$p.value > 0.05 && shy$p.value > 0.05) {
  cat("Usar Welch Two Sample t-test\n")
  print(t.test(x2, y2))  # por defecto Welch
} else {
  cat("Usar Mann-Whitney (Wilcoxon rank-sum)\n")
  print(wilcox.test(x2, y2, conf.int = TRUE, exact = FALSE))
}

# -----------------------------
# 3) Pareado: t pareado vs. W
# -----------------------------
pre  <- rnorm(20, 10, 3)
post <- pre + rnorm(20, 1, 2)  # mejora media ~1
difs <- post - pre

cat("\n--- PAREADO ---\n")
shd <- shapiro.test(difs)
print(shd)

if (shd$p.value > 0.05) {
  cat("Usar t.test pareado\n")
  print(t.test(pre, post, paired = TRUE))
} else {
  cat("Usar Wilcoxon pareado\n")
  print(wilcox.test(pre, post, paired = TRUE, conf.int = TRUE, exact = FALSE))
}

# -------------------------------------
# 4) >2 grupos: ANOVA vs. Kruskal
# -------------------------------------
g1 <- rnorm(25, 0, 1)
g2 <- rnorm(25, 0.5, 1.2)
g3 <- rnorm(25, 1, 1.5)
y  <- c(g1, g2, g3)
g  <- factor(rep(c("g1","g2","g3"), each = 25))

cat("\n--- >2 GRUPOS ---\n")
p_norm <- tapply(y, g, function(v) shapiro.test(v)$p.value)
print(p_norm)

if (all(p_norm > 0.05)) {
  cat("Usar ANOVA\n")
  fit <- aov(y ~ g)
  print(anova(fit))
  cat("\nPost-hoc Tukey:\n")
  print(TukeyHSD(fit))
} else {
  cat("Usar Kruskal-Wallis\n")
  print(kruskal.test(y ~ g))
  cat("\nPost-hoc por pares (ajuste BH):\n")
  print(pairwise.wilcox.test(y, g, p.adjust.method = "BH"))
}

cat("\nListo. Fin del flujo básico.\n")
