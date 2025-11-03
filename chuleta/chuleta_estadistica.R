# ═══════════════════════════════════════════════════════════════════════════════
# 📊 CHULETA COMPLETA DE ESTADÍSTICA EN R
# ═══════════════════════════════════════════════════════════════════════════════
# Guía Exhaustiva de Análisis Estadístico y Ciencia de Datos con R
# Compilado a partir del repositorio R_DS_MADI_Tecnun
# ═══════════════════════════════════════════════════════════════════════════════

# ===============================================================================
# 📦 1. LIBRERÍAS ESENCIALES
# ===============================================================================

# Descomente las líneas siguientes para instalar paquetes si es necesario
# install.packages(c("dplyr", "tidyr", "ggplot2", "readr", "stringr"))
# install.packages(c("viridis", "ggrepel", "gridExtra", "plotly"))
# install.packages(c("pwr", "car", "lmtest", "broom", "MASS"))

# Manipulación de datos
library(dplyr)          # Manipulación de datos
library(tidyr)          # Reestructuración de datos
library(stringr)        # Manipulación de strings
library(readr)          # Lectura rápida de datos

# Visualización
library(ggplot2)        # Gráficos avanzados
library(viridis)        # Paletas de colores
library(ggrepel)        # Etiquetas sin solapamiento
library(gridExtra)      # Múltiples gráficos
library(plotly)         # Gráficos interactivos 3D

# Estadística y modelado
library(pwr)            # Análisis de potencia estadística
library(car)            # Companion to Applied Regression
library(lmtest)         # Testing linear regression models
library(broom)          # Tidy model outputs
library(MASS)           # Funciones estadísticas avanzadas

# Configuración global
options(scipen = 999)   # Evitar notación científica
set.seed(12345)         # Reproducibilidad

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  CHULETA COMPLETA DE ESTADÍSTICA EN R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# ===============================================================================
# 📂 2. FUNDAMENTOS Y TIPOS DE DATOS
# ===============================================================================

cat("\n▶ 2. FUNDAMENTOS Y TIPOS DE DATOS\n")
cat("─────────────────────────────────────────\n")

# 2.1 Tipos de datos básicos
vector_numerico <- c(1, 2, 3, 4, 5)
vector_caracter <- c("A", "B", "C")
vector_logico <- c(TRUE, FALSE, TRUE)
factor_ejemplo <- factor(c("bajo", "medio", "alto"))

# 2.2 Estructuras de datos
matriz <- matrix(1:12, nrow = 3, ncol = 4)
lista <- list(numeros = 1:5, letras = c("a", "b"), logico = TRUE)
data_frame <- data.frame(
  id = 1:5,
  nombre = c("Ana", "Juan", "María", "Pedro", "Luis"),
  edad = c(25, 30, 28, 35, 22),
  salario = c(2500, 3000, 2800, 3500, 2200),
  stringsAsFactors = FALSE
)

# 2.3 Exploración de data frames
cat("\nEstructura del data frame:\n")
str(data_frame)
cat("\nResumen estadístico:\n")
summary(data_frame)
cat("\nPrimeras filas:\n")
head(data_frame, 3)
cat("\nDimensiones (filas x columnas):", dim(data_frame), "\n")

# 2.4 Indexación y selección
cat("\nEjemplos de indexación:\n")
cat("Primera fila:", data_frame[1, ], "\n")
cat("Columna edad:", data_frame$edad, "\n")
cat("Filtro edad > 25:", data_frame[data_frame$edad > 25, "nombre"], "\n")

# ===============================================================================
# 📊 3. ESTADÍSTICA DESCRIPTIVA
# ===============================================================================

cat("\n▶ 3. ESTADÍSTICA DESCRIPTIVA\n")
cat("─────────────────────────────────────────\n")

# Generar datos de ejemplo
datos_ejemplo <- rnorm(100, mean = 50, sd = 10)

# 3.1 Medidas de tendencia central
media <- mean(datos_ejemplo)
mediana <- median(datos_ejemplo)
moda_aprox <- as.numeric(names(sort(table(round(datos_ejemplo)), decreasing = TRUE)[1]))

cat(sprintf("\nMedidas de Tendencia Central:\n"))
cat(sprintf("  Media: %.2f\n", media))
cat(sprintf("  Mediana: %.2f\n", mediana))

# 3.2 Medidas de dispersión
desv_std <- sd(datos_ejemplo)
varianza <- var(datos_ejemplo)
rango <- max(datos_ejemplo) - min(datos_ejemplo)
iqr <- IQR(datos_ejemplo)
coef_variacion <- (desv_std / media) * 100

cat(sprintf("\nMedidas de Dispersión:\n"))
cat(sprintf("  Desviación estándar: %.2f\n", desv_std))
cat(sprintf("  Varianza: %.2f\n", varianza))
cat(sprintf("  Rango: %.2f\n", rango))
cat(sprintf("  Rango intercuartílico (IQR): %.2f\n", iqr))
cat(sprintf("  Coeficiente de variación: %.2f%%\n", coef_variacion))

# 3.3 Medidas de posición
cuartiles <- quantile(datos_ejemplo, probs = c(0.25, 0.5, 0.75))
percentiles <- quantile(datos_ejemplo, probs = c(0.1, 0.9))

cat(sprintf("\nCuartiles: Q1=%.2f, Q2=%.2f, Q3=%.2f\n", 
           cuartiles[1], cuartiles[2], cuartiles[3]))
cat(sprintf("Percentiles 10 y 90: %.2f, %.2f\n", percentiles[1], percentiles[2]))

# 3.4 Medidas de forma
library(moments)  # Si está disponible
asimetria <- (mean(datos_ejemplo) - median(datos_ejemplo)) / sd(datos_ejemplo)
cat(sprintf("\nAsimetría aproximada: %.3f\n", asimetria))
cat("  > 0: Asimetría positiva (cola derecha)\n")
cat("  < 0: Asimetría negativa (cola izquierda)\n")
cat("  ≈ 0: Distribución simétrica\n")

# 3.5 Tablas de frecuencias
datos_categoricos <- sample(c("A", "B", "C", "D"), 100, replace = TRUE)
tabla_frecuencias <- table(datos_categoricos)
tabla_proporciones <- prop.table(tabla_frecuencias)

cat("\nTabla de Frecuencias:\n")
print(tabla_frecuencias)
cat("\nProporciones:\n")
print(round(tabla_proporciones, 3))

# ===============================================================================
# 🎲 4. PROBABILIDAD Y DISTRIBUCIONES
# ===============================================================================

cat("\n▶ 4. PROBABILIDAD Y DISTRIBUCIONES\n")
cat("─────────────────────────────────────────\n")

# 4.1 Distribución Normal
cat("\n4.1 DISTRIBUCIÓN NORMAL N(μ, σ²)\n")
cat("Funciones: dnorm (densidad), pnorm (probabilidad), qnorm (cuantil), rnorm (aleatorio)\n")

# Ejemplos con N(50, 10²)
mu <- 50
sigma <- 10

# Probabilidades
prob_menor_45 <- pnorm(45, mean = mu, sd = sigma)
prob_mayor_60 <- 1 - pnorm(60, mean = mu, sd = sigma)
prob_entre_40_60 <- pnorm(60, mu, sigma) - pnorm(40, mu, sigma)

cat(sprintf("  P(X < 45) = %.4f\n", prob_menor_45))
cat(sprintf("  P(X > 60) = %.4f\n", prob_mayor_60))
cat(sprintf("  P(40 < X < 60) = %.4f\n", prob_entre_40_60))

# Cuantiles (valores críticos)
cuantil_95 <- qnorm(0.95, mean = mu, sd = sigma)
cat(sprintf("  Cuantil 95%%: %.2f\n", cuantil_95))

# Generar valores aleatorios
valores_normales <- rnorm(1000, mean = mu, sd = sigma)

# 4.2 Distribución t de Student
cat("\n4.2 DISTRIBUCIÓN t DE STUDENT\n")
cat("Usada cuando σ es desconocida o muestra pequeña (n < 30)\n")

gl <- 15  # Grados de libertad
t_critico_95 <- qt(0.975, df = gl)  # Bilateral al 95%
cat(sprintf("  Valor t crítico (95%%, gl=%d): %.3f\n", gl, t_critico_95))

# 4.3 Distribución Chi-cuadrado
cat("\n4.3 DISTRIBUCIÓN CHI-CUADRADO\n")
cat("Usada para pruebas de bondad de ajuste e independencia\n")

chi_critico <- qchisq(0.95, df = 5)
cat(sprintf("  χ² crítico (95%%, gl=5): %.3f\n", chi_critico))

# 4.4 Distribución F
cat("\n4.4 DISTRIBUCIÓN F\n")
cat("Usada en ANOVA y comparación de varianzas\n")

f_critico <- qf(0.95, df1 = 3, df2 = 20)
cat(sprintf("  F crítico (95%%, gl1=3, gl2=20): %.3f\n", f_critico))

# 4.5 Distribución Binomial
cat("\n4.5 DISTRIBUCIÓN BINOMIAL\n")
cat("Número de éxitos en n ensayos independientes\n")

n_ensayos <- 10
prob_exito <- 0.3
prob_exactamente_3 <- dbinom(3, size = n_ensayos, prob = prob_exito)
prob_hasta_3 <- pbinom(3, size = n_ensayos, prob = prob_exito)

cat(sprintf("  P(X = 3 | n=10, p=0.3) = %.4f\n", prob_exactamente_3))
cat(sprintf("  P(X ≤ 3 | n=10, p=0.3) = %.4f\n", prob_hasta_3))

# 4.6 Distribución de Poisson
cat("\n4.6 DISTRIBUCIÓN DE POISSON\n")
cat("Número de eventos en intervalo de tiempo/espacio\n")

lambda <- 5  # Tasa media
prob_poisson_3 <- dpois(3, lambda = lambda)
cat(sprintf("  P(X = 3 | λ=5) = %.4f\n", prob_poisson_3))

# ===============================================================================
# 🔬 5. INFERENCIA ESTADÍSTICA
# ===============================================================================

cat("\n▶ 5. INFERENCIA ESTADÍSTICA\n")
cat("─────────────────────────────────────────\n")

# 5.1 Intervalos de Confianza para la Media
cat("\n5.1 INTERVALOS DE CONFIANZA PARA LA MEDIA\n")

muestra <- rnorm(30, mean = 170, sd = 8)
n <- length(muestra)
media_m <- mean(muestra)
sd_m <- sd(muestra)
error_std <- sd_m / sqrt(n)

# IC usando distribución t
t_val <- qt(0.975, df = n-1)
margen_error <- t_val * error_std
ic_inferior <- media_m - margen_error
ic_superior <- media_m + margen_error

cat(sprintf("  Media muestral: %.2f\n", media_m))
cat(sprintf("  Error estándar: %.3f\n", error_std))
cat(sprintf("  IC 95%%: [%.2f, %.2f]\n", ic_inferior, ic_superior))

# Verificación con t.test
ic_resultado <- t.test(muestra, conf.level = 0.95)$conf.int
cat(sprintf("  Verificación: [%.2f, %.2f]\n", ic_resultado[1], ic_resultado[2]))

# 5.2 Test t para una muestra
cat("\n5.2 TEST t PARA UNA MUESTRA\n")
cat("H0: μ = μ0  vs  H1: μ ≠ μ0\n")

mu_0 <- 175
resultado_t <- t.test(muestra, mu = mu_0)
cat(sprintf("  Estadístico t: %.3f\n", resultado_t$statistic))
cat(sprintf("  p-value: %.4f\n", resultado_t$p.value))
cat(sprintf("  Decisión: %s\n", 
           ifelse(resultado_t$p.value < 0.05, 
                  "Rechazar H0 (hay evidencia significativa)", 
                  "No rechazar H0")))

# 5.3 Test t para dos muestras independientes
cat("\n5.3 TEST t PARA DOS MUESTRAS INDEPENDIENTES\n")

grupo1 <- rnorm(25, mean = 170, sd = 8)
grupo2 <- rnorm(30, mean = 165, sd = 7)

resultado_2grupos <- t.test(grupo1, grupo2)
cat(sprintf("  Grupo 1: n=%d, media=%.2f\n", length(grupo1), mean(grupo1)))
cat(sprintf("  Grupo 2: n=%d, media=%.2f\n", length(grupo2), mean(grupo2)))
cat(sprintf("  Diferencia de medias: %.2f\n", mean(grupo1) - mean(grupo2)))
cat(sprintf("  t = %.3f, p = %.4f\n", 
           resultado_2grupos$statistic, resultado_2grupos$p.value))

# 5.4 Test t pareado
cat("\n5.4 TEST t PAREADO (Antes-Después)\n")

antes <- rnorm(20, mean = 80, sd = 10)
despues <- antes + rnorm(20, mean = 5, sd = 3)  # Mejora de ~5 unidades

resultado_pareado <- t.test(despues, antes, paired = TRUE)
cat(sprintf("  Media antes: %.2f\n", mean(antes)))
cat(sprintf("  Media después: %.2f\n", mean(despues)))
cat(sprintf("  Diferencia media: %.2f\n", mean(despues - antes)))
cat(sprintf("  t = %.3f, p = %.4f\n", 
           resultado_pareado$statistic, resultado_pareado$p.value))

# 5.5 Test de proporción
cat("\n5.5 TEST DE PROPORCIÓN\n")

exitos <- 75
n_total <- 100
p_0 <- 0.7  # Proporción bajo H0

resultado_prop <- prop.test(exitos, n_total, p = p_0)
cat(sprintf("  Proporción muestral: %.3f\n", exitos/n_total))
cat(sprintf("  H0: p = %.2f\n", p_0))
cat(sprintf("  χ² = %.3f, p = %.4f\n", 
           resultado_prop$statistic, resultado_prop$p.value))

# 5.6 ANOVA de una vía
cat("\n5.6 ANOVA DE UNA VÍA (Comparar más de 2 grupos)\n")

grupo_a <- rnorm(15, mean = 20, sd = 3)
grupo_b <- rnorm(15, mean = 25, sd = 3)
grupo_c <- rnorm(15, mean = 22, sd = 3)

datos_anova <- data.frame(
  valor = c(grupo_a, grupo_b, grupo_c),
  grupo = factor(rep(c("A", "B", "C"), each = 15))
)

modelo_anova <- aov(valor ~ grupo, data = datos_anova)
summary_anova <- summary(modelo_anova)

cat(sprintf("  Media Grupo A: %.2f\n", mean(grupo_a)))
cat(sprintf("  Media Grupo B: %.2f\n", mean(grupo_b)))
cat(sprintf("  Media Grupo C: %.2f\n", mean(grupo_c)))
cat("\n")
print(summary_anova)

# Post-hoc: Prueba de Tukey
cat("\nComparaciones post-hoc (Tukey HSD):\n")
tukey_result <- TukeyHSD(modelo_anova)
print(tukey_result)

# 5.7 Test Chi-cuadrado de independencia
cat("\n5.7 TEST CHI-CUADRADO DE INDEPENDENCIA\n")

tabla_contingencia <- matrix(c(20, 30, 25, 45), nrow = 2,
                            dimnames = list(c("Tratamiento", "Control"),
                                          c("Éxito", "Fracaso")))
cat("Tabla de contingencia:\n")
print(tabla_contingencia)

resultado_chi2 <- chisq.test(tabla_contingencia)
cat(sprintf("\nχ² = %.3f, p = %.4f\n", 
           resultado_chi2$statistic, resultado_chi2$p.value))

# 5.8 Test exacto de Fisher
cat("\n5.8 TEST EXACTO DE FISHER\n")
cat("(Para tablas 2x2 con frecuencias pequeñas)\n")

resultado_fisher <- fisher.test(tabla_contingencia)
cat(sprintf("  p-value = %.4f\n", resultado_fisher$p.value))
cat(sprintf("  Odds Ratio = %.3f\n", resultado_fisher$estimate))

# 5.9 Correlación de Pearson
cat("\n5.9 CORRELACIÓN DE PEARSON\n")

x_var <- 1:50
y_var <- 2 * x_var + 5 + rnorm(50, mean = 0, sd = 10)

test_cor <- cor.test(x_var, y_var)
cat(sprintf("  Coeficiente r: %.3f\n", test_cor$estimate))
cat(sprintf("  p-value: %.4f\n", test_cor$p.value))
cat(sprintf("  IC 95%%: [%.3f, %.3f]\n", 
           test_cor$conf.int[1], test_cor$conf.int[2]))

# Interpretación de correlación
cat("\nInterpretación de |r|:\n")
cat("  0.00-0.19: Muy débil\n")
cat("  0.20-0.39: Débil\n")
cat("  0.40-0.59: Moderada\n")
cat("  0.60-0.79: Fuerte\n")
cat("  0.80-1.00: Muy fuerte\n")

# ===============================================================================
# 📈 6. REGRESIÓN
# ===============================================================================

cat("\n▶ 6. REGRESIÓN\n")
cat("─────────────────────────────────────────\n")

# 6.1 Regresión Lineal Simple
cat("\n6.1 REGRESIÓN LINEAL SIMPLE\n")
cat("Modelo: Y = β₀ + β₁X + ε\n")

# Generar datos con relación lineal
x <- rnorm(100, mean = 10, sd = 2)
y <- 3 + 2.5*x + rnorm(100, mean = 0, sd = 3)
datos_reg <- data.frame(x = x, y = y)

# Ajustar modelo
modelo_simple <- lm(y ~ x, data = datos_reg)
summary_modelo <- summary(modelo_simple)

cat("\nCoeficientes:\n")
cat(sprintf("  β₀ (Intercepto): %.3f (p=%.4f)\n", 
           coef(modelo_simple)[1], 
           summary_modelo$coefficients[1,4]))
cat(sprintf("  β₁ (Pendiente): %.3f (p=%.4f)\n", 
           coef(modelo_simple)[2], 
           summary_modelo$coefficients[2,4]))

cat(sprintf("\nBondad de ajuste:\n"))
cat(sprintf("  R²: %.4f\n", summary_modelo$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary_modelo$adj.r.squared))
cat(sprintf("  Error estándar residual: %.3f\n", summary_modelo$sigma))
cat(sprintf("  F-statistic: %.2f (p=%.4f)\n", 
           summary_modelo$fstatistic[1], 
           pf(summary_modelo$fstatistic[1], 
              summary_modelo$fstatistic[2], 
              summary_modelo$fstatistic[3], 
              lower.tail = FALSE)))

# Intervalos de confianza para coeficientes
ic_coef <- confint(modelo_simple, level = 0.95)
cat("\nIntervalos de confianza 95%:\n")
print(ic_coef)

# Predicciones
nuevos_datos <- data.frame(x = c(8, 10, 12))
predicciones <- predict(modelo_simple, newdata = nuevos_datos, interval = "prediction")
cat("\nPredicciones:\n")
print(predicciones)

# 6.2 Diagnóstico de regresión
cat("\n6.2 DIAGNÓSTICO DE REGRESIÓN\n")

# Residuos
residuos <- residuals(modelo_simple)
valores_ajustados <- fitted(modelo_simple)

# Prueba de normalidad de residuos
shapiro_residuos <- shapiro.test(residuos)
cat(sprintf("  Normalidad de residuos (Shapiro-Wilk): p=%.4f\n", 
           shapiro_residuos$p.value))

# Prueba de homocedasticidad (Breusch-Pagan)
bp_test <- bptest(modelo_simple)
cat(sprintf("  Homocedasticidad (Breusch-Pagan): p=%.4f\n", bp_test$p.value))

# Prueba de autocorrelación (Durbin-Watson)
dw_test <- dwtest(modelo_simple)
cat(sprintf("  Autocorrelación (Durbin-Watson): DW=%.3f, p=%.4f\n", 
           dw_test$statistic, dw_test$p.value))

# 6.3 Regresión Lineal Múltiple
cat("\n6.3 REGRESIÓN LINEAL MÚLTIPLE\n")
cat("Modelo: Y = β₀ + β₁X₁ + β₂X₂ + ... + βₖXₖ + ε\n")

# Generar datos con múltiples predictores
x1 <- rnorm(100, 10, 2)
x2 <- rnorm(100, 5, 1)
x3 <- rnorm(100, 15, 3)
y_multi <- 5 + 2*x1 - 1.5*x2 + 0.8*x3 + rnorm(100, 0, 4)
datos_multi <- data.frame(y = y_multi, x1 = x1, x2 = x2, x3 = x3)

modelo_multiple <- lm(y ~ x1 + x2 + x3, data = datos_multi)
summary_multi <- summary(modelo_multiple)

cat("\nResumen del modelo múltiple:\n")
print(summary_multi$coefficients)

cat(sprintf("\nR² ajustado: %.4f\n", summary_multi$adj.r.squared))

# Multicolinealidad (VIF)
vif_values <- vif(modelo_multiple)
cat("\nFactores de Inflación de Varianza (VIF):\n")
print(vif_values)
cat("  VIF < 5: No hay multicolinealidad preocupante\n")
cat("  VIF 5-10: Multicolinealidad moderada\n")
cat("  VIF > 10: Multicolinealidad severa\n")

# 6.4 Regresión Logística
cat("\n6.4 REGRESIÓN LOGÍSTICA\n")
cat("Para variable respuesta binaria (0/1)\n")

# Generar datos binarios
x_log <- rnorm(200, 0, 1)
prob <- 1 / (1 + exp(-(1 + 2*x_log)))
y_log <- rbinom(200, 1, prob)
datos_logistic <- data.frame(y = y_log, x = x_log)

# Ajustar modelo logístico
modelo_logistic <- glm(y ~ x, data = datos_logistic, family = binomial(link = "logit"))
summary_logistic <- summary(modelo_logistic)

cat("\nCoeficientes del modelo logístico:\n")
print(summary_logistic$coefficients)

# Odds Ratios
odds_ratios <- exp(coef(modelo_logistic))
cat("\nOdds Ratios:\n")
print(odds_ratios)
cat("\nInterpretación: Por cada unidad de incremento en X,\n")
cat(sprintf("el odds de Y=1 se multiplica por %.3f\n", odds_ratios[2]))

# Predicciones de probabilidad
nuevos_x <- data.frame(x = c(-1, 0, 1))
pred_prob <- predict(modelo_logistic, newdata = nuevos_x, type = "response")
cat("\nProbabilidades predichas:\n")
print(data.frame(x = nuevos_x$x, probabilidad = pred_prob))

# 6.5 Regresión de Poisson
cat("\n6.5 REGRESIÓN DE POISSON\n")
cat("Para variable respuesta de conteo\n")

# Generar datos de conteo
x_pois <- rnorm(100, 5, 2)
lambda <- exp(1 + 0.3*x_pois)
y_pois <- rpois(100, lambda)
datos_poisson <- data.frame(y = y_pois, x = x_pois)

modelo_poisson <- glm(y ~ x, data = datos_poisson, family = poisson(link = "log"))
summary_poisson <- summary(modelo_poisson)

cat("\nCoeficientes del modelo Poisson:\n")
print(summary_poisson$coefficients)

# ===============================================================================
# 🔍 7. ANÁLISIS MULTIVARIANTE
# ===============================================================================

cat("\n▶ 7. ANÁLISIS MULTIVARIANTE\n")
cat("─────────────────────────────────────────\n")

# 7.1 Análisis de Componentes Principales (PCA)
cat("\n7.1 ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)\n")

# Generar datos multivariantes
n <- 100
x1_pca <- rnorm(n, 10, 2)
x2_pca <- x1_pca * 2.4 + rnorm(n, 0, 1)
x3_pca <- x1_pca * (-2) + rnorm(n, 0, 1.5)
x4_pca <- rnorm(n, 20, 3)

datos_pca <- data.frame(V1 = x1_pca, V2 = x2_pca, V3 = x3_pca, V4 = x4_pca)

# Realizar PCA
pca_resultado <- prcomp(datos_pca, scale. = TRUE, center = TRUE)

# Varianza explicada
varianza_explicada <- summary(pca_resultado)$importance[2, ] * 100
varianza_acumulada <- summary(pca_resultado)$importance[3, ] * 100

cat("\nVarianza explicada por componente:\n")
for(i in 1:length(varianza_explicada)) {
  cat(sprintf("  PC%d: %.2f%% (Acumulada: %.2f%%)\n", 
             i, varianza_explicada[i], varianza_acumulada[i]))
}

# Loadings (cargas)
cat("\nLoadings (contribución de variables a PC1 y PC2):\n")
print(pca_resultado$rotation[, 1:2])

# Scores (coordenadas de observaciones en nuevo espacio)
pca_scores <- as.data.frame(pca_resultado$x[, 1:2])
cat(sprintf("\nPrimeras 5 observaciones en espacio PC:\n"))
print(head(pca_scores, 5))

# ===============================================================================
# 📉 8. PRUEBAS NO PARAMÉTRICAS
# ===============================================================================

cat("\n▶ 8. PRUEBAS NO PARAMÉTRICAS\n")
cat("─────────────────────────────────────────\n")
cat("(Para datos que no cumplen supuestos de normalidad)\n")

# 8.1 Prueba de Wilcoxon (Mann-Whitney U)
cat("\n8.1 PRUEBA DE WILCOXON/MANN-WHITNEY\n")
cat("Alternativa no paramétrica al test t de dos muestras\n")

grupo_a_np <- c(23, 25, 28, 30, 32, 35, 38, 22, 27, 29)
grupo_b_np <- c(18, 20, 22, 24, 26, 29, 31, 19, 21, 23)

resultado_wilcox <- wilcox.test(grupo_a_np, grupo_b_np)
cat(sprintf("  W = %.1f, p = %.4f\n", 
           resultado_wilcox$statistic, resultado_wilcox$p.value))

# 8.2 Prueba de Kruskal-Wallis
cat("\n8.2 PRUEBA DE KRUSKAL-WALLIS\n")
cat("Alternativa no paramétrica a ANOVA de una vía\n")

datos_kruskal <- data.frame(
  valor = c(grupo_a, grupo_b, grupo_c),
  grupo = factor(rep(c("A", "B", "C"), each = 15))
)

resultado_kruskal <- kruskal.test(valor ~ grupo, data = datos_kruskal)
cat(sprintf("  χ² = %.3f, gl = %d, p = %.4f\n", 
           resultado_kruskal$statistic, 
           resultado_kruskal$parameter, 
           resultado_kruskal$p.value))

# Post-hoc para Kruskal-Wallis
cat("\nComparaciones post-hoc (Wilcoxon pareado con ajuste):\n")
pairwise_result <- pairwise.wilcox.test(datos_kruskal$valor, 
                                       datos_kruskal$grupo, 
                                       p.adjust.method = "BH")
print(pairwise_result)

# 8.3 Prueba de Shapiro-Wilk (Normalidad)
cat("\n8.3 PRUEBA DE SHAPIRO-WILK (Normalidad)\n")

datos_normal <- rnorm(50, 100, 15)
datos_no_normal <- rexp(50, 0.1)

shapiro_normal <- shapiro.test(datos_normal)
shapiro_no_normal <- shapiro.test(datos_no_normal)

cat(sprintf("  Datos normales: W = %.4f, p = %.4f\n", 
           shapiro_normal$statistic, shapiro_normal$p.value))
cat(sprintf("  Datos no normales: W = %.4f, p = %.4f\n", 
           shapiro_no_normal$statistic, shapiro_no_normal$p.value))
cat("\n  Si p > 0.05: No se rechaza normalidad\n")
cat("  Si p ≤ 0.05: Se rechaza normalidad (usar pruebas no paramétricas)\n")

# ===============================================================================
# 🔬 9. CORRECCIÓN POR COMPARACIONES MÚLTIPLES
# ===============================================================================

cat("\n▶ 9. CORRECCIÓN POR COMPARACIONES MÚLTIPLES\n")
cat("─────────────────────────────────────────\n")

# Cuando realizamos múltiples pruebas, incrementamos el riesgo de 
# Error Tipo I (falsos positivos)

p_values <- c(0.001, 0.01, 0.03, 0.05, 0.08, 0.12, 0.25)

# 9.1 Bonferroni (muy conservador)
p_bonferroni <- p.adjust(p_values, method = "bonferroni")

# 9.2 Benjamini-Hochberg / FDR (recomendado)
p_bh <- p.adjust(p_values, method = "BH")

# 9.3 Holm (menos conservador que Bonferroni)
p_holm <- p.adjust(p_values, method = "holm")

# Tabla comparativa
comparacion_ajustes <- data.frame(
  Test = 1:length(p_values),
  P_original = p_values,
  P_Bonferroni = p_bonferroni,
  P_BH = p_bh,
  P_Holm = p_holm,
  Sig_original = ifelse(p_values < 0.05, "*", ""),
  Sig_BH = ifelse(p_bh < 0.05, "*", "")
)

cat("\nComparación de métodos de ajuste:\n")
print(comparacion_ajustes)

cat("\nInterpretación:\n")
cat("  Bonferroni: Más estricto, controla FWER (Family-Wise Error Rate)\n")
cat("  BH (FDR): Balance entre potencia y control de falsos positivos\n")
cat("  Holm: Intermedio entre Bonferroni y sin ajuste\n")

# ===============================================================================
# 💪 10. ANÁLISIS DE POTENCIA ESTADÍSTICA
# ===============================================================================

cat("\n▶ 10. ANÁLISIS DE POTENCIA ESTADÍSTICA\n")
cat("─────────────────────────────────────────\n")

# La potencia es la probabilidad de detectar un efecto cuando existe
# Potencia = 1 - β (donde β es probabilidad de Error Tipo II)

# 10.1 Potencia para test t
cat("\n10.1 POTENCIA PARA TEST t\n")

# ¿Qué tamaño de muestra necesito para detectar d=0.5 con potencia=0.80?
power_t_n <- pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80, 
                       type = "two.sample")
cat(sprintf("  Tamaño muestral por grupo: n = %d\n", ceiling(power_t_n$n)))

# Con n=30, ¿cuál es la potencia para detectar d=0.5?
power_t_pow <- pwr.t.test(n = 30, d = 0.5, sig.level = 0.05, 
                         type = "two.sample")
cat(sprintf("  Potencia con n=30: %.3f\n", power_t_pow$power))

# 10.2 Potencia para correlación
cat("\n10.2 POTENCIA PARA CORRELACIÓN\n")

# ¿Qué n necesito para detectar r=0.5 con potencia=0.80?
power_r_n <- pwr.r.test(r = 0.5, sig.level = 0.05, power = 0.80)
cat(sprintf("  Tamaño muestral: n = %d\n", ceiling(power_r_n$n)))

# 10.3 Potencia para ANOVA
cat("\n10.3 POTENCIA PARA ANOVA\n")

# Para detectar f=0.25 con 3 grupos y potencia=0.80
power_anova_n <- pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.80)
cat(sprintf("  Tamaño muestral por grupo: n = %d\n", ceiling(power_anova_n$n)))

# 10.4 Potencia para proporciones
cat("\n10.4 POTENCIA PARA PROPORCIONES\n")

# Detectar diferencia entre p1=0.3 y p2=0.5
h <- ES.h(0.3, 0.5)  # Tamaño del efecto h de Cohen
power_prop_n <- pwr.2p.test(h = h, sig.level = 0.05, power = 0.80)
cat(sprintf("  Tamaño muestral por grupo: n = %d\n", ceiling(power_prop_n$n)))

cat("\nTamaños de efecto de Cohen:\n")
cat("  d (diferencia de medias): pequeño=0.2, medio=0.5, grande=0.8\n")
cat("  r (correlación): pequeño=0.1, medio=0.3, grande=0.5\n")
cat("  f (ANOVA): pequeño=0.1, medio=0.25, grande=0.4\n")

# ===============================================================================
# 📊 11. VISUALIZACIÓN CON GGPLOT2
# ===============================================================================

cat("\n▶ 11. VISUALIZACIÓN CON GGPLOT2\n")
cat("─────────────────────────────────────────\n")

# 11.1 Scatter plot básico
datos_vis <- data.frame(
  x = rnorm(100, 50, 10),
  y = rnorm(100, 30, 5),
  grupo = sample(c("A", "B", "C"), 100, replace = TRUE)
)

p1 <- ggplot(datos_vis, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black") +
  labs(title = "Gráfico de Dispersión",
       x = "Variable X", y = "Variable Y") +
  theme_minimal() +
  scale_color_viridis_d()

cat("\n11.1 Scatter plot creado (p1)\n")

# 11.2 Boxplot
p2 <- ggplot(datos_vis, aes(x = grupo, y = y, fill = grupo)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Boxplot por Grupo",
       x = "Grupo", y = "Valor Y") +
  theme_minimal() +
  scale_fill_viridis_d()

cat("11.2 Boxplot creado (p2)\n")

# 11.3 Histograma con curva de densidad
p3 <- ggplot(datos_vis, aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 20, 
                fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Histograma con Densidad",
       x = "Variable X", y = "Densidad") +
  theme_minimal()

cat("11.3 Histograma creado (p3)\n")

# 11.4 Facetas (múltiples paneles)
p4 <- ggplot(datos_vis, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ grupo, scales = "free") +
  labs(title = "Gráficos por Grupo (Facetas)",
       x = "Variable X", y = "Variable Y") +
  theme_bw()

cat("11.4 Gráfico con facetas creado (p4)\n")

# 11.5 Gráfico de barras
datos_barras <- datos_vis %>%
  group_by(grupo) %>%
  summarise(
    media = mean(y),
    se = sd(y)/sqrt(n()),
    .groups = "drop"
  )

p5 <- ggplot(datos_barras, aes(x = grupo, y = media, fill = grupo)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), 
               width = 0.2) +
  labs(title = "Medias por Grupo con Error Estándar",
       x = "Grupo", y = "Media ± SE") +
  theme_minimal() +
  scale_fill_viridis_d()

cat("11.5 Gráfico de barras creado (p5)\n")

# 11.6 Heatmap de correlaciones
matriz_cor <- cor(datos_pca)
datos_heatmap <- reshape2::melt(matriz_cor)

p6 <- ggplot(datos_heatmap, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                      midpoint = 0, limit = c(-1,1)) +
  labs(title = "Matriz de Correlaciones",
       x = "", y = "", fill = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("11.6 Heatmap de correlaciones creado (p6)\n")

cat("\nPara visualizar los gráficos, ejecute: print(p1), print(p2), etc.\n")
cat("Para guardar: ggsave('grafico.png', p1, width=10, height=6, dpi=300)\n")

# ===============================================================================
# 🎯 12. FUNCIONES REUTILIZABLES
# ===============================================================================

cat("\n▶ 12. FUNCIONES REUTILIZABLES\n")
cat("─────────────────────────────────────────\n")

# 12.1 Función para estadísticas descriptivas completas
estadisticas_completas <- function(x, nombre_var = "Variable") {
  cat(sprintf("\n═══ Estadísticas de %s ═══\n", nombre_var))
  cat(sprintf("n: %d\n", length(x)))
  cat(sprintf("Media: %.3f\n", mean(x, na.rm = TRUE)))
  cat(sprintf("Mediana: %.3f\n", median(x, na.rm = TRUE)))
  cat(sprintf("Desv. Std: %.3f\n", sd(x, na.rm = TRUE)))
  cat(sprintf("Min: %.3f\n", min(x, na.rm = TRUE)))
  cat(sprintf("Max: %.3f\n", max(x, na.rm = TRUE)))
  cat(sprintf("Q1: %.3f\n", quantile(x, 0.25, na.rm = TRUE)))
  cat(sprintf("Q3: %.3f\n", quantile(x, 0.75, na.rm = TRUE)))
  cat(sprintf("IQR: %.3f\n", IQR(x, na.rm = TRUE)))
  cat(sprintf("NAs: %d\n", sum(is.na(x))))
}

# Ejemplo de uso
estadisticas_completas(datos_ejemplo, "Datos de Ejemplo")

# 12.2 Función para test t automático con Shapiro previo
test_t_automatico <- function(grupo1, grupo2, alpha = 0.05) {
  cat("\n═══ TEST t AUTOMÁTICO ═══\n")
  
  # Pruebas de normalidad
  shap1 <- shapiro.test(grupo1)
  shap2 <- shapiro.test(grupo2)
  
  cat(sprintf("Normalidad grupo 1: p = %.4f\n", shap1$p.value))
  cat(sprintf("Normalidad grupo 2: p = %.4f\n", shap2$p.value))
  
  if(shap1$p.value > alpha && shap2$p.value > alpha) {
    cat("\n→ Usando test t (datos normales)\n")
    resultado <- t.test(grupo1, grupo2)
  } else {
    cat("\n→ Usando Wilcoxon/Mann-Whitney (datos no normales)\n")
    resultado <- wilcox.test(grupo1, grupo2)
  }
  
  print(resultado)
  return(resultado)
}

# 12.3 Función para análisis completo de regresión
analisis_regresion_completo <- function(modelo) {
  cat("\n═══ ANÁLISIS COMPLETO DE REGRESIÓN ═══\n")
  
  # Resumen
  cat("\n1. RESUMEN DEL MODELO:\n")
  print(summary(modelo))
  
  # Intervalos de confianza
  cat("\n2. INTERVALOS DE CONFIANZA (95%):\n")
  print(confint(modelo))
  
  # Diagnósticos
  cat("\n3. PRUEBAS DE DIAGNÓSTICO:\n")
  
  # Normalidad de residuos
  residuos <- residuals(modelo)
  shap <- shapiro.test(residuos)
  cat(sprintf("  Normalidad (Shapiro-Wilk): W=%.4f, p=%.4f\n", 
             shap$statistic, shap$p.value))
  
  # Homocedasticidad
  bp <- bptest(modelo)
  cat(sprintf("  Homocedasticidad (BP): BP=%.3f, p=%.4f\n", 
             bp$statistic, bp$p.value))
  
  # Autocorrelación
  dw <- dwtest(modelo)
  cat(sprintf("  Autocorrelación (DW): DW=%.3f, p=%.4f\n", 
             dw$statistic, dw$p.value))
  
  # VIF (si es múltiple)
  if(length(coef(modelo)) > 2) {
    cat("\n4. MULTICOLINEALIDAD (VIF):\n")
    print(vif(modelo))
  }
}

# 12.4 Función para crear tabla de resultados
crear_tabla_resultados <- function(modelos_lista, nombres) {
  require(broom)
  
  resultados <- lapply(modelos_lista, function(m) {
    tidy(m) %>%
      select(term, estimate, std.error, statistic, p.value)
  })
  
  for(i in seq_along(resultados)) {
    cat(sprintf("\n%s:\n", nombres[i]))
    print(resultados[[i]])
  }
}

cat("\n12.1-12.4 Funciones reutilizables definidas\n")

# ===============================================================================
# 📚 13. BUENAS PRÁCTICAS Y CONSEJOS
# ===============================================================================

cat("\n▶ 13. BUENAS PRÁCTICAS Y CONSEJOS\n")
cat("─────────────────────────────────────────\n")

cat("\n13.1 FLUJO DE TRABAJO RECOMENDADO:\n")
cat("  1. Exploración de datos (str, summary, visualización)\n")
cat("  2. Limpieza de datos (NAs, outliers, transformaciones)\n")
cat("  3. Análisis descriptivo (medias, desviaciones, correlaciones)\n")
cat("  4. Verificar supuestos (normalidad, homocedasticidad)\n")
cat("  5. Análisis inferencial (pruebas de hipótesis)\n")
cat("  6. Modelado (regresión, GLM, etc.)\n")
cat("  7. Diagnóstico de modelos (residuos, VIF, R²)\n")
cat("  8. Interpretación y conclusiones\n")
cat("  9. Visualización de resultados\n")

cat("\n13.2 INTERPRETACIÓN DE P-VALUES:\n")
cat("  p < 0.001: *** (muy significativo)\n")
cat("  p < 0.01:  **  (significativo)\n")
cat("  p < 0.05:  *   (significativo)\n")
cat("  p ≥ 0.05:      (no significativo)\n")

cat("\n13.3 ERRORES COMUNES A EVITAR:\n")
cat("  ✗ No verificar supuestos antes de aplicar pruebas paramétricas\n")
cat("  ✗ Ignorar el tamaño del efecto (solo mirar p-value)\n")
cat("  ✗ No corregir por comparaciones múltiples\n")
cat("  ✗ Confundir correlación con causalidad\n")
cat("  ✗ Sobre-interpretar R² sin considerar otros indicadores\n")
cat("  ✗ No reportar intervalos de confianza\n")
cat("  ✗ Usar test paramétrico con datos no normales\n")
cat("  ✗ No considerar la potencia estadística en diseño de estudios\n")

cat("\n13.4 CHECKLIST PARA REPORTAR RESULTADOS:\n")
cat("  ☐ Tamaño de muestra (n)\n")
cat("  ☐ Estadísticos descriptivos (media ± SD)\n")
cat("  ☐ Prueba estadística utilizada\n")
cat("  ☐ Estadístico de prueba y grados de libertad\n")
cat("  ☐ P-value\n")
cat("  ☐ Intervalo de confianza\n")
cat("  ☐ Tamaño del efecto\n")
cat("  ☐ Supuestos verificados\n")

cat("\n13.5 RECURSOS Y AYUDA:\n")
cat("  ?funcion          - Ayuda de una función\n")
cat("  ??tema            - Búsqueda en documentación\n")
cat("  example(funcion)  - Ver ejemplos\n")
cat("  help.search('keyword') - Buscar por palabra clave\n")

# ===============================================================================
# 🎓 14. CONCEPTOS ESTADÍSTICOS CLAVE
# ===============================================================================

cat("\n▶ 14. CONCEPTOS ESTADÍSTICOS CLAVE\n")
cat("─────────────────────────────────────────\n")

cat("\n14.1 HIPÓTESIS:\n")
cat("  H₀ (Nula): No hay efecto/diferencia (status quo)\n")
cat("  H₁ (Alternativa): Sí hay efecto/diferencia\n")

cat("\n14.2 TIPOS DE ERRORES:\n")
cat("  Error Tipo I (α): Rechazar H₀ verdadera (falso positivo)\n")
cat("    → Nivel de significancia típico: α = 0.05 (5%)\n")
cat("  Error Tipo II (β): No rechazar H₀ falsa (falso negativo)\n")
cat("    → Potencia = 1 - β (típicamente ≥ 0.80)\n")

cat("\n14.3 INTERVALOS DE CONFIANZA:\n")
cat("  IC 95%: Rango donde el parámetro verdadero estará el 95% de las veces\n")
cat("  Si IC no contiene H₀, rechazamos H₀ (equivalente a p < 0.05)\n")

cat("\n14.4 TAMAÑO DEL EFECTO:\n")
cat("  Cohen's d: Diferencia estandarizada entre medias\n")
cat("  r o R²: Proporción de varianza explicada\n")
cat("  Odds Ratio: Razón de probabilidades (regresión logística)\n")

cat("\n14.5 POTENCIA ESTADÍSTICA:\n")
cat("  Probabilidad de detectar un efecto real\n")
cat("  Depende de: n (tamaño muestral), α, tamaño del efecto\n")
cat("  Recomendado: ≥ 0.80 (80%)\n")

cat("\n14.6 FDR (False Discovery Rate):\n")
cat("  Proporción esperada de falsos positivos entre rechazos\n")
cat("  Control: Método Benjamini-Hochberg (BH)\n")
cat("  Menos conservador que Bonferroni\n")

# ===============================================================================
# ✅ FIN DE LA CHULETA
# ===============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  ✅ CHULETA COMPLETA CARGADA EXITOSAMENTE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Todos los conceptos, funciones y ejemplos están ahora disponibles.\n")
cat("Ejecute las secciones que necesite o utilice las funciones reutilizables.\n\n")
cat("Para visualizar gráficos: print(p1), print(p2), etc.\n")
cat("Para más ayuda: ?nombre_funcion\n\n")
