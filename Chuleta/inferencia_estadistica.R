# ==========================================
# INFERENCIA ESTADÍSTICA EN R
# Introducción Extendida con Ejemplos Prácticos
# ==========================================

print("=== INFERENCIA ESTADÍSTICA EN R ===")

# ==========================================
# 1. CONCEPTOS FUNDAMENTALES
# ==========================================

# La inferencia estadística nos permite hacer conclusiones sobre 
# una población basándose en datos de una muestra

print("1. CONCEPTOS FUNDAMENTALES")
print("Población vs Muestra:")
print("- Población: Conjunto completo de elementos de interés")
print("- Muestra: Subconjunto representativo de la población")
print("- Parámetro: Medida descriptiva de la población (μ, σ)")
print("- Estadístico: Medida descriptiva de la muestra (x̄, s)")
print("")

# ==========================================
# 2. DISTRIBUCIONES DE PROBABILIDAD
# ==========================================

print("2. DISTRIBUCIONES DE PROBABILIDAD")

# Distribución Normal
print("2.1 DISTRIBUCIÓN NORMAL")
set.seed(123)  # Para reproducibilidad

# Generar datos normales
datos_normales <- rnorm(1000, mean = 50, sd = 10)
print(paste("Media de la muestra:", round(mean(datos_normales), 2)))
print(paste("Desviación estándar:", round(sd(datos_normales), 2)))

# Probabilidades en distribución normal
prob_menor_45 <- pnorm(45, mean = 50, sd = 10)
print(paste("P(X < 45):", round(prob_menor_45, 4)))

prob_entre_40_60 <- pnorm(60, 50, 10) - pnorm(40, 50, 10)
print(paste("P(40 < X < 60):", round(prob_entre_40_60, 4)))

# Cuantiles
cuantil_95 <- qnorm(0.95, mean = 50, sd = 10)
print(paste("Cuantil 95%:", round(cuantil_95, 2)))

print("")

# Distribución t de Student
print("2.2 DISTRIBUCIÓN t DE STUDENT")
print("Usada cuando la desviación estándar poblacional es desconocida")

grados_libertad <- 15
valor_t_95 <- qt(0.975, df = grados_libertad)  # Para 95% bilateral
print(paste("Valor t crítico (95%, gl=15):", round(valor_t_95, 3)))

print("")

# ==========================================
# 3. ESTIMACIÓN PUNTUAL
# ==========================================

print("3. ESTIMACIÓN PUNTUAL")

# Generar datos de ejemplo
set.seed(456)
muestra_alturas <- rnorm(30, mean = 170, sd = 8)

# Estimadores puntuales
media_muestral <- mean(muestra_alturas)
desv_muestral <- sd(muestra_alturas)
varianza_muestral <- var(muestra_alturas)

print(paste("Media muestral (estimador de μ):", round(media_muestral, 2), "cm"))
print(paste("Desviación estándar muestral:", round(desv_muestral, 2), "cm"))
print(paste("Varianza muestral:", round(varianza_muestral, 2)))

# Error estándar de la media
n <- length(muestra_alturas)
error_estandar <- desv_muestral / sqrt(n)
print(paste("Error estándar de la media:", round(error_estandar, 3)))

print("")

# ==========================================
# 4. INTERVALOS DE CONFIANZA
# ==========================================

print("4. INTERVALOS DE CONFIANZA")

print("4.1 INTERVALO DE CONFIANZA PARA LA MEDIA (σ conocida)")

# Supongamos σ conocida = 8
sigma_conocida <- 8
z_alpha_2 <- qnorm(0.975)  # Para 95% de confianza

margen_error_z <- z_alpha_2 * (sigma_conocida / sqrt(n))
ic_inferior_z <- media_muestral - margen_error_z
ic_superior_z <- media_muestral + margen_error_z

print(paste("IC 95% para μ (σ conocida): [", 
           round(ic_inferior_z, 2), ",", 
           round(ic_superior_z, 2), "]"))

print("")

print("4.2 INTERVALO DE CONFIANZA PARA LA MEDIA (σ desconocida)")

# Usando distribución t
t_alpha_2 <- qt(0.975, df = n-1)
margen_error_t <- t_alpha_2 * (desv_muestral / sqrt(n))
ic_inferior_t <- media_muestral - margen_error_t
ic_superior_t <- media_muestral + margen_error_t

print(paste("IC 95% para μ (σ desconocida): [", 
           round(ic_inferior_t, 2), ",", 
           round(ic_superior_t, 2), "]"))

# Usando función built-in de R
ic_resultado <- t.test(muestra_alturas, conf.level = 0.95)$conf.int
print(paste("Verificación con t.test(): [", 
           round(ic_resultado[1], 2), ",", 
           round(ic_resultado[2], 2), "]"))

print("")

print("4.3 INTERVALO DE CONFIANZA PARA LA PROPORCIÓN")

# Ejemplo: Proporción de éxito en una muestra
exitos <- 75
n_prop <- 100
p_hat <- exitos / n_prop

# IC para proporción
z_prop <- qnorm(0.975)
error_prop <- sqrt((p_hat * (1 - p_hat)) / n_prop)
margen_error_prop <- z_prop * error_prop

ic_inf_prop <- p_hat - margen_error_prop
ic_sup_prop <- p_hat + margen_error_prop

print(paste("Proporción muestral:", p_hat))
print(paste("IC 95% para proporción: [", 
           round(ic_inf_prop, 3), ",", 
           round(ic_sup_prop, 3), "]"))

print("")

# ==========================================
# 5. PRUEBAS DE HIPÓTESIS
# ==========================================

print("5. PRUEBAS DE HIPÓTESIS")

print("5.1 PRUEBA t PARA UNA MEDIA")

# H0: μ = 175 vs H1: μ ≠ 175
mu_0 <- 175

# Estadístico t
t_calculado <- (media_muestral - mu_0) / (desv_muestral / sqrt(n))
valor_p <- 2 * pt(abs(t_calculado), df = n-1, lower.tail = FALSE)

print(paste("H0: μ = 175 vs H1: μ ≠ 175"))
print(paste("Estadístico t:", round(t_calculado, 3)))
print(paste("Valor p:", round(valor_p, 4)))

if (valor_p < 0.05) {
  print("Conclusión: Rechazamos H0 (α = 0.05)")
} else {
  print("Conclusión: No rechazamos H0 (α = 0.05)")
}

# Verificación con t.test
resultado_t <- t.test(muestra_alturas, mu = 175)
print("Verificación con t.test():")
print(paste("t =", round(resultado_t$statistic, 3)))
print(paste("p-value =", round(resultado_t$p.value, 4)))

print("")

print("5.2 PRUEBA DE PROPORCIÓN")

# H0: p = 0.7 vs H1: p ≠ 0.7
p_0 <- 0.7

# Estadístico z para proporción
z_prop_calc <- (p_hat - p_0) / sqrt(p_0 * (1 - p_0) / n_prop)
valor_p_prop <- 2 * pnorm(abs(z_prop_calc), lower.tail = FALSE)

print(paste("H0: p = 0.7 vs H1: p ≠ 0.7"))
print(paste("Estadístico z:", round(z_prop_calc, 3)))
print(paste("Valor p:", round(valor_p_prop, 4)))

# Verificación con prop.test
resultado_prop <- prop.test(exitos, n_prop, p = 0.7)
print("Verificación con prop.test():")
print(paste("Chi-cuadrado =", round(resultado_prop$statistic, 3)))
print(paste("p-value =", round(resultado_prop$p.value, 4)))

print("")

print("5.3 PRUEBA t PARA DOS MUESTRAS INDEPENDIENTES")

# Generar segunda muestra
set.seed(789)
muestra_2 <- rnorm(25, mean = 165, sd = 7)

print("Comparando dos grupos independientes:")
print(paste("Grupo 1: n =", length(muestra_alturas), 
           ", media =", round(mean(muestra_alturas), 2)))
print(paste("Grupo 2: n =", length(muestra_2), 
           ", media =", round(mean(muestra_2), 2)))

# Prueba t de dos muestras
resultado_2muestras <- t.test(muestra_alturas, muestra_2)
print("Resultados de la prueba t:")
print(paste("t =", round(resultado_2muestras$statistic, 3)))
print(paste("p-value =", round(resultado_2muestras$p.value, 4)))
print(paste("IC 95% para diferencia de medias: [", 
           round(resultado_2muestras$conf.int[1], 2), ",",
           round(resultado_2muestras$conf.int[2], 2), "]"))

print("")

print("5.4 PRUEBA t PAREADA")

# Datos pareados (antes y después)
set.seed(111)
antes <- rnorm(20, mean = 80, sd = 10)
despues <- antes + rnorm(20, mean = 5, sd = 3)  # Mejora promedio de 5

print("Prueba t pareada (antes vs después):")
print(paste("Media antes:", round(mean(antes), 2)))
print(paste("Media después:", round(mean(despues), 2)))

resultado_pareado <- t.test(despues, antes, paired = TRUE)
print("Resultados:")
print(paste("t =", round(resultado_pareado$statistic, 3)))
print(paste("p-value =", round(resultado_pareado$p.value, 4)))

print("")

# ==========================================
# 6. ANÁLISIS DE VARIANZA (ANOVA)
# ==========================================

print("6. ANÁLISIS DE VARIANZA (ANOVA)")

# Generar datos para tres grupos
set.seed(222)
grupo1 <- rnorm(15, mean = 20, sd = 3)
grupo2 <- rnorm(15, mean = 25, sd = 3)
grupo3 <- rnorm(15, mean = 22, sd = 3)

# Crear data frame
datos_anova <- data.frame(
  valor = c(grupo1, grupo2, grupo3),
  grupo = factor(rep(c("A", "B", "C"), each = 15))
)

print("Medias por grupo:")
medias_grupo <- aggregate(valor ~ grupo, datos_anova, mean)
print(medias_grupo)

# ANOVA de una vía
resultado_anova <- aov(valor ~ grupo, data = datos_anova)
summary_anova <- summary(resultado_anova)
print("Resultados ANOVA:")
print(summary_anova)

# Extracción del F y p-value
f_stat <- summary_anova[[1]][["F value"]][1]
p_value_anova <- summary_anova[[1]][["Pr(>F)"]][1]
print(paste("F =", round(f_stat, 3)))
print(paste("p-value =", round(p_value_anova, 4)))

print("")

# ==========================================
# 7. CORRELACIÓN Y REGRESIÓN
# ==========================================

print("7. CORRELACIÓN Y REGRESIÓN")

# Generar datos correlacionados
set.seed(333)
x <- 1:50
y <- 2 * x + 5 + rnorm(50, mean = 0, sd = 10)

print("7.1 CORRELACIÓN")
correlacion <- cor(x, y)
print(paste("Coeficiente de correlación de Pearson:", round(correlacion, 3)))

# Prueba de significancia de la correlación
test_cor <- cor.test(x, y)
print(paste("p-value para correlación:", round(test_cor$p.value, 4)))
print(paste("IC 95% para correlación: [", 
           round(test_cor$conf.int[1], 3), ",",
           round(test_cor$conf.int[2], 3), "]"))

print("")

print("7.2 REGRESIÓN LINEAL SIMPLE")
modelo <- lm(y ~ x)
summary_modelo <- summary(modelo)

print("Coeficientes:")
print(paste("Intercepto:", round(modelo$coefficients[1], 3)))
print(paste("Pendiente:", round(modelo$coefficients[2], 3)))

print("Estadísticas del modelo:")
print(paste("R-cuadrado:", round(summary_modelo$r.squared, 3)))
print(paste("R-cuadrado ajustado:", round(summary_modelo$adj.r.squared, 3)))

# Prueba de significancia de la pendiente
t_pendiente <- summary_modelo$coefficients[2, 3]
p_pendiente <- summary_modelo$coefficients[2, 4]
print(paste("t para pendiente:", round(t_pendiente, 3)))
print(paste("p-value para pendiente:", round(p_pendiente, 4)))

print("")

# ==========================================
# 8. PRUEBAS NO PARAMÉTRICAS
# ==========================================

print("8. PRUEBAS NO PARAMÉTRICAS")

print("8.1 PRUEBA DE WILCOXON (Mann-Whitney U)")
# Para comparar dos grupos cuando no se asumen distribuciones normales

grupo_a <- c(23, 25, 28, 30, 32, 35, 38)
grupo_b <- c(18, 20, 22, 24, 26, 29, 31)

resultado_wilcoxon <- wilcox.test(grupo_a, grupo_b)
print("Prueba de Wilcoxon para dos muestras independientes:")
print(paste("W =", resultado_wilcoxon$statistic))
print(paste("p-value =", round(resultado_wilcoxon$p.value, 4)))

print("")

print("8.2 PRUEBA DE KRUSKAL-WALLIS")
# ANOVA no paramétrica para más de dos grupos

datos_kruskal <- data.frame(
  valor = c(grupo1, grupo2, grupo3),  # Usando los datos del ANOVA anterior
  grupo = factor(rep(c("A", "B", "C"), each = 15))
)

resultado_kruskal <- kruskal.test(valor ~ grupo, data = datos_kruskal)
print("Prueba de Kruskal-Wallis:")
print(paste("Chi-cuadrado =", round(resultado_kruskal$statistic, 3)))
print(paste("p-value =", round(resultado_kruskal$p.value, 4)))

print("")

# ==========================================
# 9. ANÁLISIS DE CONTINGENCIA
# ==========================================

print("9. ANÁLISIS DE CONTINGENCIA")

# Crear tabla de contingencia
tabla_contingencia <- matrix(c(20, 30, 25, 45), nrow = 2, 
                           dimnames = list(c("Tratamiento", "Control"), 
                                         c("Éxito", "Fracaso")))
print("Tabla de contingencia:")
print(tabla_contingencia)

# Prueba Chi-cuadrado de independencia
resultado_chi2 <- chisq.test(tabla_contingencia)
print("Prueba Chi-cuadrado de independencia:")
print(paste("Chi-cuadrado =", round(resultado_chi2$statistic, 3)))
print(paste("p-value =", round(resultado_chi2$p.value, 4)))

# Prueba exacta de Fisher (para muestras pequeñas)
resultado_fisher <- fisher.test(tabla_contingencia)
print("Prueba exacta de Fisher:")
print(paste("p-value =", round(resultado_fisher$p.value, 4)))

print("")

# ==========================================
# 10. SIMULACIÓN Y BOOTSTRAP
# ==========================================

print("10. SIMULACIÓN Y BOOTSTRAP")

print("10.1 BOOTSTRAP PARA INTERVALO DE CONFIANZA DE LA MEDIA")

# Función bootstrap manual
bootstrap_medias <- function(datos, n_bootstrap = 1000) {
  n <- length(datos)
  medias_boot <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    muestra_boot <- sample(datos, n, replace = TRUE)
    medias_boot[i] <- mean(muestra_boot)
  }
  
  return(medias_boot)
}

# Aplicar bootstrap
set.seed(444)
datos_originales <- rnorm(30, mean = 50, sd = 8)
medias_bootstrap <- bootstrap_medias(datos_originales, 1000)

# IC bootstrap (método percentil)
ic_bootstrap <- quantile(medias_bootstrap, c(0.025, 0.975))
print(paste("Media original:", round(mean(datos_originales), 3)))
print(paste("IC 95% Bootstrap: [", 
           round(ic_bootstrap[1], 3), ",",
           round(ic_bootstrap[2], 3), "]"))

# Comparar con IC teórico
ic_teorico <- t.test(datos_originales)$conf.int
print(paste("IC 95% teórico: [", 
           round(ic_teorico[1], 3), ",",
           round(ic_teorico[2], 3), "]"))

print("")

# ==========================================
# 11. DIAGNÓSTICOS Y VALIDACIÓN
# ==========================================

print("11. DIAGNÓSTICOS Y VALIDACIÓN")

print("11.1 PRUEBAS DE NORMALIDAD")

# Generar datos para probar
set.seed(555)
datos_normales <- rnorm(50, mean = 100, sd = 15)
datos_no_normales <- rexp(50, rate = 0.1)

# Prueba de Shapiro-Wilk
shapiro_normal <- shapiro.test(datos_normales)
shapiro_no_normal <- shapiro.test(datos_no_normales)

print("Prueba de Shapiro-Wilk para normalidad:")
print(paste("Datos normales - p-value:", round(shapiro_normal$p.value, 4)))
print(paste("Datos no normales - p-value:", round(shapiro_no_normal$p.value, 4)))

print("")

print("11.2 PRUEBA DE HOMOGENEIDAD DE VARIANZAS")

# Prueba de Levene para igualdad de varianzas
library(car)  # Nota: esta librería debe estar instalada

# Usar los datos del ANOVA anterior
levene_result <- tryCatch({
  leveneTest(valor ~ grupo, data = datos_anova)
}, error = function(e) {
  return("Error: Librería 'car' no disponible")
})

if (is.character(levene_result)) {
  print(levene_result)
  print("Alternativa: Prueba F para dos varianzas")
  var_test <- var.test(grupo1, grupo2)
  print(paste("F =", round(var_test$statistic, 3)))
  print(paste("p-value =", round(var_test$p.value, 4)))
} else {
  print("Prueba de Levene para homogeneidad de varianzas:")
  print(levene_result)
}

print("")

# ==========================================
# 12. TAMAÑO DE MUESTRA Y PODER ESTADÍSTICO
# ==========================================

print("12. TAMAÑO DE MUESTRA Y PODER ESTADÍSTICO")

print("12.1 CÁLCULO DE TAMAÑO DE MUESTRA PARA PRUEBA t")

# Para detectar una diferencia de 5 unidades con poder 80%
power_result <- power.t.test(delta = 5, sd = 8, sig.level = 0.05, power = 0.80)
print(paste("Tamaño de muestra necesario:", ceiling(power_result$n)))

print("")

print("12.2 CÁLCULO DE PODER ESTADÍSTICO")

# Con n = 30, ¿cuál es el poder para detectar diferencia de 5?
power_calc <- power.t.test(n = 30, delta = 5, sd = 8, sig.level = 0.05)
print(paste("Poder estadístico con n=30:", round(power_calc$power, 3)))

print("")

# ==========================================
# RESUMEN FINAL
# ==========================================

print("=== RESUMEN DE CONCEPTOS CLAVE ===")
print("")
print("1. TIPOS DE INFERENCIA:")
print("   - Estimación puntual: Un solo valor como estimador")
print("   - Estimación por intervalos: Rango de valores plausibles")
print("   - Pruebas de hipótesis: Decisiones sobre afirmaciones")
print("")
print("2. ELEMENTOS DE UNA PRUEBA DE HIPÓTESIS:")
print("   - H0 (hipótesis nula) vs H1 (hipótesis alternativa)")
print("   - Nivel de significancia (α, típicamente 0.05)")
print("   - Estadístico de prueba y su distribución")
print("   - Valor p: probabilidad de observar datos tan extremos")
print("   - Decisión: rechazar o no rechazar H0")
print("")
print("3. TIPOS DE ERRORES:")
print("   - Error Tipo I: Rechazar H0 cuando es verdadera (α)")
print("   - Error Tipo II: No rechazar H0 cuando es falsa (β)")
print("   - Poder = 1 - β: probabilidad de rechazar H0 falsa")
print("")
print("4. SUPUESTOS IMPORTANTES:")
print("   - Normalidad de los datos")
print("   - Independencia de las observaciones")
print("   - Homogeneidad de varianzas (cuando aplique)")
print("")
print("5. CUÁNDO USAR CADA PRUEBA:")
print("   - Una muestra: t.test() para media, prop.test() para proporción")
print("   - Dos muestras independientes: t.test() con var.equal=TRUE/FALSE")
print("   - Datos pareados: t.test() con paired=TRUE")
print("   - Más de dos grupos: aov() para ANOVA")
print("   - Datos no normales: wilcox.test(), kruskal.test()")
print("   - Variables categóricas: chisq.test(), fisher.test()")
print("")
print("=== ARCHIVO COMPLETADO ===")
print("Este archivo contiene ejemplos prácticos de todos los conceptos")
print("fundamentales de inferencia estadística en R.")
print("¡Ejecuta sección por sección para entender cada concepto!")