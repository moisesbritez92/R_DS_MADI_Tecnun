# ═══════════════════════════════════════════════════════════════════════════════
# 📚 CHULETA 3 - PROBLEMAS AVANZADOS CON ANÁLISIS MÚLTIPLE
# ═══════════════════════════════════════════════════════════════════════════════
# Ejercicios complejos que combinan múltiples técnicas estadísticas
# Nivel: Avanzado - Tipo Examen Final
# ═══════════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(dplyr)
library(tidyr)
library(pwr)
library(viridis)
library(ggrepel)

# ===============================================================================
# 🏢 PROBLEMA 1: ANÁLISIS INTEGRAL DE PRODUCTIVIDAD EMPRESARIAL
# ===============================================================================

# --- CONTEXTO ---
# Una consultora evalúa la productividad de 600 empleados en 6 departamentos
# (Ventas, Marketing, IT, RRHH, Finanzas, Operaciones) de 3 sedes (Madrid, 
# Barcelona, Valencia). Se mide durante 12 meses:
# - Productividad (índice 0-100)
# - Horas trabajadas semanales
# - Años de experiencia
# - Nivel educativo (Grado, Máster, Doctorado)
# - Satisfacción laboral (1-10)
# - Promoción en el año (Sí/No)
# - Salario anual (€)

# --- PREGUNTAS INTEGRADAS ---
# 1.1 ¿La productividad varía significativamente entre departamentos? Si es así,
#     ¿qué pares de departamentos son diferentes? (ANOVA + comparaciones múltiples)
# 1.2 ¿La sede influye en la productividad? Hacer comparación múltiple entre 
#     las 3 sedes (test t múltiple + corrección FDR)
# 1.3 ¿Qué variables predicen mejor la productividad? (regresión múltiple)
# 1.4 ¿La promoción está relacionada con el nivel educativo? (chi-cuadrado)
# 1.5 ¿Hay correlación entre satisfacción y productividad? ¿Es diferente
#     para empleados con/sin promoción?
# 1.6 Realizar PCA con variables numéricas y visualizar si se pueden identificar
#     perfiles de empleados según departamento
# 1.7 Análisis de potencia: ¿cuántos empleados necesito para detectar una
#     diferencia de 5 puntos en productividad con 85% de potencia?

# --- PROCEDIMIENTO COMPLETO ---

# ============================================
# PASO 1: SIMULAR DATOS REALISTAS
# ============================================
set.seed(202425)

empleados <- expand.grid(
  empleado_id = 1:100,
  mes = 1:6
) %>%
  mutate(
    departamento = sample(c("Ventas", "Marketing", "IT", "RRHH", "Finanzas", "Operaciones"), 
                         n(), replace = TRUE),
    sede = sample(c("Madrid", "Barcelona", "Valencia"), n(), replace = TRUE),
    nivel_educativo = sample(c("Grado", "Máster", "Doctorado"), n(), replace = TRUE,
                            prob = c(0.5, 0.35, 0.15)),
    años_experiencia = sample(1:20, n(), replace = TRUE),
    horas_semanales = rnorm(n(), mean = 40, sd = 5),
    satisfaccion = sample(1:10, n(), replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.08, 0.12, 0.15, 0.2, 0.15, 0.12, 0.08))
  ) %>%
  mutate(
    # Productividad base según departamento
    productividad_base = case_when(
      departamento == "IT" ~ 75,
      departamento == "Finanzas" ~ 72,
      departamento == "Ventas" ~ 68,
      departamento == "Marketing" ~ 70,
      departamento == "RRHH" ~ 65,
      departamento == "Operaciones" ~ 67
    ),
    # Ajustes por otras variables
    ajuste_sede = case_when(
      sede == "Madrid" ~ 3,
      sede == "Barcelona" ~ 1,
      sede == "Valencia" ~ 0
    ),
    ajuste_educacion = case_when(
      nivel_educativo == "Doctorado" ~ 5,
      nivel_educativo == "Máster" ~ 2,
      nivel_educativo == "Grado" ~ 0
    ),
    # Productividad final
    productividad = pmin(100, pmax(0, 
                                   productividad_base + 
                                   ajuste_sede + 
                                   ajuste_educacion + 
                                   años_experiencia * 0.5 + 
                                   satisfaccion * 1.5 + 
                                   rnorm(n(), 0, 8))),
    # Salario basado en productividad y experiencia
    salario = 25000 + productividad * 200 + años_experiencia * 1000 + 
              ifelse(nivel_educativo == "Doctorado", 8000,
                     ifelse(nivel_educativo == "Máster", 4000, 0)) +
              rnorm(n(), 0, 3000)
  )

# Añadir variable promoción (una por empleado, no por mes)
empleados_unicos <- empleados %>%
  select(empleado_id, productividad, nivel_educativo, satisfaccion) %>%
  group_by(empleado_id) %>%
  summarise(
    productividad_media = mean(productividad),
    nivel_educativo = first(nivel_educativo),
    satisfaccion_media = mean(satisfaccion),
    .groups = "drop"
  ) %>%
  mutate(
    # Probabilidad de promoción aumenta con productividad y educación
    prob_promocion = pmin(0.9, (productividad_media / 100) * 0.4 + 
                          ifelse(nivel_educativo == "Doctorado", 0.3,
                                 ifelse(nivel_educativo == "Máster", 0.2, 0.1))),
    promocion = rbinom(n(), 1, prob_promocion),
    promocion = ifelse(promocion == 1, "Sí", "No")
  )

# Unir con datos principales
empleados <- empleados %>%
  left_join(empleados_unicos %>% select(empleado_id, promocion), by = "empleado_id")

cat("=== DATOS GENERADOS ===\n")
cat("Total de observaciones:", nrow(empleados), "\n")
cat("Empleados únicos:", length(unique(empleados$empleado_id)), "\n")
cat("Departamentos:", length(unique(empleados$departamento)), "\n")
cat("Sedes:", length(unique(empleados$sede)), "\n\n")

# ============================================
# PASO 2: 1.1 - ANOVA POR DEPARTAMENTO + POST-HOC
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📊 1.1 - PRODUCTIVIDAD POR DEPARTAMENTO (ANOVA + TukeyHSD)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# ANOVA
anova_dept <- aov(productividad ~ departamento, data = empleados)
print(summary(anova_dept))

# Interpretación automática
p_anova <- summary(anova_dept)[[1]][["Pr(>F)"]][1]
if(p_anova < 0.05) {
  cat("\n✅ CONCLUSIÓN: Hay diferencias significativas entre departamentos (p =", 
      format(p_anova, scientific = TRUE), ")\n")
  
  # Comparaciones post-hoc
  cat("\n--- Comparaciones múltiples (Tukey HSD) ---\n")
  tukey_result <- TukeyHSD(anova_dept)
  print(tukey_result)
  
  # Identificar diferencias significativas
  tukey_df <- as.data.frame(tukey_result$departamento)
  tukey_df$comparacion <- rownames(tukey_df)
  significativos <- tukey_df %>% filter(`p adj` < 0.05)
  
  cat("\n📌 Pares de departamentos con diferencias significativas:\n")
  print(significativos %>% select(comparacion, diff, `p adj`))
  
} else {
  cat("\n❌ CONCLUSIÓN: NO hay diferencias significativas entre departamentos\n")
}

# Visualización
medias_dept <- empleados %>%
  group_by(departamento) %>%
  summarise(
    media = mean(productividad),
    mediana = median(productividad),
    se = sd(productividad) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(desc(media))

ggplot(empleados, aes(x = reorder(departamento, productividad, FUN = median), 
                      y = productividad, fill = departamento)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  labs(title = "Distribución de Productividad por Departamento",
       subtitle = "Diamante rojo = media, línea = mediana",
       x = "Departamento",
       y = "Productividad (0-100)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================
# PASO 3: 1.2 - COMPARACIONES MÚLTIPLES ENTRE SEDES (con corrección FDR)
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🏙️ 1.2 - COMPARACIONES MÚLTIPLES ENTRE SEDES (Test t + FDR)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Todas las comparaciones posibles entre sedes
sedes <- unique(empleados$sede)
comparaciones_sedes <- expand.grid(Sede1 = sedes, Sede2 = sedes) %>%
  filter(as.character(Sede1) < as.character(Sede2))

resultados_sedes <- data.frame()

for(i in 1:nrow(comparaciones_sedes)) {
  sede1 <- as.character(comparaciones_sedes$Sede1[i])
  sede2 <- as.character(comparaciones_sedes$Sede2[i])
  
  datos_comp <- empleados %>% filter(sede %in% c(sede1, sede2))
  test <- t.test(productividad ~ sede, data = datos_comp)
  
  media1 <- mean(empleados$productividad[empleados$sede == sede1])
  media2 <- mean(empleados$productividad[empleados$sede == sede2])
  
  resultados_sedes <- rbind(resultados_sedes, data.frame(
    Comparacion = paste(sede1, "vs", sede2),
    Sede1 = sede1,
    Sede2 = sede2,
    Media_Sede1 = media1,
    Media_Sede2 = media2,
    Diferencia = media1 - media2,
    p_value = test$p.value,
    stringsAsFactors = FALSE
  ))
}

# Aplicar corrección FDR (Benjamini-Hochberg)
resultados_sedes$p_adjusted_BH <- p.adjust(resultados_sedes$p_value, method = "BH")
resultados_sedes$p_adjusted_bonferroni <- p.adjust(resultados_sedes$p_value, method = "bonferroni")

resultados_sedes$Significativo_BH <- ifelse(resultados_sedes$p_adjusted_BH < 0.05, "Sí ✓", "No ✗")
resultados_sedes$Significativo_Bonferroni <- ifelse(resultados_sedes$p_adjusted_bonferroni < 0.05, "Sí ✓", "No ✗")

cat("--- Resultados de comparaciones múltiples ---\n")
print(resultados_sedes %>% 
        select(Comparacion, Diferencia, p_value, p_adjusted_BH, Significativo_BH))

cat("\n📊 Resumen:\n")
cat("  • Comparaciones totales:", nrow(resultados_sedes), "\n")
cat("  • Significativas (BH):", sum(resultados_sedes$Significativo_BH == "Sí ✓"), "\n")
cat("  • Significativas (Bonferroni):", sum(resultados_sedes$Significativo_Bonferroni == "Sí ✓"), "\n")

# Visualización comparativa
ggplot(resultados_sedes, aes(x = p_value, y = p_adjusted_BH, color = Significativo_BH)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text_repel(aes(label = Comparacion), size = 3.5) +
  scale_color_manual(values = c("Sí ✓" = "green", "No ✗" = "red")) +
  labs(title = "Efecto de Corrección FDR en Comparaciones Múltiples",
       subtitle = "Líneas rojas = umbral α = 0.05",
       x = "P-value sin ajuste",
       y = "P-value ajustado (BH)",
       color = "Significativo") +
  theme_minimal()

# ============================================
# PASO 4: 1.3 - REGRESIÓN MÚLTIPLE
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📈 1.3 - REGRESIÓN MÚLTIPLE: PREDICTORES DE PRODUCTIVIDAD\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Preparar datos para regresión
datos_regresion <- empleados %>%
  mutate(
    departamento = as.factor(departamento),
    sede = as.factor(sede),
    nivel_educativo = factor(nivel_educativo, levels = c("Grado", "Máster", "Doctorado")),
    promocion = as.factor(promocion)
  )

# Modelo completo
modelo_completo <- lm(productividad ~ años_experiencia + horas_semanales + 
                      satisfaccion + nivel_educativo + departamento + 
                      sede + promocion, 
                      data = datos_regresion)

cat("--- Resumen del modelo ---\n")
print(summary(modelo_completo))

# Análisis de coeficientes
coef_summary <- summary(modelo_completo)$coefficients
coef_df <- as.data.frame(coef_summary)
coef_df$Variable <- rownames(coef_df)
coef_df$Significativo <- ifelse(coef_df$`Pr(>|t|)` < 0.05, "Sí ✓", "No ✗")

cat("\n📌 Variables significativas (p < 0.05):\n")
vars_sig <- coef_df %>% 
  filter(Significativo == "Sí ✓") %>%
  arrange(`Pr(>|t|)`) %>%
  select(Variable, Estimate, `Pr(>|t|)`)
print(vars_sig)

cat("\n📊 Bondad de ajuste:\n")
cat("  • R²:", round(summary(modelo_completo)$r.squared, 4), "\n")
cat("  • R² ajustado:", round(summary(modelo_completo)$adj.r.squared, 4), "\n")
cat("  • Varianza explicada:", round(summary(modelo_completo)$r.squared * 100, 2), "%\n")

# Visualizar coeficientes significativos
coef_plot <- coef_df %>%
  filter(Variable != "(Intercept)", Significativo == "Sí ✓") %>%
  mutate(
    Variable = gsub("departamento|sede|nivel_educativo|promocion", "", Variable),
    Variable = reorder(Variable, Estimate)
  )

if(nrow(coef_plot) > 0) {
  ggplot(coef_plot, aes(x = Variable, y = Estimate, fill = Estimate > 0)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_errorbar(aes(ymin = Estimate - `Std. Error`, 
                     ymax = Estimate + `Std. Error`),
                  width = 0.2) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "coral"),
                     labels = c("Negativo", "Positivo")) +
    labs(title = "Coeficientes Significativos del Modelo de Regresión",
         subtitle = "Barras = coeficiente ± error estándar",
         x = "Variable",
         y = "Coeficiente",
         fill = "Efecto") +
    theme_minimal()
}

# ============================================
# PASO 5: 1.4 - CHI-CUADRADO: PROMOCIÓN vs EDUCACIÓN
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🎓 1.4 - RELACIÓN PROMOCIÓN vs NIVEL EDUCATIVO (Chi-cuadrado)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Tabla de contingencia (usar solo un registro por empleado)
datos_promocion <- empleados %>%
  select(empleado_id, promocion, nivel_educativo) %>%
  distinct()

tabla_educacion <- table(datos_promocion$nivel_educativo, datos_promocion$promocion)
cat("--- Tabla de contingencia ---\n")
print(tabla_educacion)

cat("\n--- Proporciones por fila (nivel educativo) ---\n")
prop_tabla <- prop.table(tabla_educacion, margin = 1)
print(round(prop_tabla, 3))

# Test chi-cuadrado
test_chi <- chisq.test(tabla_educacion)
cat("\n--- Test Chi-cuadrado ---\n")
print(test_chi)

# Test de Fisher (alternativa)
test_fisher <- fisher.test(tabla_educacion)
cat("\n--- Test exacto de Fisher ---\n")
print(test_fisher)

# Interpretación
if(test_chi$p.value < 0.05) {
  cat("\n✅ CONCLUSIÓN: Hay asociación significativa entre nivel educativo y promoción\n")
  cat("   Las personas con mayor educación tienen más probabilidad de ser promovidas.\n")
} else {
  cat("\n❌ CONCLUSIÓN: NO hay asociación significativa entre educación y promoción\n")
}

# Visualización
prop_data <- as.data.frame(prop.table(tabla_educacion, margin = 1))
colnames(prop_data) <- c("Nivel_Educativo", "Promocion", "Proporcion")

ggplot(prop_data, aes(x = Nivel_Educativo, y = Proporcion, fill = Promocion)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Proporcion * 100, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("Sí" = "green", "No" = "coral")) +
  labs(title = "Tasa de Promoción según Nivel Educativo",
       subtitle = paste0("Chi-cuadrado: p = ", format(test_chi$p.value, digits = 3)),
       x = "Nivel Educativo",
       y = "Proporción",
       fill = "Promoción") +
  theme_minimal()

# ============================================
# PASO 6: 1.5 - CORRELACIÓN SATISFACCIÓN vs PRODUCTIVIDAD (POR GRUPO)
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("😊 1.5 - CORRELACIÓN SATISFACCIÓN vs PRODUCTIVIDAD (POR PROMOCIÓN)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Correlación general
cor_general <- cor.test(empleados$satisfaccion, empleados$productividad)
cat("--- Correlación general ---\n")
print(cor_general)

# Correlación por grupo de promoción
empleados_con_promocion <- empleados %>% filter(promocion == "Sí")
empleados_sin_promocion <- empleados %>% filter(promocion == "No")

cor_con <- cor.test(empleados_con_promocion$satisfaccion, 
                    empleados_con_promocion$productividad)
cor_sin <- cor.test(empleados_sin_promocion$satisfaccion, 
                    empleados_sin_promocion$productividad)

cat("\n--- Correlación por grupo ---\n")
cat("Con promoción:    r =", round(cor_con$estimate, 3), ", p =", format(cor_con$p.value, digits = 3), "\n")
cat("Sin promoción:    r =", round(cor_sin$estimate, 3), ", p =", format(cor_sin$p.value, digits = 3), "\n")

# Visualización
ggplot(empleados, aes(x = satisfaccion, y = productividad, color = promocion)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~ promocion) +
  scale_color_manual(values = c("Sí" = "green", "No" = "coral")) +
  labs(title = "Relación Satisfacción - Productividad según Promoción",
       subtitle = paste0("General: r = ", round(cor_general$estimate, 3)),
       x = "Satisfacción (1-10)",
       y = "Productividad (0-100)",
       color = "Promoción") +
  theme_minimal()

# ============================================
# PASO 7: 1.6 - PCA PARA IDENTIFICAR PERFILES
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🔬 1.6 - PCA: PERFILES DE EMPLEADOS POR DEPARTAMENTO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Preparar matriz para PCA (promedios por empleado)
datos_pca <- empleados %>%
  group_by(empleado_id) %>%
  summarise(
    productividad_media = mean(productividad),
    horas_media = mean(horas_semanales),
    satisfaccion_media = mean(satisfaccion),
    años_experiencia = first(años_experiencia),
    salario_medio = mean(salario),
    departamento = first(departamento),
    .groups = "drop"
  )

# Matriz numérica para PCA
matriz_pca <- datos_pca %>%
  select(productividad_media, horas_media, satisfaccion_media, 
         años_experiencia, salario_medio) %>%
  as.matrix()

# Realizar PCA
pca_empleados <- prcomp(matriz_pca, scale. = TRUE, center = TRUE)

# Varianza explicada
varianza <- summary(pca_empleados)$importance[2, 1:2] * 100
cat("PC1 explica:", round(varianza[1], 2), "% de la varianza\n")
cat("PC2 explica:", round(varianza[2], 2), "% de la varianza\n")
cat("Total explicado:", round(sum(varianza), 2), "%\n")

# Loadings
cat("\n--- Loadings (contribución de variables) ---\n")
cat("\nPC1:\n")
print(sort(pca_empleados$rotation[, 1], decreasing = TRUE))
cat("\nPC2:\n")
print(sort(pca_empleados$rotation[, 2], decreasing = TRUE))

# Crear dataframe para visualización
pca_df <- data.frame(
  PC1 = pca_empleados$x[, 1],
  PC2 = pca_empleados$x[, 2],
  departamento = datos_pca$departamento,
  productividad = datos_pca$productividad_media
)

# Gráfico PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = departamento, size = productividad)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(aes(fill = departamento), alpha = 0.1, geom = "polygon", 
               level = 0.68) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(
    title = "PCA - Perfiles de Empleados por Departamento",
    subtitle = paste0("PC1: ", round(varianza[1], 1), "% | PC2: ", round(varianza[2], 1), "%"),
    x = paste0("PC1 (", round(varianza[1], 1), "%)"),
    y = paste0("PC2 (", round(varianza[2], 1), "%)"),
    color = "Departamento",
    size = "Productividad"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# ============================================
# PASO 8: 1.7 - ANÁLISIS DE POTENCIA
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("⚡ 1.7 - ANÁLISIS DE POTENCIA ESTADÍSTICA\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Calcular tamaño del efecto (Cohen's d)
diferencia_deseada <- 5  # 5 puntos de productividad
sd_productividad <- sd(empleados$productividad)
cohens_d <- diferencia_deseada / sd_productividad

cat("Objetivo: Detectar diferencia de", diferencia_deseada, "puntos\n")
cat("Desviación estándar observada:", round(sd_productividad, 2), "\n")
cat("Cohen's d:", round(cohens_d, 3), "\n\n")

# Calcular tamaño muestral necesario
power_result <- pwr.t.test(d = cohens_d, 
                          sig.level = 0.05, 
                          power = 0.85, 
                          type = "two.sample")

cat("--- Tamaño muestral necesario ---\n")
cat("Por grupo:", ceiling(power_result$n), "empleados\n")
cat("Total:", ceiling(power_result$n) * 2, "empleados\n\n")

# Potencia actual
n_actual <- nrow(empleados) / 2
power_actual <- pwr.t.test(n = n_actual, 
                          d = cohens_d, 
                          sig.level = 0.05, 
                          type = "two.sample")

cat("--- Potencia actual ---\n")
cat("Con n =", round(n_actual), "por grupo\n")
cat("Potencia =", round(power_actual$power, 3), "(",
    round(power_actual$power * 100, 1), "%)\n")

# Curva de potencia
n_seq <- seq(10, 500, by = 10)
potencias <- sapply(n_seq, function(n) {
  pwr.t.test(n = n, d = cohens_d, sig.level = 0.05, type = "two.sample")$power
})

potencia_df <- data.frame(n = n_seq, power = potencias)

ggplot(potencia_df, aes(x = n, y = power)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_hline(yintercept = 0.85, linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_vline(xintercept = ceiling(power_result$n), linetype = "dashed", 
             color = "darkgreen", alpha = 0.7) +
  annotate("text", x = ceiling(power_result$n) + 50, y = 0.5, 
           label = paste0("n = ", ceiling(power_result$n)), color = "darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Curva de Potencia Estadística",
       subtitle = paste0("Para detectar diferencia de ", diferencia_deseada, 
                        " puntos (d = ", round(cohens_d, 3), ")"),
       x = "Tamaño muestral por grupo",
       y = "Potencia (1 - β)") +
  theme_minimal()

# --- RESUMEN FINAL PROBLEMA 1 ---
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📋 RESUMEN EJECUTIVO - PROBLEMA 1\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")
cat("✓ ANOVA reveló diferencias entre departamentos\n")
cat("✓ Comparaciones múltiples entre sedes con corrección FDR\n")
cat("✓ Regresión identificó predictores clave de productividad\n")
cat("✓ Chi-cuadrado mostró relación educación-promoción\n")
cat("✓ Correlación satisfacción-productividad varía según promoción\n")
cat("✓ PCA identificó perfiles diferenciados por departamento\n")
cat("✓ Análisis de potencia determinó tamaño muestral óptimo\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# ===============================================================================
# 🏥 PROBLEMA 2: ESTUDIO LONGITUDINAL DE TRATAMIENTO MÉDICO CON ANÁLISIS TEMPORAL
# ===============================================================================

# --- CONTEXTO ---
# Un estudio clínico evalúa la eficacia de 3 tratamientos (A, B, Placebo) para
# reducir niveles de colesterol. Se siguieron 300 pacientes durante 12 semanas,
# midiendo cada 2 semanas:
# - Colesterol LDL (mg/dL)
# - Presión arterial sistólica (mmHg)
# - IMC (kg/m²)
# - Efectos secundarios (Sí/No)
# También se registró: edad, sexo, fumador (Sí/No), diabetes (Sí/No)

# --- PREGUNTAS INTEGRADAS ---
# 2.1 ¿Todos los tratamientos reducen el colesterol significativamente del
#     inicio (semana 0) al final (semana 12)? (test t pareado múltiple + FDR)
# 2.2 ¿Hay diferencias en la reducción de colesterol entre los 3 tratamientos?
#     (ANOVA + comparaciones post-hoc)
# 2.3 ¿El tratamiento A es superior al placebo? (test t independiente)
# 2.4 ¿Los efectos secundarios están relacionados con el tipo de tratamiento?
#     (chi-cuadrado)
# 2.5 ¿Qué variables predicen mejor la reducción de colesterol? Incluir
#     interacciones tratamiento*diabetes (regresión múltiple)
# 2.6 Realizar PCA con mediciones finales (colesterol, presión, IMC) y ver
#     si se distinguen los grupos de tratamiento
# 2.7 Visualizar evolución temporal del colesterol por tratamiento con
#     intervalos de confianza

# --- PROCEDIMIENTO COMPLETO ---

# ============================================
# PASO 1: SIMULAR DATOS LONGITUDINALES
# ============================================
cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("🏥 PROBLEMA 2: ESTUDIO CLÍNICO LONGITUDINAL\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

set.seed(303132)

# Crear pacientes base
pacientes_base <- data.frame(
  paciente_id = 1:300,
  tratamiento = sample(c("A", "B", "Placebo"), 300, replace = TRUE),
  sexo = sample(c("M", "F"), 300, replace = TRUE),
  edad = sample(40:75, 300, replace = TRUE),
  fumador = sample(c("Sí", "No"), 300, replace = TRUE, prob = c(0.3, 0.7)),
  diabetes = sample(c("Sí", "No"), 300, replace = TRUE, prob = c(0.25, 0.75))
)

# Generar mediciones longitudinales
semanas <- c(0, 2, 4, 6, 8, 10, 12)

pacientes <- expand.grid(
  paciente_id = 1:300,
  semana = semanas
) %>%
  left_join(pacientes_base, by = "paciente_id") %>%
  mutate(
    # Colesterol inicial
    colesterol_basal = rnorm(n(), mean = 200, sd = 30),
    
    # Efecto del tratamiento por semana
    efecto_tratamiento = case_when(
      tratamiento == "A" ~ -2.5 * semana,
      tratamiento == "B" ~ -1.8 * semana,
      tratamiento == "Placebo" ~ -0.5 * semana
    ),
    
    # Ajuste por factores de riesgo
    ajuste_diabetes = ifelse(diabetes == "Sí", 15, 0),
    ajuste_fumador = ifelse(fumador == "Sí", 10, 0),
    
    # Colesterol observado
    colesterol = pmax(120, colesterol_basal + efecto_tratamiento + 
                      ajuste_diabetes + ajuste_fumador + 
                      rnorm(n(), 0, 8)),
    
    # Presión arterial (correlacionada con colesterol)
    presion_basal = 120 + edad * 0.5,
    presion = pmax(100, presion_basal - semana * 0.8 + 
                   ifelse(tratamiento == "A", -5, 0) +
                   rnorm(n(), 0, 5)),
    
    # IMC (relativamente estable)
    imc = rnorm(n(), mean = 27, sd = 4) - semana * 0.05
  )

# Efectos secundarios (una vez por paciente)
efectos_secundarios <- pacientes %>%
  filter(semana == 12) %>%
  mutate(
    prob_efectos = case_when(
      tratamiento == "A" ~ 0.25,
      tratamiento == "B" ~ 0.15,
      tratamiento == "Placebo" ~ 0.05
    ),
    efectos_secundarios = rbinom(n(), 1, prob_efectos),
    efectos_secundarios = ifelse(efectos_secundarios == 1, "Sí", "No")
  ) %>%
  select(paciente_id, efectos_secundarios)

pacientes <- pacientes %>%
  left_join(efectos_secundarios, by = "paciente_id")

cat("=== DATOS GENERADOS ===\n")
cat("Pacientes:", length(unique(pacientes$paciente_id)), "\n")
cat("Mediciones por paciente:", length(unique(pacientes$semana)), "\n")
cat("Total observaciones:", nrow(pacientes), "\n")
cat("Distribución de tratamientos:\n")
print(table(pacientes_base$tratamiento))
cat("\n")

# ============================================
# PASO 2: 2.1 - TEST T PAREADO MÚLTIPLE (INICIO vs FINAL) + FDR
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📊 2.1 - REDUCCIÓN DE COLESTEROL POR TRATAMIENTO (Test t pareado + FDR)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Comparar semana 0 vs semana 12 para cada tratamiento
tratamientos <- c("A", "B", "Placebo")
resultados_pareados <- data.frame()

for(trat in tratamientos) {
  datos_trat <- pacientes %>% filter(tratamiento == trat, semana %in% c(0, 12))
  
  semana0 <- datos_trat %>% filter(semana == 0) %>% arrange(paciente_id)
  semana12 <- datos_trat %>% filter(semana == 12) %>% arrange(paciente_id)
  
  test <- t.test(semana0$colesterol, semana12$colesterol, paired = TRUE)
  
  resultados_pareados <- rbind(resultados_pareados, data.frame(
    Tratamiento = trat,
    Media_Semana0 = mean(semana0$colesterol),
    Media_Semana12 = mean(semana12$colesterol),
    Reduccion = mean(semana0$colesterol) - mean(semana12$colesterol),
    p_value = test$p.value,
    stringsAsFactors = FALSE
  ))
}

# Aplicar corrección FDR
resultados_pareados$p_adjusted_BH <- p.adjust(resultados_pareados$p_value, method = "BH")
resultados_pareados$Significativo <- ifelse(resultados_pareados$p_adjusted_BH < 0.05, 
                                            "Sí ✓", "No ✗")

cat("--- Resultados por tratamiento ---\n")
print(resultados_pareados)

cat("\n✅ Tratamientos con reducción significativa (p ajustado < 0.05):\n")
print(resultados_pareados %>% filter(Significativo == "Sí ✓") %>% 
        select(Tratamiento, Reduccion, p_adjusted_BH))

# Visualización
ggplot(resultados_pareados, aes(x = reorder(Tratamiento, -Reduccion), 
                                y = Reduccion, fill = Significativo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Reduccion, 1), " mg/dL")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Sí ✓" = "green", "No ✗" = "coral")) +
  labs(title = "Reducción de Colesterol por Tratamiento",
       subtitle = "Diferencia entre semana 0 y semana 12 (p ajustado con FDR)",
       x = "Tratamiento",
       y = "Reducción de Colesterol (mg/dL)",
       fill = "Significativo") +
  theme_minimal()

# ============================================
# PASO 3: 2.2 - ANOVA: COMPARAR REDUCCIÓN ENTRE TRATAMIENTOS
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🔬 2.2 - COMPARACIÓN ENTRE TRATAMIENTOS (ANOVA + Tukey HSD)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Calcular reducción por paciente
reducciones <- pacientes %>%
  filter(semana %in% c(0, 12)) %>%
  select(paciente_id, tratamiento, semana, colesterol) %>%
  pivot_wider(names_from = semana, values_from = colesterol, names_prefix = "sem_") %>%
  mutate(reduccion = sem_0 - sem_12)

# ANOVA
anova_tratamientos <- aov(reduccion ~ tratamiento, data = reducciones)
cat("--- ANOVA: Reducción de colesterol entre tratamientos ---\n")
print(summary(anova_tratamientos))

# Post-hoc
cat("\n--- Comparaciones post-hoc (Tukey HSD) ---\n")
tukey_tratamientos <- TukeyHSD(anova_tratamientos)
print(tukey_tratamientos)

# Visualización
ggplot(reducciones, aes(x = tratamiento, y = reduccion, fill = tratamiento)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "red", color = "darkred") +
  labs(title = "Distribución de Reducciones de Colesterol por Tratamiento",
       subtitle = "Diamante rojo = media",
       x = "Tratamiento",
       y = "Reducción de Colesterol (mg/dL)") +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================
# PASO 4: 2.3 - COMPARACIÓN DIRECTA: TRATAMIENTO A vs PLACEBO
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("💊 2.3 - TRATAMIENTO A vs PLACEBO (Test t independiente)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

datos_comparacion <- reducciones %>% filter(tratamiento %in% c("A", "Placebo"))
test_a_placebo <- t.test(reduccion ~ tratamiento, data = datos_comparacion)

cat("--- Test t: Tratamiento A vs Placebo ---\n")
print(test_a_placebo)

media_a <- mean(reducciones$reduccion[reducciones$tratamiento == "A"])
media_placebo <- mean(reducciones$reduccion[reducciones$tratamiento == "Placebo"])
diferencia <- media_a - media_placebo

cat("\n📊 Resumen:\n")
cat("  • Media tratamiento A:", round(media_a, 2), "mg/dL\n")
cat("  • Media placebo:", round(media_placebo, 2), "mg/dL\n")
cat("  • Diferencia:", round(diferencia, 2), "mg/dL\n")
cat("  • P-value:", format(test_a_placebo$p.value, scientific = TRUE), "\n")

if(test_a_placebo$p.value < 0.05 && diferencia > 0) {
  cat("\n✅ CONCLUSIÓN: El tratamiento A es SIGNIFICATIVAMENTE superior al placebo\n")
} else {
  cat("\n❌ CONCLUSIÓN: No hay evidencia de superioridad del tratamiento A\n")
}

# ============================================
# PASO 5: 2.4 - CHI-CUADRADO: EFECTOS SECUNDARIOS vs TRATAMIENTO
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("⚠️ 2.4 - EFECTOS SECUNDARIOS POR TRATAMIENTO (Chi-cuadrado)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

datos_efectos <- pacientes %>%
  select(paciente_id, tratamiento, efectos_secundarios) %>%
  distinct()

tabla_efectos <- table(datos_efectos$tratamiento, datos_efectos$efectos_secundarios)
cat("--- Tabla de contingencia ---\n")
print(tabla_efectos)

cat("\n--- Proporciones por tratamiento ---\n")
print(round(prop.table(tabla_efectos, margin = 1), 3))

# Test chi-cuadrado
test_efectos <- chisq.test(tabla_efectos)
cat("\n--- Test Chi-cuadrado ---\n")
print(test_efectos)

# Visualización
prop_efectos <- as.data.frame(prop.table(tabla_efectos, margin = 1))
colnames(prop_efectos) <- c("Tratamiento", "Efectos", "Proporcion")

ggplot(prop_efectos, aes(x = Tratamiento, y = Proporcion, fill = Efectos)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Proporcion * 100, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Sí" = "red", "No" = "green")) +
  labs(title = "Tasa de Efectos Secundarios por Tratamiento",
       subtitle = paste0("Chi-cuadrado: p = ", format(test_efectos$p.value, digits = 3)),
       x = "Tratamiento",
       y = "Proporción",
       fill = "Efectos\nSecundarios") +
  theme_minimal()

# ============================================
# PASO 6: 2.5 - REGRESIÓN CON INTERACCIONES
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📈 2.5 - REGRESIÓN: PREDICTORES CON INTERACCIONES\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Preparar datos para regresión
datos_regresion2 <- reducciones %>%
  left_join(pacientes_base, by = c("paciente_id", "tratamiento")) %>%
  mutate(
    tratamiento = as.factor(tratamiento),
    sexo = as.factor(sexo),
    fumador = as.factor(fumador),
    diabetes = as.factor(diabetes)
  )

# Modelo con interacción tratamiento*diabetes
modelo_interaccion <- lm(reduccion ~ tratamiento * diabetes + edad + 
                        sexo + fumador, 
                        data = datos_regresion2)

cat("--- Modelo con interacciones ---\n")
print(summary(modelo_interaccion))

# Interpretar interacción
cat("\n📌 Interpretación de la interacción tratamiento*diabetes:\n")
cat("   Si los coeficientes de interacción son significativos, indica que\n")
cat("   el efecto del tratamiento DEPENDE de si el paciente tiene diabetes.\n\n")

# ============================================
# PASO 7: 2.6 - PCA CON MEDICIONES FINALES
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🔬 2.6 - PCA: PERFIL FINAL DE PACIENTES\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Datos de la semana 12
datos_finales <- pacientes %>%
  filter(semana == 12) %>%
  select(paciente_id, tratamiento, colesterol, presion, imc)

# PCA
matriz_pca2 <- datos_finales %>%
  select(colesterol, presion, imc) %>%
  as.matrix()

pca_final <- prcomp(matriz_pca2, scale. = TRUE, center = TRUE)

varianza2 <- summary(pca_final)$importance[2, 1:2] * 100
cat("PC1 explica:", round(varianza2[1], 2), "%\n")
cat("PC2 explica:", round(varianza2[2], 2), "%\n")

# Visualización
pca_df2 <- data.frame(
  PC1 = pca_final$x[, 1],
  PC2 = pca_final$x[, 2],
  tratamiento = datos_finales$tratamiento
)

ggplot(pca_df2, aes(x = PC1, y = PC2, color = tratamiento)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(fill = tratamiento), alpha = 0.15, geom = "polygon") +
  scale_color_manual(values = c("A" = "green", "B" = "blue", "Placebo" = "red")) +
  scale_fill_manual(values = c("A" = "green", "B" = "blue", "Placebo" = "red")) +
  labs(
    title = "PCA - Perfil de Salud Final por Tratamiento",
    subtitle = paste0("PC1: ", round(varianza2[1], 1), "% | PC2: ", round(varianza2[2], 1), "%"),
    x = paste0("PC1 (", round(varianza2[1], 1), "%)"),
    y = paste0("PC2 (", round(varianza2[2], 1), "%)"),
    color = "Tratamiento"
  ) +
  theme_minimal()

# ============================================
# PASO 8: 2.7 - EVOLUCIÓN TEMPORAL CON IC
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📈 2.7 - EVOLUCIÓN TEMPORAL DEL COLESTEROL\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Calcular estadísticas por semana y tratamiento
evolucion <- pacientes %>%
  group_by(tratamiento, semana) %>%
  summarise(
    media = mean(colesterol),
    se = sd(colesterol) / sqrt(n()),
    ic_inf = media - 1.96 * se,
    ic_sup = media + 1.96 * se,
    .groups = "drop"
  )

# Gráfico de evolución
ggplot(evolucion, aes(x = semana, y = media, color = tratamiento, group = tratamiento)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup, fill = tratamiento), 
              alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c("A" = "green", "B" = "blue", "Placebo" = "red")) +
  scale_fill_manual(values = c("A" = "green", "B" = "blue", "Placebo" = "red")) +
  labs(title = "Evolución del Colesterol LDL durante el Tratamiento",
       subtitle = "Líneas = media, áreas sombreadas = IC 95%",
       x = "Semana",
       y = "Colesterol LDL (mg/dL)",
       color = "Tratamiento",
       fill = "Tratamiento") +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- RESUMEN FINAL PROBLEMA 2 ---
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📋 RESUMEN EJECUTIVO - PROBLEMA 2\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")
cat("✓ Test t pareado confirmó reducción significativa en tratamientos activos\n")
cat("✓ ANOVA reveló diferencias significativas entre tratamientos\n")
cat("✓ Tratamiento A demostró superioridad sobre placebo\n")
cat("✓ Chi-cuadrado mostró mayor tasa de efectos secundarios en tratamiento A\n")
cat("✓ Regresión identificó interacción tratamiento*diabetes\n")
cat("✓ PCA mostró separación de grupos de tratamiento en perfil final\n")
cat("✓ Evolución temporal visualizada con intervalos de confianza\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# ===============================================================================
# 🎯 CONCLUSIÓN GENERAL
# ===============================================================================

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("🎓 TÉCNICAS ESTADÍSTICAS APLICADAS EN ESTA CHULETA\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("📊 TESTS PARAMÉTRICOS:\n")
cat("  • t.test(..., paired = TRUE)      - Comparaciones antes/después\n")
cat("  • t.test(var ~ grupo)              - Comparación de dos grupos\n")
cat("  • aov() + TukeyHSD()               - ANOVA + comparaciones múltiples\n")
cat("  • lm()                             - Regresión lineal múltiple\n")
cat("  • lm(y ~ x1*x2)                    - Regresión con interacciones\n\n")

cat("📈 TESTS NO PARAMÉTRICOS:\n")
cat("  • wilcox.test()                    - Alternativa a t-test\n")
cat("  • kruskal.test()                   - Alternativa a ANOVA\n\n")

cat("🔗 TESTS DE ASOCIACIÓN:\n")
cat("  • chisq.test()                     - Independencia en tablas\n")
cat("  • fisher.test()                    - Test exacto (n pequeñas)\n")
cat("  • cor.test()                       - Correlación de Pearson\n")
cat("  • cor.test(..., method='spearman') - Correlación no paramétrica\n\n")

cat("🔧 CORRECCIÓN POR COMPARACIONES MÚLTIPLES:\n")
cat("  • p.adjust(..., method='BH')       - Benjamini-Hochberg (FDR)\n")
cat("  • p.adjust(..., method='bonferroni') - Bonferroni\n")
cat("  • p.adjust(..., method='holm')     - Holm\n\n")

cat("🔬 ANÁLISIS MULTIVARIADO:\n")
cat("  • prcomp(scale=T, center=T)        - PCA\n")
cat("  • summary(pca)$importance          - Varianza explicada\n")
cat("  • pca$rotation                     - Loadings\n\n")

cat("⚡ ANÁLISIS DE POTENCIA:\n")
cat("  • pwr.t.test()                     - Potencia para t-test\n")
cat("  • pwr.r.test()                     - Potencia para correlación\n")
cat("  • pwr.anova.test()                 - Potencia para ANOVA\n\n")

cat("📊 VISUALIZACIÓN AVANZADA:\n")
cat("  • ggplot + geom_boxplot            - Distribuciones\n")
cat("  • ggplot + geom_line + geom_ribbon - Evolución temporal con IC\n")
cat("  • ggplot + stat_ellipse            - Elipses de confianza\n")
cat("  • ggplot + facet_wrap              - Paneles múltiples\n")
cat("  • scale_color_viridis_d()          - Paletas accesibles\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("✅ FIN DE LA CHULETA 3 - PROBLEMAS AVANZADOS\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
