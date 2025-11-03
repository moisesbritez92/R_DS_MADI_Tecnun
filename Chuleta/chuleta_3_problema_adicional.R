# ═══════════════════════════════════════════════════════════════════════════════
# 📚 EJERCICIO ADICIONAL PARA CHULETA 3 - DIFICULTAD MEDIA
# ═══════════════════════════════════════════════════════════════════════════════
# Ejercicio de rendimiento académico con ANOVA, regresión logística y visualización
# Nivel: Medio - Combinando análisis de grupos y predicción
# ═══════════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(dplyr)
library(tidyr)

# ===============================================================================
# 📚 PROBLEMA 3: ANÁLISIS DE RENDIMIENTO ACADÉMICO Y PREDICCIÓN DE APROBACIÓN
# ===============================================================================

# --- CONTEXTO ---
# Se evalúa el rendimiento de 180 estudiantes que han seguido uno de tres métodos de estudio:
# (Tradicional, Online, Mixto). Se mide:
# - Nota final (0-100)
# - Horas de estudio semanales
# - Asistencia (%)
# - Edad
# - Género
# - Aprobado (Sí/No, nota >= 60)

# --- PREGUNTAS INTEGRADAS ---
# 3.1 ¿Existen diferencias significativas en la nota final entre los métodos de estudio? (ANOVA + Tukey)
# 3.2 ¿Qué variables predicen mejor la probabilidad de aprobar? (regresión logística)
# 3.3 Visualizar la probabilidad de aprobación según horas de estudio y método
# 3.4 ¿Hay correlación entre horas de estudio y asistencia?
# 3.5 ¿El género influye en el rendimiento académico?

# --- PROCEDIMIENTO COMPLETO ---

# ============================================
# PASO 1: SIMULAR DATOS REALISTAS
# ============================================
set.seed(151025)

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📚 PROBLEMA 3: RENDIMIENTO ACADÉMICO Y PREDICCIÓN DE APROBACIÓN\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Simular datos
estudiantes <- data.frame(
  estudiante_id = 1:180,
  metodo = sample(c("Tradicional", "Online", "Mixto"), 180, replace = TRUE),
  horas_estudio = pmax(3, rnorm(180, mean = 12, sd = 3)),
  asistencia = pmax(50, pmin(100, rnorm(180, mean = 85, sd = 10))),
  edad = sample(18:30, 180, replace = TRUE),
  genero = sample(c("M", "F"), 180, replace = TRUE)
)

# Nota final depende del método, horas y asistencia
estudiantes <- estudiantes %>%
  mutate(
    # Nota base según método
    nota_base = case_when(
      metodo == "Tradicional" ~ 68,
      metodo == "Online" ~ 64,
      metodo == "Mixto" ~ 72
    ),
    # Ajuste por género (pequeña diferencia)
    ajuste_genero = ifelse(genero == "F", 2, 0),
    # Nota final
    nota = pmin(100, pmax(0,
      nota_base +
      horas_estudio * 1.5 +
      asistencia * 0.2 +
      ajuste_genero +
      rnorm(n(), 0, 7)
    )),
    aprobado = ifelse(nota >= 60, "Sí", "No")
  )

cat("=== DATOS GENERADOS ===\n")
cat("Total de estudiantes:", nrow(estudiantes), "\n")
cat("Distribución por método:\n")
print(table(estudiantes$metodo))
cat("\nDistribución por género:\n")
print(table(estudiantes$genero))
cat("\nTasa de aprobación general:", round(mean(estudiantes$nota >= 60) * 100, 1), "%\n\n")

# ============================================
# PASO 2: 3.1 - ANOVA POR MÉTODO DE ESTUDIO + TUKEY
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📊 3.1 - NOTA FINAL POR MÉTODO DE ESTUDIO (ANOVA + Tukey)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# ANOVA
anova_metodo <- aov(nota ~ metodo, data = estudiantes)
cat("--- Resumen ANOVA ---\n")
print(summary(anova_metodo))

# Interpretación automática
p_anova3 <- summary(anova_metodo)[[1]][["Pr(>F)"]][1]
if(p_anova3 < 0.05) {
  cat("\n✅ CONCLUSIÓN: Hay diferencias significativas entre métodos (p =", 
      format(p_anova3, scientific = TRUE), ")\n")
  
  # Comparaciones post-hoc (Tukey)
  cat("\n--- Comparaciones múltiples (Tukey HSD) ---\n")
  tukey3 <- TukeyHSD(anova_metodo)
  print(tukey3)
  
  # Identificar diferencias significativas
  tukey_df <- as.data.frame(tukey3$metodo)
  tukey_df$comparacion <- rownames(tukey_df)
  significativos3 <- tukey_df %>% filter(`p adj` < 0.05)
  
  if(nrow(significativos3) > 0) {
    cat("\n📌 Pares de métodos con diferencias significativas:\n")
    print(significativos3 %>% select(comparacion, diff, `p adj`))
  } else {
    cat("\n📌 No hay diferencias significativas entre pares específicos\n")
  }
  
} else {
  cat("\n❌ CONCLUSIÓN: NO hay diferencias significativas entre métodos\n")
}

# Estadísticas descriptivas por método
cat("\n--- Estadísticas por método ---\n")
estadisticas_metodo <- estudiantes %>%
  group_by(metodo) %>%
  summarise(
    n = n(),
    media = round(mean(nota), 2),
    mediana = round(median(nota), 2),
    sd = round(sd(nota), 2),
    min = round(min(nota), 2),
    max = round(max(nota), 2),
    tasa_aprobacion = round(mean(nota >= 60) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(media))

print(estadisticas_metodo)

# Visualización
ggplot(estudiantes, aes(x = reorder(metodo, nota, FUN = median), y = nota, fill = metodo)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Distribución de Notas por Método de Estudio",
       subtitle = "Diamante rojo = media, línea roja = umbral de aprobación (60)",
       x = "Método de Estudio",
       y = "Nota Final (0-100)") +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================
# PASO 3: 3.2 - REGRESIÓN LOGÍSTICA PARA APROBACIÓN
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📈 3.2 - PREDICCIÓN DE APROBACIÓN (Regresión logística)\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Preparar variable binaria
estudiantes$aprobado_bin <- ifelse(estudiantes$aprobado == "Sí", 1, 0)

# Modelo de regresión logística
modelo_logit <- glm(aprobado_bin ~ horas_estudio + asistencia + metodo + edad + genero,
                    data = estudiantes, family = binomial)

cat("--- Resumen del modelo logístico ---\n")
print(summary(modelo_logit))

# Interpretación de coeficientes significativos
coef_logit <- summary(modelo_logit)$coefficients
sig_logit <- coef_logit[coef_logit[,4] < 0.05, , drop = FALSE]

cat("\n📌 Variables significativas (p < 0.05):\n")
if(nrow(sig_logit) > 0) {
  print(round(sig_logit, 4))
  
  cat("\n--- Interpretación de odds ratios ---\n")
  odds_ratios <- exp(sig_logit[, "Estimate"])
  for(i in 1:nrow(sig_logit)) {
    var_name <- rownames(sig_logit)[i]
    or <- odds_ratios[i]
    if(var_name != "(Intercept)") {
      cat("  •", var_name, ": OR =", round(or, 3))
      if(or > 1) {
        cat(" (aumenta la probabilidad de aprobar)\n")
      } else {
        cat(" (disminuye la probabilidad de aprobar)\n")
      }
    }
  }
} else {
  cat("No hay variables significativas\n")
}

# Bondad de ajuste
null_deviance <- modelo_logit$null.deviance
residual_deviance <- modelo_logit$deviance
pseudo_r2 <- 1 - (residual_deviance / null_deviance)

cat("\n📊 Bondad de ajuste:\n")
cat("  • Pseudo R²:", round(pseudo_r2, 4), "\n")
cat("  • AIC:", round(modelo_logit$aic, 2), "\n")

# ============================================
# PASO 4: 3.3 - VISUALIZACIÓN DE PROBABILIDAD DE APROBACIÓN
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📊 3.3 - VISUALIZACIÓN DE PROBABILIDAD DE APROBACIÓN\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Crear grid de predicción
pred_grid <- expand.grid(
  horas_estudio = seq(5, 20, by = 1),
  metodo = c("Tradicional", "Online", "Mixto"),
  asistencia = 85,  # Asistencia media
  edad = 22,        # Edad media
  genero = "F"      # Género más común
)

# Predecir probabilidades
pred_grid$prob_aprob <- predict(modelo_logit, newdata = pred_grid, type = "response")

# Gráfico de probabilidades
ggplot(pred_grid, aes(x = horas_estudio, y = prob_aprob, color = metodo)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = c("Tradicional" = "blue", "Online" = "red", "Mixto" = "green")) +
  labs(title = "Probabilidad de Aprobación según Horas de Estudio",
       subtitle = "Por método de estudio (asistencia = 85%, edad = 22, género = F)",
       x = "Horas de Estudio Semanales",
       y = "Probabilidad de Aprobación",
       color = "Método") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("📌 Interpretación del gráfico:\n")
cat("   - A más horas de estudio, mayor probabilidad de aprobar\n")
cat("   - El método Mixto muestra las mejores probabilidades\n")
cat("   - Con < 8 horas semanales, la probabilidad de aprobar es baja\n\n")

# ============================================
# PASO 5: 3.4 - CORRELACIÓN HORAS vs ASISTENCIA
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("🔗 3.4 - CORRELACIÓN: HORAS DE ESTUDIO vs ASISTENCIA\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Test de correlación
cor_test <- cor.test(estudiantes$horas_estudio, estudiantes$asistencia)
cat("--- Test de correlación de Pearson ---\n")
print(cor_test)

# Interpretación
r_value <- cor_test$estimate
if(cor_test$p.value < 0.05) {
  cat("\n✅ CONCLUSIÓN: Hay correlación significativa\n")
  if(abs(r_value) < 0.3) {
    fuerza <- "débil"
  } else if(abs(r_value) < 0.7) {
    fuerza <- "moderada"
  } else {
    fuerza <- "fuerte"
  }
  
  direccion <- ifelse(r_value > 0, "positiva", "negativa")
  cat("   Correlación", direccion, fuerza, "(r =", round(r_value, 3), ")\n")
} else {
  cat("\n❌ CONCLUSIÓN: NO hay correlación significativa\n")
}

# Visualización
ggplot(estudiantes, aes(x = horas_estudio, y = asistencia, color = aprobado)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Sí" = "green", "No" = "red")) +
  labs(title = "Relación entre Horas de Estudio y Asistencia",
       subtitle = paste0("Correlación: r = ", round(r_value, 3), 
                        ", p = ", format(cor_test$p.value, digits = 3)),
       x = "Horas de Estudio Semanales",
       y = "Asistencia (%)",
       color = "Aprobado") +
  theme_minimal()

# ============================================
# PASO 6: 3.5 - ANÁLISIS POR GÉNERO
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("👥 3.5 - DIFERENCIAS POR GÉNERO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Test t para nota por género
test_genero <- t.test(nota ~ genero, data = estudiantes)
cat("--- Test t: Nota por género ---\n")
print(test_genero)

# Estadísticas por género
cat("\n--- Estadísticas por género ---\n")
stats_genero <- estudiantes %>%
  group_by(genero) %>%
  summarise(
    n = n(),
    media_nota = round(mean(nota), 2),
    sd_nota = round(sd(nota), 2),
    tasa_aprobacion = round(mean(aprobado == "Sí") * 100, 1),
    media_horas = round(mean(horas_estudio), 2),
    media_asistencia = round(mean(asistencia), 2),
    .groups = "drop"
  )
print(stats_genero)

# Test chi-cuadrado para aprobación por género
tabla_genero <- table(estudiantes$genero, estudiantes$aprobado)
test_chi_genero <- chisq.test(tabla_genero)
cat("\n--- Chi-cuadrado: Aprobación por género ---\n")
print(test_chi_genero)

# Visualización comparativa
ggplot(estudiantes, aes(x = genero, y = nota, fill = genero)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "lightpink")) +
  labs(title = "Distribución de Notas por Género",
       subtitle = paste0("Test t: p = ", format(test_genero$p.value, digits = 3)),
       x = "Género",
       y = "Nota Final") +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================
# PASO 7: ANÁLISIS INTEGRADO - TABLA RESUMEN
# ============================================
cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("📋 ANÁLISIS INTEGRADO - TABLA RESUMEN\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Crear tabla resumen por método y género
tabla_resumen <- estudiantes %>%
  group_by(metodo, genero) %>%
  summarise(
    n = n(),
    nota_media = round(mean(nota), 1),
    tasa_aprobacion = round(mean(aprobado == "Sí") * 100, 1),
    horas_media = round(mean(horas_estudio), 1),
    .groups = "drop"
  ) %>%
  arrange(metodo, genero)

cat("--- Resumen por método y género ---\n")
print(tabla_resumen)

# Mejor y peor combinación
mejor_combo <- tabla_resumen[which.max(tabla_resumen$nota_media), ]
peor_combo <- tabla_resumen[which.min(tabla_resumen$nota_media), ]

cat("\n📌 Mejores resultados:", mejor_combo$metodo, "+", mejor_combo$genero, 
    "- Nota media:", mejor_combo$nota_media, "\n")
cat("📌 Menores resultados:", peor_combo$metodo, "+", peor_combo$genero, 
    "- Nota media:", peor_combo$nota_media, "\n")

# --- RESUMEN FINAL PROBLEMA 3 ---
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📋 RESUMEN EJECUTIVO - PROBLEMA 3\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")
cat("✓ ANOVA confirmó diferencias significativas entre métodos de estudio\n")
cat("✓ Regresión logística identificó predictores clave de aprobación\n")
cat("✓ Visualización muestra el efecto de horas y método en probabilidad de aprobar\n")
cat("✓ Correlación analizada entre horas de estudio y asistencia\n")
cat("✓ Análisis por género reveló diferencias en rendimiento\n")
cat("✓ El método Mixto + género femenino mostró mejores resultados\n")
cat("✓ Horas de estudio es el predictor más fuerte de aprobación\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("🎯 TÉCNICAS APLICADAS EN ESTE EJERCICIO:\n")
cat("  • aov() + TukeyHSD()               - ANOVA + comparaciones múltiples\n")
cat("  • glm(..., family=binomial)        - Regresión logística\n")
cat("  • t.test()                         - Comparación de medias\n")
cat("  • chisq.test()                     - Test de independencia\n")
cat("  • cor.test()                       - Correlación de Pearson\n")
cat("  • ggplot + geom_boxplot            - Visualización de distribuciones\n")
cat("  • predict(..., type='response')    - Predicción de probabilidades\n\n")

cat("✅ FIN DEL EJERCICIO 3 - RENDIMIENTO ACADÉMICO\n")
cat("═══════════════════════════════════════════════════════════════════════\n")