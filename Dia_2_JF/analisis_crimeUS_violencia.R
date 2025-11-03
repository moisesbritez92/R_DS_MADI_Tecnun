# =============================================================================
# ANÁLISIS DE CRIMINALIDAD EN ESTADOS UNIDOS
# Ejercicio: Comparación de Crímenes Violentos entre Estados
# =============================================================================

# Cargar librerías necesarias
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualización

# =============================================================================
# PASO 1: Leer el archivo crimeUS.csv
# =============================================================================

# Leer el archivo CSV con los datos de criminalidad en 50 estados de USA
# header = TRUE: la primera fila contiene los nombres de columnas
# stringsAsFactors = FALSE: no convertir texto a factores automáticamente
crime_data <- read.csv('crimeUS.csv', 
                       header = TRUE, 
                       stringsAsFactors = FALSE)

cat("\n=== PASO 1: CARGA DE DATOS ===\n")
cat("Dataset cargado exitosamente\n")
cat("Dimensiones:", nrow(crime_data), "filas x", ncol(crime_data), "columnas\n\n")

# Verificar las primeras filas del dataset
cat("Primeras filas del dataset:\n")
head(crime_data, 10)

# Ver estructura del dataset
cat("\nEstructura del dataset:\n")
str(crime_data)

# Verificar columnas relevantes para el ejercicio
cat("\nColumnas clave para el análisis:\n")
cat("- violent: número absoluto de crímenes violentos\n")
cat("- population: población del estado\n")
cat("- state: nombre del estado\n")
cat("- year: año de observación\n\n")


# =============================================================================
# PASO 2: Transformar crímenes violentos a tasa por 100,000 habitantes
# =============================================================================

cat("\n=== PASO 2: CÁLCULO DE TASA DE CRÍMENES VIOLENTOS ===\n\n")

# FÓRMULA: Tasa = (Número de crímenes / Población) * 100,000
# Esto permite comparar estados con diferentes tamaños de población
# de manera justa y estandarizada

# Crear nueva columna con la tasa de crímenes violentos por 100,000 habitantes
crime_data <- crime_data %>%
  mutate(
    violent_rate = (violent / population) * 100000
  )

cat("✓ Nueva columna 'violent_rate' creada exitosamente\n\n")

# Verificar los primeros registros con la nueva columna
cat("Ejemplo de tasas de crímenes violentos (por 100,000 hab):\n")
crime_data %>%
  select(state, year, violent, population, violent_rate) %>%
  head(10) %>%
  print()

# Estadísticas descriptivas de la tasa de criminalidad
cat("\n\nEstadísticas descriptivas de la tasa de crímenes violentos:\n")
cat("================================================\n")
cat("Mínimo:", round(min(crime_data$violent_rate, na.rm = TRUE), 2), "\n")
cat("Media:", round(mean(crime_data$violent_rate, na.rm = TRUE), 2), "\n")
cat("Mediana:", round(median(crime_data$violent_rate, na.rm = TRUE), 2), "\n")
cat("Máximo:", round(max(crime_data$violent_rate, na.rm = TRUE), 2), "\n")
cat("Desviación estándar:", round(sd(crime_data$violent_rate, na.rm = TRUE), 2), "\n\n")


# =============================================================================
# PASO 3: ANOVA - Comparar Alaska, Georgia y Missouri
# =============================================================================

cat("\n=== PASO 3: ANÁLISIS ANOVA - Alaska vs Georgia vs Missouri ===\n\n")

# Filtrar datos solo para los tres estados de interés
# %in% verifica si un valor está en un vector dado
estados_grupo1 <- c("Alaska", "Georgia", "Missouri")

datos_grupo1 <- crime_data %>%
  filter(state %in% estados_grupo1)

cat("Número de observaciones por estado:\n")
table(datos_grupo1$state)
cat("\n")

# Calcular estadísticas descriptivas por estado
cat("Estadísticas descriptivas por estado:\n")
cat("=====================================\n\n")

estadisticas_grupo1 <- datos_grupo1 %>%
  group_by(state) %>%
  summarise(
    n = n(),                                    # Número de observaciones
    media = mean(violent_rate, na.rm = TRUE),  # Media
    mediana = median(violent_rate, na.rm = TRUE),  # Mediana
    sd = sd(violent_rate, na.rm = TRUE),       # Desviación estándar
    min = min(violent_rate, na.rm = TRUE),     # Mínimo
    max = max(violent_rate, na.rm = TRUE),     # Máximo
    .groups = 'drop'
  )

print(estadisticas_grupo1, width = Inf)

# -----------------------------------------------
# Realizar prueba ANOVA de una vía
# -----------------------------------------------

cat("\n\nPRUEBA ANOVA DE UNA VÍA\n")
cat("========================\n\n")

# ANOVA (Analysis of Variance) compara las medias de tres o más grupos
# H0 (hipótesis nula): Las medias de los tres estados son iguales
# H1 (hipótesis alternativa): Al menos una media es diferente

# Realizar ANOVA
# aov(): función para ANOVA
# Formula: variable_respuesta ~ variable_agrupadora
anova_resultado1 <- aov(violent_rate ~ state, data = datos_grupo1)

# Obtener resumen del ANOVA
summary_anova1 <- summary(anova_resultado1)
print(summary_anova1)

# Extraer estadísticos clave
F_statistic <- summary_anova1[[1]][["F value"]][1]
p_value <- summary_anova1[[1]][["Pr(>F)"]][1]

cat("\n\nRESULTADOS CLAVE:\n")
cat("=================\n")
cat("Estadístico F:", round(F_statistic, 4), "\n")
cat("Valor p (p-value):", format(p_value, scientific = FALSE, digits = 6), "\n\n")

# Interpretación del resultado
alpha <- 0.05  # Nivel de significancia estándar

cat("INTERPRETACIÓN (nivel de significancia α = 0.05):\n")
cat("==================================================\n\n")

if(p_value < alpha) {
  cat("✓ p-value < 0.05: RECHAZAMOS la hipótesis nula\n")
  cat("  → Existe una diferencia SIGNIFICATIVA entre las medias\n")
  cat("  → Las tasas de criminalidad violenta NO son iguales entre\n")
  cat("    Alaska, Georgia y Missouri\n")
  cat("  → Los estados tienen niveles de criminalidad estadísticamente diferentes\n\n")
  cat("CONCLUSIÓN: Los resultados SON significativos\n")
} else {
  cat("✗ p-value ≥ 0.05: NO RECHAZAMOS la hipótesis nula\n")
  cat("  → NO hay evidencia suficiente de diferencias significativas\n")
  cat("  → Las tasas de criminalidad violenta son similares entre\n")
  cat("    Alaska, Georgia y Missouri\n\n")
  cat("CONCLUSIÓN: Los resultados NO son significativos\n")
}

# -----------------------------------------------
# Pruebas post-hoc (si hay diferencias significativas)
# -----------------------------------------------

if(p_value < alpha) {
  cat("\n\nPRUEBAS POST-HOC (Comparaciones múltiples de Tukey)\n")
  cat("===================================================\n\n")
  
  # TukeyHSD: prueba de Tukey para comparaciones múltiples
  # Compara todos los pares de estados para ver cuáles son diferentes
  tukey_result1 <- TukeyHSD(anova_resultado1)
  print(tukey_result1)
  
  cat("\nInterpretación de Tukey HSD:\n")
  cat("- 'p adj' < 0.05: diferencia significativa entre ese par de estados\n")
  cat("- 'diff': diferencia entre las medias de los dos estados\n")
  cat("- 'lwr' y 'upr': límites del intervalo de confianza del 95%\n\n")
}


# =============================================================================
# PASO 4: Comparación y Boxplot - District of Columbia, Hawaii e Idaho
# =============================================================================

cat("\n\n=== PASO 4: ANÁLISIS - District of Columbia vs Hawaii vs Idaho ===\n\n")

# Filtrar datos para los tres estados del segundo grupo
estados_grupo2 <- c("District of Columbia", "Hawaii", "Idaho")

datos_grupo2 <- crime_data %>%
  filter(state %in% estados_grupo2)

cat("Número de observaciones por estado:\n")
table(datos_grupo2$state)
cat("\n")

# Calcular estadísticas descriptivas por estado
cat("Estadísticas descriptivas por estado:\n")
cat("=====================================\n\n")

estadisticas_grupo2 <- datos_grupo2 %>%
  group_by(state) %>%
  summarise(
    n = n(),
    media = mean(violent_rate, na.rm = TRUE),
    mediana = median(violent_rate, na.rm = TRUE),
    sd = sd(violent_rate, na.rm = TRUE),
    min = min(violent_rate, na.rm = TRUE),
    max = max(violent_rate, na.rm = TRUE),
    .groups = 'drop'
  )

print(estadisticas_grupo2, width = Inf)

# -----------------------------------------------
# Realizar prueba ANOVA
# -----------------------------------------------

cat("\n\nPRUEBA ANOVA DE UNA VÍA\n")
cat("========================\n\n")

anova_resultado2 <- aov(violent_rate ~ state, data = datos_grupo2)
summary_anova2 <- summary(anova_resultado2)
print(summary_anova2)

# Extraer estadísticos
F_statistic2 <- summary_anova2[[1]][["F value"]][1]
p_value2 <- summary_anova2[[1]][["Pr(>F)"]][1]

cat("\n\nRESULTADOS CLAVE:\n")
cat("=================\n")
cat("Estadístico F:", round(F_statistic2, 4), "\n")
cat("Valor p (p-value):", format(p_value2, scientific = FALSE, digits = 6), "\n\n")

# Interpretación
cat("INTERPRETACIÓN (nivel de significancia α = 0.05):\n")
cat("==================================================\n\n")

if(p_value2 < alpha) {
  cat("✓ p-value < 0.05: RECHAZAMOS la hipótesis nula\n")
  cat("  → Existe una diferencia SIGNIFICATIVA entre las medias\n")
  cat("  → Las tasas de criminalidad violenta NO son iguales entre\n")
  cat("    District of Columbia, Hawaii e Idaho\n\n")
  cat("CONCLUSIÓN: Los resultados SON significativos\n")
} else {
  cat("✗ p-value ≥ 0.05: NO RECHAZAMOS la hipótesis nula\n")
  cat("  → NO hay evidencia suficiente de diferencias significativas\n")
  cat("  → Las tasas de criminalidad violenta son similares entre\n")
  cat("    los tres estados\n\n")
  cat("CONCLUSIÓN: Los resultados NO son significativos\n")
}

# Pruebas post-hoc si hay diferencias
if(p_value2 < alpha) {
  cat("\n\nPRUEBAS POST-HOC (Comparaciones múltiples de Tukey)\n")
  cat("===================================================\n\n")
  
  tukey_result2 <- TukeyHSD(anova_resultado2)
  print(tukey_result2)
}

# -----------------------------------------------
# CREAR BOXPLOT para visualizar diferencias
# -----------------------------------------------

cat("\n\nGENERANDO BOXPLOT...\n")
cat("====================\n\n")

# Convertir state a factor con orden específico para mejor visualización
datos_grupo2$state <- factor(datos_grupo2$state, 
                              levels = c("District of Columbia", "Hawaii", "Idaho"))

# Crear boxplot con ggplot2
# Boxplot muestra:
# - Mediana (línea central)
# - Cuartiles Q1 y Q3 (caja)
# - Rango intercuartílico
# - Valores atípicos (puntos fuera de los bigotes)

boxplot_grupo2 <- ggplot(datos_grupo2, aes(x = state, y = violent_rate, fill = state)) +
  # geom_boxplot: crear el diagrama de caja
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  
  # geom_jitter: agregar puntos individuales con dispersión aleatoria
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  
  # Paleta de colores personalizada
  scale_fill_manual(values = c("District of Columbia" = "#E74C3C", 
                                "Hawaii" = "#3498DB", 
                                "Idaho" = "#2ECC71")) +
  
  # Etiquetas y títulos
  labs(
    title = "Comparación de Tasas de Crímenes Violentos",
    subtitle = "District of Columbia vs Hawaii vs Idaho (1977-1999)",
    x = "Estado",
    y = "Tasa de crímenes violentos\n(por 100,000 habitantes)",
    caption = paste0("ANOVA F-statistic: ", round(F_statistic2, 2), 
                     " | p-value: ", format(p_value2, digits = 4))
  ) +
  
  # Tema y estilo
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none",  # Ocultar leyenda (redundante con eje x)
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5, color = "gray50", size = 9)
  ) +
  
  # Agregar línea horizontal con la media general
  geom_hline(yintercept = mean(datos_grupo2$violent_rate, na.rm = TRUE), 
             linetype = "dashed", color = "gray40", size = 0.8) +
  
  # Agregar anotación con la media
  annotate("text", x = 2, y = mean(datos_grupo2$violent_rate, na.rm = TRUE) + 50,
           label = paste("Media general:", round(mean(datos_grupo2$violent_rate), 1)),
           color = "gray40", size = 3.5)

# Mostrar el gráfico
print(boxplot_grupo2)

# Guardar el gráfico en alta resolución
ggsave(filename = "boxplot_crimenes_violentos.png", 
       plot = boxplot_grupo2,
       width = 10, 
       height = 7, 
       units = "in", 
       dpi = 300)

cat("✓ Boxplot guardado como 'boxplot_crimenes_violentos.png'\n\n")


# =============================================================================
# RESUMEN COMPARATIVO FINAL
# =============================================================================

cat("\n\n=== RESUMEN COMPARATIVO FINAL ===\n")
cat("==================================\n\n")

# Crear tabla comparativa
tabla_comparativa <- data.frame(
  Grupo = c("Grupo 1", "Grupo 2"),
  Estados = c("Alaska, Georgia, Missouri", 
              "District of Columbia, Hawaii, Idaho"),
  F_Statistic = c(round(F_statistic, 4), round(F_statistic2, 4)),
  P_Value = c(format(p_value, scientific = FALSE, digits = 6), 
              format(p_value2, scientific = FALSE, digits = 6)),
  Significativo = c(
    ifelse(p_value < alpha, "Sí (p < 0.05)", "No (p ≥ 0.05)"),
    ifelse(p_value2 < alpha, "Sí (p < 0.05)", "No (p ≥ 0.05)")
  )
)

print(tabla_comparativa, row.names = FALSE)

cat("\n\nINTERPRETACIÓN GENERAL:\n")
cat("=======================\n\n")

cat("GRUPO 1 (Alaska, Georgia, Missouri):\n")
if(p_value < alpha) {
  cat("- Existen diferencias SIGNIFICATIVAS en las tasas de criminalidad\n")
  cat("- Los estados tienen perfiles de criminalidad distintos\n")
} else {
  cat("- NO hay diferencias significativas en las tasas de criminalidad\n")
  cat("- Los estados tienen perfiles de criminalidad similares\n")
}

cat("\nGRUPO 2 (District of Columbia, Hawaii, Idaho):\n")
if(p_value2 < alpha) {
  cat("- Existen diferencias SIGNIFICATIVAS en las tasas de criminalidad\n")
  cat("- Los estados tienen perfiles de criminalidad distintos\n")
  
  # Identificar el estado con mayor y menor tasa
  estado_max <- estadisticas_grupo2$state[which.max(estadisticas_grupo2$media)]
  estado_min <- estadisticas_grupo2$state[which.min(estadisticas_grupo2$media)]
  
  cat("- ", estado_max, " tiene la tasa MÁS ALTA de criminalidad\n", sep="")
  cat("- ", estado_min, " tiene la tasa MÁS BAJA de criminalidad\n", sep="")
} else {
  cat("- NO hay diferencias significativas en las tasas de criminalidad\n")
  cat("- Los estados tienen perfiles de criminalidad similares\n")
}

cat("\n\nNOTAS METODOLÓGICAS:\n")
cat("====================\n")
cat("1. ANOVA evalúa si hay diferencias entre las MEDIAS de los grupos\n")
cat("2. Un p-value < 0.05 indica diferencias estadísticamente significativas\n")
cat("3. Las pruebas post-hoc (Tukey HSD) identifican qué pares de estados difieren\n")
cat("4. El boxplot visualiza la distribución y dispersión de los datos\n")
cat("5. La tasa por 100,000 habitantes permite comparaciones justas entre estados\n")

cat("\n\n===========================================\n")
cat("✓ ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("===========================================\n\n")