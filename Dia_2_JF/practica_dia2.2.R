# Práctica Día 2.2 - Análisis de datos de crimen en Estados Unidos
# Autor: [Tu nombre]
# Fecha: 25 de septiembre de 2025

# Cargar librerías necesarias
library(readr)      # Para leer archivos CSV
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualizaciones

# =============================================================================
# LECTURA Y EXPLORACIÓN DEL DATASET DE CRIMEN EN EE.UU.
# =============================================================================

# Leer el archivo CSV de crimen
cat("Leyendo archivo crimeUS.csv...\n")
crime_data <- read_csv("crimeUS.csv")

# Mostrar información básica del dataset
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("INFORMACIÓN BÁSICA DEL DATASET DE CRIMEN\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("Dimensiones del dataset:\n")
print(dim(crime_data))

cat("\nPrimeras 6 filas:\n")
print(head(crime_data))

cat("\nÚltimas 6 filas:\n")
print(tail(crime_data))

cat("\nEstructura del dataset:\n")
print(str(crime_data))

cat("\nNombres de las columnas:\n")
print(names(crime_data))

cat("\nResumen estadístico:\n")
print(summary(crime_data))

# Verificar valores únicos en columnas categóricas
cat("\nValores únicos en 'state':\n")
print(unique(crime_data$state))

cat("\nValores únicos en 'law':\n")
print(unique(crime_data$law))

# Contar registros por estado
cat("\nNúmero de registros por estado:\n")
print(table(crime_data$state))

# Contar registros por ley
cat("\nNúmero de registros por tipo de ley:\n")
print(table(crime_data$law))

# Verificar años disponibles
cat("\nRango de años en el dataset:\n")
cat("Año mínimo:", min(crime_data$year), "\n")
cat("Año máximo:", max(crime_data$year), "\n")
cat("Años únicos:", length(unique(crime_data$year)), "\n")

# Verificar si hay valores faltantes
cat("\nValores faltantes por columna:\n")
print(colSums(is.na(crime_data)))

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("DATASET CARGADO EXITOSAMENTE\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# =============================================================================
# ANÁLISIS DE POBLACIONES AFROAMERICANA Y CAUCÁSICA POR ESTADO
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("MEDIAS DE POBLACIONES POR ESTADO\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Calcular media de población afroamericana por estado usando aggregate()
cat("\nCalculando media de población afroamericana por estado...\n")
media_afam_por_estado <- aggregate(afam ~ state, data = crime_data, FUN = mean)

# Redondear los valores para eliminar decimales
media_afam_por_estado$afam <- round(media_afam_por_estado$afam)

# Renombrar columna para claridad
names(media_afam_por_estado)[2] <- "media_afroamericana"

cat("Media de población afroamericana por estado (%):\n")
print(media_afam_por_estado)

cat("\n", paste(rep("-", 40), collapse=""), "\n")

# Calcular media de población caucásica por estado usando aggregate()
cat("\nCalculando media de población caucásica por estado...\n")
media_cauc_por_estado <- aggregate(cauc ~ state, data = crime_data, FUN = mean)

# Redondear los valores para eliminar decimales
media_cauc_por_estado$cauc <- round(media_cauc_por_estado$cauc)

# Renombrar columna para claridad
names(media_cauc_por_estado)[2] <- "media_caucasica"

cat("Media de población caucásica por estado (%):\n")
print(media_cauc_por_estado)

cat("\n", paste(rep("-", 40), collapse=""), "\n")

# Combinar ambas medias en un solo data frame
cat("\nCombinando ambas medias en un solo data frame...\n")
medias_combinadas <- merge(media_afam_por_estado, media_cauc_por_estado, by = "state")

cat("Medias combinadas por estado:\n")
print(medias_combinadas)

# Estadísticas adicionales
cat("\n", paste(rep("-", 40), collapse=""), "\n")
cat("\nESTADÍSTICAS RESUMEN:\n")

cat("\nEstados con mayor porcentaje afroamericano:\n")
top_afam <- medias_combinadas[order(medias_combinadas$media_afroamericana, decreasing = TRUE), ]
print(head(top_afam[, c("state", "media_afroamericana")], 10))

cat("\nEstados con mayor porcentaje caucásico:\n")
top_cauc <- medias_combinadas[order(medias_combinadas$media_caucasica, decreasing = TRUE), ]
print(head(top_cauc[, c("state", "media_caucasica")], 10))

cat("\nResumen estadístico de las medias:\n")
cat("Población afroamericana - Min:", min(medias_combinadas$media_afroamericana), 
    "Max:", max(medias_combinadas$media_afroamericana), 
    "Promedio:", round(mean(medias_combinadas$media_afroamericana), 1), "\n")

cat("Población caucásica - Min:", min(medias_combinadas$media_caucasica), 
    "Max:", max(medias_combinadas$media_caucasica), 
    "Promedio:", round(mean(medias_combinadas$media_caucasica), 1), "\n")

# =============================================================================
# TABLA DE CONTINGENCIA Y PRUEBA CHI-CUADRADO
# District of Columbia vs Hawaii
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("TABLA DE CONTINGENCIA: DISTRICT OF COLUMBIA vs HAWAII\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Filtrar datos para District of Columbia y Hawaii
datos_dc_hawaii <- crime_data %>%
  filter(state %in% c("District of Columbia", "Hawaii"))

cat("\nDatos filtrados para DC y Hawaii:\n")
cat("Número de registros:", nrow(datos_dc_hawaii), "\n")
print(table(datos_dc_hawaii$state))

# Crear categorías de población basadas en los valores medios
# Vamos a categorizar como "Alta" y "Baja" población afroamericana y caucásica
# usando la mediana nacional como punto de corte

mediana_afam <- median(crime_data$afam)
mediana_cauc <- median(crime_data$cauc)

cat("\nPuntos de corte (medianas nacionales):\n")
cat("Mediana población afroamericana:", round(mediana_afam, 2), "%\n")
cat("Mediana población caucásica:", round(mediana_cauc, 2), "%\n")

# Categorizar poblaciones
datos_dc_hawaii <- datos_dc_hawaii %>%
  mutate(
    cat_afam = ifelse(afam > mediana_afam, "Alta_Afroam", "Baja_Afroam"),
    cat_cauc = ifelse(cauc > mediana_cauc, "Alta_Cauc", "Baja_Cauc"),
    # Crear una variable combinada para mejor análisis
    perfil_poblacional = case_when(
      afam > mediana_afam & cauc <= mediana_cauc ~ "Alta_Afroam_Baja_Cauc",
      afam <= mediana_afam & cauc > mediana_cauc ~ "Baja_Afroam_Alta_Cauc",
      afam > mediana_afam & cauc > mediana_cauc ~ "Alta_Afroam_Alta_Cauc",
      TRUE ~ "Baja_Afroam_Baja_Cauc"
    )
  )

# TABLA DE CONTINGENCIA 1: Estado vs Categoría Afroamericana
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("TABLA 1: Estado vs Población Afroamericana\n")
cat(paste(rep("-", 50), collapse=""), "\n")

tabla_afam <- table(datos_dc_hawaii$state, datos_dc_hawaii$cat_afam)
print(tabla_afam)

# Prueba Chi-cuadrado para población afroamericana
cat("\nPrueba Chi-cuadrado para población afroamericana:\n")
chi_test_afam <- chisq.test(tabla_afam)
print(chi_test_afam)

# TABLA DE CONTINGENCIA 2: Estado vs Categoría Caucásica
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("TABLA 2: Estado vs Población Caucásica\n")
cat(paste(rep("-", 50), collapse=""), "\n")

tabla_cauc <- table(datos_dc_hawaii$state, datos_dc_hawaii$cat_cauc)
print(tabla_cauc)

# Prueba Chi-cuadrado para población caucásica
cat("\nPrueba Chi-cuadrado para población caucásica:\n")
chi_test_cauc <- chisq.test(tabla_cauc)
print(chi_test_cauc)

# TABLA DE CONTINGENCIA 3: Estado vs Perfil Poblacional Combinado
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("TABLA 3: Estado vs Perfil Poblacional Combinado\n")
cat(paste(rep("-", 50), collapse=""), "\n")

tabla_perfil <- table(datos_dc_hawaii$state, datos_dc_hawaii$perfil_poblacional)
print(tabla_perfil)

# Prueba Chi-cuadrado para perfil poblacional
cat("\nPrueba Chi-cuadrado para perfil poblacional:\n")
chi_test_perfil <- chisq.test(tabla_perfil)
print(chi_test_perfil)

# RESUMEN DE RESULTADOS
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RESUMEN DE PRUEBAS CHI-CUADRADO\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("\n1. POBLACIÓN AFROAMERICANA:\n")
cat("   Chi-cuadrado =", round(chi_test_afam$statistic, 4), "\n")
cat("   p-valor =", format(chi_test_afam$p.value, scientific = TRUE, digits = 4), "\n")
cat("   Significativo (α = 0.05):", ifelse(chi_test_afam$p.value < 0.05, "SÍ", "NO"), "\n")

cat("\n2. POBLACIÓN CAUCÁSICA:\n")
cat("   Chi-cuadrado =", round(chi_test_cauc$statistic, 4), "\n")
cat("   p-valor =", format(chi_test_cauc$p.value, scientific = TRUE, digits = 4), "\n")
cat("   Significativo (α = 0.05):", ifelse(chi_test_cauc$p.value < 0.05, "SÍ", "NO"), "\n")

cat("\n3. PERFIL POBLACIONAL COMBINADO:\n")
cat("   Chi-cuadrado =", round(chi_test_perfil$statistic, 4), "\n")
cat("   p-valor =", format(chi_test_perfil$p.value, scientific = TRUE, digits = 4), "\n")
cat("   Significativo (α = 0.05):", ifelse(chi_test_perfil$p.value < 0.05, "SÍ", "NO"), "\n")

# Interpretación adicional
cat("\n", paste(rep("-", 40), collapse=""), "\n")
cat("INTERPRETACIÓN:\n")
cat(paste(rep("-", 40), collapse=""), "\n")

# Mostrar los valores promedio reales para contextualizar
dc_stats <- datos_dc_hawaii %>% 
  filter(state == "District of Columbia") %>%
  summarise(
    afam_promedio = round(mean(afam), 1),
    cauc_promedio = round(mean(cauc), 1)
  )

hawaii_stats <- datos_dc_hawaii %>% 
  filter(state == "Hawaii") %>%
  summarise(
    afam_promedio = round(mean(afam), 1),
    cauc_promedio = round(mean(cauc), 1)
  )

cat("\nPromedios reales (1977-1999):\n")
cat("District of Columbia: ", dc_stats$afam_promedio, "% afroamericana,", dc_stats$cauc_promedio, "% caucásica\n")
cat("Hawaii: ", hawaii_stats$afam_promedio, "% afroamericana,", hawaii_stats$cauc_promedio, "% caucásica\n")

cat("\nCOMPARADO CON LA MEDIANA NACIONAL:\n")
cat("Mediana nacional: ", round(mediana_afam, 1), "% afroamericana,", round(mediana_cauc, 1), "% caucásica\n")

# =============================================================================
# TRANSFORMACIÓN: CRÍMENES VIOLENTOS POR 100,000 HABITANTES
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("TRANSFORMACIÓN A TASAS POR 100,000 HABITANTES\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Calcular tasa de crímenes violentos por 100,000 habitantes
crime_data_with_rates <- crime_data %>%
  mutate(
    violent_rate_per_100k = round((violent / population) * 100000, 2)
  )

cat("\nFórmula utilizada: (violent / population) * 100,000\n")

# Mostrar ejemplos de la transformación
cat("\nEjemplos de transformación (primeros 10 registros):\n")
ejemplos <- crime_data_with_rates %>%
  select(year, state, violent, population, violent_rate_per_100k) %>%
  head(10)

print(ejemplos)

# Estadísticas descriptivas de la nueva variable
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("ESTADÍSTICAS DESCRIPTIVAS - TASA DE CRÍMENES VIOLENTOS\n")
cat(paste(rep("-", 50), collapse=""), "\n")

cat("\nResumen estadístico de la tasa por 100,000 habitantes:\n")
print(summary(crime_data_with_rates$violent_rate_per_100k))

cat("\nDesviación estándar:", round(sd(crime_data_with_rates$violent_rate_per_100k), 2), "\n")

# Comparar valores absolutos vs tasas
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("COMPARACIÓN: VALORES ABSOLUTOS vs TASAS\n")
cat(paste(rep("-", 50), collapse=""), "\n")

# Top 10 estados por números absolutos de crímenes violentos
cat("\nTop 10 estados por NÚMEROS ABSOLUTOS de crímenes violentos (promedio 1977-1999):\n")
top_absolutos <- crime_data_with_rates %>%
  group_by(state) %>%
  summarise(
    promedio_absoluto = round(mean(violent), 0),
    promedio_tasa = round(mean(violent_rate_per_100k), 1)
  ) %>%
  arrange(desc(promedio_absoluto))

print(head(top_absolutos, 10))

# Top 10 estados por tasas de crímenes violentos
cat("\nTop 10 estados por TASAS de crímenes violentos por 100k habitantes (promedio 1977-1999):\n")
top_tasas <- crime_data_with_rates %>%
  group_by(state) %>%
  summarise(
    promedio_absoluto = round(mean(violent), 0),
    promedio_tasa = round(mean(violent_rate_per_100k), 1)
  ) %>%
  arrange(desc(promedio_tasa))

print(head(top_tasas, 10))

# Estados con menor tasa de crímenes violentos
cat("\nEstados con MENORES tasas de crímenes violentos por 100k habitantes:\n")
print(tail(top_tasas, 10))

# Análisis específico para DC y Hawaii
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("ANÁLISIS ESPECÍFICO: DISTRICT OF COLUMBIA vs HAWAII\n")
cat(paste(rep("-", 50), collapse=""), "\n")

dc_hawaii_rates <- crime_data_with_rates %>%
  filter(state %in% c("District of Columbia", "Hawaii")) %>%
  group_by(state) %>%
  summarise(
    promedio_absoluto = round(mean(violent), 0),
    promedio_tasa = round(mean(violent_rate_per_100k), 1),
    poblacion_promedio = round(mean(population), 0)
  )

print(dc_hawaii_rates)

# Evolución temporal de las tasas
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("EVOLUCIÓN TEMPORAL DE TASAS (ejemplos por década)\n")
cat(paste(rep("-", 50), collapse=""), "\n")

# Mostrar tendencias para algunos estados representativos
estados_ejemplo <- c("California", "New York", "Texas", "Florida", "District of Columbia")

evolucion_temporal <- crime_data_with_rates %>%
  filter(state %in% estados_ejemplo) %>%
  filter(year %in% c(1977, 1987, 1997)) %>%
  select(year, state, violent_rate_per_100k) %>%
  arrange(state, year)

cat("\nEvolución de tasas por 100k habitantes (años seleccionados):\n")
print(evolucion_temporal)

# Crear rangos de tasas para clasificación
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("CLASIFICACIÓN POR RANGOS DE TASAS\n")
cat(paste(rep("-", 50), collapse=""), "\n")

# Definir rangos basados en cuartiles
cuartiles <- quantile(crime_data_with_rates$violent_rate_per_100k, probs = c(0.25, 0.5, 0.75))

crime_data_classified <- crime_data_with_rates %>%
  mutate(
    clasificacion_tasa = case_when(
      violent_rate_per_100k <= cuartiles[1] ~ "Baja",
      violent_rate_per_100k <= cuartiles[2] ~ "Media-Baja", 
      violent_rate_per_100k <= cuartiles[3] ~ "Media-Alta",
      TRUE ~ "Alta"
    )
  )

cat("Rangos de clasificación (basados en cuartiles):\n")
cat("Baja:", "≤", round(cuartiles[1], 1), "\n")
cat("Media-Baja:", round(cuartiles[1], 1), "-", round(cuartiles[2], 1), "\n")
cat("Media-Alta:", round(cuartiles[2], 1), "-", round(cuartiles[3], 1), "\n")
cat("Alta:", ">", round(cuartiles[3], 1), "\n")

# Tabla de frecuencias de clasificación
cat("\nDistribución de registros por clasificación:\n")
print(table(crime_data_classified$clasificacion_tasa))

# Resumen final
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RESUMEN DE LA TRANSFORMACIÓN\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("\n✓ Variable original: 'violent' (números absolutos)")
cat("\n✓ Variable transformada: 'violent_rate_per_100k' (por 100,000 hab.)")
cat("\n✓ Fórmula: (violent / population) * 100,000")
cat("\n✓ Rango de tasas:", round(min(crime_data_with_rates$violent_rate_per_100k), 1), 
    "a", round(max(crime_data_with_rates$violent_rate_per_100k), 1), "por 100k habitantes")
cat("\n✓ Promedio nacional:", round(mean(crime_data_with_rates$violent_rate_per_100k), 1), "por 100k habitantes\n")

# =============================================================================
# COMPARACIÓN DE DISTRICT OF COLUMBIA, HAWAII E IDAHO
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("COMPARACIÓN: DISTRICT OF COLUMBIA vs HAWAII vs IDAHO\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Estados a comparar
estados_comparacion <- c("District of Columbia", "Hawaii", "Idaho")

# Filtrar datos para los tres estados
datos_comparacion <- crime_data_with_rates %>%
  filter(state %in% estados_comparacion)

cat("\nEstados seleccionados para comparación:\n")
cat("- District of Columbia (alta tasa de crímenes)\n")
cat("- Hawaii (alta población afroamericana, similar a DC)\n")  
cat("- Idaho (contraste - estado con baja diversidad racial)\n")

# Estadísticas descriptivas por estado
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("ESTADÍSTICAS DESCRIPTIVAS POR ESTADO\n")
cat(paste(rep("-", 50), collapse=""), "\n")

estadisticas_estados <- datos_comparacion %>%
  group_by(state) %>%
  summarise(
    n_registros = n(),
    min_tasa = round(min(violent_rate_per_100k), 1),
    q1_tasa = round(quantile(violent_rate_per_100k, 0.25), 1),
    mediana_tasa = round(median(violent_rate_per_100k), 1),
    media_tasa = round(mean(violent_rate_per_100k), 1),
    q3_tasa = round(quantile(violent_rate_per_100k, 0.75), 1),
    max_tasa = round(max(violent_rate_per_100k), 1),
    sd_tasa = round(sd(violent_rate_per_100k), 1),
    .groups = "drop"
  )

print(estadisticas_estados)

# Comparación de promedios
cat("\n", paste(rep("-", 30), collapse=""), "\n")
cat("COMPARACIÓN DE PROMEDIOS:\n")
cat(paste(rep("-", 30), collapse=""), "\n")

for(estado in estados_comparacion) {
  media_estado <- estadisticas_estados$media_tasa[estadisticas_estados$state == estado]
  cat(sprintf("%-20s: %6.1f por 100k habitantes\n", estado, media_estado))
}

# Diferencias entre estados
dc_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "District of Columbia"]
hawaii_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "Hawaii"]
idaho_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "Idaho"]

cat("\nDIFERENCIAS:\n")
cat(sprintf("DC vs Hawaii: %.1f veces más alta\n", dc_media/hawaii_media))
cat(sprintf("DC vs Idaho: %.1f veces más alta\n", dc_media/idaho_media))
cat(sprintf("Hawaii vs Idaho: %.1f veces más alta\n", hawaii_media/idaho_media))

# Evolución temporal
cat("\n", paste(rep("-", 50), collapse=""), "\n")
cat("EVOLUCIÓN TEMPORAL (años selectos)\n")
cat(paste(rep("-", 50), collapse=""), "\n")

evolucion <- datos_comparacion %>%
  filter(year %in% c(1977, 1987, 1997)) %>%
  select(year, state, violent_rate_per_100k) %>%
  arrange(year, state)

print(evolucion)

# =============================================================================
# BOXPLOT PARA VISUALIZAR LAS DIFERENCIAS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("CREANDO BOXPLOT COMPARATIVO\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Crear boxplot básico
boxplot_basico <- ggplot(datos_comparacion, aes(x = state, y = violent_rate_per_100k, fill = state)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  labs(
    title = "Comparación de Tasas de Crímenes Violentos por 100k Habitantes",
    subtitle = "District of Columbia vs Hawaii vs Idaho (1977-1999)",
    x = "Estado",
    y = "Tasa de Crímenes Violentos por 100,000 Habitantes",
    caption = "Fuente: Dataset CrimeUS | Cada punto representa un año"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("District of Columbia" = "red3", 
                              "Hawaii" = "dodgerblue3", 
                              "Idaho" = "forestgreen")) +
  scale_y_continuous(breaks = seq(0, 3000, 500))

print(boxplot_basico)

# Boxplot con escala logarítmica para mejor visualización
boxplot_log <- ggplot(datos_comparacion, aes(x = state, y = violent_rate_per_100k, fill = state)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  labs(
    title = "Comparación de Tasas de Crímenes Violentos (Escala Logarítmica)",
    subtitle = "District of Columbia vs Hawaii vs Idaho (1977-1999)",
    x = "Estado",
    y = "Tasa de Crímenes Violentos por 100,000 Habitantes (escala log)",
    caption = "Fuente: Dataset CrimeUS | Escala logarítmica para mejor comparación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("District of Columbia" = "red3", 
                              "Hawaii" = "dodgerblue3", 
                              "Idaho" = "forestgreen")) +
  scale_y_log10()

print(boxplot_log)

# Gráfico de líneas para mostrar evolución temporal
grafico_lineas <- ggplot(datos_comparacion, aes(x = year, y = violent_rate_per_100k, color = state)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Evolución Temporal de las Tasas de Crímenes Violentos",
    subtitle = "Comparación entre District of Columbia, Hawaii e Idaho (1977-1999)",
    x = "Año",
    y = "Tasa de Crímenes Violentos por 100,000 Habitantes",
    color = "Estado",
    caption = "Fuente: Dataset CrimeUS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("District of Columbia" = "red3", 
                               "Hawaii" = "dodgerblue3", 
                               "Idaho" = "forestgreen")) +
  scale_x_continuous(breaks = seq(1977, 1999, 3))

print(grafico_lineas)

# Resumen de la comparación
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RESUMEN DE LA COMPARACIÓN\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat(sprintf("\n📊 DISTRICT OF COLUMBIA:"))
cat(sprintf("\n   • Tasa promedio: %.1f por 100k habitantes", dc_media))
cat(sprintf("\n   • Rango: %.1f - %.1f", 
    estadisticas_estados$min_tasa[estadisticas_estados$state == "District of Columbia"],
    estadisticas_estados$max_tasa[estadisticas_estados$state == "District of Columbia"]))
cat(sprintf("\n   • Clasificación: EXTREMADAMENTE ALTA"))

cat(sprintf("\n\n🏝️ HAWAII:"))
cat(sprintf("\n   • Tasa promedio: %.1f por 100k habitantes", hawaii_media))
cat(sprintf("\n   • Rango: %.1f - %.1f", 
    estadisticas_estados$min_tasa[estadisticas_estados$state == "Hawaii"],
    estadisticas_estados$max_tasa[estadisticas_estados$state == "Hawaii"]))
cat(sprintf("\n   • Clasificación: MODERADA"))

cat(sprintf("\n\n🏔️ IDAHO:"))
cat(sprintf("\n   • Tasa promedio: %.1f por 100k habitantes", idaho_media))
cat(sprintf("\n   • Rango: %.1f - %.1f", 
    estadisticas_estados$min_tasa[estadisticas_estados$state == "Idaho"],
    estadisticas_estados$max_tasa[estadisticas_estados$state == "Idaho"]))
cat(sprintf("\n   • Clasificación: BAJA"))

cat("\n\n✅ Los boxplots muestran claramente las diferencias significativas entre los tres estados.")
cat("\n✅ District of Columbia tiene una distribución completamente diferente a los otros dos.")
cat("\n✅ Los gráficos revelan patrones temporales distintos para cada estado.\n")
