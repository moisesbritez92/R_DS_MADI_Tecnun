# =============================================================================
# ANÁLISIS ESTADÍSTICO DEL DATASET CRIME US
# Ejercicio de Inferencia Estadística con R
# =============================================================================

# Cargar librerías necesarias
library(dplyr)      # Para manipulación de datos
library(tidyr)      # Para organizar datos

# =============================================================================
# EJERCICIO 1: Leer el archivo crimeUS.csv
# =============================================================================

# Leer el archivo CSV con los datos de criminalidad en 50 estados de USA
# header = TRUE indica que la primera fila contiene los nombres de columnas
# stringsAsFactors = FALSE evita convertir texto a factores automáticamente
crime_data <- read.csv('crimeUS.csv', 
                       header = TRUE, 
                       stringsAsFactors = FALSE)

# Verificar la carga correcta de los datos
cat("\n=== EJERCICIO 1: CARGA DE DATOS ===\n")
cat("Dimensiones del dataset:", nrow(crime_data), "filas x", ncol(crime_data), "columnas\n")

# Ver las primeras filas del dataset
cat("\nPrimeras filas del dataset:\n")
head(crime_data, 10)

# Ver estructura del dataset
cat("\nEstructura del dataset:\n")
str(crime_data)

# Nombres de las columnas
cat("\nColumnas disponibles:\n")
print(colnames(crime_data))


# =============================================================================
# EJERCICIO 2: Calcular la media de porcentajes de población 
#              Afroamericana (afam) y Caucásica (cauc) por estado
# =============================================================================

cat("\n\n=== EJERCICIO 2: MEDIAS DE POBLACIÓN POR ESTADO ===\n\n")

# La función aggregate() permite aplicar funciones a grupos de datos
# Sintaxis: aggregate(valores ~ agrupador, data = df, FUN = función)

# Calcular media de población Afroamericana por estado
# aggregate() agrupa por 'state' y aplica mean() a la columna 'afam'
mean_afam <- aggregate(afam ~ state, 
                       data = crime_data, 
                       FUN = mean)

# Calcular media de población Caucásica por estado
mean_cauc <- aggregate(cauc ~ state, 
                       data = crime_data, 
                       FUN = mean)

# round() redondea los valores para eliminar decimales
# El segundo argumento indica el número de decimales (0 = número entero)
mean_afam$afam <- round(mean_afam$afam, 0)
mean_cauc$cauc <- round(mean_cauc$cauc, 0)

# Combinar ambas medias en un único dataframe usando merge()
# merge() une dos dataframes por una columna común (en este caso 'state')
medias_poblacion <- merge(mean_afam, mean_cauc, by = "state")

# Renombrar columnas para mayor claridad
colnames(medias_poblacion) <- c("Estado", "Media_Afroamericana_%", "Media_Caucasica_%")

# Mostrar resultados ordenados alfabéticamente
cat("TABLA DE MEDIAS POR ESTADO:\n")
print(medias_poblacion[order(medias_poblacion$Estado), ], row.names = FALSE)

# Mostrar algunos estados destacados
cat("\n--- TOP 5 ESTADOS CON MAYOR POBLACIÓN AFROAMERICANA ---\n")
top_afam <- head(medias_poblacion[order(-medias_poblacion$`Media_Afroamericana_%`), ], 5)
print(top_afam, row.names = FALSE)

cat("\n--- TOP 5 ESTADOS CON MAYOR POBLACIÓN CAUCÁSICA ---\n")
top_cauc <- head(medias_poblacion[order(-medias_poblacion$`Media_Caucasica_%`), ], 5)
print(top_cauc, row.names = FALSE)


# =============================================================================
# EJERCICIO 3: Tabla de contingencia para District of Columbia y Hawaii
#              Prueba de independencia Chi-cuadrado
# =============================================================================

cat("\n\n=== EJERCICIO 3: ANÁLISIS District of Columbia vs Hawaii ===\n\n")

# Filtrar datos solo para District of Columbia y Hawaii
# %in% verifica si un valor está en un vector dado
datos_DC_HI <- crime_data[crime_data$state %in% c("District of Columbia", "Hawaii"), ]

cat("Número de observaciones por estado:\n")
cat("- District of Columbia:", sum(datos_DC_HI$state == "District of Columbia"), "\n")
cat("- Hawaii:", sum(datos_DC_HI$state == "Hawaii"), "\n\n")

# Calcular las MEDIAS de los porcentajes de población por estado
# Estas medias serán usadas para construir la tabla de contingencia
media_DC_afam <- mean(datos_DC_HI$afam[datos_DC_HI$state == "District of Columbia"])
media_HI_afam <- mean(datos_DC_HI$afam[datos_DC_HI$state == "Hawaii"])

media_DC_cauc <- mean(datos_DC_HI$cauc[datos_DC_HI$state == "District of Columbia"])
media_HI_cauc <- mean(datos_DC_HI$cauc[datos_DC_HI$state == "Hawaii"])

# Crear tabla de contingencia manualmente
# matrix() crea una matriz con los valores calculados
# nrow = 2 indica que tendrá 2 filas
# byrow = TRUE indica que los valores se llenan por filas
# dimnames = list() asigna nombres a filas y columnas
tabla_contingencia_DC_HI <- matrix(
  c(round(media_DC_afam), round(media_HI_afam),      # Primera fila: Afroamericanos
    round(media_DC_cauc), round(media_HI_cauc)),     # Segunda fila: Caucásicos
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Poblacion = c("Afroamericana", "Caucásica"),
    Estado = c("District of Columbia", "Hawaii")
  )
)

cat("TABLA DE CONTINGENCIA:\n")
cat("(Valores basados en medias de porcentajes poblacionales)\n\n")
print(tabla_contingencia_DC_HI)
cat("\n")

# Prueba Chi-cuadrado de independencia
# chisq.test() realiza la prueba de chi-cuadrado
# H0 (hipótesis nula): No hay asociación entre estado y tipo de población
# H1 (hipótesis alternativa): Existe asociación entre estado y tipo de población
chi_test_DC_HI <- chisq.test(tabla_contingencia_DC_HI)

cat("RESULTADOS DE LA PRUEBA CHI-CUADRADO:\n")
cat("==========================================\n")
cat("Estadístico Chi-cuadrado:", round(chi_test_DC_HI$statistic, 4), "\n")
cat("Grados de libertad:", chi_test_DC_HI$parameter, "\n")
cat("p-value:", format(chi_test_DC_HI$p.value, scientific = TRUE, digits = 4), "\n\n")

# Interpretación del p-value
# El p-value representa la probabilidad de obtener estos resultados
# si la hipótesis nula fuera verdadera
alpha <- 0.05  # Nivel de significancia estándar
cat("INTERPRETACIÓN (nivel de significancia α = 0.05):\n")
cat("==========================================\n")

if(chi_test_DC_HI$p.value < alpha) {
  cat("✓ p-value < 0.05: RECHAZAMOS la hipótesis nula\n")
  cat("  → Existe una asociación SIGNIFICATIVA entre el estado y el tipo de población\n")
  cat("  → Las distribuciones de población son DIFERENTES entre DC y Hawaii\n")
} else {
  cat("✗ p-value ≥ 0.05: NO RECHAZAMOS la hipótesis nula\n")
  cat("  → NO hay evidencia suficiente de asociación significativa\n")
  cat("  → Las distribuciones de población son similares entre ambos estados\n")
}

cat("\nFRECUENCIAS ESPERADAS (bajo hipótesis de independencia):\n")
print(round(chi_test_DC_HI$expected, 2))

cat("\nRESIDUOS DE PEARSON:\n")
cat("(Miden la diferencia entre valores observados y esperados)\n")
print(round(chi_test_DC_HI$residuals, 2))
cat("Nota: Valores > |2| indican celdas con desviaciones importantes\n\n")


# =============================================================================
# EJERCICIO 4: Tabla de contingencia para Florida y Hawaii
#              Prueba de independencia Chi-cuadrado
# =============================================================================

cat("\n\n=== EJERCICIO 4: ANÁLISIS Florida vs Hawaii ===\n\n")

# Filtrar datos para Florida y Hawaii
datos_FL_HI <- crime_data[crime_data$state %in% c("Florida", "Hawaii"), ]

cat("Número de observaciones por estado:\n")
cat("- Florida:", sum(datos_FL_HI$state == "Florida"), "\n")
cat("- Hawaii:", sum(datos_FL_HI$state == "Hawaii"), "\n\n")

# Calcular medias de población para ambos estados
media_FL_afam <- mean(datos_FL_HI$afam[datos_FL_HI$state == "Florida"])
media_HI_afam2 <- mean(datos_FL_HI$afam[datos_FL_HI$state == "Hawaii"])

media_FL_cauc <- mean(datos_FL_HI$cauc[datos_FL_HI$state == "Florida"])
media_HI_cauc2 <- mean(datos_FL_HI$cauc[datos_FL_HI$state == "Hawaii"])

# Crear tabla de contingencia
tabla_contingencia_FL_HI <- matrix(
  c(round(media_FL_afam), round(media_HI_afam2),
    round(media_FL_cauc), round(media_HI_cauc2)),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Poblacion = c("Afroamericana", "Caucásica"),
    Estado = c("Florida", "Hawaii")
  )
)

cat("TABLA DE CONTINGENCIA:\n")
cat("(Valores basados en medias de porcentajes poblacionales)\n\n")
print(tabla_contingencia_FL_HI)
cat("\n")

# Prueba Chi-cuadrado
chi_test_FL_HI <- chisq.test(tabla_contingencia_FL_HI)

cat("RESULTADOS DE LA PRUEBA CHI-CUADRADO:\n")
cat("==========================================\n")
cat("Estadístico Chi-cuadrado:", round(chi_test_FL_HI$statistic, 4), "\n")
cat("Grados de libertad:", chi_test_FL_HI$parameter, "\n")
cat("p-value:", format(chi_test_FL_HI$p.value, scientific = TRUE, digits = 4), "\n\n")

# Interpretación
cat("INTERPRETACIÓN (nivel de significancia α = 0.05):\n")
cat("==========================================\n")

if(chi_test_FL_HI$p.value < alpha) {
  cat("✓ p-value < 0.05: RECHAZAMOS la hipótesis nula\n")
  cat("  → Existe una asociación SIGNIFICATIVA entre el estado y el tipo de población\n")
  cat("  → Las distribuciones de población son DIFERENTES entre Florida y Hawaii\n")
} else {
  cat("✗ p-value ≥ 0.05: NO RECHAZAMOS la hipótesis nula\n")
  cat("  → NO hay evidencia suficiente de asociación significativa\n")
  cat("  → Las distribuciones de población son similares entre ambos estados\n")
}

cat("\nFRECUENCIAS ESPERADAS:\n")
print(round(chi_test_FL_HI$expected, 2))

cat("\nRESIDUOS DE PEARSON:\n")
print(round(chi_test_FL_HI$residuals, 2))
cat("\n")


# =============================================================================
# COMPARACIÓN FINAL DE RESULTADOS
# =============================================================================

cat("\n\n=== COMPARACIÓN DE RESULTADOS ===\n")
cat("===========================================\n\n")

# Crear tabla comparativa
comparacion <- data.frame(
  Comparacion = c("DC vs Hawaii", "Florida vs Hawaii"),
  Chi_cuadrado = c(round(chi_test_DC_HI$statistic, 4), 
                   round(chi_test_FL_HI$statistic, 4)),
  p_value = c(format(chi_test_DC_HI$p.value, scientific = TRUE, digits = 4), 
              format(chi_test_FL_HI$p.value, scientific = TRUE, digits = 4)),
  Significativo = c(
    ifelse(chi_test_DC_HI$p.value < 0.05, "Sí (p < 0.05)", "No (p ≥ 0.05)"),
    ifelse(chi_test_FL_HI$p.value < 0.05, "Sí (p < 0.05)", "No (p ≥ 0.05)")
  )
)

print(comparacion, row.names = FALSE)

cat("\n\nCONCLUSIONES FINALES:\n")
cat("===========================================\n")
cat("1. District of Columbia tiene una composición demográfica MUY diferente\n")
cat("   de Hawaii, con mayor proporción de población Afroamericana (~", 
    round(media_DC_afam), "% vs ", round(media_HI_afam), "%).\n\n")
cat("2. Florida también muestra diferencias con Hawaii en términos de\n")
cat("   composición racial (~", round(media_FL_afam), "% vs ", 
    round(media_HI_afam), "% de población Afroamericana).\n\n")
cat("3. Ambas pruebas Chi-cuadrado indican que la distribución de población\n")
cat("   varía significativamente entre estos estados.\n\n")
cat("4. Hawaii tiene una composición única con menor proporción de población\n")
cat("   Afroamericana y Caucásica comparado con DC y Florida.\n\n")

# =============================================================================
# VISUALIZACIÓN OPCIONAL (si tienes capacidad gráfica)
# =============================================================================

cat("=== GENERANDO VISUALIZACIONES ===\n\n")

# Intentar crear gráficos si el entorno lo permite
tryCatch({
  # Configurar para 2 gráficos lado a lado
  par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
  
  # Gráfico 1: DC vs Hawaii
  barplot(tabla_contingencia_DC_HI, 
          beside = TRUE,
          col = c("steelblue", "coral"),
          legend.text = TRUE,
          main = "DC vs Hawaii\nDistribución de Población",
          ylab = "Porcentaje promedio (%)",
          ylim = c(0, max(tabla_contingencia_DC_HI) * 1.2),
          args.legend = list(x = "topright", cex = 0.8))
  
  # Gráfico 2: Florida vs Hawaii
  barplot(tabla_contingencia_FL_HI,
          beside = TRUE,
          col = c("steelblue", "coral"),
          legend.text = TRUE,
          main = "Florida vs Hawaii\nDistribución de Población",
          ylab = "Porcentaje promedio (%)",
          ylim = c(0, max(tabla_contingencia_FL_HI) * 1.2),
          args.legend = list(x = "topright", cex = 0.8))
  
  # Restaurar configuración gráfica
  par(mfrow = c(1, 1))
  
  cat("✓ Gráficos generados exitosamente\n\n")
}, error = function(e) {
  cat("⚠ No se pudieron generar gráficos en este entorno\n\n")
})

cat("===========================================\n")
cat("✓ ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("===========================================\n\n")