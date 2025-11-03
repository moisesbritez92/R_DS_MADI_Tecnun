# =============================================================================
# ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# Ejercicio: Detección de patrones en datos de sensores
# =============================================================================

# 1. CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(tidyverse)
library(ggplot2)

# Crear carpeta output si no existe
if (!dir.exists("output")) {
  dir.create("output")
}

# 2. LEER EL ARCHIVO DE DATOS
# =============================================================================
# Leer el archivo componentes.csv
datos <- read.csv("componentes.csv", header = TRUE)

# Explorar la estructura de los datos
cat("=== ESTRUCTURA DE LOS DATOS ===\n")
cat("Dimensiones:", dim(datos), "\n")
cat("Primeras filas:\n")
print(head(datos))
cat("\nTipos de comportamiento:\n")
print(table(datos$type))

# 3. PREPARAR DATOS PARA PCA
# =============================================================================
# Separar las variables predictoras (X1-X14) de la variable tipo
variables_sensores <- datos %>% 
  select(X1:X14)  # Seleccionar solo las columnas de sensores

tipo_comportamiento <- datos$type  # Guardar la variable tipo

# Verificar datos
cat("\n=== DATOS DE SENSORES ===\n")
cat("Dimensiones de variables sensores:", dim(variables_sensores), "\n")
cat("Resumen estadístico:\n")
print(summary(variables_sensores))

# 4. CALCULAR COMPONENTES PRINCIPALES
# =============================================================================
cat("\n=== ANÁLISIS DE COMPONENTES PRINCIPALES ===\n")

# Realizar PCA (centrado y escalado de datos)
pca_resultado <- prcomp(variables_sensores, 
                        center = TRUE,   # Centrar los datos (restar media)
                        scale. = TRUE)   # Escalar los datos (dividir por SD)

# Ver resumen del PCA
cat("\nResumen del PCA:\n")
print(summary(pca_resultado))

# 5. CALCULAR LA CONTRIBUCIÓN (ENERGÍA) DE LOS PRIMEROS 2 COMPONENTES
# =============================================================================
# Obtener la varianza explicada por cada componente
varianza_explicada <- pca_resultado$sdev^2
proporcion_varianza <- varianza_explicada / sum(varianza_explicada)
proporcion_acumulada <- cumsum(proporcion_varianza)

# Crear tabla de contribución
tabla_varianza <- data.frame(
  Componente = paste0("PC", 1:length(proporcion_varianza)),
  Varianza = varianza_explicada,
  Proporcion = proporcion_varianza * 100,
  Proporcion_Acumulada = proporcion_acumulada * 100
)

cat("\n=== CONTRIBUCIÓN DE COMPONENTES PRINCIPALES ===\n")
print(tabla_varianza[1:5, ], row.names = FALSE)

# Contribución de los primeros 2 componentes
energia_pc1 <- proporcion_varianza[1] * 100
energia_pc2 <- proporcion_varianza[2] * 100
energia_total_2pc <- proporcion_acumulada[2] * 100

cat("\n=== ENERGÍA DE LOS PRIMEROS 2 COMPONENTES ===\n")
cat(sprintf("PC1 explica: %.2f%% de la varianza\n", energia_pc1))
cat(sprintf("PC2 explica: %.2f%% de la varianza\n", energia_pc2))
cat(sprintf("PC1 + PC2 explican: %.2f%% de la varianza total\n", energia_total_2pc))

# 6. OBTENER DATOS ROTADOS (SCORES)
# =============================================================================
# Extraer las coordenadas en el espacio de componentes principales
datos_rotados <- as.data.frame(pca_resultado$x)

# Agregar la variable tipo
datos_rotados$type <- tipo_comportamiento

cat("\n=== DATOS ROTADOS (primeras filas) ===\n")
print(head(datos_rotados[, c("PC1", "PC2", "type")]))

# 7. VISUALIZACIÓN: GRÁFICO DE LOS PRIMEROS 2 COMPONENTES
# =============================================================================
cat("\n=== GENERANDO VISUALIZACIÓN ===\n")

# Crear gráfico con colores según el tipo de comportamiento
plot_pca <- ggplot(datos_rotados, aes(x = PC1, y = PC2, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  
  # Etiquetas y título
  labs(
    title = "Análisis de Componentes Principales (PCA)",
    subtitle = sprintf("PC1: %.2f%% | PC2: %.2f%% | Total: %.2f%% de varianza explicada",
                      energia_pc1, energia_pc2, energia_total_2pc),
    x = sprintf("Componente Principal 1 (%.2f%%)", energia_pc1),
    y = sprintf("Componente Principal 2 (%.2f%%)", energia_pc2),
    color = "Tipo de\nComportamiento"
  ) +
  
  # Colores personalizados
  scale_color_manual(values = c("Normal" = "#2E7D32",    # Verde
                                "Error1" = "#D32F2F",    # Rojo
                                "Error2" = "#1976D2")) + # Azul
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray70", fill = NA, size = 1)
  ) +
  
  # Agregar líneas de referencia en el origen
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

# Mostrar el gráfico
print(plot_pca)

# 8. GUARDAR LA FIGURA EN LA CARPETA OUTPUT
# =============================================================================
# Guardar en alta resolución
ggsave(
  filename = "output/PCA_analisis_sensores.png",
  plot = plot_pca,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in"
)

cat("\n=== FIGURA GUARDADA ===\n")
cat("Archivo guardado en: output/PCA_analisis_sensores.png\n")

# 9. ANÁLISIS DE POSIBLES DATOS MAL ETIQUETADOS
# =============================================================================
cat("\n=== ANÁLISIS DE POSIBLES DATOS MAL ETIQUETADOS ===\n")

# Identificar puntos que podrían estar mal etiquetados
# Basándonos en la distancia a los centroides de cada grupo

# Calcular centroides por grupo
centroides <- datos_rotados %>%
  group_by(type) %>%
  summarise(
    centro_PC1 = mean(PC1),
    centro_PC2 = mean(PC2),
    .groups = 'drop'
  )

cat("\nCentroides de cada grupo:\n")
print(centroides)

# Función para calcular distancia euclidiana
calcular_distancia <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Para cada punto, calcular distancia a su propio centroide
datos_con_distancias <- datos_rotados %>%
  left_join(centroides, by = "type", suffix = c("", "_centroide")) %>%
  mutate(
    distancia_propio_centroide = calcular_distancia(PC1, PC2, 
                                                     centro_PC1, centro_PC2)
  )

# Identificar outliers (puntos muy alejados de su grupo)
# Usar criterio de 2 desviaciones estándar
outliers_por_grupo <- datos_con_distancias %>%
  group_by(type) %>%
  mutate(
    media_dist = mean(distancia_propio_centroide),
    sd_dist = sd(distancia_propio_centroide),
    es_outlier = distancia_propio_centroide > (media_dist + 2 * sd_dist)
  ) %>%
  filter(es_outlier) %>%
  select(type, PC1, PC2, distancia_propio_centroide)

if (nrow(outliers_por_grupo) > 0) {
  cat("\nPosibles datos mal etiquetados (outliers):\n")
  print(outliers_por_grupo)
  cat(sprintf("\nTotal de posibles datos mal etiquetados: %d\n", nrow(outliers_por_grupo)))
} else {
  cat("\nNo se detectaron outliers evidentes.\n")
}

# 10. GRÁFICO ADICIONAL: SCREE PLOT
# =============================================================================
# Crear scree plot para ver la importancia de cada componente
scree_data <- data.frame(
  Componente = 1:length(proporcion_varianza),
  Varianza_Explicada = proporcion_varianza * 100
)

plot_scree <- ggplot(scree_data[1:10, ], aes(x = Componente, y = Varianza_Explicada)) +
  geom_line(color = "#1976D2", size = 1) +
  geom_point(color = "#1976D2", size = 3) +
  labs(
    title = "Scree Plot - Varianza Explicada por Componente",
    x = "Componente Principal",
    y = "Varianza Explicada (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold")
  ) +
  scale_x_continuous(breaks = 1:10)

# Guardar scree plot
ggsave(
  filename = "output/PCA_scree_plot.png",
  plot = plot_scree,
  width = 8,
  height = 6,
  dpi = 300,
  units = "in"
)

cat("\nScree plot guardado en: output/PCA_scree_plot.png\n")

# 11. RESUMEN FINAL
# =============================================================================
cat("\n=== RESUMEN FINAL ===\n")
cat("1. Se analizaron", nrow(datos), "observaciones con", ncol(variables_sensores), "sensores\n")
cat("2. Los primeros 2 componentes explican el", round(energia_total_2pc, 2), "% de la varianza\n")
cat("3. Se identificaron", length(unique(tipo_comportamiento)), "tipos de comportamiento\n")
cat("4. Las figuras se guardaron en la carpeta 'output/'\n")
cat("\n=== ANÁLISIS COMPLETADO ===\n")