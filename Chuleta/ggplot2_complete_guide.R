# ==============================================================================
# GUÍA COMPLETA DE GGPLOT2 EN R
# Autor: Análisis de Datos Avanzado
# Fecha: 14 de octubre de 2025
# 
# Esta guía cubre todos los aspectos esenciales de ggplot2 con ejemplos
# prácticos y comentarios detallados para facilitar el aprendizaje.
# ==============================================================================

# ÍNDICE DE CONTENIDOS:
# 1. CONFIGURACIÓN INICIAL Y LIBRERÍAS
# 2. FUNDAMENTOS DE GGPLOT2 - GRAMÁTICA DE GRÁFICOS
# 3. GEOMETRÍAS (GEOMS) - TIPOS DE GRÁFICOS
# 4. ESTÉTICA (AESTHETICS) - MAPEO DE VARIABLES
# 5. FACETAS (FACETING) - MÚLTIPLES GRÁFICOS
# 6. ESCALAS (SCALES) - CONTROL DE EJES Y COLORES
# 7. TEMAS (THEMES) - PERSONALIZACIÓN DE APARIENCIA
# 8. ANOTACIONES Y ETIQUETAS
# 9. TRANSFORMACIONES ESTADÍSTICAS
# 10. COORDENADAS Y PROYECCIONES
# 11. EJEMPLOS AVANZADOS Y CASOS DE USO
# 12. BUENAS PRÁCTICAS Y OPTIMIZACIÓN

# ==============================================================================
# 1. CONFIGURACIÓN INICIAL Y LIBRERÍAS
# ==============================================================================

# Limpiar entorno de trabajo
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)    # Librería principal para gráficos
library(dplyr)      # Para manipulación de datos
library(tidyr)      # Para reestructuración de datos
library(scales)     # Para formateo de escalas
library(viridis)    # Paletas de colores científicas
library(RColorBrewer) # Paletas de colores adicionales
library(gridExtra)  # Para combinar múltiples gráficos
library(ggthemes)   # Temas adicionales para ggplot2
library(lubridate)  # Para manejo de fechas

# Configurar opciones globales
options(scipen = 999)  # Evitar notación científica
theme_set(theme_minimal())  # Establecer tema por defecto

# Datos de ejemplo que utilizaremos
data("mtcars")
data("mpg")
data("diamonds")
data("economics")
data("iris")

# Verificar estructura de los datos principales
cat("=== ESTRUCTURA DE DATOS DE EJEMPLO ===\n")
cat("\nDataset mtcars:\n")
str(mtcars)
cat("\nDataset mpg:\n")
str(mpg)
cat("\nDataset diamonds:\n")
str(diamonds)

# ==============================================================================
# 2. FUNDAMENTOS DE GGPLOT2 - GRAMÁTICA DE GRÁFICOS
# ==============================================================================

cat("\n=== FUNDAMENTOS DE GGPLOT2 ===\n")

# ggplot2 se basa en la "Gramática de Gráficos" de Leland Wilkinson
# Componentes principales:
# - Data: Los datos a graficar
# - Aesthetics (aes): Mapeo de variables a propiedades visuales
# - Geometries (geom): Tipo de gráfico (puntos, líneas, barras, etc.)
# - Statistics (stat): Transformaciones estadísticas
# - Scales: Mapeo de valores de datos a propiedades visuales
# - Coordinate system: Sistema de coordenadas
# - Faceting: Dividir datos en subconjuntos

# SINTAXIS BÁSICA:
# ggplot(data = <DATA>) + 
#   geom_<GEOM_FUNCTION>(aes(<MAPPINGS>)) +
#   <OTHER_COMPONENTS>

# EJEMPLO BÁSICO: Gráfico de dispersión
cat("\n--- Gráfico básico de dispersión ---\n")
basic_scatter <- ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy))
print(basic_scatter)

# COMPONENTES EXPLICADOS:
# - ggplot(data = mpg): Inicializa el gráfico con el dataset mpg
# - geom_point(): Añade puntos al gráfico
# - aes(x = displ, y = hwy): Mapea 'displ' al eje x y 'hwy' al eje y

# CONSTRUCCIÓN POR CAPAS
cat("\n--- Construcción por capas ---\n")
layered_plot <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +                    # Capa 1: Puntos
  geom_smooth(method = "lm") +      # Capa 2: Línea de regresión
  labs(title = "Eficiencia vs Desplazamiento",
       x = "Desplazamiento del motor (L)",
       y = "Millas por galón en carretera") +
  theme_minimal()                   # Tema limpio

print(layered_plot)

# ==============================================================================
# 3. GEOMETRÍAS (GEOMS) - TIPOS DE GRÁFICOS
# ==============================================================================

cat("\n=== GEOMETRÍAS DISPONIBLES EN GGPLOT2 ===\n")

# 3.1 GRÁFICOS DE PUNTOS (SCATTER PLOTS)
cat("\n--- 3.1 Gráficos de Puntos ---\n")

# Gráfico de dispersión básico
scatter_basic <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  ggtitle("Gráfico de Dispersión Básico")

# Gráfico de dispersión con color por variable categórica
scatter_color <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.7) +
  ggtitle("Dispersión por Clase de Vehículo")

# Gráfico de dispersión con tamaño variable
scatter_size <- ggplot(mpg, aes(x = displ, y = hwy, size = cyl, color = class)) +
  geom_point(alpha = 0.6) +
  ggtitle("Dispersión con Tamaño por Cilindros")

print(scatter_basic)
print(scatter_color)
print(scatter_size)

# 3.2 GRÁFICOS DE LÍNEAS
cat("\n--- 3.2 Gráficos de Líneas ---\n")

# Preparar datos de series temporales
economics_subset <- economics %>%
  filter(date >= as.Date("2000-01-01"))

# Gráfico de línea simple
line_basic <- ggplot(economics_subset, aes(x = date, y = unemploy)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Desempleo en el Tiempo",
       x = "Año", 
       y = "Número de Desempleados (miles)")

# Gráfico de múltiples líneas
economics_long <- economics_subset %>%
  select(date, unemploy, psavert) %>%
  pivot_longer(cols = c(unemploy, psavert), 
               names_to = "variable", 
               values_to = "value")

line_multiple <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Múltiples Variables Económicas",
       x = "Año", y = "Valor")

print(line_basic)
print(line_multiple)

# 3.3 GRÁFICOS DE BARRAS
cat("\n--- 3.3 Gráficos de Barras ---\n")

# Contar frecuencias por clase
mpg_counts <- mpg %>% count(class)

# Gráfico de barras básico
bar_basic <- ggplot(mpg_counts, aes(x = class, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(title = "Frecuencia por Clase de Vehículo",
       x = "Clase", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras agrupadas
mpg_grouped <- mpg %>% 
  count(class, drv) %>%
  filter(!is.na(drv))

bar_grouped <- ggplot(mpg_grouped, aes(x = class, y = n, fill = drv)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Vehículos por Clase y Tracción",
       x = "Clase", y = "Frecuencia", fill = "Tracción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras apiladas
bar_stacked <- ggplot(mpg_grouped, aes(x = class, y = n, fill = drv)) +
  geom_col(position = "stack", alpha = 0.8) +
  labs(title = "Vehículos por Clase (Apilado)",
       x = "Clase", y = "Frecuencia", fill = "Tracción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(bar_basic)
print(bar_grouped)
print(bar_stacked)

# 3.4 HISTOGRAMAS Y DENSIDAD
cat("\n--- 3.4 Histogramas y Densidad ---\n")

# Histograma básico
hist_basic <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Eficiencia en Carretera",
       x = "Millas por Galón", y = "Frecuencia")

# Histograma con múltiples grupos
hist_groups <- ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
  labs(title = "Eficiencia por Tipo de Tracción",
       x = "Millas por Galón", y = "Frecuencia", fill = "Tracción")

# Gráfico de densidad
density_plot <- ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidad de Eficiencia por Tracción",
       x = "Millas por Galón", y = "Densidad", fill = "Tracción")

print(hist_basic)
print(hist_groups)
print(density_plot)

# 3.5 BOXPLOTS Y VIOLIN PLOTS
cat("\n--- 3.5 Boxplots y Violin Plots ---\n")

# Boxplot básico
box_basic <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Eficiencia por Clase de Vehículo",
       x = "Clase", y = "Millas por Galón") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot con puntos superpuestos
box_jitter <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "red") +
  labs(title = "Boxplot con Puntos Individuales",
       x = "Clase", y = "Millas por Galón") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot
violin_plot <- ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
  labs(title = "Distribución de Eficiencia (Violin Plot)",
       x = "Clase", y = "Millas por Galón") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(box_basic)
print(box_jitter)
print(violin_plot)

# 3.6 MAPAS DE CALOR (HEATMAPS)
cat("\n--- 3.6 Mapas de Calor ---\n")

# Preparar datos para heatmap
mtcars_corr <- cor(mtcars)
mtcars_melt <- reshape2::melt(mtcars_corr)

# Heatmap de correlaciones
heatmap_corr <- ggplot(mtcars_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, name = "Correlación") +
  labs(title = "Matriz de Correlación - mtcars",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_corr)

# ==============================================================================
# 4. ESTÉTICA (AESTHETICS) - MAPEO DE VARIABLES
# ==============================================================================

cat("\n=== ESTÉTICA Y MAPEO DE VARIABLES ===\n")

# Las estéticas (aesthetics) mapean variables de los datos a propiedades visuales
# Principales aesthetics:
# - x, y: posición en los ejes
# - color: color del borde o línea
# - fill: color de relleno
# - size: tamaño
# - shape: forma (para puntos)
# - alpha: transparencia
# - linetype: tipo de línea

# 4.1 MAPEO DE COLOR
cat("\n--- 4.1 Mapeo de Color ---\n")

# Color por variable continua
color_continuous <- ggplot(mpg, aes(x = displ, y = hwy, color = cty)) +
  geom_point(size = 3) +
  scale_color_viridis_c(name = "MPG Ciudad") +
  labs(title = "Color por Variable Continua")

# Color por variable categórica
color_categorical <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Tracción") +
  labs(title = "Color por Variable Categórica")

print(color_continuous)
print(color_categorical)

# 4.2 MAPEO DE TAMAÑO Y FORMA
cat("\n--- 4.2 Mapeo de Tamaño y Forma ---\n")

# Tamaño por variable
size_mapping <- ggplot(mpg, aes(x = displ, y = hwy, size = cyl, color = class)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 8), name = "Cilindros") +
  labs(title = "Tamaño por Número de Cilindros")

# Forma por variable (máximo 6 formas diferentes)
mpg_subset <- mpg %>% filter(class %in% c("compact", "midsize", "suv", "pickup"))

shape_mapping <- ggplot(mpg_subset, aes(x = displ, y = hwy, shape = class, color = class)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(title = "Forma por Clase de Vehículo")

print(size_mapping)
print(shape_mapping)

# 4.3 COMBINACIÓN DE MÚLTIPLES AESTHETICS
cat("\n--- 4.3 Múltiples Aesthetics ---\n")

multi_aesthetics <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class, size = cyl), alpha = 0.7) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Múltiples Aesthetics Combinados",
       subtitle = "Color por clase, tamaño por cilindros",
       x = "Desplazamiento (L)", y = "MPG Carretera") +
  theme_minimal()

print(multi_aesthetics)

# ==============================================================================
# 5. FACETAS (FACETING) - MÚLTIPLES GRÁFICOS
# ==============================================================================

cat("\n=== FACETAS - MÚLTIPLES PANELES ===\n")

# Las facetas permiten crear múltiples paneles basados en variables categóricas
# Dos funciones principales: facet_wrap() y facet_grid()

# 5.1 FACET_WRAP - Una variable
cat("\n--- 5.1 Facet Wrap ---\n")

facet_wrap_single <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 2) +
  facet_wrap(~class, nrow = 2) +
  labs(title = "Eficiencia por Clase de Vehículo",
       x = "Desplazamiento", y = "MPG Carretera") +
  theme(legend.position = "bottom")

print(facet_wrap_single)

# 5.2 FACET_GRID - Dos variables
cat("\n--- 5.2 Facet Grid ---\n")

# Filtrar datos para mejor visualización
mpg_filtered <- mpg %>% 
  filter(drv %in% c("f", "r", "4"), 
         cyl %in% c(4, 6, 8))

facet_grid_double <- ggplot(mpg_filtered, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 2) +
  facet_grid(drv ~ cyl, labeller = label_both) +
  labs(title = "Eficiencia por Tracción y Cilindros",
       x = "Desplazamiento", y = "MPG Carretera") +
  theme(legend.position = "bottom")

print(facet_grid_double)

# 5.3 FACETAS CON ESCALAS LIBRES
cat("\n--- 5.3 Escalas Libres en Facetas ---\n")

facet_free_scales <- ggplot(economics_long, aes(x = date, y = value)) +
  geom_line(color = "steelblue", size = 1) +
  facet_wrap(~variable, scales = "free_y", nrow = 2) +
  labs(title = "Variables Económicas con Escalas Independientes",
       x = "Año", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(facet_free_scales)

# ==============================================================================
# 6. ESCALAS (SCALES) - CONTROL DE EJES Y COLORES
# ==============================================================================

cat("\n=== ESCALAS - CONTROL DE MAPEOS ===\n")

# Las escalas controlan cómo los datos se mapean a las propiedades visuales

# 6.1 ESCALAS DE POSICIÓN (EJES)
cat("\n--- 6.1 Escalas de Posición ---\n")

# Escala continua con límites personalizados
position_scales <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 3, alpha = 0.7) +
  scale_x_continuous(name = "Desplazamiento del Motor (Litros)",
                     limits = c(1, 7),
                     breaks = seq(1, 7, by = 1)) +
  scale_y_continuous(name = "Eficiencia Carretera (MPG)",
                     limits = c(10, 45),
                     breaks = seq(10, 45, by = 5)) +
  labs(title = "Escalas de Posición Personalizadas")

print(position_scales)

# Transformaciones logarítmicas
log_scales <- ggplot(diamonds %>% sample_n(1000), aes(x = carat, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_log10(name = "Quilates (escala log)") +
  scale_y_log10(name = "Precio (escala log)",
                labels = scales::dollar_format()) +
  labs(title = "Escalas Logarítmicas")

print(log_scales)

# 6.2 ESCALAS DE COLOR
cat("\n--- 6.2 Escalas de Color ---\n")

# Paleta manual
color_manual <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("4" = "red", "f" = "blue", "r" = "green"),
                     name = "Tipo de Tracción",
                     labels = c("4WD", "Delantera", "Trasera")) +
  labs(title = "Colores Manuales")

# Paleta viridis para variables continuas
color_viridis <- ggplot(mpg, aes(x = displ, y = hwy, color = cty)) +
  geom_point(size = 3) +
  scale_color_viridis_c(name = "MPG Ciudad",
                        option = "plasma") +
  labs(title = "Paleta Viridis")

# Paleta ColorBrewer
color_brewer <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  labs(title = "Paleta ColorBrewer")

print(color_manual)
print(color_viridis)
print(color_brewer)

# 6.3 ESCALAS DE RELLENO
cat("\n--- 6.3 Escalas de Relleno ---\n")

fill_scales <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("4" = "#E31A1C", "f" = "#1F78B4", "r" = "#33A02C"),
                    name = "Tracción") +
  labs(title = "Escalas de Relleno Personalizadas",
       x = "Clase de Vehículo", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fill_scales)

# ==============================================================================
# 7. TEMAS (THEMES) - PERSONALIZACIÓN DE APARIENCIA
# ==============================================================================

cat("\n=== TEMAS Y PERSONALIZACIÓN ===\n")

# Los temas controlan todos los elementos no relacionados con los datos

# 7.1 TEMAS PREDEFINIDOS
cat("\n--- 7.1 Temas Predefinidos ---\n")

base_plot <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3) +
  labs(title = "Comparación de Temas",
       x = "Desplazamiento", y = "MPG Carretera")

# Diferentes temas
theme_minimal_plot <- base_plot + theme_minimal() + ggtitle("theme_minimal()")
theme_classic_plot <- base_plot + theme_classic() + ggtitle("theme_classic()")
theme_bw_plot <- base_plot + theme_bw() + ggtitle("theme_bw()")
theme_dark_plot <- base_plot + theme_dark() + ggtitle("theme_dark()")

print(theme_minimal_plot)
print(theme_classic_plot)
print(theme_bw_plot)
print(theme_dark_plot)

# 7.2 PERSONALIZACIÓN DETALLADA
cat("\n--- 7.2 Personalización Detallada ---\n")

custom_theme_plot <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Tema Personalizado Completo",
       subtitle = "Con múltiples ajustes de formato",
       x = "Desplazamiento del Motor (L)",
       y = "Eficiencia en Carretera (MPG)",
       color = "Clase de\nVehículo",
       caption = "Fuente: Dataset mpg") +
  theme_minimal() +
  theme(
    # Título principal
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10)),
    # Subtítulo
    plot.subtitle = element_text(size = 12, hjust = 0.5, 
                                margin = margin(b = 15)),
    # Título de ejes
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    # Texto de ejes
    axis.text.x = element_text(size = 10, margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, margin = margin(r = 5)),
    # Leyenda
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10),
    # Panel y grilla
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.25),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    # Fondo
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "grey98", color = NA),
    # Caption
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10))
  )

print(custom_theme_plot)

# 7.3 CREAR Y GUARDAR TEMA PERSONALIZADO
cat("\n--- 7.3 Tema Personalizado Reutilizable ---\n")

# Definir tema personalizado
my_custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold")
  )

# Aplicar tema personalizado
themed_plot <- ggplot(mpg, aes(x = class, y = hwy, fill = drv)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Eficiencia por Clase y Tracción",
       x = "Clase de Vehículo", y = "MPG Carretera", fill = "Tracción") +
  my_custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(themed_plot)

# ==============================================================================
# 8. ANOTACIONES Y ETIQUETAS
# ==============================================================================

cat("\n=== ANOTACIONES Y ETIQUETAS ===\n")

# 8.1 TEXTO Y ETIQUETAS
cat("\n--- 8.1 Anotaciones de Texto ---\n")

# Gráfico con anotaciones de texto
text_annotations <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 2, alpha = 0.7) +
  # Anotar punto específico
  annotate("text", x = 5.5, y = 40, 
           label = "Zona de alta\neficiencia", 
           hjust = 0.5, vjust = 0.5, size = 4, 
           color = "red", fontface = "bold") +
  # Flecha apuntando a región específica
  annotate("segment", x = 5.5, y = 38, xend = 4.5, yend = 35,
           arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  # Rectángulo resaltando área
  annotate("rect", xmin = 1.5, xmax = 3, ymin = 35, ymax = 45,
           alpha = 0.2, fill = "blue") +
  labs(title = "Gráfico con Anotaciones",
       x = "Desplazamiento", y = "MPG Carretera")

print(text_annotations)

# 8.2 ETIQUETAS EN PUNTOS
cat("\n--- 8.2 Etiquetas en Puntos ---\n")

# Etiquetar puntos específicos
mpg_labeled <- mpg %>%
  filter(hwy > 40 | displ > 6.5) %>%
  mutate(label = paste(manufacturer, model))

labeled_points <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.5, color = "grey50") +
  geom_point(data = mpg_labeled, color = "red", size = 3) +
  ggrepel::geom_text_repel(data = mpg_labeled, 
                          aes(label = label),
                          box.padding = 0.5,
                          point.padding = 0.5,
                          segment.color = "red",
                          size = 3) +
  labs(title = "Vehículos Extremos Etiquetados",
       subtitle = "Alta eficiencia o gran desplazamiento",
       x = "Desplazamiento", y = "MPG Carretera")

print(labeled_points)

# 8.3 LÍNEAS DE REFERENCIA
cat("\n--- 8.3 Líneas de Referencia ---\n")

reference_lines <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 2, alpha = 0.7) +
  # Líneas de referencia
  geom_hline(yintercept = mean(mpg$hwy), linetype = "dashed", 
             color = "red", size = 1) +
  geom_vline(xintercept = mean(mpg$displ), linetype = "dashed", 
             color = "blue", size = 1) +
  # Anotaciones para las líneas
  annotate("text", x = 1.5, y = mean(mpg$hwy) + 1, 
           label = paste("Promedio MPG:", round(mean(mpg$hwy), 1)),
           hjust = 0, color = "red") +
  annotate("text", x = mean(mpg$displ) + 0.1, y = 45, 
           label = paste("Promedio Despl.:", round(mean(mpg$displ), 1)),
           hjust = 0, color = "blue", angle = 90) +
  labs(title = "Gráfico con Líneas de Referencia",
       x = "Desplazamiento", y = "MPG Carretera")

print(reference_lines)

# ==============================================================================
# 9. TRANSFORMACIONES ESTADÍSTICAS
# ==============================================================================

cat("\n=== TRANSFORMACIONES ESTADÍSTICAS ===\n")

# 9.1 REGRESIÓN Y SUAVIZADO
cat("\n--- 9.1 Líneas de Regresión y Suavizado ---\n")

# Diferentes métodos de suavizado
regression_plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.5) +
  # Regresión lineal
  geom_smooth(method = "lm", se = TRUE, color = "red", 
              aes(fill = "Lineal")) +
  # Suavizado loess
  geom_smooth(method = "loess", se = TRUE, color = "blue",
              aes(fill = "Loess")) +
  labs(title = "Métodos de Regresión y Suavizado",
       x = "Desplazamiento", y = "MPG Carretera",
       fill = "Método") +
  scale_fill_manual(values = c("Lineal" = "red", "Loess" = "blue"))

print(regression_plot)

# 9.2 ESTADÍSTICAS RESUMIDAS
cat("\n--- 9.2 Estadísticas Resumidas ---\n")

# Boxplot con estadísticas personalizadas
stat_summary_plot <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "red") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red", fontface = "bold") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, color = "red") +
  labs(title = "Medias con Intervalos de Confianza",
       x = "Clase", y = "MPG Carretera") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(stat_summary_plot)

# 9.3 CONTORNOS Y DENSIDADES 2D
cat("\n--- 9.3 Densidades 2D y Contornos ---\n")

# Gráfico de densidad 2D
density_2d <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.5) +
  geom_density_2d(color = "blue") +
  labs(title = "Densidad 2D con Contornos",
       x = "Desplazamiento", y = "MPG Carretera")

# Mapa de calor de densidad
density_heatmap <- ggplot(mpg, aes(x = displ, y = hwy)) +
  stat_density_2d_filled(alpha = 0.8) +
  geom_point(size = 0.5, color = "white") +
  labs(title = "Mapa de Calor de Densidad",
       x = "Desplazamiento", y = "MPG Carretera")

print(density_2d)
print(density_heatmap)

# ==============================================================================
# 10. COORDENADAS Y PROYECCIONES
# ==============================================================================

cat("\n=== SISTEMAS DE COORDENADAS ===\n")

# 10.1 COORDENADAS CARTESIANAS
cat("\n--- 10.1 Transformaciones de Coordenadas ---\n")

# Gráfico base
coord_base <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Diferentes Sistemas de Coordenadas")

# Coordenadas normales
coord_cartesian <- coord_base + 
  ggtitle("Coordenadas Cartesianas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Coordenadas volteadas
coord_flipped <- coord_base +
  coord_flip() +
  ggtitle("Coordenadas Volteadas")

print(coord_cartesian)
print(coord_flipped)

# 10.2 COORDENADAS POLARES
cat("\n--- 10.2 Coordenadas Polares ---\n")

# Gráfico de barras normal
bar_normal <- ggplot(mpg %>% count(class), aes(x = class, y = n, fill = class)) +
  geom_col() +
  labs(title = "Gráfico de Barras Normal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Convertir a coordenadas polares (gráfico de dona)
pie_chart <- bar_normal +
  coord_polar(theta = "y") +
  ggtitle("Gráfico Circular (Pie Chart)") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

print(bar_normal)
print(pie_chart)

# 10.3 LÍMITES Y ZOOM
cat("\n--- 10.3 Control de Límites ---\n")

# Zoom con coord_cartesian (mantiene todos los datos)
zoom_coord <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(2, 5), ylim = c(20, 35)) +
  labs(title = "Zoom con coord_cartesian()",
       subtitle = "Mantiene todos los datos para cálculos")

# Zoom con scale (filtra los datos)
zoom_scale <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(2, 5)) +
  scale_y_continuous(limits = c(20, 35)) +
  labs(title = "Zoom con scale_*_continuous()",
       subtitle = "Filtra datos antes de cálculos")

print(zoom_coord)
print(zoom_scale)

# ==============================================================================
# 11. EJEMPLOS AVANZADOS Y CASOS DE USO
# ==============================================================================

cat("\n=== EJEMPLOS AVANZADOS ===\n")

# 11.1 GRÁFICO MULTIPLANEL COMPLEJO
cat("\n--- 11.1 Dashboard Multiplanel ---\n")

# Preparar múltiples gráficos
p1 <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "A) Eficiencia vs Desplazamiento",
       x = "Desplazamiento", y = "MPG Carretera") +
  my_custom_theme

p2 <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge") +
  labs(title = "B) Distribución por Clase",
       x = "Clase", y = "Cantidad") +
  my_custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot(aes(fill = drv)) +
  labs(title = "C) Eficiencia por Tracción",
       x = "Tipo de Tracción", y = "MPG Carretera") +
  my_custom_theme

p4 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  labs(title = "D) Distribución de Eficiencia",
       x = "MPG Carretera", y = "Frecuencia") +
  my_custom_theme

# Combinar gráficos
dashboard <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# 11.2 GRÁFICO DE SERIES TEMPORALES AVANZADO
cat("\n--- 11.2 Series Temporales Avanzadas ---\n")

# Preparar datos económicos
econ_processed <- economics %>%
  mutate(
    year = year(date),
    recession = case_when(
      (date >= as.Date("1980-01-01") & date <= as.Date("1980-07-01")) |
      (date >= as.Date("1981-07-01") & date <= as.Date("1982-11-01")) |
      (date >= as.Date("1990-07-01") & date <= as.Date("1991-03-01")) |
      (date >= as.Date("2001-03-01") & date <= as.Date("2001-11-01")) |
      (date >= as.Date("2007-12-01") & date <= as.Date("2009-06-01")) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Gráfico avanzado de series temporales
advanced_timeseries <- ggplot(econ_processed, aes(x = date, y = unemploy)) +
  # Áreas de recesión
  geom_rect(data = econ_processed %>% filter(recession),
            aes(xmin = date, xmax = lead(date, default = max(date)),
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  # Línea principal
  geom_line(color = "steelblue", size = 1) +
  # Línea de tendencia
  geom_smooth(method = "loess", span = 0.3, se = TRUE, 
              color = "red", fill = "red", alpha = 0.2) +
  # Anotaciones
  annotate("text", x = as.Date("2008-01-01"), y = 15000,
           label = "Crisis\nFinanciera", hjust = 0.5, vjust = 0.5,
           color = "red", fontface = "bold") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Desempleo en Estados Unidos (1967-2015)",
       subtitle = "Áreas sombreadas indican recesiones oficiales",
       x = "Año", y = "Número de Desempleados (miles)",
       caption = "Fuente: FRED Economic Data") +
  my_custom_theme

print(advanced_timeseries)

# 11.3 MATRIZ DE GRÁFICOS DE CORRELACIÓN
cat("\n--- 11.3 Matriz de Correlación Visual ---\n")

# Preparar datos numéricos de mtcars
mtcars_numeric <- mtcars %>%
  select(mpg, disp, hp, drat, wt, qsec)

# Crear función para gráficos de la diagonal inferior
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Matriz de gráficos con GGally
correlation_matrix <- GGally::ggpairs(
  mtcars_numeric,
  title = "Matriz de Correlación - Variables de mtcars",
  lower = list(continuous = GGally::wrap("smooth", alpha = 0.3, size = 0.5)),
  upper = list(continuous = GGally::wrap("cor", size = 3)),
  diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5))
) + my_custom_theme

print(correlation_matrix)

# ==============================================================================
# 12. BUENAS PRÁCTICAS Y OPTIMIZACIÓN
# ==============================================================================

cat("\n=== BUENAS PRÁCTICAS ===\n")

# 12.1 PALETAS DE COLORES ACCESIBLES
cat("\n--- 12.1 Paletas Accesibles ---\n")

# Paleta para daltónicos
colorblind_safe <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("4" = "#E69F00", "f" = "#56B4E9", "r" = "#009E73"),
                     name = "Tracción") +
  labs(title = "Paleta Segura para Daltonismo",
       x = "Desplazamiento", y = "MPG Carretera")

print(colorblind_safe)

# 12.2 GRÁFICOS PARA PUBLICACIÓN
cat("\n--- 12.2 Formato para Publicación ---\n")

publication_plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(shape = drv), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_shape_manual(values = c(16, 17, 15),
                     name = "Drive Type",
                     labels = c("4WD", "Front", "Rear")) +
  labs(title = "Fuel Efficiency vs Engine Displacement",
       x = "Engine Displacement (L)",
       y = "Highway Fuel Economy (mpg)") +
  theme_classic() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black")
  )

print(publication_plot)

# 12.3 FUNCIÓN PARA GUARDAR GRÁFICOS
cat("\n--- 12.3 Función para Guardar Gráficos ---\n")

# Función personalizada para guardar gráficos
save_publication_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggsave(
    filename = paste0(filename, ".png"),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  ggsave(
    filename = paste0(filename, ".pdf"),
    plot = plot,
    width = width,
    height = height,
    device = "pdf"
  )
  
  cat("Gráfico guardado como:", paste0(filename, ".png"), "and", paste0(filename, ".pdf"), "\n")
}

# Ejemplo de uso (comentado para evitar crear archivos)
# save_publication_plot(publication_plot, "fuel_efficiency_analysis")

# 12.4 PLANTILLA DE GRÁFICO ESTÁNDAR
cat("\n--- 12.4 Plantilla Estándar ---\n")

create_standard_plot <- function(data, x_var, y_var, color_var = NULL, 
                                title = "Título del Gráfico", 
                                x_label = NULL, y_label = NULL) {
  
  # Etiquetas por defecto
  if(is.null(x_label)) x_label <- x_var
  if(is.null(y_label)) y_label <- y_var
  
  # Crear el gráfico base
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Añadir color si se especifica
  if(!is.null(color_var)) {
    p <- p + geom_point(aes_string(color = color_var), size = 3, alpha = 0.7)
  } else {
    p <- p + geom_point(size = 3, alpha = 0.7, color = "steelblue")
  }
  
  # Añadir elementos estándar
  p <- p +
    labs(title = title, x = x_label, y = y_label) +
    my_custom_theme +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Ejemplo de uso de la plantilla
template_example <- create_standard_plot(
  data = mpg,
  x_var = "displ",
  y_var = "hwy",
  color_var = "class",
  title = "Ejemplo de Plantilla Estándar",
  x_label = "Desplazamiento del Motor (L)",
  y_label = "Eficiencia en Carretera (MPG)"
)

print(template_example)

# ==============================================================================
# RESUMEN Y RECURSOS ADICIONALES
# ==============================================================================

cat("\n=== RESUMEN FINAL ===\n")

cat("
RESUMEN DE GGPLOT2:

1. COMPONENTES PRINCIPALES:
   - Data: Los datos a visualizar
   - Aesthetics: Mapeo de variables a propiedades visuales
   - Geometries: Tipo de representación gráfica
   - Statistics: Transformaciones estadísticas
   - Scales: Control de mapeos
   - Coordinates: Sistema de coordenadas
   - Faceting: División en paneles
   - Themes: Apariencia no relacionada con datos

2. FLUJO DE TRABAJO RECOMENDADO:
   a) Explorar y limpiar datos
   b) Definir objetivo del gráfico
   c) Elegir geometría apropiada
   d) Mapear variables a aesthetics
   e) Ajustar escalas y ejes
   f) Personalizar tema
   g) Añadir anotaciones si es necesario
   h) Revisar y refinar

3. MEJORES PRÁCTICAS:
   - Usar colores accesibles
   - Mantener simplicidad
   - Etiquetar claramente
   - Usar facetas para múltiples grupos
   - Documentar código
   - Guardar en múltiples formatos

4. RECURSOS ADICIONALES:
   - Documentación oficial: https://ggplot2.tidyverse.org/
   - Galería de ejemplos: https://r-graph-gallery.com/
   - Libro 'ggplot2: Elegant Graphics for Data Analysis'
   - Cheat sheet de RStudio
")

cat("\n¡Guía completa de ggplot2 finalizada!\n")
cat("Total de ejemplos creados:", length(ls(pattern = "_plot$|_chart$")), "\n")

# Limpiar variables temporales pero mantener funciones útiles
rm(list = ls(pattern = "^(p1|p2|p3|p4|mpg_|econ_|mtcars_)"))

cat("\nFunciones útiles creadas y disponibles:\n")
cat("- my_custom_theme: Tema personalizado\n")
cat("- create_standard_plot(): Plantilla para gráficos estándar\n")
cat("- save_publication_plot(): Función para guardar gráficos\n")