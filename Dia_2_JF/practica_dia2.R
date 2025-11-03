# Práctica Día 2 - Análisis de datos de temporada 18-19
# Autor: [Tu nombre]
# Fecha: 25 de septiembre de 2025

# Cargar librerías necesarias
library(readr)      # Para leer archivos CSV
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualizaciones

# Leer el archivo CSV
datos <- read_csv("season-1819_csv.csv")

# Mostrar información básica del dataset
cat("Dimensiones del dataset:\n")
print(dim(datos))

cat("\nPrimeras 6 filas:\n")
print(head(datos))

cat("\nEstructura del dataset:\n")
print(str(datos))

cat("\nResumen estadístico:\n")
print(summary(datos))

cat("\nNombres de las columnas:\n")
print(names(datos))

# =============================================================================
# ANÁLISIS DE TABLAS DE FRECUENCIA BIDIMENSIONALES
# =============================================================================

# Crear función para generar tabla de frecuencia de un equipo
crear_tabla_frecuencia <- function(equipo_nombre) {
  
  # Filtrar partidos donde el equipo juega en casa (HomeTeam)
  partidos_casa <- datos %>%
    filter(HomeTeam == equipo_nombre) %>%
    mutate(Ubicacion = "Casa")
  
  # Filtrar partidos donde el equipo juega fuera (AwayTeam)
  partidos_fuera <- datos %>%
    filter(AwayTeam == equipo_nombre) %>%
    mutate(Ubicacion = "Fuera")
  
  # Para partidos en casa: H = Victoria, D = Empate, A = Derrota
  partidos_casa <- partidos_casa %>%
    mutate(Resultado = case_when(
      FTR == "H" ~ "Victoria",
      FTR == "D" ~ "Empate", 
      FTR == "A" ~ "Derrota"
    ))
  
  # Para partidos fuera: A = Victoria, D = Empate, H = Derrota
  partidos_fuera <- partidos_fuera %>%
    mutate(Resultado = case_when(
      FTR == "A" ~ "Victoria",
      FTR == "D" ~ "Empate",
      FTR == "H" ~ "Derrota"
    ))
  
  # Combinar ambos datasets
  todos_partidos <- bind_rows(partidos_casa, partidos_fuera)
  
  # Crear tabla de frecuencia bidimensional
  tabla_freq <- table(todos_partidos$Ubicacion, todos_partidos$Resultado)
  
  return(tabla_freq)
}

# =============================================================================
# TABLA DE FRECUENCIA PARA REAL SOCIEDAD
# =============================================================================



# Crear tabla para Real Sociedad
tabla_real_sociedad <- crear_tabla_frecuencia("Sociedad")
print(tabla_real_sociedad)

# Convertir a proporción para mejor interpretación
cat("\nProporciones (por filas):\n")
prop_real_sociedad <- prop.table(tabla_real_sociedad, margin = 1) # Por filas
print(round(prop_real_sociedad, 3))

# =============================================================================
# TABLA DE FRECUENCIA PARA BARCELONA
# =============================================================================

cat("\n" , "="*60, "\n")
cat("TABLA DE FRECUENCIA BIDIMENSIONAL - BARCELONA\n")
cat("="*60, "\n")

# Crear tabla para Barcelona
tabla_barcelona <- crear_tabla_frecuencia("Barcelona")
print(tabla_barcelona)

# Convertir a proporción para mejor interpretación
cat("\nProporciones (por filas):\n")
prop_barcelona <- prop.table(tabla_barcelona, margin = 1) # Por filas
print(round(prop_barcelona, 3))

# =============================================================================
# VISUALIZACIÓN DE LAS TABLAS
# =============================================================================

# Convertir tablas a data frames para ggplot
library(tidyr)

# Real Sociedad
df_real <- as.data.frame(tabla_real_sociedad)
names(df_real) <- c("Ubicacion", "Resultado", "Frecuencia")

# Barcelona
df_barcelona <- as.data.frame(tabla_barcelona)
names(df_barcelona) <- c("Ubicacion", "Resultado", "Frecuencia")

# Gráfico para Real Sociedad
plot_real <- ggplot(df_real, aes(x = Ubicacion, y = Frecuencia, fill = Resultado)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Resultados de Real Sociedad por Ubicación",
       subtitle = "Temporada 2018-19",
       x = "Ubicación del Partido",
       y = "Número de Partidos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Victoria" = "green3", 
                              "Empate" = "orange2", 
                              "Derrota" = "red3"))

print(plot_real)

# Gráfico para Barcelona
plot_barcelona <- ggplot(df_barcelona, aes(x = Ubicacion, y = Frecuencia, fill = Resultado)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Resultados de Barcelona por Ubicación",
       subtitle = "Temporada 2018-19",
       x = "Ubicación del Partido",
       y = "Número de Partidos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Victoria" = "blue3", 
                              "Empate" = "orange2", 
                              "Derrota" = "red3"))

print(plot_barcelona)
