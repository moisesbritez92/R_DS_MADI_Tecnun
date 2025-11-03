# Comparación de District of Columbia, Hawaii e Idaho - Boxplots
library(readr)
library(dplyr)
library(ggplot2)

# Leer datos y calcular tasas
crime_data <- read_csv("crimeUS.csv", show_col_types = FALSE)
crime_data_with_rates <- crime_data %>%
  mutate(violent_rate_per_100k = round((violent / population) * 100000, 2))

# Estados a comparar
estados_comparacion <- c("District of Columbia", "Hawaii", "Idaho")

# Filtrar datos para los tres estados
datos_comparacion <- crime_data_with_rates %>%
  filter(state %in% estados_comparacion)

cat("COMPARACIÓN: DISTRICT OF COLUMBIA vs HAWAII vs IDAHO\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Estadísticas descriptivas por estado
estadisticas_estados <- datos_comparacion %>%
  group_by(state) %>%
  summarise(
    n_registros = n(),
    min_tasa = round(min(violent_rate_per_100k), 1),
    mediana_tasa = round(median(violent_rate_per_100k), 1),
    media_tasa = round(mean(violent_rate_per_100k), 1),
    max_tasa = round(max(violent_rate_per_100k), 1),
    sd_tasa = round(sd(violent_rate_per_100k), 1),
    .groups = "drop"
  )

print(estadisticas_estados)

# Comparación de promedios
cat("\nCOMPARACIÓN DE PROMEDIOS:\n")
for(estado in estados_comparacion) {
  media_estado <- estadisticas_estados$media_tasa[estadisticas_estados$state == estado]
  cat(sprintf("%-20s: %6.1f por 100k habitantes\n", estado, media_estado))
}

# Diferencias entre estados
dc_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "District of Columbia"]
hawaii_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "Hawaii"]
idaho_media <- estadisticas_estados$media_tasa[estadisticas_estados$state == "Idaho"]

cat("\nDIFERENCIAS:\n")
cat(sprintf("DC es %.1f veces más alta que Hawaii\n", dc_media/hawaii_media))
cat(sprintf("DC es %.1f veces más alta que Idaho\n", dc_media/idaho_media))
cat(sprintf("Hawaii es %.1f veces más alta que Idaho\n", hawaii_media/idaho_media))

cat("\nCREANDO BOXPLOTS...\n")

# Boxplot principal
boxplot_principal <- ggplot(datos_comparacion, aes(x = state, y = violent_rate_per_100k, fill = state)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  labs(
    title = "Comparación de Tasas de Crímenes Violentos por 100k Habitantes",
    subtitle = "District of Columbia vs Hawaii vs Idaho (1977-1999)",
    x = "Estado",
    y = "Tasa de Crímenes Violentos por 100,000 Habitantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("District of Columbia" = "red3", 
                              "Hawaii" = "dodgerblue3", 
                              "Idaho" = "forestgreen")) +
  scale_y_continuous(breaks = seq(0, 3000, 500))

print(boxplot_principal)

# Gráfico de evolución temporal
grafico_temporal <- ggplot(datos_comparacion, aes(x = year, y = violent_rate_per_100k, color = state)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Evolución Temporal de las Tasas de Crímenes Violentos",
    subtitle = "District of Columbia vs Hawaii vs Idaho (1977-1999)",
    x = "Año",
    y = "Tasa por 100,000 Habitantes",
    color = "Estado"
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
  scale_x_continuous(breaks = seq(1977, 1999, 5))

print(grafico_temporal)

cat("\nRESUMEN:\n")
cat(sprintf("🔴 DC:    %.1f por 100k (EXTREMADAMENTE ALTA)\n", dc_media))
cat(sprintf("🔵 Hawaii: %.1f por 100k (MODERADA)\n", hawaii_media)) 
cat(sprintf("🟢 Idaho:  %.1f por 100k (BAJA)\n", idaho_media))
cat("\n✅ Boxplots y gráficos creados exitosamente!\n")