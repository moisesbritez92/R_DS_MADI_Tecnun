# Script temporal para probar la transformación de tasas
library(readr)
library(dplyr)

# Leer datos
crime_data <- read_csv("crimeUS.csv", show_col_types = FALSE)

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

cat("\n✓ Variable transformada: 'violent_rate_per_100k' (por 100,000 hab.)")
cat("\n✓ Fórmula: (violent / population) * 100,000")
cat("\n✓ Rango de tasas:", round(min(crime_data_with_rates$violent_rate_per_100k), 1), 
    "a", round(max(crime_data_with_rates$violent_rate_per_100k), 1), "por 100k habitantes")
cat("\n✓ Promedio nacional:", round(mean(crime_data_with_rates$violent_rate_per_100k), 1), "por 100k habitantes\n")