# ═══════════════════════════════════════════════════════════════════════════════
# 📊 CHULETA DE R - ANÁLISIS DE DATOS Y ESTADÍSTICA
# ═══════════════════════════════════════════════════════════════════════════════
# Fecha: Octubre 2025
# Basada en ejercicios de visualización y análisis de datos en R
# ═══════════════════════════════════════════════════════════════════════════════

# ===============================================================================
# 📦 1. LIBRERÍAS ESENCIALES
# ===============================================================================

# Manipulación de datos
library(dplyr)        # Manipulación de datos (group_by, summarise, filter, mutate)
library(tidyr)        # Reestructuración de datos (pivot_longer, pivot_wider)
library(stringr)      # Manipulación de strings (str_replace, str_detect)
library(readr)        # Lectura rápida de datos

# Visualización
library(ggplot2)      # Gráficos avanzados
library(viridis)      # Paletas de colores
library(ggrepel)      # Etiquetas sin solapamiento
library(gridExtra)    # Múltiples gráficos

# Texto y NLP
library(tm)           # Text mining (removePunctuation, removeWords, stopwords)
library(textstem)     # Lemmatización (lemmatize_words)

# Estadística
library(pwr)          # Análisis de potencia estadística

# ===============================================================================
# 📂 2. CARGA Y EXPLORACIÓN DE DATOS
# ===============================================================================

# 2.1 Cargar datos
data <- read.csv('./data/archivo.csv', 
                 header = TRUE,              # Primera fila son nombres
                 sep = ',',                  # Separador
                 stringsAsFactors = FALSE,   # No convertir strings a factores
                 check.names = FALSE,        # Mantener nombres originales
                 fileEncoding = 'latin1')    # Encoding para caracteres especiales

# 2.2 Exploración básica
head(data)              # Primeras 6 filas
tail(data, 10)          # Últimas 10 filas
str(data)               # Estructura del dataset
summary(data)           # Resumen estadístico
dim(data)               # Dimensiones (filas x columnas)
colnames(data)          # Nombres de columnas
nrow(data)              # Número de filas
ncol(data)              # Número de columnas

# 2.3 Valores únicos y frecuencias
unique(data$columna)                    # Valores únicos
length(unique(data$columna))            # Número de valores únicos
table(data$columna)                     # Tabla de frecuencias
sort(table(data$columna), decreasing = TRUE)  # Frecuencias ordenadas

# ===============================================================================
# 🧹 3. LIMPIEZA DE DATOS
# ===============================================================================

# 3.1 Eliminar columnas
data <- data[, -2]                      # Eliminar segunda columna
data <- data[, -c(3, 4, 5)]            # Eliminar columnas 3, 4 y 5
data <- data %>% select(-columna_x)     # Eliminar por nombre

# 3.2 Renombrar columnas
colnames(data) <- c('Col1', 'Col2', 'Col3')
data <- data %>% rename(nuevo = antiguo)

# 3.3 Missing values
sum(is.na(data))                        # Total de NAs
sapply(data, function(x) sum(is.na(x))) # NAs por columna
data_clean <- na.omit(data)             # Eliminar filas con NAs
data$col[is.na(data$col)] <- 0         # Reemplazar NAs con 0

# 3.4 Conversión de tipos
data$col <- as.numeric(data$col)
data$col <- as.character(data$col)
data$col <- as.factor(data$col)
data$col <- as.Date(data$col)

# 3.5 Limpiar strings
data$col <- tolower(data$col)                           # A minúsculas
data$col <- toupper(data$col)                           # A mayúsculas
data$col <- gsub("\\.", "", data$col)                   # Eliminar puntos
data$col <- str_replace(data$col, "^[0-9]+[\\. ]*", "") # Eliminar códigos numéricos
data$col <- str_trim(data$col)                          # Eliminar espacios

# ===============================================================================
# 🔄 4. MANIPULACIÓN DE DATOS (dplyr)
# ===============================================================================

# 4.1 Filtrar (filter)
data %>% filter(columna > 100)
data %>% filter(columna == "valor")
data %>% filter(columna %in% c("A", "B", "C"))
data %>% filter(col1 > 10 & col2 == "X")
data %>% filter(col1 > 10 | col2 == "X")
data %>% filter(!is.na(columna))

# 4.2 Seleccionar columnas (select)
data %>% select(col1, col2, col3)
data %>% select(-col_eliminar)
data %>% select(starts_with("pre"))
data %>% select(ends_with("sufijo"))
data %>% select(contains("texto"))

# 4.3 Crear nuevas columnas (mutate)
data %>% mutate(nueva = col1 + col2)
data %>% mutate(
  nueva1 = col1 * 2,
  nueva2 = log(col2),
  nueva3 = ifelse(col3 > 10, "Alto", "Bajo")
)

# 4.4 Condicionales (case_when)
data %>% mutate(
  categoria = case_when(
    valor < 10 ~ "Bajo",
    valor < 50 ~ "Medio",
    valor >= 50 ~ "Alto",
    TRUE ~ "Desconocido"  # Caso por defecto
  )
)

# 4.5 Agrupar y resumir (group_by + summarise)
data %>%
  group_by(categoria) %>%
  summarise(
    n = n(),                           # Contar filas
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desv_std = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    suma = sum(valor, na.rm = TRUE),
    .groups = "drop"                   # Desagrupar al final
  )

# 4.6 Ordenar (arrange)
data %>% arrange(columna)              # Ascendente
data %>% arrange(desc(columna))        # Descendente
data %>% arrange(col1, desc(col2))     # Múltiples criterios

# 4.7 Combinar datasets
merge(df1, df2, by = "id")                        # Inner join
merge(df1, df2, by = "id", all.x = TRUE)          # Left join
merge(df1, df2, by = "id", all.y = TRUE)          # Right join
merge(df1, df2, by = "id", all = TRUE)            # Full join
df1 %>% inner_join(df2, by = "id")                # dplyr inner join
df1 %>% left_join(df2, by = c("col1" = "col2"))   # Join con diferentes nombres

# ===============================================================================
# 🔀 5. REESTRUCTURACIÓN DE DATOS (tidyr)
# ===============================================================================

# 5.1 De ancho a largo (pivot_longer)
data_long <- data %>%
  pivot_longer(
    cols = c(col1, col2, col3),        # Columnas a convertir
    names_to = "variable",             # Nombre de la columna de nombres
    values_to = "valor"                # Nombre de la columna de valores
  )

# Ejemplo: convertir años en columnas a formato largo
data %>%
  pivot_longer(
    cols = -Comunidad,                 # Todas excepto Comunidad
    names_to = "Año",
    values_to = "Poblacion"
  )

# 5.2 De largo a ancho (pivot_wider)
data_wide <- data %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )

# ===============================================================================
# 📊 6. VISUALIZACIÓN (ggplot2)
# ===============================================================================

# 6.1 Estructura básica de ggplot2
ggplot(data, aes(x = col_x, y = col_y)) +
  geom_point() +                       # Tipo de gráfico
  labs(title = "Título",               # Etiquetas
       x = "Eje X",
       y = "Eje Y") +
  theme_minimal()                      # Tema

# 6.2 Gráfico de líneas
ggplot(data, aes(x = year, y = value, color = category, group = category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_viridis_d() +            # Colores discretos
  theme_minimal()

# 6.3 Gráfico de barras
# Barras simples
ggplot(data, aes(x = categoria, y = valor)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip()                         # Barras horizontales

# Barras agrupadas
ggplot(data, aes(x = categoria, y = valor, fill = grupo)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8)

# Barras apiladas
ggplot(data, aes(x = categoria, y = valor, fill = grupo)) +
  geom_bar(stat = "identity", position = "stack")

# 6.4 Boxplot
ggplot(data, aes(x = categoria, y = valor, fill = categoria)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "coral"))

# 6.5 Scatter plot
ggplot(data, aes(x = var1, y = var2, color = categoria, size = var3)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Línea de tendencia
  scale_size_continuous(range = c(2, 8))

# 6.6 Facetas (múltiples gráficos)
ggplot(data, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~ categoria, scales = "free_y")  # scales: "free", "free_x", "free_y"

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  facet_grid(rows = vars(categoria1), cols = vars(categoria2))

# 6.7 Personalización avanzada
ggplot(data, aes(x = x, y = y, color = grupo)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_ellipse(aes(fill = grupo), alpha = 0.2, geom = "polygon") +  # Elipses
  geom_text_repel(aes(label = etiqueta), size = 3, max.overlaps = 15) +  # Etiquetas
  scale_color_manual(values = c("A" = "steelblue", "B" = "coral")) +
  scale_y_continuous(labels = scales::percent) +  # Eje en porcentaje
  scale_x_continuous(breaks = seq(1980, 2000, 5)) +  # Breaks personalizados
  labs(
    title = "Título Principal",
    subtitle = "Subtítulo informativo",
    x = "Eje X",
    y = "Eje Y",
    color = "Leyenda",
    caption = "Fuente: Datos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 6.8 Guardar gráficos
ggsave("grafico.png", width = 10, height = 6, dpi = 300)

# ===============================================================================
# 📈 7. ANÁLISIS ESTADÍSTICO
# ===============================================================================

# 7.1 Estadísticas descriptivas
mean(data$col, na.rm = TRUE)           # Media
median(data$col, na.rm = TRUE)         # Mediana
sd(data$col, na.rm = TRUE)             # Desviación estándar
var(data$col, na.rm = TRUE)            # Varianza
quantile(data$col, probs = c(0.25, 0.5, 0.75))  # Cuartiles
IQR(data$col, na.rm = TRUE)            # Rango intercuartílico

# 7.2 Test t de Student (comparar medias)
# Test de una muestra
t.test(data$variable, mu = 10)         # H0: media = 10

# Test de dos muestras independientes
t.test(valor ~ grupo, data = data)     # Comparar dos grupos
t.test(data$grupo1, data$grupo2)       # Alternativa

# Test pareado
t.test(data$antes, data$despues, paired = TRUE)

# Extraer resultados
resultado <- t.test(valor ~ grupo, data = data)
resultado$statistic                     # Estadístico t
resultado$p.value                       # P-valor
resultado$conf.int                      # Intervalo de confianza
resultado$estimate                      # Estimaciones

# 7.3 Correlación
cor(data$var1, data$var2)              # Correlación de Pearson
cor(data$var1, data$var2, method = "spearman")  # Spearman (no paramétrica)

# Test de correlación
cor_test <- cor.test(data$var1, data$var2)
cor_test$estimate                       # Coeficiente de correlación
cor_test$p.value                        # P-valor
cor_test$conf.int                       # Intervalo de confianza

# 7.4 Test de Fisher (tablas de contingencia)
# Crear tabla de contingencia
tabla <- matrix(c(20, 10, 15, 25), nrow = 2)

# Test de Fisher
fisher.test(tabla, alternative = "greater")  # "greater", "less", "two.sided"

# Con datos reales
tabla <- table(data$var1, data$var2)
fisher.test(tabla)

# 7.5 Chi-cuadrado (independencia)
chisq.test(tabla)

# 7.6 ANOVA (comparar más de dos grupos)
anova_result <- aov(valor ~ grupo, data = data)
summary(anova_result)

# Test post-hoc (comparaciones múltiples)
TukeyHSD(anova_result)

# 7.7 Regresión lineal
modelo <- lm(y ~ x, data = data)
summary(modelo)                         # Resumen del modelo
coef(modelo)                            # Coeficientes
confint(modelo)                         # Intervalos de confianza
predict(modelo, newdata = datos_nuevos) # Predicciones

# Regresión múltiple
modelo_multi <- lm(y ~ x1 + x2 + x3, data = data)
summary(modelo_multi)

# ===============================================================================
# 🎲 8. CORRECCIÓN POR COMPARACIONES MÚLTIPLES
# ===============================================================================

# Cuando se realizan múltiples tests estadísticos, hay que ajustar p-values

# 8.1 Métodos de ajuste
p_values <- c(0.01, 0.03, 0.05, 0.08, 0.12)

# Bonferroni (muy conservador)
p.adjust(p_values, method = "bonferroni")

# Benjamini-Hochberg / FDR (recomendado)
p.adjust(p_values, method = "BH")

# Holm (menos conservador que Bonferroni)
p.adjust(p_values, method = "holm")

# 8.2 Aplicación práctica
resultados <- data.frame(
  estado = c("A", "B", "C", "D"),
  p_value = c(0.01, 0.03, 0.05, 0.08)
)

resultados$p_adjusted <- p.adjust(resultados$p_value, method = "BH")
resultados$significativo <- resultados$p_adjusted < 0.05

# ===============================================================================
# 🔬 9. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ===============================================================================

# 9.1 Realizar PCA
# Preparar matriz (solo variables numéricas)
matriz <- data %>% select(var1, var2, var3, var4) %>% as.matrix()

# PCA
pca <- prcomp(matriz, scale. = TRUE, center = TRUE)

# 9.2 Extraer información
# Varianza explicada
varianza_explicada <- summary(pca)$importance[2, 1:2] * 100
cat("PC1 explica:", round(varianza_explicada[1], 2), "%\n")
cat("PC2 explica:", round(varianza_explicada[2], 2), "%\n")

# Componentes principales (scores)
pca_scores <- pca$x[, 1:2]              # Primeros 2 componentes

# Loadings (contribución de cada variable)
loadings <- pca$rotation[, 1:2]
pc1_loadings <- abs(pca$rotation[, 1])
top_pc1 <- head(sort(pc1_loadings, decreasing = TRUE), 5)

# 9.3 Visualizar PCA
pca_df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  grupo = data$categoria
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = grupo)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_ellipse(aes(fill = grupo), alpha = 0.2, geom = "polygon") +
  labs(
    title = "Análisis de Componentes Principales (PCA)",
    x = paste("PC1 (", round(varianza_explicada[1], 1), "%)"),
    y = paste("PC2 (", round(varianza_explicada[2], 1), "%)")
  ) +
  theme_minimal()

# 9.4 Biplot (variables + observaciones)
# Extraer loadings
loadings_df <- data.frame(
  Variable = rownames(loadings),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2]
)

scale_factor <- 3  # Factor de escala para flechas

ggplot() +
  geom_point(data = pca_df, aes(x = PC1, y = PC2, color = grupo), alpha = 0.7) +
  geom_segment(data = loadings_df, 
               aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = loadings_df, 
                  aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Variable),
                  color = "red", size = 3.5) +
  theme_minimal()

# ===============================================================================
# 💪 10. ANÁLISIS DE POTENCIA ESTADÍSTICA
# ===============================================================================

# 10.1 Test de correlación
# ¿Cuál es la potencia con n=12 para detectar r=0.5?
power_result <- pwr.r.test(n = 12, r = 0.5, sig.level = 0.05)
power_result$power

# ¿Qué n necesito para power=0.8 con r=0.5?
n_result <- pwr.r.test(r = 0.5, sig.level = 0.05, power = 0.8)
ceiling(n_result$n)

# 10.2 Test t
# Para test t de dos muestras independientes
pwr.t.test(n = 30, d = 0.5, sig.level = 0.05, type = "two.sample")

# 10.3 ANOVA
pwr.anova.test(k = 3, n = 20, f = 0.25, sig.level = 0.05)

# 10.4 Proporciones
pwr.2p.test(h = 0.3, n = 100, sig.level = 0.05)

# ===============================================================================
# 📝 11. PROCESAMIENTO DE TEXTO (NLP)
# ===============================================================================

# 11.1 Limpieza básica
texto <- "ESTE es un TEXTO de Ejemplo... con 123 números!"

texto_clean <- texto %>%
  tolower() %>%                         # Minúsculas
  removePunctuation() %>%               # Quitar puntuación
  removeNumbers() %>%                   # Quitar números
  removeWords(stopwords("english"))     # Quitar stopwords

# 11.2 Stopwords (palabras comunes sin significado)
stopwords("english")                    # Inglés
stopwords("spanish")                    # Español

# 11.3 Lemmatización (reducir a forma base)
palabras <- c("running", "ran", "runs", "driving", "drove")
lemmatize_words(palabras)              # "run", "run", "run", "drive", "drive"

# 11.4 Crear matriz documento-término
corpus <- VCorpus(VectorSource(data$texto))

# Crear matriz
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(3, Inf)))
matriz_texto <- t(as.matrix(tdm))      # Transponer (filas = documentos)

# Filtrar palabras por frecuencia
word_freq <- colSums(matriz_texto)
palabras_frecuentes <- names(word_freq[word_freq >= 50])
matriz_filtrada <- matriz_texto[, palabras_frecuentes]

# ===============================================================================
# 🔢 12. EXPRESIONES REGULARES (regex)
# ===============================================================================

# Detectar patrones
str_detect("texto123", "[0-9]+")       # TRUE si contiene números
str_detect("email@test.com", "@")      # TRUE si contiene @

# Reemplazar
str_replace("texto123", "[0-9]+", "")  # Eliminar números
str_replace_all("a-b-c", "-", "_")     # Reemplazar todos

# Extraer
str_extract("precio: 25.50", "[0-9.]+")  # Extraer número

# Patrones comunes
"^[0-9]+"           # Números al inicio
"[0-9]+$"           # Números al final
"[^0-9]"            # Cualquier cosa que NO sea número
"\\."               # Punto literal (escape)
"\\s"               # Espacio en blanco
"[a-zA-Z]+"         # Letras
"[aeiou]"           # Vocales

# ===============================================================================
# 🎯 13. CONSEJOS Y BUENAS PRÁCTICAS
# ===============================================================================

# 13.1 Pipe operator (%>%)
# En lugar de:
resultado <- arrange(filter(select(data, col1, col2), col1 > 10), col2)

# Mejor:
resultado <- data %>%
  select(col1, col2) %>%
  filter(col1 > 10) %>%
  arrange(col2)

# 13.2 Imprimir con formato
cat("=== TÍTULO ===\n")
cat("Valor:", round(valor, 2), "\n")
cat(paste("Total:", sum(data$col), "\n"))

# 13.3 Crear secuencias
1:10                                    # 1, 2, 3, ..., 10
seq(0, 100, by = 10)                   # 0, 10, 20, ..., 100
seq(0, 1, length.out = 11)             # 11 valores entre 0 y 1

# 13.4 Aplicar funciones
lapply(data, mean)                     # Lista
sapply(data, mean)                     # Vector simplificado
apply(matriz, 1, sum)                  # Por filas (1) o columnas (2)

# 13.5 Condicionales
if(condicion) {
  # código
} else if(otra_condicion) {
  # código
} else {
  # código
}

# ifelse vectorizado
data$categoria <- ifelse(data$valor > 10, "Alto", "Bajo")

# 13.6 Loops
for(i in 1:nrow(data)) {
  # procesar fila i
}

for(nombre in nombres) {
  # procesar nombre
}

# 13.7 Crear funciones
calcular_media <- function(x, quitar_na = TRUE) {
  if(quitar_na) {
    x <- x[!is.na(x)]
  }
  suma <- sum(x)
  n <- length(x)
  return(suma / n)
}

# ===============================================================================
# 📊 14. TABLAS DE CONTINGENCIA Y FRECUENCIAS
# ===============================================================================

# Tabla simple
table(data$categoria)

# Tabla de contingencia 2x2
tabla <- table(data$var1, data$var2)
prop.table(tabla)                      # Proporciones
prop.table(tabla, margin = 1)          # Por filas
prop.table(tabla, margin = 2)          # Por columnas

# Agregar totales
addmargins(tabla)

# ===============================================================================
# 🎨 15. PALETAS DE COLORES
# ===============================================================================

# Colores predefinidos
scale_fill_viridis_d()                 # Discreto
scale_fill_viridis_c()                 # Continuo
scale_color_brewer(palette = "Set1")   # ColorBrewer

# Colores manuales
scale_fill_manual(values = c("A" = "steelblue", "B" = "coral", "C" = "green"))
scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1"))

# ===============================================================================
# 📌 16. ATAJOS DE TECLADO ÚTILES EN RSTUDIO
# ===============================================================================

# Ctrl + Enter       : Ejecutar línea/selección
# Ctrl + Shift + M   : Pipe operator (%>%)
# Ctrl + Shift + C   : Comentar/descomentar
# Alt + -            : Operador asignación (<-)
# Ctrl + L           : Limpiar consola
# Ctrl + Shift + F10 : Reiniciar sesión R
# Tab                : Autocompletar

# ===============================================================================
# 🔍 17. INTERPRETACIÓN DE P-VALUES
# ===============================================================================

# p < 0.001  : Muy significativo (***)
# p < 0.01   : Significativo (**)
# p < 0.05   : Significativo (*)
# p >= 0.05  : No significativo (ns)

# Regla general:
# - Si p < α (usualmente 0.05) → Rechazar H0 (hay efecto significativo)
# - Si p ≥ α → No rechazar H0 (no hay evidencia suficiente)

# ===============================================================================
# 📈 18. FÓRMULAS ESTADÍSTICAS COMUNES
# ===============================================================================

# Media: x̄ = Σx / n
mean(x)

# Desviación estándar: s = √(Σ(x-x̄)² / (n-1))
sd(x)

# Error estándar: SE = s / √n
sd(x) / sqrt(length(x))

# Intervalo de confianza 95%: x̄ ± 1.96 * SE
media <- mean(x)
se <- sd(x) / sqrt(length(x))
ic_inferior <- media - 1.96 * se
ic_superior <- media + 1.96 * se

# Tamaño del efecto (Cohen's d)
d <- (mean(grupo1) - mean(grupo2)) / sd(c(grupo1, grupo2))

# Correlación de Pearson: r = Σ((x-x̄)(y-ȳ)) / √(Σ(x-x̄)² * Σ(y-ȳ)²)
cor(x, y)

# ===============================================================================
# 🎓 19. CONCEPTOS ESTADÍSTICOS CLAVE
# ===============================================================================

# Hipótesis nula (H0): No hay diferencia/efecto
# Hipótesis alternativa (H1): Sí hay diferencia/efecto

# Error Tipo I (α): Rechazar H0 cuando es verdadera (falso positivo)
#                   Usualmente α = 0.05

# Error Tipo II (β): No rechazar H0 cuando es falsa (falso negativo)
#                     Potencia = 1 - β (usualmente 0.80)

# FDR (False Discovery Rate): Proporción de falsos positivos esperada
#                             Se controla con Benjamini-Hochberg

# Potencia estadística: Probabilidad de detectar un efecto si existe
#                       Depende de: tamaño muestral, tamaño del efecto, α

# ===============================================================================
# 📚 20. RECURSOS ADICIONALES
# ===============================================================================

# Ayuda en R
?funcion                               # Ayuda de función
??tema                                 # Buscar en toda la documentación
example(funcion)                       # Ver ejemplos

# Cheat sheets oficiales:
# - dplyr: https://dplyr.tidyverse.org/
# - ggplot2: https://ggplot2.tidyverse.org/
# - RStudio: Help > Cheatsheets

# ═══════════════════════════════════════════════════════════════════════════════
# FIN DE LA CHULETA
# ═══════════════════════════════════════════════════════════════════════════════
