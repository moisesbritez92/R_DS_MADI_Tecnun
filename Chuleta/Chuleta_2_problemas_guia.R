# ═══════════════════════════════════════════════════════════════════════════════
# 📚 CHULETA DE PROBLEMAS GUÍA - ANÁLISIS ESTADÍSTICO EN R
# ═══════════════════════════════════════════════════════════════════════════════
# Problemas tipo examen con procedimientos paso a paso
# ═══════════════════════════════════════════════════════════════════════════════

# ===============================================================================
# 🏥 PROBLEMA 1: EFICACIA DE TRATAMIENTOS MÉDICOS
# ===============================================================================

# --- CONTEXTO ---
# Un hospital ha implementado un nuevo tratamiento para reducir la presión arterial.
# Se midió la presión arterial de 80 pacientes antes y después del tratamiento.
# También se registró el sexo (M/F), edad y el hospital de origen (A, B, C).

# --- PREGUNTAS TÍPICAS ---
# 1.1 ¿El tratamiento reduce significativamente la presión arterial?
# 1.2 ¿El efecto del tratamiento es diferente entre hombres y mujeres?
# 1.3 ¿Hay correlación entre la edad y la reducción de presión arterial?
# 1.4 ¿El hospital de origen influye en la eficacia del tratamiento?
# 1.5 Visualizar la distribución de presiones antes/después por sexo

# --- PROCEDIMIENTO ---

# 1.1 Test t pareado (antes vs después)
# --------------------------------------
# H0: No hay diferencia entre antes y después
# H1: Hay diferencia (el tratamiento tiene efecto)

# Simular datos
set.seed(123)
pacientes <- data.frame(
  id = 1:80,
  sexo = sample(c("M", "F"), 80, replace = TRUE),
  edad = sample(30:70, 80, replace = TRUE),
  hospital = sample(c("A", "B", "C"), 80, replace = TRUE),
  presion_antes = rnorm(80, mean = 140, sd = 15),
  presion_despues = rnorm(80, mean = 130, sd = 15)
)

# Test t pareado
test_tratamiento <- t.test(pacientes$presion_antes, 
                           pacientes$presion_despues, 
                           paired = TRUE)
print(test_tratamiento)

# Interpretación:
# Si p < 0.05 → El tratamiento SÍ tiene efecto significativo

# 1.2 Comparar efecto por sexo
# --------------------------------------
# Calcular reducción de presión
pacientes$reduccion <- pacientes$presion_antes - pacientes$presion_despues

# Test t de dos muestras independientes
test_sexo <- t.test(reduccion ~ sexo, data = pacientes)
print(test_sexo)

# Interpretación:
# Si p < 0.05 → El efecto del tratamiento ES diferente entre sexos

# 1.3 Correlación edad vs reducción
# --------------------------------------
cor_test <- cor.test(pacientes$edad, pacientes$reduccion)
print(cor_test)

# Interpretación:
# Si p < 0.05 Y r positivo → A mayor edad, mayor reducción
# Si p < 0.05 Y r negativo → A mayor edad, menor reducción
# Si p ≥ 0.05 → No hay correlación significativa

# 1.4 Efecto del hospital (ANOVA)
# --------------------------------------
anova_hospital <- aov(reduccion ~ hospital, data = pacientes)
print(summary(anova_hospital))

# Si es significativo, comparaciones post-hoc
TukeyHSD(anova_hospital)

# 1.5 Visualización
# --------------------------------------
library(ggplot2)
library(tidyr)

# Preparar datos en formato largo
pacientes_long <- pacientes %>%
  select(id, sexo, presion_antes, presion_despues) %>%
  pivot_longer(cols = c(presion_antes, presion_despues),
               names_to = "momento",
               values_to = "presion")

# Boxplot comparativo
ggplot(pacientes_long, aes(x = momento, y = presion, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Presión Arterial: Antes vs Después del Tratamiento",
       subtitle = "Comparación por sexo",
       x = "Momento",
       y = "Presión Arterial (mmHg)") +
  scale_fill_manual(values = c("M" = "steelblue", "F" = "coral")) +
  theme_minimal()

# --- TEST UTILIZADO ---
# ✓ t.test(..., paired = TRUE)          # Comparar antes/después
# ✓ t.test(valor ~ grupo)                # Comparar dos grupos
# ✓ cor.test()                           # Correlación
# ✓ aov() + TukeyHSD()                   # ANOVA + comparaciones múltiples

# ===============================================================================
# 🛒 PROBLEMA 2: SATISFACCIÓN DE CLIENTES EN SUPERMERCADOS
# ===============================================================================

# --- CONTEXTO ---
# Una cadena de supermercados tiene 5 tiendas (T1, T2, T3, T4, T5).
# Se encuestaron 200 clientes sobre su satisfacción (escala 1-10).
# También se preguntó: edad, gasto mensual, frecuencia de visita, 
# si recomendaría la tienda (Sí/No).

# --- PREGUNTAS TÍPICAS ---
# 2.1 ¿La satisfacción es diferente entre las 5 tiendas?
# 2.2 ¿Hay correlación entre gasto mensual y satisfacción?
# 2.3 ¿Los clientes que recomiendan tienen mayor satisfacción?
# 2.4 ¿Qué variables predicen mejor la satisfacción? (regresión múltiple)
# 2.5 ¿La proporción de clientes que recomiendan es mayor al 70%?
# 2.6 Visualizar satisfacción por tienda

# --- PROCEDIMIENTO ---

# Simular datos
set.seed(456)
clientes <- data.frame(
  cliente_id = 1:200,
  tienda = sample(paste0("T", 1:5), 200, replace = TRUE),
  satisfaccion = sample(1:10, 200, replace = TRUE),
  edad = sample(18:70, 200, replace = TRUE),
  gasto_mensual = rnorm(200, mean = 150, sd = 50),
  frecuencia = sample(c("Semanal", "Quincenal", "Mensual"), 200, replace = TRUE),
  recomienda = sample(c("Sí", "No"), 200, replace = TRUE, prob = c(0.75, 0.25))
)

# 2.1 ANOVA para comparar tiendas
# --------------------------------------
anova_tiendas <- aov(satisfaccion ~ tienda, data = clientes)
print(summary(anova_tiendas))

# Si p < 0.05, hacer comparaciones post-hoc
if(summary(anova_tiendas)[[1]][["Pr(>F)"]][1] < 0.05) {
  print(TukeyHSD(anova_tiendas))
}

# Alternativa no paramétrica (si no hay normalidad)
kruskal.test(satisfaccion ~ tienda, data = clientes)

# 2.2 Correlación gasto vs satisfacción
# --------------------------------------
cor_gasto <- cor.test(clientes$gasto_mensual, clientes$satisfaccion)
print(cor_gasto)

# Scatter plot
ggplot(clientes, aes(x = gasto_mensual, y = satisfaccion)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre Gasto Mensual y Satisfacción",
       x = "Gasto Mensual (€)",
       y = "Satisfacción (1-10)") +
  theme_minimal()

# 2.3 Comparar satisfacción según recomendación
# --------------------------------------
test_recomienda <- t.test(satisfaccion ~ recomienda, data = clientes)
print(test_recomienda)

# Boxplot
ggplot(clientes, aes(x = recomienda, y = satisfaccion, fill = recomienda)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Satisfacción según Recomendación",
       x = "¿Recomienda?",
       y = "Satisfacción (1-10)") +
  scale_fill_manual(values = c("Sí" = "green", "No" = "red")) +
  theme_minimal()

# 2.4 Regresión múltiple
# --------------------------------------
# Convertir variables categóricas a factores
clientes$tienda <- as.factor(clientes$tienda)
clientes$frecuencia <- as.factor(clientes$frecuencia)
clientes$recomienda <- as.factor(clientes$recomienda)

modelo <- lm(satisfaccion ~ edad + gasto_mensual + frecuencia + recomienda + tienda, 
             data = clientes)
print(summary(modelo))

# Interpretación:
# - Coeficientes: efecto de cada variable
# - P-values: significancia de cada predictor
# - R-squared: porcentaje de varianza explicada

# 2.5 Test binomial para proporción
# --------------------------------------
# H0: proporción = 0.70
n_recomienda <- sum(clientes$recomienda == "Sí")
n_total <- nrow(clientes)

test_proporcion <- binom.test(x = n_recomienda, n = n_total, p = 0.70)
print(test_proporcion)

cat("Proporción observada:", n_recomienda / n_total, "\n")

# 2.6 Visualización por tienda
# --------------------------------------
# Calcular estadísticas por tienda
stats_tienda <- clientes %>%
  group_by(tienda) %>%
  summarise(
    media = mean(satisfaccion),
    mediana = median(satisfaccion),
    desv_std = sd(satisfaccion),
    n = n(),
    .groups = "drop"
  )

ggplot(clientes, aes(x = tienda, y = satisfaccion, fill = tienda)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "Distribución de Satisfacción por Tienda",
       x = "Tienda",
       y = "Satisfacción (1-10)") +
  theme_minimal() +
  theme(legend.position = "none")

# --- TESTS UTILIZADOS ---
# ✓ aov() + TukeyHSD()                   # ANOVA + post-hoc
# ✓ kruskal.test()                       # ANOVA no paramétrica
# ✓ cor.test()                           # Correlación
# ✓ t.test(valor ~ grupo)                # Comparar dos grupos
# ✓ lm()                                 # Regresión múltiple
# ✓ binom.test()                         # Test de proporciones

# ===============================================================================
# 📱 PROBLEMA 3: TIEMPO DE USO DE APLICACIONES MÓVILES
# ===============================================================================

# --- CONTEXTO ---
# Una empresa de apps tiene datos de 300 usuarios durante 6 meses.
# Variables: usuario_id, mes, tiempo_uso (minutos/día), num_compras,
# categoria_app (Juegos, Social, Productividad), sistema_operativo (iOS/Android),
# edad_usuario, valoracion_app (1-5 estrellas).

# --- PREGUNTAS TÍPICAS ---
# 3.1 ¿El tiempo de uso varía entre categorías de apps?
# 3.2 ¿Hay diferencia en el número de compras entre iOS y Android?
# 3.3 ¿El tiempo de uso está correlacionado con la valoración?
# 3.4 ¿El tiempo de uso aumentó significativamente del mes 1 al mes 6?
# 3.5 Realizar PCA para resumir el comportamiento de usuarios
# 3.6 Visualizar evolución temporal del tiempo de uso por categoría

# --- PROCEDIMIENTO ---

# Simular datos
set.seed(789)
n_usuarios <- 50
meses <- 1:6

datos_app <- expand.grid(
  usuario_id = 1:n_usuarios,
  mes = meses
) %>%
  mutate(
    categoria = sample(c("Juegos", "Social", "Productividad"), n(), replace = TRUE),
    sistema = sample(c("iOS", "Android"), n(), replace = TRUE),
    edad = sample(18:50, n(), replace = TRUE),
    tiempo_uso = rnorm(n(), mean = 45 + mes * 2, sd = 15),
    num_compras = rpois(n(), lambda = 2),
    valoracion = sample(1:5, n(), replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.35, 0.3))
  )

# 3.1 Comparar tiempo de uso entre categorías
# --------------------------------------
anova_categoria <- aov(tiempo_uso ~ categoria, data = datos_app)
print(summary(anova_categoria))

# Post-hoc si es significativo
if(summary(anova_categoria)[[1]][["Pr(>F)"]][1] < 0.05) {
  print(TukeyHSD(anova_categoria))
}

# Visualización
ggplot(datos_app, aes(x = categoria, y = tiempo_uso, fill = categoria)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  labs(title = "Distribución del Tiempo de Uso por Categoría de App",
       x = "Categoría",
       y = "Tiempo de Uso (min/día)") +
  theme_minimal()

# 3.2 Comparar compras iOS vs Android
# --------------------------------------
# Test de Wilcoxon (datos de conteo, no normales)
test_compras <- wilcox.test(num_compras ~ sistema, data = datos_app)
print(test_compras)

# Alternativa paramétrica
test_compras_t <- t.test(num_compras ~ sistema, data = datos_app)
print(test_compras_t)

# Resumen por sistema
datos_app %>%
  group_by(sistema) %>%
  summarise(
    media_compras = mean(num_compras),
    mediana_compras = median(num_compras),
    total_compras = sum(num_compras),
    .groups = "drop"
  )

# 3.3 Correlación tiempo de uso vs valoración
# --------------------------------------
cor_valoracion <- cor.test(datos_app$tiempo_uso, datos_app$valoracion)
print(cor_valoracion)

# Spearman (no paramétrica, para datos ordinales como valoraciones)
cor_valoracion_spearman <- cor.test(datos_app$tiempo_uso, datos_app$valoracion, 
                                    method = "spearman")
print(cor_valoracion_spearman)

# Scatter plot
ggplot(datos_app, aes(x = tiempo_uso, y = valoracion)) +
  geom_jitter(alpha = 0.3, width = 0, height = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre Tiempo de Uso y Valoración",
       x = "Tiempo de Uso (min/día)",
       y = "Valoración (1-5 estrellas)") +
  theme_minimal()

# 3.4 Comparar mes 1 vs mes 6 (datos pareados)
# --------------------------------------
# Filtrar datos del mes 1 y mes 6
datos_mes1 <- datos_app %>% filter(mes == 1)
datos_mes6 <- datos_app %>% filter(mes == 6)

# Asegurar mismo orden de usuarios
datos_mes1 <- datos_mes1 %>% arrange(usuario_id)
datos_mes6 <- datos_mes6 %>% arrange(usuario_id)

# Test t pareado
test_temporal <- t.test(datos_mes1$tiempo_uso, datos_mes6$tiempo_uso, paired = TRUE)
print(test_temporal)

cat("Media mes 1:", mean(datos_mes1$tiempo_uso), "minutos\n")
cat("Media mes 6:", mean(datos_mes6$tiempo_uso), "minutos\n")
cat("Incremento:", mean(datos_mes6$tiempo_uso) - mean(datos_mes1$tiempo_uso), "minutos\n")

# 3.5 PCA para comportamiento de usuarios
# --------------------------------------
# Preparar matriz: promedio por usuario de cada variable numérica
matriz_usuarios <- datos_app %>%
  group_by(usuario_id) %>%
  summarise(
    tiempo_uso_promedio = mean(tiempo_uso),
    num_compras_total = sum(num_compras),
    valoracion_promedio = mean(valoracion),
    edad = first(edad),
    .groups = "drop"
  )

# Extraer solo variables numéricas
matriz_pca <- matriz_usuarios %>%
  select(tiempo_uso_promedio, num_compras_total, valoracion_promedio, edad) %>%
  as.matrix()

# Realizar PCA
pca_usuarios <- prcomp(matriz_pca, scale. = TRUE, center = TRUE)

# Varianza explicada
varianza <- summary(pca_usuarios)$importance[2, 1:2] * 100
cat("PC1 explica:", round(varianza[1], 2), "%\n")
cat("PC2 explica:", round(varianza[2], 2), "%\n")

# Crear dataframe para visualización
pca_df <- data.frame(
  PC1 = pca_usuarios$x[, 1],
  PC2 = pca_usuarios$x[, 2],
  usuario_id = matriz_usuarios$usuario_id
)

# Unir con categoría más frecuente del usuario
categoria_usuario <- datos_app %>%
  group_by(usuario_id) %>%
  summarise(categoria_freq = names(which.max(table(categoria))), .groups = "drop")

pca_df <- merge(pca_df, categoria_usuario, by = "usuario_id")

# Gráfico PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = categoria_freq)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(fill = categoria_freq), alpha = 0.2, geom = "polygon") +
  labs(
    title = "PCA - Comportamiento de Usuarios",
    subtitle = paste0("PC1: ", round(varianza[1], 1), "% | PC2: ", round(varianza[2], 1), "%"),
    x = paste0("PC1 (", round(varianza[1], 1), "%)"),
    y = paste0("PC2 (", round(varianza[2], 1), "%)"),
    color = "Categoría"
  ) +
  theme_minimal()

# Loadings (qué variables influyen más)
print("=== LOADINGS PC1 ===")
print(sort(abs(pca_usuarios$rotation[, 1]), decreasing = TRUE))

# 3.6 Evolución temporal por categoría
# --------------------------------------
evolucion <- datos_app %>%
  group_by(mes, categoria) %>%
  summarise(
    tiempo_promedio = mean(tiempo_uso),
    se = sd(tiempo_uso) / sqrt(n()),
    .groups = "drop"
  )

ggplot(evolucion, aes(x = mes, y = tiempo_promedio, color = categoria, group = categoria)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = tiempo_promedio - se, ymax = tiempo_promedio + se, fill = categoria),
              alpha = 0.2, linetype = 0) +
  labs(title = "Evolución del Tiempo de Uso por Categoría de App",
       subtitle = "Promedio mensual con error estándar",
       x = "Mes",
       y = "Tiempo de Uso Promedio (min/día)",
       color = "Categoría",
       fill = "Categoría") +
  scale_x_continuous(breaks = 1:6) +
  theme_minimal()

# --- TESTS UTILIZADOS ---
# ✓ aov() + TukeyHSD()                   # ANOVA + post-hoc
# ✓ wilcox.test()                        # Test no paramétrico (dos grupos)
# ✓ t.test(valor ~ grupo)                # Test t (dos grupos)
# ✓ cor.test(..., method = "spearman")   # Correlación no paramétrica
# ✓ t.test(..., paired = TRUE)           # Test t pareado
# ✓ prcomp()                             # PCA

# ===============================================================================
# 🎓 PROBLEMA 4: RENDIMIENTO ACADÉMICO EN UNIVERSIDADES
# ===============================================================================

# --- CONTEXTO ---
# Se tienen datos de 500 estudiantes de 4 universidades (U1, U2, U3, U4).
# Variables: nota_parcial1, nota_parcial2, nota_final, horas_estudio_semanal,
# asistencia (%), universidad, beca (Sí/No), trabajo (Sí/No).

# --- PREGUNTAS TÍPICAS ---
# 4.1 ¿El rendimiento (nota final) es diferente entre universidades?
# 4.2 ¿Los estudiantes con beca tienen mejor rendimiento?
# 4.3 ¿Hay correlación entre horas de estudio y nota final?
# 4.4 Realizar comparaciones múltiples entre universidades (corregir p-values)
# 4.5 ¿Qué variables predicen mejor la nota final? (regresión múltiple)
# 4.6 ¿La asistencia está relacionada con aprobar (nota ≥ 5)?
# 4.7 Análisis de potencia: ¿cuántos estudiantes necesito para detectar 
#     una diferencia de 0.5 puntos con 80% de potencia?

# --- PROCEDIMIENTO ---

# Simular datos
set.seed(101112)
estudiantes <- data.frame(
  estudiante_id = 1:500,
  universidad = sample(paste0("U", 1:4), 500, replace = TRUE),
  beca = sample(c("Sí", "No"), 500, replace = TRUE, prob = c(0.3, 0.7)),
  trabajo = sample(c("Sí", "No"), 500, replace = TRUE, prob = c(0.4, 0.6)),
  horas_estudio = rnorm(500, mean = 15, sd = 5),
  asistencia = rnorm(500, mean = 75, sd = 15)
) %>%
  mutate(
    nota_parcial1 = pmin(10, pmax(0, rnorm(500, mean = 6 + horas_estudio*0.1, sd = 1.5))),
    nota_parcial2 = pmin(10, pmax(0, rnorm(500, mean = 6 + horas_estudio*0.1, sd = 1.5))),
    nota_final = pmin(10, pmax(0, (nota_parcial1 + nota_parcial2) / 2 + 
                                     asistencia*0.01 + 
                                     ifelse(beca == "Sí", 0.5, 0)))
  )

# 4.1 ANOVA: rendimiento entre universidades
# --------------------------------------
anova_univ <- aov(nota_final ~ universidad, data = estudiantes)
print(summary(anova_univ))

# Visualización
ggplot(estudiantes, aes(x = universidad, y = nota_final, fill = universidad)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.5) +
  labs(title = "Distribución de Notas Finales por Universidad",
       x = "Universidad",
       y = "Nota Final (0-10)") +
  theme_minimal() +
  theme(legend.position = "none")

# 4.2 Comparar rendimiento con/sin beca
# --------------------------------------
test_beca <- t.test(nota_final ~ beca, data = estudiantes)
print(test_beca)

# Estadísticas descriptivas
estudiantes %>%
  group_by(beca) %>%
  summarise(
    n = n(),
    media = mean(nota_final),
    mediana = median(nota_final),
    desv_std = sd(nota_final),
    .groups = "drop"
  )

# Visualización
ggplot(estudiantes, aes(x = beca, y = nota_final, fill = beca)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.3, alpha = 0.5) +
  labs(title = "Rendimiento Académico según Beca",
       x = "¿Tiene Beca?",
       y = "Nota Final (0-10)") +
  scale_fill_manual(values = c("Sí" = "green", "No" = "coral")) +
  theme_minimal()

# 4.3 Correlación horas de estudio vs nota final
# --------------------------------------
cor_estudio <- cor.test(estudiantes$horas_estudio, estudiantes$nota_final)
print(cor_estudio)

cat("Coeficiente de correlación:", round(cor_estudio$estimate, 3), "\n")
cat("P-value:", format(cor_estudio$p.value, scientific = TRUE), "\n")

# Scatter plot
ggplot(estudiantes, aes(x = horas_estudio, y = nota_final)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre Horas de Estudio y Nota Final",
       subtitle = paste0("r = ", round(cor_estudio$estimate, 3), 
                        ", p = ", format(cor_estudio$p.value, scientific = TRUE)),
       x = "Horas de Estudio Semanal",
       y = "Nota Final (0-10)") +
  theme_minimal()

# 4.4 Comparaciones múltiples entre universidades
# --------------------------------------
# Primero hacer todas las comparaciones
universidades <- unique(estudiantes$universidad)
comparaciones <- expand.grid(U1 = universidades, U2 = universidades) %>%
  filter(as.character(U1) < as.character(U2))

resultados_comp <- data.frame(
  comparacion = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(comparaciones)) {
  u1 <- as.character(comparaciones$U1[i])
  u2 <- as.character(comparaciones$U2[i])
  
  datos_comp <- estudiantes %>% filter(universidad %in% c(u1, u2))
  test <- t.test(nota_final ~ universidad, data = datos_comp)
  
  resultados_comp <- rbind(resultados_comp, data.frame(
    comparacion = paste(u1, "vs", u2),
    p_value = test$p.value
  ))
}

# Ajustar p-values (Benjamini-Hochberg / FDR)
resultados_comp$p_adjusted_BH <- p.adjust(resultados_comp$p_value, method = "BH")
resultados_comp$p_adjusted_bonferroni <- p.adjust(resultados_comp$p_value, method = "bonferroni")

resultados_comp$significativo_BH <- resultados_comp$p_adjusted_BH < 0.05
resultados_comp$significativo_bonferroni <- resultados_comp$p_adjusted_bonferroni < 0.05

print("=== COMPARACIONES MÚLTIPLES ENTRE UNIVERSIDADES ===")
print(resultados_comp)

cat("\nNúmero de comparaciones significativas:\n")
cat("Con ajuste BH:", sum(resultados_comp$significativo_BH), "\n")
cat("Con ajuste Bonferroni:", sum(resultados_comp$significativo_bonferroni), "\n")

# Alternativa: usar TukeyHSD directamente
print("\n=== TUKEY HSD (Post-hoc) ===")
print(TukeyHSD(anova_univ))

# 4.5 Regresión múltiple
# --------------------------------------
modelo_completo <- lm(nota_final ~ horas_estudio + asistencia + beca + trabajo + universidad,
                      data = estudiantes)
print(summary(modelo_completo))

# Interpretar coeficientes
cat("\n=== INTERPRETACIÓN DE COEFICIENTES ===\n")
coef_summary <- summary(modelo_completo)$coefficients
print(coef_summary)

# Variables significativas (p < 0.05)
vars_significativas <- rownames(coef_summary)[coef_summary[, "Pr(>|t|)"] < 0.05]
cat("\nVariables significativas:", paste(vars_significativas, collapse = ", "), "\n")

# R-squared
cat("\nR-squared:", round(summary(modelo_completo)$r.squared, 4), "\n")
cat("Adj. R-squared:", round(summary(modelo_completo)$adj.r.squared, 4), "\n")

# 4.6 Relación asistencia con aprobar (chi-cuadrado / Fisher)
# --------------------------------------
estudiantes$aprobado <- ifelse(estudiantes$nota_final >= 5, "Aprobado", "Suspenso")

# Categorizar asistencia
estudiantes$asistencia_cat <- cut(estudiantes$asistencia, 
                                  breaks = c(0, 60, 80, 100),
                                  labels = c("Baja", "Media", "Alta"))

# Tabla de contingencia
tabla_asistencia <- table(estudiantes$asistencia_cat, estudiantes$aprobado)
print(tabla_asistencia)
print(prop.table(tabla_asistencia, margin = 1))

# Test chi-cuadrado
test_chi <- chisq.test(tabla_asistencia)
print(test_chi)

# Test de Fisher (alternativa)
test_fisher <- fisher.test(tabla_asistencia)
print(test_fisher)

# Visualización
prop_data <- as.data.frame(prop.table(tabla_asistencia, margin = 1))
colnames(prop_data) <- c("Asistencia", "Resultado", "Proporcion")

ggplot(prop_data, aes(x = Asistencia, y = Proporcion, fill = Resultado)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proporción de Aprobados según Asistencia",
       x = "Nivel de Asistencia",
       y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Aprobado" = "green", "Suspenso" = "red")) +
  theme_minimal()

# 4.7 Análisis de potencia
# --------------------------------------
library(pwr)

# ¿Cuántos estudiantes necesito para detectar una diferencia de 0.5 puntos
# con 80% de potencia? Asumiendo sd = 1.5

# Calcular Cohen's d (tamaño del efecto)
diferencia_minima <- 0.5
desviacion <- sd(estudiantes$nota_final)
cohens_d <- diferencia_minima / desviacion

cat("\n=== ANÁLISIS DE POTENCIA ===\n")
cat("Diferencia mínima a detectar:", diferencia_minima, "puntos\n")
cat("Desviación estándar:", round(desviacion, 2), "\n")
cat("Cohen's d:", round(cohens_d, 3), "\n")

# Test t de dos muestras
power_result <- pwr.t.test(d = cohens_d, 
                           sig.level = 0.05, 
                           power = 0.80, 
                           type = "two.sample")
print(power_result)

cat("\nNúmero de estudiantes necesarios por grupo:", ceiling(power_result$n), "\n")
cat("Total de estudiantes necesarios:", ceiling(power_result$n) * 2, "\n")

# Potencia actual con n = 500 (250 por grupo aprox)
power_actual <- pwr.t.test(n = 250, 
                           d = cohens_d, 
                           sig.level = 0.05, 
                           type = "two.sample")
cat("\nPotencia actual con n=250 por grupo:", round(power_actual$power, 3), "\n")

# --- TESTS UTILIZADOS ---
# ✓ aov()                                # ANOVA
# ✓ t.test(valor ~ grupo)                # Test t (dos grupos)
# ✓ cor.test()                           # Correlación
# ✓ p.adjust(..., method = "BH")         # Corrección comparaciones múltiples
# ✓ TukeyHSD()                           # Post-hoc ANOVA
# ✓ lm()                                 # Regresión múltiple
# ✓ chisq.test()                         # Chi-cuadrado
# ✓ fisher.test()                        # Test exacto de Fisher
# ✓ pwr.t.test()                         # Análisis de potencia

# ===============================================================================
# 🏭 PROBLEMA 5: CONTROL DE CALIDAD EN MANUFACTURA
# ===============================================================================

# --- CONTEXTO ---
# Una fábrica tiene 3 líneas de producción (L1, L2, L3).
# Se miden 400 piezas: dimensión (mm), peso (g), defectos (Sí/No), turno (Mañana/Tarde/Noche),
# operario (A, B, C, D, E), tiempo_produccion (segundos).

# --- PREGUNTAS TÍPICAS ---
# 5.1 ¿La dimensión media de las piezas es diferente entre líneas?
# 5.2 ¿El turno influye en la tasa de defectos?
# 5.3 ¿Hay correlación entre tiempo de producción y peso?
# 5.4 ¿El operario influye en el número de defectos?
# 5.5 ¿Las piezas de la línea L1 cumplen con la dimensión objetivo de 50mm?
# 5.6 PCA para identificar patrones en el proceso de producción

# --- PROCEDIMIENTO ---

# Simular datos
set.seed(131415)
piezas <- data.frame(
  pieza_id = 1:400,
  linea = sample(paste0("L", 1:3), 400, replace = TRUE),
  turno = sample(c("Mañana", "Tarde", "Noche"), 400, replace = TRUE),
  operario = sample(LETTERS[1:5], 400, replace = TRUE),
  dimension = rnorm(400, mean = 50, sd = 2),
  peso = rnorm(400, mean = 100, sd = 10),
  tiempo_produccion = rnorm(400, mean = 45, sd = 8)
) %>%
  mutate(
    defectos = ifelse(abs(dimension - 50) > 3 | abs(peso - 100) > 15, "Sí", "No")
  )

# 5.1 Comparar dimensión entre líneas
# --------------------------------------
anova_lineas <- aov(dimension ~ linea, data = piezas)
print(summary(anova_lineas))

# Post-hoc
TukeyHSD(anova_lineas)

# Verificar supuestos de normalidad
shapiro.test(residuals(anova_lineas)[1:5000])  # Shapiro max 5000 observaciones

# Test de homogeneidad de varianzas
bartlett.test(dimension ~ linea, data = piezas)

# Visualización
ggplot(piezas, aes(x = linea, y = dimension, fill = linea)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 3, y = 50.5, label = "Objetivo: 50mm", color = "red") +
  labs(title = "Dimensión de Piezas por Línea de Producción",
       subtitle = "Línea discontinua = dimensión objetivo",
       x = "Línea",
       y = "Dimensión (mm)") +
  theme_minimal()

# 5.2 Relación turno - defectos (chi-cuadrado)
# --------------------------------------
tabla_turno <- table(piezas$turno, piezas$defectos)
print(tabla_turno)

# Chi-cuadrado
test_turno <- chisq.test(tabla_turno)
print(test_turno)

# Proporciones
prop.table(tabla_turno, margin = 1)

# Visualización
prop_turno <- as.data.frame(prop.table(tabla_turno, margin = 1))
colnames(prop_turno) <- c("Turno", "Defecto", "Proporcion")

ggplot(prop_turno, aes(x = Turno, y = Proporcion, fill = Defecto)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tasa de Defectos por Turno",
       x = "Turno",
       y = "Proporción") +
  scale_fill_manual(values = c("Sí" = "red", "No" = "green")) +
  theme_minimal()

# 5.3 Correlación tiempo vs peso
# --------------------------------------
cor_tiempo <- cor.test(piezas$tiempo_produccion, piezas$peso)
print(cor_tiempo)

ggplot(piezas, aes(x = tiempo_produccion, y = peso)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre Tiempo de Producción y Peso",
       subtitle = paste0("r = ", round(cor_tiempo$estimate, 3), 
                        ", p = ", format(cor_tiempo$p.value, scientific = TRUE)),
       x = "Tiempo de Producción (seg)",
       y = "Peso (g)") +
  theme_minimal()

# 5.4 Operario vs defectos (chi-cuadrado)
# --------------------------------------
tabla_operario <- table(piezas$operario, piezas$defectos)
print(tabla_operario)

test_operario <- chisq.test(tabla_operario)
print(test_operario)

# Si hay diferencias, identificar qué operarios tienen más defectos
defectos_por_operario <- piezas %>%
  group_by(operario) %>%
  summarise(
    total = n(),
    defectos = sum(defectos == "Sí"),
    tasa_defectos = defectos / total,
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_defectos))

print(defectos_por_operario)

ggplot(defectos_por_operario, aes(x = reorder(operario, -tasa_defectos), 
                                   y = tasa_defectos, fill = operario)) +
  geom_bar(stat = "identity") +
  labs(title = "Tasa de Defectos por Operario",
       x = "Operario",
       y = "Tasa de Defectos") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "none")

# 5.5 Test t de una muestra (dimensión objetivo)
# --------------------------------------
# H0: media = 50mm
piezas_l1 <- piezas %>% filter(linea == "L1")

test_objetivo <- t.test(piezas_l1$dimension, mu = 50)
print(test_objetivo)

cat("\nMedia observada:", round(mean(piezas_l1$dimension), 2), "mm\n")
cat("Objetivo:", 50, "mm\n")
cat("Diferencia:", round(mean(piezas_l1$dimension) - 50, 2), "mm\n")
cat("P-value:", format(test_objetivo$p.value, scientific = TRUE), "\n")

if(test_objetivo$p.value < 0.05) {
  cat("\nCONCLUSIÓN: La dimensión media SÍ es significativamente diferente del objetivo.\n")
} else {
  cat("\nCONCLUSIÓN: La dimensión media NO es significativamente diferente del objetivo.\n")
}

# 5.6 PCA para patrones de producción
# --------------------------------------
# Preparar matriz (solo variables numéricas)
matriz_pca <- piezas %>%
  select(dimension, peso, tiempo_produccion) %>%
  as.matrix()

pca_produccion <- prcomp(matriz_pca, scale. = TRUE, center = TRUE)

# Varianza explicada
varianza <- summary(pca_produccion)$importance[2, 1:2] * 100
cat("\n=== PCA PROCESO DE PRODUCCIÓN ===\n")
cat("PC1 explica:", round(varianza[1], 2), "%\n")
cat("PC2 explica:", round(varianza[2], 2), "%\n")

# Loadings
print("\n=== LOADINGS PC1 ===")
print(pca_produccion$rotation[, 1])

print("\n=== LOADINGS PC2 ===")
print(pca_produccion$rotation[, 2])

# Visualización PCA coloreado por defectos
pca_df <- data.frame(
  PC1 = pca_produccion$x[, 1],
  PC2 = pca_produccion$x[, 2],
  defectos = piezas$defectos,
  linea = piezas$linea
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = defectos, shape = linea)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(fill = defectos), alpha = 0.1, geom = "polygon") +
  labs(
    title = "PCA - Proceso de Producción",
    subtitle = paste0("PC1: ", round(varianza[1], 1), "% | PC2: ", round(varianza[2], 1), "%"),
    x = paste0("PC1 (", round(varianza[1], 1), "%)"),
    y = paste0("PC2 (", round(varianza[2], 1), "%)"),
    color = "Defectos",
    shape = "Línea"
  ) +
  scale_color_manual(values = c("Sí" = "red", "No" = "green")) +
  theme_minimal()

# --- TESTS UTILIZADOS ---
# ✓ aov() + TukeyHSD()                   # ANOVA + post-hoc
# ✓ shapiro.test()                       # Test de normalidad
# ✓ bartlett.test()                      # Homogeneidad de varianzas
# ✓ chisq.test()                         # Chi-cuadrado (independencia)
# ✓ cor.test()                           # Correlación
# ✓ t.test(x, mu = valor)                # Test t de una muestra
# ✓ prcomp()                             # PCA

# ===============================================================================
# 📋 RESUMEN DE TESTS Y CUÁNDO USARLOS
# ===============================================================================

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📋 RESUMEN RÁPIDO: ¿QUÉ TEST USAR?\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("1️⃣ COMPARAR DOS GRUPOS (variable numérica)\n")
cat("   • t.test(valor ~ grupo)              - Dos grupos independientes\n")
cat("   • t.test(x, y, paired = TRUE)        - Dos grupos pareados (antes/después)\n")
cat("   • wilcox.test()                      - Alternativa no paramétrica\n\n")

cat("2️⃣ COMPARAR MÁS DE DOS GRUPOS (variable numérica)\n")
cat("   • aov() + TukeyHSD()                 - ANOVA + comparaciones post-hoc\n")
cat("   • kruskal.test()                     - Alternativa no paramétrica\n\n")

cat("3️⃣ CORRELACIÓN ENTRE DOS VARIABLES NUMÉRICAS\n")
cat("   • cor.test()                         - Correlación de Pearson\n")
cat("   • cor.test(..., method='spearman')   - Correlación no paramétrica\n\n")

cat("4️⃣ RELACIÓN ENTRE DOS VARIABLES CATEGÓRICAS\n")
cat("   • chisq.test()                       - Chi-cuadrado (muestras grandes)\n")
cat("   • fisher.test()                      - Test exacto (muestras pequeñas)\n\n")

cat("5️⃣ COMPARAR CON UN VALOR OBJETIVO\n")
cat("   • t.test(x, mu = objetivo)           - Media vs valor objetivo\n")
cat("   • binom.test(x, n, p)                - Proporción vs probabilidad esperada\n\n")

cat("6️⃣ PREDECIR VARIABLE NUMÉRICA\n")
cat("   • lm(y ~ x1 + x2 + ...)              - Regresión lineal múltiple\n\n")

cat("7️⃣ REDUCIR DIMENSIONALIDAD\n")
cat("   • prcomp(matriz, scale=T, center=T)  - PCA (Análisis Componentes Principales)\n\n")

cat("8️⃣ CORRECCIÓN POR COMPARACIONES MÚLTIPLES\n")
cat("   • p.adjust(..., method='BH')         - Benjamini-Hochberg (recomendado)\n")
cat("   • p.adjust(..., method='bonferroni') - Bonferroni (muy conservador)\n\n")

cat("9️⃣ ANÁLISIS DE POTENCIA\n")
cat("   • pwr.t.test()                       - Potencia para test t\n")
cat("   • pwr.r.test()                       - Potencia para correlación\n")
cat("   • pwr.anova.test()                   - Potencia para ANOVA\n\n")

cat("🔟 VERIFICAR SUPUESTOS\n")
cat("   • shapiro.test()                     - Normalidad\n")
cat("   • bartlett.test()                    - Homogeneidad de varianzas\n")
cat("   • leveneTest()                       - Homogeneidad (no asume normalidad)\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("📊 FLUJO DE DECISIÓN\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("¿Qué quiero hacer?\n\n")
cat("├─ Comparar grupos\n")
cat("│  ├─ 2 grupos → t.test() o wilcox.test()\n")
cat("│  └─ 3+ grupos → aov() + TukeyHSD() o kruskal.test()\n")
cat("│\n")
cat("├─ Relación entre variables\n")
cat("│  ├─ Ambas numéricas → cor.test()\n")
cat("│  ├─ Ambas categóricas → chisq.test() o fisher.test()\n")
cat("│  └─ Mix num+cat → t.test() o aov()\n")
cat("│\n")
cat("├─ Predecir/explicar\n")
cat("│  └─ Variable numérica → lm()\n")
cat("│\n")
cat("├─ Resumir/visualizar muchas variables\n")
cat("│  └─ PCA → prcomp()\n")
cat("│\n")
cat("└─ Múltiples tests\n")
cat("   └─ Siempre → p.adjust(method='BH')\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("✅ CHECKLIST ANTES DE ELEGIR TEST\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("1. ¿Qué tipo son mis variables?\n")
cat("   □ Numérica continua (ej: peso, temperatura)\n")
cat("   □ Numérica discreta (ej: conteos)\n")
cat("   □ Categórica nominal (ej: colores, países)\n")
cat("   □ Categórica ordinal (ej: escala 1-5)\n\n")

cat("2. ¿Cuántos grupos estoy comparando?\n")
cat("   □ 2 grupos\n")
cat("   □ 3 o más grupos\n")
cat("   □ No comparo grupos (correlación)\n\n")

cat("3. ¿Los datos son normales?\n")
cat("   □ Sí → Test paramétricos (t.test, aov)\n")
cat("   □ No → Test no paramétricos (wilcox, kruskal)\n")
cat("   □ No sé → Hacer shapiro.test()\n\n")

cat("4. ¿Son datos pareados?\n")
cat("   □ Sí (antes/después del mismo sujeto) → paired=TRUE\n")
cat("   □ No (grupos independientes) → paired=FALSE\n\n")

cat("5. ¿Hago múltiples comparaciones?\n")
cat("   □ Sí → SIEMPRE ajustar p-values con p.adjust()\n")
cat("   □ No → Usar p-value directo\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("🎯 FIN DE LA CHULETA DE PROBLEMAS GUÍA\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
