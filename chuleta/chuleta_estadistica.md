# 📚 Chuleta de Estadística en R (secciones clave)

Guía práctica y rápida basada en `Chuleta/chuleta_estadistica.R`, con ejemplos mínimos que puedes copiar/pegar. Todas las secciones usan datos simulados para que los resultados sean reproducibles.

## Requisitos

```r
# Instalar (solo una vez)
# install.packages(c("dplyr", "ggplot2", "viridis", "pwr"))

# Cargar librerías
library(dplyr)
library(ggplot2)
library(viridis)
library(pwr)

set.seed(123)
```

---

## 📊 Tests paramétricos

### t.test(..., paired = TRUE) — antes/después

```r
n <- 30
antes  <- rnorm(n, mean = 100, sd = 10)
despues <- antes - 5 + rnorm(n, mean = 0, sd = 5)  # mejora ≈ -5
t.test(despues, antes, paired = TRUE)
```

Pistas:
- Úsalo cuando son las mismas unidades medidas dos veces (o emparejadas).
- Revisa normalidad de las diferencias si quieres ser estricto.

### t.test(var ~ grupo) — dos grupos independientes

```r
grupo <- factor(rep(c("A", "B"), each = 35))
y <- c(rnorm(35, 10, 2), rnorm(35, 12, 2))
t.test(y ~ grupo)  # Welch por defecto (varianzas desiguales)
```

Pistas:
- Si asumes varianzas iguales: usa `t.test(y ~ grupo, var.equal = TRUE)`.

### aov() + TukeyHSD() — ANOVA + comparaciones múltiples

```r
grupo3 <- factor(rep(c("G1", "G2", "G3"), each = 40))
y3 <- c(rnorm(40, 50, 8), rnorm(40, 55, 8), rnorm(40, 60, 8))
fit_aov <- aov(y3 ~ grupo3)
summary(fit_aov)
TukeyHSD(fit_aov)
```

Pistas:
- ANOVA contrasta medias globalmente; si es significativo, usa TukeyHSD para pares.

### lm() — regresión lineal múltiple

```r
N <- 120
x1 <- rnorm(N, 10, 2)
x2 <- rnorm(N, 5, 1)
x3 <- rnorm(N, 15, 3)
y  <- 5 + 2*x1 - 1.5*x2 + 0.8*x3 + rnorm(N, 0, 4)
fit_lm <- lm(y ~ x1 + x2 + x3)
summary(fit_lm)
```

### lm(y ~ x1*x2) — regresión con interacción

```r
fit_int <- lm(y ~ x1 * x2)  # incluye x1, x2 y su interacción x1:x2
summary(fit_int)
```

Pistas:
- Un coeficiente significativo en `x1:x2` sugiere que el efecto de x1 depende del nivel de x2.

---

## 📈 Tests no paramétricos

### wilcox.test() — alternativa a t-test (dos grupos)

```r
g <- factor(rep(c("A", "B"), each = 25))
z <- c(rnorm(25, 0, 1), rnorm(25, 0.6, 1))
wilcox.test(z ~ g)  # Mann–Whitney U / Wilcoxon rank-sum
```

### kruskal.test() — alternativa a ANOVA

```r
gk <- factor(rep(c("G1", "G2", "G3"), each = 20))
zk <- c(rnorm(20, 1, 1), rnorm(20, 1.4, 1), rnorm(20, 1.8, 1))
kruskal.test(zk ~ gk)
```

Pistas:
- Útiles cuando no se cumple normalidad/homocedasticidad o hay outliers fuertes.

---

## 🔗 Tests de asociación

### chisq.test() — independencia en tablas

```r
tab <- matrix(c(20, 30,
								25, 45), nrow = 2, byrow = TRUE)
dimnames(tab) <- list(Tratamiento = c("A", "B"),
											Efectos = c("Sí", "No"))
tab
chisq.test(tab)
```

Pistas:
- Esperados > 5 en la mayoría de celdas. Si no, usa Fisher.

### fisher.test() — exacto (frecuencias pequeñas, 2x2)

```r
fisher.test(tab)
```

Teoría y uso de alternative:
- H0: OR = 1 (independencia). H1 depende de `alternative`.
- `"two.sided"`: H1 es OR ≠ 1 (cualquier dirección).
- `"greater"`: H1 es OR > 1, es decir, la primera FILA tiene mayor odds del evento de la PRIMERA COLUMNA que la segunda fila.
- `"less"`: H1 es OR < 1 (odds menor en la primera fila).
- Define el “evento” como la primera columna; si tu evento es “No”, invierte columnas o ajusta la dirección.

Ejemplo completo con tabla nombrada y OR:

```r
# Filas: Grupo = Tratamiento, Control | Columnas: Efectos = Sí, No
tab <- matrix(c(20, 10,
								15, 25), nrow = 2, byrow = TRUE,
							dimnames = list(
								Grupo = c("Tratamiento", "Control"),
								Efectos = c("Sí", "No")
							))

# Comprobar esperados para decidir Fisher vs Chi-cuadrado
chisq.test(tab)$expected

# Fisher bilateral y unilateral (direccional)
f2 <- fisher.test(tab, alternative = "two.sided")
f1 <- fisher.test(tab, alternative = "greater")  # ¿odds de "Sí" mayor en Tratamiento?

# Extraer OR e IC
f2$estimate    # odds ratio
f2$conf.int    # intervalo de confianza del OR
f2$p.value
```

Decidir rápidamente Fisher vs Chi-cuadrado:
- Usa Fisher (exacto) si alguna frecuencia esperada < 5 o si >20% de las celdas tienen esperados < 5.
- Chi-cuadrado es adecuado si todas las celdas tienen esperados ≥ 5 y n es moderado/grande.
- Si solo te importa una dirección (p. ej., mayor riesgo en Tratamiento), usa `alternative = "greater"` (unilateral).

Frase de reporte (plantilla):

```r
# Bilateral
ft2 <- fisher.test(tab, alternative = "two.sided")
sprintf(
	"Test exacto de Fisher (bilateral): OR=%.2f, IC95%% [%.2f, %.2f], p=%.4f",
	as.numeric(ft2$estimate), ft2$conf.int[1], ft2$conf.int[2], ft2$p.value
)

# Unilateral (greater = odds del evento mayores en la PRIMERA fila)
ftg <- fisher.test(tab, alternative = "greater")
sprintf(
	"Fisher (unilateral, greater): OR=%.2f, p=%.4f",
	as.numeric(ftg$estimate), ftg$p.value
)
```

Nota: Si tu “evento” es la segunda categoría (p. ej., "No") o el grupo de referencia debe ser la segunda fila, reordena columnas/filas antes de probar (por ejemplo, `tab <- tab[, c(2,1)]` o `tab <- tab[c(2,1), ]`).

### cor.test() — correlación de Pearson y Spearman

```r
x <- rnorm(50)
y <- 0.6*x + rnorm(50, 0, 0.8)
cor.test(x, y, method = "pearson")   # lineal, normalidad aproximada
cor.test(x, y, method = "spearman")  # monotónica, robusta a outliers
```

---

## 🔬 Análisis multivariado (PCA)

```r
df <- data.frame(
	V1 = rnorm(100, 10, 2),
	V2 = rnorm(100, 5, 1),
	V3 = rnorm(100, 15, 3)
)
pca <- prcomp(df, scale. = TRUE, center = TRUE)

# Varianza explicada
summary(pca)$importance[2, ]  # proporción por PC

# Loadings (contribuciones de variables)
pca$rotation
```

Pistas:
- Escalar y centrar es clave cuando las variables están en distintas unidades.

---

## ⚡ Análisis de potencia (pwr)

```r
# t-test de dos muestras: tamaño por grupo para potencia 0.8 y d = 0.5
pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05, type = "two.sample")

# Correlación: potencia para r = 0.3 con n = 80
pwr.r.test(r = 0.3, n = 80, sig.level = 0.05)

# ANOVA (k grupos) con tamaño de efecto f de Cohen (≈ sqrt(eta2/(1-eta2)))
pwr.anova.test(k = 3, f = 0.25, power = 0.80, sig.level = 0.05)
```

Pistas:
- Efectos aproximados: d = 0.2 (pequeño), 0.5 (medio), 0.8 (grande); f = 0.10, 0.25, 0.40.

---

## 📊 Visualización avanzada (ggplot2)

### Boxplot — distribuciones por grupo

```r
df_box <- data.frame(
	grupo = factor(rep(c("A", "B", "C"), each = 50)),
	valor = c(rnorm(50, 5, 1), rnorm(50, 6, 1), rnorm(50, 7, 1.2))
)
ggplot(df_box, aes(grupo, valor, fill = grupo)) +
	geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
	scale_fill_viridis_d() +
	theme_minimal()
```

### Línea + ribbon — evolución temporal con IC

```r
df_time <- expand.grid(
	semana = 0:6,
	trat = c("A", "B")
) %>%
	mutate(valor = 100 - 3*semana + ifelse(trat == "A", -2, 0) + rnorm(n(), 0, 3))

sum_time <- df_time %>%
	group_by(trat, semana) %>%
	summarise(media = mean(valor), se = sd(valor)/sqrt(n()), .groups = "drop") %>%
	mutate(ic_inf = media - 1.96*se, ic_sup = media + 1.96*se)

ggplot(sum_time, aes(semana, media, color = trat, group = trat)) +
	geom_line(size = 1.2) +
	geom_point(size = 2.5) +
	geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup, fill = trat), alpha = 0.2, linetype = 0) +
	scale_color_viridis_d() +
	scale_fill_viridis_d() +
	theme_minimal() +
	labs(y = "Valor", color = "Tratamiento", fill = "Tratamiento")
```

### stat_ellipse — elipses de confianza (2D)

```r
df_scatter <- data.frame(
	x = rnorm(150),
	y = rnorm(150),
	grupo = rep(c("A", "B", "C"), each = 50)
)
ggplot(df_scatter, aes(x, y, color = grupo)) +
	geom_point(alpha = 0.6) +
	stat_ellipse(type = "t", alpha = 0.2, geom = "polygon", aes(fill = grupo), color = NA) +
	scale_color_viridis_d() +
	scale_fill_viridis_d() +
	theme_minimal()
```

### facet_wrap — paneles múltiples

```r
ggplot(df_box, aes(grupo, valor, fill = grupo)) +
	geom_violin(alpha = 0.6, color = NA) +
	facet_wrap(~ grupo, nrow = 1) +
	scale_fill_viridis_d() +
	theme_minimal()
```

---

Sugerencia: Si quieres una versión ejecutable con salidas formateadas, copia estos bloques en `Chuleta/chuleta_estadistica.Rmd` o ejecuta directamente en la consola R por secciones.

---

## 🧩 Problemas propuestos (sin solución) y flujo de trabajo

Estos casos prácticos te guían sobre QUÉ pasos seguir y QUÉ test aplicar según el caso. No incluyen respuestas ni código final; usa la chuleta previa para elegir funciones y gráficos.

Guía rápida para elegir test:
- ¿Respuesta numérica o categórica?
	- Numérica, 1 grupo medido 2 veces → t pareado si diferencias ~ normales; si no, Wilcoxon pareado.
	- Numérica, 2 grupos independientes → t de Welch (por defecto). Si normales y varianzas ≈ iguales → t clásico; si no normales → Wilcoxon (Mann–Whitney).
	- Numérica, ≥3 grupos → ANOVA si normales y homocedasticidad; si no → Kruskal–Wallis.
	- Categórica (2×2) → Chi-cuadrado si esperados ≥ 5; si no → Fisher exacto. Para hipótesis direccional, Fisher unilateral.
- Supuestos: Normalidad (Shapiro/QQ-plot sobre residuales o diferencias), Homocedasticidad (Levene/Bartlett; Levene es más robusta).
- Ajustes por múltiples comparaciones: Tukey para ANOVA; BH/Holm para múltiples tests.

### Problema 1: Antes vs después (mejora de puntuación)
- Objetivo: comprobar si una intervención mejora una métrica en los mismos sujetos.
- Flujo de trabajo:
	- Verifica emparejamiento correcto (mismo id antes/después).
	- Explora diferencias (después − antes): histograma/QQ plot; revisa outliers.
	- Test a aplicar: si diferencias ~ normales → t pareado; si no → Wilcoxon pareado.
	- Reporta: media de la diferencia, IC95%, p-valor y tamaño del efecto (Cohen’s d para pareados o r de efecto en Wilcoxon).

### Problema 2: Comparación de dos grupos independientes
- Objetivo: ver si dos tratamientos difieren en una variable continua.
- Flujo de trabajo:
	- Explora con boxplots y resumen por grupo.
	- Revisa normalidad por grupo (Shapiro/QQ) y homocedasticidad (Levene). Si dudas, usa Welch por defecto.
	- Test a aplicar:
		- Normales y varianzas ≈ iguales → t-test clásico (var.equal=TRUE).
		- Normales pero varianzas distintas → t de Welch (por defecto).
		- No normales/outliers severos → Wilcoxon (Mann–Whitney).
	- Reporta: diferencia de medias/medianas, IC95%, p-valor, tamaño de efecto (Cohen’s d) y potencia/ tamaño muestral si aplica.

### Problema 3: Tres o más grupos
- Objetivo: contrastar medias entre ≥3 grupos.
- Flujo de trabajo:
	- Explora con boxplots y medias ± IC.
	- Revisa normalidad de residuales y homocedasticidad (Levene).
	- Test a aplicar: ANOVA si supuestos razonables; si no → Kruskal–Wallis.
	- Post-hoc: Tukey (ANOVA) o Wilcoxon pareado por pares con ajuste BH/Holm.
	- Reporta: F/χ², p global, pares significativos y tamaño del efecto (η² parcial o epsilon²).

### Problema 4: Asociación en tabla 2×2 (efectos vs tratamiento)
- Objetivo: evaluar si la tasa de evento difiere entre Tratamiento y Control.
- Flujo de trabajo:
	- Construye tabla con nombres claros (filas = grupos; columnas = evento Sí/No).
	- Revisa esperados: si alguna celda esperada < 5 (o >20% de celdas < 5) → Fisher; si no → Chi-cuadrado.
	- Test a aplicar: Fisher (two.sided) por defecto; usa unilateral (greater/less) si hay hipótesis direccional previa y la primera columna es tu “evento”.
	- Reporta: OR, IC95%, p-valor y breve interpretación de la dirección del efecto.

### Problema 5: Correlación y regresión lineal simple
- Objetivo: cuantificar relación entre dos variables continuas y modelar Y ~ X.
- Flujo de trabajo:
	- Scatter plot con línea de tendencia; revisar linealidad y atípicos.
	- Test a aplicar: Pearson si relación ~ lineal y sin outliers fuertes; Spearman si relación monotónica/no lineal o con outliers.
	- Ajusta lm(Y ~ X); revisa residuos (normalidad, homocedasticidad, influencia).
	- Reporta: β1, IC95%, R² (y R² ajustado), diagnóstico de supuestos y predicciones con intervalos.

### Problema 6: Regresión lineal múltiple con interacción
- Objetivo: modelar Y con múltiples predictores e interacción X1*X2.
- Flujo de trabajo:
	- Estandariza si escalas muy distintas; revisa colinealidad (VIF).
	- Ajusta lm(Y ~ X1 + X2 + ... + X1:X2).
	- Revisa significancia e interpretación de la interacción.
	- Diagnóstico de residuos; reporta coeficientes, IC95%, R² adj., VIF y hallazgos clave.

### Problema 7: PCA para reducir dimensionalidad
- Objetivo: resumir 4–8 variables numéricas en pocos componentes.
- Flujo de trabajo:
	- Escala y centra; examina correlaciones altas.
	- Ejecuta PCA; revisa varianza explicada acumulada.
	- Inspecciona loadings para interpretar PC1/PC2; si hay grupos, visualiza con elipses.
	- Reporta: % varianza por PC, principales variables por componente y patrón observado.

### Problema 8: Evolución temporal por tratamiento
- Objetivo: comparar trayectorias medias de un indicador por semanas y tratamiento.
- Flujo de trabajo:
	- Agrega por grupo×tiempo: media, SE e IC95% (media ±1.96·SE).
	- Grafica líneas con ribbon de IC; evalúa diferencias en tendencias.
	- Inferencia opcional:
		- Comparación 0 vs final por tratamiento: t pareado o Wilcoxon pareado.
		- Comparar reducciones entre tratamientos: ANOVA/Kruskal–Wallis + post-hoc.
		- Avanzado (no incluido en esta chuleta): modelos mixtos para datos longitudinales.
	- Reporta: tendencia por grupo, intervalos y comparaciones clave.

### Problema 9: Múltiples tests y control de FDR
- Objetivo: evaluar varios indicadores a la vez controlando falsos positivos.
- Flujo de trabajo:
	- Ejecuta el mismo test sobre cada variable/feature.
	- Ajusta p-values (BH recomendado) y clasifica descubrimientos.
	- Visualiza volcanos/heatmaps si procede.
	- Reporta: número de significativos tras ajuste y los principales efectos.

### Problema 10: Análisis de potencia
- Objetivo: determinar n por grupo para detectar un efecto esperado.
- Flujo de trabajo:
	- Define tamaño de efecto (Cohen’s d/f o r) con base en evidencia previa.
	- Fija α y potencia objetivo (típico 0.8).
	- Calcula n con pwr.t.test / pwr.anova.test / pwr.r.test.
	- Reporta: tamaño del efecto asumido, n requerido y sensibilidad del diseño.


