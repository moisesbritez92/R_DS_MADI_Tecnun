# RESUMEN COMPLETO DE DPLYR ----
# Herramientas para Manipulación de Datos

# dplyr es parte del tidyverse y proporciona una gramática coherente para
# manipular datos en R. Es rápido, intuitivo y diseñado para trabajar con
# data frames y tibbles.

library(dplyr)
library(tidyverse)  # Incluye dplyr y otras herramientas útiles

# 1. FUNCIONES BÁSICAS (VERBOS PRINCIPALES) ----

## 1.1 SELECT() - Seleccionar columnas ----
# Permite elegir qué columnas mantener en el dataset

# Ejemplos básicos:
mtcars %>% select(mpg, cyl, hp)           # Seleccionar columnas específicas
mtcars %>% select(1:3)                     # Seleccionar por posición
mtcars %>% select(-mpg)                    # Excluir columnas
mtcars %>% select(starts_with("m"))        # Columnas que empiezan con "m"
mtcars %>% select(ends_with("p"))          # Columnas que terminan con "p"
mtcars %>% select(contains("a"))           # Columnas que contienen "a"
mtcars %>% select(matches("^[md]"))        # Usar expresiones regulares
mtcars %>% select(where(is.numeric))       # Seleccionar por tipo de dato
mtcars %>% select(everything())            # Todas las columnas (útil para reordenar)
mtcars %>% select(mpg, everything())       # Mover mpg al principio

# Funciones auxiliares para select():
# - starts_with("abc"): nombres que empiezan con "abc"
# - ends_with("xyz"): nombres que terminan con "xyz"
# - contains("ijk"): nombres que contienen "ijk"
# - matches("(.)\\1"): nombres que coinciden con regex
# - num_range("x", 1:3): x1, x2, x3
# - one_of(c("a", "b")): columnas en un vector de nombres
# - everything(): todas las columnas
# - last_col(): última columna
# - where(fn): columnas donde fn devuelve TRUE


## 1.2 FILTER() - Filtrar filas según condiciones ----
# Mantiene las filas que cumplen una o más condiciones lógicas

# Ejemplos básicos:
mtcars %>% filter(mpg > 20)                # Condición simple
mtcars %>% filter(mpg > 20, cyl == 4)      # Múltiples condiciones (AND)
mtcars %>% filter(mpg > 20 | cyl == 4)     # Condición OR
mtcars %>% filter(mpg > 20 & cyl == 4)     # Operador & explícito
mtcars %>% filter(between(mpg, 20, 25))    # Entre dos valores
mtcars %>% filter(cyl %in% c(4, 6))        # Valor en un conjunto
mtcars %>% filter(!is.na(mpg))             # Eliminar valores NA
mtcars %>% filter(near(mpg, 21, tol = 0.1))# Comparación con tolerancia

# Operadores lógicos:
# ==  igual a
# !=  diferente de
# >   mayor que
# <   menor que
# >=  mayor o igual que
# <=  menor o igual que
# &   AND (y)
# |   OR (o)
# !   NOT (no)
# %in% está en
# is.na() es NA
# !is.na() no es NA


## 1.3 MUTATE() - Crear o modificar columnas ----
# Añade nuevas columnas o modifica existentes

# Ejemplos básicos:
mtcars %>% mutate(kpl = mpg * 0.425)       # Nueva columna
mtcars %>% mutate(
  kpl = mpg * 0.425,
  hp_per_cyl = hp / cyl
)                                           # Múltiples columnas nuevas
mtcars %>% mutate(mpg = mpg * 1.5)         # Modificar columna existente
mtcars %>% mutate(cyl = as.factor(cyl))    # Cambiar tipo de dato

# Funciones útiles dentro de mutate():
mtcars %>% mutate(
  mpg_rank = row_number(mpg),              # Asignar ranking
  mpg_scaled = scale(mpg),                 # Escalar (z-score)
  mpg_lag = lag(mpg),                      # Valor anterior
  mpg_lead = lead(mpg),                    # Valor siguiente
  mpg_cumsum = cumsum(mpg),                # Suma acumulativa
  mpg_cummean = cummean(mpg),              # Media acumulativa
  hp_percentile = percent_rank(hp)         # Percentil
)

# mutate() vs transmute():
# mutate() mantiene todas las columnas + las nuevas
# transmute() solo mantiene las columnas nuevas/especificadas
mtcars %>% transmute(kpl = mpg * 0.425)    # Solo devuelve la nueva columna


## 1.4 ARRANGE() - Ordenar filas ----
# Ordena las filas según una o más columnas

# Ejemplos básicos:
mtcars %>% arrange(mpg)                    # Orden ascendente
mtcars %>% arrange(desc(mpg))              # Orden descendente
mtcars %>% arrange(cyl, desc(mpg))         # Orden por múltiples columnas
mtcars %>% arrange(across(everything()))   # Ordenar por todas las columnas


## 1.5 SUMMARISE() / SUMMARIZE() - Resumir datos ----
# Reduce múltiples valores a un resumen único

# Ejemplos básicos:
mtcars %>% summarise(
  media_mpg = mean(mpg),
  mediana_mpg = median(mpg),
  sd_mpg = sd(mpg),
  n = n()                                  # Contar filas
)

# Funciones de resumen comunes:
# - mean(): media
# - median(): mediana
# - sd(): desviación estándar
# - var(): varianza
# - min(): mínimo
# - max(): máximo
# - sum(): suma
# - n(): contar filas
# - n_distinct(): contar valores únicos
# - first(): primer valor
# - last(): último valor
# - nth(x, n): n-ésimo valor
# - IQR(): rango intercuartílico
# - quantile(): cuantiles

mtcars %>% summarise(
  n_cars = n(),
  n_cyl_types = n_distinct(cyl),
  min_mpg = min(mpg),
  max_mpg = max(mpg),
  range_mpg = max(mpg) - min(mpg)
)


## 1.6 GROUP_BY() - Agrupar datos ----
# Agrupa datos para operaciones por grupo (combinar con summarise o mutate)

# Ejemplos básicos:
mtcars %>% 
  group_by(cyl) %>%
  summarise(
    n = n(),
    media_mpg = mean(mpg),
    media_hp = mean(hp)
  )

# Agrupar por múltiples variables:
mtcars %>%
  group_by(cyl, am) %>%
  summarise(media_mpg = mean(mpg), .groups = "drop")

# Usar mutate() con group_by():
mtcars %>%
  group_by(cyl) %>%
  mutate(
    mpg_media_grupo = mean(mpg),
    mpg_vs_media = mpg - mean(mpg)
  ) %>%
  ungroup()                                # IMPORTANTE: desagrupar después

# ungroup(): elimina la agrupación (buena práctica después de group_by)


# 2. FUNCIONES AUXILIARES Y COMPLEMENTARIAS ----

## 2.1 DISTINCT() - Eliminar filas duplicadas ----
# Mantiene solo filas únicas

mtcars %>% distinct()                      # Filas completamente únicas
mtcars %>% distinct(cyl)                   # Valores únicos de cyl
mtcars %>% distinct(cyl, am)               # Combinaciones únicas
mtcars %>% distinct(cyl, .keep_all = TRUE) # Mantener todas las columnas


## 2.2 SLICE() - Seleccionar filas por posición ----
# Selecciona filas por su posición/índice

mtcars %>% slice(1:5)                      # Primeras 5 filas
mtcars %>% slice(c(1, 5, 10))              # Filas específicas
mtcars %>% slice_head(n = 5)               # Primeras n filas
mtcars %>% slice_tail(n = 5)               # Últimas n filas
mtcars %>% slice_head(prop = 0.1)          # Primero 10% de filas
mtcars %>% slice_max(mpg, n = 5)           # Top 5 por mpg
mtcars %>% slice_min(mpg, n = 5)           # Bottom 5 por mpg
mtcars %>% slice_sample(n = 5)             # 5 filas aleatorias
mtcars %>% slice_sample(prop = 0.2)        # 20% filas aleatorias


## 2.3 RENAME() - Renombrar columnas ----
# Cambia los nombres de las columnas

mtcars %>% rename(millas_galon = mpg)      # Renombrar columna específica
mtcars %>% rename(cilindros = cyl, caballos = hp)
mtcars %>% rename_with(toupper)            # Aplicar función a todos los nombres
mtcars %>% rename_with(tolower)            # Convertir a minúsculas
mtcars %>% rename_with(~ paste0("var_", .x)) # Añadir prefijo


## 2.4 RELOCATE() - Reordenar columnas ----
# Cambia la posición de las columnas

mtcars %>% relocate(mpg, hp)               # Mover al principio
mtcars %>% relocate(mpg, .after = cyl)     # Mover después de cyl
mtcars %>% relocate(mpg, .before = hp)     # Mover antes de hp
mtcars %>% relocate(where(is.factor), .after = last_col())


## 2.5 PULL() - Extraer una columna como vector ----
# Similar a $, extrae una columna como vector

mtcars %>% pull(mpg)                       # Extraer columna mpg
mtcars %>% pull(1)                         # Extraer primera columna
mtcars %>% pull(-1)                        # Extraer última columna


## 2.6 COUNT() - Contar ocurrencias ----
# Cuenta el número de ocurrencias de cada valor

mtcars %>% count(cyl)                      # Contar por cyl
mtcars %>% count(cyl, am)                  # Contar por combinaciones
mtcars %>% count(cyl, sort = TRUE)         # Ordenar por frecuencia
mtcars %>% count(cyl, wt = hp)             # Contar ponderado (suma hp)
mtcars %>% count(cyl, name = "frecuencia") # Cambiar nombre de columna conteo

# add_count(): añade columna con conteo sin colapsar filas
mtcars %>% add_count(cyl)


## 2.7 GLIMPSE() - Vista rápida de los datos ----
# Muestra una vista transpuesta compacta del dataframe

mtcars %>% glimpse()                       # Similar a str() pero más legible


# 3. FUNCIONES DE JOINS (UNIONES) ----

# Los joins combinan dos dataframes basándose en columnas clave

# Crear dataframes de ejemplo:
df1 <- tibble(
  id = c(1, 2, 3),
  nombre = c("Ana", "Luis", "María")
)

df2 <- tibble(
  id = c(1, 2, 4),
  edad = c(25, 30, 35)
)

## 3.1 INNER_JOIN() - Solo filas coincidentes ----
# Mantiene solo las filas que tienen coincidencias en ambos dataframes

df1 %>% inner_join(df2, by = "id")         # Solo id 1 y 2


## 3.2 LEFT_JOIN() - Todas las filas de la izquierda ----
# Mantiene todas las filas del primer dataframe, añade NA si no hay coincidencia

df1 %>% left_join(df2, by = "id")          # Mantiene id 1, 2, 3


## 3.3 RIGHT_JOIN() - Todas las filas de la derecha ----
# Mantiene todas las filas del segundo dataframe

df1 %>% right_join(df2, by = "id")         # Mantiene id 1, 2, 4


## 3.4 FULL_JOIN() - Todas las filas de ambos ----
# Mantiene todas las filas de ambos dataframes

df1 %>% full_join(df2, by = "id")          # Mantiene id 1, 2, 3, 4


## 3.5 SEMI_JOIN() - Filas de A que tienen coincidencia en B ----
# Filtra el primer dataframe manteniendo solo filas con coincidencias

df1 %>% semi_join(df2, by = "id")          # Solo devuelve columnas de df1


## 3.6 ANTI_JOIN() - Filas de A sin coincidencia en B ----
# Filtra el primer dataframe manteniendo solo filas SIN coincidencias

df1 %>% anti_join(df2, by = "id")          # Solo id 3 (sin edad en df2)


# Joins con diferentes nombres de columna:
df1 %>% left_join(df2, by = c("id" = "persona_id"))

# Joins sin especificar columna (usa columnas con mismo nombre):
df1 %>% left_join(df2)


# 4. FUNCIONES DE CONJUNTOS ----

# Operaciones que tratan dataframes como conjuntos

df_a <- tibble(x = 1:3, y = c("a", "b", "c"))
df_b <- tibble(x = 2:4, y = c("b", "c", "d"))

## 4.1 BIND_ROWS() - Combinar filas ----
# Apila dataframes verticalmente

bind_rows(df_a, df_b)                      # Añade filas de df_b bajo df_a
bind_rows(df_a, df_b, .id = "origen")      # Añade columna indicando origen


## 4.2 BIND_COLS() - Combinar columnas ----
# Une dataframes horizontalmente (¡cuidado con el orden!)

bind_cols(df_a, df_b)                      # Añade columnas lado a lado


## 4.3 INTERSECT() - Intersección ----
# Filas que aparecen en AMBOS dataframes

intersect(df_a, df_b)                      # Solo filas comunes


## 4.4 UNION() - Unión ----
# Todas las filas únicas de ambos dataframes

union(df_a, df_b)                          # Combina y elimina duplicados


## 4.5 UNION_ALL() - Unión con duplicados ----
# Todas las filas de ambos dataframes (mantiene duplicados)

union_all(df_a, df_b)                      # Combina manteniendo duplicados


## 4.6 SETDIFF() - Diferencia ----
# Filas en primer dataframe que NO están en el segundo

setdiff(df_a, df_b)                        # Filas solo en df_a
setdiff(df_b, df_a)                        # Filas solo en df_b


# 5. FUNCIONES AVANZADAS ----

## 5.1 ACROSS() - Aplicar función a múltiples columnas ----
# Permite aplicar la misma transformación a varias columnas

# Usar con summarise:
mtcars %>%
  summarise(across(c(mpg, hp, wt), mean))  # Media de varias columnas

mtcars %>%
  summarise(across(where(is.numeric), mean)) # Media de todas numéricas

mtcars %>%
  summarise(across(starts_with("m"), 
                   list(media = mean, sd = sd))) # Múltiples funciones

# Usar con mutate:
mtcars %>%
  mutate(across(where(is.numeric), scale))  # Escalar todas numéricas

mtcars %>%
  mutate(across(c(mpg, hp), ~ .x * 2))      # Multiplicar por 2


## 5.2 IF_ELSE() - Condicional vectorizado ----
# Versión de dplyr del ifelse base

mtcars %>%
  mutate(
    consumo = if_else(mpg > 20, "Bajo", "Alto"),
    consumo_detalle = if_else(
      mpg > 25, "Muy bajo",
      if_else(mpg > 20, "Bajo", "Alto")
    )
  )


## 5.3 CASE_WHEN() - Condicionales múltiples ----
# Múltiples condiciones if-else de forma elegante

mtcars %>%
  mutate(
    categoria = case_when(
      mpg > 25 ~ "Eficiente",
      mpg > 20 ~ "Moderado",
      mpg > 15 ~ "Normal",
      TRUE ~ "Ineficiente"              # Caso por defecto (else)
    )
  )

# Ejemplo más complejo:
mtcars %>%
  mutate(
    tipo = case_when(
      cyl == 4 & mpg > 25 ~ "Pequeño eficiente",
      cyl == 4 ~ "Pequeño",
      cyl == 6 ~ "Mediano",
      cyl == 8 & hp > 200 ~ "Grande potente",
      cyl == 8 ~ "Grande",
      TRUE ~ "Otro"
    )
  )


## 5.4 NA_IF() - Convertir valores específicos a NA ----
# Convierte un valor específico a NA

mtcars %>%
  mutate(mpg = na_if(mpg, 21))              # Convierte 21 a NA


## 5.5 COALESCE() - Primer valor no-NA ----
# Devuelve el primer valor no-NA de una secuencia

df <- tibble(
  a = c(1, NA, 3),
  b = c(NA, 2, 4),
  c = c(5, 6, 7)
)

df %>% mutate(primero_disponible = coalesce(a, b, c))


## 5.6 ROWWISE() - Operaciones por fila ----
# Similar a group_by() pero agrupa cada fila individualmente

df <- tibble(
  x = 1:3,
  y = 4:6,
  z = 7:9
)

df %>%
  rowwise() %>%
  mutate(
    suma = sum(c(x, y, z)),
    media = mean(c(x, y, z))
  ) %>%
  ungroup()

# Útil para funciones que no son naturalmente vectorizadas:
df %>%
  rowwise() %>%
  mutate(suma = sum(c_across(everything()))) %>%
  ungroup()


## 5.7 C_ACROSS() - Seleccionar columnas dentro de rowwise() ----
# Complemento de across() para operaciones rowwise

df %>%
  rowwise() %>%
  mutate(
    suma = sum(c_across(x:z)),
    media = mean(c_across(where(is.numeric)))
  ) %>%
  ungroup()


## 5.8 CUR_DATA(), CUR_GROUP(), CUR_GROUP_ID() - Datos del contexto actual ----
# Funciones que dan acceso al contexto actual en operaciones agrupadas

mtcars %>%
  group_by(cyl) %>%
  summarise(
    grupo_id = cur_group_id(),            # ID numérico del grupo
    datos_grupo = list(cur_data())        # Datos del grupo actual
  )


## 5.9 N() y N_DISTINCT() - Conteo ----
# n() cuenta filas, n_distinct() cuenta valores únicos

mtcars %>%
  group_by(cyl) %>%
  summarise(
    n_cars = n(),
    n_am_types = n_distinct(am)
  )


# 6. FUNCIONES DE VENTANA (WINDOW FUNCTIONS) ----

# Funciones que operan sobre un "ventana" de filas relacionadas

## 6.1 ROW_NUMBER(), RANK(), DENSE_RANK() - Rankings ----

mtcars %>%
  mutate(
    row_num = row_number(mpg),            # 1, 2, 3, 4, 5...
    rank_mpg = rank(mpg),                 # Con empates: 1, 2, 2, 4, 5...
    dense_rank_mpg = dense_rank(mpg),     # Con empates: 1, 2, 2, 3, 4...
    percent_rank_mpg = percent_rank(mpg), # Percentil (0 a 1)
    cume_dist_mpg = cume_dist(mpg)        # Distribución acumulativa
  )

# Por grupos:
mtcars %>%
  group_by(cyl) %>%
  mutate(rank_en_grupo = row_number(desc(mpg))) %>%
  ungroup()


## 6.2 LAG() y LEAD() - Valores anteriores/siguientes ----

mtcars %>%
  mutate(
    mpg_anterior = lag(mpg),              # Valor anterior
    mpg_siguiente = lead(mpg),            # Valor siguiente
    mpg_2_atras = lag(mpg, n = 2),        # 2 posiciones atrás
    diferencia = mpg - lag(mpg)           # Diferencia con anterior
  )


## 6.3 CUMSUM(), CUMMAX(), CUMMIN(), CUMMEAN() - Acumulativas ----

mtcars %>%
  arrange(mpg) %>%
  mutate(
    suma_acum = cumsum(mpg),              # Suma acumulativa
    max_acum = cummax(mpg),               # Máximo acumulativo
    min_acum = cummin(mpg),               # Mínimo acumulativo
    media_acum = cummean(mpg)             # Media acumulativa
  )


## 6.4 NTILE() - Dividir en n grupos ----

mtcars %>%
  mutate(
    cuartil = ntile(mpg, 4),              # Dividir en 4 grupos (cuartiles)
    decil = ntile(mpg, 10)                # Dividir en 10 grupos (deciles)
  )


# 7. FUNCIONES TIDYR (COMPAÑERAS DE DPLYR) ----

# Aunque son de tidyr, se usan frecuentemente con dplyr

library(tidyr)

## 7.1 PIVOT_LONGER() - De ancho a largo ----
# Convierte columnas en filas (formato largo)

df_ancho <- tibble(
  pais = c("España", "Francia"),
  año_2020 = c(100, 120),
  año_2021 = c(110, 130)
)

df_ancho %>%
  pivot_longer(
    cols = starts_with("año"),
    names_to = "año",
    values_to = "valor"
  )


## 7.2 PIVOT_WIDER() - De largo a ancho ----
# Convierte filas en columnas (formato ancho)

df_largo <- tibble(
  pais = c("España", "España", "Francia", "Francia"),
  año = c(2020, 2021, 2020, 2021),
  valor = c(100, 110, 120, 130)
)

df_largo %>%
  pivot_wider(
    names_from = año,
    values_from = valor
  )


## 7.3 SEPARATE() - Separar una columna en varias ----

df <- tibble(nombre_completo = c("Ana García", "Luis Pérez"))

df %>%
  separate(nombre_completo, 
           into = c("nombre", "apellido"), 
           sep = " ")


## 7.4 UNITE() - Unir varias columnas en una ----

df <- tibble(nombre = c("Ana", "Luis"), apellido = c("García", "Pérez"))

df %>%
  unite("nombre_completo", nombre, apellido, sep = " ")


## 7.5 FILL() - Rellenar NAs con valores anteriores/siguientes ----

df <- tibble(
  grupo = c("A", NA, NA, "B", NA),
  valor = 1:5
)

df %>% fill(grupo, .direction = "down")    # Rellenar hacia abajo
df %>% fill(grupo, .direction = "up")      # Rellenar hacia arriba


## 7.6 DROP_NA() - Eliminar filas con NA ----

df <- tibble(x = c(1, NA, 3), y = c(NA, 2, 3))

df %>% drop_na()                           # Eliminar filas con cualquier NA
df %>% drop_na(x)                          # Eliminar solo si x es NA


## 7.7 REPLACE_NA() - Reemplazar NA con un valor ----

df <- tibble(x = c(1, NA, 3), y = c(NA, 2, 3))

df %>% replace_na(list(x = 0, y = 0))     # Reemplazar NA con 0


# 8. OPERADOR PIPE Y ENCADENAMIENTO ----

# El operador pipe %>% (o |> en R >= 4.1) permite encadenar operaciones

# Sin pipe (difícil de leer):
arrange(
  mutate(
    filter(mtcars, mpg > 20),
    kpl = mpg * 0.425
  ),
  desc(kpl)
)

# Con pipe (más legible):
mtcars %>%
  filter(mpg > 20) %>%
  mutate(kpl = mpg * 0.425) %>%
  arrange(desc(kpl))

# El pipe nuevo de R base (requiere R >= 4.1):
mtcars |>
  filter(mpg > 20) |>
  mutate(kpl = mpg * 0.425)

# Placeholder en pipe:
mtcars %>% lm(mpg ~ hp, data = .)         # . representa los datos


# 9. EJEMPLOS PRÁCTICOS COMPLETOS ----

## 9.1 Análisis exploratorio básico ----

mtcars %>%
  # Seleccionar columnas de interés
  select(mpg, cyl, hp, wt, am) %>%
  # Filtrar solo autos con más de 100 HP
  filter(hp > 100) %>%
  # Crear categorías
  mutate(
    transmission = if_else(am == 0, "Automática", "Manual"),
    peso_cat = case_when(
      wt < 3 ~ "Ligero",
      wt < 4 ~ "Medio",
      TRUE ~ "Pesado"
    )
  ) %>%
  # Agrupar y resumir
  group_by(cyl, transmission) %>%
  summarise(
    n = n(),
    mpg_medio = mean(mpg),
    hp_medio = mean(hp),
    .groups = "drop"
  ) %>%
  # Ordenar resultados
  arrange(cyl, desc(mpg_medio))


## 9.2 Limpieza y transformación de datos ----

# Dataset de ejemplo con problemas
df_sucio <- tibble(
  id = c(1, 2, 2, 3, 4, 5),               # Duplicados
  nombre = c("Ana", "luis", "MARÍA", "Pedro", NA, "Laura"),
  edad = c(25, NA, 30, -5, 35, 40),       # NA y valor negativo
  ciudad = c("Madrid", "madrid", "Barcelona", "Madrid", "Valencia", NA)
)

# Limpieza paso a paso
df_limpio <- df_sucio %>%
  # Eliminar duplicados
  distinct(id, .keep_all = TRUE) %>%
  # Eliminar filas con NA en nombre
  filter(!is.na(nombre)) %>%
  # Normalizar texto
  mutate(
    nombre = str_to_title(nombre),
    ciudad = str_to_title(ciudad)
  ) %>%
  # Corregir valores negativos
  mutate(edad = if_else(edad < 0, NA_real_, edad)) %>%
  # Rellenar NAs
  mutate(
    edad = coalesce(edad, median(edad, na.rm = TRUE)),
    ciudad = coalesce(ciudad, "Desconocida")
  )


## 9.3 Análisis por grupos con múltiples métricas ----

mtcars %>%
  group_by(cyl) %>%
  summarise(
    # Estadísticos básicos
    n = n(),
    mpg_media = mean(mpg),
    mpg_mediana = median(mpg),
    mpg_sd = sd(mpg),
    
    # Percentiles
    mpg_p25 = quantile(mpg, 0.25),
    mpg_p75 = quantile(mpg, 0.75),
    
    # Rango
    mpg_min = min(mpg),
    mpg_max = max(mpg),
    mpg_rango = max(mpg) - min(mpg),
    
    # Otras métricas
    hp_medio = mean(hp),
    peso_total = sum(wt),
    
    .groups = "drop"
  )


## 9.4 Top N por grupo ----

# Top 3 autos con mejor mpg por número de cilindros
mtcars %>%
  group_by(cyl) %>%
  slice_max(mpg, n = 3) %>%
  ungroup()

# Alternativamente con row_number:
mtcars %>%
  group_by(cyl) %>%
  mutate(rank = row_number(desc(mpg))) %>%
  filter(rank <= 3) %>%
  ungroup()


## 9.5 Detección de outliers ----

mtcars %>%
  mutate(
    # Calcular z-score
    mpg_z = scale(mpg),
    # Identificar outliers (|z| > 2)
    es_outlier = abs(mpg_z) > 2,
    # Método IQR
    mpg_q1 = quantile(mpg, 0.25),
    mpg_q3 = quantile(mpg, 0.75),
    mpg_iqr = IQR(mpg),
    es_outlier_iqr = mpg < (mpg_q1 - 1.5 * mpg_iqr) | 
                     mpg > (mpg_q3 + 1.5 * mpg_iqr)
  ) %>%
  select(mpg, mpg_z, es_outlier, es_outlier_iqr)


## 9.6 Crear tabla de resumen con múltiples variables ----

mtcars %>%
  summarise(
    across(
      c(mpg, hp, wt),
      list(
        media = ~mean(.x),
        sd = ~sd(.x),
        min = ~min(.x),
        max = ~max(.x)
      ),
      .names = "{.col}_{.fn}"
    )
  )


# 10. CONSEJOS Y MEJORES PRÁCTICAS ----

# 1. Siempre usar ungroup() después de group_by() cuando termines
mtcars %>%
  group_by(cyl) %>%
  mutate(media = mean(mpg)) %>%
  ungroup()  # ¡IMPORTANTE!

# 2. Usar .groups = "drop" en summarise() para evitar warnings
mtcars %>%
  group_by(cyl, am) %>%
  summarise(media = mean(mpg), .groups = "drop")

# 3. Nombrar pasos complejos para claridad
mtcars_filtrado <- mtcars %>%
  filter(mpg > 20)

resultado <- mtcars_filtrado %>%
  group_by(cyl) %>%
  summarise(media = mean(mpg))

# 4. Usar across() para operaciones repetitivas
# Mal:
mtcars %>%
  mutate(
    mpg_scaled = scale(mpg),
    hp_scaled = scale(hp),
    wt_scaled = scale(wt)
  )

# Bien:
mtcars %>%
  mutate(across(c(mpg, hp, wt), scale, .names = "{.col}_scaled"))

# 5. Verificar datos después de joins
resultado_join <- df1 %>%
  left_join(df2, by = "id")

# Verificar NAs introducidos
resultado_join %>%
  summarise(across(everything(), ~sum(is.na(.x))))

# 6. Usar count() en lugar de group_by + summarise(n = n())
# Mal:
mtcars %>%
  group_by(cyl) %>%
  summarise(n = n())

# Bien:
mtcars %>%
  count(cyl)

# 7. Usar case_when() para múltiples condiciones en lugar de if_else anidados

# 8. Usar where() para seleccionar columnas por tipo
mtcars %>%
  select(where(is.numeric))

# 9. Usar .before y .after en mutate() para posicionar nuevas columnas
mtcars %>%
  mutate(kpl = mpg * 0.425, .after = mpg)

# 10. Comentar código complejo
mtcars %>%
  # Filtrar solo autos eficientes
  filter(mpg > 20) %>%
  # Calcular métricas por cilindros
  group_by(cyl) %>%
  summarise(media_mpg = mean(mpg))


# 11. FUNCIONES MENOS CONOCIDAS PERO ÚTILES ----

## 11.1 BETWEEN() - Verificar si está entre dos valores ----
mtcars %>% filter(between(mpg, 20, 25))


## 11.2 NEAR() - Comparación con tolerancia para números flotantes ----
mtcars %>% filter(near(mpg, 21, tol = 0.1))


## 11.3 RECODE() - Recodificar valores ----
mtcars %>%
  mutate(cyl_nombre = recode(cyl,
    `4` = "cuatro",
    `6` = "seis",
    `8` = "ocho"
  ))


## 11.4 TOP_N() - Top n filas (DEPRECATED, usar slice_max) ----
# Usar slice_max() en su lugar
mtcars %>% slice_max(mpg, n = 5)


## 11.5 SAMPLE_N() y SAMPLE_FRAC() - Muestreo (DEPRECATED, usar slice_sample) ----
# Usar slice_sample() en su lugar
mtcars %>% slice_sample(n = 5)
mtcars %>% slice_sample(prop = 0.1)


## 11.6 ADD_ROW() - Añadir filas ----
mtcars %>%
  add_row(mpg = 25, cyl = 4, hp = 100, .before = 1)


## 11.7 ROWS_INSERT(), ROWS_UPDATE(), ROWS_DELETE() - Operaciones de fila ----
df_base <- tibble(id = 1:3, valor = c(10, 20, 30))
df_nuevas <- tibble(id = 4:5, valor = c(40, 50))

# Insertar nuevas filas
rows_insert(df_base, df_nuevas, by = "id")

# Actualizar filas existentes
df_actualizaciones <- tibble(id = 2:3, valor = c(200, 300))
rows_update(df_base, df_actualizaciones, by = "id")


# 12. COMBINACIONES AVANZADAS Y PATRONES ----

## 12.1 Cálculos rolling/moving window ----
library(zoo)  # Para rolling windows

mtcars %>%
  arrange(mpg) %>%
  mutate(
    mpg_rolling_mean = zoo::rollmean(mpg, k = 3, fill = NA, align = "right")
  )


## 12.2 Múltiples agrupaciones en un solo análisis ----
bind_rows(
  mtcars %>%
    summarise(grupo = "Total", media_mpg = mean(mpg)),
  
  mtcars %>%
    group_by(cyl) %>%
    summarise(grupo = paste("Cyl", cyl), media_mpg = mean(mpg))
)


## 12.3 Crear múltiples columnas basadas en condiciones ----
mtcars %>%
  mutate(
    across(
      c(mpg, hp),
      list(
        bajo = ~.x < median(.x),
        alto = ~.x > median(.x)
      ),
      .names = "{.col}_{.fn}"
    )
  )


## 12.4 Trabajar con listas-columnas ----
mtcars %>%
  group_by(cyl) %>%
  summarise(
    datos = list(tibble(mpg = mpg, hp = hp)),
    modelos = list(lm(mpg ~ hp))
  )


# 13. RECURSOS Y REFERENCIAS ----

# Cheatsheet oficial de dplyr:
# https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf

# Documentación oficial:
# https://dplyr.tidyverse.org/

# Libro "R for Data Science":
# https://r4ds.had.co.nz/

# Para aprender más sobre tidyverse:
# vignette("dplyr")
# help(package = "dplyr")

# FIN DEL RESUMEN ----

# Este documento contiene un resumen exhaustivo de las principales funciones
# y herramientas de dplyr. Se recomienda practicar con datasets propios y
# consultar la documentación oficial para casos específicos.

# ¡Happy coding! 📊
