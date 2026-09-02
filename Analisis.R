# =============================================================
# LAB 3 · Análisis de métricas de software y DevOps
# Fase 1: Carga y limpieza
# Fase 2: Descriptiva univariada
# =============================================================

# --- 0. Paquetes -------------------------------------------------
# Ejecutar solo la primera vez (o si falta algún paquete):
# install.packages(c("tidyverse", "moments", "rmarkdown", "knitr"))

library(tidyverse)
library(moments)  

# =============================================================
# FASE 1 · CARGA Y LIMPIEZA
# =============================================================

# --- 1.1 Cargar los datos ----------------------------------------
#df = dataframe, lo que significa una coleccion de datos. Con la funcion lee el archivo
df <- read_csv("devops_metrics.csv")

# --- 1.2 Inspección inicial --------------------------------------
glimpse(df)     # un vistazo de los datos que cargamos
summary(df)     # estadísticos rápidos de cada variable que tiene datos numericos
dim(df)         # n de filas y columnas

# --- 1.3 Detectar valores faltantes -------------------------------
colSums(is.na(df)) #funcion que muestra numero de valores faltantes
# En este dataset esperamos NA solo en test_coverage_pct y ticket_resolution_h

# Visualizar qué % representa cada NA
colMeans(is.na(df)) %>% round(4) * 100

# --- 1.4 Detectar duplicados ---------------------------------------
sum(duplicated(df))               # filas 100% repetidas
df <- df %>% distinct()           # eliminamos duplicados exactos

# --- 1.5 Corregir inconsistencias de texto -------------------------
# La variable 'team' tiene algunos valores en MAYÚSCULA por error de digitación (p. ej. "BETA" en vez de "Beta"). 
# Normalizamos:
df <- df %>% mutate(team = str_to_title(team))

# Revisamos categorías únicas para confirmar que quedaron limpias
unique(df$team)
unique(df$module)
unique(df$priority)
unique(df$deploy_status)

# --- 1.6 Corregir tipos de variable ---------------------------------
df <- df %>%
  mutate(
    team          = as.factor(team),
    module        = as.factor(module),
    deploy_status = as.factor(deploy_status),
    priority      = factor(priority,
                           levels = c("baja", "media", "alta", "critica"),
                           ordered = TRUE)
  )

str(df)  # confirmar que priority quedó como ordered factor

# --- 1.7 Revisar valores fuera de rango / outliers evidentes --------
# test_coverage_pct debe estar entre 0 y 100
df %>% filter(test_coverage_pct < 0 | test_coverage_pct > 100)


# Regla simple: valores > percentil 99.5 se consideran errores de
# digitación y se acotan en vez de eliminarse para no perder filas completas.
# reemplaza por un valor permitido cercano
p995 <- quantile(df$build_time_min, 0.995, na.rm = TRUE)
df <- df %>%
  mutate(build_time_min = ifelse(build_time_min > p995, p995, build_time_min))

# --- 1.8 Decisión sobre los NA ---------------------------------------
# Para este proyecto, dado que los NA son <5% del total y no hay un
# patrón evidente de "faltante no aleatorio", se documenta la decisión
# de EXCLUIRLOS solo en los cálculos puntuales donde afectan
# (usando na.rm = TRUE), en vez de imputarlos, para no introducir
# supuestos artificiales en variables clave del análisis.


# --- 1.9 Guardar el dataset limpio ------------------------------------
write_csv(df, "devops_metrics_clean.csv")


# =============================================================
# FASE 2 · DESCRIPTIVA UNIVARIADA
# =============================================================

# --- 2.1 Función --------------------
# Calcula tendencia central, dispersión y forma para una variable
describir_numerica <- function(x, nombre) {
  x <- x[!is.na(x)]
  tibble(
    variable = nombre,
    n        = length(x),
    media    = mean(x),
    mediana  = median(x),
    moda     = as.numeric(names(sort(table(round(x)), decreasing = TRUE))[1]),
    sd       = sd(x),
    varianza = var(x),
    cv       = sd(x) / mean(x),
    rango    = max(x) - min(x),
    iqr      = IQR(x),
    asimetria = skewness(x),
    curtosis  = kurtosis(x)
  )
}

# --- 2.2 Aplicar a cada variable cuantitativa ------------------------
vars_cuant <- c("build_time_min", "deploy_time_min", "commit_size_loc",
                "num_bugs", "test_coverage_pct", "ticket_resolution_h")

tabla_descriptiva <- map_dfr(vars_cuant, ~ describir_numerica(df[[.x]], .x))
print(tabla_descriptiva, width = Inf)

# --- 2.3 Interpretación rápida de asimetría y curtosis ---------------
# skewness > 0  -> cola a la derecha (valores altos atípicos)
# skewness < 0  -> cola a la izquierda
# skewness ~ 0  -> razonablemente simétrica
# kurtosis > 3  -> leptocúrtica (colas más pesadas que la normal)
# kurtosis < 3  -> platicúrtica (colas más livianas)
tabla_descriptiva %>%
  select(variable, asimetria, curtosis) %>%
  mutate(
    forma_asimetria = case_when(
      asimetria >  0.5 ~ "asimétrica a la derecha",
      asimetria < -0.5 ~ "asimétrica a la izquierda",
      TRUE             ~ "aprox. simétrica"
    ),
    forma_curtosis = if_else(curtosis > 3, "leptocúrtica", "platicúrtica/mesocúrtica")
  )

# --- 2.4 Coeficiente de variación ---
tabla_descriptiva %>%
  select(variable, cv) %>%
  arrange(desc(cv))

# --- 2.5 Descriptiva rápida de las variables cualitativas -------------
df %>% count(team, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(module, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(priority) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(deploy_status) %>% mutate(pct = round(100 * n / sum(n), 1))

# --- 2.6 Guardar la tabla de resultados para usarla en el reporte -----
write_csv(tabla_descriptiva, "tabla_descriptiva_fase2.csv")


# =============================================================
# Fase 3: Frecuencias y agrupación
# Fase 4: Análisis por grupos
# =============================================================

# =============================================================
# FASE 3 · FRECUENCIAS Y AGRUPACIÓN
# =============================================================

# --- 3.1 Regla de Sturges ----------
# La regla de Sturges da un número razonable de tramos según cuántos datos tengo.
k <- ceiling(1 + 3.322 * log10(nrow(df)))
k   # con ~5000 filas, esto da aprox. 13-14 tramos

# --- 3.2 Tabla de frecuencias de build_time_min ----------------------
tabla_build <- df %>%
  mutate(clase = cut(build_time_min, breaks = k)) %>%
  count(clase, name = "fa") %>%              # fa = frecuencia absoluta
  mutate(
    fr  = round(fa / sum(fa), 4),             # fr = frecuencia relativa
    fac = cumsum(fa),                         # fac = frecuencia acumulada
    frac = round(cumsum(fr), 4)               # frecuencia relativa acumulada
  )

print(tabla_build, n = Inf)

# --- 3.3 Identificar la clase modal ----------------------------------
# La "clase modal" es simplemente el tramo que junta más observaciones.
clase_modal <- tabla_build %>% filter(fa == max(fa))
clase_modal

# --- 3.4 Repetir el mismo análisis para otra variable continua -------
# (deploy_time_min, como ejemplo adicional)
tabla_deploy <- df %>%
  mutate(clase = cut(deploy_time_min, breaks = k)) %>%
  count(clase, name = "fa") %>%
  mutate(fr = round(fa / sum(fa), 4), fac = cumsum(fa))

print(tabla_deploy, n = Inf)

# --- 3.5 Frecuencias de variables discretas 
# num_bugs y commit_size_loc ya son números enteros pequeños en su
# mayoría, así que a veces conviene ver la frecuencia de cada valor
# exacto en vez de agruparlo en tramos:
df %>%
  count(num_bugs, name = "fa") %>%
  mutate(fr = round(fa / sum(fa), 4), fac = cumsum(fa))

# --- 3.6 Frecuencias de variables cualitativas ------------------------
# 
df %>%
  count(priority, name = "fa") %>%
  mutate(fr = round(fa / sum(fa), 4))

df %>%
  count(deploy_status, name = "fa") %>%
  mutate(fr = round(fa / sum(fa), 4))


# =============================================================
# FASE 4 · ANÁLISIS POR GRUPOS
# =============================================================

# --- 4.1 Comparar build_time_min y bugs entre equipos ------------------
# group_by() + summarise() es el patrón central de esta fase:
# "divide el dataset en sub-grupos según una columna, y calcula un
# resumen para cada sub-grupo por separado".
resumen_equipos <- df %>%
  group_by(team) %>%
  summarise(
    n_eventos  = n(),
    build_media = round(mean(build_time_min, na.rm = TRUE), 2),
    build_mediana = round(median(build_time_min, na.rm = TRUE), 2),
    bugs_promedio = round(mean(num_bugs, na.rm = TRUE), 2),
    tasa_fallos = round(mean(deploy_status == "failed", na.rm = TRUE), 3)
  ) %>%
  arrange(desc(tasa_fallos))

resumen_equipos

# --- 4.2 Comparar por módulo -------------------------------------------
resumen_modulo <- df %>%
  group_by(module) %>%
  summarise(
    n_eventos = n(),
    cobertura_media = round(mean(test_coverage_pct, na.rm = TRUE), 1),
    bugs_promedio = round(mean(num_bugs, na.rm = TRUE), 2),
    tasa_fallos = round(mean(deploy_status == "failed", na.rm = TRUE), 3)
  ) %>%
  arrange(desc(bugs_promedio))

resumen_modulo

# --- 4.3 Comparar por prioridad del ticket ------------------------------
resumen_prioridad <- df %>%
  group_by(priority) %>%
  summarise(
    n_eventos = n(),
    resolucion_media_h = round(mean(ticket_resolution_h, na.rm = TRUE), 1),
    resolucion_mediana_h = round(median(ticket_resolution_h, na.rm = TRUE), 1)
  )

resumen_prioridad
# Como 'priority' quedó como factor ordenado (Fase 1), esta tabla
# ya sale ordenada de "baja" a "critica", no alfabéticamente.

# --- 4.4 Tabla cruzada: equipo por estado del despliegue -------------------
# table() cuenta cuántas veces se combina cada valor de una variable
# con cada valor de otra. Es la forma más simple de comparar dos
# variables categóricas al mismo tiempo.
tabla_cruzada <- table(df$team, df$deploy_status)
tabla_cruzada

# Lo mismo pero en porcentaje POR FILA 
prop.table(tabla_cruzada, margin = 1) %>% round(3)

# =============================================================
# Fase 5: Relaciones bivariadas
# Fase 6: Visualización
# =============================================================


# =============================================================
# FASE 5 · RELACIONES BIVARIADAS
# =============================================================

# --- 5.1 Matriz de correlación entre TODAS las variables numéricas ----
# select(where(is.numeric)) elige automáticamente solo las columnas
# numéricas (descarta team, module, priority, deploy_status, que son
# categóricas y no tiene sentido correlacionar).
num <- df %>% select(where(is.numeric)) %>% select(-event_id)

matriz_cor <- cor(num, use = "complete.obs") %>% round(2)
matriz_cor

# use = "complete.obs" le dice a R: si una fila tiene NA en alguna
# de las dos variables que estás comparando, ignórala solo para ese
# cálculo (no borra la fila del resto del análisis).

# --- 5.2 Ver la matriz como "mapa de calor"  --------
# Convertimos la matriz a formato largo (una fila por cada par de
# variables) para poder graficarla con ggplot.
matriz_larga <- as.data.frame(matriz_cor) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlacion")

ggplot(matriz_larga, aes(var1, var2, fill = correlacion)) +
  geom_tile() +
  geom_text(aes(label = correlacion), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlación entre variables numéricas", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 5.3 Tamaño del commit vs. bugs detectados  --------

ggplot(df, aes(commit_size_loc, num_bugs)) +
  geom_jitter(alpha = 0.15, width = 0, height = 0.15) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(x = "Líneas de código modificadas", y = "Número de bugs")
# --- 5.4 Cobertura de pruebas vs. bugs detectados  --------

ggplot(df, aes(test_coverage_pct, num_bugs)) +
  geom_jitter(alpha = 0.15, height = 0.15) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(x = "Cobertura de pruebas (%)", y = "Número de bugs")


# --- 5.5 Tabla de contingencia: prioridad x estado del despliegue -------
# Para dos variables CATEGÓRICAS, no se usa correlación (esa es para
# números) sino tablas de contingencia con porcentajes.
tabla_prioridad_estado <- prop.table(
  table(df$priority, df$deploy_status), margin = 1
) %>% round(3)

tabla_prioridad_estado

# =============================================================
# FASE 6 · VISUALIZACIÓN
# =============================================================


# --- 6.1 Histograma: distribución de build_time_min ----------------------
ggplot(df, aes(build_time_min)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribución del tiempo de build",
       x = "Tiempo de build (minutos)", y = "Frecuencia")

# --- 6.2 Histograma: distribución de test_coverage_pct --------------------
ggplot(df, aes(test_coverage_pct)) +
  geom_histogram(bins = 20, fill = "darkgreen", color = "white") +
  labs(title = "Distribución de la cobertura de pruebas",
       x = "Cobertura de pruebas (%)", y = "Frecuencia")

# --- 6.3 Boxplot: tiempo de resolución de tickets por equipo ---------------
ggplot(df, aes(team, ticket_resolution_h)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Horas de resolución de tickets por equipo",
       x = "Equipo", y = "Horas de resolución")

# --- 6.4 Boxplot: bugs por prioridad ----------------------------------------
ggplot(df, aes(priority, num_bugs)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Número de bugs según prioridad del ticket",
       x = "Prioridad", y = "Número de bugs")

# --- 6.5 Gráfico de barras: cantidad de eventos por módulo -------------------
df %>%
  count(module) %>%
  ggplot(aes(x = reorder(module, -n), y = n)) +
  geom_col(fill = "orange") +
  labs(title = "Cantidad de eventos por módulo",
       x = "Módulo", y = "Cantidad de eventos")

# --- 6.6 Gráfico de barras: % de despliegues fallidos por equipo -------------
df %>%
  group_by(team) %>%
  summarise(tasa_fallos = mean(deploy_status == "failed")) %>%
  ggplot(aes(x = reorder(team, -tasa_fallos), y = tasa_fallos)) +
  geom_col(fill = "firebrick") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tasa de despliegues fallidos por equipo",
       x = "Equipo", y = "% de despliegues fallidos")

# --- 6.7 Dispersión: commit_size_loc vs deploy_time_min -----------------------
ggplot(df, aes(commit_size_loc, deploy_time_min)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  labs(title = "Tamaño del commit vs. tiempo de despliegue",
       x = "Líneas de código modificadas", y = "Tiempo de despliegue (min)")

# --- 6.8 Guardar los gráficos más importantes como archivos ------------------
# ggsave() guarda el ÚLTIMO gráfico dibujado. Se recomienda asignar
# cada gráfico a un objeto y luego guardarlo explícitamente para
# no perder el control de cuál gráfico se está exportando.
g_hist_build <- ggplot(df, aes(build_time_min)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribución del tiempo de build",
       x = "Tiempo de build (minutos)", y = "Frecuencia")

ggsave("hist_build_time.png", g_hist_build, width = 7, height = 5)

g_box_team <- ggplot(df, aes(team, ticket_resolution_h)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Horas de resolución de tickets por equipo",
       x = "Equipo", y = "Horas de resolución")

ggsave("box_resolucion_equipo.png", g_box_team, width = 7, height = 5)



