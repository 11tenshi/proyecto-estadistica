#@abrazoconejito en ig
#Se importan las librerias necesarias para el analisis

library(tidyverse)
library(moments)

#se declara un dataframe que contiene los datos importando el csv
#abrazoconejito en tiktok

df <- read_csv("devops_metrics.csv")
glimpse(df); summary(df)

dim(df)

# 1.3 Revisar duplicados exactos
sum(duplicated(df))
df <- df %>% distinct()

# 1.4 Revisar valores faltantes por columna
colSums(is.na(df))

# Decisión de limpieza (documentar SIEMPRE el porqué):
# - test_coverage_pct y ticket_resolution_h tienen NA (~1-3%).
#   Como son pocos y el resto de la fila es válido, se opta por
#   IMPUTAR con la mediana de su grupo (team) en vez de eliminar
#   filas completas, para no perder información de otras variables.
df <- df %>%
  group_by(team) %>%
  mutate(
    test_coverage_pct = ifelse(is.na(test_coverage_pct),
                               median(test_coverage_pct, na.rm = TRUE),
                               test_coverage_pct),
    ticket_resolution_h = ifelse(is.na(ticket_resolution_h),
                                 median(ticket_resolution_h, na.rm = TRUE),
                                 ticket_resolution_h)
  ) %>%
  ungroup()

# Verificar que ya no quedan NA
colSums(is.na(df))

# 1.5 Corregir tipos de dato
df <- df %>%
  mutate(
    team = as.factor(team),
    module = as.factor(module),
    priority = factor(priority,
                      levels = c("baja", "media", "alta", "critica"),
                      ordered = TRUE),
    deploy_status = as.factor(deploy_status)
  )

str(df)

# 1.6 Revisar valores fuera de rango / outliers evidentes
# test_coverage_pct debe estar en [0,100]
df %>% filter(test_coverage_pct < 0 | test_coverage_pct > 100)

# build_time_min y deploy_time_min no pueden ser negativos
df %>% filter(build_time_min <= 0 | deploy_time_min <= 0)

# Se identifican algunos build_time_min extremos (posibles builds
# colgados / reintentos). NO se eliminan sin justificar: se marcan
# como sospechosos para revisarlos en Fase 6 con boxplot, pero se
# conservan porque son valores reales (no errores de captura).
df <- df %>%
  mutate(build_time_outlier = build_time_min > (quantile(build_time_min, 0.75) +
                                                  1.5 * IQR(build_time_min)))
sum(df$build_time_outlier)

# 1.7 Guardar el dataset limpio para las siguientes fases
write_csv(df, "devops_metrics_clean.csv")


# ---- 2. FASE 2: Descriptiva univariada ------------------------

# Función auxiliar para no repetir código en cada variable cuantitativa
describir <- function(x, nombre) {
  tibble(
    variable = nombre,
    n        = sum(!is.na(x)),
    media    = mean(x, na.rm = TRUE),
    mediana  = median(x, na.rm = TRUE),
    moda     = as.numeric(names(sort(table(round(x, 0)), decreasing = TRUE))[1]),
    sd       = sd(x, na.rm = TRUE),
    varianza = var(x, na.rm = TRUE),
    cv       = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE),
    rango    = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    iqr      = IQR(x, na.rm = TRUE),
    asimetria = skewness(x, na.rm = TRUE),
    curtosis  = kurtosis(x, na.rm = TRUE) # kurtosis "normal" = 3 en {moments}
  )
}

vars_cuant <- c("build_time_min", "deploy_time_min", "commit_size_loc",
                "num_bugs", "test_coverage_pct", "ticket_resolution_h")

tabla_descriptiva <- map_dfr(vars_cuant, ~ describir(df[[.x]], .x))
print(tabla_descriptiva, width = Inf)

# 2.1 Interpretación rápida de asimetría y curtosis
# asimetria > 0  -> cola larga a la derecha (ej: commit_size_loc, num_bugs)
# asimetria ~ 0  -> distribución aprox. simétrica
# curtosis > 3   -> leptocúrtica (colas más "pesadas" que la normal)
# curtosis < 3   -> platicúrtica (colas más livianas)

# 2.2 Variables cualitativas: frecuencias
df %>% count(team, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(module, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(priority) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(deploy_status, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))

# 2.3 Guardar tabla descriptiva como insumo para el reporte final
write_csv(tabla_descriptiva, "tabla_descriptiva_fase2.csv")
