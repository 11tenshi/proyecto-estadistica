# Análisis de métricas de software y DevOps
# Nombre: Yicela Jorquera

# importar paquetes necesarios para usar
library(tidyverse)
library(moments)

# FASE 1: Carga y limpieza

# crear un dataframe "df" que carga los datos del csv generado
df <- read_csv("devops_metrics.csv")

# inspeccion de datos preliminar, glimpse para ver tipos de dato y summary para ver los estadisticos
glimpse(df)

summary(df)

# revision de registros duplicados, la salida da el numero de filas repetidas
sum(duplicated(df))

# eliminar filas repetidas para dejar registros únicos
df <- df %>% distinct()

# revisar la cantidad de valores faltantes por cada columna (la salida es un numero por columna)
colSums(is.na(df))

# Decisión de limpieza:

# se hace un group_by para tratar los datos como grupos separados, para calcular por grupo y no por la tabla completa
# mutate para crear o reemplazar una columna, fila por fila
# se valida si la celda es NA, si es asi entonces se reemplaza el valor por la mediana de su grupo en vez de eliminar
# ungroup para no arrastrar los agrupamientos que hicimos recien a operaciones futuras

# se usó la mediana en vez del promedio ya que no se arrastra por valores extremos como si lo hace el promedio

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

# verificar que no queden valores "NA"
colSums(is.na(df))

# correccion de tipos de dato por factores, agregandole nivel a las prioridaes
# es importante en este caso porque los niveles de la columna priority tienen un orden logico que debe respetarse
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

# mantener solo valores que están dentro de un rango
# test_coverage_pct debe estar en [0,100]
df %>% filter(test_coverage_pct < 0 | test_coverage_pct > 100)

# build_time_min y deploy_time_min no pueden ser negativos
df %>% filter(build_time_min <= 0 | deploy_time_min <= 0)


# tomando la regla de Tukey, consideramos que cualquier valor que supere el 
# tercer cuartil Q3 más 1.5 veces el rango intercuartílico IQR se considera atípico
# no los eliminamos pero se marcan como sospechosos para revisarlos en Fase 6 con boxplot; se
# conservan porque son valores reales, no errores

df <- df %>%
  mutate(build_time_outlier = build_time_min > (quantile(build_time_min, 0.75) +
                                                  1.5 * IQR(build_time_min)))
sum(df$build_time_outlier)

# guardar el dataset limpio para la siguiente fase de procesamiento
write_csv(df, "devops_metrics_clean.csv")


# FASE 2: Descriptiva univariada

# la idea en esta fase es escribir cada variable con las tres familias: tendencia central, dispersión y forma
# (asimetría y curtosis).

# aca se define una funcion auxiliar para no repetir código en cada variable cuantitativa
# la cual devuelve una fila con todas las metricas calculadas
# el na.rm lo que hace es ignorar los NA al realizar cálculos (a pesar de que ya fueron limpiados en F1, es por seguridad)

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

# aplicar la funcion definida a varias variables
vars_cuant <- c("build_time_min", "deploy_time_min", "commit_size_loc",
                "num_bugs", "test_coverage_pct", "ticket_resolution_h")

tabla_descriptiva <- map_dfr(vars_cuant, ~ describir(df[[.x]], .x))
print(tabla_descriptiva, width = Inf)

# interpretacion de asimetria y curtosis
# asimetria > 0  -> cola larga a la derecha (ej: commit_size_loc, num_bugs)
# asimetria ~ 0  -> distribución aprox. simétrica
# curtosis > 3   -> leptocúrtica (colas más "pesadas" que la normal)
# curtosis < 3   -> platicúrtica (colas más livianas)

# variables cualitativas: tablas de frecuencia
df %>% count(team, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(module, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(priority) %>% mutate(pct = round(100 * n / sum(n), 1))
df %>% count(deploy_status, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))

# guardar tabla descriptiva para el reporte final
write_csv(tabla_descriptiva, "tabla_descriptiva_fase2.csv")


# FASE 3

k <- ceiling(1 + 3.322 * log10(nrow(df)))

df %>%
  mutate(clase = cut(build_time_min, k)) %>%
  count(clase, name = "fa") %>%
  mutate(fr = fa / sum(fa),
         fac = cumsum(fa))

# FASE 4

df %>%
  group_by(team) %>%
  summarise(
    build_md = median(build_time_min),
    bugs = mean(num_bugs),
    fallos = mean(deploy_status == "failed"))

# FASE 5

num <- df %>% select(where(is.numeric))
cor(num, use = "complete.obs") %>% round(2)

cor(df$commit_size_loc, df$num_bugs)

prop.table(table(df$priority, df$deploy_status), margin = 1)


# FASE 6

ggplot(df, aes(build_time_min)) +
  geom_histogram(bins = 20)

ggplot(df, aes(team, ticket_resolution_h)) +
  geom_boxplot()

ggplot(df, aes(team, fill = deploy_status)) +
  geom_bar(position = "fill")

ggplot(df, aes(commit_size_loc, num_bugs)) +
  geom_point()

ggplot(df, aes(build_time_min)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribución del tiempo de build",
       x = "Tiempo de build (minutos)",
       y = "Frecuencia")