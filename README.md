# Análisis de métricas de software y DevOps

Proyecto integrador del curso de Probabilidad y Estadística Computacional. Analiza un histórico de ~5.000 eventos de integración/despliegue (`devops_metrics.csv`) mediante estadística descriptiva en R: limpieza de datos, medidas de tendencia central/dispersión/forma, tablas de frecuencia, comparación por grupos, relaciones bivariadas y visualización.

## Requisitos

- R (≥ 4.2) y RStudio
- Paquetes:
  ```r
  install.packages(c("tidyverse", "moments", "rmarkdown", "knitr"))
  ```

## Estructura del repositorio

| Archivo | Descripción |
|---|---|
| `proyecto-estadistica.Rproj` | Proyecto de RStudio. Ábrelo primero para que las rutas relativas funcionen. |
| `Analisis.R` | Script único con el desarrollo completo del proyecto (Fases 1 a 6: carga y limpieza, descriptiva univariada, frecuencias y agrupación, análisis por grupos, relaciones bivariadas y visualización). |
| `devops_metrics.csv` | Dataset original, sin procesar. |
| `README.md` | Este archivo. |

## Cómo ejecutar

1. Abrir `proyecto-estadistica.Rproj` en RStudio.
2. Abrir `Analisis.R`.
3. Ejecutar el script de principio a fin (o bloque por bloque para revisar cada resultado).
4. El script genera automáticamente `devops_metrics_clean.csv`, `tabla_descriptiva_fase2.csv` y los gráficos exportados.

## Entregables asociados

- Repositorio Git (este repo, con historial de commits por fase)
- Script de R (`Analisis.R`)
- Reporte reproducible (`Reporte.Rmd`)
- Bitácora de prompts (`Bitacora de prompts - Proyecto.txt`)
