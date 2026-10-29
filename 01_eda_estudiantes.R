---
title: '"Estudio Estadístico sobre el Impacto del Uso de Redes Sociales en Estudiantes"'
author: '"Estudiante de Análisis Estadístico"'
date: "2026-10-29"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# ==============================================================================
# PROYECTO: Estudio Estadístico del Uso de Redes Sociales en Estudiantes
# SCRIPT:   01_eda_estudiantes.R
# OBJETIVO: Carga, estandarización de nombres, tipado y EDA inicial
# ==============================================================================

# 1. Instalación y carga de librerías -----------------------------------------
paquetes <- c("tidyverse", "janitor", "skimr", "summarytools")
nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos_paquetes) > 0) {
  install.packages(nuevos_paquetes)
}

library(tidyverse)
library(janitor)
library(skimr)

# 2. Carga del conjunto de datos ----------------------------------------------
# El archivo debe estar ubicado en data/4. Estudiantes.csv
ruta_datos <- "data/4. Estudiantes.csv"

# Lectura del CSV
estudiantes_raw <- read_csv(ruta_datos, show_col_types = FALSE)

# 3. Limpieza de nombres y estandarización (snake_case) ------------------------
# Convierte variables como 'Avg_Daily_Usage_Hours' a 'avg_daily_usage_hours'
estudiantes <- estudiantes_raw %>%
  clean_names()

# Inspección de los nuevos nombres de columnas
print("Variables disponibles:")
names(estudiantes)

# 4. Ajuste y tipado de variables ---------------------------------------------
estudiantes_clean <- estudiantes %>%
  mutate(
    # Factores nominales
    gender = as.factor(gender),
    country = as.factor(country),
    most_used_platform = as.factor(most_used_platform),
    affects_academic_performance = as.factor(affects_academic_performance),
    relationship_status = as.factor(relationship_status),
    
    # Factor ordinal para nivel académico
    academic_level = factor(
      academic_level,
      levels = c("High School", "Undergraduate", "Graduate"),
      ordered = TRUE
    ),
    
    # Variables numéricas enteras y continuas
    student_id = as.integer(student_id),
    age = as.integer(age),
    conflicts_over_social_media = as.integer(conflicts_over_social_media),
    mental_health_score = as.integer(mental_health_score),
    addicted_score = as.integer(addicted_score)
  )

# 5. Auditoría de calidad de datos --------------------------------------------
cat("\n--- DIMENSIONES DEL DATASET ---\n")
dim(estudiantes_clean)

cat("\n--- VALORES FALTANTES POR COLUMNA ---\n")
colSums(is.na(estudiantes_clean))

cat("\n--- ESTRUCTURA INTERNA ---\n")
glimpse(estudiantes_clean)

# 6. Resumen estadístico descriptivo -----------------------------------------
cat("\n--- RESUMEN GLOBAL (SKIMR) ---\n")
skim(estudiantes_clean)

# Resumen de variables numéricas clave
resumen_numericas <- estudiantes_clean %>%
  summarise(
    media_uso = mean(avg_daily_usage_hours),
    sd_uso = sd(avg_daily_usage_hours),
    mediana_uso = median(avg_daily_usage_hours),
    media_sueno = mean(sleep_hours_per_night),
    sd_sueno = sd(sleep_hours_per_night),
    media_salud_mental = mean(mental_health_score),
    media_adiccion = mean(addicted_score)
  )
print(resumen_numericas)

# 7. Exploración gráfica preliminar -------------------------------------------
# Configurar tema base para visualizaciones
theme_set(theme_minimal(base_size = 12))

# A. Distribución del uso diario de redes sociales
p1 <- ggplot(estudiantes_clean, aes(x = avg_daily_usage_hours)) +
  geom_histogram(binwidth = 0.5, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(avg_daily_usage_hours)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución de Horas Diarias de Uso de Redes",
    subtitle = "Línea punteada: media muestral",
    x = "Horas diarias promedio",
    y = "Frecuencia"
  )
print(p1)

# B. Uso diario por plataforma más utilizada
p2 <- ggplot(estudiantes_clean, aes(x = reorder(most_used_platform, avg_daily_usage_hours, FUN = median), 
                                    y = avg_daily_usage_hours, 
                                    fill = most_used_platform)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Uso Diario según Plataforma Principal",
    x = "Plataforma",
    y = "Horas diarias"
  )
print(p2)

# C. Relación entre horas de uso y horas de sueño
p3 <- ggplot(estudiantes_clean, aes(x = avg_daily_usage_hours, y = sleep_hours_per_night)) +
  geom_point(aes(color = affects_academic_performance), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid") +
  labs(
    title = "Horas de Uso Diario vs. Horas de Sueño",
    subtitle = "Agrupado por percepción de afectación académica",
    x = "Horas diarias en redes sociales",
    y = "Horas de sueño por noche",
    color = "¿Afecta rendimiento?"
  )
print(p3)

# D. Relación entre salud mental y nivel de adicción
p4 <- ggplot(estudiantes_clean, aes(x = addicted_score, y = mental_health_score)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, color = "#d95f02") +
  geom_smooth(method = "loess", color = "#7570b3") +
  labs(
    title = "Puntuación de Salud Mental vs. Nivel de Adicción",
    x = "Nivel de adicción (1-10)",
    y = "Puntaje de salud mental (1-10)"
  )
print(p4)

# 8. Guardar datos procesados para los siguientes scripts ---------------------
dir.create("output", showWarnings = FALSE)
saveRDS(estudiantes_clean, file = "output/estudiantes_clean.rds")
cat("\nDataset limpio guardado exitosamente en 'output/estudiantes_clean.rds'\n")
```
