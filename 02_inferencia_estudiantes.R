# ==============================================================================
# PROYECTO: Estudio Estadístico del Uso de Redes Sociales en Estudiantes
# SCRIPT:   02_inferencia_estudiantes.R
# OBJETIVO: Estadística Inferencial (Correlaciones, Pruebas de Hipótesis, Regresión)
# ==============================================================================

# 1. Cargar librerías necesarias ----------------------------------------------
paquetes <- c("tidyverse", "corrplot", "car", "broom", "ggpubr")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos) > 0) install.packages(nuevos)

library(tidyverse)
library(corrplot)
library(car)      # Diagnóstico de regresión (VIF, Durbin-Watson)
library(broom)    # Salidas ordenadas tipo tibble de modelos
library(ggpubr)   # Visualización de supuestos y pruebas estadísticas

# 2. Cargar datos procesados ---------------------------------------------------
df <- readRDS("output/estudiantes_clean.rds")

# ==============================================================================
# MÓDULO 1: ANÁLISIS DE CORRELACIÓN BIVARIADA
# Pregunta: ¿Existe correlación lineal entre uso diario, horas de sueño,
# conflictos y adicción?
# ==============================================================================

# Seleccionar solo variables cuantitativas
vars_num <- df %>%
  select(
    avg_daily_usage_hours,
    sleep_hours_per_night,
    mental_health_score,
    conflicts_over_social_media,
    addicted_score
  )

# Matriz de correlación de Pearson y matriz de p-valores
matriz_corr <- cor(vars_num, method = "pearson", use = "complete.obs")
cat("\n--- MATRIZ DE CORRELACIÓN DE PEARSON ---\n")
print(round(matriz_corr, 3))

# Gráfico de correlación
png("output/01_matriz_correlacion.png", width = 800, height = 700, res = 120)
corrplot(
  matriz_corr,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Matriz de Correlación Bivariada",
  mar = c(0, 0, 2, 0)
)
dev.off()

# Test formal de correlación entre uso diario y horas de sueño
test_cor_sueno <- cor.test(df$avg_daily_usage_hours, df$sleep_hours_per_night)
cat("\n--- TEST DE CORRELACIÓN (Uso Diario vs Horas de Sueño) ---\n")
print(test_cor_sueno)

# ==============================================================================
# MÓDULO 2: PRUEBAS DE HIPÓTESIS
# ==============================================================================

# ------------------------------------------------------------------------------
# Hipótesis 1 (Comparación de dos medias independientes):
# H0: La media de horas de sueño es igual entre estudiantes que perciben
#     afectación en su rendimiento académico y los que no.
# H1: Los estudiantes con afectación duermen en promedio menos horas.
# ------------------------------------------------------------------------------

# Comprobación de normalidad por grupo (Shapiro-Wilk o muestra grande n > 30 CLT)
shapiro_afecta <- shapiro.test(df$sleep_hours_per_night[df$affects_academic_performance == "Yes"])
shapiro_no_afecta <- shapiro.test(df$sleep_hours_per_night[df$affects_academic_performance == "No"])

# Comprobación de homocedasticidad (Igualdad de varianzas: Test de Levene)
levene_sueno <- car::leveneTest(sleep_hours_per_night ~ affects_academic_performance, data = df)
cat("\n--- TEST DE LEVENE (HOMOCEDASTICIDAD DE VARIANZAS) ---\n")
print(levene_sueno)

# Prueba t de Student para muestras independientes (Welch robusta a varianzas desiguales)
t_test_sueno <- t.test(
  sleep_hours_per_night ~ affects_academic_performance,
  data = df,
  alternative = "two.sided",
  var.equal = FALSE
)
cat("\n--- PRUEBA T DE WELCH (HORAS DE SUEÑO SEGÚN RENDIMIENTO) ---\n")
print(t_test_sueno)

# ------------------------------------------------------------------------------
# Hipótesis 2 (ANOVA de una vía):
# H0: La puntuación media de adicción es igual en todos los niveles académicos.
# H1: Al menos un nivel académico presenta una media de adicción distinta.
# ------------------------------------------------------------------------------
modelo_anova <- aov(addicted_score ~ academic_level, data = df)
cat("\n--- TABLA ANOVA (ADICCIÓN ~ NIVEL ACADÉMICO) ---\n")
print(summary(modelo_anova))

# Si p < 0.05, prueba post-hoc de Tukey:
cat("\n--- PRUEBA POST-HOC DE TUKEY HSD ---\n")
print(TukeyHSD(modelo_anova))

# ------------------------------------------------------------------------------
# Hipótesis 3 (Independencia de variables cualitativas - Chi-cuadrado):
# H0: El género y la percepción de afectación académica son independientes.
# H1: Existe asociación entre el género y la afectación académica percibida.
# ------------------------------------------------------------------------------
tabla_contingencia <- table(df$gender, df$affects_academic_performance)
chi_test <- chisq.test(tabla_contingencia)
cat("\n--- TABLA DE CONTINGENCIA Y TEST CHI-CUADRADO ---\n")
print(tabla_contingencia)
print(chi_test)

# ==============================================================================
# MÓDULO 3: MODELO DE REGRESIÓN LINEAL MÚLTIPLE
# Objetivo: Modelar la puntuación de adicción (addicted_score) a partir del uso
# diario, salud mental, horas de sueño y conflictos en redes.
# ==============================================================================

modelo_regresion <- lm(
  addicted_score ~ avg_daily_usage_hours + sleep_hours_per_night + 
    mental_health_score + conflicts_over_social_media,
  data = df
)

cat("\n--- RESUMEN DEL MODELO DE REGRESIÓN LINEAL ---\n")
print(summary(modelo_regresion))

# Coeficientes en formato ordenado con intervalos de confianza
cat("\n--- COEFICIENTES ESTIMADOS E IC (95%) ---\n")
coef_ordenados <- tidy(modelo_regresion, conf.int = TRUE)
print(coef_ordenados)

# Diagnóstico de Multicolinealidad (Factor de Inflación de la Varianza - VIF)
cat("\n--- FACTORES DE INFLACIÓN DE LA VARIANZA (VIF) ---\n")
vif_valores <- vif(modelo_regresion)
print(vif_valores) # Si VIF < 5, no hay problemas severos de multicolinealidad

# 4. Diagnóstico de supuestos de los residuos ----------------------------------
png("output/02_diagnostico_residuos.png", width = 900, height = 700, res = 120)
par(mfrow = c(2, 2))
plot(modelo_regresion)
par(mfrow = c(1, 1))
dev.off()

# Test formal de normalidad de residuos
shapiro_residuos <- shapiro.test(residuals(modelo_regresion)[1:min(length(residuals(modelo_regresion)), 5000)])
cat("\n--- PRUEBA DE NORMALIDAD DE RESIDUOS (SHAPIRO-WILK) ---\n")
print(shapiro_residuos)

# Guardar los objetos y modelos para el reporte final RMarkdown
save(
  matriz_corr,
  test_cor_sueno,
  t_test_sueno,
  modelo_anova,
  chi_test,
  modelo_regresion,
  coef_ordenados,
  file = "output/modelos_inferenciales.RData"
)
cat("\nTodos los modelos inferenciales se guardaron en 'output/modelos_inferenciales.RData'\n")

