# Examen Parcial II
# Fecha: 22 / 10 / 2025
# Alumno: Víctor Andrés Corpus Aguirre

# Datos
# Sacamos los datos de Tropenbos Cameron Programme (TCP)
suelo <- read.csv(url("https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1"))

#Variables
suelo$zone <- as.factor(suelo$zone)
suelo$wrb <- as.factor(suelo$wrb)

head(suelo)

# ----------------------------------------------------------
# Actividad 1

# P1. ¿Cual es ña tendencia del contenido promedio de Arcilla
# con repecto a la profundidad?

summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)

# Promedio general de todas las capas
colMeans(suelo[, c("Clay1", "Clay2", "Clay5")])

# ------------------------------------------------------------
# Actividad 2

boxplot(suelo$Clay1,
        main = "Contenido de Arcilla (0–10 cm)",
        ylab = "% de Arcilla",
        col = "green")
# P2. ¿Existe evidencia de outliers? R= Si 
# Detección de outliers

#Para conocer un poco mas de los outliers y como filtrarlos use la funion de "help"
help
outliers_Clay1 <- boxplot.stats(suelo$Clay1)$out
outliers_Clay1
# P3. Identificar observaciones con outliers
suelo[outliers_Clay1 %in% suelo$Clay1, ]
# Filtrar valores 

suelo_filtrado <- suelo[suelo$Clay1 < 60, ]
summary(suelo_filtrado$Clay1)

# ------------------------------------------------------------
# Actividad 3

# P4. Comparación de la media observada con 30%
mean(suelo_filtrado$Clay1)
t.test(suelo_filtrado$Clay1, mu = 30)

# ------------------------------------------------------------
# Actividad 4

# P5. Relación entre Clay1 y Clay5
plot(suelo$Clay1, suelo$Clay5,
     main = "Relación entre Clay1 y Clay5",
     xlab = "Clay1 (%)",
     ylab = "Clay5 (%)",
     pch = 19, col = "green")
# Existe una relacion  es muy buena y  positiva entre los datos

# P7. Correlación entre perfiles
cor.test(suelo$Clay1, suelo$Clay5)
# La correlacion es significativa

# -----------------------------------------------------------
# Actividad 5
# P6. ¿Existe una forma de identificar la variacion entre las cuatro zonas 
# que se encuentran en el estudio? R= Si yo en este caso use un boxplot y
# pude identificar como varia el porcentaje de 
# arcilla en cada una de las 4 zonas


# P7. Variación del contenido de arcilla (30–50 cm) entre zonas
boxplot(Clay5 ~ zone, data = suelo,
        main = "Contenido de Arcilla (30–50 cm) por Zona",
        xlab = "Zona",
        ylab = "% de Arcilla",
        col = "green")
# Dependiendo de la zona hay mas o menos arcilla, por ejemplo en la zona 1 y 2 
# es donde mas porcentaje hay mientras que en la 3 y 4 es donde menos
# se encuentra

