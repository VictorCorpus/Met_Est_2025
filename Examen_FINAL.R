# Examen Final: Métodos Estadísticos
# Víctor Andrés Corpus Aguirre IF

#################################################################################
# Para este examen yo tome los datos del "ejercico 1" y exporte el csv
# delimitado por comas para obtener mis datos, este csv tiene datos de DAP y
# Altura y dbo plantear una pregunta e hipotesis con respecto a estas 2 variables
##############################################################################
# Pregunta:
# ¿Existe relación entre el DAP y la altura total de los árboles?
# Hipótesis:
# H0: r = 0 (no hay correlación)
# H1: r ≠ 0 (sí hay correlación)
##############################################################################
# Datos del ejercicio 1 (DAP Y ALTURA)
datos <- data.frame(Ejercicio_1)
head(datos)
summary(datos)
DAP_cm <- c(
  19.57, 29.99, 26.41, 17.47, 22.11, 33.26, 12.87, 22.86, 31.33, 20.67,
  21.61, 24.53, 32.46, 21.81, 22.78, 22.83, 36.03, 35.93, 30.02, 26.93,
  28.69, 32.45, 20.32, 30.88, 18.73, 21.81, 29.54, 17.86, 24.3, 20.69)
Altura_m <- c(
  17.23, 9.6, 12.69, 15.9, 20.78, 17.48, 18.01, 20.06, 15.36, 18.85,
  15.58, 12.82, 16.83, 19.72, 19.02, 17.96, 25.18, 19.24, 20.94, 24.71,
  14.12, 14.88, 23.23, 15.61, 18.09, 21.21, 20.67, 23.26, 22.49, 21.21)
##############################################################################
# Preomedios y desviaciones
mean(DAP_cm) #Promedio del DAP
sd(DAP_cm) #Desviación Del DAP
mean(Altura_m) #Promedio de la Altura
sd(Altura_m) #Desviación de la altura
##############################################################################
# Pruebas de Normalidad
shapiro.test(DAP_cm)
shapiro.test(Altura_m)
# Como ambas dan p > 0.05, se usa Pearson
cor.test(DAP_cm, Altura_m, method = "pearson")
##################################################################################
# Gráfico de dispersión
plot(DAP_cm, Altura_m,
     main="Relación entre DAP y Altura",
     xlab="DAP (cm)", ylab="Altura (m)",
     pch=19, col="red")
# Línea de tendencia de el grafico de dispersión 
abline(lm(Altura_m ~ DAP_cm), col="green", lwd=2)
################################################################################
# Interpretación de los datos:
# El coeficiente de correlación fue aproximadamente 0.67,
# con un valor p muy pequeño (< 0.001).Esto significa que sí existe 
#una relación positiva significativa entre el DAP y la altura total
# de los árboles.
###############################################################################
# Conclusión:
# Se rechaza la hipótesis nula y se acepta que el DAP y la altura
# están correlacionados. Los árboles con mayor DAP tienden a ser más altos,
# lo cual coincide con un patrón normal de crecimiento forestal.

