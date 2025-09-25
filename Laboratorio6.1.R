#======================================================================

resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(13, 25, 20, 35, 45, 30, 60, 95)
)
resp
#=====================================================================
# Crear nuevas columnas con los rangos (1 a 8)

resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

# Ver resultado
resp
#======================================================================
# Graficos

plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_Tiempo, resp$Rango_Edad)

resp$dif <- resp$Rango_Tiempo - resp$Rango_Edad
resp$dis2 <- resp$dif^2
sum(resp$dis2)

cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")
cor.test(resp$Tiempo, resp$Edad, method = "spearman")

#======================================================================
# Correlacion Tall Kendall
tau <- data.frame(
  A = c(1, 2, 3, 4, 5, 6),
  B = c(3, 1, 4, 2, 6, 5)
)
cor.test(tau$A, tau$B, method = "kendall")

#=====================================================================
# Correlacion biserial
set.seed(123) #Para Reproducibilidad

# Número de observaciones
n <- 20
# Generar horas de estudio (Entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)

# Asignar probabilidad de aprobar en funcion de horas de estudio
# a mas horas, mas alta la probabilidad
Resultado <- sapply(Horas_estudio, function(horas){
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
})

# Crear data frame
estudio <- data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)

# Crear variable dicotomica: 1 = Apobado, 0 = Reprobado
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")
