
#==============================================================================
# Asignacion 5: Correlacion
# Victor Andrés Corpus Aguirre IF
# 02/10/2025
#==============================================================================
ANALISIS DE CORRELACION - R SCRIPT
# ===============================
# EJERCICIO 1: Efímeras y velocidad del arroyo
# Datos del cuadro 1
speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)
# Scatter plot
plot(speed, abundance, 
     main="Relación entre velocidad del arroyo y abundancia de efímeras",
     xlab="Velocidad del arroyo", ylab="Abundancia de efímeras",
     pch=19, col="green")
# Correlación de Pearson
cor_test1 <- cor.test(speed, abundance, method="pearson")
print(cor_test1)
## 
## Pearson's product-moment correlation
## 
## data: speed and abundance
## t = 3.8568, df = 6, p-value = 0.008393
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## 0.3442317 0.9711386
## sample estimates:
## cor 
## 0.8441408
# Resultados
cat("\nEjercicio 1 - Correlación de Pearson\n")
## 
## Ejercicio 1 - Correlación de Pearson
cat("r =", cor_test1$estimate, "\n")
## r = 0.8441408
cat("p-valor =", cor_test1$p.value, "\n")
## p-valor = 0.008393413
cat("Grados de libertad =", cor_test1$parameter, "\n")
## Grados de libertad = 6
# EJERCICIO 2: Suelo (pH vs otras variables)
# Datos del cuadro 2
datos <- data.frame(
  Gp = c("T0","T0","T0","T0","T1","T1","T1","T1"),
  Block = c(5.40,5.65,5.14,5.14,5.14,5.10,5.10,4.70),
  pH = c(5.40, 5.65, 5.14, 5.14, 5.14, 5.10, 5.10, 4.70),
  N = c(0.188,0.165,0.260,0.169,0.164,0.164,0.091,0.100),
  Dens = c(0.92,1.04,0.95,1.10,1.12,1.22,1.22,1.52),
  P = c(215,125,300,225,174,129,162,117),
  Ca = c(16.35,12.55,13.02,15.22,14.17,8.55,8.55,8.74),
  Mg = c(7.65,5.15,5.68,7.88,8.12,6.92,6.92,8.16),
  K = c(0.72,0.71,0.68,1.01,0.70,0.81,2.67,0.39),
  Na = c(1.14,0.94,0.60,1.27,0.90,2.17,3.18,3.32),
  Condu = c(1.09,1.35,1.41,1.64,1.85,1.64,3.18,4.16)
)
# Seleccionamos solo variables numéricas
datos_num <- datos[, c("pH","N","Dens","P","Ca","Mg","K","Na","Condu")]
# Correlación de pH con otras variables
resultados <- data.frame(
  Conjunto = character(),
  r = numeric(),
  p_valor = numeric()
)
for (var in names(datos_num)[-1]) {
  test <- cor.test(datos_num$pH, datos_num[[var]], method="pearson")
  resultados <- rbind(resultados, 
                      data.frame(Conjunto = paste("pH -", var),
                                 r = test$estimate,
                                 p_valor = test$p.value))
}
print(resultados)
## Conjunto r p_valor
## cor pH - N 0.382988032 0.34902822
## cor1 pH - Dens -0.775918057 0.02361381
## cor2 pH - P 0.094722016 0.82345570
## cor3 pH - Ca 0.514565766 0.19196819
## cor4 pH - Mg -0.601648535 0.11457784
## cor5 pH - K -0.001340161 0.99748720
## cor6 pH - Na -0.651582450 0.08003433
## cor7 pH - Condu -0.771536199 0.02493725
# Opcional: Mapa de correlaciones entre todas las variables
library(corrplot)
## corrplot 0.95 loaded
M <- cor(datos_num)
corrplot(M, method="circle", type="upper", 
         tl.col="green", tl.cex=0.8,
         title="Matriz de correlaciones del suelo")
# ====================================================
# Ejercicio 3: El cuarteto de Anscombe
# ====================================================
# Cargar datos del Cuadro 5
x <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y1 <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 
        5.68)
y2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
y3 <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 
        5.73)
y4 <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 
        6.89)
# ====================================================
# Gráficas de los cuatro conjuntos
# ====================================================
par(mfrow=c(2,2)) # Ventana gráfica en 2 filas y 2 columnas
plot(x, y1, main="Conjunto I", pch=19, col="green")
abline(lm(y1 ~ x), col="blue")
plot(x, y2, main="Conjunto II", pch=19, col="green")
abline(lm(y2 ~ x), col= "blue")
plot(x, y3, main="Conjunto III", pch=19, col="green")
abline(lm(y3 ~ x), col="blue")
plot(x, y4, main="Conjunto IV", pch=19, col="green")
abline(lm(y4 ~ x), col="blue")
par(mfrow=c(1,1)) # Restablecer formato de gráficos
# ====================================================
# Ejercicio III: Estadísticas descriptivas
# ====================================================
cat("Resumen del Cuarteto de Anscombe:\n")
## Resumen del Cuarteto de Anscombe:
for (i in 1:4) {
  yi <- get(paste0("y", i))
  cat(paste("\nConjunto", i, ":\n"))
  cat("Media de x:", mean(x), "\n")
  cat("Media de y:", mean(yi), "\n")
  cat("Varianza de x:", var(x), "\n")
  cat("Varianza de y:", var(yi), "\n")
  cat("Correlación:", cor(x, yi), "\n")
  modelo <- lm(yi ~ x)
  cat("Regresión lineal: y =", round(coef(modelo)[1],3), "+", 
      round(coef(modelo)[2],3), "* x\n")
  cat("R²:", round(summary(modelo)$r.squared,3), "\n")}
## 
## Conjunto 1 :
## Media de x: 9 
## Media de y: 7.500909 
## Varianza de x: 11 
## Varianza de y: 4.127269 
## Correlación: 0.8164205 
## Regresión lineal: y = 3 + 0.5 * x
## R²: 0.667 
## 
## Conjunto 2 :
## Media de x: 9 
## Media de y: 7.500909 
## Varianza de x: 11 
## Varianza de y: 4.127629 
## Correlación: 0.8162365 
## Regresión lineal: y = 3.001 + 0.5 * x
## R²: 0.666 
## 
## Conjunto 3 :
## Media de x: 9 
## Media de y: 7.5 
## Varianza de x: 11 
## Varianza de y: 4.12262 
## Correlación: 0.8162867 
## Regresión lineal: y = 3.002 + 0.5 * x
## R²: 0.666 
## 
## Conjunto 4 :
## Media de x: 9 
## Media de y: 7.500909 
## Varianza de x: 11 
## Varianza de y: 4.123249 
## Correlación: -0.3140467 
## Regresión lineal: y = 9.231 + -0.192 * x
## R²: 0.099