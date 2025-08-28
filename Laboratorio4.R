# Pruebas de t
# caso de Muestras independientes
# VACA
# 27/08/2025

# Importar datos de Índice de calidad

calidad <-read.csv("Calidad_planta.csv", header = T)

calidad$Tratamiento <- as.factor(calidad$Tratamiento)

colores <- c("purple", "skyblue")
boxplot(calidad$IE ~ calidad$Tratamiento,
        col = colores,
        xlab = "Tratamientos",
        ylab = "Índice de calidad",
        ylim = c(0.4,1.2),
        main = "Vivero Iturbide")

# Estadistica descriptiva
# tapply sirve para obtener un valor cuando contamos
# con varios grupos

tapply(calidad$IE, calidad$Tratamiento, mean)
tapply(calidad$IE, calidad$Tratamiento, var)
tapply(calidad$IE, calidad$Tratamiento, sd)

# Observamos que la varianza del grupo Fertilizado (Fert) es 3 veces 
# más grande que el grupo Control (Ctrl)

# Revisar el comportamiento de los datos
library(ggplot2)

ggplot(calidad, aes(x = IE, color = Tratamiento))+geom_density()
ggplot(calidad, aes(x = IE, color = Tratamiento))+geom_histogram()

# Separar los datos por tratamiento
df_ctrl <- subset(calidad, Tratamiento == "Ctrl")
df_fert <- subset(calidad, Tratamiento != "Ctrl")

# qqnormt revisar normalidad
par(mfrow = c(1, 2))
qqnorm(df_ctrl$IE); qqline(df_ctrl$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)
par(mfrow = c(1,1))


# Prueba de normalidad 

shapiro.test(df_ctrl$IE)
shapiro.test(df_fert$IE)

# Revisar homogenialidad de varianzas
var.test(df_ctrl$IE, df_fert$IE)
var.test(calidad$IE ~ calidad$Tratamiento)

# Aplicar la prueba de t, varianzas iguales
# dos colas = two.sided

t.test(calidad$IE ~ calidad$Tratamiento,
       var.equal = T,
       alternative = "two.sided")

cohens_efecto <- function(x, y) {
  n1 <- length(x);n2 <- length(y)
  s1 <- sd(x);  s2 <-sd(y)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2 / (n1 + n2 - 2))
             (mean(x) - mean(y)) / sp
}
