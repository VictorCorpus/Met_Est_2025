# Asignacion 3: Contraste de Medias
# Víctor Andrés Corpus Aguirre IF
# 03/09/2025

# El primer paso es encontrar la base de datos Iris en R
library(datasets)
data("iris")
summary(iris)
summary(iris$Petal.Length)
head(iris)
head(iris$Petal.Length)

# La funcion aggregate sirve para realizar un reumen por especie 
# la busque en internet para ordenar mejor los datos que 
# utlizamos y separsrlos de los que no se observan tanto.
aggregate(Petal.Length ~ Species, data =iris[iris$Species %in% c("versicolor", "virginica"),], summary)

# Prueba estadistica
# ¿Existen diferencias significativas entre las especies
# versicolor y virginica?

# Datos
iris_sub <- subset (iris, Species %in% c("versicolor", "virginica"))
versicolor <- iris_sub$Petal.Length[iris_sub$Species == "versicolor"]
virginica <- iris_sub$Petal.Length[iris_sub$Species == "virginica"]

# Funcion Shapiro para calcular la normalidad de los datos
shapiro.test(versicolor)
shapiro.test(virginica)

# Calcuar la varianza y analizar su homogenialidad
var.test(versicolor, virginica)

# Prueba de T
t.test(versicolor, virginica,
       var.equal = F,
       alternative = "two.sided")

# Efecto cohens
if(!require(effsize)) install.packages("effsize")
library(effsize)
cohen.d(versicolor, virginica)

# Graficos
boxplot(Petal.Length ~ Species, data = iris_sub,
        main = "Comparacion de PETAL.LENGTH",
        xlab = "Especie",
        ylab = "PETAL.LENGTH",
        col = c("lightblue", "lightgreen"))


