# Laboratorio 5
# Víctor Andrés Corpus Aguirre, IF

# Un comprador de alimento compro 44 bolsas de 80 kg y el dice que peso las 44 bolsas 
# y no se cumplia con la media de 80 kg por lo que demando a la empresa

# Datos
costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23, 
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

# Pruebas estadisticas
mean(costal)        
var(costal)
sd(costal)

# Grafico
boxplot(costal)

# Prueba de T
t.test(costal, mu =80)

# Al final el comprador ganó la demanda ya que la empresa no cumplia con los
# controles de calidad y por eso no cumplian con los 80 kg