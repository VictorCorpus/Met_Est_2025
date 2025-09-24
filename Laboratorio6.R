#======================================================
# Correlación de Pearson
# Datos del geyser Old Faithful
# 24/09/2025
#======================================================

data("faithful")

plot(faithful$waiting, faithful$eruptions,
     xlab = "Tiempo de espera (min)",
     ylab = "Erupcion",
     col = "purple",
     pch = 20)

# Correlacionar las dos variables

shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)

# Pearson solo se utiliza cuando tenemos datos normales
#======================================================
cor.test(faithful$waiting, faithful$eruptions,
         method = "pearson")



#======================================================
# Sperman se utiliza como contraparte para datos no normales
#======================================================

cor.test(faithful$waiting, faithful$eruptions,
         method = "spearman")

