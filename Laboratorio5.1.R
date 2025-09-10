




sem <- read.csv("Kilo_sem.csv", header = T)
sem$Tiempo <- as.factor(sem$Tiempo)

tapply(sem$Kgsem, sem$Tiempo, mean)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col = "lightblue",
        xlab = "Año",
        ylab = "Semilla (Kg)")

t2012 <- subset(sem, sem$Tiempo =="T2012")
t2013 <- subset(sem, sem$Tiempo =="T2013")
t.test(t2012$Kgsem, t2013$Kgsem, paired = T)

t.test(t2012$Kgsem, t2013$Kgsem, paired = T, alternative = "less")
