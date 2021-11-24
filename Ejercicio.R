# -----------------------------------------------
#                 EJERCICIO
# -----------------------------------------------

# 1.- Lectura de datos
datos <-datasets::rivers

# Si requiere información adicional sobre la matriz,
# utiliza el comando de ayuda.
? rivers

# 2.- Anota el comando y respondedor las preguntas

# a) Genera el histograma.

library(stats)
library(nortest)
hist(rivers, col="pink", main="Histograma", ylab="Frecuencia", xlab="Rivers")


# b) ¿Cuántas observaciones tiene la matriz de datos "rivers"?
View(datos)
#Cuenta con 141 observaciones.

# c) ¿Qué prueba de normalidad vas a emplear?
#Se empleará la pruena de normalidad de KOLMOGOROV-SMIRNOV ya que se observan
#más de 50 datos.

# d) Realiza la prueba de normalidad.

lillie.test(rivers)

# e) Anota la interpretación.# -----------------------------------------------


*p-valor*= 0.000000000000000022, es **menor** a 0.05. Por lo tanto, **RECHAZO Ho**.
Los datos siguen una distribución diferente a la normal.