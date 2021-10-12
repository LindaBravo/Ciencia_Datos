#_________________________________________________________
# Busqueda de datos perdidos con el paquete "MICE"
#_________________________________________________________
#Instalacion de paquetes con dependencias
install.packages("mice", dependencies = TRUE)
#Se abre libreria
library(mice)
#Usamos la funcion md.pattern()
md.pattern(iris)
#Exportamos el grafico y se guarda en la carpeta de R que se encuentra en el 
#escritorio