#--------Estudio antropométrico de los alumnos de la Licenciatura 
#en Ciencias y Técnicas Estadísticas-------#
#Autor: "Linda Estefany Bravo López"
#Fecha: "23/11/2021"

#--------------Resumen--------------#
#Resumen: El presente trabajo se realizó con la finalidad de obtener 
#información de medidas antropométricas en alumnos de la facultad de 
#estadística en la región de Xalapa, fue un estudio transversal y observacional. 
#Así como realizar un análisis de regresión múltiple con los datos obtenidos. 
#Metodología: Se seleccionaron 58 alumnos de manera aleatoria a través de una 
#fórmula. De los cuales se valoraron datos antropométricos como lo son: peso, 
#estatura, envergadura, perímetro de cintura e ileospinal. Para la recolección 
#de los datos se utilizó: Un formato de registro en una hoja de cálculo de 
#Excel versión 2016 que contenían los datos, y para el análisis de los datos 
#obtenidos se utilizó el paquete estadístico R Project.
#Palabras clave: alumnos, análisis, regresión, medidas antropométricas.

#-------INTRODUCCIÓN--------#
#Los primeros conocimientos sobre la utilidad de las mediciones del cuerpo 
#humano se remontan a los inicios de la historia, generalmente como referencia 
#a la necesidad y utilidad de estas en la selección de las personas más idóneas 
#para la guerra o el trabajo, así como para valoraciones estéticas y 
#artísticas. (LINO CARMENATE MILIÁN, 2014)

#La antropometría fue presentada como una ciencia en 1976, en el Congreso 
#Internacional de las Ciencias de la Actividad Física, celebrado en Montreal,
#y 2 años después fue aceptada como ciencia por la UNESCO, en el International 
#Council of Sport and Physical Education.

#Como referencia principal para la localización de los puntos antropométricos 
#y la toma de las medidas deberá utilizar el Manual de la I.S.A.K (ISAK,2001)

#OBJETIVO
#Estudiar la relación del peso de los alumnos de la carrera de Ciencias y 
#Técnicas Estadísticas respecto a su edad, altura, envergadura, perímetro de 
#cintura e ileospinal.

#METODOLOGÍA
#Tipo de investigación
#Por su enfoque el estudio es transversal, porque todas las variables 
#fueron medidas en un solo momento.
#Descriptivo ya que solo se describen o estiman parámetros en la población de 
#estudio a partir de una muestra.

#Población y muestra
#Población. Estudiantes adscritos en el periodo Agosto 2017- Enero 2018 a 
#la Licenciatura en Ciencias y Técnicas Estadísticas en la Facultad de 
#Estadística e Informática de la Universidad Veracruzana.
#Muestra. Conformada por 58 estudiantes de la Licenciatura en Ciencias y 
#Técnicas Estadística.
#Técnicas de muestreo. Muestreo aleatorio simple para recolectar la información.


#Para analizar los datos obtenidos primero importamos la base de datos. 
d1<-read_csv("C:/Users/Linda/Desktop/proyecto 2.1.csv")
d1
d1<-d1[,-1]
d1<-d1[,-1]
#visualización del nombre de las variables
names(d1)


#Para obtener la matriz de correlación 
cor(d1)

#Para obtener el gráfico de la matriz de correlación.
plot(d1)


#Para obtener los gráficos de cajas y alambres de las variables.
boxplot(d1,ylab="Frecuencias",col="green",main="Grafico de cajas",col.main="brown")

#En el gráfico anterior no se observa variabilidad entre **Amplitud**
#y **Estatura** por lo que se obtendrá un gráfico de cajas y alambres sin estas variables.

boxplot(d1[,-3][,-4],ylab="Frecuencias",col="green",main="Grafico de cajas sin amplitud y estatura",col.main="Brown")

#Para obtener los estadísticos descriptivos
summary(d1)

#Para obtener los gráficos de cajas y alambre de la variable peso respecto a 
#las demás variables.
x11()
par(mfrow=c(2,3))
boxplot(d1$PESO~d1$EDAD,ylab="Peso",xlab="Edad",main="Peso vs Edad ",col.main="brown",col="green")
boxplot(d1$PESO~d1$ESTATURA,ylab="Peso",xlab="Estatura",main="Peso vs Estaturaa ",col.main="brown",col="green")
boxplot(d1$PESO~d1$CINTURA,ylab="Peso",xlab="Cintura",main="Peso vs Cintura ",col.main="brown",col="green")
boxplot(d1$PESO~d1$AMPLITUD,ylab="Peso",xlab="Amplitu de brazos",main="Peso vs Amplitud ",col.main="Brown",col="green")
boxplot(d1$PESO~d1$OMBLIGO,ylab="Peso",xlab="Distancia del ombligo al suelo",main="Peso vs Distancia ",col.main="Brown",col="green")
#se propone el primer modelo de regresión lineal multiple con todas las variables.
fit<-lm(PESO~EDAD+ESTATURA+CINTURA+AMPLITUD+OMBLIGO,data=d1)
fit

# se observan las variables que más aportan a la variable peso.
summary(fit)
l<-aov(fit)
l
summary(fit)

#Se obtiene el gráfico de residuales y normalidad para comprobar que se 
#cumplan los supuestos.
x11()
par(mfrow=c(2,2))
plot(fit)

#Prueba de Shapiro y Bartlett
shapiro.test(fit$residuals)
bartlett.test(d1)


