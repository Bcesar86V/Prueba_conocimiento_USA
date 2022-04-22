install.packages("readxl")
library(readxl)
library(dplyr)

#Cargando los datos
base<-read_xls("../Downloads/Distancia.xls")
#Analizando valores NAN, NA, 
my_na<-is.na(base)
sum(my_na)
#No hay presencia de valores NA

my_nan<-sum(is.nan(base$prueba_conocimiento))
my_nan
my_nan2<-sum(is.nan(base$salario))
my_nan2
my_nan3<-sum(is.nan(base$desempleo))
my_nan3
my_nan4<-sum(is.nan(base$distancia))
my_nan4
my_nan5<-sum(is.nan(base$educacion))
my_nan5
my_nan6<-sum(is.nan(base$arancel))
my_nan6

my_infinite<-sum(is.infinite(base$prueba_conocimiento))
my_infinite2<-sum(is.infinite(base$desempleo))
my_infinite3<-sum(is.infinite(base$salario))
my_infinite4<-sum(is.infinite(base$distancia))
my_infinite5<-sum(is.infinite(base$educacion))
my_infinite6<-sum(is.infinite(base$arancel))
my_infinite6
my_infinite
my_infinite2
my_infinite3
my_infinite4
my_infinite5

summary(base)
#No hay presencia de valores NAN ni de INFINITE

#En la data hay una buena cantidad de variables ficticias. Por lo tanto, realizare 
#su respectiva transformacion a factor.
base$genero<-as.factor(base$genero)
base$raza<-as.factor(base$hispano)
base$educPadre<-as.factor(base$educPadre)
base$educMadre<-as.factor(base$educMadre)
base$casaPropia<-as.factor(base$casaPropia)
base$urbano<-as.factor(base$urbano)
base$ingresos<-as.factor(base$ingresos)
base$hispano<-as.factor(base$hispano)

#Visualicemos si existen outlier
boxplot(base$prueba_conocimiento)
boxplot(base$desempleo)
#La variable desempleo contiene varios outlier

boxplot(base$salario)
boxplot(base$distancia)
#La variable distancia contiene varios outlier

boxplot(base$arancel)
boxplot(base$educacion)

#No es necesario imputar la data, todos los valores han sido declarados. Sin embargo,
#se requerira sustituir ciertos valores outlier.

#Para corregir estos outlier, calculare los percentiles 10 y 90, y sustituire
#los respectivos outlier por los respectivos percentiles (segun sea el caso)
quantile(base$desempleo, probs=c(0.1, 0.9))
quantile(base$distancia, probs=c(0.1, 0.9))

base$desempleo[base$desempleo>11.5]<-11.5
base$desempleo[base$desempleo<4.55]<-4.55
base$distancia[base$distancia>4.0]<-4.0
base$distancia[base$distancia<0.1]<-0.1

#Ya ha quedado limpia la base de datos

#PLANTEAMIENTO DE HIPOTESIS
#H0: Existe relacion entre la distancia del hogar al establecimiento educacional Vs
#Ha: No existe relacion entre la distancia del hogar al establecimiento educacional

#Gráfique y cálcule algún indicador que permita apoyar el objetivo de este estudio.
#¿Qué se puede observar?


boxplot(base$prueba_conocimiento~base$raza, col=c("blue", "yellow"))
salida=aggregate(base$prueba_conocimiento, list(base$raza), quantile, probs=c(0.5, 0.75))
salida
#75% de los estudiantes afroamericanos poseen una puntuacion, de a lo mas 58.485 puntos Vs una puntuacion maxima de 53.93 de los afroamericanos
#Ademas, la mitad de los estudiantes afroamericanos poseen una puntuacion maxima de 48.62 puntos Vs una puntuacion de 52.03 de los estudiantes
#de otras razas. Esto pudiese indicar cierto efecto de la raza en la prueba_conocimiento, por lo que es necesario realizar un análisis más exhaustivo.



#¿Cuál sería la idea de recolectar entre los datos, la variable prueba_conocimiento?
#Tratar de determinar si el rendimiento estudiantil en las escuelas de USA esta correlacionado
#con el conjunto de variables en estudio. Incluso, si fuera posible, estimar un posible rendimiento
#estudiantil, considerando las variables estudiadas (distancia, desempleo, raza,...)


#Genere una regresión simple con la variable objetivo, explique claramente si es significativa e interprete su resultado.¿Cómo es el R-squared?. 
#Nombre a esta regresión commo modelo 1

modelo1<-lm(base$prueba_conocimiento~base$distancia)
summary(modelo1)
#El valor de probabilidad asociado a la variable distancia, indica que podemos rechazar H0
#Es decir, la variable distancia ayuda a explicar a la prueba_conocimiento.
#Esto quiere decir, que en promedio, por cada millas que se aleje el establecimiento educacional del hogar
#el resultado en la prueba de conocimiento de dicho estudiante, descendera en 0.43 puntos.

#El valor del R^2 indica que la distancia solo explica a la prueba de conocimiento en aproximadamente 4,4%.
#Es decir, un 4,4% de los resultados obtenidos en prueba_conocimiento son explicados por la distancia.


#Genere una regresión multiple, con 3 variables, donde al menos una sea factor. 
#Interprete sus coeficientes, significancia y la robustez del modelo planteado. 
#Nombre a esta regresión como modelo 2

modelo2<-lm(base$prueba_conocimiento~base$distancia+base$raza+base$urbano)
summary(modelo2)

#Todas las variables son significativas para el modelo, todos los valores p son menores, incluso al 1%. Es decir, todas son influyentes para explicar la prueba_conocimiento
#por cada milla que se aleje el hogar del establecimiento, en promedio, la prueba_conocimiento del estudiante descendera en 0.78 puntos, mantiendose constante el resto de los predictores.
#Los estudiantes Afroamericanos, en promedio, tienen casi 3 puntos menos en la prueba_conocimiento respecto a otras razas.
#Los colegios que estan en zonas urbanas, en promedio presentan 2.64 puntos menos en la prueba_conocimiento respecto a los colegios que no estan en dichas zonas.

#El modelo es poco preciso para estimar, puesto que el coeficiente de determinacion es de apenas 3,6%. Esto indica que solo el 3,6%
#de la prueba_conocimiento estara explicada por las variables distancia, raza y urbano.


#¿Alguna de las variables del data set, tiene una relación de doble efecto (no lineal)?. 
#Si la respuesta es sí, explique y genere un modelo 3, donde considere como base al modelo 2 e incorporé esta variable.Si la respuesta es no, genere una variable que tenga este efecto. 
#En ambos casos interprete la variable, argumente su incorporación y determine si es significativa.
 

#Pienso que si. Particularmente la variable educacion pudiese tener relacion doble efecto.
#Esto porque mientras mayor educacion se tenga, es mas probable alcanzar la maxima puntuacion en la prueba
#se confirma a continuacion

base$educacion2<-base$educacion*base$educacion
matplot(base$educacion2, main="educacion2 Vs prueba_conocimiento", base$prueba_conocimiento, pch =100, xlab = "educacion2", ylab = "prueba_conocimiento")


modelo3 <- lm(prueba_conocimiento ~ distancia+raza+urbano+educacion2, data = base )
summary(modelo3)

#La variable agregada tambien es significativa. En promedio, por cada año adicional que posea una persona en educacion
#el puntaje en su prueba aumentara, en promedio, proporcionalmente el factor cuadratico, manteniendose constante el resto de los predictores..
#Al visualizar la incorporacion de la variable educacion 2 al modelo, esta mejora significativamente
#el valor del R^2 el cual es de aproximadamente 24.76% lo que indica que las variables del modelo 3
#explican en aproximadamente 25% a la variable prueba_conocimiento



#Genere un modelo 4 a su gusto, sin perder el objetivo del estudio. 
#Interprete las variables que no hayan sido analizadas y determine que tan bueno es este modelo.


install.packages("caret")
library(caret)
sub<- createDataPartition( y = base$prueba_conocimiento , p = 0.7 , list = FALSE )
tren<-base[sub,]
prueba<-base[- sub,]


modelo4<-glm(prueba_conocimiento~., data=tren)
summary(modelo4)

step(modelo4, scope=list(upper=modelo4), data=tren, direction="backward")
#Las variables a seleccionar para construir el modelo son educacion, educacion2, arancel
#salario, desempleo, urbano, educPadre, raza y genero.

modelo4_<-lm(prueba_conocimiento~genero+raza+educPadre+casaPropia+urbano+desempleo+salario+arancel+educacion+educacion2, data=tren)
summary(modelo4_)

#Las mujeres, en promedio, tienen casi un punto menos que los hombres en la prueba_seleccion
#Los estudiantes de padres graduados en universidad, en promedio tienen dos puntos mas en la prueba_seleccion que los estudiantes
#cuyo padre no esta graduado en la universidad

#Los estudiantes cuya familia tiene casa propia, tienen en promedio 1.21 puntos mas en prueba_seleccion
#que los estudiantes cuyas familias no tienen casa propia

#Por cada dolar que incremente el salario en el sector manofacturero, los estudiantes provenientes de dichas familias
#tendran un incemento promedio en su prueba_seleccion de 0.44 puntos, siempre que el resto de predictores permanezca constante

#Por cada anho adicional en educacion, habra un incremento promedio de 6 puntos en la prueba_seleccion
#de los estudiantes, siempre que se mantengan constantes el resto de predictores

#Por cada mil dolares de incremento en el arancel, la puntiacion de prueba_seleccion incrementara
#en promedio 3.62 puntos, de mantenerse constantes el resto de predictores.

#Todas las variables son significativas al 5% e incluso al 1%. Esto quiere decir, que todas las variables son influyentes
#en el modelo, contrubuyen significativamente a explicar a prueba_seleccion.

#El coeficiente de correlacion indica que 28.35% aproximado de la variable prueba_seleccion es explicada
#por las variables predictores, esto es aproximadamente un 50% de correlacion. Este valor es bajo para predecir
#Por lo que concluyo, que el modelo que mejor se ajusta no es un modelo lineal

predict(modelo4_, data=tren)



