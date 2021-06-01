
#Cargar paquetes
library(car); library(tidyverse); library(ggpubr)
library(viridis); library(ggsci); library(ggthemes) #Método 1
pacman::p_load(car, tidyverse, ggpubr, viridis, ggsci) #Método 2

setwd("C:/Users/facmed/Desktop/Clase")

#Cargar base de datos
data(iris)

iris #base de datos completa
head(iris) #Ver las primeras 6 filas de la base 
tail(iris,10) #Ver los últimas 6 filas de la base

iris$Species #Tipo de flor
table(iris$Species)

iris$Sepal.Length #Longitud del sépalo
summary(iris$Sepal.Length)

iris$Sepal.Width #Anchura del sépalo
summary(iris$Sepal.Width)

iris$Petal.Length #Longitud del pétalo
summary(iris$Petal.Length)

iris$Petal.Width #Anchura del pétalo
summary(iris$Petal.Width)


####---- Gráficas predeterminadas en R ----####

hist(iris$Sepal.Length) #Histograma
plot(iris$Petal.Length,iris$Petal.Width) #Gráfico de dispersión
boxplot(iris$Sepal.Length~iris$Species) #Grafico de cajones

hist(iris$Sepal.Leng, #Variable a graficar
     col="midnightblue", #Color
     border="white", #Borde
     main="Distribución de la longitud de sépalo", #Título
     xlab="Longitud del sépalo",ylab="Conteo (frecuencias)") #Etiquetas de los ejes

plot(x=iris$Petal.Length,y=iris$Petal.Width, #Variables
     col="gold2", #Color
     pch=16, #Forma del punto
     cex=1.15, #Tamaño del punto
     main = "Relación largo-ancho del pétalo", #Título
     xlab="Longitud",ylab="Anchura" #Etiquetas
     ); abline(lm(Petal.Width~Petal.Length,iris),col="gold4") #Línea de regresión

boxplot(iris$Sepal.Length~iris$Species,
        col=c("indianred1","red2","red4"), #Colores (vector para especificar varios)
        main = "Longitud de sépalo según la especie", #Título
        xlab="Especie",ylab="Longitud") #Etiquetas


####---- Gráficos con ggplot2: bases ----####

#Para funcionar, ggplot requiere 3 cosas:
#1) Base de datos: data
#2) Variables que se van a graficar: mapping=aes()

ggplot(data = iris, mapping = aes(x=Sepal.Length))
ggplot(data = iris, mapping = aes(x=iris$Sepal.Length)) #No es necesario volver a especificar la base

#3) Parámetro gráfico al que se mapean los datos (funciones "geom"): CAPAS!!!
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_histogram()

#Se pueden omitir los nombres de los argumentos "data" y "mapping", siempre y cuando se pongan en ese orden
ggplot(iris, aes(x=Sepal.Length)) + geom_histogram()


##Debemos elegir el tipo de gráfico de acuerdo con los tipos de variables

##UNA VARIABLE CONTINUA
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_histogram()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_dotplot()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_freqpoly()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_area(stat="count")


##UNA VARIABLE CATEGÓRICA
ggplot(data = iris, mapping = aes(x=Species)) + geom_bar()

#El gráfico de barras nos muestra el número de flores setosa, versicolor y virginica
#Hay 50 de cada una, para que haya un número distinto para cada una haremos una nueva base

#Filtramos la base para que solo aparezcan las flores con longitud de sépalo >=5.5
iris2 <- iris %>% filter(Sepal.Length>=5.5) 

#Ahora hay 5 setosas, 44 versicolor y 49 virginica
table(iris2$Species)
ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar() 

#PERO
#Esto solo funciona si en nuestra base de datos tenemos una variable en la que la categoría que
#queremos contar se repita n número de veces

#Por otro lado, podemos tener una base de datos en la que tengamos en una variable las etiquetas
#Y en otra variable el número de veces que se repiten

data1 <- data.frame("Respuesta"=c(LETTERS[1:4]), "N_alumnos"=c(10,2,5,1))
data1 #Base de datos con los incisos de una pregunta y el número de alumnos que contestó ese inciso

#Si usamos "geom_barplot" en este caso, NO nos arroja el número de alumnos que contestó cada inciso,
#sino el número de veces que se repite cada inciso en la base
ggplot(data = data1, mapping = aes(x=Respuesta)) + geom_bar()

#Para poder graficar los datos en este caso, necesitamos saber cómo funciona el argumento "stat"
#"stat=" argumento que indica qué transformación estadística se le hará a los datos
#stat=bin: separa una variable continua en bloques y cuenta la frecuencia de esos bloques
#stat=count: cuenta el número de veces que se repite una variable 
#stat=identity: deja las variables tal cuál como están

ggplot(data = data1, mapping = aes(x=Respuesta)) + geom_bar() #stat=count

#Solución 1: usar un geom que tenga por default el stat que necesitamos
#geom_bar utiliza stat=count
#geom_col utiliza stat=identity
#Para usar geom_col necesitamos especificar DOS VARIABLES (etiqueta y conteo)
ggplot(data = data1, mapping = aes(x=Respuesta, y=N_alumnos)) + geom_col()

#Solución 2: usar la misma función de antes pero especificar el stat
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_bar(stat="identity")

#Solución 3: usar una función "stat" en vez de una función geom
#La función stat_identity tiene como objeto geométrico predeterminado "point"
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + stat_identity()
#Debemos cambiarlo por geom="bar"
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + stat_identity(geom="bar")

#Cambiar el stat predeterminado aplica para muchas funciones además de geom_bar
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_area(stat="count")


##DOS VARIABLES CONTINUAS

#Jitter agrega variación aleatoria a la posición de los puntos
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_jitter()

#quantile grafica la distribución de x en función de los quantiles de y, de forma predeterminada
#grafica los cuartiles, pero se puede especificar qué cuantiles se quieren
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_quantile()
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_quantile(quantiles=c(0.2,0.3,0.4,0.5))

#smooth muestra las líneas de regresión de los datos para ayudar a la visualización de patrones
#El método de regresión default es method="loess", que ajusta datos polinómicos
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth()
#Si queremos visualizar una línea de regresión lineal cambiamos a method="lm"
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_smooth(method = "lm")
#También podemos especificar la fórmula que queremos
# y= x^2 (y predicha en terminos de x al cuadrado)
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_smooth(formula = y~poly(x,2))

#Para visualizar mejor esta relación cuadrática, crearemos una base de datos con dos variables:
#X estará compuesta de 1000 números aleatorios del 1 al 100 y Y serán esos números elevados al cuadrado
set.seed(123); data2 <- (data.frame("X"=runif(n= 1000, min= 1, max= 100))) %>%
  mutate("Y"=(X**2))
ggplot(data = data2, mapping = aes(x=X, y=Y)) + geom_smooth(formula = y~poly(x,2))

#rug grafica las distribuciones de ambas variables
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_rug()


##UNA VARIABLE CONTINUA Y UNA CATEGÓRICA
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_point()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_jitter()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_col()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_boxplot()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_violin()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + 
  geom_dotplot(binaxis = "y", stackdir ="center")


##COMBINAR CAPAS (agregar varias funciones gráficas)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width))+ geom_rug() +
  geom_point() + geom_smooth(method="lm")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, )) + geom_boxplot() +
  geom_point()


##AGREGAR TEXTO
#Ya sea con las funciones geom_text o annotate

#Para usar la función geom_text debemos especificar una etiqueta para cada una de nuestras observaciones
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_col() +
  geom_text(label=c("55.5%","11.1%","27.7%","5.5%")) 

#El argumento nudge permite determinar qué tanto se aleja el texto de nuestro objeto
#nudge_y permite alejar al objeto en el eje de las y. El argumento color permite cambiar el color
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_col() +
  geom_text(label=c("55.5%","11.1%","27.7%","5.5%"),color="white", nudge_y = -0.5) #Porcentaje de alumnos que contestaron cada pregunta


#Para la función annotate se tienen que especificar:
#El tipo de anotación (geom="text)
#Las coordenadas de la anotación (x=, y=)
#El texto que queremos anotar (label=)
#Como queremos hacer múltiples anotaciones, se especifica esto con vectores

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, )) + geom_boxplot() +
  annotate(geom="text", x=c(1,2,3), y=c(3,3,3),label=c(0.2,1.3,2)) #Mediana de longitud de pétalo de acuerdo con la especie


#Las anotaciones se pueden especificar a mano (como arriba) o nombrar previamente un objeto con las anotaciones que queremos obtener

anot1 <- as.vector(tapply(iris$Petal.Width, iris$Species, median)) #Obtenemos la mediana de anchura de sépalo para cada especie de flor
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, )) + geom_boxplot() +
  geom_point() + annotate(geom="text", x=c(1,2,3), y=c(3,3,3),label=anot1) #En vez de poner el texto a mano, ponemos el objeto que guardamos



##FACETING
#Usaremos la base de datos mpg
data(mpg)
?mpg
table(mpg$manufacturer)
table(mpg$year)
table(mpg$cyl)
summary(mpg$hwy)

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() #Graficamos las millas por hora en función del desplazamiento del motor

#Podemos desglosar esta relación según el año de fabricación con facet_grid
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year)) #En filas
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(cols = vars(year)) #En columnas

#También podemos utilizar la función facet_wrap
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~year)

#Estos subgráficos pueden tener todos la misma escala (default)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl))
#O podemos permitir que tengan escalas distintas (scales="free")
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl), scales="free")

#También podemos desglosar la gráfica en función de 2 variables al mismo tiempo
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(year~cyl)
#Con facet_wrap se eliminan los subgráficos vacíos y se tiene más control sobre la distribución del gráfico
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=1, ncol=7)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=2, ncol=4)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=3, ncol=3)


####---- Gráficos con ggplot2: color, fill, alpha, shape, linetype, size... ----####

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_boxplot()
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_point()

#Estos parámetros se pueden especificar en el parámetro geométrico o en aes()
#Si se especifica en aes() se le tiene que asignar una variable de la base de datos

#"Color" afecta contornos, líneas y puntos
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(color="blue")
ggplot(data = iris, mapping = aes(x=Sepal.Length, color=Species)) + geom_density()

ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar(color="blue")
ggplot(data = iris2, mapping = aes(x=Species, color=Species)) + geom_bar()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_point(color="blue")
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", color="blue")
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_smooth(method="lm")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(color="blue")
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, color=Species)) + geom_boxplot()


#"Fill" afecta rellenos
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="midnightblue")
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density()

ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar(fill="indianred3")
ggplot(data = iris2, mapping = aes(x=Species, fill=Species)) + geom_bar()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_point(fill="blue") #No sucede nada porque fill no afecta puntos
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, fill=Species)) + geom_point()

#En geom_smooth fill afecta a la sombra y color a la línea
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", fill="blue", color="black")
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, fill=Species)) + geom_smooth(method="lm", color="black")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(fill="gold3")
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species)) + geom_boxplot()


#"Shape" modifica la forma de un punto
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_jitter(shape=10)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, shape=Species)) + geom_jitter()


#"Size" modifica el tamaño de los puntos y el grosor de las líneas
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(size=3)
ggplot(data = iris, mapping = aes(x=Sepal.Length, size=Species)) + geom_density()

ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar(color="black", size=3)
ggplot(data = iris2, mapping = aes(x=Species, size=Species)) + geom_bar(color="black")

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_jitter(size=3)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, size=Species)) + geom_jitter()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", color="black", size=2)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, size=Species)) + geom_smooth(method="lm", color="black")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(size=2)
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, size=Species)) + geom_boxplot()


#"Alpha" modifica la transparencia de los objetos
#alpha puede tomar cualquier valor entre 0 y 1
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="blue", alpha=0)
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="blue", alpha=0.25)
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="blue", alpha=0.50)
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="blue", alpha=0.75)
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(fill="blue", alpha=1)
ggplot(data = iris, mapping = aes(x=Sepal.Length, alpha=Species)) + geom_density(fill="blue")

ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar(fill="blue", alpha=0.5)
ggplot(data = iris2, mapping = aes(x=Species, alpha=Species)) + geom_bar(fill="blue")

ggplot(data = iris2, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_jitter(size=2, alpha=0.5)
ggplot(data = iris2, mapping = aes(x=Petal.Length, y=Petal.Width, alpha=Species)) + geom_jitter(size=2)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", color="black", alpha=1) #Modifica la sombra
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, alpha=Species)) + geom_smooth(method="lm", color="black")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(fill="deeppink2", alpha=0.5)
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, alpha=Species)) + geom_boxplot(fill="deeppink2")


#"Linetype" modifica el tipo de línea
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density(size=2, linetype=2)
ggplot(data = iris, mapping = aes(x=Sepal.Length, linetype=Species)) + geom_density(size=2)

ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar(color="black", size=2, linetype=2)
ggplot(data = iris2, mapping = aes(x=Species, linetype=Species)) + geom_bar(color="black", size=2)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", color="black", linetype=2)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, linetype=Species)) + geom_smooth(method="lm", color="black")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(fill="steelblue2", size=0.75, linetype=2)
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, linetype=Species)) + geom_boxplot(fill="steelblue2", size=0.75)


#Si se especifica tanto en aes como en el parámetro gráfico, se "sobreescribe" el atributo
ggplot(data = iris2, mapping = aes(x=Species, size=Species)) + geom_bar(color="black")
ggplot(data = iris2, mapping = aes(x=Species, size=Species)) + geom_bar(color="black", size=3)


#Si se especifica en aes, tiene que aisgnarse una variable, que puede ser categórica o continua
#Si es categórica, se asigna un color para cada categoría
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size=2)
#Si es continua, se genera un gradiente de colores entre el valor mínimo y el máximo de la variable
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Length)) + geom_point(size=2)


#Estos atributos pueden combinarse para crear gráficos más estéticos
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(color="midnightblue", size=1.25, linetype=5)

ggplot(data = iris2, mapping = aes(x=Species, linetype=Species, fill=Species)) + geom_bar(color="black", alpha=0.5, size=2)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Length, size=Species)) + geom_point() +
  geom_rug(color="black", size=1) + geom_smooth(method="lm", color="black", size=1)

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species)) +
  geom_point(size=2)+geom_boxplot(color="black", size=0.75)



####---- Gráficos con ggplot2: personalización de escalas ----####

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species)) +
  geom_boxplot(color="black", size=0.75)

#Con las funciones scale manual se pueden personalizar manualmente las variables categóricas
#Se tiene que especificar en la función qué es lo que se quiere cambiar
#scale_fill_manual // scale_color_manual // scale_linetype_manual // etc.


#En las funciones scale manual se pueden especificar los siquientes argumentos: values, labels, name, limits

#Con el argumento values se puede cambiar el relleno de cada una de las categorías
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75) +
  scale_fill_manual(values = c("indianred1","red2","red4"))

#Con el argumento labels se puede cambiar la etiqueta de las categorías en las leyendas
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels = c("F1","F2","F3"))

#Con el argumento name se puede cambiar el nombre de la leyenda
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"),
                    name="Tipo \n de \n flor") #"\n" significa salto de línea

#Con el argumento limits se puede cambiar el orden de la leyenda
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), name="Tipo \n de Flor",
                    limits=c("virginica", "setosa", "versicolor"))


#Si se quiere modificar otro parámetro se tienen que agregar exactamente los mismos argumentos
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species))+
  geom_point(size=2) + geom_boxplot(color="black", size=0.75) + 
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor") +
  scale_color_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor")

#De otra forma, aparecerán dos leyendas diferentes
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species))+
  geom_point(size=2) + geom_boxplot(color="black", size=0.75) + 
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor")


#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#https://www.rapidtables.com/web/color/RGB_Color.html


#Se pueden cambiar las variables con paletas predeterminadas 
##PALETAS PARA VARIABLES CATEGÓRICAS

##PAQUETE GGPLOT2
#scale brewer
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + ggplot2::scale_fill_brewer()

RColorBrewer::display.brewer.all()

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_brewer(palette = "Spectral")

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_brewer(palette = "Accent")

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + ggplot2::scale_fill_brewer(palette = "Set3")

#Aquí también se pueden modificar las etiquetas y nombre de la leyenda
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) + geom_point()+geom_boxplot(color="black") +
  ggplot2::scale_fill_brewer(palette = "Set3", name="Tipo", labels=LETTERS[1:7])


#scale discrete
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_discrete()

#scale grey
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_grey()

#scale ordinal
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_ordinal()

#scale viridis
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_viridis_d()



##PAQUETE GGTHEMES
#scale calc
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_calc()

#scale colorblind
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_colorblind()

#scale economist
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_economist()

#scale excel
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_excel()

#scale gdocs
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_gdocs()

#scale hc
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_hc()

#scale pander
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_pander()

#scale ptol
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_ptol()

#scale stata
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_stata()



##PAQUETE GGSCI

#scale aaas
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_aaas()

#scale jama
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_jama()

#scale lancet
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_lancet()

#scale nejm
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_nejm(name="GGG",labels=1:7)

#scale futurama
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_futurama()

#scale rickandmorty
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_rickandmorty()

#scale simpson
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_simpsons()

#scale startrek
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_startrek()



##PALETAS PARA VARIABLES CONTINUAS

#Scale binned
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggplot2::scale_color_binned()

#Scale gradient2
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggplot2::scale_color_gradient2()

#Scale gradient2 tableau
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggthemes::scale_color_gradient2_tableau()

#Scale viridis
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis() #Default (D)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A") #Magma

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "B") #Inferno

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "C") #Plasma

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "E") #Cividis


##ESCALAR LOS EJES DE LAS GRÁFICAS
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A")

#Logaritmo
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_log10()

#Raíz cuadrada
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_y_sqrt()

#Inverso
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_reverse()

#Separar en segmentos
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_y_binned()

#Convertir en tiempo
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_time() + scale_y_time()


####---- Gráficos con ggplot2: títulos, etiquetas y temas ----####

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=TRUE, name="Tipo \nde flor")

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "C", name="Anchura\nde sépalo") +
  geom_smooth(color="black", method="lm")

ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_point(size=1.9, shape=21)+ geom_boxplot(color="black") +
  scale_fill_viridis(option = "C", discrete=T, name="Tipo \nde flor")+
  scale_color_viridis(option = "C", discrete=T, name="Tipo \nde flor")


#AGREGAR TÍTULOS (ggtitle)
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T, name="Tipo \nde flor") +
  ggtitle("Distribución de la longitud de sépalo")

#MODIFICAR EJES (xlab, ylab)
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T, name="Tipo \nde flor") +
  ggtitle("Distribución de la longitud de sépalo")+
  xlab("Longitud de sépalo") + ylab("Densidad")


#TEMAS

#Paquete ggplot
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggplot2::theme_get()

#Paquete ggppubr
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggpubr::theme_cleveland()

#Paquete ggthemes
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_stata() + ggthemes::theme_stata()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_economist() + ggthemes::theme_economist()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_wsj() + ggthemes::theme_wsj()


#Minimalistas
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggplot2::theme_minimal()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggthemes::theme_calc()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggpubr::theme_pubr()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T) + ggpubr::theme_pubclean()



#Todo junto
F1A<-ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis(option = "C", discrete=T, name="Tipo \nde flor")+
  ggtitle("Distribución de la \nlongitud de sépalo")+ xlab("Longitud de sépalo") + ylab("Densidad") +
  ggpubr::theme_pubclean()

F1B<-ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Width, fill=Species)) +
  geom_point(size=1.9, shape=21)+ geom_boxplot(color="black", size=0.75) +
  scale_fill_viridis(option = "C", discrete=T, name="Tipo \nde flor") +
  scale_color_viridis(option = "C", discrete=T, name="Tipo \nde flor") +
  ggtitle("Anchura de sépalo para \n cada tipo de flor")+ xlab("") + ylab("Anchura de sépalo") +
  ggpubr::theme_pubclean()

F1C<-ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2, alpha=0.75) + scale_colour_viridis_b(option="C", name="Anchura\nde sépalo") +
  geom_smooth(color="black", method="lm") + geom_rug(color="black") + ggpubr::theme_pubclean() +
  ggtitle("Relación longitud-anchura \nde pétalo")+ xlab("Longitud de pétalo") + ylab("Anchura de pétalo")


#Función "theme" para modificar el formato de la gráfica

#Borrar elementos: element_blank
F1A + theme(plot.title = element_blank() ) #Título
F1A + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #Texto del eje
F1A + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #Título del eje
F1A + theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) #Líneas del eje
F1A + theme(legend.text = element_blank()) #Texto de la leyenda
F1A + theme(legend.title = element_blank()) #Título de la leyenda

#Borrar todo
F1A + theme(plot.title = element_blank(), axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = "none")

#Cambiar posición de la leyenda
F1A + theme(legend.position = "top")
F1A + theme(legend.position = "bottom")
F1A + theme(legend.position = "right")
F1A + theme(legend.position = "left")
F1A + theme(legend.position = "none")


#Element_text: fuente
F1A + theme(plot.title = element_text(family = "sans")) #Arial
F1A + theme(plot.title = element_text(family = "serif")) #Times New Roman
F1A + theme(plot.title = element_text(family = "mono")) #Monospace

#Element_text: formato
F1A + theme(plot.title = element_text(face = "plain"))
F1A + theme(plot.title = element_text(face = "bold"))
F1A + theme(plot.title = element_text(face = "italic"))
F1A + theme(plot.title = element_text(face = "bold.italic"))

#Element_text: color
F1A + theme(plot.title = element_text(colour = "red4"))
F1A + theme(plot.title = element_text(colour = "midnightblue"))
F1A + theme(plot.title = element_text(colour = "gold4"))

#Element_text: tamaño
F1A + theme(plot.title = element_text(size = 1))
F1A + theme(plot.title = element_text(size = 10))
F1A + theme(plot.title = element_text(size = 20))

#Element_text: justificación horizontal
F1A + theme(plot.title = element_text(size=11, hjust = 0))
F1A + theme(plot.title = element_text(size=11, hjust = 1))
F1A + theme(plot.title = element_text(size=11, hjust = 0.5))

#Element_text: justificación vertical
F1A + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 0)) #Hasta abajo
F1A + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 1)) #Hasta arriba

#Element_text: ángulo
F1A + theme(axis.text = element_text(angle = 0))
F1A + theme(axis.text = element_text(angle = 90))
F1A + theme(axis.text = element_text(angle = 180))
F1A + theme(axis.text = element_text(angle = 270))


####---- Gráficos con ggplot2: anotaciones, comparaciones ----####

#Paste0 pega los objetos que se le indiquen como caracteres sin dejar espacios
paste0("Cruz","Azul","campeón", 2021)
paste0("Cruz ","Azul ","campeón ", 2021)
paste0("La letra número ", 1:4, " del abecedario es ", LETTERS[1:4])

#Round redondea un número con los decimales que se le indiquen
round(3.14159265359, 4)
round((1:10)/7, 2)

#Combinar round y paste0
paste0(1:10, " entre 7 es igual a ", round((1:10)/7, 2))


#HACER ANOTACIONES PARA CADA GRÁFICO
F1A <- F1A + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1A
F1B <- F1B + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1B
F1C <- F1C + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1C

#Mediana de longitud de sépalo para cada especie
ann1<-tapply(iris$Sepal.Length, iris$Species, median)
ann1<-paste0("M= ", ann1)

F1A + annotate("text", x= c(5,5.8,6.5), y=c(1.3,0.85,0.8), label=ann1, size=3.5)


#Coeficiente de correlación
ann2 <- cor(x = iris$Petal.Length, y = iris$Petal.Width, method = "spearman")
ann2 <- round(ann2, 3)
ann2 <- paste0("Rho de \nSpearman= ", ann2)

F1C + annotate("text", x=2.25, y=2, label=ann2, size=3.5)


#stat compare means permite hacer pruebas de hipótesis y plasmar el resultado en la gráfica

F1B + ggpubr::stat_compare_means()
F1B + ggpubr::stat_compare_means(label.x = 2, label.y = 4.25, size=3.5)
F1B + ggpubr::stat_compare_means(label.x = 2, label.y = 4.25, size=3.5, method="anova")

F1B + ggpubr::stat_compare_means(size=3.5, 
                                 comparisons = list(c("setosa", "virginica"),
                                                    c("setosa", "versicolor"),
                                                    c("versicolor", "virginica")))

F1B + ggpubr::stat_compare_means(size=3.5, 
                                 comparisons = list(c("setosa", "virginica"),
                                                    c("setosa", "versicolor"),
                                                    c("versicolor", "virginica")),
                                 symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 0.1),
                                                    symbols = c("***", "**", "*", ".")))


F1A <- F1A + annotate("text", x= c(5,5.8,6.5), y=c(1.3,0.85,0.8), label=ann1, size=3.5)

F1B <- F1B + ggpubr::stat_compare_means(size=3.5, 
                                        comparisons = list(c("setosa", "virginica"),
                                                           c("setosa", "versicolor"),
                                                           c("versicolor", "virginica")),
                                        symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 0.1),
                                                           symbols = c("***", "**", "*", ".")))

F1C <- F1C + annotate("text", x=2.25, y=2, label=ann2, size=3.5)


####---- Gráficos con ggplot2: agrupar y guardar ----####

F1 <- ggarrange(F1A, F1B, F1C, ncol=3, nrow=1, labels = LETTERS[1:3])

ggsave(filename="Graph1.png", plot = F1,
       units=c("cm"), width = 45, height = 20,
       dpi=200, limitsize=FALSE)


####---- Juntar con dplyr ----####

ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year))
mpg %>% ggplot(aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year))


iris2 <- iris %>% filter(Sepal.Length>=5.5) 
ggplot(data = iris2, mapping = aes(x=Species)) + geom_bar()
iris %>% filter(Sepal.Length>=5.5) %>% ggplot(aes(x=Species)) + geom_bar() 


set.seed(123); data2 <- (data.frame("X"=runif(n= 1000, min= 1, max= 100))) %>%
  mutate("Y"=(X**2))
ggplot(data = data2, mapping = aes(x=X, y=Y)) + geom_smooth(formula = y~poly(x,2))

set.seed(123); (data.frame("X"=runif(n= 1000, min= 1, max= 100))) %>%
  mutate("Y"=(X**2)) %>% ggplot(aes(x=X, y=Y)) + geom_smooth(formula = y~poly(x,2))


mpg %>%
  filter(cyl!=5) %>% 
  mutate("year2"=factor(year)) %>% 
  ggplot(aes(x=cyl, y=hwy, fill=year2)) + geom_col(position = "dodge") #stack, fill, dodge




