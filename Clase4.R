
#Cargar paquetes
library(car); library(tidyverse); library(ggpubr)
library(viridis); library(ggsci) #M�todo 1
pacman::p_load(car, tidyverse, ggpubr, viridis, ggsci) #M�todo 2

setwd("C:/Users/facmed/Desktop/Clase")

#Cargar base de datos
data(iris)

iris #base de datos completa
head(iris) #Ver las primeras 6 filas de la base 
tail(iris) #Ver los �ltimas 6 filas de la base

iris$Species #Tipo de flor
table(iris$Species)

iris$Sepal.Length #Longitud del s�palo
summary(iris$Sepal.Length)

iris$Sepal.Width #Anchura del s�palo
summary(iris$Sepal.Width)

iris$Petal.Length #Longitud del p�talo
summary(iris$Petal.Length)

iris$Petal.Width #Anchura del p�talo
summary(iris$Petal.Width)


####---- Gr�ficas predeterminadas en R ----####

hist(iris$Sepal.Length) #Histograma
plot(iris$Petal.Length,iris$Petal.Width) #Gr�fico de dispersi�n
boxplot(iris$Sepal.Length~iris$Species) #Grafico de cajones

hist(iris$Sepal.Leng, #Variable a graficar
     col="midnightblue", #Color
     border="white", #Borde
     main="Distribuci�n de la longitud de s�palo", #T�tulo
     xlab="Longitud del s�palo",ylab="Conteo (frecuencias)") #Etiquetas de los ejes

plot(x=iris$Petal.Length,y=iris$Petal.Width, #Variables
     col="gold2", #Color
     pch=16, #Forma del punto
     cex=1.15, #Tama�o del punto
     main = "Relaci�n largo-ancho del p�talo", #T�tulo
     xlab="Longitud",ylab="Anchura" #Etiquetas
     ); abline(lm(Petal.Width~Petal.Length,iris),col="gold4") #L�nea de regresi�n

boxplot(iris$Sepal.Length~iris$Species,
        col=c("indianred1","red2","red4"), #Colores (vector para especificar varios)
        main = "Longitud de s�palo seg�n la especie", #T�tulo
        xlab="Especie",ylab="Longitud") #Etiquetas


####---- Gr�ficos con ggplot2: bases ----####

#Para funcionar, ggplot requiere 3 cosas:
#1) Base de datos: data
#2) Variables que se van a graficar: mapping=aes()
ggplot(data = iris, mapping = aes(x=Sepal.Length))
ggplot(data = iris, mapping = aes(x=iris$Sepal.Length)) #No es necesario volver a especificar la base


#3) Par�metro gr�fico al que se mapean los datos (funciones "geom"): CAPAS!!!
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_histogram()


##Elegir el tipo de gr�fico de acuerdo con los tipos de variables

##Una variable continua
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_histogram()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_density()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_dotplot()
ggplot(data = iris, mapping = aes(x=Sepal.Length)) + geom_freqpoly()

##Una variable categ�rica
ggplot(data = iris, mapping = aes(x=Species)) + geom_bar()

data1 <- data.frame("Respuesta"=c(LETTERS[1:4]), "N_alumnos"=c(10,2,5,1))
data1

ggplot(data = data1, mapping = aes(x=Respuesta)) + geom_bar()

#stat= argumento que indica qu� transformaci�n estad�stica se le har� a los datos
#stat=bin: separa una variable continua en bloques y cuenta la frecuencia de esos bloques
#stat=count: cuenta el n�mero de veces que se repite una variable 
#stat=identity: deja las variables tal cu�l como est�n

ggplot(data = data1, mapping = aes(x=Respuesta)) + geom_bar() #stat=count

#Soluci�n 1: usar un geom que tenga por default el stat que necesitamos
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_col()
ggplot(data = iris, mapping = aes(y=Sepal.Length, x=Species)) + geom_col()

#Soluci�n 2: usar la misma funci�n de antes pero especificar el stat
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_bar(stat="identity")
ggplot(data = iris, mapping = aes(y=Sepal.Length, x=Species)) + geom_bar(stat="identity")

#Soluci�n 3: usar una funci�n "stat" en vez de una funci�n geom
ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + stat_identity(geom="bar")
ggplot(data = iris, mapping = aes(y=Sepal.Length, x=Species)) + stat_identity(geom="bar")

#Cambiar el stat predeterminado aplica para muchas funciones (geom_bar, geom_area)


##Dos variables continuas
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_point()
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_jitter()
?geom_jitter

ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_jitter()

ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_quantile()
?geom_quantile
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_quantile(quantiles=c(0.2,0.3,0.4,0.5))

ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth()
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth(method = "lm")
ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth(formula = y~poly(x,2))

set.seed(123); data2 <- (data.frame("X"=runif(n= 100, min= 1, max= 100))) %>%
  mutate("Y"=(X**2))
ggplot(data = data2, mapping = aes(x=X, y=Y)) + geom_smooth(formula = y~poly(x,2))

ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + geom_rug()


##Una variable continua y una categ�rica
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_point()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_jitter()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_col()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_boxplot()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_violin()
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + 
  geom_dotplot(binaxis = "y", stackdir ="center")


##COMBINAR capas
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width))

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) 

ggplot(data = data1, mapping = aes(y=N_alumnos, x=Respuesta)) + geom_col() +
  geom_text(label=) #Porcentaje de alumnos que contestaron ese inciso


##FACETING
data(mpg)
?mpg
table(mpg$manufacturer)
table(mpg$year)
table(mpg$cyl)
table(mpg$fl)
table(mpg$class)

ggplot(mpg, aes(displ, hwy)) + geom_point()
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year))
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(cols = vars(year))

ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~year, nrow=2, ncol=1)

ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl))
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl),
                                                         scales="free")

ggplot(mpg, aes(displ, hwy)) + geom_point() +facet_grid(cyl~year)
ggplot(mpg, aes(displ, hwy)) + geom_point() +facet_wrap(cyl~year, nrow=4)



####---- Gr�ficos con ggplot2: agrupar y guardar ----####

Fig1A <- ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_boxplot()
Fig1B <- ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_jitter() +
  geom_smooth(method="lm")

ggpubr::ggarrange(Fig1, Fig2)

F1 <- ggarrange(Fig1, Fig2)

ggsave(filename="F1.jpg", plot = F1,
       width = 35, height = 20, units=c("cm"),
       dpi=200, limitsize=FALSE)


####---- Gr�ficos con ggplot2: personalizaci�n ----####

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width)) + geom_boxplot()
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_point()

#Color, fill, alpha, shape, linetype


#Scale manual (values, name, limits, breaks, labels)




#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#https://www.rapidtables.com/web/color/RGB_Color.html


#Scale (brewer, grey, gradient, viridis, ggsci)

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species)) + 
  geom_boxplot()+scale_fill_brewer()


#ggtitle, xlab, ylab, annotation


#Themes


#Legends (size, position, get_legend)














