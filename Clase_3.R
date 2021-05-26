#### Clase dplyr, lubridate, stringr####

#### Instalando paquetes ####
## CRAN, necesita conexión a internet
## GitHUB devtools
install.packages("dplyr") ## Instalar paquetes
dplyr::filter() ## :: precedido por nombre de paquete me llama una función específica del paquete
library(dplyr);library(tidyr);library(pacman);library(gapminder);library(tidyverse)
## Concatenación de dos lineas de código se puede hacer usando el simbolo ;

pacman::p_load(dplyr, tidyr, gapminder, tidyverse, cowplot)
##p_load de el paquete pacman carga *e instala* rápidamente los paquetes

#### Crear datos, guardarlos y cargarlos con readr y haven ####
library(gapminder) ## Es el paquete que contiene el dataset
data("gapminder") ## Cargo los datos
data<-gapminder #Asignar gapminder a un objeto llamado data
?gapminder
head(data)
View(data)
pacman::p_load(readr, haven)
## readr lee, guarda datos en formato .csv, .txt
## haven stata, sas, spss, xlxs
setwd("~/Downloads") ## Especifican directorio de trabjo

write_csv(data, "datos.csv")
write_sav(data, "datos.sav") # SPSS
write_sas(data, "datos") #SAS

datos <- read_csv("datos.csv") ## Funciones read_csv, read_sas, read_sav
View(datos)

#### Manipulando datos con dplyr y tidyr ####

## Tidyverse
library(tidyverse)
## dplyr,tidyr, ggplot2
?gapminder
# Pipeline
Objeto %>% Funcion ## El pipeline toma objetos y los concatena con funciones
Z %>% U## Ctrl (Cmd) + Shift + m

str(datos) ## Función estructura

datos$country<-as.factor(datos$country) #Recodificando
class(datos$country) ## Recodificación funcionó
datos$continent<-as.factor(datos$continent) #Recodificando
class(datos$continent)

str(datos) ## Función estructura
x=3
datos$continent

#Me interesa quedarme solo con países de África
datos_africa<-datos %>% filter(continent=="Africa")
write_csv(datos_africa, "datos_africa.csv")

#Me interesa quedarme solo con países de Afica despues de 1980
datos_africa2<- datos %>% filter(continent=="Africa" & year>1980)

#Me interesa quedarme solo con países de Africa despues de 1980 o antes de 1960
#Operadores boleanos AND = &, OR = |
datos_africa3<- datos %>% filter((continent=="Africa" & year>1980) | 
                                   (continent=="Africa" & year<1960))

##Puedo concatenar TODAS las funciones que quiera a un mismo objeto
datos_africa4<- datos %>% filter(continent=="Africa") %>% 
  filter(year>1980 | year<1960)

## Promedio de expectativa de vida por continente por año
t1<-tapply(datos$lifeExp, datos$continent, mean)
tapply(datos$lifeExp, datos$continent, sd)
tapply(datos$lifeExp, datos$continent, min)
tapply(datos$lifeExp, datos$continent, max)
class(t1)
# group_by, summarise
?group_by()
?summarise

datos_agrupados<- datos %>% group_by(continent) %>% 
  summarise(meanExp=mean(lifeExp))
class(datos_agrupados) # Objetos en R pueden tener más de una clase

##Dos agrupaciones
datos_agrupados2<- datos %>% group_by(year, continent) %>% 
  summarise(meanExp=mean(lifeExp))

## Dos agrupaciones, cinco estadísticas de resumen
datos_agrupados2<- datos %>% group_by(year, continent) %>% 
  summarise(meanExp=mean(lifeExp), sdExp=sd(lifeExp), minExp=min(lifeExp),
            maxExp=max(lifeExp), medianExp=median(lifeExp))

write_csv(datos_agrupados2, "resumen.csv")

##Dos agrupaciones, cinco estadísticas de resumen, ordenado por el minimo máximo
## arrange solo ordena de menor a mayor
## arrange(desc(x)) ordena x de mayor a menor

datos_agrupados3<- datos %>% group_by(year, continent) %>% 
  summarise(meanExp=mean(lifeExp), sdExp=sd(lifeExp), minExp=min(lifeExp),
            maxExp=max(lifeExp), medianExp=median(lifeExp)) %>% arrange(desc(minExp))
?arrange()
head(datos_agrupados3)

set.seed(123) ## Reproducibilidad de procesos aleatorios
datos_muestra<- datos %>% sample_n(10, replace=F)
set.seed(123);datos_muestra<- datos %>% sample_n(10, replace=F)

## Select
names(datos) ## Nombres de las variables

datos_2<- datos %>% filter(year>1970) %>% select(country, lifeExp, pop)

## Mutate ##Crear nuevas variables
head(datos)

datos_gdp<- datos %>% mutate(gdp=gdpPercap*pop)

datos_agrupados3<- datos %>% group_by(year, continent) %>% 
  summarise(meanExp=mean(lifeExp), sdExp=sd(lifeExp), minExp=min(lifeExp),
            maxExp=max(lifeExp), medianExp=median(lifeExp)) %>% arrange(desc(minExp)) %>% 
  mutate(ciInf=meanExp-1.96*sdExp, ciSup=meanExp+1.96*sdExp)

## Left Join
a1<-c("A", "B", "C")
b1<-c(1,2,3)
df1<-data.frame(a1, b1)
a1<-c("A", "B", "D")
b2<-c(43,56,78)
df2<-data.frame(a1, b2)

df3<- df1 %>% left_join(df2, by="a1")

df4<- df1 %>% right_join(df2, by="a1")

df4<- df1 %>% inner_join(df2, by="a1")

df5<- df1 %>% full_join(df2, by="a1")


#### Manipulando fechas con lubridate ####

## Trabajar con fechas
#REVISEN LA DOCUMENTACIÓN DEL PAQUETE LUBRIDATE



