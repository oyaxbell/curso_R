## Sesión 9, Curso de programación en R: Modelos lineales generalizados
## Ponente: Omar Yaxmehen Bello-Chavolla, MD, PhD, @OmarBelloMD
## Martes 15 de junio de 2021


pacman::p_load(MixAll, lmtest, car, gvlma, MASS, NHSRdatasets, ResourceSelection, 
               rcompanion, nortest,tidyverse, caret,mgcv, rms, jtools)

## Modelo lineal generalizado - Extensión de los modelos lineal general (Regresión lineal)
## Aplicar una transformación a la variable dependiente, puedes usar los GLMs para cualquier tipo de variable
## Gaussiana (Identidad) - Modelo de regresión lineal
## Binomial - Modelo de regresión logística
## Poisson - Modelo de regresión Poisson
## Binomial negativo - Conteos con varianza variable
## QuasiPoisson, Zero-inflated Poissson regression
## Regresión de Cox - Distribución exponencial
## Transformación a la variable dependiente - Función liga (link function)

## Modelo lineal Y~beta0+beta1*x1+beta2*x2+...+betan*xn


#### Regresión logística binaria  ####

## Función liga: Función logit, probit
## Componente aleatorio: Distribución binomial - Distribución de Bernoulli
## Variable dependiente: Variable cualitativa nominal dicotómica {0, ausencia de atributo & 1, presencia de atributo}
## Objetivo: Modelar la probabilidad de pertenecer a la clase 0 o a la clase 1 en función de ciertos predictores

#Y{0,1}~beta0+beta1*x1+...+betan*xn

##Función logit-> exp(beta1)-> Razón de momios (OR)

# Cargamos los datos y eliminamos valores perdidos 
##MAÑANA VEREMOS COMO Y PORQUÉ NO HACER ESTO
data("PimaIndiansDiabetes2", package = "mlbench")

PimaIndiansDiabetes2 <- PimaIndiansDiabetes2 %>% drop_na() #Eliminar sujetos con datos faltantes
View(PimaIndiansDiabetes2)

colSums(is.na(PimaIndiansDiabetes2))
str(PimaIndiansDiabetes2)

# Inspeccionamos los datos
sample_n(PimaIndiansDiabetes2, 3)

# Separamos los datos en población de entrenamiento y de validación
set.seed(123) # Preservar reproducibilidad de eventos aleatorios
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Ajustamos un modelo con todos los predictores de la base
model <- glm( diabetes ~., data = train.data, family = "binomial")

# Inspeccionamos el modelo
summary(model)

# Hacemos las predicctiones del modelo con los datos de prueba

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Evaluamos el desempeño
mean(predicted.classes == test.data$diabetes)
confusionMatrix(factor(predicted.classes), factor(test.data$diabetes))


##Ajustamos un modelo de regresión logística simple
model <- glm( diabetes ~ glucose, data = train.data, family = "binomial")
summary(model)

coef(model) #Extrae coeficientes beta de los modelos
exp(coef(model)) ## Calculo los OR del modelo
confint(model) ## Esto calcula intervalos de confianza para los BETAS
exp(confint(model)) #Esto calcular intervalos de confianza para los OR
summ(model, exp=T, confint = T) #jtools y permite ver rápidamente el modelo de regresión logística
plot_summs(model, exp=T, confint=T)
forestmodel::forest_model(model)

newdata <- data.frame(glucose = c(80,  180))
probabilities <- model %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

##Visualizamos lo datos
train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )

## Regresión logística múltiple ###

model <- glm( diabetes ~ glucose + mass + pregnant, 
              data = train.data, family = "binomial")
summary(model)
summ(model, exp=T, confint=T)
forestmodel::forest_model(model)


##Interpretación ###

model <- glm( diabetes ~ pregnant + glucose + pressure + mass + pedigree, 
              data = train.data, family = binomial)
forestmodel::forest_model(model, limits = c(0,5))
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

### Precisión promedio ###
mean(predicted.classes == test.data$diabetes)

##Modelo 2
model2 <- glm( diabetes ~  glucose + pregnant + mass + pedigree, 
              data = train.data, family = "binomial")
summary(model2)

### Evaluamos modelo

### Comparando modelos###

anova(model, model2, test = "Chisq")
BIC(model)
BIC(model2)

##Supuesto del modelo de regresión logística
##Bondad de ajuste: Que el modelo de ajuste a los datos
## Prueba de Hosmer-Lemeshow - Chi cuadrada
## H0: El modelo se ajusta a los datos

hoslem.test(train.data$diabetes, model2$fitted.values, g = 10)

#### Regresión Poisson ####
cases <-  
  structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L, 
                          8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L, 
                          23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L, 
                          42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L, 
                          49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L, 
                          67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L, 
                          80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L, 
                          95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L, 
                          106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L), 
                 Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L, 
                              4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L, 
                              3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L, 
                              5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L, 
                              5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L, 
                              0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L, 
                              2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L, 
                              0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)), .Names = c("Days", "Students"
                              ), class = "data.frame", row.names = c(NA, -109L))
head(cases)
plot(cases$Days, cases$Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

m1<-lm(Students~Days, data=cases)
summary(m1)

cases %>%
  ggplot(aes(x=Days, y=Students))+geom_point(alpha=0.5)+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))+ 
  theme_classic()

gvlma(m1)
trafo::assumptions(m1)

#K fold cross validation- Parte tus datos de forma artificial en k partes (k=10)
# 1. Ajustar el modelo en parte 2-10 y validarlo en parte 1
# 2. Ajustar modelo en parte 1, 3-10, validarlo en parte 2
##...
## Promedia el error en cada k segmento y se obtiene una métrica más cercana
model1 <- glm(Students~Days, data=cases, family="poisson")
summary(model1)

## Función liga: Función logarítmia (Poisson)
## Componente aleatorio: Distribución Poisson - Conteos de casos en tiempo o espacio
## Variable dependiente: Variable cuantitativa discreta (No tiene valores intermedios)
## Objetivo: Modelar la probabilidad de ocurrencia de un evento o conteo en un tiempo o espacio

## Lambda es el parámetro de la regresión Poisson, Mean(Pois)=Lambda, Var(Pois)=Lambda
## Prueba de la devianza -> Que tantas veces es más grande la devianza vs. el lambda promedio >1
## Binomial negativa, QuasiPoisson

#Y{Pois(Lambda)}~beta0+beta1*x1+...+betan*xn+offset(log(X))
#X-> Espacio o tiempo e regresión Poisson


##Función Poisson-> exp(beta1/X)-> Razón de Incidencias (RI), razon de tasas


#### Ajuste Poisson ####
data(DebTrivedi)
?DebTrivedi
head(DebTrivedi)

m1 <- glm(ofp ~ ., data = DebTrivedi, family = "poisson")
summary(m1)
?coeftest
coeftest(m1)

## Prueba de sobredispersión
deviance(m1)/summary(m1)$df[2]

##Que tantas veces es > varianza que la media en una distribución

## Modelo quasi-poisson
m1q <- glm(ofp ~ ., data = DebTrivedi, family = quasipoisson)
summary(m1q)
deviance(m1q)/summary(m1q)$df[2]

## Modelo binomial negativo
fm_nbin <- MASS::glm.nb(ofp ~ ., data = DebTrivedi)
summary(fm_nbin)

#### Generalized additive models ####
library("mgcv")
# GAM -> Permitir flexibilidad las funciones de coeficientes utilizando funciones suavizadas 
gam.model <- gam(diabetes ~ s(glucose) + s(mass) + pregnant,
                 data = train.data, family = "binomial")
# Summarize model
summary(gam.model )
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities> 0.5, "pos", "neg")
# Model Accuracy
mean(predicted.classes == test.data$diabetes)

## Splines
data(mtcars)
names(mtcars)
mtcars %>%
  ggplot(aes(x=hp, y=mpg))+geom_point(alpha=0.3)+geom_smooth()+
  geom_smooth(method="lm", col="red")

##Regresión lineal SÍ MODELA relaciones NO LINEALES
##Polinomios ortogonales
## Splines -> Cortan la curva en curvas mas pequeñar para mejorar el ajuste

##Regresiones polinómicas 

m1<-lm(mpg~hp, data=mtcars)
summary(m1)

#Polinomios ortogonales
m2<-lm(mpg~poly(hp,2), data=mtcars)
summary(m2)
BIC(m2)

#Restricted cubic splines -> Paquete rms
m2<-lm(mpg~rcs(hp,3), data=mtcars)
summary(m2)
BIC(m2)

#Local polynomial regression (Loess)
m3<-loess(mpg~hp, data=mtcars)
summary(m3)

