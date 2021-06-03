####Datos categoricos y Pruebas Diagnosticas desde R###
##Ponentes: Carlos Fermin Martinez y Neftali Eduardo Antonio-Villa
##02-Junio-2021

#####Paquetes a cargar#####
pacman::p_load(pROC, OptimalCutpoints, caret, epiR,ggplot,dplyr,tidyverse, ggthemes,
               ggpubr, ggsci, blandr, irr)

### Prueba de McNemar####

#A) Descripción de la prueba
#La prueba de McNemar se utiliza para determinar si existen diferencias en una variable dependiente dicotómica entre dos grupos relacionados. 
#Puede considerarse similar a la prueba t de muestras pareadas, pero para una variable dependiente dicotómica en lugar de continua.

#B) Supuestos de la prueba
#Supuesto 1: Se tiene una variable dependiente categórica con dos categorías (es decir, una variable dicotómica) y una variable independiente categórica con dos grupos relacionados.
#Supuesto 2: Los dos grupos de su variable dependiente deben ser mutuamente excluyentes. 
#Supuesto 3: Los casos son una muestra aleatoria de la población de interés.


#1. Dataset a trabajar
## Aplicación de una prueba a estudiantes previo y posterior a una intervención propedeutica
## ¿Existen diferencias atribuibles a la intervención sobre los resultados de la prueba?
set.seed(150)
data <- data.frame(before = sample(c("Aprobado",
                                     "Reprobado",
                                     "Reprobado",
                                     "Aprobado",
                                     "Reprobado"),
                                   300, replace = TRUE),
                   after = sample(c("Aprobado",
                                    "Aprobado",
                                    "Aprobado",
                                    "Aprobado",
                                    "Reprobado"),
                                  300, replace = TRUE))

data$before<-as.factor(data$before)
data$after<-as.factor(data$after)

#2. Visualización de la información

nrow(data) #Numero de observaciones
summary(data) #Estadisticos de resumen

#Frecuencia de resultados
table(data$before) 
table(data$after)

#Proporcion de resultados
prop.table(table(data$before))*100
prop.table(table(data$after))*100

table(data$before,data$after) #Realizar tablas de contingencia
prop.table(table(data$after,data$before),1)*100 #Evaluar proporción de acuerdo a columnas

#3. Definir prueba a usar. 
tab1<-table(data$before,data$after)
mcnemar.test(tab1)

### Coeficiente Kappa####

#A) Descripción de la prueba
#El kappa de Cohen se utiliza para medir el acuerdo de dos evaluadores (es decir, "jueces", "observadores") o métodos de calificación en escalas categóricas.
#Este proceso de medir el grado en que dos evaluadores asignan las mismas categorías o puntuación al mismo sujeto se llama confiabilidad entre evaluadores.
#Se podría usar la kappa de Cohen para determinar el acuerdo entre dos médicos en el diagnóstico de pacientes en casos pronósticos "buenos", "intermedios" y "malos".

#B) Supuestos de la prueba

#Supuesto #1: La respuesta (por ejemplo, juicio) que hacen sus dos calificadores se mide en una escala nominal (es decir, variable ordinaria o nominal) y las categorías deben ser mutuamente excluyentes.
#Supuesto #2: Los datos de respuesta son observaciones emparejadas del mismo fenómeno, lo que significa que ambos evaluadores evalúan las mismas observaciones. 
#Suposición #3: Cada variable de respuesta debe tener el mismo número de categorías y la tabulación cruzada debe ser simétrica
#Supuesto #4: Los dos evaluadores son independientes (es decir, el juicio de un calificador no afecta el juicio del otro calificador). 

#1. Dataset a trabajar

## Dos psiquiatras evaluaron a un grupo de pacientes el grado de intensidad

anxiety <- as.table(
  rbind(
    c(11, 3, 1, 0), c(1, 9, 0, 1),
    c(0, 1, 10, 0 ), c(1, 2, 0, 10)
  )
)

dimnames(anxiety) <- list(
  Doctor1 = c("Normal", "Moderate", "High", "Very high"),
  Doctor2 = c("Normal", "Moderate", "High", "Very high")
)

#2. Visualización de la información

anxiety
prop.table(anxiety,2)*100

#3. Definir prueba a usar. Recordar supuestos.

res.k <- Kappa(anxiety)
?Kappa
#Si considera que cada diferencia de categoría es igual de importante, debe elegir pesos lineales (es decir, pesos de espaciado iguales).
res.k
confint(res.k)

#La estadística Kappa varía de 0 a 1, donde.
#0 = acuerdo equivalente al azar.
#0,1 - 0,20 = leve acuerdo.
#0,21 - 0,40 = acuerdo justo.
#0,41 - 0,60 = acuerdo moderado.
#0,61 - 0,80 = acuerdo sustancial.
#0,81 - 0,99 = concordancia casi perfecta
#1 = acuerdo perfecto.



### Matrices de confusión y pruebas diagnósticas####

#¿Cómo se comparan dos métodos diagnósticos entre sí?
#Matrices que muestran las frecuencias observadas cuando se compara una prueba alternativa
#con una prueba de referencia o estándar (gold-standard)

# Sensibilidad: Proporción de sujetos CON la condición (enfermos) que están correctamente identificados por la prueba
# Especificidad: Proporción de sujetos SIN la condición (sanos) que están correctamente identificados por la prueba

# Valor predictivo positivo: Proporción de sujetos con una prueba positiva que sí tienen la condición.
# Valor predictivo negativo: Proporción de sujetos con una prueba negativo que no tienen la condición.


#1. Dataset a trabajar
# EJEMPLO: se probó un nuevo biomarcador para detectar a pacientes con alto riesgo de
# infarto del miocardio. El biomarcador es una variable continua del 0 al 1 y se piensa
# que un valor >= 0.6 implica mayor riesgo de IAM en los siguientes 12 meses

##Crear Dataframe
set.seed(1234)
iam <- data.frame("riesgo" = rbinom(n = 200, size = 1, prob = 0.35), 
                  "bm" = runif(n = 200, min = 0, max = 1))

#Categorizar a la variable
iam <- iam %>% mutate("bm_cat"=ifelse(bm>=0.6, 1, 0))


#2. Generar una tabla de 2x2
table("Prueba"=iam$bm_cat, "Referencia"=iam$riesgo)[2:1,2:1] #25 VP, 52 FP, 34 FN, 89 VN


#3. Obtener los parámetros
#Sensibilidad = VP/(VP+FN) = VP/Enfermos
25/(25+34)
sen <- 25/(25+34)

#Especificidad = VN/(VN+FP) = VN/Sanos
89/(89+52)
spe <- 89/(89+52)

#Valor predictivo positivo = VP/(VP+FP) = VP/Resultados positivos
25/(25+52)

#Valor predictivo negativo = VN/(VN+FN) = VN/Resultados negativos
89/(89+34)

#Tasa de falsos positivos =  FP/(VN+FP) = FP/Sanos = 1-especificidad
52/(52+89)
1-spe

#Tasa de falsos negativos =  FN/(VP+FN) = FN/Enfermos = 1-sensibilidad
34/(34+25)
1-sen


#LR = Comparación de probabilidades.
#Un LR = 1 es nulo

#LR+ = Compara la probabilidad de un resultado POSITIVO en pacientes enfermos vs sanos
#LR+ = sensibilidad/(1-especificidad)
sen/(1-spe) 
#De forma convencional, un LR+ mayor a 10 se considera significativo


#LR- = Compara la probabilidad de un resultado NEGATIVO en pacientes enfermos vs sanos
#LR- = (1-sensibilidad)/especificidad
(1-sen)/spe
#De forma convencional, un LR- menor a 0.3 o 0.1 se considera significativo


#Automáticamente
tab <- table("Prueba"=iam$bm_cat, "Referencia"=iam$riesgo)[2:1,2:1]
rval <- epiR::epi.tests(tab, conf.level = 0.95); rval

rval$elements$pfp #Tasa de falsos positivos
rval$elements$pfn #Tasa de falsos negativos


#4. Visualizar estos resultados con un Nomograma de Fagan
source("https://raw.githubusercontent.com/achekroud/nomogrammer/master/nomogrammer.r")

#Con los datos ficticios sobre el biomarcador
nomogrammer(Prevalence = 0.35, Sens = sen, Spec = spe, Detail = T)

#Otro ejemplo
nomogrammer(Prevalence = 0.034, Sens = 0.435, Spec = 0.985, Detail=T, NullLine = F)


#Personalizar el nomograma: ggplot
nomograma <- nomogrammer(Prevalence = 0.034, Sens = 0.435, Spec = 0.985, Detail=T) +
  ggsci::scale_color_jama()+theme_minimal()+
  ggtitle("Posterior probability of MI\nafter ST elevation") +
  theme(plot.title = element_text(size=13, hjust=0.5, face="bold"),
        legend.position = "bottom")

setwd("C:/Users/facmed/Desktop/Clase/7")
#El wd es diferente en cada computadora, ustedes deben poner una dirección que sí exista en la suya

ggsave(plot = nomograma, filename = "Nomograma.png", units="cm",
       width = 14, height = 20, dpi = 200, limitsize = F)



### Curvas ROC ####

#¿Cómo se comparan dos métodos diagnósticos entre sí?
#Receiver Operating Characteristic
#Compara la tasa de verdaderos positivos (sensibilidad) con la tasa de falsos positivos (1-especificidad)

#1: Dataset a trabajar
#aSAH (aneurysmal SubArachnoid Hemorrhage / hemorragia subaracnoidea por aneurisma)
#Base de datos con 113 observaciones, se busca obtener herramientas para predecir daños cerebrales
#irresversibles a largo plazo durante el periodo agudo de una aSAH con 1 escala clínica (WFNS)
#y varios biomarcadores sanguíneos (s100b, ndka)


#2: Visualizar nuestros datos
data(aSAH); asah <- aSAH
head(asah)

table(asah$outcome)
table(asah$outcome) %>% prop.table() %>% round(4)*100

with(asah, tapply(age, outcome, summary))
with(asah, tapply(gender, outcome, summary)) %>% lapply(prop.table)

with(asah, tapply(wfns, outcome, summary))
with(asah, tapply(s100b, outcome, summary))
with(asah, tapply(ndka, outcome, summary))

asah$wfns <- as.numeric(asah$wfns)


## 3: Obtener las áreas bajo la curva (AUC) ##

##AUC 0.5 al 1
pROC::roc()
roc(response = asah$outcome, predictor = asah$s100b, ci = T)
asah %>% roc(response = outcome, predictor = s100b, ci = T)

roc1 <- asah %>% roc(outcome, s100b, ci = T); roc1
roc2 <- asah %>% roc(outcome, ndka, ci = T); roc2
roc3 <- asah %>% roc(outcome, wfns, ci = T); roc3

## Comparar las AUROC con método de remuestreo ##
roc.test(roc1, roc2, method = "bootstrap", boot.n=100)
roc.test(roc1, roc3, method = "bootstrap", boot.n=100)
roc.test(roc2, roc3, method = "bootstrap", boot.n=100)

# Se puede escribir como una fórmula
roc(formula = outcome ~ s100b, data = asah)
#Y obtener todos los ROC al mismo tiempo
roc(formula = outcome ~ s100b + ndka + wfns, data = asah)

## Se puede obtener el AUC de forma aislada ##
auc(roc1); auc(roc2); auc(roc3)

## Se puede obtener un AUC parcial (para cierto rango de valores de la especificidad) ##
auc(roc1, partial.auc = c(1, 0))
auc(roc1, partial.auc = c(1, 0.5))
auc(roc1, partial.auc = c(0.5, 0))


## 4: Graficar la curva ##

#Con la función base de plot
plot(roc1)
asah %>% roc(outcome, s100b, ci = T, plot = T) #Desde que se genera la ROC

## Suavizar la curva ##
smooth(roc1)
plot(smooth(roc1))


## Con ggplot2 (función ggroc) ##
ggroc(roc1)
ggroc(roc2)
ggroc(roc3)

rocs <- roc(formula = outcome ~ s100b + ndka + wfns, data = asah)
ggroc(rocs) #Todas juntas

#Personalizar ggplot
names(rocs) <- c("s100b", "ndka", "wfns")

ggroc(rocs, aes = c("linetype", "color"), size=1.15) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="black", linetype=1)+
  xlab("False Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") +
  labs(col="Scores", linetype="Scores") +
  theme(axis.title.x = element_text(size =17, face = 'plain'),
        axis.title.y = element_text(size =17, face = 'plain'),
        axis.text.x = element_text(size = 14, face ='plain'),
        axis.text.y = element_text(size = 14, face ='plain'),
        legend.text = element_text(size = 17))+ggpubr::theme_pubclean()+ggsci::scale_color_nejm()


## Mejor punto de corte (índice de Youden) con el paquete OptimalCutpoints ##

Youden <- optimal.cutpoints(X = wfns ~ outcome, tag.healthy = "Good",
                            methods = "Youden", data = asah, pop.prev = NULL, 
                            control = control.cutpoints(), ci.fit=FALSE, conf.level = 0.95)
summary(Youden)


#En caso de que la variable respuesta tuviera solo 0 y 1
asah$outcome_num <- as.numeric(aSAH$outcome=="Poor")
table(asah$outcome)
table(asah$outcome_num)

Youden <- optimal.cutpoints(X = wfns ~ outcome_num, tag.healthy = 0,
                            methods = "Youden", data = asah, pop.prev = NULL, 
                            control = control.cutpoints(), conf.level = 0.95)
summary(Youden)


## Puntos de corte diferentes para distintas categorías ##

Youden2 <- optimal.cutpoints(X = wfns ~ outcome, tag.healthy = "Good", categorical.cov = "gender",
                             methods = "Youden", data = asah, pop.prev = NULL, 
                             control = control.cutpoints(), ci.fit=FALSE, conf.level = 0.95)
summary(Youden2)

## Categorización y matriz de confusión ##

asah$wfns_cat<-factor(asah$wfns>4, labels = c("Good", "Poor"))
table("Test"=asah$wfns_cat,"Ref"=asah$outcome)
epi.tests(table("Test"=asah$wfns_cat,"Ref"=asah$outcome))

caret::confusionMatrix(asah$wfns_cat, aSAH$outcome)



### Analisis de Bland-Altman ####

#¿Qué tan precisas y reproducibles son nuestras mediciones?

#1: Datos con los que trabajaremos
# Se utilizan dos instrumentos diferentes para calcular una variable que se mide en 20 sujetos

#Supuestos: ¡Los valores deben estar en la misma escala! (ej. cm con cm, mmHg con mmHg, mg/dL con mg/dL)

A1 <- c(-0.358, 0.788, 1.23, -0.338, -0.789, -0.255, 0.645, 0.506, 
        0.774, -0.511, -0.517, -0.391, 0.681, -2.037, 2.019, -0.447, 
        0.122, -0.412, 1.273, -2.165)
B1 <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
        0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
        0.173, 1.508, -1.955)


#2: Visualizar nuestros datos
cbind(A1, B1)

#Puede existir correlación aunque no exista acuerdo
cor(A1, B1)
data.frame(A1,B1) %>% ggplot(aes(x=A1, y=B1)) + geom_point(color="midnightblue", alpha=0.6) +
  geom_smooth(method = "lm", color="indianred2") + theme_pubclean()

data.frame(A1, B1, "Dif"=A1-B1, "Mean"=(A1+B1)/2) 

#Cálculo de los estadísticos de Bland-Altman
ba1<-blandr.statistics(A1, B1 , sig.level=0.95)

#Presentación resumida de los estadísticos de Bland-Altman
blandr.output.text(A1, B1 , sig.level=0.95)

# 4. GRAFICAR
blandr.draw(A1, B1)
#Y= Diferencias entre métodos A y B
#X= Promedio de las mediciones como mejor estimador del valor real de la variable

#Personalizar gráfico
FBA1 <- blandr.draw(A1, B1) + ggtitle("Gráfico de Bland-Altman") + 
  theme_pubclean() + theme(plot.title = element_text(face="bold", hjust=0.5))


# Segundo ejemplo

#1: Variables que usaremos
A2 <- c (1, 5, 10, 20, 50, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 
         300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900,
         950, 1000)
B2 <- c (8, 16, 30, 24, 39, 54, 40, 68, 72, 62, 122, 80, 181, 259, 275, 
         380, 320, 434, 479, 587, 626, 648, 738, 766, 793, 851, 871, 957, 
         1001, 960)

#2: Visualizar nuestros datos
cbind(A2, B2)

#Puede existir correlación aunque no exista acuerdo
cor(A2, B2)
data.frame(A2,B2) %>% ggplot(aes(x=A2, y=B2)) + geom_point(color="midnightblue", alpha=0.6) +
  geom_smooth(method = "lm", color="indianred2") + theme_pubclean()

data.frame(A2, B2, "Dif"=A2-B2, "Mean"=(A2+B2)/2) 

#Cálculo de los estadísticos de Bland-Altman
ba1<-blandr.statistics(A2, B2 , sig.level=0.95)

#Presentación resumida de los estadísticos de Bland-Altman
blandr.output.text(A2, B2 , sig.level=0.95)

# 4. GRAFICAR
blandr.draw(A2, B2)
#Y= Diferencias entre métodos A y B
#X= Promedio de las mediciones como mejor estimador del valor real de la variable

#Personalizar gráfico
FBA2 <- blandr.draw(A2, B2) + ggtitle("Gráfico de Bland-Altman") + 
  theme_pubclean() + theme(plot.title = element_text(face="bold", hjust=0.5))

#Comparar ambos ejemplos
ggarrange(FBA1, FBA2)