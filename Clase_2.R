####FUNCIONES####
#1. Definiendo una funci�n

nombre<-function(#PAR�METROS){
 ){
  #Aqu� va a ir tu funci�n
  x
}

#2. Haciendo que mi funci�n haga algo

funcion1<-function(x){
  #Qu� requisitos tienen los par�metros (clase, dimensiones, etc) para la funci�n que est�n definiendo
  x<-x^2
  print(x)
}


#3. Proponiendo un condicional
data(iris); data1<-iris

funcion_flores<-function(x,y){
  #X corresponde a un vector num�rico de valores de longitudes de p�talo para mis 3 especies conocidas
  #Y corresponde a un vector categ�rico con valores de setosa, versicolor y virginica que me agrupe las observaciones por especie
  suma<-0
  suma2<-0
  suma3<-0
  contador<-1
  for(value in x){
    if(y[contador]=="setosa"){
      suma<-suma+x[contador]
    }else if(y[contador]=="versicolor"){
      suma2<-suma2+x[contador]
    }else{
      suma3<-suma3+x[contador]
    }
    contador<-contador+1
  }
print(c(suma, suma2, suma3))
}

#==
#<=
#>=
#>
#<


?ifelse()

x<-c(6:-4)
sqrt(x)  


sqrt(ifelse(x >= 0, as.numeric(x), NA)) 



#4. Loops
#4.1 FOR

#4.2 WHILE
funcion_flores2<-function(x,y,z,w){
  #X corresponde a un vector num�rico de valores de longitudes de p�talo para mis 3 especies conocidas
  #Y corresponde a un vector categ�rico con valores de setosa, versicolor y virginica que me agrupe las observaciones por especie
  #z corresponde al n�mero de iteraciones
  suma<-0
  suma2<-0
  suma3<-0
  contador<-1
  while(contador<z){
    if(y[contador]=="setosa"){
      suma<-suma+x[contador]
    }else if(y[contador]=="versicolor"){
      suma2<-suma2+x[contador]
    }else{
      suma3<-suma3+x[contador]
    }
    if(contador==w){
      break
    }else{
      contador<-contador+1
      next
    }
  }
  print(c(suma, suma2, suma3))
}

#7. Apply, sapply

?apply

filas<-c(seq(1,50,1))
columnas<-c(seq(1,50,1))

m1<-matrix(c(filas, columnas), nrow=10, ncol=10)
  
apply(X=m1, MARGIN = 2, FUN=function(x) x/2)  
