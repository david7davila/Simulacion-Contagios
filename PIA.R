pob<-10000
Dias<-100
PeriodoIncubacion<-14
PeriodoEnfermedad<-42

#Columna 1: Identificador del individuo (nï¿½mero:ID)
#Columna 2: Posicion dentro del Tablero (fila)
#Columna 3: Posicion dentro del Tablero (columna)
#Columna 4: Estado del individuo [0:susceptible, 1:infectado, 2:vacunado, 3:recuperado, 4:latente, 5:muerto, 6:cuarentena]
#Columna 5: Edad [años]
#Columna 6: Tasa de contagio [probabilidad]
#Columna 7: Tasa de letalidad [probabilidad]
#Columna 8: Tasa de vacunaciï¿½n [probabilidad]
#Columna 9: Radio
#Columna 10: Trabaja [0:No,1:Si]
#Columna 11: TipoTraslado [0:Coche, 1:Transporte publico]
#Columna 12: TipodeTrabajo [0:Notrabja, 1:BajoRiesgo,2:MedioRiesgo,3:AltoRiesgo]

Mapa<-matrix(1,nrow=sqrt(pob),ncol=sqrt(pob)) 
Poblacion<-matrix(NA,nrow=pob,ncol=12)

#1-Susceptible, 2-Incubacion, 3-Infectado, 4-Recuperado, 5-Deceso, 6-Cuarentena
Poblacion[,1]<-c(1:pob) #Asignamos los id
Poblacion[,4]<-rep(0) #Todos inician siendo Susceptible
Poblacion[,9]<-rep(15) #Todos inician con total libertad
Poblacion[,5]<-sample(0:100,size=10000,TRUE) #Asigno edades aleatorias
Poblacion[,10]<-rep(0) #Asigno que no trabajan a toda la poblacion
Poblacion[,11]<-rep(1) #Asigno usan transporte publico a toda la poblacion
Poblacion[,12]<-rep(0)

L<-length(Poblacion[,5][Poblacion[,5]>=18 & Poblacion[,5]<=65])
Poblacion[,10][Poblacion[,5]>=18 & Poblacion[,5]<=65]<-sample(c(0,1),size=L,TRUE,prob=c(.6,.4)) #De los mayores de 18 asigna quienes trabajan y quienes no

LC<-length(Poblacion[,5][Poblacion[,5]>=18 & Poblacion[,5]<=85])
Poblacion[,11][Poblacion[,5]>=18 & Poblacion[,5]<=85]<-sample(c(0,1),size=LC,TRUE,prob=c(.4,.6)) #De los mayores de 18 que conducen

LT<-length(Poblacion[,10][Poblacion[,10]==1])
Poblacion[,12][Poblacion[,10]==1]<-sample(c(1,2,3),size=LT,TRUE,prob=c(0.3,0.6,0.1)) #De los que trabajan que tipo de riesgo

Mapa[sample(1:100,1),sample(1:100,1)]<-1 #Generamos el primer infectado del modelo

#For principal
for(i in 1:Dias){ #Comienza el conteo en dias
  
  Poblacion<-Contagio(i,Poblacion) 
  Poblacion<-MedidasPrecaucion(i,Poblacion)
  Poblacion<-Recuperacion(i,Poblacion)
  Graficar(Poblacion)
}

#-----------------FUNCIONES ----------------------------------------------------------
#Hospital capacity 

MedidasPrecaucion=function(i,Poblacion){
  FASE1<-20   # Numero de infectados para activar la fase
  FASE2<-400  # Numero de infectados para activar la fase
  FASE3<-4000 # Numero de infectados para activar la fase
  ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
  Trabaja<- 1.20 #Proporcion de aumento de la tasa de infeccion
  
 
  #Poblacion$TasaContagio[Poblacion$Trabja==1]<-0.90*Trabja
  
  Infectados<-length(Poblacion$Estado[Poblacion$Estado==3]) #Numero de infectados
  
  if(Infectados>=FASE1){
    Poblacion$Radio<-10
    Poblacion$TasaContagio<-Poblacion$TasaContagio*.80
  }
  if(Infectados>=FASE2){
    Poblacion$Radio<-5
    Poblacion$TasaContagio<-Poblacion$TasaContagio*.60
    ControlDeFronteras<-1
  }
  if(Infectados>=FASE3){
    Poblacion$Radio<-1
    Poblacion$TasaContagio<-Poblacion$TasaContagio*.30
  }
  
  if(ControlDeFronteras==0){
    A<-sample(c(3,0), 1, prob=c(0.5,0.5)) #Probabilidad de .5 que el que llegue al mapa este infectado
    PX<-sample(1:100,1)
    PY<-sample(1:100,1)
    if(A==3){
      Mapa[PX,PY]<-A #Se generan mas infectados que llegan a cualquier lugar del mapa  
    }
  }
  if(ControlDeFronteras==1){ 
    A<-sample(c(3,0), 1, prob=c(0.1,0.9)) #Probabilidad de .1 que el que llegue al mapa este infectado
    PX<-sample(1:100,1)
    PY<-sample(1:100,1)
    if(A==3 && Poblacion$Estado[PY]!= 3 && Poblacion$Estado[PY]!= 5){
      Mapa[PX,PY]<-A #Se generan Menos infectados que llegan a cualquier lugar del mapa  
      Poblacion$Estado[PY]<-6
    }
  }
  
  return(Poblacion)
}