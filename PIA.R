pob<-10000
Dias<-100
PeriodoIncubacion<-14
PeriodoEnfermedad<-42

#Columna 1: Identificador del individuo (número:ID)
#Columna 2: Posicion dentro del Tablero (fila)
#Columna 3: Posicion dentro del Tablero (columna)
#Columna 4: Estado del individuo [0:susceptible, 1:infectado, 2:vacunado, 3:recuperado, 4:latente, 5:muerto, 6:cuarentena]
#Columna 5: Edad [años]
#Columna 6: Tasa de contagio [probabilidad]
#Columna 7: Tasa de letalidad [probabilidad]
#Columna 8: Tasa de vacunación [probabilidad]

Mapa<-matrix(1,nrow=sqrt(pob),ncol=sqrt(pob)) 
Poblacion<-matrix(NA,nrow=pob,ncol=6)
colnames(Poblacion)<-c("Estado","Fila","Columna","Vacunado","Radio","TasaContagio")
DataPob<- as.data.frame(Poblacion)

#1-Susceptible, 2-Incubacion, 3-Infectado, 4-Recuperado, 5-Deceso
DataPob$Estado<-rep(1) #Todos inician siendo Susceptible
DataPob$Radio<-rep(15) #Todos inician con total libertad
#DataPob$Estado<-sample(1:5,size=10000,TRUE)

Mapa[sample(1:100,1),sample(1:100,1)]<-3 #Generamos el primer infectado del modelo

for(i in 1:Dias){ #Comienza el conteo en dias
  
  DataPob<-MedidasPrecaucion(i,DataPob)
  
}

#-----------------FUNCIONES ----------------------------------------------------------
#Hospital capacity 

MedidasPrecaucion=function(i,DataPob){
  FASE1<-10   # Numero de infectados para activar la fase
  FASE2<-200  # Numero de infectados para activar la fase
  FASE3<-2000 # Numero de infectados para activar la fase
  ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
  
  Infectados<-length(DataPob$Estado[DataPob$Estado==3]) #Numero de infectados
  
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
    if(A==3){
      Mapa[PX,PY]<-A #Se generan Menos infectados que llegan a cualquier lugar del mapa  
    }
  }
  if(Infectados>=FASE1){
    DataPob$Radio<-10
    DataPob$TasaContagio<-0.90
  }
  if(Infectados>=FASE2){
    DataPob$Radio<-5
    DataPob$TasaContagio<-0.60
    ControlDeFronteras<-1
  }
  if(Infectados>=FASE3){
    DataPob$Radio<-1
    DataPob$TasaContagio<-0.30
  }

  return(DataPob)
}