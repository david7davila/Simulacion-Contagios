#Columna 1: Identificador del individuo (nï¿½mero:ID)
#Columna 2: Posicion dentro del Tablero (fila)
#Columna 3: Posicion dentro del Tablero (columna)
#Columna 4: Estado del individuo [0:susceptible, 1:infectado, 2:vacunado, 3:recuperado, 4:latente, 5:muerto, 6:cuarentena]
#Columna 5: Edad [aï¿½os]
#Columna 6: Tasa de contagio [probabilidad]
#Columna 7: Tasa de letalidad [probabilidad]
#Columna 8: Tasa de vacunaciï¿½n [probabilidad]
#Columna 9: Periodo de latencia [días]
#Columna 10: Periodo de enfermedad [días]
#Columna 11: Radio de Movilidad
#Columna 12: Trabaja [0:No,1:Si]
#Columna 13: TipoTraslado [0:CochePropio, 1:Transporte publico]
#Columna 14: TipodeTrabajo [0:Notrabja, 1:BajoRiesgo,2:MedioRiesgo,3:AltoRiesgo]
#Columna 15: Tasa de recuperación [probabilidad]
#--------------------------------------------------------------------------------------------
Pob<-100
Dias<-100
#For principal
for(i in 1:Dias){ #Comienza el conteo en dias
  if(i==1){
    
    Mapa<-matrix(1,nrow=sqrt(Pob),ncol=sqrt(Pob)) 
    Poblacion<-matrix(NA,nrow=Pob,ncol=15)
    
    #0-Susceptible, 1-Infectado, 2-vacunado, 3-Recuperado, 4:Latente, 5:muerto, 6-Cuarentena
    Poblacion[,1]<-c(1:Pob) #Asignamos los id
    Poblacion[,4]<-rep(0) #Todos inician siendo Susceptible
    Poblacion[,11]<-rep(15) #Todos inician con total libertad
    Poblacion[,5]<-sample(0:100,size=Pob,TRUE) #Asigno edades aleatorias
    Poblacion[,12]<-rep(0) #Asigno que no trabajan a toda la poblacion
    Poblacion[,13]<-rep(1) #Asigno usan transporte publico a toda la poblacion
    Poblacion[,14]<-rep(0)
    Poblacion[,6]<-rep(.5) #Todos inician con una Tasa Contagio
    
    L<-length(Poblacion[,5][Poblacion[,5]>=18 & Poblacion[,5]<=65])
    Poblacion[,12][Poblacion[,5]>=18 & Poblacion[,5]<=65]<-sample(c(0,1),size=L,TRUE,prob=c(.6,.4)) #De los mayores de 18 asigna quienes trabajan y quienes no
    LC<-length(Poblacion[,5][Poblacion[,5]>=18 & Poblacion[,5]<=85])
    Poblacion[,13][Poblacion[,5]>=18 & Poblacion[,5]<=85]<-sample(c(0,1),size=LC,TRUE,prob=c(.4,.6)) #De los mayores de 18 que conducen
    LT<-length(Poblacion[,12][Poblacion[,12]==1])
    Poblacion[,14][Poblacion[,12]==1]<-sample(c(1,2,3),size=LT,TRUE,prob=c(0.3,0.6,0.1)) #De los que trabajan que tipo de riesgo
    PA<-sample(1:Pob,1)
    PX<-sample(1:sqrt(Pob),1)
    PY<-sample(1:sqrt(Pob),1)
    Poblacion[PA,2]<-PY #Coordenada en Y o Fila
    Poblacion[PA,3]<-PX #Coordenada en X o Columna
    Poblacion[PA,4]<-1 #Generamos el primer infectado del modelo
  }
  Poblacion<-MedidasPrecaucion(i,Poblacion)
}


#-----------------FUNCIONES ----------------------------------------------------------
#Hospital capacity 
MedidasPrecaucion=function(i,Poblacion){
  FASE1<-20   # Numero de infectados para activar la fase
  FASE2<-200  # Numero de infectados para activar la fase
  FASE3<-4000 # Numero de infectados para activar la fase
  ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
 
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1]
  d<-(x-1)
  T_Trabaja<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]
  d<-(x-1)
  T_Transporte<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por transporte publico
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]
  d<-(x-1)
  T_MedioR<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo de medio riesgo
  #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]
  d<-(x-1)
  T_AltoR<-(1+(d*.3)) #Proporcion de aumento de la tasa de infeccion por trabajo de alto riesgo

  Infectados<-length(Poblacion[,4][Poblacion[,4]==1]) #Numero de infectados

  if(i==1){ #Actualizacion de las probabilidades de contagio segun las caracteristicas de las personas
  Poblacion[,6][Poblacion[,12]==1]<-Poblacion[,6][Poblacion[,12]==1]*T_Trabaja
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]*T_Transporte
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]*T_MedioR
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]*T_AltoR
  }

  if(Infectados==FASE1){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.05,.95)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    Poblacion[,11]<-10
    Poblacion[,6]<-Poblacion[,6]*.95
  }
  if(Infectados==FASE2){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.1,.9)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    Poblacion[,11]<-5
    Poblacion[,6]<-Poblacion[,6]*.90
    ControlDeFronteras<-1
  }
  if(Infectados==FASE3){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.3,.7)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    Poblacion[,11]<-1
    Poblacion[,6]<-Poblacion[,6]*.80
  }
  if(ControlDeFronteras==0){
    A<-sample(c(3,0), 1, prob=c(0.5,0.5)) #Probabilidad de .5 que el que llegue al mapa este infectado
    PX<-sample(1:sqrt(Pob),1)
    PY<-sample(1:sqrt(Pob),1)
    if(A==3){
      Mapa[PX,PY]<-A #Se generan mas infectados que llegan a cualquier lugar del mapa  
    }
  }
  if(ControlDeFronteras==1){ 
    A<-sample(c(3,0), 1, prob=c(0.1,0.9)) #Probabilidad de .1 que el que llegue al mapa este infectado
    PX<-sample(1:sqrt(Pob),1)
    PY<-sample(1:sqrt(Pob),1)
    if(A==3 && Poblacion[PY,4]== 0 ){
      Mapa[PX,PY]<-A #Se generan Menos infectados que llegan a cualquier lugar del mapa  
      Poblacion[PY,4]<-6
    }
  }
  return(Poblacion)
}
