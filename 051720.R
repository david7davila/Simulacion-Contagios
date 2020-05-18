

     #Matriz de estados
     #1.- 
    pob=10000
    no_estados=4
    m_estados=matrix(NA,nrow=pob,ncol=no_estados)
    for(i in 1:pob){
      estados=rep(0,4)
      pos=sample(c(1:3),1)
      estados[pos]=1
      m_estados[i,]=estados
    }
    m_estados
    
    

    AAA<-c("A","B","C","D")
    for(i in AAA){
      print(i)

    }  
    
    #Impresion Numero 2