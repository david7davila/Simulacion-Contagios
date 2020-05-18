pob=10000
m_estados=matrix(NA,nrow=pob,ncol=no_estados)
for(i in 1:pob){
  estados=rep(0,4)
  pos=sample(c(1:3),1)
  estados[pos]=1
  m_estados[i,]=estados
}
m_estados

for(i in 1:100){
  medidasprecaucion(i)
  
  
  
}
medidasprecaucion=function(i){
  if(i>=40){
    prob_cont=prob_cont-0.1
  }
  return 
}