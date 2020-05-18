
    print("Holoa")
    m_estados=matrix(NA,nrow=pob,ncol=length(estados))
    for(i in 1:dim(m_estados)[1]){
      estados=rep(0,8)
      pos=sample(c(3:8),1)
      estados[pos]=1
      m_estados[i,]=estados
    }
    m_estados