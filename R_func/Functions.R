Contact.generation <- function(alpha,times,matrixRData,alpha.variation=NULL, x=NULL,eta,probMask=NULL)
{
  
  load(matrixRData)
  
  C = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )
  C_q = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )
  C_h = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )
  
  for(i in 0:length(times) )
  {
    load(matrixRData)
    if( i  != 0 )
    {
      AL_tmp<-alpha[[i]]

      if(!is.null(x)){
         if( i == 2) AL_tmp<-list(S = rep(0,3), W = c( rep(x[7],3) ), O =  rep(x[8],3) )
         if( i == 3){
             AL_tmp<-list(S = rep(0,3), W = c( rep(x[9],3) ), O = rep(x[10],3)  )
         }
          
      }
      
      if(!is.null(AL_tmp$S) ) Ms= diag(AL_tmp$S) %*% Ms
      if(!is.null(AL_tmp$H) ) Mh= diag(AL_tmp$H) %*% Mh
      if(!is.null(AL_tmp$W) ){
        if(length(AL_tmp$W)==3) {
          Mw= diag(AL_tmp$W) %*% Mw
        }else{
          Mw= matrix(AL_tmp$W,nrow=3, byrow = T) * Mw
        }
      } 
      if(!is.null(AL_tmp$O) ){
        if(length(AL_tmp$O)==3) {
          Mo= diag(AL_tmp$O) %*% Mo
        }else{
          Mo= matrix(AL_tmp$O,nrow=3, byrow = T) * Mo
        }
      } 
      
      M= Ms+Mw+Mh+Mo 
      
      M_q = Mh*(1-eta[2])
      M_h = Mo*(1-eta[3])
      
      if(i!= length(times))
      {
        t=c(times[i],times[i+1])
      }else{
        t = c(times[i],365)
      }
      
    }else{
      M= Ms+Mw+Mh+Mo 
      t=c(0,times[1])
    }
    
    if(!is.null(probMask)  & i>=4)
    {  
      M = M * (1-probMask)
      M_quarantine = Mh*(1-eta[2])* (1-probMask)
      M_hospital = Mo*(1-eta[3])* (1-probMask)
    }else{
      M = M 
      M_quarantine = Mh*(1-eta[2])
      M_hospital = Mo*(1-eta[3])
    }
    
      C[i+1,]<-c( 0,t, c(t(M)) )
      C_q[i+1,]<-c(1, t, c(t(M_quarantine)) )
      C_h[i+1,]<-c(2, t, c(t(M_hospital)) )
    #save(Mall,Mh,Mo,Ms,Mw,file=paste0("MatrixTime",i,".RData"))
  }
  Ctot<-rbind(C,C_q,C_h)
  
  
  return(Ctot)
}

init_m <- function(n_file, x=NULL,perc.undetected=NULL)
{
  yini.names <- readRDS(n_file)
  
  yini <- rep(0,length(yini.names))
  dim(yini)<- c(1,length(yini.names))
  yini <- as.data.frame(yini)
  names(yini)<-yini.names
  yini["s_a0"]<-733130 
  yini["s_a1"]<-2780600
  yini["s_a2"]<-842676
  if (!is.null(x))
  {
    yini[c("i_a1_s1")]<-x[11]
  }else{
    yini[c("i_a1_s1")]<-1
  }
  
  if(perc.undetected =="1:1"){
    Ntot<-yini["s_a0"]+yini["s_a1"]+yini["s_a2"]
    
    yini["i_a0_s0"]<- x[12]*yini["s_a0"]/Ntot
    yini["i_a1_s0"]<- x[12]*yini["s_a1"]/Ntot
    yini["i_a2_s0"]<- x[12]*yini["s_a2"]/Ntot
    
  }else if(perc.undetected =="1:1.5")
  {
    yini["i_a1_s0"]<- 1.5*yini["i_a1_s1"]
  }else if(perc.undetected =="1:10")
  {
    yini["i_a1_s0"]<- 10*yini["i_a1_s1"]
  }else{
    warning("Error: no match with the undected individuals!!!")
  }
  
  return(matrix(yini, ncol = 1))
}


Death<-function(n,x=NULL)
{
  if(!is.null(x))
  {
    if(n==1){d=x[4]}# 2
    if(n==2){d=x[5]}# 3
  }
  else{
    if(n==1){d=runif(1,min=.001, max=.2) }
    if(n==2){d=runif(1,min=.01, max=.25) }
  }
  
  return(d)
}

k_calib<-function(min=400,max=1200,x=NULL){
  if(!is.null(x))
  {
    k=x[6] # 4
  }
  else{
    k= runif(min=min,max=max,n=1)
  }
  return(k) ##non ha senso ora
}

beta.generation<-function(min=0,max=1,x=NULL){
  if(!is.null(x))
  {
    b=x[1:3]
  }
  else{
    b= runif(n=3,min,max)
  }
  return(matrix(b, nrow = 1))
}

l.generation<-function(age,sy,p,x=NULL)
{
  prob<-matrix(c(p,1-p,1-p),ncol=3)
  colnames(prob) = paste0("s",0:2)
  row.names(prob) = paste0("a",0:2)
  # updated at 26/04
  # it is the mean!
  perc.H <-0.4495165
  perc.Q <- 1-perc.H
  
  prob[,2]<-prob[,2]*perc.Q
  prob[,3]<-prob[,3]*perc.H
  
  l=prob[age,sy]*1/5
  
  return(l)
}

SW.generation=function(time.activation = NULL,x=NULL)
{
  rate = matrix(0,ncol=365,nrow = 3)
 
  if(!is.null(x))
  {
    # from 01/04 to 04/05 there is detection in a2
    # rate[ 3, 41 : 74 ] <- x[length(x)]  
    rate[ 3, 41 : 365 ] <- x[length(x)]  
  }else{
    rate[ 3, 41 : 74 ] <- runif(1,0,1)
  }
    
  if(!is.null(time.activation))
  {
    for(i in 1: length(time.activation))
    {
      l=time.activation[[i]]
      t=l$time 
      a=l$age  # 1,2,3
      perc=l$perc
      for(j in 1:length(t))
      {
        if(j==length(t)) rate[ a, t[j]: length(rate[1,])] <- perc[j]
        else rate[ a, t[j]: (t[j+1]-1)] <- perc[j]
      }
      
    }
    
  }
  
  return(rate)

}
