beta.generation <- function(min,max, x=NULL)
{
  if(is.null(x)){
    line<-runif(15,min=min,max=max )
  }else{
    line<-x[1:15]
  }
  
  beta<-data.frame(a0=line[1:5],a1=line[c(2,6:9)],a2=line[c(3,7,10:12)],a3=line[c(4,8,11,13:14)],a4=line[c(5,9,12,14,15)])
  rownames(beta)<-paste0("a",0:4)
  
  return(beta)
}

init_m <- function(n_file, x=NULL,perc.undetected=NULL)
{
  yini.names <- readRDS(n_file)
  
  yini <- rep(0,length(yini.names))
  dim(yini)<- c(1,length(yini.names))
  yini <- as.data.frame(yini)
  names(yini)<-yini.names
  yini["s_a0"]<-733130 
  yini["s_a1"]<-881208
  yini["s_a2"]<-1340552
  yini["s_a3"]<-1038395
  yini["s_a4"]<-363121
  yini[c("i_a4_s1","i_a3_s1","i_a2_s1")]<-1
  if(!is.null(perc.undetected)) yini[c("i_a4_s0","i_a3_s0","i_a2_s0")]<- 1
  
  return(matrix(yini, ncol = 1))
}

alpha <- function(alpha_variation, x=NULL)
{
  #### from 21 to 25 Febb  alpha=0
  #### from 26 to 08/03  alpha=0.3
  #### from 09/03 to .... Febb  alpha=0.8
  
  if(is.null(x))
  { ## this mean that we are doing sensitivity
    
    alf1 <- runif(1, min= .1, max= .3)
    alf2 <- runif(1, min= .5, max= .7)
    a<-c(rep(0,5),rep(alf1,12),rep(alf2, 60))
    
  }else
  {
    #
    a<-c(rep(0,5),rep(x[21],12),rep(x[22],13),rep(x[23],60))
    
    if(!is.null(alpha_variation))
    {
      if(is.numeric(alpha_variation)){
        al<-alpha_variation
      }else{
        al<-runif(1,min=x[22],max = .95)
      }
      
      a<-c(rep(0,5),rep(x[21],12),rep(x[22], 13),rep(al, 60))
    }
    
  }
  
  return(a)
  
}

eta<-function(e, x=NULL)
{
  return(matrix(e, ncol = 1))
}

Death<-function(n,x=NULL)
{
  if(!is.null(x))
  {
    if(n==1){d=x[16]}
    if(n==2){d=x[17]}
    if(n==3){d=x[18]}
    if(n==4){d=x[19]}
  }
  else{
    if(n==1){d=runif(1,min=.0001, max=.002) }
    if(n==2){d=runif(1,min=.001, max=.006) }
    if(n==3){d=runif(1,min=.01, max=.08) }
    if(n==4){d=runif(1,min=.1, max=.25) }
  }
  return(d)
}

k_calib<-function(n,x=NULL){
  return(x[20])
}

l_rates<-function(p,x=NULL)
{
  prob<-matrix(c(p,1-p,1-p),ncol=3)

  # updated at 28/03 
  #perc.H <- 0.6152938
  perc.H <- 0.4605658
  perc.I <- 1-perc.H
  
  prob[,2]<-prob[,2]*perc.I
  prob[,3]<-prob[,3]*perc.H
  
  return(prob*1/5)
  # return(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25),nrow = 5,byrow = TRUE))
  # return(matrix(c(1,2,3,4,5),nrow = 5,byrow = TRUE))
}




# lambda.generation<-function(n,perc.undetected,x=NULL)
# {
# 
#     # updated at 23/03 from Seremi report
#     perc.H <- 0.4855578
#     perc.I <- 0.5144422
#     if(perc.undetected<1)
#       {
#         lambdas <-c(perc.undetected,(1-perc.undetected)*perc.I,(1-perc.undetected)*perc.H)
#     }else{
#         # non penso funzioni se la perc>1
#         lambdas <-c(perc.undetected,perc.I,perc.H)
#         lambdas <-lambdas/sum(lambdas)
#       }
#   
#   l=1/5*lambdas[n]
#   
#   return(l)
# }