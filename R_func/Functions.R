Contact.generation <- function(alpha,times,matrixRData,alpha.variation=NULL, optim_v=NULL,eta,probMask=NULL, type_mask=1)
{

  load(matrixRData)
	Mh_init = Mh
	Ms_init = Ms
	Mo_init = Mo
	Mw_init = Mw

	times_mask = c()
	prob_mask = c()
	times_restriction = times
	restriction_index = -1
	mask_index = -1
	C_index = 0
	if(!is.null(probMask)) {
		times_mask = probMask[["times"]]
		prob_mask = probMask[["prob"]]
		if(length(times_mask) != length(prob_mask)) {
			print("probmask MUST contain two vector of the same size.")
			options(warn=2)
		}
		times = sort(union(times_mask, times))
	}


  C = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )
  C_q = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )
  C_h = matrix(0, nrow = length(times)+1,ncol=length(Ms)+3 )

  if(length(times) == 0){
  	t = c(0,365)
  	M=Ms+Mw+Mo+Mh
  	M_quarantine = Mh*(1-eta[2])
  	M_hospital = Mo*(1-eta[3])
  	C[1,] <- c( 0,t, c(t(M)) )
  	C_q[1,] <-c(1, t, c(t(M_quarantine)))
  	C_h[1,] <- c(2, t, c(t(M_hospital)))
  } else {

  	for(i in 0:length(times)) {

	    Mh = Mh_init
	  	Ms = Ms_init
	  	Mo = Mo_init
	  	Mw = Mw_init

	    if(i != 0 && times[i] %in% times_restriction) {
					restriction_index = which(times_restriction == times[i]);
					C_index = C_index + 1
	    } else if( i !=0 && !(times[i] %in% times_restriction) && times[i] %in% times_mask ) {
	    	if(!(times_mask[1] < times_restriction[1]) && length(times_restriction) != 0){
	    			maxless <- max(times_restriction[times_restriction <= times[i]])
	    			restriction_index = which(times_restriction == maxless)
	    		} else {
	    			restriction_index = 0
	    		}
	    	C_index = C_index + 1
	  		# find out which value that is
	    } else if(i == 0 && length(times_restriction) != 0) {
	    	restriction_index = 0
	    }



	  	if(restriction_index != 0 && restriction_index != -1){
	      AL_tmp<-alpha[[restriction_index]]

	      if(!is.null(optim_v)){
	        if(restriction_index == 2) AL_tmp<-list(S = rep(0,3), W = c( rep(optim_v[7],3) ), O =  rep(optim_v[8],3) )
	        if(restriction_index  == 3){
	          AL_tmp<-list(S = rep(0,3), W = c( rep(optim_v[9],3) ), O = rep(optim_v[10],3)  )
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


	      M_q = Mh*(1-eta[2])
	      M_h = Mo*(1-eta[3])

	  	}

	  	if(i == 0){
	  		t=c(0,times[1])
	  	} else if(i != length(times)){
	  		t=c(times[i],times[i+1])
	  	}else{
	  		t = c(times[i],365)
	  	}

	  	M=Ms+Mw+Mo

	  	if(length(times_mask) !=  0 && i != 0 && times[i] %in% times_mask){

		    	mask_index <- which(times_mask == times[i])
	  	}

	  	if(mask_index != -1){
	  		prob_mask_i <- (prob_mask[mask_index] * type_mask)
	  		M = M * (1-prob_mask_i)
	  		M_quarantine = Mh*(1-eta[2])* (1-prob_mask_i)
	  		M_hospital = Mo*(1-eta[3])* (1-prob_mask_i)
	  	}else{
	  		M_quarantine = Mh*(1-eta[2])
	  		M_hospital = Mo*(1-eta[3])
	  	}

	  	M = M + Mh

		  C[C_index+1,]<-c( 0,t, c(t(M)) )
		  C_q[C_index+1,]<-c(1, t, c(t(M_quarantine)) )
		  C_h[C_index+1,]<-c(2, t, c(t(M_hospital)) )
  	}

	}

  Ctot<-rbind(C,C_q,C_h)


  return(Ctot)
}

init_m <- function(n_file, optim_v=NULL,perc.undetected=NULL)
{
  yini.names <- readRDS(n_file)

  yini <- rep(0,length(yini.names))
  dim(yini)<- c(1,length(yini.names))
  yini <- as.data.frame(yini)
  names(yini)<-yini.names
  yini["s_a0"]<-733130
  yini["s_a1"]<-2780600
  yini["s_a2"]<-842676
  if (!is.null(optim_v))
  {
    yini[c("i_a1_s1")]<-optim_v[11]
  }else{
    yini[c("i_a1_s1")]<-1
  }

  if(perc.undetected =="1:1"){
    Ntot<-yini["s_a0"]+yini["s_a1"]+yini["s_a2"]

    yini["i_a0_s0"]<- optim_v[12]*yini["s_a0"]/Ntot
    yini["i_a1_s0"]<- optim_v[12]*yini["s_a1"]/Ntot
    yini["i_a2_s0"]<- optim_v[12]*yini["s_a2"]/Ntot

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


Death<-function(n,optim_v=NULL)
{
  if(!is.null(optim_v))
  {
    if(n==1){d=optim_v[4]}# 2
    if(n==2){d=optim_v[5]}# 3
  }
  else{
    if(n==1){d=runif(1,min=.001, max=.2) }
    if(n==2){d=runif(1,min=.01, max=.25) }
  }

  return(d)
}

k_calib<-function(min=400,max=1200,optim_v=NULL){
  if(!is.null(optim_v))
  {
    k=optim_v[6] # 4
  }
  else{
    k= runif(min=min,max=max,n=1)
  }
  return(k) ##non ha senso ora
}

beta.generation<-function(min=0,max=1,optim_v=NULL){
  if(!is.null(optim_v))
  {
    b=optim_v[1:3]
  }
  else{
    b= runif(n=3,min,max)
  }
  return(matrix(b, nrow = 1))
}

age= "a0"
sy = "s0"
p=c(.5,.5,.5)

l.generation<-function(age,sy,p,optim_v=NULL)
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

SW.generation=function(time.activation = NULL,optim_v=NULL)
{
  rate = matrix(0,ncol=365,nrow = 3)

  if(!is.null(optim_v))
  {
    # from 01/04 to 04/05 there is detection in a2
    # rate[ 3, 41 : 74 ] <- optim_v[length(optim_v)]
    rate[ 3, 41 : 365 ] <- optim_v[length(optim_v)]
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
