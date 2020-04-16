msqd<-function(reference, output)
{
  
  Infect_li<- output[-c(1:3),c("c_Li_a0","c_Li_a1","c_Li_a2","c_Li_a3","c_Li_a4")]
  Infect_lh <- output[-c(1:3),c("c_Lh_a0","c_Lh_a1","c_Lh_a2","c_Lh_a3","c_Lh_a4")]
  Infect<- Infect_li+Infect_lh
  
  Death <- output[-c(1:3),c("d_a1","d_a2","d_a3","d_a4")]
  
  infect_cases<- reference[-c(1:3),1:5]
  death_cases<- reference[-c(1:3),6]
  
  # updated at 23/02
  death_percentages<-c(0.007,0.046,.378,.569)
  
  Death_ages <-as.data.frame(matrix((death_cases),ncol=1) %*% matrix(death_percentages,ncol=4))
  
  infect_cases[infect_cases<1]=1
  Death_ages[Death_ages<1]=1
  
  if(length(infect_cases[,1]) != length(Infect[,1]) ){
    diff.Infect <- 10^9   ### it means that the lsoda resolution had some problems
  }else{
    diff.Infect <- sum( rowSums( ( Infect - infect_cases )^2/infect_cases )  +  rowSums( ( (Death - Death_ages )^2/Death_ages) ) ) 
  }
  
  return(diff.Infect)
}
# 
# reference <- as.data.frame(t(read.csv("input/reference.csv", header = FALSE, sep = "")))
# output=read.csv("./results_sensitivity_analysis/COVID-19Piemonte-sensitivity-1.trace", sep = "")
# output=read.csv("./results_model_analysis/COVID-19Piemonte-analysys-1.trace", sep = "")
