Target<-function(output)
{
  Infect_li<- output[,c("c_Li_a0","c_Li_a1","c_Li_a2","c_Li_a3","c_Li_a4")]
  Infect_lh <- output[,c("c_Lh_a0","c_Lh_a1","c_Lh_a2","c_Lh_a3","c_Lh_a4")]
  Infect<- Infect_li+Infect_lh
  
  Infect<- rowSums(Infect)
  
	return(Infect)
}