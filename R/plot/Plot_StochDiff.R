
id=1
library(ggplot2)
library(cowplot)

# If median = T then the median will be plotted, otherwise the mean
medianTrue = T

trace=as.data.frame(read.csv( "./Results/Model1/results_model_analysisStoch81/COVID-19Piemonte-analysys-1.trace", sep = ""))

### deleting if there are points beyond te final time
n_sim<-unique(table(trace$Time))

time_to_delete<-unique(trace$Time)[which(table(trace$Time)!=max(n_sim))]
if(length(time_to_delete)!=0) trace<-trace[-which(trace$Time%in%time_to_delete),]
n_sim<-max(n_sim)
####################
AgesNames=c("0-19 years old","20-39 years old","40-59 years old","60-79 years old","80++ years old","Comulative")


l2<-list("c_La_a0","c_Li_a0","c_Lh_a0","c_La_a1","c_Li_a1","c_Lh_a1","c_La_a2","c_Li_a2","c_Lh_a2","c_La_a3","c_Li_a3","c_Lh_a3","c_La_a4"
         ,"c_Li_a4" ,"c_Lh_a4" ,c(paste0("c_La_a",0:4)), c( paste0("c_Li_a",0:4)),c( paste0("c_Lh_a",0:4) ) )


trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time)) )

#### Reference:
reference <- as.data.frame(t(read.csv("input/reference.csv",
                                      header = FALSE,
                                      sep = "")))


death_percentages<-c(0.007,0.046,.375,.572)

Death_ages <-as.data.frame(matrix((reference[,6]),ncol=1) %*% matrix(death_percentages,ncol=4))


reference<- data.frame(Time=trace$Time[0:length(reference$V1)], reference[,1:5],rowSums(reference[,1:5]),Death_ages,reference[,6])
reference <- reference[which(reference$Time >= "2020-02-24" ),]

reference_big_hist<- lapply(1:6, function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Idiff=c(reference[1,j+1], diff(reference[,j+1],lag = 1) ),Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist) 


Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)

data<-lapply(1:length(l2), function(i){
  l2[[i]]->ltemp
  ### fixed the age and symptom class, rows= each simulation col=times
  sim_traces_tmp<-sapply(Time, function(k){
    if(length(ltemp)==1){
      trace[which(trace$Time==k),ltemp]}
    else{rowSums( trace[which(trace$Time==k),ltemp] ) }
  }) 
  
  if(medianTrue) m<-apply(sim_traces_tmp,2,median)
  else  m<-apply(sim_traces_tmp,2,mean)
  
  
  Idiff=c(m[1], diff(m,lag = 1) )
  data.frame(Time=Time,I=m,Idiff=Idiff)
  
})

dataGG<-do.call("rbind",data)

dataGG$Sympt =ordered( rep(rep(c("Iu","Iq","Ih"),each=length(Time)),6), levels = c("Ih","Iq","Iu")[3:1])
dataGG$Ages =rep(AgesNames,each=length(Time)*3)

 pl<- ggplot(data=dataGG[which(dataGG$Ages == "Comulative"),],
             aes(x=Time,y=Idiff) )+
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative"),],
            aes(x=Time,y=Idiff,col="red"),size=1.1)+
    scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red"),
                        limits = c("Ih","Iq","Iu","red"),
                        labels = c("Ih","Iq","Iu","Surveillance\n data") )+
    scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA),
                      limits = c("Ih","Iq","Iu","red"),
                      labels = c("Ih","Iq","Iu","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
   theme(axis.text=element_text(size = 15, hjust = 0.5),
         axis.text.x=element_text(angle=+90),
         axis.title=element_text(size=18,face="bold"),
         axis.line = element_line(colour="black"),
         plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
         legend.text=element_text(size=14),
         legend.position=c(.1,.75),
         legend.title=element_blank(),
         legend.key=element_blank(),
         legend.key.size = unit(.9, "cm"),
         legend.key.width = unit(.9,"cm"),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         plot.margin=unit(c(5,5,5,5),"mm"), 
         strip.background = element_blank(),
         strip.text.x = element_blank() )+
  labs(x="Days", y="Infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))



if(medianTrue){
  ggsave(plot = pl,filename = "StochMedianDiff.pdf",
         dpi = 400, width = 15, height = 8,device = "pdf")
}else{  ggsave(plot = pl,filename = "StochMeanDiff.pdf",
               dpi = 400,width = 15, height = 8,device = "pdf") }

