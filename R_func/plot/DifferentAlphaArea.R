library(cowplot)
library(ggplot2)
setwd("./../../Results/Model1/")
load("./results_model_analysisDIFFALPHA/COVID-19Piemonte-analysys.RData")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(4)

listFile<-list.files("./results_model_analysisDIFFALPHA/",
                     pattern = ".trace")

id.traces<-1:(length(listFile)-1)

ListTraces<-lapply(id.traces,
                   function(x){
                     trace.tmp=read.csv(paste0(
                       "./results_model_analysisDIFFALPHA/COVID-19Piemonte-analysys-",
                       x,
                       ".trace"), sep = "")
                     config[[4]][[x]][[3]]->alp
                     trace.tmp=data.frame(trace.tmp, ID=x, alpha = alp[length(alp)]  )
                     if(length(trace.tmp$ID)==71) return(trace.tmp)
                     
                   })

trace <- do.call("rbind", ListTraces)

n_rep<-unique(table(trace$Time))
trace$Time<-rep(seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))),n_rep)



### reference
reference <- as.data.frame(t(read.csv("./../../input/reference.csv",
                                      header = FALSE,
                                      sep = "")))


# updated at 23/02
death_percentages<-c(0.007,0.046,.378,.569)

Death_ages <-as.data.frame(matrix((reference[,6]),ncol=1) %*% matrix(death_percentages,ncol=4))


reference<- data.frame(Time=trace$Time[0:length(reference$V1)], reference[,1:5],rowSums(reference[,1:5]),Death_ages,reference[,6])
a0=c("c_Li_a0","c_Lh_a0")
a1=c("c_Li_a1","c_Lh_a1")
a2=c("c_Li_a2","c_Lh_a2")
a3=c("c_Li_a3","c_Lh_a3")
a4=c("c_Li_a4","c_Lh_a4")
d=c("d_a1","d_a2","d_a3","d_a4")
l<-list(a0,a1,a2,a3,a4,c(a0,a1,a2,a3,a4),"d_a1","d_a2","d_a3","d_a4",d)



titleList<-c("Infects 00-19 years old","Infects 20-39 years old","Infects 40-59 years old","Infects 60-79 years old","Infects 80++ years old","Infects all ages",
             "Deaths 20-39 years old","Deaths 40-59 years old","Deaths 60-79 years old","Deaths 80++ years old","Deaths all ages")
ylabel<-c("Total cases in a0","Total cases in a1","Total cases in a2","Total cases in a3","Total cases in a4","Total cases",
          "Total deaths in a1","Total deaths in a2","Total deaths in a3","Total deaths in a4","Total deaths")


AgesNames=c("0-19 years old","20-39 years old","40-59 years old","60-79 years old","80++ years old","Comulative")

Trace_big_hist<-data.frame( Time = rep(trace$Time, 6 ),
                            I = c(rowSums(trace[,a0] ),rowSums(trace[, a1] ),rowSums(trace[,a2]),rowSums(trace[,a3] ),rowSums(trace[, a4] ),rowSums(trace[,l[[6]]])),
                            Ages=rep(AgesNames,each=length(trace$Time)),
                            alpha= rep(trace$alpha,6), id=rep(trace$ID,6),
                            Interval = rep(0,length(trace$Time)*6 ))

intervals<-c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]" )
min = .55
max = .65
for( j in 1:4)
{
  Trace_big_hist$Interval[which( Trace_big_hist$alpha <= max & Trace_big_hist$alpha > min  )] <- intervals[j]
  min = min + .1
  max = max + .1
}




######################## Trace estimated:

traceEstim=as.data.frame(read.csv( "./results_model_analysisDIFFALPHA/COVID-19Piemonte-analysysAlpha3Estimated.trace", sep = ""))
traceEstim$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(traceEstim$Time))

Estim<-lapply(1:length(l),function(g){
  if( length(l[[g]])>1 ){
    return(rowSums(traceEstim[,l[[g]]] ) )
  }else{
    return(traceEstim[,l[[g]]] )
  }
})

Estim<-do.call("cbind",Estim)

EstimAges <-data.frame( Time = rep(traceEstim$Time, 6 ),
                   I = c( Estim[,1],Estim[,2],Estim[,3],Estim[,4],Estim[,5],Estim[,6] ) ,
                   Ages= rep(AgesNames,each=length(traceEstim$Time)),
                   alpha= "Estimated")


###########################################################################################
###### Plot with areas

InfoTraces<-lapply(1:4,function(j){
  
  Trace_big_hist[which( Trace_big_hist$Interval == intervals[j] ),] -> tmp
  info.tmp<-lapply(unique(Trace_big_hist$Time), function(date) {
    tmp[tmp$Time==date, c("I", "Ages") ] -> tmp2
    tapply(tmp2$I, tmp2$Ages, mean) -> Meantmp2
    tapply(tmp2$I, tmp2$Ages, max) -> Maxtmp2
    tapply(tmp2$I, tmp2$Ages, min) -> Mintmp2
    names(Meantmp2)->labelAges
    return(data.frame(Time = rep(date,length(labelAges)), Mean = Meantmp2,Max= Maxtmp2,Min = Mintmp2, Ages = labelAges  ))
  } )
  info<- do.call("rbind",info.tmp)
  return(data.frame(info, Interval=intervals[j] ) )
}
)
InfoTracesDataframe<- do.call("rbind",InfoTraces)

pl2<-  ggplot( data=InfoTracesDataframe[which(InfoTracesDataframe$Ages=="Comulative"),])+
  geom_ribbon(aes(x=Time,ymin=Min,ymax=Max,fill=Interval),color="grey70",alpha=0.4)+
  facet_wrap(~ Ages, scales="free", ncol = 2)+
  geom_line(data = EstimAges[which(EstimAges$Ages=="Comulative"),] , aes(x=Time, y=I, color="Estimated alpha_3"),size=1) +
  scale_colour_manual("alpha(March 21st)" , values= c("Low \n [0.55,0.65]"=NA,"Medium-Low  \n (0.65,0.75]"=NA,"Medium-High \n (0.75,0.85]"=NA,"High \n (0.85,0.95]"=NA,"Estimated alpha_3"="blue"),
                      limits = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated alpha_3"),
                      labels = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated evolution") )+
  scale_fill_manual("alpha(March 21st)" , values= c(cols,NA),
                    limits = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated alpha_3"),
                    labels = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated evolution") )+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=14),
        legend.position=c(.1,.75),
        legend.title=element_text(size=18,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases",title = "" )+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))

ggsave(plot = pl2,filename = "Plot/diffalpha.pdf",
       dpi = 400, width = 20, height = 10,device = "pdf")


##### split for each age class

pl2<-ggplot( data=InfoTracesDataframe)+
  geom_ribbon(aes(x=Time,ymin=Min,ymax=Max,fill=Interval),color="grey70",alpha=0.4)+
  facet_wrap(~ Ages, scales="free", ncol = 2)+
  scale_colour_manual("alpha(March 21st)" , values= c("Low \n [0.55,0.65]"=NA,"Medium-Low  \n (0.65,0.75]"=NA,"Medium-High \n (0.75,0.85]"=NA,"High \n (0.85,0.95]"=NA,"Estimated alpha_3"="blue"),
                      limits = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated alpha_3"),
                      labels = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated evolution") )+
  scale_fill_manual("alpha(March 21st)" , values= c(cols,NA),
                    limits = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated alpha_3"),
                    labels = c("Low \n [0.55,0.65]","Medium-Low  \n (0.65,0.75]","Medium-High \n (0.75,0.85]","High \n (0.85,0.95]","Estimated evolution") )+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=14),
        legend.position="bottom",
        legend.title=element_text(size=18,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases",title = "" )+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))



######################
#### DEATHS
Trace_big_hist<-data.frame( Time = rep(trace$Time,5 ),
                            D = c(trace[,"d_a1"] ,trace[,"d_a2"],trace[,"d_a3"],trace[,"d_a4"],rowSums(trace[, d] ) ),
                            Ages=rep(AgesNames[-1],each=length(trace$Time)),
                            alpha= rep(trace$alpha,5), id=rep(trace$ID,5) )

pl2<-ggplot( data=Trace_big_hist)+
  geom_line(aes(x=Time,y=D,col=alpha,group=id))+
  facet_wrap(~ Ages, scales="free", ncol = 2)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=14),
        legend.position="bottom",
        legend.title=element_text(size=18,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases",title = "" )+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))
