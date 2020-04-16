library(cowplot)
library(ggplot2)
library(ggthemes)

trace=as.data.frame(read.csv( "./Results/Model1/results_model_analysis/COVID-19Piemonte-analysys-1.trace", sep = ""))
trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(trace$Time))
reference <- as.data.frame(t(read.csv("input/reference.csv",
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

ALLPlot<-lapply(1:length(l), function(i){
  
  if(length(l[[i]])>1){
    Trace_plot<-data.frame(Time=trace$Time,I=rowSums(trace[, l[[i]] ]) )
  }else{
    Trace_plot<-data.frame(Time=trace$Time, I=trace[, l[[i]] ] )
  }
  
pl<-ggplot( )+
    geom_line(data=Trace_plot,
              aes(x=Time,y=I))+
    geom_line(data=reference,
              aes(x=Time,y=reference[,i+1]),
              col="red")+
    theme(axis.text=element_text(size = rel(1.2), hjust = 0.5),
          axis.title=element_text(size=15,face="bold"),
          axis.line = element_line(colour="black"),
          plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
          legend.text=element_text(size=18),
          legend.title=element_text(size=20,face="bold"),
          legend.position="right",
          legend.key.size = unit(1.3, "cm"),
          legend.key.width = unit(1.3,"cm"),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))+
    labs(x="Days", y=ylabel[i],title = paste(titleList[i]) )+
    geom_vline(xintercept=as.Date("2020-02-25"), colour="blue", linetype = "longdash" )+
    geom_text(aes(x=as.Date("2020-02-28"), label="\n First public restriction", y= Inf), colour="blue",size=5) +
    geom_vline(xintercept=as.Date("2020-03-08"), colour="blue", linetype = "longdash" )+
    geom_text(aes(x=as.Date("2020-03-12"), label="\n Second public restriction", y= Inf), colour="blue", size=5)
  
  return(pl)
  
}
)


a0=c("c_La_a0","c_Li_a0","c_Lh_a0")
a1=c("c_La_a1","c_Li_a1","c_Lh_a1")
a2=c("c_La_a2","c_Li_a2","c_Lh_a2")
a3=c("c_La_a3","c_Li_a3","c_Lh_a3")
a4=c("c_La_a4","c_Li_a4","c_Lh_a4")
d=c("d_a1","d_a2","d_a3","d_a4")
l<-list(a0,a1,a2,a3,a4,c(a0,a1,a2,a3,a4),"d_a1","d_a2","d_a3","d_a4",d)

################################################
AgesNames=c("0-19 years old","20-39 years old","40-59 years old","60-79 years old","80++ years old","Comulative")

reference_big_hist<- lapply(1:6, function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist)  

l2<-list("c_La_a0","c_Li_a0","c_Lh_a0","c_La_a1","c_Li_a1","c_Lh_a1","c_La_a2","c_Li_a2","c_Lh_a2","c_La_a3","c_Li_a3","c_Lh_a3","c_La_a4"
,"c_Li_a4" ,"c_Lh_a4" ,c(paste0("c_La_a",0:4)), c( paste0("c_Li_a",0:4)),c( paste0("c_Lh_a",0:4) ) )

Idiff=lapply(1:length(l2), function(i){
  
  if(length(l2[[i]])>1){
    Trace_plot<-rowSums(trace[, l2[[i]] ])
  }else{
    Trace_plot<-trace[, l2[[i]] ] 
  }
  
  c(Trace_plot[1], diff(Trace_plot,lag = 1) )
})
Idiff=unlist(Idiff)


Trace_big_hist <- data.frame( Time = rep(trace$Time, length(l[[6]])+3 ),
                            I = c(unlist(trace[, l[[6]] ]), rowSums(trace[, paste0("c_La_a",0:4)] ),rowSums(trace[, paste0("c_Li_a",0:4)] ),rowSums(trace[, paste0("c_Lh_a",0:4)] )),
                            Idiff = Idiff,
                            Sympt=ordered( rep(rep(c("Iu","Iq","Ih"),each=length(trace$Time)),6), levels = c("Ih","Iq","Iu")[3:1]),
                            Ages=rep(AgesNames,each=length(trace$Time)*3) )


BIGHIST<- ggplot(data=Trace_big_hist,
                aes(x=Time,y=I) )+
  geom_line(data=reference_big_hist,
            aes(x=Time,y=I,
                col="red")) +
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  scale_fill_tableau("Tableau 20",direction=-1,name=" " ,
                     breaks=c("Ih","Iq","Iu"),
                     labels=c("Ih","Iq","Iu"))+
  scale_colour_tableau("Tableau 20",direction=-1,name=" " ,
                       breaks=c("Ih","Iq","Iu"),
                       labels=c("Ih","Iq","Iu"))+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = rel(1.2), hjust = 0.5),
        axis.title=element_text(size=15,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20,face="bold"),
        legend.position="bottom",
        legend.key.size = unit(1.3, "cm"),
        legend.key.width = unit(1.3,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold",size = 15))+
  labs(x="Days", y=ylabel[6],title = paste(titleList[6]) )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="blue", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="blue",size=5,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="blue", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="blue", size=5,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="blue", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="blue", size=5,hjust = 0)

ggsave(plot = BIGHIST,filename = "Plot/InfectsHistALL.pdf",
       dpi = 400, width = 35, height = 15,device = "pdf")

HIST<- ggplot(data=Trace_big_hist[which(Trace_big_hist$Ages == "Comulative"),],
              aes(x=Time,y=I) )+
  geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative" & reference_big_hist$Time >= "2020-02-24" ),],
            aes(x=Time,y=I,
                col="red"),size=1.1) +
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  scale_colour_manual("" , values= c("Ih"=NA,"Iq"=NA,"Iu"=NA,"red"="red"),
                      limits = c("Ih","Iq","Iu","red"),
                      labels = c("Ih","Iq","Iu","Surveillance\n data") )+
  scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA),
                    limits = c("Ih","Iq","Iu","red"),
                    labels = c("Ih","Iq","Iu","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free")+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.8),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))


############################################################################
## DEATHS

reference_big_hist_d<- lapply(1:5, function(j){
  data.frame(Time=reference$Time,D=reference[,j+7],Ages=AgesNames[j+1])
})
reference_big_hist_d<-do.call("rbind",reference_big_hist_d)  

Trace_big_histD<-data.frame( Time = rep(trace$Time, length(l[[11]])+1 ),
                             D = c(unlist(trace[, l[[11]] ]), rowSums(trace[, l[[11]]] ) ),
                             Ages=rep(AgesNames[-1],each=length(trace$Time)) )


BIGHIST_d<-ggplot(data=Trace_big_histD,
                  aes(x=Time,y=D) )+
  geom_bar( aes(fill="Deaths",col="Deaths" ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist_d,
            aes(x=Time,y=D,col="red")
            )

BIGHIST_d<-BIGHIST_d+
  scale_fill_tableau("Tableau 20",direction=-1,name=" ")+
  scale_colour_tableau("Tableau 20",direction=-1,name=" " )+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = rel(1.2), hjust = 0.5),
        axis.title=element_text(size=15,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20,face="bold"),
        legend.position="right",
        legend.key.size = unit(1.3, "cm"),
        legend.key.width = unit(1.3,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold",size = 15))+
  labs(x="Days", y= ylabel[11],title = paste(titleList[11]) ) +  theme(legend.position = "none") 

ggsave(plot = BIGHIST_d,filename = "Plot/DeathsHistALL.pdf",
       dpi = 400, width = 30, height = 10,device = "pdf")



HIST_d<-ggplot(data=Trace_big_histD[which(Trace_big_histD$Ages == "Comulative"),],
               aes(x=Time,y=D) )+
  geom_bar( aes(fill="Deaths",col="Deaths" ),
            stat="identity",alpha=.9)+
  geom_line(data=reference_big_hist_d[which(reference_big_hist_d$Ages == "Comulative"& reference_big_hist_d$Time >= "2020-02-24"),],
            aes(x=Time,y=D,
                col="red"))+
  facet_wrap(~ Ages, scales="free")+
  scale_colour_manual("" , values= c("Deaths"=NA,"red"="red"),
                      limits = c("Deaths","red"),
                      labels = c("D","Surveillance\n data") )+
  scale_fill_manual("" , values= c("Deaths"="#499894", "red"=NA),
                    limits = c("Deaths","red"),
                    labels = c("D","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free")+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.8),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Deaths" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))



ggsave(plot = plot_grid(plotlist = list(HIST,HIST_d),labels = c("A","B"),nrow=2,label_size = 20 ),filename = "Plot/Comulatives.pdf",
       dpi = 400, width = 15, height = 15,device = "pdf")


ggsave(plot = HIST_d,filename = "Plot/DeathsComulative.pdf",
       dpi = 400, width = 15, height = 8,device = "pdf")

