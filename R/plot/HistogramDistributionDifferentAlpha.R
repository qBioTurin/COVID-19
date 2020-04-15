id=1
library(ggplot2)
library(cowplot)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(4)[c(1,3,4)]

##### reading traces:
trace55=as.data.frame(read.csv( "./../../Results/Model1/results_model_analysisStoch55/COVID-19Piemonte-analysys-1.trace", sep = "") )
trace81=as.data.frame(read.csv( "./../../Results/Model1/rresults_model_analysisStoch81/COVID-19Piemonte-analysys-1.trace", sep = "") )
trace95=as.data.frame(read.csv( "./../../Results/Model1/rresults_model_analysisStoch95/COVID-19Piemonte-analysys-1.trace", sep = "") )

trace55$alpha<-"0.55"
trace81$alpha<-"0.81"
trace95$alpha<-"0.95"

trace <- rbind(trace55,trace81,trace95)
### deleting if there are points beyond te final time
n_sim<-unique(table(trace$Time))

time_to_delete<-unique(trace$Time)[which(table(trace$Time)!=max(n_sim))]
if(length(time_to_delete)!=0) trace<-trace[-which(trace$Time%in%time_to_delete),]
n_sim<-max(n_sim)
####################

trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time)) )

Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)
# Get the indexes of the places to consider


a0=c("c_Li_a0","c_Lh_a0")
a1=c("c_Li_a1","c_Lh_a1")
a2=c("c_Li_a2","c_Lh_a2")
a3=c("c_Li_a3","c_Lh_a3")
a4=c("c_Li_a4","c_Lh_a4")
d=c("d_a1","d_a2","d_a3","d_a4")
l<-list(a0,a1,a2,a3,a4,c(a0,a1,a2,a3,a4),"d_a1","d_a2","d_a3","d_a4",d)

titleList<-c("Infects 00-19 years old","Infects 20-39 years old","Infects 40-59 years old","Infects 60-79 years old","Infects 80++ years old","",
             "Deaths 20-39 years old","Deaths 40-59 years old","Deaths 60-79 years old","Deaths 80++ years old","Deaths all ages")
ylabel<-c("Total cases in a0","Total cases in a1","Total cases in a2","Total cases in a3","Total cases in a4","Desease cases",
          "Total deaths in a1","Total deaths in a2","Total deaths in a3","Total deaths in a4","Desease deaths")

# AllPlot<-lapply(1:length(l),function(categ){
categ=6

ltemp<-l[[categ]]

# Compute the increments in the number of infects year by year
sim_traces_tmp<-lapply(0:((n_sim) -1),
                       function(k){
                         out<-trace[(1+k*(length(Time))):(length(Time)+k*length(Time)),ltemp]
                         if(length(ltemp)>1)
                         {
                           infected<-rowSums(out)
                         }else{
                           infected<-out
                         }
                         data.frame(Time= Time,
                                    I = infected, 
                                    Alpha = trace$alpha[(1+k*(length(Time))):(length(Time)+k*length(Time))] )
                         
                       }
)


sim_traces <- do.call("rbind", sim_traces_tmp)

sim_traces[which(sim_traces$Time == max(sim_traces$Time)),]->ggplotdf
myFun <- function(x) {
  c(median(x))
}

medians<-tapply(ggplotdf$I, ggplotdf$Alpha, myFun)
medians<-unlist(medians)

pl<- ggplot(data = sim_traces[which(sim_traces$Time == max(sim_traces$Time)),],aes(x=I)  )+
  geom_histogram(aes(y=..density.. , colour = Alpha),position="identity", fill = "white",bins = sqrt(n_sim))+
  geom_density(aes(fill=Alpha,group=Alpha),alpha=.2,position="identity")+
  scale_fill_manual("alpha(March 21st)",values=cols)+
  scale_colour_manual("alpha(March 21st)",values=cols)+
  geom_vline(xintercept=medians, colour="darkgreen", linetype = "longdash" )+
  theme(axis.text=element_text(size = 13, hjust = 0.5),
        axis.title=element_text(size=15,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=9),
        legend.position=c(.9,.85),
        legend.title=element_text(size=12,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(y="Density", x="Infected cases",title = "" )


ggsave(plot = pl, filename = "./StochFutComDistribution.pdf",
       dpi = 400, width = 12, height =8,device = "pdf")

