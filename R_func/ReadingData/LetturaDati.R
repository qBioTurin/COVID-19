
#### Fissaare come working diretory la cartella COVID-19 del git


regione = "Piemonte"

setwd("~/Desktop/COVD19/COVID-19/dati-regioni/") 
library(readr)

listFile<-list.files("./",
                     pattern = ".csv")
listFile<-listFile[-which(listFile%in%c("dpc-covid19-ita-regioni.csv","dpc-covid19-ita-regioni-latest.csv"))]

DatiList<-lapply(listFile , function(x) {
  dati <- read_csv(x)
  
  datiReg<-dati[dati$denominazione_regione==regione,]
  
  datiReg[,c("data","nuovi_positivi", "totale_positivi", "totale_ospedalizzati", "isolamento_domiciliare",  "terapia_intensiva", "ricoverati_con_sintomi", "deceduti","totale_casi","dimessi_guariti")]
})

DatiCOVID <- do.call("rbind", DatiList)

DatiCOVID<-DatiCOVID[order(DatiCOVID$data),]

DatiCOVIDlast<-DatiCOVID[,c("data","totale_casi","totale_ospedalizzati", "deceduti")]


mean((DatiCOVID$totale_ospedalizzati)/ DatiCOVID$totale_casi)

### perc prese dal report Seremi
#aggiornate al 26/04

percentages<-c(1.1,54.2,44.7)

#percentages<-c(65+71, 665+1006+1877+2827+2182, 2460+3026+1128)/DatiCOVID$totale_casi[length(DatiCOVID$totale_casi)]

Cases_a1= DatiCOVIDlast$totale_casi*percentages[1]
Cases_a2= DatiCOVIDlast$totale_casi*percentages[2]
Cases_a3= DatiCOVIDlast$totale_casi*percentages[3]

setwd("~/Desktop/COVD19/OurGit/covid-19/input/")

DatiCOVID_ages <-data.frame(Cases_a1,Cases_a2,Cases_a3,DatiCOVIDlast$deceduti)
reference <-data.frame(c(0,0,0,Cases_a1),c(0,0,0,Cases_a2),c(0,0,0,Cases_a3),c(0,0,0,DatiCOVIDlast$deceduti))

p<-.252+.305+.24
death_percentages<-c(1-p,p)

Death_ages <-as.data.frame(matrix((DatiCOVIDlast$deceduti),ncol=1) %*% matrix(death_percentages,ncol=4))


write.table(t(reference),"./reference.csv",row.names = F, col.names= F, sep =" ")

#write.csv(DatiCOVIDlast,"./DatiCOVID_piemonte.csv",row.names = F)
#write.csv(DatiCOVID_ages,"./DatiCOVID_piemonte_ages.csv",row.names = F)




########à province
setwd("~/Desktop/COVD19/COVID-19/dati-province/") ## update at 04/01

library(readr)

listFile<-list.files("./",
                     pattern = ".csv")

listFile<-listFile[-which(listFile%in%c("dpc-covid19-ita-province.csv","dpc-covid19-ita-province-latest.csv"))]

DatiList<-lapply(listFile , function(x) {
  dati <- read_csv(x)
  
  datiRegione<-dati[dati$denominazione_regione=="Piemonte",]
  
  a<-as.data.frame(t(datiRegione[,c("totale_casi")]) )
  a$data=unique(datiRegione$data)
  
  colnames(a) <- c(datiRegione$denominazione_provincia,"Data")
  a
})

DatiCOVIDPr <- do.call("rbind", DatiList)

DatiCOVIDPr<-DatiCOVIDPr[order(DatiCOVIDPr$Data),]


write.csv(DatiCOVIDPr,"../../DatiCOVID_piemonteProvinceCasiTotali.csv",row.names = F)


