
##### First Step: upload the library EPIMOD and download the Docker containers
# To install the library: https://github.com/qBioTurin/epimod
library(patchwork)
library(epimod)
#downloadContainers()

#### Second Step: generate the solver of the Petri Net representing the COVID19 model
# saved in COVID-19Piemonte.PNPRO

model.generation(net_fname = "net/COVID-19Piemonte.PNPRO",
                 transitions_fname = "cpp/transitions.cpp")


#####################################################################
##### Here we exploit te best configuration discovered to solve the
##### system of ODEs corresponding to the model.

################ Scenario: BASELINE ###############################
#####################################################################

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,
				 0.4,0.3,
         4,100,
				 0.12)


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

# Automatically the plots regarding the Infected individuals and deaths
# are generated

source("R_func/plot/PlotGenerationMB.R")
plotBaseline = plot.generation("./COVID-19Piemonte_analysis/COVID-19Piemonte-analysis-1.trace")

plotBaseline$Infecttion$Comulative
plotBaseline$Infecttion$AgeDivision


######### Scenario 1A: Anticipiamo il terzo lockdown come secondo (al 2 MArzo) #################
#####################################################################

optim<-c(0.0095,0.08,0.285, # definiscono il vettore Beta ->
         0.019,0.33, # tassi di morte che vengono associati alle persone infetti (detected) adulte e anziane (rispettivamenbte)
         60, # defisce k
         0.4,0.3, # II restrizione  in perc delle matrici di contatto relative a W (work) O (other)
         0.0,0.0,  # III restrizione in perc delle matrici di contatto relative a W (work) O (other)
         4,100, # valori usati per definire il numero di infetti al tempo 0
         0.12) # tasso  detection in a2 from 01/04 to 04/05

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/ScenariNuovi/plist_Scenario1A.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

#cat("Valore di k:", optim[6], "\n") #per capire se prende il valore corretto di K ed effettivamente lo fa

source("R_func/plot/PlotGenerationMA.R")
plotSc1A = plot.generation("./COVID-19Piemonte_analysis/COVID-19Piemonte-analysis-1.trace")

plotSc1A$Infecttion$Comulative
plotSc1A$Infecttion$AgeDivision



######### Scenario 2A: Anticipiamo uso mascherina chirurgica #################
#####################################################################

optim<-c(0.0095,0.08,0.285, # definiscono il vettore Beta ->
         0.019,0.33, # tassi di morte che vengono associati alle persone infetti (detected) adulte e anziane (rispettivamenbte)
         60, # defisce k
         1,1, # II restrizione  in perc delle matrici di contatto relative a W (work) O (other)
         1,1,  # III restrizione in perc delle matrici di contatto relative a W (work) O (other)
         4,100, # valori usati per definire il numero di infetti al tempo 0
         0.12) # tasso  detection in a2 from 01/04 to 04/05


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/ScenariNuovi/plist_Scenario2A.csv",
               functions_fname = "R_func/FunctionsA.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source("R_func/plot/PlotGenerationMSA.R")
plotSc2A = plot.generation("./COVID-19Piemonte_analysis/COVID-19Piemonte-analysis-1.trace")

plotSc2A$Infecttion$Comulative
plotSc2A$Infecttion$AgeDivision



######### Scenario 3A: Anticipiamo uso mascherina ffp2 con I lockD #################
#####################################################################

optim<-c(0.0095,0.08,0.285, # definiscono il vettore Beta ->
         0.019,0.33, # tassi di morte che vengono associati alle persone infetti (detected) adulte e anziane (rispettivamenbte)
         60, # defisce k
         1,1, # II restrizione  in perc delle matrici di contatto relative a W (work) O (other)
         1,1,  # III restrizione in perc delle matrici di contatto relative a W (work) O (other)
         4,100, # valori usati per definire il numero di infetti al tempo 0
         0.12) # tasso  detection in a2 from 01/04 to 04/05


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/ScenariNuovi/plist_Scenario3A.csv",
               functions_fname = "R_func/FunctionsA.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source("R_func/plot/PlotGenerationMNA.R")
plotSc3A = plot.generation("./COVID-19Piemonte_analysis/COVID-19Piemonte-analysis-1.trace")

plotSc3A$Infecttion$Comulative
plotSc3A$Infecttion$AgeDivision

################ salvare i tre plot in colonna
plotBaseline$Infecttion$Comulative/plotSc1A$Infecttion$Comulative/plotSc2A$Infecttion$Comulative/plotSc3A$Infecttion$Comulative

plotBaseline$Infecttion$Comulative/plotSc1A$Infecttion$Comulative
plotSc2A$Infecttion$Comulative/plotSc3A$Infecttion$Comulative
