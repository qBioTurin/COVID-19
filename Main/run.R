
library(epimod)

model_generation(net_fname = "net/COVID-19Piemonte.PNPRO", 
                 functions_fname = "cpp/transitions.cpp")

### reference updated until 04/16
# 
# sensitivity_analysis(n_config = 2,
#                      solver_fname = "net/COVID-19Piemonte.solver",
#                      f_time = 38,
#                      s_time = 1,
#                      parameters_fname = "input/plist.csv",
#                      functions_fname = "R_func/Functions.R",
#                      target_value_fname = "R_func/Target.R",
#                      #distance_measure_fname = "R_func/msqd.R",
#                      # reference_data = "input/reference.csv",
#                      parallel_processors = 1)

# optim<-c(0.0155,0.1465933,0.5258945,
#          0.0265,0.219,1800,
#          0.4739693,0.446729,0.184,0.104,
#          3.979615,.01)

optim<-c(0.0205,0.155,0.531,
         0.026765,0.22119,600,
         0.68,0.5,0.2,0.15,
         4,0.11)

model_calibration(solver_fname = "net/COVID-19Piemonte.solver",
                  f_time = 71,
                  s_time = 1,
                  parameters_fname = "input/plist.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  ini_v = optim,
                  lb= c(optim[c(1:3)]*.999,optim[c(4:5)]*.99 ,300, optim[c(7:10)]*.8, 3, .05),
                  ub= c(optim[c(1:3)]*1.001,optim[c(4:5)]*1.01 ,1000, optim[c(7:10)]*1.3, 5, .15) )

#####################################################################
# optim<-c(0.016,0.155,0.53,
#          0.02649735,0.2234019,
#          370,
#          0.6,0.5,0.2,0.1,
#          3.6,0.12)

# optim<-c(0.012, 0.11 ,0.39,
#          0.02649735,0.2234019,
#          370,
#          0.6,0.5,0.2,0.1,
#          11,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('./R_func/plot/PlotModelAnalysis.R')
paste(trace$c_SW_a2)

#########################
#### Stochastic simulation
optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario1"
folder = "results_model_analysisScenario1"
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")
source("R_func/plot/StochLinePlot.R")
############## Scenario 2
#### All open, no school

optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2"
folder = "results_model_analysisScenario2"
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")
source("R_func/plot/StochLinePlot.R")

############## Scenario 3
#### work open

optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario3.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario3"
folder = "results_model_analysisScenario3"
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")
source("R_func/plot/StochLinePlot.R")

############## Scenario 3 with detection
#### work open

optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario3detection.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario3detection"
folder = "results_model_analysisScenario3detection"
source("R_func/plot/StochLinePlot.R")
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")

############## Scenario 4
#### School and all open in September

optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 310,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario4.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario4"
folder = "results_model_analysisScenario4"
source("R_func/plot/StochLinePlot.R")
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")

############## Scenario 4 with detection
#### School and all open in September

optim<-c(0.016,0.155,0.53,
         0.02649735,0.2234019,
         370,
         0.6,0.5,0.2,0.1,
         4,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 310,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario4detection.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario4detection"
folder = "results_model_analysisScenario4detection"
source("R_func/plot/StochLinePlot.R")
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")
