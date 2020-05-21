
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

optim<-c( 0.0085, 0.086,0.285,
          0.024,0.28,
          50,
          0.7,0.6,0.4,0.2,
          4,90,0.1)

model_calibration(solver_fname = "net/COVID-19Piemonte.solver",
                  f_time = 71,
                  s_time = 1,
                  parameters_fname = "input/plist.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  ini_v = optim,
                  lb= c(optim[c(1:3)]*.9,optim[c(4:5)]*.9999 ,10, optim[c(7:10)]*.8, 3,40, .09),
                  ub= c(optim[c(1:3)]*1.00001,optim[c(4:5)]*1.0001 ,200, optim[c(7:10)]*1.3, 5,100, .12) )

#####################################################################
optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)


model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('./R_func/plot/PlotModelAnalysis.R')
#paste(trace$c_SW_a2)

#### different alpha

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochEstimated"
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf(folder)) )


optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.75,0.65,
         4,100,0.12)


model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochWorst"
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf(folder)) )


#### best option
optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.1,0.05,
         4,100,0.12)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochBest"
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf(folder)) )

source("R_func/plot/HistogramDistributionDifferentAlpha.R")

#########################
#### Stochastic simulation

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)


model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario1"
folder = "results_model_analysisScenario1"
system(paste('mv', 
            sprintf("results_model_analysis"),
            sprintf(folder)) )
      
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")
source("R_func/Rt_calculation.R")
#source("R_func/plot/StochLinePlot.R")
