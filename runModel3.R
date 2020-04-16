##################################
#### Model 3: number of asyntomatic individual 1:10

library(epimod)

model_generation(net_fname = "net/COVID-19Piemonte.PNPRO", 
                 functions_fname = "cpp/transitions.cpp")



### Paramter fixed:
# death rates: 0.003310085,0.006914,0.05622526,0.29

optim<-c(0.008315868,0.117783,0.2097733,0.2771335,0.1191084,
         0.3212045, 0.5019978,0.265834,0.3004656,
         0.4773104,0.8481202,0.2435786,
         0.1149504,0.2649952,
         0.2038236, 900, .27 ,.55, .75 )

model_calibration(solver_fname = "net/COVID-19Piemonte.solver",
                  #solver_type = "TAUG",
                  f_time = 40,
                  s_time = 1,
                  parameters_fname = "input/Model3/plistCalibration.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  ini_v = optim,
                  lb= optim*.8,
                  ub= optim*1.2
)

optim<- c(0.0011493,0.00238,0.01080617,0.009774,0.02624,
          0.095823,0.1944663,0.3062032,0.2875488,
          0.2378576,0.5948769,0.4190774,
          0.380354,0.698265,
          0.458116,
          1000,0.36563,0.587574,0.8502)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 36,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/Model3/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('~/R_func/plot/PlotModelAnalysis.R')



############## different alpha
model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               n_config = 20,
               parameters_fname = "input/Model3/plistWIAlpha.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

#########################
model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",
               taueps = .001,
               n_run = 1000,
               parallel_processors = 20,
               parameters_fname = "input/Model3/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)
source('~/R_func/plot/PlotStochModelAnalysis.R')
