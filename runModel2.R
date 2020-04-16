##################################
#### Model 1: number of asyntomatic individual 1:1.4

library(epimod)

model_generation(net_fname = "net/COVID-19Piemonte.PNPRO", 
                 functions_fname = "cpp/transitions.cpp")



### Paramter fixed:
# death rates: 0.003310085,0.006914,0.05622526,0.29


optim<-c(0.00991,0.0101713,0.072,0.35105,0.37852,
         0.63961,0.5166,0.3916049,0.445661,
         0.692051,0.672406,0.65031924,
         0.881,0.953124,
         0.9509,
         900,
         0.29564,0.5571,0.815)

model_calibration(solver_fname = "net/COVID-19Piemonte.solver",
                  #solver_type = "TAUG",
                  f_time = 36,
                  s_time = 1,
                  parameters_fname = "input/Model2/plistCalibration.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  parallel_processors = 40,
                  ini_v = optim,
                  lb= optim*.8,
                  ub= optim*1.2
)


optim<-c(0.009015867,0.009044065,0.1604,0.2988197,0.3606761,
         0.4242012,0.7405782,0.2548115,0.4297474,
         0.5754576,0.7022766,0.355137,
         0.4570149,0.5208891,
         0.6070305,
         950.3511,0.2368297,0.552018,0.819)

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 36,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/Model2/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('~/R_func/plot/PlotModelAnalysis.R')



############## different alpha
model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               n_config = 20,
               parameters_fname = "input/Model2/plistWIAlpha.csv",
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
               parameters_fname = "input/Model2/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('./R_func/plot/PlotStochModelAnalysis.R')
