##################################
#### Model 1: number of asyntomatic individual 1:1

library(epimod)

model_generation(net_fname = "net/COVID-19Piemonte.PNPRO", 
                 functions_fname = "cpp/transitions.cpp")

### reference updated until 04/01

# optim<-c(0.031409,0.0947,0.2144,0.2357,0.2956,
#          0.4531179,0.472473,0.5713654,0.620562,
#          0.4283095,0.8117375,0.9776311,0.6454852,0.6069407,0.8306476,0.00384672,0.007469435,0.05584898,0.2303179,900,0.21,0.55,0.75)

model_calibration(solver_fname = "net/COVID-19Piemonte.solver",
                  f_time = 40,
                  s_time = 1,
                  parameters_fname = "input/Model1/plistCalibration.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  ini_v = optim,
                  lb= optim*.6,
                  ub= optim*1.4
)

### optimal configuration estimated:

optim<-c(0.00991,0.0101713,0.072,0.35105,0.37852,
         0.63961,0.5166,0.3916049,0.445661,
         0.692051,0.672406,0.65031924,
         0.881,0.953124,
         0.9509,
         0.003310085,0.006914,0.05622526,0.29,900,
         0.29564,0.5571,0.815)


model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 40,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/Model1/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

source('./R_func/plot/PlotModelAnalysis.R')

############## different alpha
model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               n_config = 200,
               parameters_fname = "input/Model1/plistWIAlpha.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               parallel_processors = 20,
               ini_vector_mod = TRUE)

source('./R_func/plot/DifferentAlphaArea.R')

#########################
#### Stochastic simulation

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",
               taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/Model1/plistCalibration.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

##########################
## The extreme case of alpha_3 = .95

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",
               taueps = .001,
               n_run = 5000,
               parallel_processors = 30,
               parameters_fname = "input/Model1/plistWIAlpha_9.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

##########################
## The extreme case of alpha_3 = .55

model_analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",
               taueps = .001,
               n_run = 5000,
               parallel_processors = 30,
               parameters_fname = "input/Model1/plistWIAlpha_55.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)