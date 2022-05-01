######################################################################
## Developed by: Pedro Skorin                                       ##
## Find more: https://github.com/pedroskorin/boosting-thesis-ufrgs  ##
## Paper:                                                           ##
## Forecasting with high-dimensional data using linear and          ##
## p-splines L2-boosting: An Exercise for the unemployment rate     ##
## in Brazil                                                        ##
######################################################################

rm(list=ls()) # clean environment
require(mboost)
require(forecast)
require(readxl)
source("helper_functions.R")

##############
## Data Imp ##
##############

X <- tail(read.csv("data/data.csv")[,-c(1,2)],-12)
date <- tail(read.csv("data/data.csv")[,2],-12)
info <- read.csv("data/info.csv")
X = X[,info$Filter==1]
Y_or <- tail(read.csv("data/des.csv")[,2],-11)
Y_all <- tail(read.csv("data/des.csv")[-1,3:14],-11)

##############
## Settings ##
##############

save_directory <- "objects/"    # Add / to end or leave empty!

# Boosting parameters
v_in <- 0.1               # Def=0.1   #
h_set <- 1:12             # Def=1:12  #
Mstop_in <- 1000          # Def=1000  #
ratio_start_in <- 0.75    # Def=0.75  #
store_objects <- 1        # Def=0     #

###################
## 2 Forecasting ##
###################

# Data Processing
ratio_start_lag <- lag_slicer(nrow(Y_all), 11 , ratio_start_in)

colnames(X) = info$Name[info$Filter==1]
X_lag <- add_lags(X,Y)
Y_all_lag <- tail(Y_all,-11)
Y_or_lag <- tail(Y_or,-11)
names <- colnames(X_lag)
colnames(X_lag) <- x_namer(ncol(X_lag))

# Ensure reproducibility
set.seed(12345)

for (h_in in h_set) {
  
  # Point Estimate  
  # Fit L2-Boosting kfold
  
  b <- spline_boosting_reg(Y_or_lag, Y_all_lag[,h_in], X_lag, v_in, h_in, ratio_start_lag, 500,
                           M_crit = "k-fold", learner_index = rep(0, ncol(X_lag)/12))

  bs <- spline_boosting_reg(Y_or_lag, Y_all_lag[,h_in], X_lag, v_in, h_in, ratio_start_lag, 200, df=4,
                            differences = 2, degree = 1, learner_index = rep(1, ncol(X_lag)/12), M_crit = "k-fold")

  bs_misto <- spline_boosting_reg(Y_or_lag, Y_all_lag[,h_in], X_lag, v_in, h_in, ratio_start_lag, 100, df=4,
                                  degree=1, differences = 2, learner_index = info$splines[info$Filter==1], M_crit = "k-fold")
  
  # Fit Sarima Benchmark
  b_sarima <- SARIMA_bench(Y_or, h = h_in, ratio_start = ratio_start_in)
  
  # Storing Objects
  if (store_objects) {
    saveRDS(b, file = paste(save_directory, "final_b_", as.character(h_in), ".RData", sep=""))
    saveRDS(bs, file = paste(save_directory, "final_bs_", as.character(h_in), ".RData", sep=""))
    saveRDS(bs_misto, file = paste(save_directory, "final_bs_misto_", as.character(h_in), ".RData", sep=""))
    saveRDS(b_sarima, file = paste(save_directory, "final_sarima_", as.character(h_in), ".RData", sep=""))
    
  }
}

##############################
## 3.1 Performance measures ##
##############################

# Index Configuration

n_tot_lag <- nrow(Y_all_lag)
n_out_lag <- ceiling(n_tot_lag - ratio_start_lag*n_tot_lag)
ind_out_lag <- seq(to = n_tot_lag, by = 1, length = n_out_lag)

n_tot <- nrow(Y_all)
n_out <- ceiling(n_tot - ratio_start_in*n_tot)
ind_out <- seq(to = n_tot, by = 1, length = n_out)
