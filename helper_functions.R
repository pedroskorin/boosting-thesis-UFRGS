
##########################################
## Point Estimate Forecasting Functions ##
##########################################

# Data processing functions

lag_slicer <- function(n, w, a) {
  return(1-n*(1-a)/(n-w))
}

x_namer <- function(n) {
  names <- c()
  for(i in 1:n) {
    names <- c(names, paste0("x_", i)) 
  }
  return(names)
}

# L2-Boosting function
spline_boosting_reg <- function(Y_or, Y, X, v, h, ratio_start = 0.8, Mstop = 3500, df=4, degree = 3, differences = 2, learner_index, M_crit = "AIC") {
  
  n_tot <- length(Y)
  n_out <- ceiling(n_tot - ratio_start*n_tot)
  ind_out <- seq(to = n_tot, by = 1, length = n_out)
  Y_predicted <- c(Y_or[ind_out[1]])
  varimp_df <- data.frame(rep(0, (ncol(X))))
  selected_var <- c()
  selected_m <- c()
  
  for(i in 1:n_out){
    print(Sys.time())
    print(n_out)
    start.time <- Sys.time()
    
    ind_in <- seq(from = 1, to = ind_out[i] - (2*h-1), by = 1)
    x_reg <- X[head(ind_in,-1),] # x independent t = 1, ..., T.in-h
    x0_reg <- matrix(X[tail(ind_in,1)+(h-1),], nrow = 1)
    
    # expanding window
    y_dep <- Y[tail(ind_in,-1)]
    y_reg <- as.matrix(y_dep)
    trdata = cbind(y_reg, x_reg)
    bbsxnam <- c()
    
    k=1
    for (ind_spl in rep(learner_index, 12)) {
      if (ind_spl == 1) {
        bbsxnam <-c(bbsxnam, paste0("bbs(x_", k, ", df=",df,", degree =", degree, ", differences = ", differences,")"))
      } 
      if (ind_spl == 0) {
        bbsxnam <-c(bbsxnam, paste0("bols(x_", k, ")"))
      }
      k = k+1
    }
    
    fmla <- as.formula(paste("y_reg ~", paste(bbsxnam, collapse="+")))
    
    # finding m*
    model<-gamboost(fmla,
                    data=trdata,
                    family=Gaussian(),
                    control = boost_control(mstop = Mstop, nu = v))
    
    if (M_crit == "AIC") {
      AIC <- AIC(model, method = "corrected" , df = "actset")
      m_selected = mstop(AIC)
    }
    if (M_crit == "k-fold") {
      cvf <- cv(model.weights(model), type = "kfold", B=5)
      cvm <- cvrisk(model, folds = cvf, papply = lapply)
      
      mean_risk = rep(NA, ncol(cvm))
      
      for (l in 1:ncol(cvm)) {
        mean_risk[l] = mean(cvm[1:5,l])
      }
      
      #m_selected = mstop(cvm)
      m_selected = min(c(which(diff(mean_risk) > 0)[1], which.min(mean_risk)), na.rm = TRUE)
      
      plot(mean_risk, type="l",main = paste("k-fold | step = ", i, "| h_in = ", h))
      abline(v=m_selected, col="red")
      abline(v=mstop(cvm), col="blue")
    }
    
    x0_reg_df <- data.frame(t(data.frame(unlist(x0_reg))))
    colnames(x0_reg_df) <- colnames(x_reg)
    y_predicted <- unname(predict(model[m_selected], newdata = x0_reg_df,
                                  type = "response")[1,1])
    cat("Selected M is: ", m_selected, "\n")
    selected_m = append(selected_m, m_selected)
    
    # visualizing selected predictors varimp
    varimp_df_partial <- data.frame(varimp(model))
    sum_reduction <- sum(varimp_df_partial[,1])
    varimp_partial <- varimp_df_partial[,1]/sum_reduction
    varimp_df <- cbind(varimp_df, varimp_partial)
    
    # visualizing selected predictors frequency
    selected_var <- append(selected_var, list(model$xselect()))
    
    # output
    Y_predicted <- append(Y_predicted, Y_or[(ind_out[1]+i-(h))] * (1 + y_predicted))
    print(i/n_out)
    print(tail(Y_predicted,1))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("Time Taken was: ", time.taken, "| Expected time to end is: ", (n_out - i)*time.taken ))
    
  }
  results <- list(forecast = Y_predicted,
                  varimp = varimp_df[,-1],
                  selected = selected_var,
                  M_selected = selected_m
  )
  return(results)
}

# SARIMA Function
SARIMA_bench <- function(Y_or, h, ratio_start = 0.8) {
  
  n_tot <- length(Y_or)-1
  n_out <- ceiling(n_tot - ratio_start*n_tot)
  ind_out <- seq(to = n_tot, by = 1, length = n_out)
  Y_arima <- c(Y_or[ind_out[1]])
  print(n_out)
  coef_list = list()
  for(i in 1:n_out){
    ind_in <- seq(from = 1, to = ind_out[i] - h, by = 1)
    #bench <- arima((Y_or[ 1:(ind_out[i] - h + 1) ]), c(2,1,3)
    #              , seasonal = list(order = c(1,0,0), period = 12)
    #)
    bench <- auto.arima(Y_or[ 1:(ind_out[i] - h + 1) ], seasonal=T, stepwise = F, approximation = F)
    print((bench$coef))
    coef_list = append(coef_list, bench$coef)
    
    forecast_bench <- forecast(bench, h)
    y_predicted_bench <- forecast_bench$mean[h]
    Y_arima <- append(Y_arima, (y_predicted_bench))
    print(i/n_out)
  }
  
  results <- list(forecast = Y_arima, coef = coef_list)
  return(results)
}

#####################
## Other Functions ##
#####################

# Performance measures
evaluation <- function(Z, W, index, texto) {
  
  cat("Evaluation of", texto)
  
  MAPE = mean((abs((W[index+1])-(Z[-1]))/(W[index+1])))*100
  MAE = mean((abs((W[index+1])-(Z[-1]))))
  cat("\n MAPE:", MAPE)
  
  MPE = max(((Z[-1]) - (W[index+1]))/((W[index+1])))*100
  MNE = min(((Z[-1]) - (W[index+1]))/((W[index+1])))*100
  cat("\n MPE: ", MPE)
  cat("\n MNE: ", MNE)
  
  P90 = quantile(abs(((Z[-1]) - (W[index+1]))/((W[index+1])))*100, 0.9)
  P95 = quantile(abs(((Z[-1]) - (W[index+1]))/((W[index+1])))*100, 0.95)
  cat("\n P90: ", P90)
  cat("\n P95: ", P95)
  
  RMSFE = mean(((Z[-1]) - (W[index+1]))^2)
  cat("\n RMSFE: ", RMSFE)
  return(c(MAPE, MAE, RMSFE, P95, P90))

}

# Adding Lags (There is definitely an easier way to do this)
add_lags <- function(X,Y) {
  #X <- cbind(Y, X)
  
  X_sem_1 <- head(X,-1)
  X_sem_2 <- head(X_sem_1,-1)
  X_sem_3 <- head(X_sem_2,-1)
  X_sem_4 <- head(X_sem_3,-1)
  X_sem_5 <- head(X_sem_4,-1)
  X_sem_6 <- head(X_sem_5,-1)
  X_sem_7 <- head(X_sem_6,-1)
  X_sem_8 <- head(X_sem_7,-1)
  X_sem_9 <- head(X_sem_8,-1)
  X_sem_10 <- head(X_sem_9,-1)
  X_sem_11 <- head(X_sem_10,-1)
  
  X_s_0 <- tail(X,nrow(X_sem_11))
  X_s_1 <- tail(X_sem_1,nrow(X_sem_11))
  X_s_2 <- tail(X_sem_2,nrow(X_sem_11))
  X_s_3 <- tail(X_sem_3,nrow(X_sem_11))
  X_s_4 <- tail(X_sem_4,nrow(X_sem_11))
  X_s_5 <- tail(X_sem_5,nrow(X_sem_11))
  X_s_6 <- tail(X_sem_6,nrow(X_sem_11))
  X_s_7 <- tail(X_sem_7,nrow(X_sem_11))
  X_s_8 <- tail(X_sem_8,nrow(X_sem_11))
  X_s_9 <- tail(X_sem_9,nrow(X_sem_11))
  X_s_10 <- tail(X_sem_10,nrow(X_sem_11))
  X_s_11 <- tail(X_sem_11,nrow(X_sem_11))

  X_total <- cbind(X_s_0,
                  X_s_1,
                  X_s_2,
                  X_s_3,
                  X_s_4,
                  X_s_5,
                  X_s_6,
                  X_s_7,
                  X_s_8,
                  X_s_9,
                  X_s_10,
                  X_s_11
                  )
  
  troca_nome <- function(x, numero = n) {return(paste("L",n," - ", x, sep = ""))}
  nome_all <- c()
  for (i in 1:12) {
    n <- i
    nome_in <- unname(lapply(colnames(X), troca_nome))
    nome_all <- c(nome_all, nome_in)
  }
  colnames(X_total) <- unlist(nome_all)
  return(X_total)
}
