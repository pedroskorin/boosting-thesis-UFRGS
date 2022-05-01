# Pedro Skorin's Undergraduate Thesis
# Tables

# urca tests

summary(ur.df(Y_or[1:212], type = "none"))
summary(ur.df(Y_or[1:212], type = "drift"))
summary(ur.df(Y_or[1:212], type = "trend"))

summary(ur.df(diff(log(Y_or[1:212])), type = "none"))
summary(ur.df(diff(log(Y_or[1:212])), type = "drift"))
summary(ur.df(diff(log(Y_or[1:212])), type = "trend"))

# results

result_organizer = function(basename, original, index)
  {
  r1 = evaluation(readRDS(paste0(basename, "1.RData"))$forecast, original, index, "t")
  df = data.frame("MAE" = r1[1],
                  "MAPE" = r1[2],
                  "RMSFE" = r1[3],
                  "P95" = r1[4],
                  "P90" = r1[5])
  for (i in 2:12) {
    Ob = evaluation(readRDS(paste0(basename, as.character(i), ".RData"))$forecast, original, index, "t")
    newline = c(Ob[1], Ob[2], Ob[3], Ob[4], Ob[5])
    df = rbind(df, newline)
  }
  rownames(df) = 1:12
  return(df)
}

m_linear = result_organizer("objects/final_b_kfold_M500_", Y_or_lag, ind_out_lag)
m_nonlinear = result_organizer("objects/final_bs_kfold_M1000_d1_df4_", Y_or_lag, ind_out_lag)
m_mixed = result_organizer("objects/final_bs_misto_kfold_M700_d1_df4_", Y_or_lag, ind_out_lag)
m_arima = result_organizer("objects/b_autoarima_", Y_or, ind_out)

df_result = data.frame("Metric" = NA, "Model" = NA, "h=1" = NA,
                       "h=2" = NA,"h=3" = NA,"h=4" = NA,
                       "h=5" = NA,"h=6" = NA,"h=7" = NA,
                       "h=8" = NA,"h=9" = NA,"h=10" = NA,
                       "h=11" = NA,"h=12" = NA)
for (i in 1:5) {
  df_result = rbind(df_result, c(colnames(m_linear)[i], "linear", round(t(m_linear)[i,], digits = 4)))
  df_result = rbind(df_result, c(colnames(m_nonlinear)[i], "nonlinear", round(t(m_nonlinear)[i,], digits = 4)))
  df_result = rbind(df_result, c(colnames(m_mixed)[i], "mixed", round(t(m_mixed)[i,], digits = 4)))
  df_result = rbind(df_result, c(colnames(m_arima)[i], "arima", round(t(m_arima)[i,], digits = 4)))
}

write_xlsx(df_result, "resultados_final.xlsx")

# Diebold-Mariano Test

dm_test = function(basename1, basename2, original, n_tail, power_test, h_test) {
  return(dm.test(readRDS(paste0(basename1, as.character(h_test), ".RData"))$forecast[-1] - tail(Y_or, n_tail),
                 readRDS(paste0(basename2, as.character(h_test), ".RData"))$forecast[-1] - tail(Y_or, n_tail),
                  h = h_test, power = power_test, alternative = "less")$p.value)
}

dm_matrix = function(m_vector, original, n_tail, power_test, h_test) {
  matrix_dm = matrix(NA, nrow=4, ncol=4)
  k1=1
  k2=1
  for (i in m_vector) {
    for (j in m_vector) {
      if (i == j) {
        matrix_dm[k1,k2] = "-"
      } else {
      matrix_dm[k1,k2] = round(dm_test(i, j, original, n_tail, power_test, h_test), 4)
      }
      k2 = k2 + 1
    }
    k2 = 1
    k1 = k1 + 1
  }
  return(matrix_dm)
}

model_vectors = c('objects/final_b_kfold_M500_', 'objects/final_bs_kfold_M1000_d1_df4_', 'objects/final_bs_misto_kfold_M700_d1_df4_', 'objects/b_autoarima_')

df_dm_1 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 1),
                dm_matrix(model_vectors, Y_or, 71, 2, 2))
df_dm_2 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 3),
                dm_matrix(model_vectors, Y_or, 71, 2, 4))
df_dm_3 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 5),
                dm_matrix(model_vectors, Y_or, 71, 2, 6))
df_dm_4 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 7),
                dm_matrix(model_vectors, Y_or, 71, 2, 8))
df_dm_5 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 9),
                dm_matrix(model_vectors, Y_or, 71, 2, 10))
df_dm_6 = cbind(dm_matrix(model_vectors, Y_or, 71, 2, 11),
                dm_matrix(model_vectors, Y_or, 71, 2, 12))
df_dm = as.data.frame(rbind(df_dm_1, df_dm_2, df_dm_3, df_dm_4, df_dm_5, df_dm_6))
write_xlsx(df_dm, "df_dm_final.xlsx")

# Giacomini & White test of Predictive Ability

gw_test = function(basename1, basename2, original, n_tail, power_test, h_test) {
  return(gw.test(x = readRDS(paste0(basename1, as.character(h_test), ".RData"))$forecast[-1],
                 y = readRDS(paste0(basename2, as.character(h_test), ".RData"))$forecast[-1],
                 p = tail(Y_or, n_tail),
                 T = n_tail-1,
                 tau = h_test, alternative = "less", method = "HAC")$p.value)
}

gw_matrix = function(m_vector, original, n_tail, h_test) {
  matrix_gw = matrix(NA, nrow=4, ncol=4)
  k1=1
  k2=1
  for (i in m_vector) {
    for (j in m_vector) {
      if (i == j) {
        matrix_gw[k1,k2] = "-"
      } else {
        matrix_gw[k1,k2] = round(gw_test(i, j, original, n_tail, power_test, h_test), 4)
      }
      k2 = k2 + 1
    }
    k2 = 1
    k1 = k1 + 1
  }
  return(matrix_gw)
}

model_vectors = c('objects/final_b_kfold_M500_', 'objects/final_bs_kfold_M1000_d1_df4_', 'objects/final_bs_misto_kfold_M700_d1_df4_', 'objects/b_autoarima_')

df_gw_1 = cbind(gw_matrix(model_vectors, Y_or, 71, 1),
                gw_matrix(model_vectors, Y_or, 71, 2))
df_gw_2 = cbind(gw_matrix(model_vectors, Y_or, 71, 3),
                gw_matrix(model_vectors, Y_or, 71, 4))
df_gw_3 = cbind(gw_matrix(model_vectors, Y_or, 71, 5),
                gw_matrix(model_vectors, Y_or, 71, 6))
df_gw_4 = cbind(gw_matrix(model_vectors, Y_or, 71, 7),
                gw_matrix(model_vectors, Y_or, 71, 8))
df_gw_5 = cbind(gw_matrix(model_vectors, Y_or, 71, 9),
                gw_matrix(model_vectors, Y_or, 71, 10))
df_gw_6 = cbind(gw_matrix(model_vectors, Y_or, 71, 11),
                gw_matrix(model_vectors, Y_or, 71, 12))
df_gw = as.data.frame(rbind(df_gw_1, df_gw_2, df_gw_3, df_gw_4, df_gw_5, df_gw_6))
write_xlsx(df_gw, "df_gw_final.xlsx")

# Varimp

varimp_ranking <- function(model, var_names, n_top) {
  importance = model$varimp[[1]]
  for (i in 2:length(model$varimp)) {
    importance = importance + model$varimp[[i]]
  }
  importance = importance/length(model$varimp)
  names(importance) = var_names
  imp_df = data.frame("Name" = names(importance[order(importance, decreasing = T)])[1:n_top],
                      "Importance" = importance[order(importance, decreasing = T)][1:n_top])
  return(imp_df)
}

varimp <- varimp_ranking(readRDS("objects/final_b_kfold_M500_12.RData"),  c(1,names), 5)

write_xlsx(varimp, "varimp_l12.xlsx")

# Ceiling Value for M #

m_result_organizer = function(basename, original, index)
{
  df = data.frame("M_20" = evaluation(get(paste0(basename, "1_M20"))$forecast, original, index, "t")[3],
                  "M_50" = evaluation(get(paste0(basename, "1_M50"))$forecast, original, index, "t")[3],
                  "M_100" = evaluation(get(paste0(basename, "1_M100"))$forecast, original, index, "t")[3],
                  "M_300" = evaluation(get(paste0(basename, "1_M300"))$forecast, original, index, "t")[3])
  for (i in 2:12) {
    newline = c(evaluation(get(paste0(basename, as.character(i), "_M20"))$forecast, original, index, "t")[3],
                evaluation(get(paste0(basename, as.character(i), "_M50"))$forecast, original, index, "t")[3],
                evaluation(get(paste0(basename, as.character(i), "_M100"))$forecast, original, index, "t")[3],
                evaluation(get(paste0(basename, as.character(i), "_M300"))$forecast, original, index, "t")[3])
    df = rbind(df, newline)
  }
  rownames(df) = 1:12
  return(df)
}

m_linear = m_result_organizer("m_b_aic_", Y_or_lag, ind_out_lag)
m_nonlinear = m_result_organizer("m_bs_aic_", Y_or_lag, ind_out_lag)
m_mixed = m_result_organizer("m_bs_misto_aic_", Y_or_lag, ind_out_lag)

m_df_result = data.frame("Metric" = NA, "Model" = NA, "h=1" = NA,
                       "h=2" = NA,"h=3" = NA,"h=4" = NA,
                       "h=5" = NA,"h=6" = NA,"h=7" = NA,
                       "h=8" = NA,"h=9" = NA,"h=10" = NA,
                       "h=11" = NA,"h=12" = NA)
for (i in 1:4) {
  m_df_result = rbind(m_df_result, c(colnames(m_linear)[i], "linear", round(t(m_linear)[i,], digits = 4)))
  m_df_result = rbind(m_df_result, c(colnames(m_nonlinear)[i], "nonlinear", round(t(m_nonlinear)[i,], digits = 4)))
  m_df_result = rbind(m_df_result, c(colnames(m_mixed)[i], "mixed", round(t(m_mixed)[i,], digits = 4)))
}

write_xlsx(m_df_result, "m_resultados.xlsx")
