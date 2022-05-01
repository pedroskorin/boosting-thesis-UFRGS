# Pedro Skorin's Undergraduate Thesis
# Plots

library(ggplot2)
library(grid)
library(hrbrthemes)


# Plot 1:
y_p1 <- tail(read.csv("des.csv")[,2],-11)
d_p1 <- as.Date(substr(tail(read.csv("data.csv")[,2],-11), 1, 10))
data2 <- data.frame("Time"= d_p1, "des"= y_p1)

# Value used to transform the data

p_1 = ggplot(data2, aes(x=Time)) +
  geom_line(aes(y=des), size=0.5, color="black") + 
  scale_y_continuous(
    # Features of the first axis
    name = "Unemployment Rate in MRSP (%)",
  )

ggsave("plots/des.pdf", p_1, width = 12/1.5, height = 6/1.5)

p_2 = ggplot(data2, aes(x=Time)) +
  geom_line(aes(y=des), size=0.5, color="black") + 
  scale_y_continuous(
    # Features of the first axis
    name = "Unemployment Rate in MRSP (%)",
  ) + geom_vline(xintercept = as.Date("2013-08-31"), linetype="dashed", color = "red")

ggsave("plots/des_2.pdf", p_2, width = 12/1.5, height = 6/1.5)

# Plot 3 - serie estacionarizada:
y_p3 <- tail(read.csv("des.csv")[-1,3],-11)
d_p3 <- as.Date(substr(tail(read.csv("data.csv")[,2],-12), 1, 10))
data3 <- data.frame("Time"= d_p3, "des"= y_p3)

p_3 = ggplot(data3, aes(x=Time)) +
  geom_line(aes(y=des), size=0.5, color="black") + 
  scale_y_continuous(
    # Features of the first axis
    name = "Transformed Unemployment Rate in MRSP",
  ) + geom_vline(xintercept = as.Date("2013-08-31"), linetype="dashed", color = "red")

ggsave("plots/des_3.pdf", p_3, width = 12/1.5, height = 6/1.5)

# adf test

bacf <- acf(Y_or[1:212], plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  scale_y_continuous(name = "Autocorrelation Function") +
  scale_x_continuous(name = "Lag")
ggsave("plots/adf.pdf", q, width = 12/1.5, height = 3)

# transformation plots
library(tidyverse)

# create a list with a specific length 
plot_lst <- vector("list", length = 8)

for (i in 1:8) {
  g <- ggplot(data = mtcars, aes(x = hp, y = wt)) +
    geom_point()
  plot_lst[[i]] <- g
}

plot_stat_ts <- function(code, title) {
  time_series = code
  df_ts = read.csv(paste("C:\\Users\\pedro\\Documents\\Github\\IME-UFRGS-Research\\code_data\\", time_series, sep=""))
  data_ts <- data.frame("Time"= as.Date(df_ts$DATE[df_ts$DATE > "1995-01-01" & df_ts$DATE < "2019-06-31"]),
                        "TS"= df_ts[df_ts$DATE > "1995-01-01" & df_ts$DATE < "2019-06-31",7])
  p_ts = ggplot(data_ts, aes(x=Time)) +
    geom_line(aes(y=TS), size=0.5, color="black") +
    ggtitle(title) +
    theme(axis.title = element_blank())
  return(p_ts)
}

plot_lst[[1]] <- plot_stat_ts('BM12_IPCAPL12.csv', 'T0: Extended Consumer Price Index: market prices - rate of change')
plot_lst[[2]] <- plot_stat_ts('BM12_FCTN12.csv', 'T1: Monetary base conditioning factors - National Treasury')
plot_lst[[3]] <- plot_stat_ts('ANP12_COLCOM12.csv', 'TL0: Apparent consumption - fuel oil - daily average')
plot_lst[[4]] <- plot_stat_ts('GAC12_SALMINRE12.csv', 'TL1: Real minimum wage')
plot_lst[[5]] <- plot_stat_ts('BM12_PIB12.csv', 'TL2: Brazilian GDP')
plot_lst[[6]] <- plot_stat_ts('BM12_OPTFCN12.csv', 'TLK1: Payment Method - Expanded - M3')
plot_lst[[7]] <- plot_stat_ts('BM12_IPCAPLBSD12.csv', 'TR1: Prices - IPCA - free prices - semi-durable goods')
plot_lst[[8]] <- plot_stat_ts('BM12_IPCANCOM12.csv', 'TR2: Prices - IPCA - free prices - not tradable - var')

plot_ts = cowplot::plot_grid(plotlist = plot_lst, nrow = 4)
ggsave("plots/plot_ts.pdf", plot_ts, width = 12, height = 16)

par(mfrow=c(2,2))
hist(readRDS("objects/final_b_kfold_M500_9.RData")$M_selected, xlab="", main = "h=1", xlim = c(0,500))
hist(readRDS("objects/final_b_kfold_M500_10.RData")$M_selected, xlab="", main = "h=4", xlim = c(0,500))
hist(readRDS("objects/final_b_kfold_M500_11.RData")$M_selected, xlab="", main = "h=8", xlim = c(0,500))
hist(readRDS("objects/final_b_kfold_M500_12.RData")$M_selected, xlab="", main = "h=12", xlim = c(0,500))


# example plot - splines

x <- runif(300)
x1 <- x[x<0.3]
x2 <- x[x>=0.3 & x<0.6]
x3 <- x[x>0.6]

y1 <- 90*x1 + rnorm(length(x1), 0, 4)
y2 <- 30-12*x2 + rnorm(length(x2), 0, 4)
y3 <- 100*x3-40 + rnorm(length(x3), 0, 4)

plot(x = c(x1, x2, x3), y = c(y1, y2, y3))

df = data.frame(xvar = c(x1, x2, x3), yvar = c(y1, y2, y3))

ps1 <- ggplot(df, aes(x=xvar, y=yvar)) +
  geom_point(shape=1)  +
  scale_y_continuous(name = "Y") +
  scale_x_continuous(name = "X")

y <- c(y1, y2, y3)

x0 <- seq(0,1,0.01)

y1 <- x0
y2 <- ifelse(x0<0.3, 0, x0 - 0.3)
y3 <- ifelse(x0<0.6, 0, x0-0.6)
y0 <- rep(1, length(x0))

df2 = data.frame(xvar = x0, "base 1" = y0, "base 2" = y1, "base 3" = y2, "base 4" = y3)

test_data_long <- melt(df2, id="xvar")  # convert to long format

ps2 <- ggplot(data=test_data_long,
       aes(x=xvar, y=value, colour=variable)) +
  geom_line(size = 0.7)  +
  scale_y_continuous(name = "Y") +
  scale_x_continuous(name = "X")

plot(x, y1[order(y1)], type="l")
lines(x, y2)
lines(x, y3)  

model <- mboost(y ~ bbs(x, degree = 1, df=2, differences = 1, knots =3))

lines(seq(0,0.3,0.001), seq(0,0.3,0.001)*90)
lines(seq(0.3,0.6,0.001), 30 - 12*seq(0.3,0.6,0.001))
lines(seq(0.6,1,0.001), -28 + 85*seq(0.6,1,0.001))

df = df[order(df$xvar),]

splines <- function(x) {
  if (x < 0.3) {return(90*x)}
  if (x >= 0.3 & x < 0.6) {return(30 - 12*x)}
  if (x >= 0.6) {return(-28 + 85*x)}
}

df3 = cbind(df, "splines" = unlist(lapply(df$xvar, splines)))

ps3 <- ggplot(data=df3, aes(x=xvar)) +
  geom_point(aes(y=yvar), shape=1) + 
  scale_colour_hue(h = c(180,0)) + 
  geom_line(aes(y=splines), color="red")   +
  scale_y_continuous(name = "Y") +
  scale_x_continuous(name = "X")

ggsave("plots/plot_spline_1.pdf", ps1, width = 12/1.5, height = 6/1.5)
ggsave("plots/plot_spline_2.pdf", ps2, width = 12/1.5, height = 6/1.5)
ggsave("plots/plot_spline_3.pdf", ps3, width = 12/1.5, height = 6/1.5)

# Plot results 

df_results <- data.frame("id" = seq(1,12), "Linear" = m_linear[,3]/m_arima[,3],
                         "Non-Linear" = m_nonlinear[,3]/m_arima[,3],
                         "Mixed" = m_mixed[,3]/m_arima[,3])

df_results_m <- melt(df_results, id="id")  # convert to long format

colnames(df_results_m) = c("id", "Model", "value")

df_rmsfe <- ggplot(data=df_results_m,
              aes(x=id, y=value, colour=Model)) +
  geom_line(size = 0.7)  +
  scale_y_continuous(name = "RMSFE in Relation to Benchmark") +
  scale_x_continuous(name = "Forecast Horizon", breaks = seq(1,12))

ggsave("plots/plot_rmsfe.pdf", df_rmsfe, width = 12/1.5, height = 5/1.5)
