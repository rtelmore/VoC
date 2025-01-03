## Ryan Elmore
## 12 Nov 2024
## Simple Combined Forecast

library(R.matlab)
library(ggplot2)

#tmp <- readMat("Code/rdac_sims/maxP-12000-trnwin-360-gamma-2-stdize-1-demean-0-v2/iSim1.mat")

gw <- readMat("Code/EmpiricalAnalysis/Step1_Predictions/GYdata.mat")
df <- data.frame(ym = lubridate::ym(gw$dates), 
                 Y = gw$Y,
                 gw$X)
#write.csv(df, "data/gw-data-from-matlab.csv")
window <- 360
N <- dim(df)[1]
results <- rep(NA, N)
results_mat <- matrix(NA, nr = N, nc = 14)
#i <- 13
for (j in 1:14){
  cat(sprintf("\n Iteration: %s at %s \n", j, Sys.time()))
  for (i in (window+1):N){
    index <- (i-window):(i-1)
    tmp_df <- data.frame(x = df[index, j+2],
                         y = df$Y[index])
    reg <- lm(y ~ x, data = tmp_df)
    results[i] <- predict(reg, newdata = data.frame(x = df[i, j+2]))
  }
  results_mat[, j] <- results
}

scf_df <- data.frame(date = df$ym, 
                     Y = df$Y,
                     scp = apply(results_mat, 1, mean))
write.csv(scf_df, "data/simple-combined-360.csv")
