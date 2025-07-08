## Ryan Elmore
## Extract predictions
## 16 Sept 2024
##  Revised: 24 June 2025

library(R.matlab)
library(lubridate)
library(dplyr)
library(ggplot2)

## GW Benchmark
df <- readMat("Code/EmpiricalAnalysis/Step1_Predictions/tryrff_v2_SeparateSims/gybench-trnwin-360-stdize-1-stdizey-0-demean-1.mat")
tmp <- data.frame(timing_stategy = df$timing.gy,
                  Y_hat = df$Yprd.gy)

write.csv(tmp, "data/gybench-trnwin-360-stdize-1-stdizey-0-demean-1.csv", row.names = F)
