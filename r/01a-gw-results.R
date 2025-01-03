## Ryan Elmore
## Extract predictions
## 16 Sept 2024

library(R.matlab)
library(lubridate)
library(dplyr)
library(ggplot2)

## GW Benchmark
df <- readMat("Code/rdac_sims/gybench-trnwin-360-stdize-0-demean-0.mat")
tmp <- data.frame(timing_stategy = df$timing.gy,
                  Y_hat = df$Yprd.gy)

write.csv(tmp, "data/linear-360-std-0-demean-0.csv", row.names = F)
