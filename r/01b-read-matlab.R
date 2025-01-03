## Ryan Elmore
## Extract predictions
## 16 Sept 2024

library(R.matlab)
library(lubridate)
library(dplyr)
library(ggplot2)

## GW Benchmark
# df <- readMat("data/gybench-trnwin-12-stdize-1-demean-0.mat")
df <- readMat("Code/rdac_sims/maxP-12000-trnwin-60-gamma-2-stdize-0-demean-0-v2/iSim1.mat")

dat <- data.frame(date = lubridate::ym(df$dates), 
                  z_minus_3 = NA,
                  z_minus_2 = NA,
                  z_minus_1 = NA,
                  z_0 = NA,
                  z_1 = NA,
                  z_2 = NA,
                  z_3 = NA,
                  Y = as.vector(df$Y))
#pred = apply(results, 1, mean),

## Read sim data
dir_path <- "Code/rdac_sims/maxP-12000-trnwin-60-gamma-2-stdize-0-demean-0-v2/"
files <- dir(path = dir_path)


results <- matrix(NA, nc = 1000, nr = 1092)
## Note that the 165th entry is the prediction with 12000 RFFs for 12
## Note that the 91st entry is the prediction with 12000 RFFs for 60
## Note that the 82nd entry is the prediction with 12000 RFFs for 120
## Note that the 77th entry is the prediction with 12000 RFFs for 240
## Note that the 76th entry is the prediction with 12000 RFFs for 360

for(j in 1:7){
  cat(sprintf("\n Iteration: %s at %s \n", j, Sys.time()))
  for (i in seq_along(files)){
    if(i %% 50 == 0){
      cat(sprintf("\n -- File: %s at %s \n", i, Sys.time()))
    }
    try({
      str_file <- paste0(dir_path, files[i])
      tmp <- readMat(str_file)
      results[, i] <- tmp$Yprd[, 91, j]
    }
    )
  }
  dat[, j+1] <- apply(results, 1, mean)
}

write.csv(dat, "data/matlab-sims-rff-60-std-0-demean-0.csv")


df <- readMat("Code/rdac_sims/maxP-12000-trnwin-12-gamma-2-stdize-0-demean-0-v2/iSim1.mat")


p <- ggplot(data = dat,
            aes(date, pred))
p + geom_line() +
  theme_bw()
ggsave("fig/matlab-preds-12.pdf", hei = 6, wid = 8)

## GYdata

gy <- readMat("Code/EmpiricalAnalysis/Step1_Predictions/GYdata.mat")

tmp <- data.frame(dates = lubridate::ym(gy$dates),
                  return = gy$Y)

write.csv(tmp, "data/gy.csv", row.names = F)
