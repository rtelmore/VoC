## Ryan Elmore
## Extract Beta
## 20 Dec 2024
##  revised: 08 July 2025

library(R.matlab)
library(lubridate)
library(dplyr)
library(ggplot2)

## Read sim data
## Path to your data
N_train <- 60
root_dir <- "Code/EmpiricalAnalysis/Step1_Predictions/tryrff_v2_SeparateSims/"
df <- readMat(paste0(root_dir, 
                     "maxP-12000-trnwin-",
                     N_train,
                     "-gamma-2-stdize-1-stdizey-0-demean-1-v2/iSim1.mat"))

dat <- data.frame(date = lubridate::ym(df$dates), 
                  z_minus_3 = NA,
                  z_minus_2 = NA,
                  z_minus_1 = NA,
                  z_0 = NA,
                  z_1 = NA,
                  z_2 = NA,
                  z_3 = NA,
                  Y = as.vector(df$Y))

dir_path <- paste0(root_dir,
                   "maxP-12000-trnwin-",
                   N_train,
                   "-gamma-2-stdize-1-stdizey-0-demean-1-v2/")
files <- dir(path = dir_path)
N_files <- length(files)

results <- matrix(NA, nc = N_files, nr = 1092)
## Note that the 165th entry is the prediction with 12000 RFFs for 12
## Note that the 91st entry is the prediction with 12000 RFFs for 60
## Note that the 82nd entry is the prediction with 12000 RFFs for 120
## Note that the 77th entry is the prediction with 12000 RFFs for 240
## Note that the 76th entry is the prediction with 12000 RFFs for 360

#tmp$Bnrm

for(j in 1:7){
  cat(sprintf("\n Iteration: %s at %s \n", j, Sys.time()))
  for (i in seq_along(files)){
    if(i %% 50 == 0){
      cat(sprintf("\n -- File: %s at %s \n", i, Sys.time()))
    }
    try({
      str_file <- paste0(dir_path, files[i])
      tmp <- readMat(str_file)
      results[, i] <- tmp$Bnrm[, 91, j]
    }
    )
  }
  dat[, j+1] <- apply(results, 1, mean)
}

saveRDS(dat, paste0("data/avg-beta-norm-", N_train, ".rds"))

df <- dat[, c(1, 2:8)] |> 
  tidyr::pivot_longer(!date, names_to = "shrinkage", values_to = "beta_norm")

p <- ggplot(data = df, aes(x = date, y = beta_norm, col = shrinkage))

p + geom_line() +
  facet_grid(shrinkage ~ .) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Date", y = "Average L2 Norm") +
  theme_bw()

ggsave("fig/beta-norm-revision.pdf", hei = 11, wid = 8.5)
