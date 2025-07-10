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
N_train <- 120
root_dir <- "Code/EmpiricalAnalysis/Step1_Predictions/tryrff_v2_SeparateSims/"
df <- readMat(paste0(root_dir, 
                     "maxP-12000-trnwin-",
                     N_train,
                     "-gamma-2-stdize-1-stdizey-0-demean-1-v2/iSim1.mat"))

dat <- data.frame(date = lubridate::ym(df$dates), 
                  "0.001" = NA,
                  "0.01" = NA,
                  "0.1" = NA,
                  "1" = NA,
                  "10" = NA,
                  "100" = NA,
                  "1000" = NA,
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
      results[, i] <- tmp$Bnrm[, 82, j]
    }
    )
  }
  dat[, j+1] <- apply(results, 1, mean)
}

# dat <- readRDS("data/avg-beta-norm-12.rds")
colnames(dat)[2:8] <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
saveRDS(dat, paste0("data/avg-beta-norm-", N_train, ".rds"))

df <- dat[, c(1, 2:8)] |> 
  tidyr::pivot_longer(!date, names_to = "penalty", values_to = "beta_norm")

p <- ggplot(data = df |> 
              filter(date >= ymd("1950-01-01")), 
            aes(x = date, y = beta_norm, col = penalty))

p + geom_line() +
  facet_grid(penalty ~ .) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Date", y = "Average L2 Norm") +
  scale_x_date(date_breaks = "5 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw()

ggsave(paste0("fig/beta-norm-", N_train, "-revision.pdf"), hei = 11, wid = 10)
