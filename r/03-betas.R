## Ryan Elmore
## Extract Beta
## 20 Dec 2024

library(R.matlab)
library(lubridate)
library(dplyr)
library(ggplot2)

## Read sim data
## Path to your data
dir_path <- "~/py_research/ridgeless-finance/Code/rdac_sims/maxP-12000-trnwin-12-gamma-2-stdize-1-demean-0-v2/"
files <- dir(path = dir_path)

# str_file <- paste0(dir_path, files[10])
# tmp <- readMat(str_file)
df <- readMat("~/py_research/ridgeless-finance/Code/rdac_sims/maxP-12000-trnwin-12-gamma-2-stdize-1-demean-0-v2/iSim1.mat")

dat <- data.frame(date = lubridate::ym(df$dates), 
                  z_minus_3 = NA,
                  z_minus_2 = NA,
                  z_minus_1 = NA,
                  z_0 = NA,
                  z_1 = NA,
                  z_2 = NA,
                  z_3 = NA,
                  Y = as.vector(df$Y))


results <- matrix(NA, nc = 1000, nr = 1092)
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
      results[, i] <- tmp$Bnrm[, 165, j]
    }
    )
  }
  dat[, j+1] <- apply(results, 1, mean)
}

saveRDS(dat, "data/betas.rds")
df <- dat[, c(1, 2:8)] |> 
  tidyr::pivot_longer(!date, names_to = "shrinkage", values_to = "beta_norm")

p <- ggplot(data = df, aes(x = date, y = beta_norm, col = shrinkage))

p + geom_line() +
  facet_grid(shrinkage ~ .) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Date", y = "Average L2 Norm") +
  theme_bw()

ggsave("fig/beta_norm.pdf", hei = 11, wid = 8.5)
