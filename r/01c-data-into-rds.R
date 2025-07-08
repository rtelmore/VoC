## Ryan Elmore
## 27 Jan 2025
## Process data into rds files
##  revised: 08 July 2025


library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# path_str <- "~/py_research/ridgeless-finance/data/"
# files <- dir(path = path_str)
# files_of_interest <- files[grep("^matlab\\-sims\\-rff.*\\.csv", files)]
# files_of_interest <- files_of_interest[c(3, 13, 5, 7, 9, 11)]
#^linear.*1\\-demean\\-0\\.csv
# tmp <- read.csv(paste0(path_str, files_of_interest[1])) |> 
#   rename(return = Y)
rm(results)
files <- dir(path = "data/")
files_of_interest <- files[grep("^matlab", files)]
#file <- files_of_interest[6]

for(file in files_of_interest){
  window_num <- as.numeric(str_extract(file, "\\d+"))
  print(window_num)
  tmp <- read.csv(paste0("data/", file)) |>
    select(-X) |>
    rename(return = Y)
  
  rff_df <- tmp |> 
    select(-return) |> 
    rename("10e-3" = z_minus_3,
           "10e-2" = z_minus_2,
           "10e-1" = z_minus_1,
           "10" = z_0,
           "10e+1" = z_1,
           "10e+2" = z_2,
           "10e+3" = z_3) |> 
    pivot_longer(-1, names_to = "penalty", values_to = "y_hat") |> 
    left_join(
      tmp |> 
        mutate(across(starts_with("z_"), ~.x*return)) |> 
        rename("10e-3" = z_minus_3,
               "10e-2" = z_minus_2,
               "10e-1" = z_minus_1,
               "10" = z_0,
               "10e+1" = z_1,
               "10e+2" = z_2,
               "10e+3" = z_3) |> 
        select(-return) |> 
        pivot_longer(-1, names_to = "penalty", values_to = "ts")    
    ) |> 
    mutate(window = window_num,
           method = "RFF")
  
  if(exists("results")){
    results <- rbind(results, rff_df)
  } else results <- rff_df
}

saveRDS(results, "data/rff-std-1-stdy-0-demean-1-data.rds")

return_df <- tmp |> 
  select(date, return)
saveRDS(return_df, "data/returns.rds")

#return_df <- readRDS("data/returns.rds")
files <- dir(path = "data/")
files_of_interest <- files[grep("^gybench", files)]
#file <- files_of_interest[1]
for(file in files_of_interest){
  tmp <- read.csv(paste0("data/", file)) 
  window_num <- as.numeric(str_extract(file, "\\d+"))
  print(window_num)  
  linear_df <- tmp |> 
    select(starts_with("Y_hat")) |> 
    rename("None" = Y_hat.1,
           "10e-3" = Y_hat.2,
           "10e-2" = Y_hat.3,
           "10e-1" = Y_hat.4,
           "10" = Y_hat.5,
           "10e+1" = Y_hat.6,
           "10e+2" = Y_hat.7,
           "10e+3" = Y_hat.8) |>
    cbind(return_df) |> 
    select(-return) |> 
    pivot_longer(-date, names_to = "penalty", values_to = "y_hat") |> 
    left_join(
      tmp |> 
        select(starts_with("timing")) |> 
        rename("None" = timing_stategy.1,
               "10e-3" = timing_stategy.2,
               "10e-2" = timing_stategy.3,
               "10e-1" = timing_stategy.4,
               "10" = timing_stategy.5,
               "10e+1" = timing_stategy.6,
               "10e+2" = timing_stategy.7,
               "10e+3" = timing_stategy.8) |> 
        cbind(return_df) |> 
        select(-return) |> 
        pivot_longer(-date, names_to = "penalty", values_to = "ts")
    ) |> 
    mutate(window = window_num,
           method = "Linear")
  if(exists("results_linear")){
    results_linear <- rbind(results_linear, linear_df)
  } else results_linear <- linear_df
}

saveRDS(results_linear, "data/linear-std-1-stdy-0-demean-1-data.rds")

results <- readRDS("data/rff-std-1-stdy-0-demean-1-data.rds")
sharpes <- results |> 
  filter(date >= lubridate::ymd("1950-01-01")) |> 
  group_by(method, penalty, window) |> 
  summarize(m = mean(ts, na.rm = T),
            s = sd(ts, na.rm = T),
            sharpe = sqrt(12)*m/s) |> 
  ungroup() |> 
  rbind(
    results_linear |> 
      filter(date >= lubridate::ymd("1950-01-01")) |> 
      group_by(method, penalty, window) |> 
      summarize(m = mean(ts, na.rm = T),
                s = sd(ts, na.rm = T),
                sharpe = sqrt(12)*m/s) |> 
      ungroup()
  )

saveRDS(sharpes, "data/all-sharpes.rds")

# write.csv(sharpes, "data/all-sharpes.csv", row.names = F)
# 
# saveRDS(results_linear, "data/linear-data.rds")
# 
# fd <- readRDS("data/rff-data.rds")
