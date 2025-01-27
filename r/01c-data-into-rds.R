## Ryan Elmore
## 27 Jan 2025
## Process data into rds files

library(dplyr)
library(tidyr)
library(lubridate)

files <- dir(path = "~/py_research/ridgeless-finance/data/")
files_of_interest <- files[grep("^matlab-sims-rff.*demean\\-0\\.csv", files)]
tmp <- read.csv("~/py_research/ridgeless-finance/data/matlab-sims-rff-12-std-0-demean-0.csv") |> 
  select(-X) |> 
  rename(return = Y)

fd <- tmp |> 
  select(-return) |> 
  rename_with(~gsub("z_", "", .x)) |> 
  pivot_longer(-1, names_to = "penalty", values_to = "y_hat") |> 
  left_join(
    tmp |> 
      mutate(across(starts_with("z_"), ~.x*return)) |> 
      rename_with(~gsub("z_", "", .x)) |> 
      select(-return) |> 
      pivot_longer(-1, names_to = "penalty", values_to = "ts")    
  )

