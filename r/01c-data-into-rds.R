## Ryan Elmore
## 27 Jan 2025
## Process data into rds files

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

path_str <- "~/py_research/ridgeless-finance/data/"
files <- dir(path = path_str)
files_of_interest <- files[grep("^matlab-sims-rff.*demean\\-0\\.csv", files)]


tmp <- read.csv(paste0(path_str, files_of_interest[1])) |> 
  select(-X) |> 
  rename(return = Y)

return_df <- tmp |> 
  select(date, return)
saveRDS(return_df, "data/returns.rds")

for(file in files_of_interest){
  window_num <- as.numeric(str_extract(file, "\\d+"))
  print(window_num)
  tmp <- read.csv(paste0(path_str, file)) |>
    select(-X) |>
    rename(return = Y)
  
  rff_df <- tmp |> 
    select(-return) |> 
    rename_with(~gsub("z_", "", .x)) |> 
    pivot_longer(-1, names_to = "penalty", values_to = "y_hat") |> 
    left_join(
      tmp |> 
        mutate(across(starts_with("z_"), ~.x*return)) |> 
        rename_with(~gsub("z_", "", .x)) |> 
        select(-return) |> 
        pivot_longer(-1, names_to = "penalty", values_to = "ts")    
    ) |> 
    mutate(window = window_num)
  
  if(exists("results")){
    results <- rbind(results, rff_df)
  } else results <- rff_df
}

saveRDS(results, "data/rff-data.rds")

sharpes <- results |> 
  filter(date >= lubridate::ymd("1950-01-01")) |> 
  group_by(penalty, window) |> 
  summarize(m = mean(ts, na.rm = T),
            s = sd(ts, na.rm = T),
            sharpe = sqrt(12)*m/s) |> 
  ungroup()

saveRDS(sharpes, "data/rff-sharpes.rds")

write.csv(sharpes, "data/rff-sharpes.csv", row.names = F)
