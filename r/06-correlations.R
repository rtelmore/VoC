## Ryan Elmore
## Correlations
## 23 July 2025

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

tb_1 <- readRDS("data/rff-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("data/linear-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2) |> 
  mutate(method = ifelse(method == "RFF", "Ridge/RFF", "Ridge/GW"),
         penalty = gsub("10", "1", penalty)) 

## Note: the penalties are actual 1e-3, 1e-2, etc., not 10e-3, 10e-2
##       but I'm not changing all the code

all_returns <- read.csv("data/matlab-sims-rff-12.csv") |> 
  select(date, Y) 

window_size <- 120

returns <- all_returns |> 
  mutate(date = ymd(date),
         mov_avg = lag(rollapply(Y, window_size, mean, align = "right", fill = NA)))

tb_w_returns <- tb |> 
  filter(window == window_size,
         date >= ymd("1950-01-01")) |> 
  left_join(returns)  

tb_w_returns |> 
  group_by(method, penalty) |> 
  na.omit() |>
  summarize(r = cor(y_hat, mov_avg))

