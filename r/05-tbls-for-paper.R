## Ryan Elmore
## 19 Feb 2025
## Tables for paper

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(xtable)

tb_1 <- readRDS("data/rff-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("data/linear-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2) |> 
  mutate(method = ifelse(method == "RFF", "Ridge/RFF", "Ridge/GW"))

tb$penalty_f = factor(tb$penalty,
                      levels = c("None", "10e-3", "10e-2", "10e-1", "10",
                                 "10e+1", "10e+2", "10e+3"))

tmp <- tb |>  
  filter(date >= lubridate::ymd("1950-01-01")) |> 
  group_by(method, penalty_f, window) |> 
  summarize(m = mean(ts, na.rm = T),
            s = sd(ts, na.rm = T),
            sharpe = sqrt(12)*m/s) |> 
  ungroup() 

## Table Three

tmp |> 
  filter(method == "Ridge/RFF") |> 
  select(-method) |> 
  rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
         Penalty = penalty_f, Window = window) |> 
  pivot_longer(3:5) |> 
  pivot_wider(names_from = Penalty) |> 
  xtable(caption = "The means, standard 
       deviations, and timing strategies under various penalty parameters in the 
       Ridge/RFF", 
         digits = c(0, 0, 1, rep(5, 7)), 
         label = "tab:sharpe_rff") |> 
  print.xtable(include.rownames = F,
               booktabs = T,
               hline.after = c(0, 3, 6, 9, 12, 15, 18))

tmp |> 
  filter(method == "Ridge/GW") |> 
  select(-method) |> 
  rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
         Penalty = penalty_f, Window = window) |> 
  pivot_longer(3:5) |> 
  pivot_wider(names_from = Penalty) |> 
  xtable(caption = "The means, standard 
       deviations, and timing strategies under various penalty parameters in the 
       Ridge/GW", 
         digits = c(0, 0, 1, rep(4, 8)),
         label = "tab:sharpe_gw") |> 
  print.xtable(include.rownames = F,
               booktabs = T,
               hline.after = c(0, 3, 6, 9, 12, 15, 18))

tmp |> 
  filter(method == "Ridge/RFF") |> 
  select(-method) |> 
  pivot_longer(3:5) |> 
  pivot_wider(names_from = penalty_f)
