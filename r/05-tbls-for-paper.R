## Ryan Elmore
## 19 Feb 2025
## Tables for paper
##  revised: 08 July 2025

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(xtable)

tb_1 <- readRDS("data/rff-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("data/linear-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2) |> 
  mutate(method = ifelse(method == "RFF", "Ridge/RFF", "Ridge/GW"))
  
tb$penalty_f = factor(tb$penalty,
                      levels = c("None", "10^-3", "10^-2", "10^-1", "1",
                                 "10^+1", "10^+2", "10^+3"))

tmp <- tb |>  
  filter(date >= lubridate::ymd("1950-01-01")) |> 
  group_by(method, penalty_f, window) |> 
  summarize(m = mean(ts, na.rm = T),
            s = sd(ts, na.rm = T),
            sharpe = sqrt(12)*m/s) |> 
  ungroup() 

## Table One and Two

tmp |> 
  filter(method == "Ridge/RFF",
         !(penalty_f %in% c("10e+2", "10e-2"))) |> 
  select(-method) |> 
  rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
         Penalty = penalty_f, Window = window) |> 
  pivot_longer(3:5) |> 
  pivot_wider(names_from = Penalty) |> 
  xtable(caption = "The means, standard 
       deviations, and timing strategies under various penalty parameters in the 
       Ridge/RFF", 
         digits = c(0, 0, 1, rep(6, 5)), 
         label = "tab:sharpe_rff") |> 
  print.xtable(include.rownames = F,
               booktabs = T,
               hline.after = c(0, 3, 6, 9, 12, 15, 18))

tmp |> 
  filter(method == "Ridge/GW",
         !(penalty_f %in% c("10e+2", "10e-2"))) |> 
  select(-method) |> 
  rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
         Penalty = penalty_f, Window = window) |> 
  pivot_longer(3:5) |> 
  pivot_wider(names_from = Penalty) |> 
  xtable(caption = "The means, standard 
       deviations, and timing strategies under various penalty parameters in the 
       Ridge/GW", 
         digits = c(0, 0, 1, rep(6, 6)),
         label = "tab:sharpe_gw") |> 
  print.xtable(include.rownames = F,
               booktabs = T,
               hline.after = c(0, 3, 6, 9, 12, 15, 18))

### Tables one and two
# 
# tb_1 <- readRDS("data/rff-std-1-demean-0-data.rds") |> 
#   dplyr::mutate(date = lubridate::ymd(date))
# 
# tb_2 <- readRDS("data/linear-std-1-demean-0-data.rds") |> 
#   dplyr::mutate(date = lubridate::ymd(date))
# 
# tb <- rbind(tb_1, tb_2) |> 
#   mutate(method = ifelse(method == "RFF", "Ridge/RFF", "Ridge/GW"))
# 
# tb$penalty_f = factor(tb$penalty,
#                       levels = c("None", "10e-3", "10e-2", "10e-1", "10",
#                                  "10e+1", "10e+2", "10e+3"))
# 
# tmp <- tb |>  
# #  filter(date >= lubridate::ymd("1950-01-01")) |>
#   group_by(method, penalty_f, window) |> 
#   summarize(m = mean(ts, na.rm = T),
#             s = sd(ts, na.rm = T),
#             sharpe = sqrt(12)*m/s) |> 
#   ungroup() 
# 
# ## Table One/Two
# 
# tmp |> 
#   filter(method == "Ridge/RFF",
#          !(penalty_f %in% c("10e+2", "10e-2"))) |>  
#   select(-method) |> 
#   rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
#          Penalty = penalty_f, Window = window) |> 
#   pivot_longer(3:5) |> 
#   pivot_wider(names_from = Penalty) |> 
#   xtable(caption = "The means, standard 
#        deviations, and timing strategies under various penalty parameters in the 
#        Ridge/RFF", 
#          digits = c(0, 0, 1, rep(6, 5)), 
#          label = "tab:sharpe_rff_kelly") |> 
#   print.xtable(include.rownames = F,
#                booktabs = T,
#                hline.after = c(0, 3, 6, 9, 12, 15))
# 
# tmp |> 
#   filter(method == "Ridge/GW",
#          !(penalty_f %in% c("10e+2", "10e-2"))) |> 
#   select(-method) |> 
#   rename(Mean = m, `Std. Dev.` = s, `Sharpe` = sharpe, 
#          Penalty = penalty_f, Window = window) |> 
#   pivot_longer(3:5) |> 
#   pivot_wider(names_from = Penalty) |> 
#   xtable(caption = "The means, standard 
#        deviations, and timing strategies under various penalty parameters in the 
#        Ridge/GW", 
#          digits = c(0, 0, 1, rep(6, 6)),
#          label = "tab:sharpe_gw_kelly") |> 
#   print.xtable(include.rownames = F,
#                booktabs = T,
#                hline.after = c(0, 3, 6, 9, 12, 15, 18))
