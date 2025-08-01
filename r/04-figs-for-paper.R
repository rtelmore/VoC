## Ryan Elmore
## 22 Jan 2025
## Figures for paper

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

tb_1 <- readRDS("data/rff-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("data/linear-std-1-stdy-0-demean-1-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2) |> 
  mutate(method = ifelse(method == "RFF", "Ridge/RFF", "Ridge/GW"))

## Note: the penalties are actual 1e-3, 1e-2, etc., not 10e-3, 10e-2
##       but I'm not changing all the code
tb$penalty_f = factor(tb$penalty, 
                      levels = c("None", "10e-3", "10e-2", "10e-1", "10",
                                 "10e+1", "10e+2", "10e+3"))

p <- ggplot(data = tb |> 
              filter(date >= lubridate::ymd("1950-01-01"),
                     window == 120),
            aes(x = penalty_f, fill = penalty_f, y = ts))
p + geom_boxplot() + 
  facet_grid(method ~ .) +
  scale_fill_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Ridge Penalty",
       y = "Estimated Timing Strategy") +
  scale_y_continuous(limits = c(-0.001, 0.001)) +
  scale_x_discrete(labels = c("None", 0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
  theme_bw() +
  guides(fill = "none")

ggsave("fig/boxplots-ts-120-revision.png", height = 8.5, width = 8.5)

aaa_baa <- read.csv("data/aaa-baa.csv") |> 
  select(-Y) |> 
  rename(AAA = exAAARF, BAA = exBAARF) |> 
  pivot_longer(-1, names_to = "penalty_f", values_to = "ts") |> 
  mutate(date = mdy(date),
         method = "Ridge/GW") |> 
  rbind(read.csv("data/aaa-baa.csv") |> 
          select(-Y) |> 
          rename(AAA = exAAARF, BAA = exBAARF) |> 
          pivot_longer(-1, names_to = "penalty_f", values_to = "ts") |> 
          mutate(date = mdy(date),
                 method = "Ridge/RFF"))

aaa_baa$penalty_f = factor(aaa_baa$penalty_f, 
                      levels = c("10e-3", "10e-2", "10e-1", "10",
                                 "10e+1", "10e+2", "10e+3", "AAA", "BAA"))

tmp <- tb |> 
  filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
         window == 120) |> 
  bind_rows(aaa_baa) |> 
  filter(date >= ymd("1950-01-01")) |> 
  mutate(bond = ifelse(penalty_f %in% c("AAA", "BAA"),
                       "Interest Rate", "Regressions"))

p <- ggplot(data = tmp,
            aes(x = penalty_f, fill = penalty_f, y = ts))
p + geom_boxplot() + 
  facet_grid(method ~ .) +
  scale_fill_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Ridge Penalty and Security",
       y = "Estimated Timing Strategy and Interest Rate") +
  scale_y_continuous(limits = c(-0.01, 0.01)) +
  scale_x_discrete(labels = c(0.001, 0.1, 10, 1000, "AAA", "BAA")) +
  theme_bw() +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  guides(fill = "none")

ggsave("fig/boxplots-ts-120-appendix-revision.png", height = 8.5, width = 8.5)

pens <- c("0.001", "0.1", "10", "1000")
names(pens) <- c("10e-3", "10e-1", "10e+1", "10e+3")

p <- ggplot(data = tb |>
              filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
                     window == 120,
                     date >= ymd("1950-01-01")),
            aes(x = date, y = ts, color = penalty_f, group = penalty_f))

p + geom_line() + 
  facet_grid(penalty_f ~ method,
             labeller = labeller(penalty_f = pens)) +
  scale_color_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Date",
       y = "Estimated Timing Strategy") +
  scale_x_date(date_breaks = "5 year") +#, date_minor_breaks = "2 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw() +
  guides(col = "none")

ggsave("fig/ts-lineplot-120-revision.png", height = 6.5, width = 8)

p <- ggplot(data = tb |> 
              filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
                     window == 120,
                     date >= ymd("1950-01-01")),
            aes(x = date, y = y_hat, color = penalty_f, group = penalty_f))

p + geom_line() + 
  facet_grid(penalty_f ~ method,
             labeller = labeller(penalty_f = pens)) +
  scale_color_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Date",
       y = "Estimated Return") +
  scale_x_date(date_breaks = "5 year") +#, date_minor_breaks = "2 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw() +
  guides(col = "none")
ggsave("fig/return-lineplot-120-revision.png", height = 6.5, width = 8)

## 
returns <- read.csv("data/matlab-sims-rff-12.csv") |> 
  select(date, Y) |> 
  mutate(date = ymd(date),
         mov_avg = lag(rollapply(Y, 120, mean, align = "right", fill = NA)))

tb_w_returns <- tb |> 
  filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
         window == 120,
         date >= ymd("1950-01-01")) |> 
  left_join(returns)
  
p <- ggplot(data = tb_w_returns,
            aes(x = date, y = y_hat, color = penalty_f, group = penalty_f))

p + geom_line() + 
  facet_grid(penalty_f ~ method,
             labeller = labeller(penalty_f = pens),
             scales = "free_y") +
  geom_line(aes(x = date, y = mov_avg), col = "black") +
  scale_color_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Date",
       y = "Estimated Return") +
  scale_x_date(date_breaks = "5 year") +#, date_minor_breaks = "2 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw() +
  guides(col = "none")

ggsave("fig/return-ma-freey-lineplot-120-revision.png", height = 6.5, width = 8)
