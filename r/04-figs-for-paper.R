## Ryan Elmore
## 22 Jan 2025
## Figures for paper

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

tb_1 <- readRDS("data/rff-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb_2 <- readRDS("data/linear-data.rds") |> 
  dplyr::mutate(date = lubridate::ymd(date))

tb <- rbind(tb_1, tb_2)

tb$penalty_f = factor(tb$penalty, 
                      levels = c("None", "10e-3", "10e-2", "10e-1", "10",
                                 "10e+1", "10e+2", "10e+3"))

p <- ggplot(data = tb |> 
              filter(date >= lubridate::ymd("1950-01-01"),
                     window == 12),
            aes(x = penalty_f, fill = penalty_f, y = ts))
p + geom_boxplot() + 
  facet_grid(method ~ .) +
  scale_fill_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Ridge Penalty",
       y = "Estimated Timing Strategy") +
  scale_y_continuous(limits = c(-0.001, 0.001)) +
  theme_bw()

ggsave("fig/boxplots-ts-12.png", height = 8.5, width = 8.5)

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
  theme_bw()

ggsave("fig/boxplots-ts-120.png", height = 8.5, width = 8.5)

p <- ggplot(data = tb |> 
              filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
                     window == 120,
                     date >= ymd("1940-01-01")),
            aes(x = date, y = ts, color = penalty_f, group = penalty_f))

p + geom_line() + 
  facet_grid(penalty_f ~ method) +
  scale_color_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Date",
       y = "Estimated Timing Strategy") +
  scale_x_date(date_breaks = "5 year") +#, date_minor_breaks = "2 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw() +
  guides(col = "none")

ggsave("fig/ts-lineplot-120.png", height = 6.5, width = 8)

p <- ggplot(data = tb |> 
              filter(penalty %in% c("10e-3", "10e-1", "10e+1", "10e+3"),
                     window == 120,
                     date >= ymd("1940-01-01")),
            aes(x = date, y = y_hat, color = penalty_f, group = penalty_f))

p + geom_line() + 
  facet_grid(penalty_f ~ method) +
  scale_color_brewer("Ridge Penalty", palette = "Dark2") +
  labs(x = "Date",
       y = "Estimated Return") +
  scale_x_date(date_breaks = "5 year") +#, date_minor_breaks = "2 year") +
  guides(x =  guide_axis(angle = 45)) +
  theme_bw() +
  guides(col = "none")
ggsave("fig/return-lineplot-120.png", height = 6.5, width = 8)
