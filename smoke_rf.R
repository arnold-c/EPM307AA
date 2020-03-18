library(tidyverse)
library(readxl)
library(magrittr)
library(stats)
library(ggpubr)
library(ggsci)


# Smoking -----------------------------------------------------------------

smoking_rf <- read_excel(here::here("data", "smoking_rf.xlsx"))

smoking_rf %<>%
  group_by(Exposure) %>%
  arrange(OR) %>%
  mutate(order = row_number())

smoking_rf$`Lower CI` <- as.numeric(smoking_rf$`Lower CI`)
smoking_rf$`Upper CI` <- as.numeric(smoking_rf$`Upper CI`)

ggplot(data = smoking_rf) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  scale_fill_jco() +
  scale_color_jco() +
  coord_trans(x = "log10") +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = smoking_rf$order, labels = ) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  facet_wrap("Exposure", ncol = 1, scales = "free_y") +
  ylab("") +
  xlab("Odds ratio (log base 10 scale)") +
  theme_bw()
