library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)
library(ggsci)

# Smoking -----------------------------------------------------------------

smoking_rf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "smoking")

smoking_rf %<>%
  group_by(Exposure) %>%
  arrange(OR) %>%
  mutate(order = row_number())
  
smoking_rf$`Lower CI` <- as.numeric(smoking_rf$`Lower CI`)
smoking_rf$`Upper CI` <- as.numeric(smoking_rf$`Upper CI`)

smoke_preg <- subset(smoking_rf, Exposure == "Smoking during pregnancy")
smoke_2w <- subset(smoking_rf, Exposure != "Smoking during pregnancy")

smoke_preg_plot <- 
  ggplot(data = smoke_preg) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = smoke_preg$order, labels = smoke_preg$`Analysis Method`) +
  scale_x_log10(breaks = seq(0,12,1)) +
  ylab("") +
  xlab("") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

smoke_2w_plot <- 
  ggplot(data = smoke_2w) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = smoke_2w$order, labels = smoke_2w$`Analysis Method`) +
  scale_x_log10(breaks = seq(0,12,1), limits = c(1, 12)) +
  ylab("") +
  xlab("Odds ratio (log base 10 scale)") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggarrange(smoke_preg_plot, smoke_2w_plot, ncol = 1, 
          common.legend = TRUE, align = "hv",
          labels = c("A", "B"))


# Sleeping ----------------------------------------------------------------

sleeping_rf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "sleeping")

sleeping_rf %<>%
  group_by(`Non-exposure`) %>%
  arrange(OR) %>%
  mutate(order = row_number(),
         labels = paste(Exposure, " - ", `Analysis Method`))

sleeping_rf$`Lower CI` <- as.numeric(sleeping_rf$`Lower CI`)
sleeping_rf$`Upper CI` <- as.numeric(sleeping_rf$`Upper CI`)

sleep_back <- subset(sleeping_rf, `Non-exposure` == "Back")
sleep_any <- subset(sleeping_rf, `Non-exposure` != "Back")

sleep_back_plot <- 
  ggplot(data = sleep_back) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = sleep_back$order, labels = sleep_back$labels) +
  scale_x_log10(breaks = seq(0,14,1)) +
  ylab("") +
  xlab("") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

sleep_any_plot <- 
  ggplot(data = sleep_any) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = sleep_any$order, labels = sleep_any$labels) +
  scale_x_log10(breaks = seq(0,14,1), limits = c(1, 14)) +
  ylab("") +
  xlab("Odds ratio (log base 10 scale)") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggarrange(sleep_back_plot, sleep_any_plot, ncol = 1, 
          common.legend = TRUE, align = "hv",
          labels = c("A", "B"))


# Breast Feeding ----------------------------------------------------------

bf_rf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "breastfeeding")

bf_rf %<>%
  group_by(Exposure) %>%
  arrange(OR) %>%
  mutate(order = row_number())

bf_rf$`Lower CI` <- as.numeric(bf_rf$`Lower CI`)
bf_rf$`Upper CI` <- as.numeric(bf_rf$`Upper CI`)

bf_any <- subset(bf_rf, Exposure == "No breast feeding")
bf_exclusive <- subset(bf_rf, Exposure != "No breast feeding")

bf_any_plot <- 
  ggplot(data = bf_any) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = bf_any$order, labels = bf_any$`Analysis Method`) +
  scale_x_log10(breaks = seq(0,7,1)) +
  ylab("") +
  xlab("") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

bf_exclusive_plot <- 
  ggplot(data = bf_exclusive) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = bf_exclusive$order, labels = bf_exclusive$`Analysis Method`) +
  scale_x_log10(breaks = seq(0,7,1), limits = c(1, 7)) +
  ylab("") +
  xlab("Odds ratio (log base 10 scale)") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggarrange(bf_any_plot, bf_exclusive_plot, ncol = 1, 
          common.legend = TRUE, align = "hv",
          labels = c("A", "B"))

# Bed Sharing -------------------------------------------------------------

bed_rf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "bed-sharing")

bed_rf %<>%
  group_by(Exposure) %>%
  arrange(OR) %>%
  mutate(order = row_number())

bed_rf$`Lower CI` <- as.numeric(bed_rf$`Lower CI`)
bed_rf$`Upper CI` <- as.numeric(bed_rf$`Upper CI`)

bed_sharing_plot <- 
  ggplot(data = bed_rf) +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`, y = order),
                 size = .5, height = .2, color = "gray50") +
  geom_point(aes(x = OR, y = order, color = Study)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  scale_y_continuous(breaks = bed_rf$order, labels = bed_rf$`Analysis Method`) +
  scale_x_log10(breaks = seq(0,11,1)) +
  ylab("") +
  xlab("Odds ratio (log base 10 scale)") +
  scale_fill_jco() +
  scale_color_jco() +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggarrange(bed_sharing_plot, ncol = 1, 
          common.legend = TRUE, align = "hv",
          labels = c("A"))

