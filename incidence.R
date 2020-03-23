library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)
library(ggsci)


df <- read_excel(here::here("data", "incidence.xlsx"))

count_df <- select(df, -`Live births`)

rate_df <- df %>%
  mutate(`SIDS rate` = `SIDS deaths` * 1000 / `Live births`,
         `SUDI rate` = `SUDI deaths` * 1000 / `Live births`) %>%
  rowwise %>%
  mutate(
    `SUDI lower CI` = poisson.test(`SUDI deaths`)$conf.int[1]*1000/`Live births`,
    `SUDI upper CI` = poisson.test(`SUDI deaths`)$conf.int[2]*1000/`Live births`) 

rate_df %<>%
  select(-c("Live births":"SUDI deaths")) %>%
  mutate_if(is.numeric, round, digits = 2)

df %<>%
  pivot_longer(names_to = "Type", cols = 3:4, values_to = "Number")

df %<>%
  mutate(Type = ifelse(Type == "SIDS deaths", "SIDS", "SUDI"),
           Rate = Number * 1000 / `Live births`) %>%
  rowwise %>%
  mutate(Rate_lower_CI = ifelse(
    Type == "SUDI", poisson.test(Number)$conf.int[1]*1000/`Live births`, NA),
         Rate_upper_CI = ifelse(
           Type == "SUDI", poisson.test(Number)$conf.int[2]*1000/`Live births`, NA))


# Counts ------------------------------------------------------------------

count_plot <- ggplot(data = df) +
  geom_bar(aes(x = Year, y = Number, fill = Type),
           alpha = 0.7, color = "black",
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(color = "grey80",
                                          linetype = "dashed"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

incidence_plot <- ggplot(data = df) +
  geom_line(aes(x = Year, y = Rate, color = Type), size = 1.2) +
  geom_errorbar(aes(x = Year, ymin = Rate_lower_CI, ymax = Rate_upper_CI),
                size = .5, width = 0.2, color = "gray50") +
  geom_point(aes(x = Year, y = Rate, color = Type, fill = Type), 
             shape = 21, color = "black", size = 2.5) +
  scale_x_continuous(breaks = seq(2006, 2015, 1), labels = seq(2006, 2015, 1)) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(color = "grey80",
                                          linetype = "longdash"),
        legend.position = "none")

count_df_p <- ggtexttable(count_df, rows = NULL, 
                        theme = ttheme("default"))

rate_df_p <- ggtexttable(select(rate_df, -c("SUDI lower CI", "SUDI upper CI")), rows = NULL, 
                          theme = ttheme("default"))
  
ggarrange(count_plot, count_df_p, incidence_plot, rate_df_p,
          ncol = 2, nrow = 2,
          align = "hv",
          labels = c("A", "", "B"))



