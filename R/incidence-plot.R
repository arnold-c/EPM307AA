library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)
library(ggsci)

df <-
  read_excel(here::here("data", "incidence.xlsx"), sheet = "Ethnic Group")

df %<>%
  mutate(Type = case_when(
    Type == "SIDS" ~ "SIDS deaths",
    Type == "SUDI" ~ "SUDI deaths",
    TRUE ~ Type
  ))

df %<>%
  pivot_longer(
    cols = c(`2006`:`2015`),
    names_to = "Year",
    values_to = "Number"
  ) %>%
  pivot_wider(names_from = "Type", values_from = "Number") %>%
  mutate(
    `SIDS rate` = `SIDS deaths` * 1000 / `Live Births`,
    `SUDI rate` = `SUDI deaths` * 1000 / `Live Births`
  ) %>%
  mutate_if(is.numeric, round, digits = 2)

# Incidence bar plot ------------------------------------------------------

inc_df <- df %>%
  filter(`Ethnic Group` == "Total") %>%
  select(Year, `SUDI rate`, `SIDS rate`) %>%
  pivot_longer(
    cols = c(`SUDI rate`, `SIDS rate`),
    names_to = "Type",
    values_to = "Rate"
  )

inc_bar_plot <- ggplot(data = inc_df) +
  geom_bar(
    aes(x = Year, y = Rate, fill = Type),
    alpha = 0.7,
    color = "black",
    stat = "identity",
    position = "dodge"
  ) +
  ylim(0, 1.15) +
  ylab("Rate (per 1000 live births)") +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey80",
                                      linetype = "dashed"),
    legend.text = element_text(size = 12)
  )

# SUDI ethnic incidence ---------------------------------------------------

SUDI_eth_inc <- ggplot(data = df,
                       mapping = aes(x = Year, y = `SUDI rate`,
                                     color = `Ethnic Group`)) +
  geom_line(aes(group = `Ethnic Group`),
            size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = alpha(
    c("#0073C2FF", "#EFC000FF", "#CD534CFF",
      "#868686FF", "black"),
    c(0.3, 0.3, 0.3, 0.3, 1)
  ),
  guide = guide_legend(nrow = 2)) +
  ylab("Rate (per 1000 live births)") +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey80",
                                      linetype = "longdash"),
    legend.text = element_text(size = 12)
  )

# Combined Plot -----------------------------------------------------------


comb_inc_plot <- ggarrange(
  inc_bar_plot,
  SUDI_eth_inc,
  ncol = 1,
  align = "h",
  labels = c("A", "B")
)

ggsave(
  filename = "incidence.png",
  plot = comb_inc_plot,
  path = here::here("out"),
  width = 15,
  height = 15,
  units = "cm"
)
