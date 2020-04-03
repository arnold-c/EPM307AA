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

# Incidence Plot ----------------------------------------------------------

SIDS_eth_inc <- ggplot(data = df,
                   mapping = aes(x = Year, y = `SIDS rate`,
                                 color = `Ethnic Group`)) +
  geom_line(aes(group = `Ethnic Group`),
            size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = alpha(
    c("#0073C2FF", "#EFC000FF", "#CD534CFF",
      "#868686FF", "black"),
    c(0.3, 0.3, 0.3, 0.3, 1)
  )) +
  ylab("Rate (per 1000 live births)") +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey80",
                                      linetype = "longdash"),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

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
  )) +
  ylab("Rate (per 1000 live births)") +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey80",
                                      linetype = "longdash"),
    legend.text = element_text(size = 12)
  )

comb_eth_inc_plot <- ggarrange(
  SIDS_eth_inc,
  SUDI_eth_inc,
  ncol = 1,
  align = "h",
  common.legend = TRUE,
  labels = c("A", "B")
)

ggsave(
  filename = "eth_inc.png",
  plot = comb_eth_inc_plot,
  path = here::here("out"),
  width = 16,
  height = 18,
  units = "cm"
)


# SUDI Incidence and SIDS and SUDI Count ----------------------------------

count_df <- df %>%
  filter(`Ethnic Group` == "Total") %>%
  select(Year, `SUDI deaths`, `SIDS deaths`) %>%
  pivot_longer(
    cols = c(`SUDI deaths`, `SIDS deaths`),
    names_to = "Type",
    values_to = "Number"
  )

count_plot <- ggplot(data = count_df) +
  geom_bar(
    aes(x = Year, y = Number, fill = Type),
    alpha = 0.7,
    color = "black",
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_pubr() +
  theme(
    panel.grid.major.y = element_line(color = "grey80",
                                      linetype = "dashed")
  )

comb_count_plot <- ggarrange(
  count_plot,
  SUDI_eth_inc,
  ncol = 1,
  align = "h",
  labels = c("A", "B")
)

ggsave(
  filename = "count_inc.png",
  plot = comb_count_plot,
  path = here::here("out"),
  width = 17,
  height = 18,
  units = "cm"
)

# Incidence and SUDI ethnic breakdown -------------------------------------

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
  width = 17,
  height = 18,
  units = "cm"
)
