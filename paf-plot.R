library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)
library(ggsci)

paf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "PAF")

paf %<>%
  mutate(Exposure = case_when(
    Exposure == "Not breast feeding at any stage of life" ~
    "Not breast feeding \nat any stage of life",
    Exposure == "Prone sleeping position" ~ "Prone sleeping \nposition",
    Exposure == "Smoking during pregnancy" ~ "Smoking during \npregnancy",
    TRUE ~ Exposure))

order <- c("Not breast feeding \nat any stage of life", "Prone sleeping \nposition",
           "Bed sharing", "Smoking during \npregnancy")

paf$Exposure <- factor(paf$Exposure, levels = order)

paf_plot <- ggplot(data = paf) +
  geom_bar(aes(x = PAF, y = Exposure, fill = Study), 
           alpha = 0.7, color = "black",
           stat = "identity", position = "dodge") +
  scale_fill_jco() +
  scale_color_jco() +
  ylab("Population Attributable Fraction (%)") +
  theme_pubr() +
  theme(panel.grid.major.x = element_line(color = "grey80",
                                          linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey80",
                                          linetype = "dashed"))

paf_table <- paf %>%
  select(-c("OR", "pc")) %>%
  pivot_wider(names_from = Study, values_from = "PAF") %>%
  select("Exposure", "Mitchell 1992", "Mitchell 2017") %>%
  mutate_if(is.numeric, round, digits = 2)

or_table <- paf %>%
  select(-c("PAF", "pc")) %>%
  pivot_wider(names_from = Study, values_from = "OR") %>%
  select("Exposure", "Mitchell 1992", "Mitchell 2017") %>%
  mutate_if(is.numeric, round, digits = 2)


paf_table_p <- ggtexttable(paf_table, rows = NULL, 
            theme = ttheme("default"))

or_table_p <- ggtexttable(or_table, rows = NULL, 
                           theme = ttheme("default"))

ggarrange(paf_plot, 
          ggarrange(paf_table_p, or_table_p, 
                    nrow = 2,
                    labels = c("Population Attributable Fraction (%)",
                               "Odds Ratio (Univariate)"),
                    hjust = c(-0.3, -0.8)),
          ncol = 2)

