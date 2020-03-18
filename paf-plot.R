library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)
library(ggsci)

paf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "PAF")

order <- c("Smoking during pregnancy", "Bed sharing", 
           "Prone sleeping position", "Not breast feeding at any stage of life")

paf$Exposure <- factor(paf$Exposure, levels = order)

paf_plot <- ggplot(data = paf) +
  geom_bar(aes(x = Exposure, y = PAF, fill = Study), 
           alpha = 0.7, color = "black",
           stat = "identity", position = "dodge") +
  scale_fill_jco() +
  scale_color_jco() +
  ylab("Population Attributable Fractions (%)") +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(color = "grey80",
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
                               "Odds Ratio (Univariate)")),
          ncol = 2)

