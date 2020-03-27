library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)

paf <- read_excel(here::here("data", "risk-factors.xlsx"), 
                         sheet = "PAF")

paf %<>%
  mutate(Exposure = case_when(
    Exposure == "Not breast feeding at any stage of life" ~
    "Not breast feeding \nat any stage of life",
    Exposure == "Prone sleeping position" ~ "Prone sleeping \nposition *",
    Exposure == "Smoking during pregnancy" ~ "Smoking during \npregnancy",
    Exposure == "Not sharing parental bedroom" ~ "Not sharing \nparental bedroom",
    TRUE ~ Exposure))

order <- c("Not breast feeding \nat any stage of life", "Prone sleeping \nposition *",
           "Not sharing \nparental bedroom", "Bed sharing",
           "Smoking during \npregnancy")

paf$Exposure <- factor(paf$Exposure, levels = order)


# PAF Bar Plot ------------------------------------------------------------

paf_plot <- ggplot(data = paf) +
  geom_bar(aes(x = PAF, y = Exposure, fill = Study), 
           alpha = 0.7, color = "black",
           stat = "identity",
           position = position_dodge2(padding = 0, preserve = "single")) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  xlab("Population Attributable Fraction (%)") +
  ylab("Exposure") +
  theme_pubr() +
  theme(panel.grid.major.x = element_line(color = "grey80",
                                          linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey80",
                                          linetype = "dashed"))

# PAF Table ---------------------------------------------------------------

paf_table <- paf %>%
  select(-c(pc:`Upper CI`)) %>%
  pivot_wider(names_from = Study, values_from = "PAF") %>%
  select("Exposure", "Mitchell 1992", "Mitchell 2017") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  replace(is.na(.), "-") %>%
  arrange(desc(Exposure))


# OR Table ----------------------------------------------------------------

table_df <- paf %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(OR = paste0(
    sprintf("%.2f", OR), 
    " (", 
    sprintf("%.2f",`Lower CI`), 
    ", ", 
    sprintf("%.2f",`Upper CI`), 
    ")"))

or_table <- table_df %>%
  select(-c(pc, `Lower CI`:PAF)) %>%
  pivot_wider(names_from = Study, values_from = "OR") %>%
  replace(is.na(.), "-") %>%
  arrange(desc(Exposure))

paf_table_p <- ggtexttable(paf_table, rows = NULL, 
            theme = ttheme("default"))

or_table_p <- ggtexttable(or_table, rows = NULL, 
                           theme = ttheme("default"))

paf_comb_p <- ggarrange(paf_plot, 
          ggarrange(paf_table_p, or_table_p, 
                    nrow = 2,
                    labels = c("Population Attributable Fraction (%)",
                               "Odds Ratio (Univariate)"),
                    hjust = c(-0.17, -0.6),
                    vjust = c(1, 1)),
          ncol = 2)

ggsave(filename = "paf-plot.png", plot = paf_comb_p, path = here::here("out"),
       width = 23, height = 15, units = "cm")
