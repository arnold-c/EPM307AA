library(tidyverse)
library(readxl)
library(magrittr)
library(ggpubr)

paf <- read_excel(here::here("data", "risk-factors.xlsx"),
                  sheet = "PAF")

aor <- read_excel(here::here("data", "risk-factors.xlsx"),
                  sheet = "aOR")

paf %<>%
  mutate(
    Exposure = case_when(
      Exposure == "Not breast feeding at any stage of life" ~
        "Not breast feeding \nat any stage of life",
      Exposure == "Prone sleeping position" ~ "Prone sleeping \nposition *",
      Exposure == "Smoking during pregnancy" ~ "Smoking during \npregnancy",
      Exposure == "Not sharing parental bedroom" ~ "Not sharing \nparental bedroom",
      TRUE ~ Exposure
    )
  )

paf_order <-
  c(
    "Not breast feeding \nat any stage of life",
    "Prone sleeping \nposition *",
    "Not sharing \nparental bedroom",
    "Bed sharing",
    "Smoking during \npregnancy"
  )

paf$Exposure <- factor(paf$Exposure, levels = paf_order)

aor %<>%
  mutate(
    Exposure = case_when(
      Exposure == "Not breast feeding at any stage of life" ~
        "Not breast feeding \nat any stage of life",
      Exposure == "Not exclusively breast feeding on discharge" ~
        "Not exclusively breast \nfeeding on discharge",
      Exposure == "Prone sleeping position relative to other" ~ 
        "Prone sleeping position \nrelative to other",
      Exposure == "Prone sleeping position relative to back" ~ 
        "Prone sleeping position \nrelative to back",
      Exposure == "Smoking during pregnancy" ~ 
        "Smoking during \npregnancy",
      Exposure == "Smoking during final 2w of pregnancy" ~ 
        "Smoking during final \n2w of pregnancy",
      Exposure == "Not sharing parental bedroom" ~ 
        "Not sharing \nparental bedroom",
      TRUE ~ Exposure
    )
  )

aor_order <-
  c(
    "Not breast feeding \nat any stage of life",
    "Not exclusively breast \nfeeding on discharge",
    "Prone sleeping position \nrelative to back",
    "Prone sleeping position \nrelative to other",
    "Not sharing \nparental bedroom",
    "Bed sharing",
    "Smoking during final \n2w of pregnancy",
    "Smoking during \npregnancy"
  )

aor$Exposure <- factor(aor$Exposure, levels = aor_order)


# PAF Bar Plot ------------------------------------------------------------

paf_plot <- ggplot(data = paf) +
  geom_bar(
    aes(x = PAF, y = Exposure, fill = Study),
    alpha = 0.7,
    color = "black",
    stat = "identity",
    position = position_dodge2(padding = 0, preserve = "single")
  ) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  scale_color_manual(values = c("#0073C2FF", "#868686FF")) +
  xlab("Population Attributable Fraction (%)") +
  ylab("Exposure") +
  theme_pubr() +
  theme(
    panel.grid.major.x = element_line(color = "grey80",
                                      linetype = "dashed"),
    panel.grid.minor.x = element_line(color = "grey80",
                                      linetype = "dashed")
  )

ggsave(
  filename = "paf-plot.png",
  plot = paf_plot,
  path = here::here("out"),
  width = 15,
  height = 15,
  units = "cm"
)

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
    sprintf("%.2f", `Lower CI`),
    ", ",
    sprintf("%.2f", `Upper CI`),
    ")"
  ))

or_table <- table_df %>%
  select(-c(pc, `Lower CI`:PAF)) %>%
  pivot_wider(names_from = Study, values_from = "OR") %>%
  replace(is.na(.), "-") %>%
  arrange(desc(Exposure))


# PAF and OR Plot ---------------------------------------------------------

paf_table_p <- ggtexttable(paf_table, rows = NULL,
                           theme = ttheme("default"))

or_table_p <- ggtexttable(or_table, rows = NULL,
                          theme = ttheme("default"))

paf_comb_p <- ggarrange(
  paf_plot,
  ggarrange(
    paf_table_p,
    or_table_p,
    nrow = 2,
    labels = c(
      "Population Attributable Fraction (%)",
      "Odds Ratio (Univariate)"
    ),
    hjust = c(-0.17,-0.6),
    vjust = c(1, 1)
  ),
  ncol = 2
)

ggsave(
  filename = "paf-or-plot.png",
  plot = paf_comb_p,
  path = here::here("out"),
  width = 23,
  height = 15,
  units = "cm"
)


# aOR Table ---------------------------------------------------------------

aor_summary <- aor %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(aOR = paste0(
    sprintf("%.2f", aOR),
    " (",
    sprintf("%.2f", `Lower CI`),
    ", ",
    sprintf("%.2f", `Upper CI`),
    ")"
  ))

aor_table <- aor_summary %>%
  select(-c(`Lower CI`, `Upper CI`)) %>%
  pivot_wider(names_from = Study, values_from = "aOR") %>%
  replace(is.na(.), "-") %>%
  arrange(desc(Exposure))

aor_table_p <- ggtexttable(aor_table, rows = NULL,
                           theme = ttheme("default"))

ggsave(
  filename = "aOR-table.png",
  plot = aor_table_p,
  path = here::here("out"),
  width = 12,
  height = 10.5,
  units = "cm"
)



# PAF and aOR Plot --------------------------------------------------------

aor_comb_p <- ggarrange(
  paf_plot,
  ggarrange(
    aor_table_p,
    or_table_p,
    nrow = 2,
    labels = c("Odds Ratio (Multivariate)",
               "Odds Ratio (Univariate)"),
    hjust = c(-0.69,-0.75),
    vjust = c(1.3, 1)
  ),
  ncol = 2
)

ggsave(
  filename = "paf-aor-plot.png",
  plot = aor_comb_p,
  path = here::here("out"),
  width = 27,
  height = 15,
  units = "cm"
)
