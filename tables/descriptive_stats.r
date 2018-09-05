# This file creates descriptive stats
rm(list=ls())
options(stringsAsFactors = FALSE)

library(xtable)
library(netdiffuseR)
library(magrittr)
library(dplyr)

# Part 1: Party attributes -----------------------------------------------------

model_data <- readr::read_csv("data/model_data.csv", na="<NA>")

counter <- 1
# Summarize computing mean and sd
meanandsd <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (!length(x))
    return("")
  
  thousands <- function(y) {
    if (all(y > 0) && (log(y, 10) %/% 1) >= 3) {
      if ((y %% 1e3) < 100) sprintf("%d,0%.2f", y %/% 1e3, y %% 1e3)
      else sprintf("%d,%.2f", y %/% 1e3, y %% 1e3)
      
    } else sprintf("%.2f", y)
  }
  
  sprintf("%s (%s)",
          thousands(mean(x, na.rm=TRUE)),
          thousands(sd(x, na.rm = TRUE))
  )
}

# Summarize computing pcents
pcents <- function(x) {
  sprintf("%.1f%%", sum(x, na.rm=TRUE)/length(x)*100)
}

ans <- model_data %>% 
  # group_by(entry) %>%
  # mutate(
  #   bloomberg_count = sum(bloomberg_count)
  # ) %>%
  # select(entry, year, starts_with("bloomb")) %>% View
  # ungroup %>%
  group_by(Year = year) %>%
  summarise(
    `Number of Countries`                = n(),
    # Proportion of Regions
    `Eastern Mediterranean`              = pcents(`Eastern Mediterranean`),
    Americas                             = pcents(Americas),
    African                              = pcents(African),
    European                             = pcents(European),
    `Southeast Asia`                     = pcents(`South-East Asia`),
    `Western Pacific`                    = pcents(`Western Pacific`),
    `Democracy (POLITY)`                 = meanandsd(POLITY),
    `GDP per capita (thousands)`         = meanandsd(gdp_percapita_ppp/1e3),
    `Tobacco Prod. (thousands tons)`     = meanandsd(tobacco_prod/1e3),
    `Smoking prevalence Female`          = meanandsd(smoke_female),
    `Smoking prevalence Male`            = meanandsd(smoke_male),
    `Women's Labor`                      = meanandsd(labor),
    # `Women's rights`                     = meanandsd(womens_rights),
    # `Population (millions)`              = meanandsd(population/1e6),
    # Political shifts
    `% Political Shift`                  = pcents(pol_shift),
    `% Political Shift (missings)`       = pcents(!complete.cases(pol_shift)),
    `# of Bloomberg projects`            = meanandsd(bloomberg_count),
    `USD Bloomberg projects (thousands)`             = meanandsd(bloomberg_amount/1e3),
    `# of Bloomberg FCTC projects`       = meanandsd(bloomberg_fctc_count),
    `USD Bloomberg FCTC projects (thousands)`        = meanandsd(bloomberg_fctc_amount/1e3),
    `Government Ownership`               = meanandsd(govtown),
    # Economic indicators
    # `Control of Corruption`              = meanandsd(ctrl_corrup),
    # `Rule of Law`                        = meanandsd(rule_of_law),
    `Health Exp. per capita (ppp)`       = meanandsd(health_exp),
    # Tobacco
    # `Years since signing (in 2012)`      = meanandsd(`Years since Sign.`),
    `Years since ratification (in 2012)` = meanandsd(`Years since Ratif.`),
    # Bloomberg Initiative
    `# Items Implemented Art. 5`         = meanandsd(sum_art05),
    `# Items Implemented Art. 6`         = meanandsd(sum_art06),
    `# Items Implemented Art. 8`         = meanandsd(sum_art08),
    `# Items Implemented Art. 11`        = meanandsd(sum_art11),
    `# Items Implemented Art. 13`        = meanandsd(sum_art13),
    `# Items Implemented Art. 14`        = meanandsd(sum_art14)
  )

# Exporting in TAB
cbind(colnames(ans), t(ans)) %>%
  as_tibble %>%
  set_colnames(c("Variable", 2010, 2012, 2014, 2016)) %>%
  readr::write_csv("tables/descriptive_stats.csv")
