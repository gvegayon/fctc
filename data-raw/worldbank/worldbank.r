library(wbstats)

library(dplyr)
library(magrittr)

indicators <- wbindicators()

"SH.PRV.SMOK.FE" # Smoking Prevalence % female
"SH.PRV.SMOK.MA" # Smoking Prevalence % male
"NV.MNF.FBTO.ZS.UN" # Food, beverages and tobacco (% of value added in manufacturing),
# Value added in manufacturing is the sum of gross output less the value of intermediate inputs used in production for industries classified in ISIC major division D. Food, beverages, and tobacco correspond to ISIC divisions 15 and 16.
# United Nations Industrial Development Organization, International Yearbook of Industrial Statistics.
# World Development Indicators

# SH.XPD.GHED.PP.CD Domestic general government health expenditure per capita, PPP (current international $)
# CC.EST Control of Corruption (estimate) 
# RL.EST Rule of Law (estimate) 
# GV.VOIC.AC.ES Voice and Accountability (estimate)
# RQ.EST Regulatory Quality: Estimate
# SG.GEN.MNST.ZS Proportion of women in ministerial level positions (%)
# NY.GDP.PCAP.PP.KD GDP per capita, PPP (constant 2011 international $)


# The variable of health expenditure is not 
cachedlist <- wbcache(lang = "en") 

health_exp   <- wb(indicator = "SH.XPD.GHED.PP.CD", cache=cachedlist) %>% as_tibble
smoke_female <- wb(indicator = "SH.PRV.SMOK.FE") %>% as_tibble
smoke_male   <- wb(indicator = "SH.PRV.SMOK.MA") %>% as_tibble

population   <- wb(indicator = "SP.POP.TOTL") %>% as_tibble
birth_death  <- wb(indicator = "SP.DYN.CBRT.IN", cache=cachedlist) %>% as_tibble # Birth rate, crude (per 1,000 people)

rule_of_law  <- wb(indicator = "RL.EST") %>% as_tibble
ctrl_corrup  <- wb(indicator = "CC.EST") %>% as_tibble
gdp_percapita_ppp <- wb(indicator = "NY.GDP.PCAP.PP.KD") %>% as_tibble
womens_rights <- wb(indicator="SG.GEN.MNST.ZS") %>% as_tibble
labor         <- wb(indicator="SL.TLF.TOTL.FE.ZS") %>% as_tibble # Labor force, female (% of total labor force)

# Merging all data
datasets <- ls()[sapply(mget(ls()), inherits, "tbl_df")]
worldbank <- get(datasets[1]) %>%
  select(iso3c, iso2c, value, date) %>%
  set_colnames(c("iso3c", "iso2c", datasets[1], "date"))
  

for (d in datasets[-1]) {
  
  # Getting subset
  subd <- get(d) %>%
    select(iso3c, iso2c, value, date) %>%
    set_colnames(c("iso3c", "iso2c", d, "date"))
  
  # Joining
  worldbank <- left_join(worldbank, subd)
  
}

readr::write_csv(worldbank, "data-raw/worldbank/worldbank.csv", na = "<NA>")
