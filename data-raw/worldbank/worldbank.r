library(wbstats)
indicators <- wbindicators()

"SH.PRV.SMOK.FE" # Smoking Prevalence % female
"SH.PRV.SMOK.MA" # Smoking Prevalence % male
"NV.MNF.FBTO.ZS.UN" # Food, beverages and tobacco (% of value added in manufacturing),
# Value added in manufacturing is the sum of gross output less the value of intermediate inputs used in production for industries classified in ISIC major division D. Food, beverages, and tobacco correspond to ISIC divisions 15 and 16.
# United Nations Industrial Development Organization, International Yearbook of Industrial Statistics.
# World Development Indicators

# H.XPD.PCAP.PP.KD
# Health expenditure per capita, PPP (constant 2011 international $)
# NA
# Total health expenditure is the sum of public and private health expenditures as a ratio of total population. It covers the provision of health services (preventive and curative), family planning activities, nutrition activities, and emergency aid designated for health but does not include provision of water and sanitation. Data are in international dollars converted using 2011 purchasing power parity (PPP) rates.
# World Health Organization Global Health Expenditure database (see http://apps.who.int/nha/database for the most recent updates).
# 57
# WDI Database Archives

smoke_female <- wb(indicator = "SH.PRV.SMOK.FE")
smoke_male   <- wb(indicator = "SH.PRV.SMOK.FE")
tobacco_prod <- wb(indicator = "NV.MNF.FBTO.ZS.UN")
population   <- wb(indicator = "SP.POP.TOTL")
