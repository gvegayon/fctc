library(wbstats)
indicators <- wbindicators()

"SH.PRV.SMOK.FE" # Smoking Prevalence % female
"SH.PRV.SMOK.MA" # Smoking Prevalence % male
"NV.MNF.FBTO.ZS.UN" # Food, beverages and tobacco (% of value added in manufacturing),
# Value added in manufacturing is the sum of gross output less the value of intermediate inputs used in production for industries classified in ISIC major division D. Food, beverages, and tobacco correspond to ISIC divisions 15 and 16.
# United Nations Industrial Development Organization, International Yearbook of Industrial Statistics.
# World Development Indicators

# SH.XPD.PCAP.GX Government health expenditure per capita (current
# CC.EST Control of Corruption (estimate) 
# RL.EST Rule of Law (estimate) 
# UPP.COM.POL.XQ Combined polity score
# GV.VOIC.AC.ES Voice and Accountability (estimate)
# RQ.EST Regulatory Quality: Estimate
# SG.GEN.MNST.ZS Proportion of women in ministerial level positions (%)



smoke_female <- wb(indicator = "SH.PRV.SMOK.FE")
smoke_male   <- wb(indicator = "SH.PRV.SMOK.FE")
tobacco_prod <- wb(indicator = "NV.MNF.FBTO.ZS.UN")
population   <- wb(indicator = "SP.POP.TOTL")

rule_of_law <- wb(indicator = "RL.EST")
control_of_corruption <- wb(indicator = "CC.EST")
polity <- wb(indicator="UPP.COM.POL.XQ")
