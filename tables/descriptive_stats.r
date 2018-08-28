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
    `GDP per capita (thousands)`         = meanandsd(gdp_percapita_ppp/1e3),
    `Tobacco Prod. (thousands tons)`     = meanandsd(tobacco_prod/1e3),
    `Women's rights`                     = meanandsd(womens_rights),
    `Women's Labor`                      = meanandsd(labor),
    `Population (millions)`              = meanandsd(population/1e6),
    # Political shifts
    `% Political Shift`                  = pcents(pol_shift),
    `% Political Shift (missings)`       = pcents(!complete.cases(pol_shift)),
    `# of Bloomberg projects`            = meanandsd(bloomberg_count),
    `USD Bloomberg projects (thousands)`             = meanandsd(bloomberg_amount/1e3),
    `# of Bloomberg FCTC projects`       = meanandsd(bloomberg_fctc_count),
    `USD Bloomberg FCTC projects (thousands)`        = meanandsd(bloomberg_fctc_amount/1e3),
    `Government Ownership`               = meanandsd(govtown),
    # Economic indicators
    `Control of Corruption`              = meanandsd(ctrl_corrup),
    `Rule of Law`                        = meanandsd(rule_of_law),
    `Health Exp. per capita (ppp)`       = meanandsd(health_exp),
    # Tobacco
    `Years since signing (in 2012)`      = meanandsd(`Years since Sign.`),
    `Years since ratification (in 2012)` = meanandsd(`Years since Ratif.`),
    `Smoking prevalence Female`          = meanandsd(smoke_female),
    `Smoking prevalence Male`            = meanandsd(smoke_male),
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

# Number of years since ratification -------------------------------------------
dat <- subset(model_data, year == 2012)
apply(dat[, c("Years since Sign.", "Years since Ratif.")], 2, mean, na.rm=TRUE)
apply(dat[, c("Years since Sign.", "Years since Ratif.")], 2, sd, na.rm=TRUE)


# Part 2: Ratifying countries --------------------------------------------------

dat <- read.csv("data/treaty_dates.csv", na="<NA>")
dat <- subset(dat, select=c(entry, country_name, signature, ratification))
colnames(dat) <- c("Code", "Name", "Signature date", "Ratification Date")

dat <- xtable::xtable(dat)
xtable::caption(dat) <- paste(
  "Signature and Ratification dates. The statistical models only include",
  "those countries that ratified the treaty. Notably, US is not included"
)

print(dat, file="fig/signature_ratification_dates.tex", booktabs=TRUE,
      tabular.environment="longtable")

# Part 3: Network attributes ---------------------------------------------------


rm(list=ls())
model_data <- read.csv("data/model_data.csv", na="<NA>")
load("data/adjmats.rda")
load("data/adjmat_border.rda")
load("data/adjmat_mindist.rda")
load("data/adjmat_centroid_dist.rda")

# Statistics: 
# 1. Density
# 2. Size
# 3. Valued
# 4. Degree distribution

implemented <- group_by(model_data, entry) %>%
  summarize(one_or_more=
              (max(sum_art05 + sum_art06 + sum_art08 + 
                     sum_art11 + sum_art13) > 0L) + 0L
  )

graph_stats <- function(g, valued=TRUE) {
  
  if (is.list(g)) g <- g[[1]]
  
  # Checking whether directed or not
  gm         <- as.matrix(g)
  undirected <- all((gm - t(gm)) == 0)
  
  data.frame(
    Description     = "",
    Homophily       = "",
    Density         = nlinks(g)/(nnodes(g)-1)/nnodes(g),
    Size            = nnodes(g),
    #`Avg Degree`    = mean(dgr(g, cmode = "degree")),
    `Modal Degree`  = quantile(dgr(g, cmode = "degree"), probs = .5),
    Valued          = ifelse(valued, "Yes", "No"),
    Uirected        = ifelse(undirected, "Yes", "No"),
    `% Implemented` = with(implemented,sum(one_or_more[entry %in% nodes(g)]))/nnodes(g),
    #`# communities` = 0,
    check.names = FALSE
  )
}

# Preprocessing networks gl_posts and referrals
g0 <- adjmat_gl_posts[as.character(2008:2010)] # 
adjmat_gl_posts <- g0[[1]]
for (g in g0[-1])
  adjmat_gl_posts <- adjmat_gl_posts + g

g0 <- adjmat_referrals # 
adjmat_referrals <- g0[[1]]
for (g in g0[-1])
  adjmat_referrals <- adjmat_referrals + g

# Networks to analyzie
nets <- list(adjmat_general_trade, 
             adjmat_tobacco_trade,
             adjmat_mindist, 
             # adjmat_border,
             adjmat_centroid_dist, 
             adjmat_gl_posts,
             adjmat_referrals)

valued      <- c(
  "Yes",
  "Yes",
  "Yes",
  # "Yes",
  "Yes",
  "No",
  "No")

fancy_names <- c(
  "General Trade",
  "Tobacco Trade",
  "Minimum Distance",
  # "Shared borders (Wiki)",
  "Centroid Distance",
  "GL co-subscription",
  "GL Referrals "
)

homophilous <- c(
  "No",
  "No",
  "No",
  # "No",
  "No",
  "Yes",
  "Yes"
)

stats_mat <- NULL
for (g in nets) {
  stats_mat <- rbind(stats_mat, graph_stats(g))
}

stats_mat <- data.frame(stats_mat, check.names = FALSE)
stats_mat$`Homophily` <- homophilous
stats_mat$Description <- fancy_names
stats_mat$Valued      <- valued

stats_mat

stats_mat <- xtable::xtable(stats_mat)
xtable::caption(stats_mat) <- paste(
  "Networks descriptive statistics. The last column shows what portion of",
  "the nodes in the network implemented at least one part of the treaty,",
  "whereas a full article or part of it."
  )
  
  
print(stats_mat, include.rownames=FALSE, booktabs=TRUE,
      file="fig/networks_descriptive_stats.tex")
