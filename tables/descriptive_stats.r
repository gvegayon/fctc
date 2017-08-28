# This file creates descriptive stats
rm(list=ls())
options(stringsAsFactors = FALSE)

library(xtable)
library(netdiffuseR)
library(magrittr)
library(dplyr)

# Part 1: Party attributes -----------------------------------------------------

model_data <- read.csv("data/model_data_unscaled.csv", na="<NA>", check.names = FALSE)

# Summarize computing mean and sd
meanandsd <- function(x) {
  
  thousands <- function(y) {
    if ( (log(y, 10) %/% 1) >= 3) {
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

ans <- group_by(model_data, Year = year) %>%
  summarise(
    `Number of Countries`                = n(),
    # Proportion of Regions
    Americas                             = pcents(Americas),
    `Eastern Mediterranean`              = pcents(`Eastern Mediterranean`),
    Europe                               = pcents(Europe),
    Africa                               = pcents(Africa),
    `Western Pacific`                    = pcents(`Western Pacific`),
    `Southeast Asia`                     = pcents(`South-East Asia`),
    # Economic indicators
    `Democracy`                          = meanandsd(democracy),
    `GDP per capita`                     = meanandsd(GDP),
    `Population (millions)`              = meanandsd(population/1e6),
    `Womens Rights`                      = meanandsd(womens_rights),
    # Tobacco
    `Years since signing (in 2012)`      = meanandsd(`Years since Sign.`),
    `Years since ratification (in 2012)` = meanandsd(`Years since Ratif.`),
    `Tobacco Production (tons)`          = meanandsd(tobac_prod),
    `Smoking prevalence Female`          = meanandsd(perc_female_smoke),
    `Smoking prevalence Male`            = meanandsd(perc_male_smoke),
    # Political shifts
    `% Political Shift`                  = pcents(pol_shift),
    `% Political Shift (missings)`       = pcents(!complete.cases(pol_shift)),
    # Bloomberg Initiative
    `# of Bloomberg projects`            = meanandsd(bloomberg_count),
    `USD Bloomberg projects`             = meanandsd(bloomberg_amount),
    `# of Bloomberg FCTC projects`       = meanandsd(bloomberg_fctc_count),
    `USD Bloomberg FCTC projects`        = meanandsd(bloomberg_fctc_amount)
  )

# Exporting in TAB
write.table(t(ans), file="fig/diffusion_variables.tab", sep="\t", col.names = FALSE)

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