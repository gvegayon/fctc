# This file creates descriptive stats
rm(list=ls())
options(stringsAsFactors = FALSE)

library(xtable)
library(netdiffuseR)

# ------------------------------------------------------------------------------
#
# Part 1: Party attributes
#
# ------------------------------------------------------------------------------
model_data <- read.csv("data/model_data.csv", na="<NA>")

# On Smoking
summary(model_data[,c("tobac_prod","perc_female_smoke", "perc_male_smoke")])

# On the treaty and the bloomberg iniciaitive
l <- c("sum_art05","sum_art06", "sum_art08", "sum_art11", "sum_art13")
dat <- reshape(model_data, varying = l, direction = "long", v.names="sum_art",
               times = c(5,6,8,11,13), timevar="Article")
dat <- subset(dat, select=c(year, sum_art, Article))
dat$sum_art <- ifelse(dat$sum_art >= 5, ">=5", sprintf("% 2d",dat$sum_art))
dat <- with(dat, table(Article, sum_art, year))
dat <- prop.table(dat,c(1,3))

# Preparing for xtable
dat <- list(dat[,,1], dat[,,2])
dat <- lapply(dat, function(x) {
  rownames(x) <- paste("Art.", rownames(x))
  x
})
attr(dat, "subheadings") <- c(2010, 2012)

# Storing data as CSV
write.table(
  rbind(cbind(year=2010,dat[[1]]), cbind(year=2012,dat[[2]])),
  file = "fig/dist_of_items_implemented.csv")


cap <- paste(
  "Distribution of number of items implemented per article.",
  "Row-sums are equal to 1."
)

dat <- xtable::xtableList(dat, caption=cap)

xtable::print.xtableList(dat, booktabs = TRUE, file="fig/implementation_dist.tex")
  
# Ratifying countries ----------------------------------------------------------
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

# ------------------------------------------------------------------------------
#
# Part 2: Network attributes
#
# ------------------------------------------------------------------------------

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

graph_stats <- function(g, valued=TRUE) {
  if (is.list(g)) g <- g[[1]]
  data.frame(
    Description     = "",
    Homophily       = "",
    Density         = nlinks(g)/nnodes(g)/nnodes(g),
    Size            = nnodes(g),
    `Avg Degree`    = mean(dgr(g)),
    `Mode Degree`   = quantile(dgr(g), probs = .5),
    Valued          = ifelse(valued, 1L, 0L),
    #`# communities` = 0,
    check.names = FALSE
  )
}

# Networks to analyzie
nets <- list(adjmat_general_trade, adjmat_mindist, adjmat_border,
             adjmat_centroid_dist, adjmat_fctc_cop_coparticipation_twomode,
             adjmat_fctc_inb_coparticipation_twomode)

valued      <- c("Yes", "Yes", "Yes", "Yes", "No", "No")

fancy_names <- c(
  "Trade",
  "Minimum Distance",
  "Shared borders (Wiki)",
  "Centroid Distance",
  "COP co-participation",
  "INB co-participation"
)

homophilous <- c(
  "No (*)",
  "No",
  "No",
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
  "Networks descriptive stats. In the case of the Trade network,",
  "while trading is homophilous indeed, we have no reason to belive that",
  "the implementation of the FCTC can change it significantly, hence",
  "we consider it to be exogenous to our analysis."
  )
  
  
print(stats_mat, include.rownames=FALSE, booktabs=TRUE,
      file="fig/networks_descriptive_stats.tex")
