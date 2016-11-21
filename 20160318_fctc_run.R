
rm(list = ls())
library(foreign)
library(netdiffuseR)

# fctc_att    <- read.dta("c:/misc/fctc/data/attributes_v3.dta")
# fctc_net    <- read.dta("c:/misc/fctc/data/allnets.dta")
fctc_att    <- read.dta("playground/20160317_fctc/attributes_v3.dta")
fctc_net    <- read.dta("playground/20160317_fctc/allnets.dta")

# [2016-03-17]: A nice thing you can do in R is create lists to be used in
#  loops. Here I'm creating two lists where I'll store the diffnet and the
#  output from the model.
fctc_diffnets <- vector("list", 6)
fctc_logits   <- vector("list", 6)

relations <- c(1, 2, 3, 12, 13, 14)

# [2016-03-17]: Sometimes it is more convenient to write the loop using
#  indexes from 1:n instead of a list.
# for (r in relations) {
for (i in 1:length(relations)) {


  # [2016-03-17]: By doing it this way, at the second repetition, you are
  # subsetting on a dataset that only has relation == 1!
  # fctc_net <- subset(fctc_net, relation==r)
  sub_fctc_net <- subset(fctc_net, relation==relations[i])

  # [2016-03-17]: Both the edgelist and the data must span the same time period
  #  otherwise you get an error. Hence, after subsetting the edgelist, we get
  #  the time range of the year variable
  tran <- range(sub_fctc_net$year)

  # [2016-03-17]: I've realized that the first network is static (asked stephanie
  #  about it). So I'm creating this condition on t1 so it only varies for
  #  the networks other than the one inferred by relation == 1
  t1 <- if (relations[i] %in%  c(1,12:14)) NULL else sub_fctc_net$year

  fctcdiffnet2 <- edgelist_to_diffnet(
    edgelist = sub_fctc_net[,c("id","nom")],
    t0       = sub_fctc_net$year,
    t1       = t1,
    # [2016-03-17]: We have to make sure that the data is the same as the spanned
    #  time in the network!.
    # dat      = fctc_att
    dat      = subset(fctc_att, year %in% tran[1]:tran[2]), # [2016-03-17]
    idvar = "id", toavar = "toa_year_fctc", timevar="year",
    # [2016-03-17]: We already know that the data is not integer and it is going
    #  to be coerced. So in this case we can ask the function not to warn us!
    warn.coercion = FALSE,
    # [2013-03-17]: This is the new option we talked about. Since some edges are
    #  not present in this subset of the edgelist, the function will fill them.
    fill.missing = "both"
  )

  # Computing exposure
  fctcdiffnet2[["cohexp"]]  <- exposure(fctcdiffnet2)
  #fctcdiffnet2[["seexp"]]   <- exposure(fctcdiffnet2, alt.graph = "se", valued = TRUE)

  # summary(fctcdiffnet)

  test <- diffnet.attrs(fctcdiffnet2, as.df = TRUE)
  test$adopted <- as.integer(with(test, toa == per))
  #drop post adoption cases
  test <- subset(test, per <=  toa)
  #mod_all <- as.formula(paste("adopted ~ factor(per) + cohexp + seexp + factor(continent) + population "))
  mod_all <- as.formula(paste("adopted ~ factor(per) + cohexp + factor(continent) + population "))
  out_all <- glm(mod_all, data=test, family = binomial(link="logit"))

  # [2016-03-17] Printing objects within loops has to be explicit, otherwise you
  #  wont see anything
  # summary(out_all)
  print(summary(out_all))


  # [2016-03-17] Storing outputs
  fctc_diffnets[[i]] <- fctcdiffnet2
  fctc_logits[[i]]   <- out_all

}

# Plotting
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,3))
for (i in 1:length(fctc_diffnets))
  plot_threshold(fctc_diffnets[[i]], main=paste("TOA and Threshold for relation",i))

par(oldpar)

# [2016-03-18]: Mergin exposure data -------------------------------------------
for (i in 1:length(fctc_diffnets)) {

  # Getting the data
  exposurei <- diffnet.attrs(fctc_diffnets[[i]], as.df = TRUE)
  exposurei <- subset(exposurei, select=c(id, per, cohexp))

  # Renaming the last column (the cohesive)
  colnames(exposurei)[3] <- sprintf("cohexp_%02d", relations[i])

  # Merging
  if (i == 1) exposures <- exposurei
  else exposures <- merge(exposures, exposurei, by=c("id", "per"))
}

# Changing names of the id and per columns to merge it with the data
colnames(exposures)[1:2] <- c("id", "year")

# Merging with the original data
fctc_att <- merge(fctc_att, exposures, by=c("id", "year"))

# Running the model
mod_all <- as.formula(paste(
  # [2016-03-18]: Notice that I generate the variable 'adopted' in the formula
  "I(as.integer(toa_year_fctc==year)) ~ factor(year) + factor(continent) + population +",
  # [2016-03-18]: Try running this line alone so you know exactly what it does
  paste(sprintf("cohexp_%02d", relations), collapse = " + ")
  )
  )

# [2016-03-18]: Notice that I can use the subset argument to filter data
out_all <- glm(mod_all, data=fctc_att, family = binomial(link="logit"),
               subset = year <=  toa_year_fctc)

summary(out_all)
