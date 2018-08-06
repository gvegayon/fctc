# This file creates the final dataset that will be used to run the models
# the network data is in a separate file called data/adjmats.rda.

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)
library(magrittr)
library(readxl)
library(netdiffuseR)

# parameter
years_reported <- c(2010, 2012, 2014, 2016) # 2014 (need to extend data)
articles_to_use <- c(5, 6, 8, 11, 13, 14)

# Reading datasets -------------------------------------------------------------
country_codes    <- readr::read_csv("data-raw/country_codes/country_codes.csv", na = "<NA>")
country_codes    <- subset(country_codes, select=c(-subdivision_assigned_codes))

political_shifts <- readr::read_csv("data/political_shifts.csv", na = "<NA>")
political_shifts <- subset(political_shifts, select=c(-country_name, -execrlc))

who_region <- readr::read_csv("data/party_attributes.csv", na = "<NA>") %>%
  select(entry, who_region, continent)

worldbank <- readr::read_csv("data-raw/worldbank/worldbank.csv", na = "<NA>")
qog <- readr::read_csv("data-raw/quality_of_government/qog.csv", na = "<NA>")

tobacco_prod <- readr::read_csv("data/tobacco_prod.csv")

bloomberg        <- read.csv("data/bloomberg.csv", na = "<NA>")
bloomberg        <- subset(bloomberg, select=c(-country_name))

treaty_dates     <- read.csv("data/treaty_dates.csv", na = "<NA>")
treaty_dates     <- subset(treaty_dates, select=c(entry, signature, ratification))

implementation   <- read.csv("data/implementation.csv", na = "<NA>")
implementation_post2014 <- readr::read_csv("data/implementation-post2014.csv", na = "<NA>")

govtown          <- read.csv("data/govtown.csv", na = "<NA>")

# Imputation for implementation ------------------------------------------------

# Merging two implementations

implementation <- implementation %>%
  filter(year != 2014) %>% # Getting 2014 from  implementation_post2014
  rbind(implementation_post2014) %>%
  arrange(entry, year) %>%
  as_tibble


# Merging ----------------------------------------------------------------------
dat <- worldbank %>% 
  left_join(political_shifts, by=c("date" = "year", "iso2c" = "entry")) %>%
  rename(entry = iso2c, year = date) %>%
  left_join(treaty_dates, by="entry") %>%
  left_join(implementation, by=c("year", "entry")) %>%
  left_join(bloomberg, by=c("year", "entry")) %>%
  left_join(govtown, by = "entry") %>%
  left_join(tobacco_prod, by = c("entry", "year")) %>%
  left_join(who_region, by = c("entry")) %>%
  arrange(entry, year)

# Bloomberg data should be filled with zeros instead of NAs
dat$bloomberg_amount      <- coalesce(dat$bloomberg_amount, 0)
dat$bloomberg_count       <- coalesce(dat$bloomberg_count, 0L)
dat$bloomberg_fctc_count  <- coalesce(dat$bloomberg_fctc_count, 0L)
dat$bloomberg_fctc_amount <- coalesce(dat$bloomberg_fctc_amount, 0)

# Implementation data should be filled with zeros instead of NAs
# we will assume that they did not implemented
# > subset(dat, year == 2012 & !is.na(ratification))$no_report %>% table(useNA="always")
# .
# FALSE  TRUE  <NA> 
#   143    22    11 

# Compuiting some basic stats, how often the number of items implementad decreased?
test <- implementation %>%
  group_by(entry) %>%
  mutate(
    diff05 = sum_art05 - lag(sum_art05),
    diff06 = sum_art06 - lag(sum_art06),
    diff08 = sum_art08 - lag(sum_art08),
    diff11 = sum_art11 - lag(sum_art11),
    diff13 = sum_art13 - lag(sum_art13),
    diff14 = sum_art14 - lag(sum_art14)
  ) %>%
  mutate(
    diff05 = if_else(diff05 > 0, "+", if_else(diff05 == 0, "=", "-")),
    diff06 = if_else(diff06 > 0, "+", if_else(diff06 == 0, "=", "-")),
    diff08 = if_else(diff08 > 0, "+", if_else(diff08 == 0, "=", "-")),
    diff11 = if_else(diff11 > 0, "+", if_else(diff11 == 0, "=", "-")),
    diff13 = if_else(diff13 > 0, "+", if_else(diff13 == 0, "=", "-")),
    diff14 = if_else(diff14 > 0, "+", if_else(diff14 == 0, "=", "-"))
  ) %>%
  ungroup 

lapply(
    colnames(test)[grepl("^diff", colnames(test))],
    function(x) {
      ans <- prop.table(table(test[[x]], test$year), 2)
      rownames(ans) <- paste(gsub("diff", "", x), rownames(ans))
      ans
    }) %>%
  do.call("rbind", .) %>%
  as_tibble(rownames = "change") %>%
  readr::write_csv(path="data/model_data-variation-of-implementation.csv")
  

# Sorting
dat$no_report[is.na(dat$no_report)] <- 1L
articles <- sprintf("sum_art%02i", articles_to_use)

# Imputing ---------------------------------------------------------------------
dat2 <- dat
library(tidyr)
library(tidyselect)

# Carry forward
dat2 <- group_by(dat, entry) %>%
  fill(starts_with("sum_art"), .direction = "down") %>%
  arrange(entry, year) %>%
  as.data.frame

# Zero back
for (j in articles)
  for (i in nrow(dat2):2) {
    
    # Are we in the current
    if (dat2$entry[i] != dat2$entry[i-1])
      next
    
    if (!is.na(dat2[i, j]) && (dat2[i, j] == 0 & is.na(dat2[i-1, j])))
      dat2[i-1, j] <- dat2[i, j]
    
  }
    

dat2$no_report <- apply(
  dat2[,grepl("^sum_art[0-9]+",colnames(dat2))], 
  1,
  function(x) all(is.na(x))
)
dat2$no_report <- as.integer(dat2$no_report)

for (y in years_reported)
  print(table(
    `n/a (Original)` = dat$no_report[dat$year == y],
    Imputed  = dat2$no_report[dat2$year == y]
    )
  )

# dat %>%
#   select(entry, year, sum_art05, sum_art11) %>%
#   cbind(
#     subset(dat2[,c("entry", "year", "sum_art05", "sum_art11")])
#   ) %>%
#   View("Are we OK?")

# Comparing before and after
for (art in articles)
print(table(
  Original = dat[[art]][dat$year %in% c(2012)],
  Imputed  = dat2[[art]][dat2$year %in% c(2012)],
  useNA="always"
  ))

# We'll just used the imputed data instead...
# Egypt did not reported on 2012 but did on 2010
# So carry forward only works on it!
# See http://apps.who.int/fctc/implementation/database/article/article-13/indicators/5346/reports
# Comprehensive ban on all tobacco advertising, promotion and sponsorship
# Party	2014	2012	2010
# Egypt	| Yes |	Answer/report not provided |	Yes
dat <- dat2

dat$sum_art05[is.na(dat$sum_art05)] <- 0L
dat$sum_art06[is.na(dat$sum_art06)] <- 0L
dat$sum_art08[is.na(dat$sum_art08)] <- 0L
dat$sum_art11[is.na(dat$sum_art11)] <- 0L
dat$sum_art13[is.na(dat$sum_art13)] <- 0L


# Pasting names
dat <- left_join(dat, country_codes, by="entry")

# Creating dummies -------------------------------------------------------------
for (cont in unique(dat$continent))
  if (!is.na(cont))
    dat[[cont]] <- ifelse(dat$continent == cont, 1L, 0L)

for (who in unique(dat$who_region))
  if (!is.na(who))
    dat[[who]] <- ifelse(dat$who_region == who, 1L, 0L)


# Filtering --------------------------------------------------------------------
dat <- filter(dat, year %in% years_reported)
dat <- filter(dat, !is.na(ratification)) # Only those which ratified

# Sorting the data
dat <- dat %>% arrange(entry, year)

# View(subset(
#   dat,
#   select=c(
#     country_name, year, pol_shift, bloomberg_amount, ratification,
#     sum_art05, sum_art06, sum_art08)))
# 

# Updating year of ratification and signature ----------------------------------
dat$year_ratification <- dat$ratification %/% 10000L
dat$year_signature    <- dat$signature %/% 10000L

# Fixing some NAs
dat$year_signature <- with(dat, ifelse(is.na(year_signature), year_ratification,
                                       year_signature))

# With respect to 2010 (trucate it to 2010)
# 
# Prior to 2010, implementation cannot affect ratification/signature since it
# was an event that happened before. But, the implementation level of 2010 can
# certainly affect signature and ratification as it is a posterior event. Therefore
# we truncate these variables at 0.
dat$`Years since Ratif.` <- with(dat, 2014L - year_ratification)
dat$`Years since Sign.`  <- with(dat, 2014L - year_signature)

dat$`Years since Ratif.`[dat$`Years since Ratif.` < 0] <- 0L
dat$`Years since Sign.`[dat$`Years since Sign.` < 0]   <- 0L

# If NA, it means that these didn't showed in the lists of reports, but may
# have ratified.
dat$no_report[is.na(dat$no_report)] <- 1L

write.csv(dat, "data/model_data_unscaled.csv", row.names = FALSE, na = "<NA>")

# Rescaling variables ----------------------------------------------------------
dat$tobac_prod_pp            <- with(dat, tobacco_prod/population)
dat$bloomberg_amount_pp      <- with(dat, bloomberg_amount/population)
dat$bloomberg_fctc_amount_pp <- with(dat, bloomberg_fctc_amount/population)
dat$logPopulation            <- log(dat$population)

for (v in colnames(dat))
  if (is.double(dat[[v]])) 
    dat[[v]] <- dat[[v]]/sd(dat[[v]], na.rm = TRUE)

  #   {
  #   cat(sprintf("%30s: Yes\n", v))
  # } else
  #   cat(sprintf("%30s:     No\n", v))
  #   # dat[[v]] <- dat[[v]]/sd(dat[[v]])

# Including interest on policy (subscribed to GL posts) ------------------------

load("data/adjmats.rda")
graph <- adjmat_gl_posts[c("2008", "2009", "2010")] # 
graph <- graph[[1]] + graph[[2]] + graph[[3]]
adjmat_gl_posts <- graph # 

posts <- dgr(adjmat_gl_posts)
posts <- data.frame(subscribed = posts, entry = rownames(posts))
posts$subscribed <- as.integer(posts$subscribed)

dat <- merge(dat, posts, by="entry", all.x=TRUE, all.y=FALSE)
dat$subscribed[is.na(dat$subscribed)] <- 0L

# Saving the data --------------------------------------------------------------
write.csv(dat, "data/model_data.csv", row.names = FALSE, na = "<NA>")

