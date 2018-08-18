library(Matrix)
library(dplyr)
library(tidyr)

# Loading implementation data --------------------------------------------------
implementation          <- readr::read_csv("data/implementation.csv", na="<NA>")
implementation_post2014 <- readr::read_csv("data/implementation-post2014.csv", na="<NA>")

implementation <- implementation %>%
  filter(year < 2014) %>%
  rbind(implementation_post2014)

# Loading networks -------------------------------------------------------------
load("data/adjmats.rda")
adjmat_border        <- readRDS("data/adjmat_border.rds")
adjmat_mindist       <- readRDS("data/adjmat_mindist.rds")
adjmat_tobacco_trade <- readRDS("data/adjmat_tobacco_trade.rds")
adjmat_general_trade <- readRDS("data/adjmat_general_trade.rds")
adjmat_centroid_dist <- readRDS("data/adjmat_centroid_dist.rds")

# Preprocessing networks gl_posts and referrals
g0 <- adjmat_gl_posts[as.character(2008:2010)] # 
adjmat_gl_posts <- g0[[1]]
for (g in g0[-1])
  adjmat_gl_posts <- adjmat_gl_posts + g

# image(adjmat_gl_posts)
# nlinks(adjmat_gl_posts)/(nnodes(adjmat_gl_posts)*(nnodes(adjmat_gl_posts)-1))

g0 <- adjmat_referrals # 
adjmat_referrals <- g0[[1]]
for (g in g0[-1])
  adjmat_referrals <- adjmat_referrals + g

adjmat_interest_group_comembership_twomode <-
  adjmat_interest_group_comembership_twomode$`2010`

# image(adjmat_referrals)
# nlinks(adjmat_referrals)/(nnodes(adjmat_referrals)*(nnodes(adjmat_referrals)-1))

# If i referred j, then i has an influence over j, hence we transpose to compute
# exposures.
adjmat_referrals <- t(adjmat_referrals)

# We will only work with static networs, this is, we will exclude the
# bilateral investment network
rm(adjmat_bilateral_investment_treaties)

# Adjusting the trade networks

# Checking how much time each spans
adjmats    <- ls(pattern="adjmat_")
time_spans <- matrix(
  "",
  nrow = length(adjmats),
  ncol = length(2002:2014),
  dimnames = list(adjmats, 2002:2014)
  )

for (m in ls(pattern="adjmat_")) {
  
  A <- get(m)
  if (!inherits(A, "list"))
    time_spans[m, ] <- " - "
  else 
    time_spans[m, names(A)] <- " Yes "

}

as.data.frame(time_spans)

# Function to compute exposure -------------------------------------------------
calc_exposure <- function(y, x, postfix) {
  
  if (length(y) > 1)
    return(
      do.call(
        rbind, 
        lapply(y, calc_exposure, x, postfix)
        )
    )
  
  if (missing(postfix))
    postfix <- substitute(x)
  postfix <- gsub("adjmat_", "", postfix)
  
  # Filtering the data
  res <- implementation %>%
    subset(year == y) %>%
    select(entry, year, starts_with("sum_art")) %>%
    as.data.frame
  
  # Checking that all names are in place
  countries <- res$entry
  
  test <- countries[which(!(countries %in% colnames(x)))]
  if (length(test)) {
    x <- rbind(
      x,
      matrix(0, nrow=length(test), ncol=ncol(x),
             dimnames = list(test, colnames(x)))
    )

    x <- cbind(
      x,
      matrix(0, ncol=length(test), nrow=nrow(x),
             dimnames = list(rownames(x), test))
    )

  }

  # Giving it the right sorting
  x <- x[countries, countries]
  x <- as.matrix(x)
  x <- x / (rowSums(x) + 1e-10)
  
  # Computing exposures
  E <- colnames(res)[grepl("^sum_art[0-9]+$",colnames(res))]
  for (e in E) {
    artname <- gsub("sum_", "", e)
    res[[paste0(artname, "exp_", postfix)]] <- x %*% coalesce(res[[e]], 0L) %>%
    as.vector
  }
  
  as_tibble(res) %>%
    select(entry, year, contains("exp_"))
  # res
}

# Calculating exposures
exposures_centroid <- calc_exposure(c(2010, 2012, 2014), adjmat_centroid_dist, "centroid_dist")
exposures_mindist <- calc_exposure(c(2010, 2012, 2014), adjmat_mindist, "mindist")
exposures_referrals <- calc_exposure(c(2010, 2012, 2014), adjmat_referrals, "referrals")
exposures_interest_group <- calc_exposure(c(2010, 2012, 2014), adjmat_interest_group_comembership_twomode, "interest_group_comembership_twomode")

exposures_general_trade <- 
  Map(
    calc_exposure,
    c(2010, 2012, 2014),
    adjmat_general_trade[c("2010", "2012", "2014")],
    "general_trade"
    ) %>% do.call(rbind, .)

exposures_tobacco_trade <- 
  Map(
    calc_exposure,
    c(2010, 2012, 2014),
    adjmat_tobacco_trade[c("2010", "2012", "2014")],
    "tobacco_trade"
  ) %>% do.call(rbind, .)

exposure <- implementation %>%
  filter(year >= 2010, year <= 2014, !is.na(entry)) %>%
  left_join(exposures_centroid) %>%
  left_join(exposures_mindist) %>%
  left_join(exposures_referrals) %>%
  left_join(exposures_interest_group) %>%
  left_join(exposures_general_trade) %>%
  left_join(exposures_tobacco_trade) %>%
  arrange(entry, year)

# Calculating lag 
exposure <- exposure %>%
  mutate(year = year + 2) %>%
  select(entry, year, contains("exp_")) %>%
  arrange(year, entry)

readr::write_csv(exposure, "data/exposure.csv", "<NA>")
