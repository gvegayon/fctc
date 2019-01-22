library(Amelia)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/model_data.csv", na="<NA>") %>%
  unique %>%
  arrange(entry, year)

to_skip <- c(
  "country_name",
  "iso3c",
  "no_report",
  # "pol_shift",
  "pol_shift_left",
  "pol_shift_right",
  "continent",
  "who_region",
  "signature",
  "sum_art05",
  "sum_art06",
  "sum_art08",
  "sum_art11",
  "sum_art13",
  "sum_art14",
  "year_signature",
  "year_ratification",
  "Years since Ratif.",
  "Years since Sign.",
  colnames(dat)[grepl("^art[0-9]+exp", colnames(dat))],
  "Eastern Mediterranean"
)

set.seed(17778841)

ans <- amelia(
  x        = as.data.frame(dat),
  m        = 10,
  idvars   = to_skip,
  ts       = "year",
  polytime = 1,
  cs       = "entry",
  logs     = c("gdp_percapita_ppp", "health_exp", "birth_death"),
  sqrts    = c("population", "tobacco_prod"),
  lgstc    = c("smoke_female", "smoke_male"), 
  emburn   = c(10, 1000)
)

View(tibble(
  country = dat$country_name,
  year    = dat$year,
  smk_fem_im1 = round(ans$imputations$imp1$smoke_female, 2),
  smk_fem_im2 = round(ans$imputations$imp2$smoke_female, 2),
  smk_fem_im3 = round(ans$imputations$imp3$smoke_female, 2),
  smk_fem_im4 = round(ans$imputations$imp5$smoke_female, 2),
  smk_fem_im5 = round(ans$imputations$imp5$smoke_female, 2),
  smk_mal_im1 = round(ans$imputations$imp1$smoke_male, 2),
  smk_mal_im2 = round(ans$imputations$imp2$smoke_male, 2),
  smk_mal_im3 = round(ans$imputations$imp3$smoke_male, 2),
  smk_mal_im4 = round(ans$imputations$imp5$smoke_male, 2),
  smk_mal_im5 = round(ans$imputations$imp5$smoke_male, 2),
  smk_fem = dat$smoke_female,
  smk_mal = dat$smoke_male
), "Imputed")

View(tibble(
  country = dat$country_name,
  year    = dat$year,
  health_exp_im1 = round(ans$imputations$imp1$health_exp, 2),
  health_exp_im2 = round(ans$imputations$imp2$health_exp, 2),
  health_exp_im3 = round(ans$imputations$imp3$health_exp, 2),
  health_exp_im4 = round(ans$imputations$imp5$health_exp, 2),
  health_exp_im5 = round(ans$imputations$imp5$health_exp, 2),
  health_exp     = dat$health_exp
), "Imputed")

graphics.off()
pdf("data/multiple-imputation.pdf")
missmap(ans, y.cex = .25, x.cex = .5)
dev.off()

# Rescaling variables ----------------------------------------------------------

rescale_data <- function(dat) {
  
  dat$tobac_prod_pp            <- with(dat, tobacco_prod/population)
  dat$bloomberg_amount_pp      <- with(dat, bloomberg_amount/population)
  dat$bloomberg_fctc_amount_pp <- with(dat, bloomberg_fctc_amount/population)
  dat$logPopulation            <- log(dat$population)
  dat$logTobac_prod_pp         <- log(dat$tobac_prod_pp)
  dat$logHealth_exp            <- log(dat$health_exp)
  dat$logGDP_percapita_ppp     <- log(dat$gdp_percapita_ppp)

  # Replacing infinite values with NAs
  replace_not_finite <- function(x) {
    ifelse(!is.finite(x), NA, x)
  }
  dat <- dat %>%
    mutate_if(is.numeric, replace_not_finite)
  
  # Adding lags
  # Year fixed effects: 2010 as reference
  year0_1           <- model.matrix(~0+factor(year), dat)
  colnames(year0_1) <- gsub(".+([0-9]{4})$", "Year \\1", colnames(year0_1))
  dat      <- cbind(dat, year0_1[,-1]) 
  
  dat %>%
    arrange(entry, year) %>%
    group_by(entry) %>%
    mutate(
      sum_art05_lagged = lag(sum_art05),
      sum_art06_lagged = lag(sum_art06),
      sum_art08_lagged = lag(sum_art08),
      sum_art11_lagged = lag(sum_art11),
      sum_art13_lagged = lag(sum_art13),
      sum_art14_lagged = lag(sum_art14)
    )
  
  #   {
  #   cat(sprintf("%30s: Yes\n", v))
  # } else
  #   cat(sprintf("%30s:     No\n", v))
  #   # dat[[v]] <- dat[[v]]/sd(dat[[v]])
  
  # Including interest on policy (subscribed to GL posts) ------------------------
  # dat
}

ans$imputations <- ans$imputations %>% lapply(rescale_data)
  

readr::write_csv(rescale_data(dat), path = "data/multiple_imputation.csv")
write.amelia(ans, file.stem = "data/multiple-imputation")
