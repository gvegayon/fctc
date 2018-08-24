

dat <- readr::read_csv("data/multiple_imputation.csv")

with(dat, {
  list(
    bloomberg_amount_pp = bloomberg_amount_pp,
    bloomberg_fctc_amount = bloomberg_fctc_amount,
    bloomberg_count = bloomberg_count,
    bloomberg_fctc_count = bloomberg_fctc_count
  ) %>%
    lapply(function(x) cor.test(x, subscribed))}) %>%
  lapply(function(x) c(cor = x$estimate, pval = x$p.value)) %>%
  do.call(rbind, .) %>%
  as.data.frame %>%
  cbind(
    variable = rownames(.),
    .
  ) %>%
  as_tibble %>%
  readr::write_csv("data/cor_bloomberg_and_subscribed.csv")
