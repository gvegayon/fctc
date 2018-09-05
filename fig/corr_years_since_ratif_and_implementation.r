library(magrittr)
library(ggplot2)

dat <- readr::read_csv("data/multiple_imputation.csv")
load("models/tobit_lagged_specifications.rda")

sums <- sprintf("sum_art%02i", c(5, 6, 8, 11, 13, 14))

test <- split.data.frame(dat, dat$year) %>%
  lapply(function(x) {
    
    lapply(sums, function(s) {
      
      cor.test(x[[s]], x$`Years since Ratif.`, alternative = "two.sided") %$%
        data.frame(s, estimate, lower=conf.int[1], upper = conf.int[2])
      
    }) %>% do.call(rbind, .)
    
  }) 

lapply(names(test), function(year) {
  cbind(year, test[[year]])
}) %>% do.call(rbind, .)

correlations <-
  c(sums, "Years since Ratif.") %>%
  gsub("`", "", .) %>%
  dat[,.] %>%
  cor(use = "pairwise.complete.obs")

correlations <- correlations[,"Years since Ratif.",drop=FALSE] %>%
  t %>%
  .[,order(.)]

names(correlations) <- gsub(
  "sum_art0?([0-9]+)",
  "Article \\1",
  names(correlations)
  )

graphics.off()
png("fig/corr_years_since_ratif_and_implementation.png")
op <- par(mai = par()$mai + c(0,.5,0,0), las=1)
barplot(
  correlations[-length(correlations)], horiz=TRUE, xlim = c(0,1),
  main = "Pearson correlation between 'Years since Ratif.'\nand levels of implementation",
  col  = adjustcolor("steelblue", alpha.f = .75),
  border = "steelblue",
  xlab = "Correlation level"
  )
par(op)

dev.off()