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

library(dplyr)
library(tidyr)

fig <- dat %>%
  group_by(year) %>%
  summarise(
    sum_art05 = cor(`Years since Ratif.`, sum_art05, use="pairwise.complete"),
    sum_art06 = cor(`Years since Ratif.`, sum_art06, use="pairwise.complete"),
    sum_art08 = cor(`Years since Ratif.`, sum_art08, use="pairwise.complete"),
    sum_art11 = cor(`Years since Ratif.`, sum_art11, use="pairwise.complete"),
    sum_art13 = cor(`Years since Ratif.`, sum_art13, use="pairwise.complete"),
    sum_art14 = cor(`Years since Ratif.`, sum_art14, use="pairwise.complete")
  ) %>% 
  ungroup %>%
  gather(key = "Article", value = "Correlation", -year) %>%
  mutate(
    Article = stringr::str_remove(Article, ".+_art0?"),
    year    = jitter(year)
  ) %>% 
  rename(Year=year) %>%
  ggplot(., aes(x=Year, y=Correlation)) +
  geom_point(aes(col = Article), size=4)  +
  ggtitle("Correlation levels between 'Years since Ratif' and\nImplementation levels by Year") + 
  theme(
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    axis.text    = element_text(size=15),
    legend.text  = element_text(size=15),
    legend.title = element_text(size=15),
    title = element_text(size=15)
    ) +
  scale_color_brewer(type = "qual")
    
  
ggsave("fig/corr_years_since_ratif_and_implementation.png", plot = fig)
