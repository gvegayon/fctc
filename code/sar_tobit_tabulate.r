rm(list = ls())

library(spatialprobit)

networks      <- c(
  "adjmat_general_trade", "General Trade",
  "adjmat_tobacco_trade", "Tobacco Trade",
  "adjmat_mindist", "Minimal distance",
  "adjmat_centroid_dist", "Centroid Distance",
  "adjmat_border", "Country Borders",
  "adjmat_gl_posts", "GlobalLink Posts",
  "adjmat_referrals", "GlobalLink Referrals",
  "adjmat_fctc_cop_coparticipation_twomode", "FCTC COP co-participation",
  "adjmat_fctc_inb_coparticipation_twomode", "FCTC INB co-participation",
  "adjmat_interest_group_comembership_twomode", "Interest Group co-membership"
)

networks <- matrix(networks, ncol = 2, byrow = TRUE)

# Finding files
files <- list.files(path = "code/", pattern = "sar_tobit_.+.rda", full.names = TRUE)

# Program to fetch the alphas

get_rho <- function(object, art) {
  # Calling the summary
  sink(tempfile())
  ans <- summary(object)
  sink()
  ans <- ans["rho", c("Estimate", "Std. Dev", "Pr(>|z|)"), drop=FALSE]
  rownames(ans) <- art
  ans
}

bigtable <- NULL
for (fn in networks[,1]) {
  # Trying to clean up
  tryCatch(rm(list = ls(pattern = "^sar_tobit_sum.+", envir = .GlobalEnv)))
  load(sprintf("code/sar_tobit_%s.rda", fn))
  
  
  # Listing models
  ans_model_1 <- ls(pattern = "sar_tobit_sum.+1$")
  ans_model_2 <- ls(pattern = "sar_tobit_sum.+2$")
  ans_model_3 <- ls(pattern = "sar_tobit_sum.+3$")
  ans_model_4 <- ls(pattern = "sar_tobit_sum.+4$")
  ans_model_5 <- ls(pattern = "sar_tobit_sum.+5$")
  ans_model_6 <- ls(pattern = "sar_tobit_sum.+6$")
  
  # Extracting coefficients
  rho_model_1 <- do.call(rbind, Map(get_rho, lapply(ans_model_1, get), c(5, 6, 8, 11, 13)))
  rho_model_2 <- do.call(rbind, Map(get_rho, lapply(ans_model_2, get), c(5, 6, 8, 11, 13)))
  rho_model_3 <- do.call(rbind, Map(get_rho, lapply(ans_model_3, get), c(5, 6, 8, 11, 13)))
  rho_model_4 <- do.call(rbind, Map(get_rho, lapply(ans_model_4, get), c(5, 6, 8, 11, 13)))
  rho_model_5 <- do.call(rbind, Map(get_rho, lapply(ans_model_5, get), c(5, 6, 8, 11, 13)))
  rho_model_6 <- do.call(rbind, Map(get_rho, lapply(ans_model_6, get), c(5, 6, 8, 11, 13)))
  
  # Assingning to the overall tab
  bigtable <- rbind(bigtable, rho_model_1[,3])
  
}

# Adding fancy names
rownames(bigtable) <- networks[,2]
print(bigtable, digits=1)

formatted_bigtable <- bigtable
formatted_bigtable[] <- 
  ifelse(formatted_bigtable > .1, sprintf("%.2f", formatted_bigtable),
         ifelse(formatted_bigtable > .05, sprintf("%.2f*", formatted_bigtable),
                sprintf("%.2f**", formatted_bigtable)
  ))

formatted_bigtable[,ncol(formatted_bigtable)] <-
  sprintf("%s\n", formatted_bigtable[,ncol(formatted_bigtable)])

cat(formatted_bigtable)
