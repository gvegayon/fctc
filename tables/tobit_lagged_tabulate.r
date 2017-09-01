rm(list = ls())

library(AER)

# List of networks
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

# Function to compute pseudo R2 using McFadden's method
pseudoR2 <- function(x) {
  sprintf("%.2f", 1 - x$loglik[2]/x$loglik[1])
}

# Function to get coefficients
get_coefs <- function(netname, depvar, varnames, modelnum=1, digits = 2) {
  env <- new.env()
  
  # Fetching the estimates
  load(sprintf("models/tobit_lagged_%s.rda", netname), envir = env)
  
  estimates <- env[[sprintf("tobit_lagged_%s_%i", depvar, modelnum)]]
  
  # Retrieving the betas and sds
  nobs <- nrow(estimates$y)
  estimates <- summary(estimates)$coefficients[]

  # Getting the 
  ans <- estimates[varnames,,drop=FALSE]
  
  pvals <- ans[,"Pr(>|z|)"]
  pvals <- ifelse(pvals > .1, "", ifelse(pvals >.05, "*", ifelse(pvals >.01, "**", "***")))
  
  mask_b  <- sprintf("%%0.%if %%s", digits, digits)
  mask_sd <- sprintf("(%%0.%if)", digits)
  
  tab <- matrix(ncol=1, nrow=nrow(ans)*2, dimnames = list(1:(nrow(ans)*2), netname))
  rowid_betas <- seq(1, nrow(tab) - 1L, by = 2)
  rowid_sds   <- seq(2, nrow(tab), by = 2)
  
  rownames(tab)[rowid_betas] <- names(varnames)
  rownames(tab)[rowid_sds]   <- ""
  
  tab[rowid_betas,1] <- sprintf(mask_b, ans[,"Estimate"], pvals)
  tab[rowid_sds,  1] <- sprintf(mask_sd, ans[,"Std. Error"])
  
  
  # # Number of observations
  ans <- rbind(
    tab,
    matrix(nobs, ncol = 1, dimnames = list("N", netname)),
    matrix(pseudoR2(env[[sprintf("tobit_lagged_%s_%i", depvar, modelnum)]]), ncol = 1, dimnames = list("Pseudo R2", netname))
    )
  
  ans
}

# Variables to include in the table
varnames <- list(
  c(
  rho                     = "rho", 
  `Gov. Ownership`        = "govtown",
  `Tobacco Prod. PP`      = "tobac_prod_pp",
  `Years since Sign.`     = "`Years since Sign.`", 
  `Years since Ratif.`    = "`Years since Ratif.`",
  `GDP pp`                = "GDP",
  `Democracy`             = "democracy",
  `% Female Smoke`        = "perc_female_smoke",
  `% Male Smoke`          = "perc_male_smoke",
  `Labor`                 = "labor",
  `Women's rights`        = "womens_rights",
  `Population`            = "population",
  `Eastern Mediterranean` = "`Eastern Mediterranean`",
  European                = "European",
  African                 = "African",
  `Western Pacific`       = "`Western Pacific`",
  `South-East Asia`       = "`South-East Asia`",
  `(Intercept)`           = "(Intercept)"
  )
)

varnames[[2]] <- c(varnames[[1]], PolShift = "pol_shift")
varnames[[3]] <- c(varnames[[1]], `Bloomberg Amount` = "bloomberg_amount")
varnames[[4]] <- c(varnames[[1]], `Bloomberg Count` = "bloomberg_count")
varnames[[5]] <- c(varnames[[1]], `Bloomberg FCTC Amount` = "bloomberg_fctc_amount")
varnames[[6]] <- c(varnames[[1]], `Bloomberg FCTC Count` = "bloomberg_fctc_count")

articles <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

# Loop through model number
for (m in 0:6) {
  
  # Creating new environment
  env <- new.env()
  
  # Looping through depvars
  rhos <- matrix(NA, nrow = length(articles), ncol = nrow(networks),
                 dimnames = list(articles, networks[,1]))
  
  rhos <- matrix(NA,
    nrow = nrow(networks)*2,
    ncol = length(articles),
    dimnames = list(
      1:(nrow(networks)*2),
      articles)
    )
  
  rowid_betas <- structure(seq(1, nrow(rhos) - 1L, by = 2), names = networks[,1])
  rowid_sds   <- structure(seq(2, nrow(rhos), by = 2), names = networks[,1])
  
  rownames(rhos)[rowid_betas] <- networks[,1]
  rownames(rhos)[rowid_sds]   <- ""
  
  for (a in articles) {
    
    ans <- NULL
    
    # Looping throught networks
    for (net in networks[,1]) {
      
      # Fetching the data and rowbinding
      ans0 <- get_coefs(
        netname  = net,
        depvar   = a,
        varnames = if (m == 0) {
          c(varnames[[m + 1]],
            structure(
              paste0(a, "_lagged"),
              names = sprintf("Art %s (t-1)",gsub("sum_art0?", "", a))
              )
            )
          } else varnames[[m]],
        modelnum = m, 
        digits   = 2
      )
      
      ans <- cbind(
        ans, ans0
      )
      
      # Adding to the overall list
      rhos[rowid_betas[net], a] <- ans0["rho",1,drop=TRUE]
      rhos[rowid_sds[net], a] <- ans0[
        match("rho", rownames(ans0)) + 1,
        1,drop=TRUE]
      
      # Are we there yet?
      message("Network: ", net, " variable: ", a, " done.")
    }
    
    # Pretty dimnames
    colnames(ans) <- networks[,2]
    
    # Saving
    assign(a, ans, envir = env)
  }
  
  # Storing rhos
  rownames(rhos)[rowid_betas] <- networks[match(names(rowid_betas), networks[,1]),2]
  
  write.table(
    matrix(c("", gsub("sum_art0?", "Article ",colnames(rhos))), nrow=1),
    sprintf("tables/tobit_lagged_tabulate_rhos_model=%i.csv", m),
    row.names = FALSE, col.names = FALSE, sep=","
    )
  write.table(
    rhos,
    sprintf("tables/tobit_lagged_tabulate_rhos_model=%i.csv", m),
    row.names = TRUE, sep=",", quote=TRUE,
    col.names = FALSE, append=TRUE
  )
  
  # Putting all tables together
  fn <- sprintf("tables/tobit_lagged_tabulate_model=%i.csv", m)
  # for (a in articles) {
  #   
  #   # Article number
  #   write.table(
  #     matrix(c(gsub("sum_art0?", "Art. ", a), colnames(get(a, envir = env))), nrow=1) ,
  #     fn, row.names = FALSE, col.names = FALSE, quote = TRUE, sep=",",
  #     append = ifelse(a == articles[1], FALSE, TRUE)
  #   )
  #   
  #   # Writing values
  #   write.table(
  #     get(a, envir = env), fn, append = TRUE, row.names = TRUE, sep = ",",
  #     col.names = FALSE,
  #     quote=FALSE)
  # }
  
  for (i in 1:length(networks[,1])) {
    # Getting the data
    tmpdat <- do.call(cbind, lapply(env, "[", i=, j=networks[i,2]))
    tmpdat <- tmpdat[, order(colnames(tmpdat))]
    colnames(tmpdat) <- gsub("sum_art0?", "Article ", colnames(tmpdat))
    
    # Article number
    write.table(
      matrix(c(networks[i,2], colnames(tmpdat)), nrow=1) ,
      fn, row.names = FALSE, col.names = FALSE, quote = TRUE, sep=",",
      append = ifelse(i == 1, FALSE, TRUE)
    )
    
    write.table(
      tmpdat,
      fn, row.names = TRUE, col.names = FALSE, quote = TRUE, sep=",",
      append = TRUE
    )

  }
   
  # Adding note
  write.table(
    "Standard Errors in parenthesis. Signif. codes: 0.01: '***' 0.05: '**' 0.10 '*'",
    fn, append = TRUE, row.names = FALSE, col.names = FALSE, quote=FALSE)

  # Cleaning everything 
  message("Model ", m, " done ----------------------------------------------------")

  rm(env)
}

