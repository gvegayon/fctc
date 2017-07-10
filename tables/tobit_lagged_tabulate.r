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
  
  ans <- matrix(
    sprintf(sprintf("%%.%if (%%.%if) %%s", digits, digits),
            ans[,"Estimate"],
            ans[,"Std. Error"],
            pvals),
    ncol = 1,
    dimnames = list(names(varnames), netname)
  )
  
  # # Number of observations
  ans <- rbind(ans, matrix(nobs, ncol = 1, dimnames = list("N", netname)))
  
  ans
}

# Variables to include in the table
varnames <- list(
  c(
  rho = "rho", 
  `Gov. Ownership` = "govtown",
  `Tobacco Prod. PP` = "tobac_prod_pp",
  `Years since Sign.` = "`Years since Sign.`", 
  `Years since Ratif.` = "`Years since Ratif.`",
  `GDP pp` = "GDP",
  `Democracy` = "democracy",
  `% Female Smoke` = "perc_female_smoke",
  `% Male Smoke` = "perc_male_smoke",
  `Labor` = "labor",
  `Women's rights` = "womens_rights",
  `Population` = "population"
  )
)

varnames[[2]] <- c(varnames[[1]], PolShift = "pol_shift")
varnames[[3]] <- c(varnames[[1]], `Bloomberg Amount` = "bloomberg_amount")
varnames[[4]] <- c(varnames[[1]], `Bloomberg Count` = "bloomberg_count")
varnames[[5]] <- c(varnames[[1]], `Bloomberg FCTC Amount` = "bloomberg_fctc_amount")
varnames[[6]] <- c(varnames[[1]], `Bloomberg FCTC Count` = "bloomberg_fctc_count")

articles <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

# Loop through model number
for (m in 1:6) {
  
  # Creating new environment
  env <- new.env()
  
  # Looping through depvars
  rhos <- matrix(NA, nrow = length(articles), ncol = nrow(networks),
                 dimnames = list(articles, networks[,1]))
  for (a in articles) {
    
    ans <- NULL
    
    # Looping throught networks
    for (net in networks[,1]) {
      
      # Fetching the data and rowbinding
      ans0 <- get_coefs(
        netname  = net,
        depvar   = a,
        varnames = varnames[[m]],
        modelnum = m, 
        digits   = 2
      )
      
      ans <- cbind(
        ans, ans0
      )
      
      # Adding to the overall list
      rhos[a, net] <- ans0["rho",1,drop=TRUE]
      
      # Are we there yet?
      message("Network: ", net, " variable: ", a, " done.")
    }
    
    # Pretty dimnames
    colnames(ans) <- networks[,2]
    
    # Saving
    assign(a, ans, envir = env)
  }
  
  # Storing rhos
  colnames(rhos) <- networks[,2]
  write.table(
    rhos,
    sprintf("tables/tobit_lagged_tabulate_rhos_model=%i.csv", m),
    row.names = TRUE, sep=",", quote=FALSE
  )
  
  # Putting all tables together
  fn <- sprintf("tables/tobit_lagged_tabulate_model=%i.csv", m)
  for (a in articles) {
    
    # Article number
    write.table(
      gsub("sum_art0?", "Art. ", a),
      fn, row.names = FALSE, col.names = FALSE, quote = FALSE,
      append = ifelse(a == articles[1], FALSE, TRUE)
    )
    
    # Writing values
    write.table(
      get(a, envir = env), fn, append = TRUE, row.names = TRUE, sep = ",",
      col.names = ifelse(a == articles[1], TRUE, FALSE),
      quote=FALSE)
  }
  
  # Adding note
  write.table(
    "Standard Errors in parenthesis. Signif. codes: 0.01: '***' 0.05: '**' 0.10 '*'",
    fn, append = TRUE, row.names = FALSE, col.names = FALSE, quote=FALSE)

  # Cleaning everything 
  message("Model ", m, " done ----------------------------------------------------")
  rm(env)
}

