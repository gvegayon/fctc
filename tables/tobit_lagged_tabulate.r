rm(list = ls())

library(AER)

# Reading the models data (not the results themselfs) --------------------------

# Loading the model specifications, this includes:
#  - models,
#  - networks, and
#  - articles
load("models/tobit_lagged_specifications.rda")

# Processing the models (names of the variables) -------------------------------

# Changing the varnames of the models
fancy_varnames <- c(
  `Lagged Exposure (rho)` = "rho", 
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
  `(Intercept)`           = "(Intercept)",
  `Implemented 2010`      = "y_lagged",
  PolShift                = "pol_shift",
  `Bloomberg $$$ PP`      = "bloomberg_amount_pp",
  `Bloomberg #`           = "bloomberg_count",
  `Bloomberg FCTC $$$ PP` = "bloomberg_fctc_amount_pp",
  `Bloomberg FCTC #`      = "bloomberg_fctc_count",
  `No report`             = "no_report"
)

models <- lapply(models, function(x) {
  # Which of this model's variables are in fancy names?
  ids <- which(x$vars %in% fancy_varnames)
  
  # Issuing a warning
  test <- which(!(x$vars %in% fancy_varnames))
  if (length(test))
    warning("The following variables don't show in -fancu_varnames-:",
            paste(x$vars[test], collapse=", "), ".")
  
  # These have to have a name... otherwise, there's nothing to replace!
  names(x$vars)      <- x$vars
  names(x$vars)[ids] <- names(fancy_varnames[match(x$vars[ids], fancy_varnames)])
  x
})

# Functions to process the data ------------------------------------------------

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
  model_obj_name <- sprintf("tobit_lagged_%s_%i", depvar, modelnum)
  ans <- rbind(
    tab,
    matrix(nobs, ncol = 1, dimnames = list("N", netname)),
    matrix(pseudoR2(env[[model_obj_name]]), ncol = 1, dimnames = list("Pseudo R2", netname))
    )
  
  ans
}

tryGet_coef <- function(...) tryCatch(get_coefs(...), error = function(e) e)


# Loop through model number
for (m in seq_along(models)) {
  
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
      ans0 <- tryGet_coef(
        netname  = net,
        depvar   = a,
        varnames = models[[m]]$vars,
        modelnum = m, 
        digits   = 2
      )
      
      if (inherits(ans0, "error")) {
        message("!!!ERROR: Network ", net, " model ", m," didn't worked...")
        ans <- cbind(ans, rep(NA, length(models[[m]]$vars)*2 + 2))
        next
      }
      
      ans <- cbind(
        ans, ans0
      )
      
      # Adding to the overall list
      rho_fancy_name            <- names(fancy_varnames)[fancy_varnames == "rho"]
      rhos[rowid_betas[net], a] <- ans0[rho_fancy_name, 1,drop=TRUE]
      rhos[rowid_sds[net], a]   <- ans0[
        match(rho_fancy_name, rownames(ans0)) + 1,
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
  
  # Extracting the model name...
  model_name <- names(models)[[m]]
  fn <- sprintf("tables/tobit_lagged_tabulate_model=%s_rhos.csv", model_name)
  write.table(
    matrix(c("", gsub("sum_art0?", "Article ",colnames(rhos))), nrow=1),
    fn,
    row.names = FALSE, col.names = FALSE, sep=","
    )
  write.table(
    rhos, fn, row.names = TRUE, sep=",", quote=TRUE, col.names = FALSE,
    append=TRUE
  )
  
  # Adding note
  write.table(
    "Standard Errors in parenthesis. Signif. codes: 0.01: '***' 0.05: '**' 0.10 '*'",
    fn, append = TRUE, row.names = FALSE, col.names = FALSE, quote=FALSE)
  cat(paste("\"Note:", models[[m]]$about), "\"", file = fn, append = TRUE)
  
  # Putting all tables together
  fn <- sprintf("tables/tobit_lagged_tabulate_model=%s.csv", model_name)

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
  cat(paste("\"Note:", models[[m]]$about), "\"", file = fn, append = TRUE)

  # Cleaning everything 
  message("Model ", model_name, " done ----------------------------------------------------")

  rm(env)
}

