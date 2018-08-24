rm(list = ls())

library(AER)
library(magrittr)

# Reading the models data (not the results themselfs) --------------------------

# Loading the model specifications, this includes:
#  - models,
#  - networks, and
#  - articles
load("models/tobit_lagged_specifications.rda")

# Processing the models (names of the variables) -------------------------------

# Networks to use
networks <- networks[networks[,1] != "adjmat_mindist",]

# Changing the varnames of the models
fancy_varnames <- c(
  `Lagged Exposure (rho)` = "rho", 
  # `Gov. Ownership`        = "govtown",
  `Tobacco Prod. per capita`= "tobac_prod_pp",
  # `Years since Sign.`     = "`Years since Sign.`", 
  `Years since Ratif.`    = "`Years since Ratif.`",
  `GDP per capita (ppp)`  = "gdp_percapita_ppp",
  `Control of Corruption` = "ctrl_corrup",
  `Rule of Law`           = "rule_of_law",
  `% Female Smoke`        = "smoke_female",
  `% Male Smoke`          = "smoke_male",
  `Health Exp. per capita (ppp)` = "health_exp",
  `Women's Labor`         = "labor",
  `Women's rights`        = "womens_rights",
  `Population`            = "logPopulation",
  Americas                = "Americas",
  African                 = "African",
  European                = "European",
  `Western Pacific`       = "`Western Pacific`",
  `South-East Asia`       = "`South-East Asia`",
  `Lagged Y (Number of Items)` = "y_lagged",
  PolShift                = "pol_shift",
  `Government Ownership`  = "govtown",
  `Bloomberg $$$ PP`      = "bloomberg_amount_pp",
  `Bloomberg #`           = "bloomberg_count",
  `Bloomberg FCTC $$$ PP` = "bloomberg_fctc_amount_pp",
  `Bloomberg FCTC #`      = "bloomberg_fctc_count",
  `No report`             = "no_report",
  `(Intercept)`           = "(Intercept)",
  `Year 2014`             = "`Year 2014`",
  `Year 2016`             = "`Year 2016`"
)

# # "`Eastern Mediterranean`",
# "Americas",
# "African",
# "European",
# "`Western Pacific`",
# "`South-East Asia`",
# # "democracy",
# "ctrl_corrup",
# "rule_of_law",
# "gdp_percapita_ppp",
# "`Years since Ratif.`",
# "tobac_prod_pp",
# "smoke_female",
# "smoke_male",
# "labor",
# "womens_rights",
# # "logPopulation",
# "govtown",
# "`Year 2014`",
# "`Year 2016`"

# # If you want to filter models, for now we will only focus on the model described
# # in the paper. The other specifications showed no significance.
# models <- models["Imp2010_report"]

models <- lapply(models, function(x) {
  # Which of this model's variables are in fancy names?
  ids <- which(x$vars %in% fancy_varnames)
  
  # Issuing a warning
  test <- which(!(x$vars %in% fancy_varnames))
  if (length(test)) {
    warning("The following variables don't show in -fancy_varnames-:",
            paste(x$vars[test], collapse=", "), ".")
    
  }
  
  # These have to have a name... otherwise, there's nothing to replace!
  names(x$vars)      <- x$vars
  names(x$vars)[ids] <- names(fancy_varnames[match(x$vars[ids], fancy_varnames)])
  x
})

# Functions to process the data ------------------------------------------------

# Function to compute pseudo R2 using McFadden's method
getAIC <- function(x) {
  sprintf("%.2f", AIC(x))
}

# Function to get coefficients
get_coefs <- function(netname, depvar, varnames, modelnum=1, digits = 2) {
  
  # Fetching the estimates
  model_files <- list.files(
    path    = "models/",
    pattern = sprintf("tobit_lagged_%s-imputed[0-9]+[.]rda", netname),
    full.names = TRUE
  )
  
  models <- lapply(
    model_files,
    function(x) {
      # Empty environment
      env <- new.env()
      
      # Loading the data in the space
      load(x, envir = env)
      
      # Extracting the required table
      sprintf("tobit_lagged_%s_%i", depvar, modelnum) %>%
        get(envir = env) 
    }
  )
  
  names(models) <- model_files
  
  estimates <- lapply(models, function(x) {
    tab <- coef(summary(x))
    
    # Getting the coef and error tables
    cbind(estimate = tab[,"Estimate"], error = tab[,"Std. Error"])
  })
  
  # Aggregating coefficients and sd.
  estimates <- list(
    estimate = lapply(estimates, "[", i=, j= "estimate") %>% do.call(rbind, .),
    error    = lapply(estimates, "[", i=, j= "error") %>% do.call(rbind, .)
  ) %$%
    Amelia::mi.meld(estimate, error) %>%
    do.call(rbind, .) %>%
    t %>%
    set_colnames(c("Estimate", "Std. Error"))
  
  # Computing pvalues
  estimates <- cbind(
    estimates, 
    "z value" = estimates[,1]/estimates[,2]
  )
  
  estimates <- cbind(
    estimates,
    "Pr(>|z|)" = pnorm(-abs(estimates[,"z value"]))*2
  )
   
  # Retrieving the betas and sds
  N <- length(models[[1]]$y)

  # Getting the 
  varnames <- varnames[varnames %in% fancy_varnames]
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
    matrix(N, ncol = 1, dimnames = list("N", netname))
    # matrix(getAIC(env[[model_obj_name]]), ncol = 1, dimnames = list("AIC", netname))
    )
  
  ans
}

tryGet_coef <- function(...) tryCatch(get_coefs(...), error = function(e) e)


# Loop through model number
for (m in seq_along(models)) {
  
  # Creating new environment
  env <- new.env()
  
  # Looping through depvars
  # rhos <- matrix(NA, ncol = length(articles), nrow = nrow(networks),
  #                dimnames = list(networks[,1], articles))
  
  rhos <- matrix(NA,
    nrow = length(articles)*2,
    ncol = nrow(networks),
    dimnames = list(
      1:(length(articles)*2),
      networks[,1])
    )
  
  rowid_betas <- structure(seq(1, nrow(rhos) - 1L, by = 2), names = articles)
  rowid_sds   <- structure(seq(2, nrow(rhos), by = 2), names = articles)
  
  rownames(rhos)[rowid_betas] <- articles
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
      rhos[rowid_betas[a], net] <- ans0[rho_fancy_name, 1,drop=TRUE]
      rhos[rowid_sds[a], net]   <- ans0[
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
  colnames(rhos) <- networks[match(colnames(rhos), networks[,1]),2]
  
  # Extracting the model name...
  model_name <- names(models)[[m]]
  fn <- sprintf("tables/tobit_lagged_tabulate_model=%s_rhos.csv", model_name)
  
  rownames(rhos) <- gsub("sum_art0?", "Article ",rownames(rhos))
  write.table(
    matrix(c("", colnames(rhos)), nrow=1),
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

  for (a in articles) {
    fn <- sprintf("tables/tobit_lagged_tabulate_model=%s-art=%s.csv",
                  model_name, gsub(".+art", "", a))
    
    # Getting the data
    tmpdat <- env[[a]]
    
    # Writing article number
    write.table(
      matrix(c(gsub(".+art(0|_)?", "Article ", a), colnames(tmpdat)), nrow=1),
      file = fn, append = FALSE, sep=",",
      quote=TRUE, row.names = FALSE, col.names = FALSE
    )

    # Article number
    write.table(
      tmpdat,
      fn, row.names = TRUE, col.names = FALSE, quote = TRUE, sep=",",
      append = TRUE
    )
    
    # Adding note
    write.table(
      "Standard Errors in parenthesis. Signif. codes: 0.01: '***' 0.05: '**' 0.10 '*'",
      fn, append = TRUE, row.names = FALSE, col.names = FALSE, quote=FALSE)
    cat(paste("\"Note:", models[[m]]$about), "\"", file = fn, append = TRUE)

  }
   
  

  # Cleaning everything 
  message("Model ", model_name, " done ----------------------------------------------------")

  rm(env)
}

