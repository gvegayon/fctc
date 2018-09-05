# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
options(stringsAsFactors = FALSE)
rm(list=ls())

library(netdiffuseR)
library(texreg)
library(Matrix)
library(AER)
library(magrittr)
library(dplyr)

# Function to create formulas
makeformula <- function(x) {
  paste(paste(x, collapse=" + "))
}

common_covars <- c(
  # "`Eastern Mediterranean`",
  "Americas",
  "African",
  "European",
  "`Western Pacific`",
  "`South-East Asia`",
  "POLITY",
  # "ctrl_corrup",
  # "rule_of_law",
  "logGDP_percapita_ppp",
  "`Years since Ratif.`",
  # "`Years since Sign.`",
  "logTobac_prod_pp",
  "smoke_female",
  "smoke_male",
  "labor",
  # "womens_rights",
  # "logPopulation",
  "govtown",
  "`Year 2014`",
  "`Year 2016`",
  "logHealth_exp"
  # "`Year 2014`*`Years since Ratif.`",
  # "`Year 2016`*`Years since Ratif.`"
  )

articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13", "sum_art14")

# List of networks (with pretty names) that will be used
networks      <- c(
  "adjmat_centroid_dist", "Centroid Distance",
  # "adjmat_mindist", "Minimal distance",
  "adjmat_general_trade", "General Trade",
  "adjmat_tobacco_trade", "Tobacco Trade",
  "adjmat_referrals", "GLOBALink Referrals",
  "adjmat_interest_group_comembership_twomode", "GLOBALink co-subscription"#,"Interest Group co-membership",
  # "adjmat_fctc_cop_coparticipation_twomode", "FCTC COP co-participation",
  # "adjmat_fctc_inb_coparticipation_twomode", "FCTC INB co-participation",
  # "adjmat_border", "Country Borders",
  # "adjmat_gl_posts", "GlobalLink Posts"
  )

# Listing models
# Models are lists that hold the following values:
# - vars: List of the variables that will be used as predictors.
# - filter (optional): A function that will be applied prior to run the model.
# - about (optional): A brief description
models <- list(
  # Baseline              = list(
  #   vars = c("rho", common_covars),
  #   about  = "This is the baseline specification."),
  Imp2010               = list(
    vars = c("rho", "y_lagged", common_covars),
    about  = "This specification includes the lagged number of items reported."),
  Imp2010_report        = list(
    vars   = c("rho", "y_lagged", common_covars),
    filter = function(x) {
      subset(x, no_report == 0L)
    },
    about  = "This specification includes the lagged number of items reported. Also, it only includes members that provided a reported on 2012."),
  # Imp2010_dummy_report  = list(
  #   vars   = c("rho", "y_lagged", "no_report", common_covars),
  #   about  = "This specification includes a dummy equal to 1 when the member did not provided a report on 2012."),
  # PolShift              = list(
  #   vars   = c("rho", "y_lagged", "pol_shift", common_covars),
  #   about  = "This specification includes a variable capturing political shifts."),
  # Bloomberg_amount      = list(
  #   vars   = c("rho", "y_lagged", "bloomberg_amount_pp", common_covars),
  #   about  = "This specification includes 'percapita amount of Bloomberg funds'."),
  # Bloomberg_count       = list(
  #   vars   = c("rho", "y_lagged", "bloomberg_count", common_covars),
  #   about  = "This specification includes 'Number of Bloomberg project'."),
  # Bloomberg_amount_fctc = list(
  #   vars   = c("rho", "y_lagged", "bloomberg_fctc_amount_pp", common_covars),
  #   about  = "This specification includes 'percapita amount of FCTC Bloomberg funds'."),
  # Bloomberg_count_fctc  = list(
  #   vars   = c("rho", "y_lagged", "bloomberg_fctc_count", common_covars),
  #   about  = "This specification includes 'Number of FCTC Bloomberg project'."),
  Full     = list(
    vars   = c("rho", "y_lagged", "bloomberg_fctc_count", "pol_shift", common_covars),
    about  = "This specification includes Bloomberg FCTC count, Political Shifts, and a lagged number of items implemented.",
    filter = function(x) {
      subset(x, no_report == 0L)
    }
  )
)

# If i referred j, then i has an influence over j, hence we transpose to compute
# exposures.
networks      <- matrix(networks, byrow = TRUE, ncol=2)
asterisks     <- c(.05, .01, .001)

# The general model is
# Y = rho W Y + X B + eps, where 
# Y: number of articles implemented
# X: party attributes

# Creating the lagged exposure -------------------------------------------------

# Reading the model data
read_data <- function(fn) {
  
  # Reading the data in
  x <- suppressMessages({readr::read_csv(fn)})
  x <- x %>%
    mutate(
      entry = if_else(is.na(entry), "NA", entry)
    )
  
  x %>%
    arrange(entry, year) %>%
    group_by(entry) %>%
    filter(year >= 2012, n() >= 3) %>%
    ungroup %>%
    as.data.frame
}

# Distance network -------------------------------------------------------------

for (data_path in list.files("data/", pattern="multiple.+[0-9][.]csv", full.names = TRUE)) {
  
  # Reading the data
  model_data <- read_data(data_path)
  localenvir <- new.env()
  
  local(
    {
    # Checking which covariates are included
    which(!(gsub("`", "", common_covars) %in% colnames(model_data)))
    
    SIGNIFICANCE_MODEL1 <- NULL
    for (Wnum in 1:nrow(networks)) {
      
      # Picking the network
      Wname <- networks[Wnum, 1]
      
      # This table will store the values (and add asterisks at the end) on the values
      # of rho and the sifnificance level.
      rho_asterisk_table <- NULL
      rho_per_article    <- vector("numeric", length(articles))
      names(rho_per_article) <- articles
      
      # Looping through the models
      for (m in seq_along(models)) {
        
        for (art in articles) {
          
          # Extracting lagged variable
          art_lagged <- sprintf("%s_lagged", art)
          model_data[["y_lagged"]] <- model_data[[art_lagged]] %>%
            as.vector
          
          # Extracting exposure variable
          art_exp <- paste0(gsub("sum_", "", art), "exp_", gsub("adjmat_", "", Wname))
          model_data[["rho"]] <- model_data[[art_exp]] %>% as.vector
    
          # Write down the model
          mod <- as.formula(paste0(art, " ~ ", makeformula(models[[m]]$vars)))
          
          # Filtering the data (if needed)
          if (length(models[[m]]$filter))
            tmpdata <- models[[m]]$filter(model_data)
          else tmpdata <- model_data
          
          # Run the model
          ans <<- tryCatch(
            AER::tobit(mod, data=tmpdata),
            # Run the model
            # stats::glm(mod, data=tmpdata, family = poisson()),
            error   = function(e) e,
            warning = function(w) structure(w, class="warning")
            )
        
          # Did it run?  
          if (inherits(ans, "error")) {
            message("!!! Error in network ", Wname, " article ", art, " model.")
            next
          }
          
          if (inherits(ans, "warning")) {
            message("!!! WARNING in network ", Wname, " article ", art, " model.")
            next
          }
            
          
          # Creating the object
          assign(paste("tobit_lagged",art, m, sep="_"), ans, envir = localenvir)
          message("Network ", Wname, " article ", art, " model ", m,  " done.")
        }
        
      }
      
      # Saving results -----------------------------------------------------------
      save(
        Wname, list = ls(pattern = "tobit_lagged_sum.+[0-9]$", envir = localenvir),
        file = sprintf(
          "models/tobit_lagged_%s-imputed%02i.rda",
          Wname,
          as.integer(stringr::str_extract(data_path, "[0-9]+(?=[.csv])"))
          ),
        compression_level = 9
        )
      
      message("Network ", Wname, " done.")
    }
  },
  envir = localenvir
  )
}

# Saving the models specifications
save(models, networks, articles,
     file = sprintf("models/tobit_lagged_specifications.rda"))