# This script takes the results from the SAR Tobit models and compares the fitted values with
# observed ones on distribution. It generates the file 'fig/sar_tobit_fitted.pdf'.

rm(list = ls())

model_data <- read.csv("data/model_data.csv", na="<NA>", check.names = FALSE)
model_data <- model_data[with(model_data, order(year, entry)),]

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
files <- list.files(path = "models/", pattern = "tobit_lagged_.+.rda", full.names = TRUE)

model_data <- subset(model_data, year == 2012)

histoplot <- function(
  net,
  var,
  xlab = "",
  ylab = "",
  cols = adjustcolor(c("tomato", "gray"), alpha.f =.5),
  pch = c(16, 17),
  lgnd = NULL,
  ...
  ) {
  
  env    <- new.env()
  envols <- new.env()
  load(sprintf("models/tobit_lagged_%s.rda", net), envir = env)
  load(sprintf("models/ols_lagged_%s.rda", net), envir = envols)
  
  
  # Getting observed data
  obs <- model_data[[var]]
  
  # Getting predicted
  ans            <- env[[sprintf("tobit_lagged_%s_0", var)]]
  pred           <- ans$linear.predictors
  pred[pred < 0] <- 0
  
  ansols  <- envols[[sprintf("ols_lagged_%s_0", var)]]
  predols <- ansols$fitted.values
  # predols[predols < 0] <- 0
  
  # Drawing qqplots
  ans    <- qqplot(pred, obs, plot.it = FALSE)
  ansols <- qqplot(predols, obs, plot.it = FALSE)
  
  # ans0 <- hist(obs, plot=FALSE)
  # ans  <- hist(pred, plot=FALSE)
  # ansols <- hist(predols, plot=FALSE)
  
  # Plotting region
  plot.new()
  plot.window(xlim = range(c(ans$x, ansols$x)), ylim = range(c(ans$y, ansols$y)))
  
  # Axis
  axis(1);axis(2)
  
  with(ans,    points(x, jitter(y), col=cols[1], pch=pch[1]))
  with(ansols, points(x, jitter(y), col=cols[2], pch=pch[2]))
  abline(b=1, a=0, xpd=FALSE)
  
  
  # Writting legend number
  if (length(lgnd))
    legend("topleft", bty="n", legend = lgnd, cex = 1.5, text.font=2)
}

graphics.off()
# setEPS()
# postscript("fig/tobit_lagged_fitted.eps", width = 7.92, height = 8.42)
# pdf("fig/tobit_lagged_fitted.pdf", width = 7.92, height = 8.42)
png("fig/tobit_lagged_fitted.png", width = 600, height = 600)
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mai = c(0,0,0,0)+.2, oma = c(7, 6, 2, 1), las = 1,
    cex.main = 1.5, font.main = 1, xpd = NA, xaxs = "i")
# General Trade
histoplot("adjmat_general_trade", "sum_art11", lgnd = "General Trade")

# General Trade
histoplot("adjmat_referrals", "sum_art11", lgnd = "Referrals")

# General Trade
histoplot("adjmat_fctc_cop_coparticipation_twomode", "sum_art11", lgnd = "COP co-participation")

# General Trade
histoplot("adjmat_interest_group_comembership_twomode", "sum_art11", lgnd = "Interest Group\nco-membership")

legend("bottomright",
       legend = c("Tobit", "OLS"),
       pch    = 16:17,
       col    = c(pred="tomato", ols="gray"),
       bty    = "n", cex = 1.25,
       horiz  = TRUE
)

# Adding text
par(mfrow = c(1,1), las = oldpar$las)

mtext(side = 1, text = "Predicted", outer = TRUE, line=4)
mtext(side = 2, text = "Observed", outer = TRUE, line=4)


par(oldpar)
dev.off()
# 
# system("convert fig/tobit_lagged_fitted.pdf -resize 100% fig/tobit_lagged_fitted.png")
# Description: Fitted and observed distribution of number of items implemented per countries.
# Panel A1 and A2 show observed values of Article 11 and fitted values of the same article
# from the SAR Tobit model using the General Trade Network, like-wise, B1-B2, C1-C2, D1-D2 show
# observed values for Article 11, 5, and 11, and fitted values from SAR Tobit models using
# GlobalLink Referrals, FCTC INB co-participation, and Interest Group co-membership
# respectively. Panels in the left show such distribution on 2010, and panels on the right
# on 2012.