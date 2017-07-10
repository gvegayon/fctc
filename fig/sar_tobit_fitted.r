# This script takes the results from the SAR Tobit models and compares the fitted values with
# observed ones on distribution. It generates the file 'fig/sar_tobit_fitted.pdf'.

rm(list = ls())

model_data <- read.csv("data/model_data.csv", na="<NA>", check.names = FALSE)
model_data <- model_data[with(model_data, order(year, entry)),]
model_data$`year2012` <- as.integer(model_data$year == 2012)

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
files <- list.files(path = "models/", pattern = "sar_tobit_.+.rda", full.names = TRUE)


histoplot <- function(
  net,
  var,
  use2012 = TRUE,
  xlab = "",
  ylab = "",
  cols = c(obs = "blue", pred="red"),
  ltys = c(obs = 1, pred=2),
  lgnd = NULL,
  ...
  ) {
  
  env <- new.env()
  load(sprintf("models/sar_tobit_%s.rda", net), envir = env)
  
  # Getting observed data
  obs <- subset(model_data, year2012 == use2012)[[var]]
  
  # Getting predicted
  ans <- env[[sprintf("sar_tobit_%s_1", var)]]
  pred <- ans$fitted.response[ans$X[,"year2012"] == use2012]
  
  # Getting histogram points
  breaks  <- hist(c(obs, pred), plot=FALSE, breaks = 10)$breaks
  points1 <- hist(obs, plot=FALSE, breaks = breaks)
  points2 <- hist(pred, breaks = breaks, plot = FALSE)
  
  ylim <- range(c(points1$counts, points2$counts, 0))
  
  plot(points1$counts, col = cols[1], type = "b", ylim = ylim, lwd = 2,
       xlab = xlab, ylab = ylab, lty = ltys[1], ...)
  points(points2$counts, col = cols[2], type = "b", ylim = ylim, lwd = 2,
         lty = ltys[2])
  
  # Writting legend number
  if (length(lgnd))
    legend("topright", bty="n", legend = lgnd, cex = 1.5)
}

graphics.off()
setEPS()
# postscript("fig/sar_tobit_fitted.eps", width = 7.92, height = 8.42)
tiff("fig/sar_tobit_fitted.tiff", width = 761, height = 809)
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2), mai = c(0,0,0,0)+.2, oma = c(9, 6, 4, 1), las = 1,
    cex.main = 1.5, font.main = 1, xpd = NA)
# General Trade
histoplot("adjmat_general_trade", "sum_art11", use2012 = FALSE, main = "2010\n", lgnd = "A1")
histoplot("adjmat_general_trade", "sum_art11", use2012 = TRUE, main = "2012\n", lgnd = "A2")

# General Trade
histoplot("adjmat_referrals", "sum_art11", use2012 = FALSE, lgnd = "B1")
histoplot("adjmat_referrals", "sum_art11", use2012 = TRUE, lgnd = "B2")

# General Trade
histoplot("adjmat_fctc_cop_coparticipation_twomode", "sum_art05", use2012 = FALSE, lgnd = "C1")
histoplot("adjmat_fctc_cop_coparticipation_twomode", "sum_art05", use2012 = TRUE, lgnd = "C2")

# General Trade
histoplot("adjmat_interest_group_comembership_twomode", "sum_art11",use2012 = FALSE, lgnd = "D1")
histoplot("adjmat_interest_group_comembership_twomode", "sum_art11",use2012 = TRUE, lgnd = "D2")

par(mfrow = c(1,1), las = oldpar$las)

mtext(side = 1, text = "Number of items implemented", outer = TRUE, line=4)
mtext(side = 2, text = "Number of Countries", outer = TRUE, line=4)
legend(x = mean(par()$usr[1:2]), y = -25,
       legend = c("Observed", "Fitted"),
       lty    = c(1, 2),
       lwd    = 2,
       col    = c(obs = "blue", pred="red"),
       bty    = "n", xjust = .5,
       horiz  = TRUE
       )

par(oldpar)
dev.off()

system("convert fig/sar_tobit_fitted.pdf -resize 100% fig/sar_tobit_fitted.png")
# Description: Fitted and observed distribution of number of items implemented per countries.
# Panel A1 and A2 show observed values of Article 11 and fitted values of the same article
# from the SAR Tobit model using the General Trade Network, like-wise, B1-B2, C1-C2, D1-D2 show
# observed values for Article 11, 5, and 11, and fitted values from SAR Tobit models using
# GlobalLink Referrals, FCTC INB co-participation, and Interest Group co-membership
# respectively. Panels in the left show such distribution on 2010, and panels on the right
# on 2012.