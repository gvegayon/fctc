rm(list = ls())

options(stringsAsFactors = FALSE)

model_data <- read.table("data/model_data.csv", sep=",", na.strings = "<NA>",
                         header = TRUE)

articles <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13") 

# New variables: Sum all
model_data[,"sum_all"] <- rowSums(as.matrix(model_data[,articles]))
articles <- c(articles, "sum_all")

# Average Number of items ------------------------------------------------------
means2010 <- NULL
means2012 <- NULL
for(a in articles) {
  means2010 <- c(means2010, mean(model_data[model_data$year == 2010, a]))
  means2012 <- c(means2012, mean(model_data[model_data$year == 2012, a]))
}
  

# Number of articles per year --------------------------------------------------
counts2010 <- matrix(0, nrow=length(articles), ncol=6,
                     dimnames = list(
                       articles, c(0:4, "5 or more")
                     ))
counts2012 <- counts2010

# Tabulating 
for (a in articles) {
  model_data[[a]][model_data[[a]] >= 5] <- "5 or more" 
  
  ans <- table(model_data[model_data$year == 2010, a])
  counts2010[a,names(ans)] <- ans
  
  ans <- table(model_data[model_data$year == 2012, a])
  counts2012[a,names(ans)] <- ans
}

# Preparing output -------------------------------------------------------------

# As percentages and appending the means
counts2010 <- counts2010/rowSums(counts2010)
counts2010 <- cbind(counts2010, "Avg. #"=means2010)
counts2010[] <- sprintf("%.2f",counts2010[])

counts2012 <- counts2012/rowSums(counts2012)
counts2012 <- cbind(counts2012, "Avg. #"=means2012)
counts2012[] <- sprintf("%.2f",counts2012[])


# Fixing names
rownames(counts2010) <- c(
  gsub("sum_art0?", "Art. ", rownames(counts2010)[-6]),
  "Total"
  )

rownames(counts2012) <- c(
  gsub("sum_art0?", "Art. ", rownames(counts2012)[-6]),
  "Total"
  )
       

# Writing the file -------------------------------------------------------------
fn <- "tables/dist_num_articles_implemented.csv"
cat("\"2010\",", file = fn, sep = "", append = FALSE)
write.table(counts2010, file = fn, append=TRUE, sep=",")
cat("\"2012\",", file = fn, sep = "", append=TRUE)
write.table(counts2012, file = fn, append=TRUE, sep=",")

