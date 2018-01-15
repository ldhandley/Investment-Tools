library(dplyr)
##Before you pass file location in here, make sure the data is "clean". Check in TextWrangler.
check_asset_mix <- function(file_location){
  ##Read in current data from Vanguard and tidy
  investments <- read.csv(file_location)
  lookuptable1 <- read.csv("./symbol_assettype_lookup.csv")
  investments <- mutate(investments,assetType=sapply(Symbol,dolookup))
  names(investments) <- gsub("\\.","",names(investments))
  names(investments) <- tolower(names(investments))
  
  #Group data by asset type and figure out current percentage investment vs. target
  asset_grouped_investments <- group_by(investments, assettype)
  summary_table <- summarize(asset_grouped_investments, sum=sum(totalvalue))
  total <- sum(summary$sum)
  summary_table <- summarize(asset_grouped_investments, sum = sum(totalvalue), percent = (sum / total))
  lookuptable2 <- read.csv("./assettype_target_lookup.csv")
  summary_table <- mutate(summary_table,target=sapply(assettype,dolookup2))
  summary_table
}

##Make a function that takes a table and returns a lookup function that uses that table
dolookup <- function(factor){
  str = as.character(factor)
  lookuptable1[as.character(lookuptable1$Symbol)==str,2]
}

dolookup2 <- function(factor){
  str = as.character(factor)
  lookuptable2[lookuptable2$AssetType == str,2]
}