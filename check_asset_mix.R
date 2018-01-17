library(dplyr)

symbol_assettype_lookup <- read.csv("./symbol_assettype_lookup.csv")
assettype_target_lookup <- read.csv("./assettype_target_lookup.csv")

##Make a function that takes a table and returns a lookup function that uses that table
dolookup <- function(table){
  if(length(table$AssetType) > 0 && length(table$Symbol) > 0){  ##If its the symbol_assettype_lookup table
    dolookup2 <- function(factor){
      str = as.character(factor)
      symbol_assettype_lookup[as.character(symbol_assettype_lookup$Symbol)==str,2]
    }
  }
  else if(length(table$AssetType) > 0 && length(table$TargetPercentage) > 0){ ##If its the assettype_target_lookup table
    dolookup2 <- function(factor){
      str = as.character(factor)
      assettype_target_lookup[assettype_target_lookup$AssetType == str,2]
    }
  }
  else{
    print("Unknown lookup table. Please create new dolookup function here.")
  }
  
  return(dolookup2)
}



##Before you pass file location in here, make sure the data is "clean". Check in TextWrangler.
check_asset_mix <- function(file_location){
  ##Read in current data from Vanguard and find asset allocation types
  investments <- read.csv(file_location)
  investments <- mutate(investments,assetType=sapply(Symbol,dolookup(symbol_assettype_lookup)))
  
  ##Tidy up column names
  names(investments) <- gsub("\\.","",names(investments))
  names(investments) <- tolower(names(investments))
  names(investments) <- sub("totalvalue", "investmentallocation", names(investments))
  
  ##Group data by asset type and figure out current percentage investment vs. target
  asset_grouped_investments <- group_by(investments, assettype)
  summary_table <- summarize(asset_grouped_investments, currentallocation=sum(investmentallocation))
  total <- sum(summary_table$currentallocation)
  summary_table <- mutate(summary_table, currentpercent = (currentallocation / total))
  summary_table <- mutate(summary_table, target=sapply(assettype, dolookup(assettype_target_lookup)))
  
  ##Find perfect allocation and allocation difference
  summary_table <- mutate(summary_table, perfectallocation = target * total)
  summary_table <- mutate(summary_table, idealdifference = perfectallocation - currentallocation)
  
  ##Format columns appropriately
  summary_table <- mutate(summary_table, currentallocation = sprintf("$%.2f",currentallocation))
  summary_table <- mutate(summary_table, perfectallocation = sprintf("$%.2f",perfectallocation))
  summary_table <- mutate(summary_table, idealdifference = sprintf("$%.2f",idealdifference))
  summary_table <- mutate(summary_table, currentpercent = sprintf("%#.3f",currentpercent))
  summary_table <- mutate(summary_table, target = sprintf("%#.3f",target))
  summary_table
}


##Figure out how to look up share prices for stock symbol
##Recommend # of Shares to purchase to reach AllocationDifference (but less than)
##Calculate the PostPurchaseAllocation && PostPurchasePercentage