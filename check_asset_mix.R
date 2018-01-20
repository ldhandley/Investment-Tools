library(dplyr)
library(quantmod)

##UTILITY FUNCTIONS

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
  else if(length(table$AssetType) > 0 && length(table$DefaultSymbol) > 0){ ##If its the assettype_defaultsymbol_lookup table
    dolookup2 <- function(factor){
      str = as.character(factor)
      assettype_defaultsymbol_lookup[assettype_defaultsymbol_lookup$AssetType == str,2]
    }
  }
  else{
    print("Unknown lookup table. Please create new dolookup function here.")
  }
  
  return(dolookup2)
}


##READ IN DATA

symbol_assettype_lookup <- read.csv("./symbol_assettype_lookup.csv")
assettype_target_lookup <- read.csv("./assettype_target_lookup.csv")
assettype_defaultsymbol_lookup <- read.csv("./assettype_defaultsymbol_lookup.csv")


##MAIN FUNCTION

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
  summary_table <- mutate(summary_table, currentpercent = (currentallocation / total)) %>% mutate(target=sapply(assettype, dolookup(assettype_target_lookup)))
  
  ##Find perfect allocation and allocation difference 
  summary_table <- mutate(summary_table, perfectallocation = target * total) %>% mutate(idealdifference = perfectallocation - currentallocation)
  
  
  ##Find appropriate symbols to buy or sell and how much
  buy_sell_table <- select(summary_table, assettype, idealdifference) 
  buy_sell_table <- mutate(buy_sell_table, symboltobuyorsell=sapply(assettype, dolookup(assettype_defaultsymbol_lookup)))
  buy_sell_table <- select(buy_sell_table, assettype, symboltobuyorsell, idealdifference)
    
  ##Get current share prices and format
  buy_sell_table <- mutate(buy_sell_table, currentshareprice = (getQuote(as.character(symboltobuyorsell), src = "yahoo"))$Last)
  buy_sell_table <- mutate(buy_sell_table, sharesdiff = as.numeric(idealdifference) / as.numeric(currentshareprice)) 
  buy_sell_table <- mutate(buy_sell_table, sharestobuy = ifelse(sharesdiff > 0, sharesdiff, 0))
  buy_sell_table <- mutate(buy_sell_table, sharestosell = ifelse(sharesdiff < 0, sharesdiff, 0))
  buy_sell_table <- select(buy_sell_table, -sharesdiff) 
  

  ##TIDYING
  ##Format summary_table
  summary_table <- mutate(summary_table, currentallocation = sprintf("$%.2f",currentallocation)) %>%
    mutate(perfectallocation = sprintf("$%.2f",perfectallocation)) %>% mutate(idealdifference = sprintf("$%.2f",idealdifference)) %>% 
    mutate(currentpercent = sprintf("%#.3f",currentpercent)) %>% mutate(target = sprintf("%#.3f",target))
  
  ##Format buy_sell_table
  buy_sell_table <- mutate(buy_sell_table, currentshareprice = sprintf("$%.2f",currentshareprice)) %>% mutate(idealdifference = sprintf("$%.2f",idealdifference)) %>% 
    mutate(sharestobuy = floor(sharestobuy)) %>% mutate(sharestobuy = as.integer(sharestobuy)) %>% 
    mutate(sharestosell = ceiling(sharestosell)) %>% mutate(sharestosell = as.integer(sharestosell)) 
  
  
  ##PRINTING
  print("###CURRENT INVESTMENTS: ###")
  print(investments)
  
  print("###SUMMARY TABLE BEFORE PURCHASE: ###")
  print(summary_table)
  
  print("###STUFF TO BUY OR SELL: ###")
  buy_sell_table
}

##Need to prettify everything at the VERY END instead of in the middle
##Recommend # of Shares to sell (round down)
##Recommend # of Shares to purchase to reach AllocationDifference (round down)
##Calculate the PostPurchaseAllocation && PostPurchasePercentage