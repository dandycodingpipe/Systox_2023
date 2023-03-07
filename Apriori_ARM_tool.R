library(arules)


ARM <- function(data, min_supp, min_conf, min_p ){
  
  #rule generation
  print("Converting pasrsed abstracts into item/transaction format...")
  txns <- as(split(data$lemma, data$doc_id), "transactions")
  
  print("Initiating apriori algorithm...")
  rules <- apriori(txns, parameter = list(supp = min_supp, conf = min_conf, target = 'rules'))
  
  #statistical filtering
  print("Removing rules that do not meet p-value thresholds and type-1 errors...")
  table <- is.significant(rules, txns, method = "Fisher", alpha = min_p, adjust = "fdr")
  falses <- which(table == FALSE)
  rules <- rules[-falses,]
  
  print("Formatting rules into R-data frame...")
  df_rules <- DATAFRAME(rules)
  df_rules <- df_rules[-which(df_rules$RHS == "{-}"),]
  print("Done!")
  return(df_rules)
}





