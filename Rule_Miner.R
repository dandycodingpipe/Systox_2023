library(arules)

source("PubMed_Extractor.R")
source("SpacyR_TextParser.R")

my_query <- '((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980/01/01:2000/01/01[dp]'
#Corpus Extraction
raw_pubmed_df <- Abstract_Extractor(my_query, 1000)

#raw_pubmed_df <- Abstract_Trimmer(raw_pubmed_df, 0.2)

model = "en_core_web_sm"; venv = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a"
raw_NLP_df <- Text_Parser(raw_pubmed_df,venv, model, 0.2)




( trans <- as(split(raw_NLP_df$lemma, raw_NLP_df$doc_id), "transactions") )
# transactions in sparse format with
#  2 transactions (rows) and
#  6 items (columns)

inspect(trans)
#   items                    transactionID
# 1 {bread,butter,eggs,milk} A123         
# 2 {meat,milk,peas}         B456 

ARM <- function(data, min_supp, min_conf, min_p ){
  
  #rule generation
  print("Converting rules into item/transaction format...")
  txns <- as(split(data$lemma, data$doc_id), "transactions")
  
  print("Initiating apriori algorithm...")
  rules <- apriori(trans, parameter = list(supp = min_supp, conf = min_conf, target = 'rules'))
  
  #statistical filtering
  print("Removing rules that do not meet p-value thresholds and type-1 errors...")
  table <- is.significant(rules, trans, method = "Fisher", alpha = min_p, adjust = "fdr")
  falses <- which(table == FALSE)
  rules <- rules[-falses,]
  
  print("Formatting rules into R-data frame...")
  df_rules <- DATAFRAME(rules)
  df_rules <- df_rules[-which(df_rules$RHS == "{-}"),]
  print("Done!")
  return(df_rules)
}


t.start <- Sys.time()
rules <- apriori(trans, parameter = list(supp = 0.01, conf =.7, target = 'rules'))
t.end <- Sys.time()
print(t.end - t.start)


table <- is.significant(rules, trans, method = "Fisher", alpha = 0.05, adjust = "fdr")
falses <- which(table == FALSE)
rules <- rules[-falses,]

#removing rules which do not meet the P = 1E-4 requirements



df_rules <- DATAFRAME(rules)
filt_rules <- which(df_rules$lift <= 2)
df_rules <- df_rules[-filt_rules,]
write.csv(df_rules, file = "sig_rules.csv")

#removing significant rules that do not meet the lift requirements

wordsearch <- df_rules[which(df_rules$RHS == "{protein}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{right}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{reserve}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{article}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{enrichement}"),]

which(df_rules$RHS == "{%}")
freq = df_rules %>% count(RHS) %>% arrange(desc(n))





