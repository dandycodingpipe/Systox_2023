library(arules)

data <- data.frame(read.csv("data.csv"))


( trans <- as(split(data$lemma, data$doc_id), "transactions") )
# transactions in sparse format with
#  2 transactions (rows) and
#  6 items (columns)
inspect(trans)
#   items                    transactionID
# 1 {bread,butter,eggs,milk} A123         
# 2 {meat,milk,peas}         B456 

t.start <- Sys.time()
rules <- apriori(trans, parameter = list(supp = 0.01, conf =.7, target = 'rules'))
t.end <- Sys.time()
print(t.end - t.start)

inspect(sort(rules, by = "lift"))[1:20,]
#rules have been generated 

table <- is.significant(rules, trans, method = "Fisher", alpha = 0.05, adjust = "fdr")
falses <- which(table == FALSE)
rules <- rules[-falses,]

#removing rules which do not meet the P = 1E-4 requirements


df_rules <- DATAFRAME(rules)
filt_rules <- which(df_rules$lift <= 2)
df_rules <- df_rules[-filt_rules,]
write.csv(df_rules, file = "sig_rules.csv")

#removing significant rules that do not meet the lift requirements

wordsearch <- df_rules[which(df_rules$RHS == "{pathway}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{right}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{reserve}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{article}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{enrichement}"),]

which(df_rules$RHS == "{-}")
freq = df_rules %>% count(RHS) %>% arrange(desc(n))

inspect(sort(rules, by = "lift"))[trues,]

confint(rules, "table", level = 0.95, side = "two.sided")

rhs(rules)
length(rules)


