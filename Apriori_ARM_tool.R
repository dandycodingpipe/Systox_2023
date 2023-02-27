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
rules <- apriori(trans, parameter = list(supp = 0.01, conf =.5, target = 'rules'))
t.end <- Sys.time()
print(t.end - t.start)

#rules have been generated 

table <- is.significant(rules, trans, method = "Fisher", alpha = 0.05, adjust = "fdr")
falses <- which(table == FALSE)
rules <- rules[-falses,]

#removing rules which do not meet the P = 1E-4 requirements


df_rules <- DATAFRAME(rules)
filt_rules <- which(df_rules$lift <= 10)
df_rules <- df_rules[-filt_rules,]
write.csv(df_rules, file = "sig_rules.csv")

#removing significant rules that do not meet the lift requirements

df_rules <- df_rules[-which(df_rules$RHS == "{copyright}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{right}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{reserve}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{article}"),]
df_rules <- df_rules[-which(df_rules$RHS == "{enrichement}"),]

inspect(sort(rules, by = "lift"))[trues,]

confint(rules, "table", level = 0.95, side = "two.sided")

rhs(rules)
length(rules)


