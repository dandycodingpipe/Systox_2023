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
rules <- apriori(trans, parameter = list(supp = 0.1, conf =.7, target = 'rules'))
t.end <- Sys.time()
print(t.end - t.start)

inspect(sort(rules, by = "lift")[100:150])
str(rules)
inspect()
table <- is.significant(rules, trans, method = "Fisher", alpha = 0.001, adjust = "fdr")
trues <- which(table == TRUE)
inspect(sort(rules, by = "rhs"))[trues,]

confint(rules, "table", level = 0.95, side = "two.sided")

rhs(rules)
length(rules)
