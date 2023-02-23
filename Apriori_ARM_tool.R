library(arules)
library(rJava)
library(rCBA)
data <- data.frame(read.csv("data.csv"))

trans <- transactions(data$lemma)

( trans <- as(split(data$lemma, data$doc_id), "transactions") )
# transactions in sparse format with
#  2 transactions (rows) and
#  6 items (columns)
inspect(trans)
#   items                    transactionID
# 1 {bread,butter,eggs,milk} A123         
# 2 {meat,milk,peas}         B456 

1

inspect(sort(rules, by = "lift")[100:200])


