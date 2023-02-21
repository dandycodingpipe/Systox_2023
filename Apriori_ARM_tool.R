library(arules)
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

t.start <- Sys.time()
rules <- apriori(trans, parameter = list(supp = 0.1, conf =.3, target = 'rules'))
t.end <- Sys.time()
print(t.end - t.start)

inspect(sort(rules, by = "lift")[100:200])


