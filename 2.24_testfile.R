library(arules)
library(rtools)

data <- read.csv("data.csv")

txns <- as(split(data$lemma, data$doc_id), "transactions")