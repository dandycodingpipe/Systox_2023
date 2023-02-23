data <- read.csv("data.csv")
library(reshape2)
library(dplyr)
library(arules)


trans <- transactions(data$lemma)
( trans <- as(split(data$lemma, data$doc_id), "transactions") )

trans2 <- as(trans, "list")
len <- length(trans2)
trans_final <- data.frame(transactions = c(1:len), items = c(1:len))
for(i in 1:len){
  trans_final$items[i] = trans2[i] 
}
head(trans_final)

res <- aggregate(lemma~doc_id, data = data, FUN = 'list')
res2 <- unique(res[,:])
str(res)
res
head(res)

install.packages('sparklyr')


library(sparklyr)
spark_install()

sc <- spark_connect(master ='local')
res_sparky <- sdf_copy_to(sc, trans_final, overwrite = TRUE)

fpg <- ml_fpgrowth(res_sparky, items_col = "items", min_confidence = 0.02, min_support = 0.01)
ml_association_rules(fpg)
ml_freq_itemsets(fpg)

