#loading packages
library(reshape2)
library(dplyr)
library(arules)

#retrieving and organizing data frame

data <- read.csv("data.csv")
trans <- as(split(data$lemma, data$doc_id), "transactions")
trans2 <- as(trans, "list")
len <- length(trans2)
trans_final <- data.frame(transactions = c(1:len), items = c(1:len))

for(i in 1:len){
  trans_final$items[i] = trans2[i] 
}
head(trans_final)


#apache spark extension for R

library(sparklyr)

#environment creation
sc <- spark_connect(master ='local')
#converting data frame into spark table
res_sparky <- sdf_copy_to(sc, trans_final, overwrite = TRUE)
#model construction
fpg <- ml_fpgrowth(res_sparky, items_col = "items", min_confidence = 0.02, min_support = 0.01)
rules <- ml_association_rules(fpg)
ml_freq_itemsets(fpg)

class(rules)
dim(rules)

#closing spark connection
spark_disconnect(sc)
