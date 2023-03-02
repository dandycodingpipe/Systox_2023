#visualization of significant rules
library(tidyverse)
library(tidytext)
library(tm)

data_post <- data.frame(read.csv("sig_rules.csv"))

frequency_dataframe2 = data_post %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(frequency_dataframe2, 20)

ggplot(short_dataframe2, aes(x = RHS, y = n, fill = RHS)) + geom_col() 

removal_words <- short_dataframe2$RHS[1:7]

for(i in 1:7){
  data_post <- data_post[-which(data_post$RHS == removal_words[i]),]
}

#i should make a function that receives a list of strings "{}" in proper RHS format that removes them and returns
#the number of RHS arguments removed. it can streamline the post processing.

#at least i know that all of these rules meet criteria of significance (p <= 1E-4)
data_post <- data_post[-which(data_post$RHS == "{chinese}"),]
data_post <- data_post[-which(data_post$RHS == "{ingredient}"),]
data_post <- data_post[-which(data_post$RHS == "{docking}"),]
data_post <- data_post[-which(data_post$RHS == "{construct}"),]
data_post <- data_post[-which(data_post$RHS == "{screen}"),]

data_post[which(data_post$RHS == "{core}"),]
