#visualization of significant rules
library(tidyverse)
library(tidytext)
library(tm)

data_post <- data.frame(read.csv("sig_rules.csv"))

frequency_dataframe2 = data_post %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(frequency_dataframe2, 20)

ggplot(short_dataframe2, aes(x = RHS, y = n, fill = RHS)) + geom_col() 
removal_words <- short_dataframe2$RHS[1:6]

data_post2 = data_post[-which(data_post$RHS == removal_words),]
frequency_dataframe3 = data_post2 %>% count(RHS) %>% arrange(desc(n))
short_dataframe3 = head(frequency_dataframe2, 20)
which(data_post$RHS == "{network}")
