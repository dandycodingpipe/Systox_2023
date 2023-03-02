#visualization of significant rules
library(tidyverse)
library(tidytext)
library(tm)

source("Abstract_Extractor.R") 
source("Text_Parser.R")
source("Rule_Miner.R")

# Step 1: Corpus Retrieval and Extraction
myQuery <- "((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies)"

raw_pub_info <- Abstract_Extractor(myQuery,500)

#Step 2: Natural Language Processing
parsed_abstracts <- Text_Parser(raw_pub_info, venv_path = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a", 
            lang_model = "en_core_web_sm", 0.2)

#Step 3: Mining Association Rules (this includes statistical filtering)
rules <- ARM(parsed_abstracts$lemma, min_supp = 0.01, min_conf = 0.8, min_p = 0.05)

df_rules <- DATAFRAME(rules)
filt_rules <- which(df_rules$lift <= 2)
df_rules <- df_rules[-filt_rules,]
write.csv(df_rules, file = "sig_rules.csv")

#removing significant rules that do not meet the lift requirements

wordsearch <- df_rules[which(df_rules$RHS == "{protein}"),]
freq = df_rules %>% count(RHS) %>% arrange(desc(n))
frequency_dataframe2 = data_post %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(frequency_dataframe2, 20)

ggplot(short_dataframe2, aes(x = RHS, y = n, fill = RHS)) + geom_col() 

removal_words <- short_dataframe2$RHS[1:7]

for(i in 1:7){
  data_post <- data_post[-which(data_post$RHS == removal_words[i]),]
}



