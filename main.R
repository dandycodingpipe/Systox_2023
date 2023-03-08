#visualization of significant rules
library(tidyverse)
library(tidytext)
library(tm)
library(arules)
library(arulesViz)


source("PubMed_Extractor.R") 
source("SpacyR_TextParser.R")
source("Apriori_ARM_tool.R")

# Step 1: Corpus Retrieval and Extraction
#myQuery <- "((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980:2023[dp]"
#myQuery <- (Chemical OR Biochemical OR Clinical OR Medical) AND (Techniques OR Detection OR Methodology)
myQuery <- "PFAS toxicity"
raw_pub_info <- Abstract_Extractor(myQuery,1500)

#Step 2: Natural Language Processing
parsed_abstracts <- Text_Parser(raw_pub_info, venv_path = "C:\\Users\\Chris\\venv\\mar6", 
            lang_model = "en_core_web_sm", 0.1)

#Step 3: Mining Association Rules (this includes statistical filtering)
rules <- ARM(parsed_abstracts, min_supp = 0.1, min_conf = 0.5, min_p = 0.05)

#thematic filtering

#prebuilt transactions of common themes in toxicology/exposomics, medicine, and technology 



#lift filtering

filt_rules <- which(rules$lift <= 2)
rules <- rules[-filt_rules,]
write.csv(df_rules, file = "sig_rules.csv")

wordsearch <- rules[which(rules$RHS == "{sulfonate}"),]


#visualisation
#you cant just make bargraphs for rules ans frequency. it has to be lift support variate
#i want a technology rules over time graph

#i want a toxicological associatoons for PFAS

#i want a medical disease prognosis condition guideline proposal
freq = rules %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(freq, 20)

ggplot(short_dataframe2, aes(x = RHS, y = n, fill = RHS)) + geom_col() 

ggplot(freq[2:11,] ,aes(x = RHS, y = n, fill = RHS)) + geom_col()

removal_words <- short_dataframe2$RHS[1:7]

