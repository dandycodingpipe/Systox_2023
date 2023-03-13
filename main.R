#visualization of significant rules
library(tidyverse)
library(tidytext)
library(tm)
library(arules)
library(arulesViz)


source("Information_Retrieval.R")
source("NLP.R")
source("Apriori_ARM_tool.R")

# Step 1: Corpus Retrieval and Extraction
#myQuery <- "((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980:2023[dp]"

#myQuery <- "hip dysplasia AND (Canis familiaris OR canis lupus familiaris OR dog OR canine)"
myQuery <- "DNA polymerase AND (damage or repair)"
myQuery <- "(dry scalp OR dandruff) AND (psoriasis OR treatment)"

raw_pub_info <- info_retrieval(myQuery, 5000, "pmc")

#Step 2: Natural Language Processing
parsed_abstracts <- Text_Parser(raw_pub_info, venv_path = "C:\\Users\\Chris\\venv\\mar6", 
            lang_model = "en_core_web_sm", 0.2)

#spacy_initialize(model="en_core_web_sm", virtualenv = "/Users/Notebook/mar7" )

#Step 3: Mining Association Rules (this includes statistical filtering)

#rules that account for at least 1% of the data (generally finds interesting rules)
rules <- ARM(parsed_abstracts, min_supp = 0.01, min_conf = 0.5, min_p = 0.005)

#rules that account for 10% of the data (generally finds trivia)
rules <- ARM(parsed_abstracts, min_supp = 0.1, min_conf = 0.75, min_p = 0.005)

#lift filtering
copy_rules <- rules
filt_rules <- which(copy_rules$lift <= 2)
copy_rules <- copy_rules[-filt_rules,]
write.csv(copy_rules, file = "sig_rules.csv")

#thematic filtering
source("MeSH_API.R")
classed <- MeSHer_class(copy_rules)
cats <- MeSH_filter(classed)

#A for Anatomy, B for Organisms, C for Diseases, D for Chemicals and Drugs, E for Technology
Prinicipal_RHS <- cats[which(cats$MeSH_tree == "E"),]

#Other
Other_RHS <- cats[which(cats$MeSH_tree != "A" & cats$MeSH_tree !="B" & cats$MeSH_tree != "C" & cats$MeSH_tree != "D" & cats$MeSH_tree !="E"),]
cats[which(cats$MeSH_tree != c("A","B","C","D","E")),]

copy_rules$RHS[Anatomy_RHS$V1]
Anatomy <- copy_rules[which(copy_rules$RHS == Anatomy_RHS$V1),]
freq = Theme %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(freq, 25)

ggplot(short_dataframe2[10:25,], aes(x = RHS, y = n, fill = RHS)) + geom_col() 

plot(freq, method = 'graph', measure = 'n', shading = 'n')


wordsearch <- rules[which(rules$RHS == "{improvement}"),]
lhs_search <- rules[which(rules$LHS == "{symptom,dandruff, satisfaction}"),]

#wordsearcher fxn
wordsearch <- rules[which(rules$RHS == "{glomerulonephritis}"),]



#visualisation
#you cant just make bargraphs for rules ans frequency. it has to be lift support variate
#i want a technology rules over time graph

#i want a toxicological associatoons for PFAS

#i want a medical disease prognosis condition guideline proposal
freq = rules %>% count(RHS) %>% arrange(desc(n))
short_dataframe2 = head(freq, 25)

ggplot(short_dataframe2[10:25,], aes(x = RHS, y = n, fill = RHS)) + geom_col() 

ggplot(freq[1:12,] ,aes(x = RHS, y = n, fill = RHS)) + geom_col() + ggtitle(myQuery)

removal_words <- short_dataframe2$RHS[1:7]
ggplot(removal_words, aes(x = RHS, y = n, fill = RHS)) + geom_col()

for(i in 1:7){
  data_post <- data_post[-which(data_post$RHS == removal_words[i]),]
}

interesting_rules <- c(783949,1242305,242779,2925,1268339,1448828,2933776,1448844)
rules[interesting_rules,]
rules[783949,]
