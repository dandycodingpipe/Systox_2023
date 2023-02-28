#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

library(spacyr)

source("PubMed_Extractor.R")

my_query <- 'complement system AND (pathology OR pathologies)'
raw_pubmed_df <- Abstract_Extractor(my_query, 1000)

#text parser creates a parsed data frame of the extracted abstracts. a path to a python virtual environment
#must be called as well as the spacy language model
Text_Parser <- function(data, venv_path, lang_model){
  
  spacy_initialize(model = lang_model, virtualenv = venv_path )
  parsed_text <- spacy_parse(raw_pubmed_df$abstract)
  return(parsed_text)
}


#C:\Users\Chris\.virtualenvs\Python-FsgYzr6a>C:\Users\Chris\.virtualenvs\Python-FsgYzr6a\Scripts\activate.bat
spacy_initialize(model = "en_core_web_sm", virtualenv = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a")
papers_3 <- spacy_parse(raw_pubmed_df$abstract, entity = TRUE)
#spacy_install(conda = 'auto' , version = 'latest', lang_models = 'en-core_web_sm', python_version = '3.6')
#DISCLAIMER: Before you can initialize Spacy in R, you must have spacy installed for Python in your computer
#I did it through conda


#this is where a word frequenct counter is necessary
#removing some keywords that bloat my output!
STUDY <- which(papers_3$lemma == "study")
EFFECT <- which(papers_3$lemma == "effect")
CHEMICAL <- which(papers_3$lemma == "chemical")
STOP <- which(papers_3$lemma == "-")
STOP2 <- which(papers_3$lemma == "%")
REMOVE <- c(STUDY,EFFECT, STOP, STOP2)
papers_3 <- papers_3[-REMOVE,]
which(papers_3$lemma == "study") #should be 0

#i want to focus on nouns, verbs, and adjectives!
concat <- nounphrase_consolidate(papers_3)

NOUN_POS <- which(concat$pos == "nounphrase")
VERB_POS <- which(concat$pos == "VERB")
ADJ_POS <- which(concat$pos == "ADJ")

master_pos <- c(NOUN_POS,VERB_POS,ADJ_POS)
concat <- concat[master_pos,]

which(concat$lemma == "study")

papers_3 <- papers_3[NOUN_POS,]
frequency_dataframe1 = papers_3 %>% count(lemma) %>% arrange(desc(n))
nounphrase_freq = concat %>% count(lemma) %>% arrange(desc(n))

short_dataframe1 = head(frequency_dataframe1, 20)
ggplot(short_dataframe1, aes(x = lemma, y = n, fill = lemma)) + geom_col() 

papers_3
write.csv(concat, file = "data.csv")

spacy_finalize()



