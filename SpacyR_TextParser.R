#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

source("PubMed_Extractor.R")

my_query <- 'BPA and Derivatives AND "2013"[PDAT]:"2023"[PDAT]'

t.start <- Sys.time()
raw_pubmed_df <- Abstract_Extractor(my_query)
t.end <- Sys.time()
print(t.end - t.start)
#raw_pubmed_df$abstract

library(spacyr)

#C:\Users\Chris\.virtualenvs\Python-FsgYzr6a>C:\Users\Chris\.virtualenvs\Python-FsgYzr6a\Scripts\activate.bat
spacy_download_langmodel(model = "en_core_web_trf")
spacy_initialize(model = "en_core_web_sm", virtualenv = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a")
papers_3 <- spacy_parse(raw_pubmed_df$abstract)
#spacy_install(conda = 'auto' , version = 'latest', lang_models = 'en-core_web_sm', python_version = '3.6')
#DISCLAIMER: Before you can initialize Spacy in R, you must have spacy installed for Python in your computer
#I did it through conda

NOUN_POS <- which(papers_3$pos == "NOUN")
VERB_POS <- which(papers_3$pos == "VERB")
ADJ_POS <- which(papers_3$pos == "ADJ")
STUDY <- which(papers_3$lemma == "study")
EFFECT <- which(papers_3$lemma == "effect")
REMOVE <- c(STUDY,EFFECT)
master_pos <- c(NOUN_POS,VERB_POS,ADJ_POS)
master_pos <- master_pos[-REMOVE]

papers_3 = papers_3[master_pos,]

papers_3
write.csv(papers_3, file = "data.csv")



