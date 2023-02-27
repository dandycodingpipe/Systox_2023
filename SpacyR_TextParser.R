#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

source("PubMed_Extractor.R")

my_query <- 'complement system activation or complement system pathologies AND "2000"[PDAT]:"2023"[PDAT]'

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
head(papers_3)

#this is where a word frequenct counter is necessary
#removing some keywords that bloat my output!
STUDY <- which(papers_3$lemma == "study")
EFFECT <- which(papers_3$lemma == "effect")
CHEMICAL <- which(papers_3$lemma == "chemical")
STOP <- which(papers_3$lemma == "-")
STOP2 <- which(papers_3$lemma == "%")
REMOVE <- c(STUDY,EFFECT, CHEMICAL, STOP, STOP2)
papers_3 <- papers_3[-REMOVE,]
which(papers_3$lemma == "study") #should be 0

#i want to focus on nouns, verbs, and adjectives!

NOUN_POS <- which(papers_3$pos == "NOUN")
VERB_POS <- which(papers_3$pos == "VERB")
ADJ_POS <- which(papers_3$pos == "ADJ")

master_pos <- c(NOUN_POS,VERB_POS,ADJ_POS)
papers_3 <- papers_3[master_pos,]

which(papers_3$lemma == "study")

frequency_dataframe1 = papers_3 %>% count(lemma) %>% arrange(desc(n))

short_dataframe1 = head(frequency_dataframe1, 20)
ggplot(short_dataframe1, aes(x = lemma, y = n, fill = lemma)) + geom_col() 

papers_3
write.csv(papers_3, file = "data.csv")

spacy_finalize()



