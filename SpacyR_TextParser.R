#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

source("PubMed_Extractor.R")

my_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2013"[PDAT]:"2023"[PDAT]'
raw_pubmed_df <- Abstract_Extractor(my_query)
raw_pubmed_df$abstract

library(spacyr)

#C:\Users\Chris\.virtualenvs\Python-FsgYzr6a>C:\Users\Chris\.virtualenvs\Python-FsgYzr6a\Scripts\activate.bat
spacy_download_langmodel(model = "en_core_web_trf", virtualenv_root = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a")
spacy_initialize(model = "en_core_web_sm", virtualenv = "C:\\Users\\Chris\\.virtualenvs\\Python-FsgYzr6a")
papers_3 <- spacy_parse(raw_pubmed_df$abstract[9:11])
#spacy_install(conda = 'auto' , version = 'latest', lang_models = 'en-core_web_sm', python_version = '3.6')
#DISCLAIMER: Before you can initialize Spacy in R, you must have spacy installed for Python in your computer
#I did it through conda

#update

t.start <- Sys.time()
parsedtxt <- spacy_parse(txt)
t.end <- Sys.time()
print(t.end - t.start)



