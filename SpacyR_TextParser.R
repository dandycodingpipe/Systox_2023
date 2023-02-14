#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

source("PubMed_Extractor.R")

my_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2013"[PDAT]:"2023"[PDAT]'
raw_pubmed_df <- Abstract_Extractor(my_query)
raw_pubmed_df$abstract

install.packages('spacyr')
library(spacyr)
spacy_install(lang_models = 'en_core_web_sm')

#spacyR comes from python and its incredibly useful for condensing all of the NLP info
#you need into one data frame!!!!!

spacy_initialize(model = 'en_core_web_sm')
spacy_download_langmodel(model = 'en')

#DISCLAIMER: Before you can initialize Spacy in R, you must have spacy installed for Python in your computer
#I did it through conda

txt <- raw_pubmed_df$abstract[1]

t.start <- Sys.time()
parsedtxt <- spacy_parse(txt)
t.end <- Sys.time()
print(t.end - t.start)



