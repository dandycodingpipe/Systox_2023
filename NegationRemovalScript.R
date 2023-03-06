library(tidyverse)
library(textdata)
library(spacyr)
library(dplyr)
library(quanteda)

#parser testing environment
spacy_initialize(model = lang_model, virtualenv = venv_path )
parsed_text <- spacy_parse(raw_pub_info$abstract, dependency = TRUE)
spacy_finalize()

copy_parsed <- parsed_text
negation_df <- parsed_text[which(copy_parsed$dep_rel == "neg"),]


length(negation_df$dep_rel)
count_words <- NULL
#for as many negations as there are
for(i in 1:length(negation_df$dep_rel)){
  
  #find the document id for that negation
  text_id <- negation_df$doc_id[i]
  #find the sentence id for that negation
  sentence_id <- negation_df$sentence_id[i]
  
  #find the proper location of all text_ids equal to the one im interested in
  id_location <- which(copy_parsed$doc_id == text_id)
  
  
  #use these text_id locations to identify which sentence ids = the relevant sentence id
  sentence_location <- which(copy_parsed$sentence_id[id_location] == sentence_id)
  token_location <- id_location[sentence_location]
  copy_parsed <- copy_parsed[-token_location,]
  count_words <- c(count_words,token_location)
  
  }

which(copy_parsed$dep_rel == 'neg')

