#Objective: To generate a clean list of tokens applicable to the Apriori algorithm

# Part I) publication information retrieval

library(spacyr)
library(tidytext)
library(tidyverse)



Abstract_Trimmer <- function(raw_data, fraction){
  

  NAs <- which(is.na(raw_data$abstract))
  raw_data <- raw_data[-NAs,]
      
  for(i in 1:length(raw_data$abstract)){
      #reduced search
      total <- nchar(raw_data$abstract[i])
      new_length <- total - round((fraction*total), digits = 0)
      new_length <- as.integer(new_length)
  
      raw_data$abstract[i] <- str_trunc(raw_data$abstract[i], new_length, 'left')
  }
  return(raw_data)
}

Text_Parser <- function(data, venv_path, lang_model, reduced_search){
      
      #abstract search reduction
      Abstract_Trimmer(data, reduced_search)
      print(paste("1) search Reduction. removing",reduced_search*100,"% of abstract head..."))
      
      #spacy environment creation and text parsing
      spacy_initialize(model = lang_model, virtualenv = venv_path )
      print("2) spaCy initialized! parsing the abstracts...")
      parsed_text <- spacy_parse(data$abstract, dependency = TRUE)
  
      #negation modifier filtering
      source('NLP_negation_study.R')
      parsed_text <- rm_negation(parsed_text)
      print('3) Sentences containing negation modifiers removed...')
  
      print('4) Filtering Parts-of-Speech...')
  
  
      #POS filtering and data cleaning
      NOUN_POS <- which(parsed_text$pos == "NOUN")
      VERB_POS <- which(parsed_text$pos == "VERB")
      ADJ_POS <- which(parsed_text$pos == "ADJ")
  
      MASTER_POS <- c(NOUN_POS,VERB_POS,ADJ_POS)
      parsed_text <- parsed_text[MASTER_POS,]
      print(paste("filtered ",length(parsed_text$lemma),"potentially relevant lemma."))
  
      #dont really know what im doing with this
      
  #item frequency pre-ARM
  word_frequency <- parsed_text %>% count(lemma) %>%arrange(desc(n))
  top20words <- head(word_frequency,20)
  ggplot(top20words, aes(x = lemma, y = n, fill = lemma)) + geom_col() + ggtitle("Top 20 Words in Query")
  #for some reason this is not working
  spacy_finalize()
  print('natural language processing complete!')
  
  return(parsed_text)
}






