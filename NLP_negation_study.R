#Natural Language Processing Study 1 (Negation modifier removal)

#Dependencies: tidyverse, textdata, spacyr, dplyr, quanteda 

#I) python virtual environment creation
    
      #in windows command prompt
            
            #python -m venv /path/to/directory

            #python -m venv \Users\Chris\venv\mar6 (remember to remove your venv when you are done)
      
      #in mac terminal

#II) virtual environment activation
            
      #for Windows:

            #path\to\venv\Scripts\activate.bat

            #\Users\Chris\venv\mar6\Scripts\activate.bat
      
      #you should see (your_env) on the left side of the screen 

      #for Mac:

#III) virtual environment installation of dependencies (spaCy, language model)
      
      #Windows:
            #In command prompt:
                  #pip install spacy
                  
                  #spacy language model download
                  
                  #python -m spacy download en_core_web or en_core_trf (if possible) --- other language models can be found in https://spacy.io/usage/models
                  
#IV) virtual environment removal (keeps your computer free of mess and can always be re-installed)
      #Windows:
            #rm /path/to/directory

            #rm \Users\Chris\venv\mar6 
            
#1) spaCy initialization 

library(spacyr)
spacy_initialize(model="en_core_web_sm", virtualenv = "C:\\Users\\Chris\\venv\\mar6" )

#1.1) retrieving easyPubMed 

source("PubMed_Extractor.R")

PubMed_Search <- Abstract_Extractor("(PFAS OR Per-fluorylalkyl substances) AND (Toxicity OR Pathology OR Pathologies)",500)

#1.2) retrieving europePMI


#1.3) raw xml input

#2) Parsing Abstracts with Dependency Labeling

Abstract_Parse <- spacy_parse(PubMed_Search$abstract, dependency = TRUE)

#3) Identifying Coordinates of Negative Modifiers

Copy_Abstract_Parse <- Abstract_Parse
Negation_Coord <- which(Copy_Abstract_Parse$dep_rel == "neg")
print(paste("there are", length(Negation_Coord), "negations in the abstracts..."))

#4) Function for Identifying 

   master_token_list <- c()
   
for(neg in 1:length(Negation_Coord)){
      
      #identifying text_id and sentence_id corresponding to the negation coordinate
      text_id <- Copy_Abstract_Parse$doc_id[Negation_Coord[neg]]
      
      sentence_id <- Copy_Abstract_Parse$sentence_id[Negation_Coord[neg]]
      
      #retrieving the coordinates for each token in the sentence containing negation
      text_id_coordinates <- which(Copy_Abstract_Parse$doc_id == text_id)
      
      sentence_id_coordinates <- which(Copy_Abstract_Parse$sentence_id[text_id_coordinates] == sentence_id)
      
      token_ids <- text_id_coordinates[sentence_id_coordinates]
      
      master_token_list <- c(master_token_list, token_ids)
      }

