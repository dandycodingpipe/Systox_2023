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
                  
            
#1) spaCy initialization 

library(spacyr)
spacy_initialize(model="en_core_web_sm", virtualenv = "C:\\Users\\Chris\\venv\\mar6" )