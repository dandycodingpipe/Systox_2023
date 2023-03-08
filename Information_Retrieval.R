#     Information Retrieval   #

# dependencies: 
      # databases - easyPubMed, europepmc
      
      # other dependencies - 
            library(dplyr)
            library(kableExtra)


      #attributes:
            
            query <- "my pubmed search query"
            how_many_articles <- 500
            database <- c("pubmed","pmc")
            
info_retrieval <- function(query, how_many_articles, database){    
      
      if(database == "pubmed"){
      
            # PubMed
            # benefits: global standard but limited to (n = 500-1500) extractions
            
            # code:
                  library(easyPubMed)
                  retrieved_info <- pubmed_retrieval(query = query, retmax = how_many_articles)
            
            
      }else if(database == "pmc"){
            # Europe PMC
            # benefits: more data can be extracted (n = 20,000)
            # code:
                  library(europepmc)
            
                  #strict PMC condition
                        print("Would you like to conduct a strictly Europe PMC search?")
                        strictPMC <- input("Y/N:")
                        
                  # start if statement
                        if(strictPMC == 'Y' || 'y'){
                              #much a much larger sample size is required for strict PMC searches
                              #because not all of the entries in Europe PMC are PMC entries
                              how_many_articles = how_many_articles*5
                              retrieved_info <- europepmc_retrieval(query = query, retmax = how_many_articles)
                        }else{
              
                              retrieved_info <- europepmc_retrieval(query = query, retmax = how_many_articles)
                        }
                        # end if statement
            
      }else{
            print("Error: database must equal pubmed or pmc") 
      }#end if-'else if'-else loop
      
      return(retrieved_info)
}

# pubmed search and retrieve function
pubmed_retrieval <- function(query, retmax){
      #1 PMID Retrieval
      
      PMIDs <- get_pubmed_ids(query)
      print(paste(PMIDs$Count, "PMIDs retrieved. Fetching article information..."))
      
      #2 XML Format Record Download w/ PMID list
      articleInfo <- fetch_pubmed_data(PMIDs, retmax = retmax)
      
      #4 Convert XML to String
      #(this is to convert the content of each PubMed record to a character-class object)
      xmlToString <- articles_to_list(articleInfo)
      print(paste(length(xmlToString), "abstracts were retrieved. Creating output dataframe... (this may take a while)"))
      
      #5 Dataframe Retrieval
      stringToDF <- do.call(rbind,lapply(xmlToString, article_to_df, max_chars = -1, getAuthors = FALSE))
      return(stringToDF)
}
# pmc search and retrieve function
europepmc_retrieval <- function(query, retmax){
      
      #1 PMID/PMC Retrieval
      
      #2 
      
      stringToDF <- do.call(rbind, lapply(articleIn, article_to_df,max_chars = -1, getAuthors = FALSE))
      return(stringToDF)
}
