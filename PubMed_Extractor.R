#This script creates a function that returns a data frame of extractable information from a publication

library(easyPubMed)
library(dplyr)
library(kableExtra)

#important packages for the organization


#my_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2013"[PDAT]:"2023"[PDAT]'
#my_query <- get_pubmed_ids(my_query)

Abstract_Extractor <- function(Query_String) {
  
  #1) PMID Retrieval of Query
  query <- Query_String
  my_query <- get_pubmed_ids(my_query)
  
  #2) XML Retrieval
  my_abstracts_xml <- fetch_pubmed_data(my_query)
  all_xml <- articles_to_list(my_abstracts_xml)
  
  #3) XML Conversion to data frame with computation time
  
  t.start <- Sys.time()
  
  final_df <- do.call(rbind, lapply(all_xml, article_to_df,max_chars = -1, getAuthors = FALSE))
  
  t.end <- Sys.time()
  print(t.end - t.start)
  
  #HTML Formatting (reference)
  #final_df[,c("pmid", "year", "abstract")] %>% kable() %>% kable_styling(bootstrap_options = 'striped')
  return(final_df[,c('pmid','year', 'abstract')])
}