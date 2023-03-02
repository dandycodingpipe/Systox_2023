#dependencies
library(easyPubMed)
library(dplyr)
library(kableExtra)

#function
Abstract_Extractor <- function(Query_String, Sample_Thresh) {
  
  t.start <- Sys.time()
  
  #1) PMID retrieval from query
  query <- Query_String
  my_query <- get_pubmed_ids(query)
  print(paste(my_query$Count, "PMIDs retrieved. Fetching article information..."))
  
  #2) XML Retrieval and Reorganizing
  sample_max <- Sample_Thresh
  my_abstracts_xml <- fetch_pubmed_data(my_query, retmax = sample_max)
  all_xml <- articles_to_list(my_abstracts_xml)
  print(paste(length(all_xml), "abstracts were retrieved. Creating output dataframe... (this may take a while)"))
  
  #3) XML Conversion to data frame
  final_df <- do.call(rbind, lapply(all_xml, article_to_df,max_chars = -1, getAuthors = FALSE))
  print("Extraction Complete!")
  t.end <- Sys.time()
  print(t.end - t.start)
  
  return(final_df)
}

#HTML Formatting (reference)
#final_df[,c("pmid", "year", "abstract")] %>% kable() %>% kable_styling(bootstrap_options = 'striped')