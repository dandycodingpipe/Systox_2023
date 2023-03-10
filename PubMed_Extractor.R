#dependencies
library(easyPubMed)
library(europepmc)
library(dplyr)
library(kableExtra)

europe_search <- epmc_search("(PFAS OR Perfluoro alkyl substances)", output = 'raw', limit = 20000)
library(tidyverse)
#extremely deep and advanced process here
dfr <- purrr::keep(europe_search, function(x) (x[['source']] == 'PMC') && !is.null(x[['abstractText']])) %>%
      map_dfr(~.x)
dfr1_1 <- unique(dfr$abstractText)
dfr2 <- purrr::keep(europe_search, function(x) (x[['source']] == 'MED') && !is.null(x[['abstractText']])) %>%
      map_dfr(~.x)

purrr::keep()
#PMC strict search
print('A strict PMC search will 5x longer than a non-PMC search. Initiating...')

PMC_coords <- c()
counter <- 0

for(i in 1:length(europe_search)) {
      
      if(europe_search[[i]][["source"]] == "PMC") {
            PMC_coords <- c(PMC_coords,i)
            print( europe_search[[i]][["pmcid"]] )
            
            counter <- counter +1
      }
}


article_to_df_epmc <- function(PMC_coords) {
      
      row <- data.frame()
      
      #PMCID
      row <- c(row,cbind(europe_search[[PMC_coords]][["pmcid"]]))
      #Title
      #row <- c(row,cbind(europe_search[[PMC_coords]][["title"]]))
      #abstractText
      row <- c(row,cbind(europe_search[[PMC_coords]][["abstractText"]]))
      #pubYear
      #row <- c(row,cbind(europe_search[[PMC_coords]][["pubYear"]]))
      
      #creating row
      if(is.null(europe_search[[PMC_coords]][["abstractText"]]) == TRUE){
            
      } else {
            return(row) 
      }
}

row <- lapply(PMC_coords,article_to_df_epmc)
Nulls <- c()
for(i in 1:length(row)){
      if(is.null(row[i])==TRUE){
            nonNulls <- c(nonNulls,i)
      }
}

row <- list2DF(row)
#function

check2 <- get_pubmed_ids('6-2 FTS')
print(check2$Count)
Abstract_Extractor <- function(Query_String, Sample_Thresh) {
  
  t.start <- Sys.time()
  
  #1) PMID retrieval from query
  query <- Query_String
  #troubleshooting query <- "((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980:2023[dp]"
  my_query <- get_pubmed_ids(query)
  print(paste(my_query$Count, "PMIDs retrieved. Fetching article information..."))
  
  #2) XML Retrieval and Reorganizing
  sample_max <- Sample_Thresh
 
  
  #there is a bug with retmax = sample_max so for the time being i am letting easyPubMed auto-retmax.
  #unfortunately that limits my sample size to 500 articles but there can be workarounds
  my_abstracts_xml <- fetch_pubmed_data(my_query, retmax = Sample_Thresh)
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