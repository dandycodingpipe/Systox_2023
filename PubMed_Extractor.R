#Goal for today: create an effective data frame (PMID, Year, Abstract)
#of specific PubMed Entrez queries

library(easyPubMed)
library(dplyr)
library(kableExtra)

#important packages for the organization



#1) R query (you must learn more Entrez/Query synatx to optimize your search)

my_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2013"[PDAT]:"2023"[PDAT]'
my_query <- get_pubmed_ids(my_query)

#Fetch Data

my_abstracts_xml <- fetch_pubmed_data(my_query)

#Consolidating Abstracts into a list
all_xml <- articles_to_list(my_abstracts_xml)

#end pubmed query data preparation

t.start <- Sys.time()

final_df <- do.call(rbind, lapply(all_xml, article_to_df,max_chars = -1, getAuthors = FALSE))

t.end <- Sys.time()
print(t.end - t.start)

#this function outputs in HTML a chouette table

final_df[,c("pmid", "year", "abstract")] %>% kable() %>% kable_styling(bootstrap_options = 'striped')

