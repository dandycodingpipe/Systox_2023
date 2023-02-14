install.packages('easyPubMed')
library(easyPubMed)

my_query <- 'fluorotelomer sulfonic acids'
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_xml <- fetch_pubmed_data(my_entrez_id, retmax = 50, format = "xml")

my_abstracts_xml
my_abstracts <- custom_grep(my_abstracts_xml, "Abstract", 'char')
#this function is what you will use to extract all the information you need from
#pubmed queries

TTM <- nchar(my_abstracts) > 75

my_abstracts[TTM] <- paste(substr(my_abstracts[TTM],1,70),"...",sep = "")
my_abstracts[1]

#this appears to linearize the abstracts
article_to_df(my_abstracts_xml[1])
