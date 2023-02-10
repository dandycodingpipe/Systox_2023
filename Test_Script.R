install.packages('easyPubMed')
library(easyPubMed)

my_query <- '6-2 FTS'
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_xml <- fetch_pubmed_data(my_entrez_id, retmax = 50, format = "xml")

my_abstracts_xml
custom_grep(my_abstracts_xml, "ArticleTitle", 'list')
#this function is what you will use to extract all the information you need from
#pubmed queries

