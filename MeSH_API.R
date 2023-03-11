# Webscraping MeSH Study #

# Objective: MeSH classifications for most of every 

# E-utility URL (Entrez)

#https://eutils.ncbi.nlm.nih.gov/entrez/eutils/

      #Why is this valuable? Contrast the API approach to pure web scraping. When a programmer scrapes 
      #a web page, they receive the data in a messy chunk of HTML. While there are certainly libraries 
      #out there that make parsing HTML text easy, these are all cleaning steps that need to be taken 
      #before we even get our hands on the data we want!
      #source:
      #https://www.dataquest.io/blog/r-api-tutorial/

#MeSH has an API, no reason to webscrape. 
      #this script uses the MeSH RDF API through SPARQL which is an RDF query language
      library(httr)
      library(jsonlite)
      library(RecordLinkage)

rules <- read.csv("sig_rules.csv")
unique_rules <- unique(rules$RHS)
      #removes bracketing {RHS}
      for(i in 1:length(unique_rules)){
            word <- toString(unique_rules[i])
            penul <- nchar(unique_rules[i])-1
            unique_rules[i] <- substr(word,2,penul)
      }
#removes problematic consequents that shouldnt exist anyways
unique_rules <- unique_rules[-which(unique_rules == "%")]
unique_rules <- unique_rules[-which(unique_rules == "=")]


test <- MeSHer_class(unique_rules)
MeSHer_class <- function(word_list) {

#this unique token must be modified for each unique RHS
getsparq_left <- "https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20%20SELECT%20%3Fd%20%3FdName%20%3Fc%20%3FcName%20%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20%20%20WHERE%20%7B%20%20%20%20%20%20%3Fd%20a%20meshv%3ADescriptor%20.%20%20%3Fd%20meshv%3Aactive%201%20.%20%20%3Fd%20meshv%3Aconcept%20%3Fc%20.%20%20%3Fd%20rdfs%3Alabel%20%3FdName%20.%20%20%3Fc%20rdfs%3Alabel%20%3FcName%20%20FILTER(REGEX(%3FdName%2C%22"
getsparq_right1 <- "%22%2C%22i%22)%20%7C%7C%20REGEX(%3FcName%2C%22"
get_sqarq_right2 <-"%22%2C%22i%22))%20%20%20%20%7D%20%20ORDER%20BY%20%3Fd&format=JSON&inference=true&offset=0&limit=50"


MeSH_Ancestry <- c()
MeSH_tree <- c()
query_pull <- c()
coord_matches <- 0

#scan all the rules
for(i in 1:length(word_list)){
      print(i)
      #clean problematic characters
      unique_rules[i] <- gsub(')','',unique_rules[i])
      
      #create the SQL query that returns the links for possible RDF id matches
      SQL_token_query_modifier <- paste(getsparq_left,unique_rules[i],getsparq_right1,unique_rules[i],get_sqarq_right2, sep = "")
     
      #get the list and convert it
      tokenID_Finder <- GET(SQL_token_query_modifier)
      tokenID_from_rdf <-fromJSON(rawToChar(tokenID_Finder$content))
      
      #preparing for matching
      unique_rules[i] <- paste(toupper(substr(unique_rules[i], 1, 1)), substr(unique_rules[i], 2, nchar(unique_rules[i])), sep="")
      print(paste("Rule:",unique_rules[i]))
      
      #check if any of the items are identical to the rule's string
      coord <- which(tokenID_from_rdf$results$bindings$cName$value == unique_rules[i])
      
      #if its not identical
      if(length(coord) == 0) {
            
            word_match <- c()
            #check every item in the list and if null gets returned immediately check the next item
            for(j in 1:length(tokenID_from_rdf$results$bindings$cName)){
                  if(is.null(tokenID_from_rdf$results$bindings$cName[j])==TRUE){
                        next
                  }
                  
                  word_match <- c(word_match,levenshteinSim(unique_rules[i],tokenID_from_rdf$results$bindings$cName[j]))
            }
            
            if(max(word_match==-Inf)){
                  next
                  } else {
                        raw_link <- tokenID_from_rdf$results$bindings$d$value[which.max(word_match)]
                        query_pull <- tokenID_from_rdf$results$bindings$cName$value[which.max(word_match)]
                  }
            
      } else {
         
            raw_link <- tokenID_from_rdf$results$bindings$d$value[coord]
            coord_matches <- coord_matches +1
      }
      query_pull<- c(query_pull, tokenID_from_rdf$results$bindings$d$value[1])
      
      RDF_ID <-substr(raw_link,28,nchar(raw_link))
      #make a list of RDF_IDs that correspond to each rule
      #RDF_ID <- "D005138"
#apply to lower, 

#this query must be modified for each unique Token

ancestry_L <- "https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20SELECT%20%3FtreeNum%20%3FancestorTreeNum%20%3Fancestor%20%3Falabel%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20WHERE%20%7B%20%20%20%20mesh%3A"
ancestry_R <- "%20meshv%3AtreeNumber%20%3FtreeNum%20.%20%20%20%20%3FtreeNum%20meshv%3AparentTreeNumber%2B%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20meshv%3AtreeNumber%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20rdfs%3Alabel%20%3Falabel%20%7D&format=JSON&inference=true&offset=0&limit=40"
ancestry_Query <- paste(ancestry_L,RDF_ID,ancestry_R, sep = '')
ancestry_Finder <- GET(ancestry_Query)
#ancestry_Finder <- GET("https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20SELECT%20%3FtreeNum%20%3FancestorTreeNum%20%3Fancestor%20%3Falabel%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20WHERE%20%7B%20%20%20%20mesh%3AD015033%20meshv%3AtreeNumber%20%3FtreeNum%20.%20%20%20%20%3FtreeNum%20meshv%3AparentTreeNumber%2B%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20meshv%3AtreeNumber%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20rdfs%3Alabel%20%3Falabel%20%7D%20%20ORDER%20BY%20%3FtreeNum%20%3FancestorTreeNum&format=JSON&inference=true&offset=0&limit=50")
nchar("http://id.nlm.nih.gov/mesh")
ancestry_Finder
data <- fromJSON(rawToChar(ancestry_Finder$content))

#Extraction of MeSH RDF API data
d1 <-substr(data$results$bindings$ancestorTreeNum$value[1],28,nchar(data$results$bindings$ancestorTreeNum$value[1]))
if(is_empty(d1)==TRUE){
      MeSH_tree <- c(MeSH_tree,"X")
}

d1 <- substr(d1,1,1)
c1 <- data$results$bindings$alabel$value[1]
if(is_empty(c1)==TRUE){
      MeSH_Ancestry <- c(MeSH_tree,"X")
}
print(paste("ID:",d1))
print(paste("Classification:", c1))

MeSH_tree <- c(MeSH_tree,d1)
MeSH_Ancestry <- c(MeSH_Ancestry,c1)
print(paste(100*i/length(unique_rules),"%"))
      }
      return(MeSH_Ancestry)
      }



