# Webscraping MeSH Study #

# Objective: MeSH classifications for most of every 

# E-utility URL (Entrez)

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/

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


#this unique token must be modified for each unique RHS
tokenID_Finder <- GET("https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20%20SELECT%20%3Fd%20%3FdName%20%3Fc%20%3FcName%20%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20%20%20WHERE%20%7B%20%20%20%20%20%20%3Fd%20a%20meshv%3ADescriptor%20.%20%20%3Fd%20meshv%3Aactive%201%20.%20%20%3Fd%20meshv%3Aconcept%20%3Fc%20.%20%20%3Fd%20rdfs%3Alabel%20%3FdName%20.%20%20%3Fc%20rdfs%3Alabel%20%3FcName%20%20FILTER(REGEX(%3FdName%2C%22zinc%22%2C%22i%22)%20%7C%7C%20REGEX(%3FcName%2C%22zinc%22%2C%22i%22))%20%20%20%20%7D%20%20ORDER%20BY%20%3Fd&format=JSON&inference=true&offset=0&limit=50")
tokenID_from_rdf <-fromJSON(rawToChar(tokenID_Finder$content))
head(tokenID_from_rdf)

#this query must be modified for each unique Token
ancestry_Query <-
ancestry_Finder <- GET("https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20SELECT%20%3FtreeNum%20%3FancestorTreeNum%20%3Fancestor%20%3Falabel%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20WHERE%20%7B%20%20%20%20mesh%3AD015033%20meshv%3AtreeNumber%20%3FtreeNum%20.%20%20%20%20%3FtreeNum%20meshv%3AparentTreeNumber%2B%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20meshv%3AtreeNumber%20%3FancestorTreeNum%20.%20%20%20%20%3Fancestor%20rdfs%3Alabel%20%3Falabel%20%7D%20%20ORDER%20BY%20%3FtreeNum%20%3FancestorTreeNum&format=JSON&inference=true&offset=0&limit=50")


data <- fromJSON(rawToChar(res$content))

data$paths$`/lookup/term`$get$tags(zinc)
