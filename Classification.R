#MeSH API Classification

#this script calls MeSH RDF API which allows the user to communicate with the MeSH vocabulary library
library(httr)
library(jsonlite)
library(RecordLinkage)
#troubleshooting script

test <- read.csv("sig_rules.csv")
words <- Word_Cleaner(test)
MeSH <- MeSH_Mapper(words)

#this function prepares the RHS words for MeSH mapping
Word_Cleaner <- function(rules) {
      
      #unique word list
      words <- unique(rules$RHS)
      print(length(words))
      unique_rules <- as.character(words)
      
      #removes bracketing {RHS}
      for(i in 1:length(unique_rules)) {
            
            word <- toString(unique_rules[i])
            penul <- nchar(unique_rules[i])-1
            
            #substring the word by removing its first and last char
            unique_rules[i] <- substr(word,2,penul)
            
      }
      #removes problematic consequents that shouldnt exist anyways
      if(length(which(unique_rules=="%"))!=0) {
            unique_rules <- unique_rules[-which(unique_rules == "%")]
      }
      if(length(which(unique_rules=="="))!= 0) {
            unique_rules <- unique_rules[-which(unique_rules == "=")]
      }
      
      lilMatrix <- data.frame(unique_rules)
      return(lilMatrix)
}

MeSH_Mapper <- function(words){
      
      #this is a link that is used to interface with MeSH RDF API
      getsparq_left <- "https://id.nlm.nih.gov/mesh/sparql?query=PREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%20PREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%20PREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%20PREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%20PREFIX%20meshv%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2Fvocab%23%3E%20PREFIX%20mesh%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F%3E%20PREFIX%20mesh2015%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2015%2F%3E%20PREFIX%20mesh2016%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2016%2F%3E%20PREFIX%20mesh2017%3A%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%2F2017%2F%3E%20%20%20SELECT%20%3Fd%20%3FdName%20%3Fc%20%3FcName%20%20FROM%20%3Chttp%3A%2F%2Fid.nlm.nih.gov%2Fmesh%3E%20%20%20%20WHERE%20%7B%20%20%20%20%20%20%3Fd%20a%20meshv%3ADescriptor%20.%20%20%3Fd%20meshv%3Aactive%201%20.%20%20%3Fd%20meshv%3Aconcept%20%3Fc%20.%20%20%3Fd%20rdfs%3Alabel%20%3FdName%20.%20%20%3Fc%20rdfs%3Alabel%20%3FcName%20%20FILTER(REGEX(%3FdName%2C%22"
      getsparq_right1 <- "%22%2C%22i%22)%20%7C%7C%20REGEX(%3FcName%2C%22"
      get_sqarq_right2 <-"%22%2C%22i%22))%20%20%20%20%7D%20%20ORDER%20BY%20%3Fd&format=JSON&inference=true&offset=0&limit=50"
      
      SQL_token <- c()
      #for every word, 
      for(i in 1:length(words$unique_rules)){
            #clean problematic characters
            words$unique_rules[i] <- gsub(')','',words$unique_rules[i])
            
            #create the SQL query that returns the links for possible RDF id matches
            SQL_token_query_modifier <- paste(getsparq_left,words$unique_rules[i],getsparq_right1,words$unique_rules[i],get_sqarq_right2, sep = "")
            
            #get the list and convert it
            tokenID_Finder <- GET(SQL_token_query_modifier)
            tokenID_from_rdf <-fromJSON(rawToChar(tokenID_Finder$content))
            
            #preparing for matching
            words$unique_rules[i] <- paste(toupper(substr(words$unique_rules[i], 1, 1)), substr(words$unique_rules[i], 2, nchar(words$unique_rules[i])), sep="")
            print(paste("Rule:",unique_rules[i]))
            
            check <- Match_Scoring(words$unique_rules[i],words$unique_rules, tokenID_from_rdf$results$bindings$cName$value)
            
            #check if any of the items are identical to the rule's string
            #coord <- which(tokenID_from_rdf$results$bindings$cName$value == unique_rules[i])
            
            SQL_token <- c(SQL_token,check)
      }
      return(SQL_token)
}

Match_Scoring <- function(word,words, dnames){
      
      word_in_question <- word
      print(word_in_question)
      scores <- c()
      
      for(query in 1:length(dnames)){
            
            if(length(dnames[query]) == 0){
                  next
            }
            
            if(word_in_question == dnames[query]){
                  
                  scores <- c(scores, query)
                  print("exact match!")
            
            } else {
                  
                  distances_per_word <- c()
                        
                        for(mot in 1:length(words)) {
                              
                              word <- paste(word_in_question,words[mot], sep = " ")
                              
                              if(word == dnames[query]){
                                    print("exact match level 2")
                                    print(word)
                                    next
                              }
                              distances_per_word <- c(distances_per_word,levenshteinDist(word, dnames[query]))
                              }
                        
                        coordinates <- which(distances_per_word == max(distances_per_word))
                        
            
                        scores <- c(scores,max(distances_per_word))
            }
      }
      
      final_decision <- which(scores == max(scores))
      singular <- which(final_decision == max(final_decision))
      
      if(length(final_decision[singular])==0){
            final_decision <- 0
      }
      else{
            final_decision <- final_decision[singular]
      }
      print(paste("Final Decision Coord", final_decision))
      if(final_decision != 0){
      print(paste("Best Match:", dnames[final_decision]))
      }
      return(final_decision)
}
