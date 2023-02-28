library(ggplot2)
library(dplyr)
#1) extractor PMID data

Queries = c('complement system', '(complement system AND immunology) AND humans', 
            '((complement system OR complement cascade) AND (immunology OR immune system)) AND humans',
            'complement system AND (pathology OR pathologies)')
Query = c("Unspecific", "Very Broad", "Broad", "Specific")


PMIDs = c(74639, 23232,37420,10496)

df_extractor <- data.frame(Queries,PMIDs)
df_extractor <- df_extractor %>% arrange(-PMIDs)
df_extractor$Queries <- factor(df_extractor$Queries, levels = c('complement system',
    '((complement system OR complement cascade) AND (immunology OR immune system)) AND humans',
    '(complement system AND immunology) AND humans','complement system AND (pathology OR pathologies)'))

ggplot(df_extractor, aes(x = reorder(Query, PMIDs, decreasing = TRUE) , y = PMIDs, fill = Queries)) + geom_col() +
  labs(x = "Query Specificity", y = "PMIDs Retrieved")

#initial parameters: min supp (0.01), min conf (0.5)

#lemma selected: NOUNS, ADJECTIVES, VERBS

#pvalue calculation method: Fisher's Exact Test
#pvalue adjustment: benjamini-hochberg FDR

#pvalue effect on association rule filtering

pvalues <- c(0.05,0.005, 0.0005, 0.00005)

lift2 <- c(3348423,911095,236973,63211)
lift10 <- c(109110,109110,107218,51921)

plot(log10(lift2), pvalues, type = "l", col = "blue", xlab = "# of Associations 10^x")
points(log10(lift10), pvalues, type = "l", col = "red")
legend(1,95,legend = c("lift = 2", "lift = 10"), col = c("blue", "red"))

