# Installing the packages
# install.packages("readtext")
# install.packages("quanteda")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("wordcloud")
# install.packages("ggplot2")

# Loading the necessary packages
library(readtext)
library(quanteda)
library(magrittr)

# Loading the documents
snp <-  corpus(readtext("SNP_corpus.csv", text_field = "post_message"))
snp[1:10] # print the first 10 documents
ndoc(snp) # Number of Documents
docnames(snp) # Document Names
nchar(snp[1:10]) # Number of character for the first 10 documents
ntoken(snp[1:10]) # Number of tokens for the first 10 documents
ntoken(snp[1:10], remove_punct = TRUE) # Number of tokens for the first 10 documents after removing punctuation

# Defining custom stopwords
customstopwords <- c("s", "http")

# Creating DFM
snptokens <- tokens(snp, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
snpdfm <- dfm(snptokens, stem = FALSE) %>% 
  dfm_trim(min_doc = 5, min_count = 10) %>% 
  dfm_weight(type = 'tfidf')

# Inspecting the results
topfeatures(snpdfm, 30) 
textplot_wordcloud(snpdfm)

# Plotting a histogram
library(ggplot2)
snpfeatures <- topfeatures(snpdfm, 100)
topDf <- data.frame(list(term = names(snpfeatures), frequency = unname(snpfeatures))) # Create a data.frame for ggplot
topDf$term <- with(topDf, reorder(term, -frequency)) # Sort by reverse frequency order
ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

# Doing it again, removing stop words this time!
snpdfm <- dfm(snptokens, remove = c(stopwords('english'), customstopwords), stem = FALSE) %>% 
  dfm_trim(min_doc = 5, min_count = 10) %>% 
  dfm_weight(type = 'tfidf')

# Inspecting the results again
topfeatures(snpdfm, 30) 
textplot_wordcloud(snpdfm)

# Plotting it again
snpfeatures <- topfeatures(snpdfm, 100)
topDf <- data.frame(list(term = names(snpfeatures), frequency = unname(snpfeatures))) # Create a data.frame for ggplot
topDf$term <- with(topDf, reorder(term, -frequency)) # Sort by reverse frequency order
ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

# Wait? What happens with "shared"?
kwic(snp, "shared", 3)

# Keyword in Context
kwic(snp, "brexit", 4)
kwic(snp, "eu", 4)




### Keyword Analysis

# Loading the UKIP corpus
ukip <-  corpus(readtext("ukip_corpus.csv", text_field = "post_message"))
cat(ukip[1:3])
ukiptokens <- tokens(ukip, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
ukipdfm <- dfm(ukiptokens, remove = c(stopwords('english'), customstopwords)) %>% 
  dfm_trim(min_doc = 5, min_count = 10) %>% 
  dfm_weight(type = 'tfidf')
topfeatures(ukipdfm)
textplot_wordcloud(ukipdfm)

# plotting it
ukipfeatures <- topfeatures(ukipdfm, 100)
topDf <- data.frame(list(term = names(ukipfeatures), frequency = unname(ukipfeatures))) # Create a data.frame for ggplot
topDf$term <- with(topDf, reorder(term, -frequency)) # Sort by reverse frequency order
ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))


# =================================== Keyness Analysis =======================================

kwds <- textstat_keyness(rbind(snpdfm, ukipdfm), target = seq_along(snptokens))
head(kwds, 20)
tail(kwds, 20)
textplot_keyness(kwds)

library(dplyr)
#Select all word with p-value <= 0.05 and then make a comparison wordcloud
kwdssig <- data.frame(term = row.names(kwds), chi2 = kwds$chi2, p=kwds$p) %>% 
  filter(kwds$p <= 0.05) %>% 
  select(term, chi2)
row.names(kwdssig) <- kwdssig$term
kwdssig$SNP <- kwdssig$chi2
kwdssig$UKIP <- kwdssig$chi2
kwdssig$UKIP[kwdssig$UKIP > 0] <- 0
kwdssig$SNP[kwdssig$SNP < 0] <- 0
kwdssig <- kwdssig[,-1:-2]
head(kwdssig)
tail(kwdssig)

library(wordcloud)
set.seed(1024)
png("SNPvsUKIP.png", width = 1200, height = 1200)
comparison.cloud(kwdssig, random.order=FALSE, colors = c("goldenrod1","blueviolet"),scale=c(10,.6), title.size=5, max.words=500)
dev.off()

### Challenge: 
### 1. Expand the customstopword
### 2. Re-do the analysis
### 3. Any different result? Share with the others!

