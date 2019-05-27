# ---
# title: "Basic Text Analysis Using R (workshop material)"
# author: "Justin Ho"
# last updated: "27/05/2019"
# ---

# Installing the packages
install.packages("quanteda")
install.packages("magrittr")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("ggplot2")

# Loading the necessary packages
library(quanteda)
library(magrittr)

# Loading the documents
snp <-  read.csv("SNP_corpus.csv", stringsAsFactors = FALSE) %>% 
  corpus(text_field = "post_message")

# The followings are not necessary steps, but it is always a good idea to view a portion of your data
snp[1:10] # print the first 10 documents
ndoc(snp) # Number of Documents
docnames(snp) # Document Names
nchar(snp[1:10]) # Number of character for the first 10 documents
ntoken(snp[1:10]) # Number of tokens for the first 10 documents
ntoken(snp[1:10], remove_punct = TRUE) # Number of tokens for the first 10 documents after removing punctuation

# Defining custom stopwords
customstopwords <- c("s", "http", "stopword")

# Creating DFM
snptokens <- tokens(snp, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
snpdfm <- dfm(snptokens, stem = FALSE) %>% 
  dfm_trim(min_doc = 5, min_termfreq = 10)

# Inspecting the results
topfeatures(snpdfm, 30) 

# A very slow way to plot a wordcloud, use with caution
# textplot_wordcloud(snpdfm)

# Plotting a histogram
library(ggplot2)
# Don't worry about the codes, just change "snpdfm" into the dfm you want to plot
snpfeatures <- topfeatures(snpdfm, 100)  # Putting the top 100 words into a new object
topDf <- data.frame(list(term = names(snpfeatures), frequency = unname(snpfeatures))) # Create a data.frame for ggplot
topDf$term <- with(topDf, reorder(term, -frequency)) # Sort by reverse frequency order
ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

# Doing it again, removing stop words this time!
snpdfm <- dfm(snptokens, remove = c(stopwords('english'), customstopwords), stem = FALSE) %>% 
  dfm_trim(min_doc = 5, min_termfreq = 10)

# Inspecting the results again
topfeatures(snpdfm, 30) 

# textplot_wordcloud(snpdfm,
#                    min.freq = 1,
#                    colors = c("#1B9E77","#D95F02","#7570B3","#E7298A"))

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




# =================================== Keyness Analysis =======================================

# Loading the UKIP corpus
ukip <-  read.csv("ukip_corpus.csv", stringsAsFactors = FALSE) %>% 
  corpus(text_field = "post_message")
cat(ukip[1:3])
ukiptokens <- tokens(ukip, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
ukipdfm <- dfm(ukiptokens, remove = c(stopwords('english'), customstopwords)) %>% 
  dfm_trim(min_doc = 5, min_termfreq = 10)
topfeatures(ukipdfm)

# plotting it
ukipfeatures <- topfeatures(ukipdfm, 100)
topDf <- data.frame(list(term = names(ukipfeatures), frequency = unname(ukipfeatures))) # Create a data.frame for ggplot
topDf$term <- with(topDf, reorder(term, -frequency)) # Sort by reverse frequency order
ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

# Estimating Keyness
kwds <- textstat_keyness(rbind(snpdfm, ukipdfm), target = seq_along(snptokens)) # Making SNP tokens as the target
head(kwds, 20)
tail(kwds, 20)
textplot_keyness(kwds)

# A user-defined function to plot comparison cloud
keyness_cloud <- function(x, a = "A", b = "B", acol = "#00C094", bcol = "#F8766D", w = 600, h = 600, maxword = 500, png = TRUE){
  require(wordcloud)
  require(dplyr)
  set.seed(1024)
  #Select all word with p-value <= 0.05 and then make a comparison wordcloud
  kwdssig <- data.frame(term = kwds$feature, chi2 = x$chi2, p=x$p) %>% 
    filter(x$p <= 0.05) %>% 
    select(term, chi2)
  row.names(kwdssig) <- kwdssig$term
  kwdssig$a <- kwdssig$chi2
  kwdssig$b <- kwdssig$chi2
  kwdssig$b[kwdssig$b > 0] <- 0
  kwdssig$a[kwdssig$a < 0] <- 0
  kwdssig <- kwdssig[,-1:-2]
  colnames(kwdssig) <- c(a, b)
  if (png) {
    png(paste0(deparse(substitute(x)), ".png"), width = w, height = h)
    comparison.cloud(kwdssig, random.order=FALSE, colors = c(acol, bcol),scale=c(6,.6), title.size=3, max.words = maxword)
    dev.off()
  } else {
    comparison.cloud(kwdssig, random.order=FALSE, colors = c(acol, bcol),scale=c(6,.6), title.size=3, max.words = maxword)
  }
}

keyness_cloud(kwds, # Name of the keyness result object
              a = "SNP", # Name of the target corpus
              b = "UKIP", # Name of the reference corpus
              acol = "goldenrod1", # Colour of the target corpus
              bcol = "blueviolet", # Colour of the reference corpus
              png = TRUE) # Save to png?

