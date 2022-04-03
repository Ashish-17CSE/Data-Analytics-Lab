#install.packages('SnowballC')
#install.packages('tm')
library(tm)


tw <- read.csv("C:/Users/Ashish/Downloads/Ashish - Classes/D.A/LAB/Assig-5/Trump_Data.csv" ,stringsAsFactors = FALSE)


#tw1 <- tw$text
tw1 <- tw["text"]
head(tw1)



# Create Corpus
docs <- VCorpus(VectorSource(tw1))
summary(docs)   
inspect(docs)



# inspect a particular document 


writeLines(as.character(docs[[1]]))



##
## ----- Start Preprocessing ------- ##
##
toSpace <- content_transformer(function(X,pattern) { return(gsub(pattern, " ", X))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " _")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "https")


# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Treansform to lower case 
docs <- tm_map(docs, content_transformer(tolower))
# strip digits
docs <- tm_map(docs, removeNumbers)
# Remove stopword from standard stopword list (How to chek this? How to add your own?)
docs <- tm_map(docs, removeWords, stopwords("english"))
# Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
# inspect output
writeLines(as.character(docs[[1]]))


## Need Snowballc library for stemming
library(SnowballC)
# Stem document 
docs <- tm_map(docs, stemDocument)
##
##
## some clean up ----------------------  ##
##
##
docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ") 
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterp")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")
#inspect 
#
#

writeLines(as.character(docs[[1]]))

##
##
## ---- Create document-term matrix
##
##
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# - collapse matrix by summing over columns - this gets total counts (over all docs) for each te
freq <- rowSums(as.matrix(dtm))
# - length should be total number of terms
length(freq)
# - create sort order (asc)
ord <- order(freq, decreasing =  TRUE)
# - inspect most frequently occurring terms
freq[head(ord)]
# - inspect least frequently occurring terms
freq[tail(ord)]


##
##
# remove  words
##
##
dtmr <- DocumentTermMatrix(docs, control = list(wordLengths = c(2,20), bounds = list(global = c(1,27))))

dtmr <- TermDocumentMatrix(docs)
m <- as.matrix(dtmr)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# - collapse matrix by summing over columns - this gets total counts (over all docs) for each te
freqr <- rowSums(as.matrix(dtmr))
# - length should be total number of terms
length(freqr)
# - create sort order (asc)
ord <- order(freqr, decreasing =  TRUE)
# - inspect most frequently occurring terms
freq[head(ord)]
# - inspect least frequently occurring terms
freq[tail(ord)]


# list most frequent terms. Lower bound Specified as second argument 
findFreqTerms(dtmr, lowfreq = 10)
##
##
# histogram plot
##
##
wf = data.frame(term = names(freqr), occurrencse = freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>300), aes(term, occurrencse))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

##
##
# - Wordcloud --------- #
##
##
#install.packages('wordcloud')
library(wordcloud)
set.seed(42)
# limit words by spcifying min frequency
wordcloud(names(freqr), freqr, min.freq = 70, colors = brewer.pal(10, "Dark2"))

