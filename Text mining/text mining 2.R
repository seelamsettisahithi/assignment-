install.packages("tidyverse")
library(tidyverse)
install.packages("wordcloud")
library(wordcloud)
install.packages("RcolorBrewer")
library(RColorBrewer)
library(qdap)
install.packages("tm")
library(tm)
install.packages("gridExtra")
library(gridExtra)
install.packages("dendextend")
library(dendextend)
install.packages("ggthemes")
library(ggthemes)
install.packages("Rweka")
library(RWeka)
install.packages("tidytext")
library(tidytext) 
install.packages("textdata")
library(textdata)
install.packages("lubridate")
library(lubridate)
install.packages("anytime")
library(anytime)
install.packages('devtools')
devtools::install_github("lchiffon/wordcloud2")
install.packages("wordcloud2")
library(wordcloud2)
install.packages("SnowballC")
library(SnowballC)
install.packages("smapr")
install.packages("devtools") 
devtools::install_github("ropensci/smapr")
library(smapr)
data <- read.csv(file.choose())
str(data)
View(data)

# Build Corpus and DTM/TDM
corpus <- data$Text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('Elon_musk','Text'))
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)

inspect(cleanset[1:5])


#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 2547 words and 1000 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
idx <- which(dimnames(tdm)$Terms %in% c("ai", "tesla", "yes"))
as.matrix(tdm[idx, 100:115])
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

mat <- as.matrix(tdm)
# Frequency#
word.freq <- sort(rowSums(mat), decreasing = T)
# Colors#
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# Generate wordcloud#
wordcloud(word = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, 
          colors = pal, scale = c(2, 0.5))

findAssocs(tdm, "tesla", 0.2)

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))


# the word account as the highest frequency. This implies
# that facebook is more concerned about people's account

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)


w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 

letterCloud(w,word = "F",frequency(5), size=1)
# Sentiment Analysis for tweets:

install.packages("syuzhet")
library(syuzhet)

# Read File 
twdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(twdata$Text)
class(tweets)




# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweets)

head(s)

tweets[4]
get_nrc_sentiment('pretending')


# "@prpltnkr Hi there. You can learn how to report a #Page that's 
# pretending to be you in our Help Center: https://t.co/n1CJLpv30Z. -KN
# the above tweet has value 1 for anger, value 1 for Negative 
# and value 2 for positive which reinstates that it has a mixture of 
# all three emotions in the above statement.
# Pretend has one value of negative and one value for anger

get_nrc_sentiment('can learn') #1 for positive

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for  Tweets')
