## Assignment 1 part2
## NLP using part of Chiness version book of Steve Jobs.

Needed <- c("tibble", "dplyr","ggplot2", "tm", "SnowballCC", "RColorBrewer", "ggplot2", 
            "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)

## install tmcn package for Chinese language
install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")

## install Rwordseg for the purpose of separating chinese words
install.packages("Rwordseg")

## install JiebaR for removing chinese stopwords
install.packages("jiebaR")

## Using library
library(tibble)
library(dplyr)
library(ggplot2)
library(tm)
library(SnowballCC)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(tmcn)
library(Rwordseg)
library(jiebaR)
library(jiebaRD)

## load local txt book.
my_books <- file.path(getwd(), "AssignmentDataSet") 
my_books

## checking loaded books
dir(my_books)

## Load the R package for text mining and then load the texts into R.
books <- Corpus(DirSource(my_books))   
summary(books) 

## view the whole book in terminal
inspect(books[1])

## Removing punctuation and number
for(j in seq(books))   
{   
  books[[j]] <- gsub("\n|！|。|，|”|“|…|、|（|）", " ", books[[j]])  
  books[[j]] <- gsub("[A-Za-z0-9]", "", books[[j]])
} 
books <- tm_map(books, removePunctuation)
books <- tm_map(books, removeNumbers)

## Removing white space
books <- tm_map(books, stripWhitespace)  

## view the content
inspect(books[1])

## Separating chinese words
books <- segmentCN(books$content)

## Removing stopwords
stopwords <- readLines("./stopWordForChinese.txt")

removeStopwords <- function(targetwords,stopwords){
  targetwords = targetwords[targetwords %in% stopwords == FALSE]
  return(targetwords)
}
pure_books <- lapply(books,removeStopwords, stopwords)

## create a document term matrix.
pure_books <- unlist(pure_books)
pure_books_fre <- table(pure_books)

## Organize terms by their frequency:
books_result <- sort(pure_books_fre[order(pure_books_fre)], decreasing=TRUE)

## Plot the most frequently used words.
par(family='STKaiti')
wordcloud(names(books_result),books_result,random.order=FALSE, colors=brewer.pal(7, "Dark2"))
set.seed(142)   
wordcloud(names(books_result), books_result, min.freq=250, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 

##Plot words that occur at least 250 times.
set.seed(142)   
wordcloud(names(books_result), books_result, min.freq=250)  

## Length of whole word list
length(pure_books_fre)

## most and least frequently occurring words.
tail(books_result, 20)
head(books_result, 20)

## the frequency of frequencies.
head(table(books_result), 20)
tail(table(books_result), 20)


## Plot words that appear at least 20 times.
wf <- data.frame(word=names(books_result), freq=books_result)
par(family='STKaiti')
p <- ggplot(subset(wf,freq.Freq>10), aes(word, freq.Freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))


## ============================================================================
## DATA SCIENCE CONCLUSIONS:
## ============================================================================
## My text set using narratage of 'A Bite of China', the most famous show of food in China,
## to draw a map of nostalgia. It describes the eating habits and customs of Chinese people 
## in different regions. The wordcloud told us that this show regarding the people and dishes,
## The words that appear more frequently are tasted, secret, nature, kitchen and tradition, 
## indicating that this program does not blindly inform the process of cooking, but spreads 
## a culture about food through stories. Age, month and year, those words also shows a certain 
## relationship between food and time.




