## Part 5: advanced natural language processing using udpipe package, 
## and a slew of other nifty ones..

## ============================================================================
## ADVANCED NLP NATURAL LANGUAGE PROCESSING
## ============================================================================

## If it works, adapted by dino from code on a kaggle competition,
## otherwise i have no idea who wrote this!

## You don't have to do this because you already installed these.. But
## you would have to do the below if you did not do my previous labs on
## this machine...
## install.packages("tibble")
## install.packages("ggplot2")
## install.packages("dplyr")

# Bring in the namespaces in a NEW RStudio session in this order..
library(tibble)
library(dplyr)
library(ggplot2)

## udpipe is available on CRAN and you may install it now
## &reply 'y' to 'Do you want to install from sources the package which needs compilation?'
install.packages('udpipe')

## Input Dataset includes the entire corpus of articles published by the ABC 
## website in the given time range. With a volume of 200 articles per day 
## and a good focus on international news. Unpack abcnews-date-text.csv and
## put the file in RStudio's current work directory (getwd()/setwd())
getwd()
# Session | Set Working Directory | Choose directory..


## ============================================================================
## Section 1: Data cleaning and exploration
## ============================================================================

## let's read in the data
news <- read.csv('abcnews-date-text.csv', header = T, stringsAsFactors = F)

## this is a SQL-like construct that groups news by publish date and count,
## in dscending count order
news %>% group_by(publish_date) %>% count() %>% arrange(desc(n))

## Let?s split year, month and date and see the distribution of data based on year
## This uses the stringr library and dplyr's mutate functionality to create new data 
## columns
library(stringr)
news_more <- mutate(news, year = str_sub(publish_date,1,4),
                          month = str_sub(publish_date,5,6),
                          date = str_sub(publish_date,7,8))

## This plot will allow us to see the distribution of data based on year:
## Almost 100K articles in 2013!
news_more %>% group_by(year) %>% count() %>% ggplot() + geom_bar(aes(year,n), stat ='identity')


## ============================================================================
## Section 2: Staging with pretrained language model
## ============================================================================

## Let's download the english data model
library(udpipe)

## At first time model download execute the below line
model <- udpipe_download_model(language = "english")

## And then
udmodel_english <- udpipe_load_model(model$file_model)
## udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

## Now subset data only for 2008
news_more_2008 <- news_more %>% filter(year == 2008 & month == 10)

## It is not enough to simply provide a computer with a large amount of data 
## and expect it to understand it. The data has to be prepared in such a way 
## that the program can more easily find patterns and inferences. This is usually 
## done by adding relevant and accurate metadata to a dataset. Any metadata tag 
## used to mark up elements of the dataset is called an annotation over the input.
## Datasets of natural language are referred to as corpora (one dataset is a corpus),
## and a single set of data annotated with the same specification is called an 
## annotated corpus.

## There the different aspects of language that are studied and used for annotations.
## Grammar is the name typically given to the mechanisms responsible for creating 
## well-formed structures in language. Most linguists view grammar as itself consisting 
## of distinct modules or systems, either by cognitive design or for descriptive convenience.
## These areas usually include syntax, semantics, morphology, phonetics, and the lexicon. 
## Areas beyond grammar that relate to how language is embedded in human activity include 
## discourse, pragmatics, and text theory.

## udpipe_annotate() takes the language model and annoates the given text data
s <- udpipe_annotate(udmodel_english, news_more_2008$headline_text)
x <- data.frame(s)

## ============================================================================
## Section 3: Analysis
## ============================================================================

## To plot Part-of-speech tags from the given text, use package lattice
library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## Since we?ve got the text annotated with Part of Speech, let?s tally up the 
## most common nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## In the english language, you exaggerate with adjective. So, let?s explore
## the most occurring adjectives
## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

## We can conduct a sentiment analysis with verbs used. Do the bring any 
## signs of optimision or infuse pessimism instead?
## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

## RAKE is one of the most popular unsupervised algorithms for extracting 
## keywords in Information retrieval. RAKE is short for Rapid Automatic 
## Keyword Extraction. It is a domain independent algorithm which tries to 
## determine key phrases in a body of text by analyzing the frequency of
## word appearance as well as its co-occurrence with other words in the corpus
## Let's goup nouns and adjectives for a better understanding of the roles of 
## nouns
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

## A noun and a verb form a simple phrase. For example, "man died".
## That may help us understand the context of sentences
## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

## Let's plot a word cloud
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))

## ============================================================================
## Section 4: ## DATA SCIENCE CONCLUSIONS:
## ============================================================================
##
## The 2008 financial crisis was not just a US market meltdown but also
## a hot topic all over the world, with Australian market not an exception,
## since Financial Crisis is at the top of the list and Wall st not too far off.
## Climate change and the Bali terrorist attack was also hot on the minds of
## Australians in 2008. The government of New South Wales (nsw) also a hot
## topic of discussion :-)








## ************************************************************************************
## Assignment 1
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




