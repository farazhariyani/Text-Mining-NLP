#load libraries
library(rvest)
library(XML)
library(magrittr)

surl <- "https://www.snapdeal.com/product/reliance-4g-black-data-cards/618575312815/reviews?page"
snapdeal_reviews <- NULL

for (i in 1:20){
  murl <- read_html(as.character(paste(surl,i,sep="=")))
  rev <- murl %>% html_nodes("#defaultReviewsCard p") %>% html_text()
  snapdeal_reviews <- c(snapdeal_reviews,rev)
}

write.table(amazon_reviews,"TextMining-Reviews-Jio4GDataCard.txt", row.names = FALSE)
getwd()

#### Text Mining - Web Scraping ####
txt <- snapdeal_reviews

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)
#top 20 rows and columns
tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 5)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats maximum number of times
x1 <- tm_map(x1, removeWords, c('snapdeal','delivery','jio','thank'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 5)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

##### Word cloud #####
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered
#better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)


#### Bigram ####
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

#trigram
minfreq_trigram <- 3
tritoken <- NGramTokenizer(x1, Weka_control(min = 3, max = 3))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq, decreasing = TRUE), ]

wordcloud(sort_three$tritoken, sort_three$Freq, random.order = F, scale = c(3, 0.35), min.freq = minfreq_trigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

#Sentiment Analysis
# Loading Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt

stopwdrds <-  readLines(file.choose())

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))