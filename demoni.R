library(twitteR)
library(tm)
library(plyr)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(ROAuth)
library(stringr)
library(ggplot2)
library(skmeans)
library(syuzhet)
library(dplyr)
library(stringi)
library(topicmodels)
library(readr)
setwd("C:\\Users\\DIPU\\Desktop\\Demoni")

Kaggle_demonetization_tweets <- read_csv("C:\\Users\\DIPU\\Desktop\\Demoni\\demonetization-tweets.csv", 
                                         col_types = cols(id = col_character(), 
                                                          replyToSID = col_character()))


# Twitter handles with maximum posts (TOP 10)
# Cleanup all URLs in statusSource column. In other words, extract all characters between > and < 
Kaggle_demonetization_tweets$statusSource <- gsub("http[^[:blank:]]+", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub("[[:punct:]]", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub(" a ", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub("[ \t]{2,}", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub("^\\s+|\\s+$", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub(" a href rel nofollow ", " ", Kaggle_demonetization_tweets$statusSource)
Kaggle_demonetization_tweets$statusSource <- gsub(" href rel nofollow ", " ", Kaggle_demonetization_tweets$statusSource)

tweetsBySource <- Kaggle_demonetization_tweets %>%
  group_by(statusSource) %>%
  summarize(freqSrc=n()) %>%
  arrange(desc(freqSrc))

tweetsBySource.Top <- tweetsBySource[order(-tweetsBySource$freqSrc),]
tweetsBySource.Top7 <- tweetsBySource.Top[1:7,]

ggplot(tweetsBySource.Top7, aes(sort(tweetsBySource.Top7$statusSource,decreasing = T),tweetsBySource.Top7$freqSrc*10)) + 
  geom_bar(stat = "identity", width=0.5) + coord_flip() +
  geom_text(aes(label=tweetsBySource.Top7$freqSrc*10), hjust = -0.3, size =3, color = "red") +
  ylab("Number of Tweets") + xlab("Twitter handle") + labs(title = "Top 7 Sources that contribute to maximum posts") +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16, colour = "blue"))



# Finding the Twitter handles that were the most active or those who had the maximum number of posts
tweetsPerUser <- Kaggle_demonetization_tweets %>%
  group_by(screenName) %>%
  summarize(freq=n()) %>%
  arrange(desc(freq))

tweetsPerUser.Top <- tweetsPerUser[order(-tweetsPerUser$freq),]
tweetsPerUser.Top10 <- tweetsPerUser.Top[1:10,]

ggplot(tweetsPerUser.Top10, aes(sort(tweetsPerUser.Top10$screenName,decreasing = T),tweetsPerUser.Top10$freq*10)) + 
  geom_bar(stat = "identity", width=0.5) + coord_flip() +
  geom_text(aes(label=tweetsPerUser.Top10$freq*10), hjust = 2, size =3, color = "white") +
  ylab("Number of Tweets") + xlab("Twitter handle") + labs(title = "Twitter handles with maximum posts\n Top 10") +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16, colour = "blue"))



sum(tweets.df$retweetCount)








tweets.df <- read.csv("C:\\Users\\DIPU\\Desktop\\Demoni\\demonetization-tweets.csv")
str(tweets.df)

tweets <- as.character(tweets.df$text)


tweets <- iconv(tweets, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)  # Make everything consistently lower case
tweets <- gsub("rt", " ", tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)  # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)  # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets <- gsub("amp", " ", tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)  # Leading blanks
tweets <- gsub(" $", "", tweets)  # Lagging blanks
tweets <- gsub(" +", " ", tweets) # General spaces (should just do all whitespaces no?)
tweets <- unique(tweets)  # Now get rid of duplicates!

# remove numbers
sentence <- gsub('[[:digit:]]', '', tweets)

# remove extra spaces
sentence <- gsub('[ \t]{2,}', '', sentence)
sentence <- gsub('^\\s+|\\s+$', '', sentence)





mysentiment<-get_nrc_sentiment((sentence))
####used to classify sentiment scores
Sentimentscores<-data.frame(colSums(mysentiment[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score*10.25))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("No. of tweets")+ggtitle("Total sentiment based on scores")





# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(tweets))  # Create corpus object

# Remove English stop words. This could be greatly expanded!
# Don't forget the mc.cores thing
corpus <- tm_map(corpus, removeWords, stopwords("english"))  

myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","make","ask","come","end",
                 "two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","now","think","âve",
                 "âre ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "sinc","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli",
                 "never","bit","entir",
                 "lot","a","about","above","across","after","afterwards","again",
                 "I", 
                 "a","away","know","that","has","gets","but","back","their","very","says","your","his","they",
                 "about","after","has","why","what","who","whom","you","and",
                 "an","off","over", 
                 "are", 
                 "as", 
                 "at", 
                 "be", 
                 "by", 
                 "com", 
                 "for", 
                 "from",
                 "how",
                 "just",
                 "in", 
                 "is", 
                 "it", 
                 "of", 
                 "on", 
                 "or", 
                 "that",
                 "the", 
                 "this",
                 "to", 
                 "was", 
                 "what", 
                 "when",
                 "where",
                 "who", 
                 "will", 
                 "with",
                 "the",
                 "www"
)





corpus <- tm_map(corpus,removeWords,c("demonetization"))

corpus <- tm_map(corpus,removeWords,myStopwords)

# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers)

# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument)

# Remove the stems associated with our search terms!
corpus <- tm_map(corpus, removeWords, c("energi", "electr"))



# Why not visualize the corpus now?
# Mine had a lot to do with {solar, power, renew, new, can, save, suppl, wind, price, use}
pal <- brewer.pal(9, "Set1")
wordcloud(corpus, min.freq=40, max.words = 100, random.order = TRUE, col = pal)







# Now for Kmeans clustering

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
# model <- LDA(dtm, 10)  # Go ahead and test a simple model if you want

str(dtm)

term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
summary(term_tfidf)

llisreduced.dtm <- dtm[,term_tfidf >= 0.155]
summary(slam::col_sums(llisreduced.dtm))

#findFreqTerms(dtm,100)
#findAssocs(dtm,"blackmoney",0.5)


harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

seqk <- seq(2, 50, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(llisreduced.dtm, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))
#####seems like feel positive about demonitization!!!!
#####There are people who feel negative about it, mixed feelings eh??


mydata.dtm3 <- TermDocumentMatrix(corpus)
tdm2 = as.matrix(mydata.dtm3)
head(tdm2)
popu = sort(colSums(tdm2), decreasing=TRUE)
head(popu)

popu1 = data.frame(Number_of_Docs = names(popu), topics=popu)


library(ggplot2)
ggplot(popu1[1:15,], aes(x=topics, y=Number_of_Docs)) + geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Popularity") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), axis.text.y=element_text(size=13,colour="black",                                        face="bold"), axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))+theme_bw()
















## do tfxidf
#dtm_tfxidf <- weightTfIdf(dtm)
#inspect(dtm_tfxidf[1:10, 5001:5010])



tdm=t(dtm)
tdm2 <- removeSparseTerms(tdm,sparse=0.75)
tdm2.matrix <- as.matrix(tdm2)

distMatrix <- dist(scale(tdm2.matrix))

hclust.fit <-hclust(distMatrix, method = "ward")

plot(hclust.fit,cex=0.9,hang=-1,main="Word Cluster Dendogram after removing sparse terms ")

rect.hclust(hclust.fit,k=5)

(hclust.groups <- cutree(hclust.fit,k=5))


library(fpc)
d <- dist(t(dtm_tfxidf),method="euclidian")

kfit <- kmeans(d,10)

clusplot(as.matrix(d),kfit$cluster,color=TRUE,shade=TRUE,labels =2, lines=0 )

# #dtm_tfxidf <- weightTfIdf(dtm)
# m1 <- as.matrix(dtm_tfxidf)
# m<-t(m1)
# rownames(m) <- 1:nrow(m)
# norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
# m_norm <- norm_eucl(m)
# num_cluster<-10
# cl <- kmeans(m_norm, num_cluster)
# round(cl$centers, digits = 1)


for (i in 1:10) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kfit$centers[i,], decreasing = T)
  cat(names(s)[1:10], "\n")
}

# clusplot(m_norm, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# 
# require(fpc)
# plotcluster(m_norm,cl$cluster)




# 
# ## do document clustering
# 
# ### k-means (this uses euclidean distance)
# m <- as.matrix(dtm_tfxidf)
# rownames(m) <- 1:nrow(m)
# 
# ### don't forget to normalize the vectors so Euclidean makes sense
# norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
# m_norm <- norm_eucl(m)
# 
# mydata <-data.frame(m_norm,cl$cluster)
# require(cluster)
# clusplot(mydata,cl$cluster,color=TRUE,shade=TRUE,labels=2,lines =0 )
# 
# 
# 
# ### cluster into 10 clusters
# cl <- kmeans(m_norm, 10)
# cl
# cl$cluster[1:100]
# 
# count(cl$cluster)
# table(cl$cluster)
# 
# 
# require(cluster)
# clusplot(m_norm, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# 
# 
# 
# 
# ### show clusters using the first 2 principal components
# plot(prcomp(m_norm), col=cl$cl)
# 
# library(cluster)
# library(fpc)
# #clusplot(m_norm, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# 
# 
# plotcluster(m_norm,cl$cluster)
# #hparty<- skmeans(dtm,10,control=list(verbose=TRUE))
# #hparty$value


#class_ids <- attr(dtm, "rclass")
#table(class_ids, hparty$cluster)

#require("cluster")
#plot(silhouette(hparty))


#for finding optimal no of k topics

best.model <- lapply(seq(2,20, by=1), function(k){LDA(dtm, k)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

best.model.logLik.df <- data.frame(topics=c(2:20), LL=as.numeric(as.matrix(best.model.logLik)))


ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  theme(axis.text.x = element_text(vjust = -0.25, size = 14)) + 
  theme(axis.text.y = element_text(size = 14, angle=90))
best.model.logLik.df[which.max(best.model.logLik.df$LL),]






# Now for some topics and topic modelling
SEED = sample(1:1000000, 1)  # Pick a random seed for replication
k = 15  # Let's start with 10 topics

# # This might take a minute!
# models <- list(
#   # CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
#   # VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
#   # VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
#   Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
#                                                                thin = 100,    iter = 1000))
# )


ldaOut <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                               thin = 100,    iter = 1000))


desc_lda <- topicmodels::LDA(dtm, k = 15, control = list(seed = 1234))

tidy_lda <- tidytext::tidy(desc_lda)











#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,20))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


term <- read.csv("C:\\Users\\DIPU\\Desktop\\Demoni\\LDAGibbs 15 TopicsToTerms.csv")




mysentiment1 <-get_nrc_sentiment(ldaOut.terms[,9])
####used to classify sentiment scores
Sentimentscores<-data.frame(colSums(mysentiment1[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score*10.25))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("No. of tweets")+ggtitle("Total sentiment based on Topic 9")













#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#hist(as.numeric(unlist(topicProbabilities)))





n <- 100; palette = "Greens"; lda <- ldaOut

p <- posterior(lda)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>%
  mutate(word = rownames(w1)) %>%
  tidyr::gather(topic, weight, -word) 

pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]

wd <- setwd("C:\\Users\\DIPU\\Desktop\\Demoni")

# need to be careful for warnings that words didn't fit in, 
# in which case make the png device larger
# remove any PNG files sitting around in the temp folder
unlink("*.png")
# create a PNG for each frame of the animation
for(i in 1:ncol(w1)){
  png(paste0(i + 1000, ".png"), 8 * 100, 8 * 100, res = 100)
  par(bg = "grey95")
  w3 <- w2 %>%
    filter(topic == i) %>%
    arrange(desc(weight))
  with(w3[1:n, ], 
       wordcloud(word, freq = weight, random.order = FALSE, 
                 ordered.colors = TRUE, colors = pal))
  title(paste("Demonetization Topic", i))
  dev.off()
}



#The column  ????  tells us the probability of that term being generated from that topic for that document. It is the probability of that term (word) belonging to that topic.
#Notice that some of the values for  ????  are very, very low, and some are not so low



top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)







top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")



#Notice that some of the probabilites visible at the top of the data frame are low and some are higher. Our model has assigned a probability to each description belonging to each of the topics we constructed from the sets of words. How are the probabilities distributed? Let's visualize them



lda_gamma <- tidytext::tidy(desc_lda, matrix = "gamma")

#Plot top 10 terms in each lda topics

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 5) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))


#First notice that the y-axis is plotted on a log scale; otherwise it is difficult to make out any detail in the plot. Next, notice that  ????  runs from 0 to 1 in each panel.
#Remember that this is the probability that a given document belongs in a given topic. There are many values near zero, which means there are many documents that do not belong in each topic. 
#Also, most of these panels show a higher number of documents near  ??=1??=1 ; these are the documents that do belong in those topics. For example, let's look specifically at topic 18 in Figure 9.7, a topic that had documents cleanly sorted in and out of it. 
#There are many documents with  ????  close to 1; these are the documents that do belong to topic 18 according to the model. There are also many documents with  ????  close to 0; these are the documents that do not belong to topic 18.
#Each document appears in each panel in this plot, and its  ????  for that topic tells us that document's probability of belonging in that topic.

#This plot displays the type of information we used to choose how many topics for our topic modeling procedure.
#When we tried options higher than 24 (such as 32 or 64), the distributions for  ????  started to look very flat toward  ??=1??=1 ; documents were not getting sorted into topics very well.








