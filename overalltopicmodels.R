load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\restaurant_ids.RData')


load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\review.RData')

# Computes a topic model for the corpus of restaurant reviews.

if(!require(topicmodels)){
  install.packages("topicmodels",dependencies = TRUE)
}
require(topicmodels)

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
}

require(dplyr)

if(!require(caret)){
  install.packages("caret",dependencies = TRUE)
}

require(caret)





load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\restaurant.reviews.unigram.RData')









seed <- 200

set.seed(seed)



training <- as.vector(unlist(createDataPartition(1:nrow(dtm.restaurant.review.unigram),p=0.5)))
test <- as.vector(unlist(createDataPartition(1:nrow(dtm.restaurant.review.unigram),p=0.05)))
library(syuzhet)

mysentiment<-get_nrc_sentiment(as.vector(dtm.restaurant.review.unigram[test,]))
####used to classify sentiment scores
Sentimentscores<-data.frame(colSums(mysentiment[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Total sentiment based on scores")







k <-20

#n.topics <- c(seq(2,20, by=2),seq(30,100,by=10))

#for(k in n.topics){
  
  
  #print(paste0(Sys.time()," N topics:",k))
  
  fit <- LDA(dtm.restaurant.review.unigram[training,], k,method="Gibbs",control = list(seed=seed,verbose=100,thin=100,burnin=1000))  
  #fit <- LDA(dtm.restaurant.review.unigram, k,method="Gibbs",control = list(seed=seed,verbose=100,thin=100,burnin=1000))  
  
#   
#   ll <- as.numeric(logLik(fit))
#   
#   
#   print(paste0("N topics:",k))
#   print(paste0("LogLikelihood:",ll))
#   
#   print(data.frame(terms(fit,10),stringsAsFactors = FALSE))
#   
#   topics.reviews <- posterior(fit)
#   
#   print(head(topics.reviews$topics))
#   
#   save(k,ll,fit,file=paste0("C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\topics\\review_topics_LDA_Gibbs_k_",k,".RData"))
#   
# }


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(fit))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 20 terms in each topic
ldaOut.terms <- as.matrix(terms(fit,20))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(fit@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#hist(as.numeric(unlist(topicProbabilities)))





require(wordcloud)
require(RColorBrewer)

n <- 100; palette = "Greens"; lda <- fit

p <- posterior(lda)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>%
  mutate(word = rownames(w1)) %>%
  tidyr::gather(topic, weight, -word) 

pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]

wd <- setwd("C:\\Users\\DIPU\\Desktop\\yelpTopicM\\wordcloud")

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
  title(paste("Overall Reviews Topics", i))
  dev.off()
}





tidy_lda <- tidytext::tidy(fit)




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
  labs(title = "Top 10 positive terms in each Overall Review LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")





lda_gamma <- tidytext::tidy(fit, matrix = "gamma")

#Plot top 10 terms in each lda topics

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each reviews topic",
       y = "Number of documents", x = expression(gamma))





library(readr)
library(dplyr)

library(stringr)
library(jsonlite)




# Each line is a JSON object- the fastest way to process is to combine into a
# single JSON string and use fromJSON and flatten
#reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]")

reviews <-  review %>%
  flatten() %>%
  tbl_df()

reviews

library(tidytext)

review_words <- reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

review_words


AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN


reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment


library(ggplot2)
theme_set(theme_bw())


ggplot(reviews_sentiment, aes(stars, sentiment, group = stars)) +
  geom_boxplot() +
  ylab("Average sentiment score")



review_words_counted <- review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

review_words_counted



word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup()

word_summaries



#We can start by looking only at words that appear in at least 200 (out of 200000) reviews. This makes sense both because rare words will have a noisier measurement (a few good or bad reviews could shift the balance), and because they’re less likely to be useful in classifying future reviews or text.
#I also filter for ones that appear in at least 10 businesses (others are likely to be specific to a particular restaurant).



word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

word_summaries_filtered



#What were the most positive and negative words?


word_summaries_filtered %>%
  arrange(desc(average_stars))


#Looks plausible to me! What about negative?

word_summaries_filtered %>%
  arrange(average_stars)


#Also makes a lot of sense. I can also plot positivity by frequency:


ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Stars")



#When we perform sentiment analysis, we’re typically comparing to a pre-existing lexicon, one that may have been developed for a particular purpose. That means that on our new dataset (Yelp reviews), some words may have different implications.
#We can combine and compare the two datasets with inner_join.

words_afinn <- word_summaries_filtered %>%
  inner_join(AFINN)

words_afinn


ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")




