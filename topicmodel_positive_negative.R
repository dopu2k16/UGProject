#Computes a topic model for the positive and for the negative restaurant reviews corpora.

if(!require(topicmodels)){
  install.packages("topicmodels")
}
require(topicmodels)

if(!require(dplyr)){
  install.packages("dplyr")
}
require(dplyr)


if(!require(caret)){
  install.packages("caret")
}
require(caret)

vars1 <- load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\restaurant.reviews.unigram.positive.negative.RData')




seed <- 200




set.seed(seed)
n <- min(nrow(dtm.restaurant.review.unigram.positive),nrow(dtm.restaurant.review.unigram.negative))
training.positive <- sample(1:nrow(dtm.restaurant.review.unigram.positive),size=n)
training.negative <- sample(1:nrow(dtm.restaurant.review.unigram.negative),size=n)

n.topics <- 20


restaurant.reviews.positive.topicmodel <- LDA(dtm.restaurant.review.unigram.positive[training.positive,], n.topics,method="Gibbs",control = list(seed=seed,verbose=100,thin=100,burnin=1000))  

restaurant.reviews.negative.topicmodel <- LDA(dtm.restaurant.review.unigram.negative[training.negative,], n.topics,method="Gibbs",control = list(seed=seed,verbose=100,thin=100,burnin=1000))  



save(restaurant.reviews.positive.topicmodel,restaurant.reviews.negative.topicmodel,file="C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\topics\\review_topics_positive_negative.RData")



#write out results
#docs to topics
pldaOut.topics <- as.matrix(topics(restaurant.reviews.positive.topicmodel))
write.csv(pldaOut.topics,file=paste("PositiveReviews",k,"DocsToTopics.csv"))

#top 10 terms in each topic
pldaOut.terms <- as.matrix(terms(restaurant.reviews.positive.topicmodel,10))
write.csv(pldaOut.terms,file=paste("PositiveReviews",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
ptopicProbabilities <- as.data.frame(restaurant.reviews.positive.topicmodel@gamma)
write.csv(ptopicProbabilities,file=paste("PositiveReviews",k,"TopicProbabilities.csv"))
#hist(as.numeric(unlist(topicProbabilities)))





#write out Negative reviews topics results
#docs to topics
nldaOut.topics <- as.matrix(topics(restaurant.reviews.negative.topicmodel))
write.csv(nldaOut.topics,file=paste("NegaitiveReviews",k,"DocsToTopics.csv"))

#top 10 terms in each topic
nldaOut.terms <- as.matrix(terms(restaurant.reviews.negative.topicmodel,10))
write.csv(nldaOut.terms,file=paste("NegativeReviews",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
ntopicProbabilities <- as.data.frame(restaurant.reviews.negative.topicmodel@gamma)
write.csv(ntopicProbabilities,file=paste("NegativeReviews",k,"TopicProbabilities.csv"))
#hist(as.numeric(unlist(topicProbabilities)))



require(wordcloud)
require(RColorBrewer)

n <- 100; palette = "Greens"; lda <- restaurant.reviews.positive.topicmodel

p <- posterior(lda)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>%
  mutate(word = rownames(w1)) %>%
  tidyr::gather(topic, weight, -word) 

pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]

wd <- setwd("C:\\Users\\DIPU\\Desktop\\yelpTopicM\\wordcloud\\PositiveReviews")

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
  title(paste("Positive Reviews Topics", i))
  dev.off()
}



#WordClouds for negative reviews topics


n <- 100; palette = "Greens"; lda <- restaurant.reviews.negative.topicmodel

p <- posterior(lda)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>%
  mutate(word = rownames(w1)) %>%
  tidyr::gather(topic, weight, -word) 

pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]

wd <- setwd("C:\\Users\\DIPU\\Desktop\\yelpTopicM\\wordcloud\\NegativeReviews")

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
  title(paste("Negative Reviews Topics", i))
  dev.off()
}










#Positive Reviews topics

tidy_lda <- tidytext::tidy(restaurant.reviews.positive.topicmodel)




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
  labs(title = "Top 10 positive terms in each Positive Restaurant LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")





lda_gamma <- tidytext::tidy(restaurant.reviews.positive.topicmodel, matrix = "gamma")

#Plot top 10 terms in each lda topics

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each positive reviews topic",
       y = "Number of documents", x = expression(gamma))







#Negative Reviews topics

tidy_lda <- tidytext::tidy(restaurant.reviews.negative.topicmodel)




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
  labs(title = "Top 10 terms in each Negative Restaurant LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")





lda_gamma <- tidytext::tidy(restaurant.reviews.negative.topicmodel, matrix = "gamma")

#Plot top 10 terms in each lda topics

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each negative reviews topic",
       y = "Number of documents", x = expression(gamma))



