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



training <- as.vector(unlist(createDataPartition(1:nrow(dtm.restaurant.review.unigram),p=0.2)))


n.topics <- c(seq(2,20, by=2),seq(30,100,by=10))

for(k in n.topics){
  
  
  print(paste0(Sys.time()," N topics:",k))
  
  fit <- LDA(dtm.restaurant.review.unigram[training,], k,method="Gibbs",control = list(seed=seed,verbose=100,thin=100,burnin=1000))  
  
  
  
  ll <- as.numeric(logLik(fit))
  
  
  print(paste0("N topics:",k))
  print(paste0("LogLikelihood:",ll))
  
  print(data.frame(terms(fit,10),stringsAsFactors = FALSE))
  
  topics.reviews <- posterior(fit)
  
  print(head(topics.reviews$topics))
  
  save(k,ll,fit,file=paste0("C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\topics\\review_topics_LDA_Gibbs_k_",k,".RData"))
  
}


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(fit))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(fit,10))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(fit@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#hist(as.numeric(unlist(topicProbabilities)))






