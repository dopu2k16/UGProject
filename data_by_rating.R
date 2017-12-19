# Splits the corpus of restaurant reviews into two corpora. One for positive reviews (stars >3) and another for negative reviews (stars < 3).

vars1 <- load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\restaurant_ids.RData')


vars2 <- load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\review.RData')

review.restaurant <- review[review$review_id %in% review_restaurant_id,]

positive_review_restaturant_id <- review.restaurant[review.restaurant$stars >3,"review_id"]
negative_review_restaurant_id <- review.restaurant[review.restaurant$stars < 3,"review_id"]


rm(review.restaurant,list=c(vars1,vars2))
rm(vars1,vars2)

vars3 <- load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\restaurant.reviews.unigram.RData')

dtm.restaurant.review.unigram.positive <- dtm.restaurant.review.unigram[rownames(dtm.restaurant.review.unigram) %in% positive_review_restaturant_id,]
dtm.restaurant.review.unigram.negative <- dtm.restaurant.review.unigram[rownames(dtm.restaurant.review.unigram) %in% negative_review_restaurant_id,]


rm(list=vars3)
rm(vars3)

save(dtm.restaurant.review.unigram.positive,dtm.restaurant.review.unigram.negative,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\results\\restaurant.reviews.unigram.positive.negative.RData')
save(positive_review_restaturant_id,negative_review_restaurant_id,file="C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\restaurant_reviews_id_positive_negative.RData")

