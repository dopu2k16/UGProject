library(readr)
library(dplyr)

library(stringr)
library(jsonlite)


load('C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\review.RData')

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









# ggplot(words_afinn, aes(afinn_score, average_stars, size = reviews)) + 
#   geom_smooth(method="lm", se=FALSE, show.legend=FALSE) +
#   geom_text(aes(label = word, size = NULL), check_overlap = TRUE, vjust=1, hjust=1) +
#   geom_point() +
#   scale_x_continuous(limits = c(-6,6)) +
#   xlab("AFINN sentiment score") +
#   ylab("Average Yelp stars")
# 
# 
# 
# 
# ggplot(words_afinn, aes(reviews, average_stars, color = afinn_score)) +
#   geom_point() +
#   geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
#   scale_x_log10() +
#   geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
#   scale_colour_gradient2("AFINN", low = "red", mid = "white", high = "blue", limits = c(-5,5)) +
#   xlab("# of reviews") +
#   ylab("Average Stars")



words_afinn %>%
  arrange(desc(reviews)) %>%
  ggplot(aes(afinn_score, average_stars)) +
  geom_point(aes(size = reviews)) +
  geom_text(aes(label = word), vjust = 1, hjust = 1, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("AFINN Sentiment Score") +
  ylab("Average Yelp Stars") +
  expand_limits(x = -6)



#For example, we can see that most profanity has an AFINN score of -4, and that while some words, like "wtf", successfully predict a negative review, others, like "damn", are often positive (e.g. "the roast beef was damn good!").
#Some of the words that AFINN most underestimated included "die" ("the pork chops are to die for!"), and one of the words it most overestimated was "joke" ("the service is a complete joke!").
#One other way we could look at misclassifications is to add AFINN sentiments to our frequency vs average stars plot:



word_summaries_filtered %>%
  inner_join(AFINN, by = "word") %>%
  ggplot(aes(reviews, average_stars, color = afinn_score)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
  scale_color_gradient2(low = "red", high = "blue", midpoint = 0, mid = "gray") +
  labs(x = "# of reviews",
       y = "Average Stars",
       color = "AFINN")