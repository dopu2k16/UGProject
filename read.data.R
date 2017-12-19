# Reads Yelp data in JSON format and saves the resulting data.frames in RData files.

if(!require(jsonlite)){
  install.packages("jsonlite")
}

require(jsonlite)

business.file <- 'D:\\BTP\\yelp_dataset_challenge_academic_dataset\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_business.json'

business <- stream_in(file(business.file))

save(business,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\business.RData')


rm(business)

checkin.file <- 'D:\\BTP\\yelp_dataset_challenge_academic_dataset\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_checkin.json'

checkin <- stream_in(file(checkin.file))

save(checkin,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\checkin.RData')

rm(checkin)



review.file <- 'D:\\BTP\\yelp_dataset_challenge_academic_dataset\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_review.json'

review <- stream_in(file(review.file))

save(review,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\review.RData')

rm(review)



tip.file <- 'D:\\BTP\\yelp_dataset_challenge_academic_dataset\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_tip.json'

tip <- stream_in(file(tip.file))

save(tip,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\tip.RData')

rm(tip)



user.file <- 'D:\\BTP\\yelp_dataset_challenge_academic_dataset\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_user.json'

user <- stream_in(file(user.file))

save(user,file='C:\\Users\\DIPU\\Desktop\\yelpTopicM\\data\\user.RData')

rm(user)