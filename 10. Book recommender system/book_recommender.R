# data source - http://www2.informatik.uni-freiburg.de/~cziegler/BX/
  
library(plyr)
library(tidyverse)
library(caret)

books <- read_csv2('C://Users/Lherbeur/Documents/Projects/R/10. Book recommender system/BX-CSV-Dump/BX-Books.csv')
ratings <- read_csv2('C://Users/Lherbeur/Documents/Projects/R/10. Book recommender system/BX-CSV-Dump/BX-Book-Ratings.csv')
head(ratings)

merge_tables <- inner_join(books, ratings, by="ISBN")

average_bk_ratings <- merge_tables %>% 
                    group_by(ISBN,`Book-Title`,`Book-Author`) %>% 
                    dplyr::summarise(amount = n(), average = mean(`Book-Rating`))

average_bk_ratings <- filter(average_bk_ratings, average > 0.0)
ggplot(data = average_bk_ratings, mapping = aes(x = average)) + geom_histogram(binwidth = 0.5)
ggplot(data = average_bk_ratings, mapping = aes(x = log1p(amount))) + geom_density() 
ggplot(data = average_bk_ratings, mapping = aes(x = log1p(amount), y = average)) + geom_point() 


size = floor(0.67*nrow(merge_tables))
trainIndex = sample(seq_len(nrow(merge_tables)), size = size )
trainData <- merge_tables[trainIndex,]
testData <- merge_tables[-trainIndex,]

# baseline - compare overall average over all movies
overallMovieAverageRating <- mean(trainData$`Book-Rating`)

testDataPreds <- mutate(testData, overallMovieAverageRating=overallMovieAverageRating)
postResample(testDataPreds$overallMovieAverageRating, testDataPreds$`Book-Rating`)

# 1 - no filter on count
trainDataAvg <- trainData %>% 
  group_by(ISBN) %>% 
  dplyr::summarise(amount = n(), average = mean(`Book-Rating`))

testDataPreds <- inner_join(testData, trainDataAvg, by="ISBN")
postResample(testDataPreds$average, testDataPreds$`Book-Rating`)

# 2 - no filter on count
trainDataAvg <- trainData %>% 
  group_by(ISBN) %>% 
  dplyr::summarise(amount = n(), average = mean(`Book-Rating`))

trainDataAvg <- filter(trainDataAvg, amount >= 10 )

testDataPreds <- inner_join(testData, trainDataAvg, by="ISBN")
postResample(testDataPreds$average, testDataPreds$`Book-Rating`)


# 3 - no filter on count
trainDataAvg <- trainData %>% 
  group_by(ISBN) %>% 
  dplyr::summarise(amount = n(), average = mean(`Book-Rating`))

trainDataAvg <- filter(trainDataAvg, amount >= 20)

testDataPreds <- inner_join(testData, trainDataAvg, by="ISBN")
postResample(testDataPreds$average, testDataPreds$`Book-Rating`)

#best model is amount>=10 









