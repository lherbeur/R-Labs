# data source - https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip



library(tidyverse)
library(dplyr)

#1. Merges the training and the test sets to create one data set.
# and 4. Appropriately labels the data set with descriptive variable names. 
# somewhat does 4
train_path <- 'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/UCI HAR Dataset/train/X_train.txt'
test_path <- 'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/UCI HAR Dataset/test/X_test.txt'
features_path <- 'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/UCI HAR Dataset/features.txt'

features <- read.table(features_path)
features <- as.vector(features[, "V2"])
features <- make.names(names=features, unique=TRUE, allow_ = TRUE)
features <- gsub('\\.', '', features)
# ?make.names

train <- read.table(train_path, header = FALSE)
test <- read.table(test_path, header = FALSE)

merged_data <- bind_rows(train, test)
names(merged_data) <- features


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
meanSDData <- merged_data %>% 
  select(matches('mean|std'))

#3. Uses descriptive activity names to name the activities in the data set
train_activities_path <- 'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/UCI HAR Dataset/train/y_train.txt'
test_activities_path <- 'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/UCI HAR Dataset/test/y_test.txt'

train_activities <- read.table(train_activities_path, header = FALSE)
train_activities <- as.character(train_activities[, 'V1'])

train_activities <- lapply(train_activities, function(x)
 {
   train_activities[x] <- if (x == '1')
   {
     'WALKING'
   }
   else if(x == '2')
   {
     'WALKING_UPSTAIRS'
   }
   else if(x == '3')
   {
     'WALKING_DOWNSTAIRS'
   }
   else if(x == '4')
   {
     'SITTING'
   }
   else if(x == '5')
   {
     'STANDING'
   }
   else if(x == '6')
   {
     'LAYING'
   }
 })


test_activities <- read.table(test_activities_path, header = FALSE)
test_activities <- as.character(test_activities[, 'V1'])

test_activities <- lapply(test_activities, function(x)
{
  test_activities[x] <- if (x == '1')
  {
    'WALKING'
  }
  else if(x == '2')
  {
    'WALKING_UPSTAIRS'
  }
  else if(x == '3')
  {
    'WALKING_DOWNSTAIRS'
  }
  else if(x == '4')
  {
    'SITTING'
  }
  else if(x == '5')
  {
    'STANDING'
  }
  else if(x == '6')
  {
    'LAYING'
  }
})

# declare fxn n use in lapply
# convert <- function(x)
# {
#   
# }
train_activities <- unlist(train_activities)
test_activities <- unlist(test_activities)

activities_data <- c(train_activities,test_activities)
merged_data <- mutate(merged_data, activity = as.vector(activities_data))

# 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
activityGroupedData <- merged_data %>%
  group_by(activity) %>%
  print

# ?summarise_all
# 
# activityGroupedData %>%
#   summarise(averagetBodyAccmeanX = mean(tBodyAccmeanX))

print('Mean of all ')
summarisedByActivity <- activityGroupedData %>%
  summarise_all(mean)


# mean(merged_data[merged_data$activity == 'WALKING_UPSTAIRS',c('tBodyAccmeanX')] )

# for the 2nd part ...i.e the average based on subject, just join the subjects data, group by subjects n summarise all.

write.table(summarisedByActivity,'C://Users/Lherbeur/Documents/Projects/R/7.R Labs/final.txt', row.names = FALSE)












