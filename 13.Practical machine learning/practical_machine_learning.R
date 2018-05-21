library(caret)
library(tidyverse)
library(glmnet)
library(glmnetUtils)


train_data <- read.csv(file = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = T)
test_data <- read.csv(file = "C://Users/Lherbeur/Downloads/pml-testing__.csv", header = T)
# write.csv(test_data, "C://Users/Lherbeur/Downloads/pml-testing__.csv")


train_data <- na.omit(train_data)
test_data <- test_data[, -length(names(test_data))]
test_data <- test_data[, -1]
test_data <- mutate(test_data, classe  = 'A')
## extract factor columns and drop redundant levels
fctr <- lapply(train_data[sapply(train_data, is.factor)], droplevels)
## count levels
sapply(fctr, nlevels)

## extract factor columns and drop redundant levels
fctr <- lapply(test_data[sapply(test_data, is.factor)], droplevels)
## count levels
sapply(fctr, nlevels)


train_data <- as.tibble(train_data)
test_data <- as.tibble(test_data)


set.seed(325)
trainIndex <- createDataPartition(train_data$classe, p=0.8, list = FALSE, times=1)
sub_train <- train_data[trainIndex, ]
sub_test <- train_data[-trainIndex, ]


scaler <- preProcess(sub_train, method = c('center', 'scale', 'knnImpute'))
sub_train <- predict(scaler, sub_train)

scaler <- preProcess(sub_test, method = c('center', 'scale', 'knnImpute'))
sub_test <- predict(scaler, sub_test)

modFitGBM  <- train(classe ~ ., data=train_data, method = "gbm",
                    verbose = FALSE)


lrModel <- glmnet(classe ~ .,data = sub_train, family = 'multinomial')
predictions <- predict(lrModel, sub_test, type = 'class', s = 0.01)
predictions <- as.factor(predictions)  #wondering y i had to convert it
confusionMatrix(predictions, sub_test$classe)$overall['Accuracy']

# on orig test set
predictions1 <- predict(lrModel, test_data, type = 'class', s = 0.01)
predictions1 <- as.factor(predictions1)  #wondering y i had to convert it
# confusionMatrix(predictions1, sub_test$classe)$overall['Accuracy']
# put the predictions in the testdata

# contained <- (grepl(names(train_data), names(test_data), fixed = F))


