# data source - https://archive.ics.uci.edu/ml/datasets/car+evaluation

library(tidyverse)
library(caret)

car_data <- read_csv('C:/Users/Lherbeur/Documents/Projects/R/8.Car analysis/car.data', col_names = c('buying_cost', 'maint_cost', 'doors', 'persons', 'lug_boot', 'safety', 'class'),
                    col_types =  cols(
                       buying_cost = col_factor(levels = c('vhigh', 'high', 'med', 'low')),
                       maint_cost = col_factor(levels = c('vhigh', 'high', 'med', 'low')),
                       doors = col_factor(levels = c('2', '3', '4', '5more')),
                       persons = col_factor(levels = c('2', '4', 'more')), 
                       lug_boot = col_factor(levels = c('small', 'med', 'big')),
                       safety = col_factor(levels = c('low', 'med', 'high')),
                       class = col_factor(levels = c('unacc', 'acc', 'good', 'vgood'))
                     ))
# head(car_data)

# ggplot(data = car_data, mapping = aes(x = buying_cost, y = maint_cost)) + geom_tile(mapping = aes(fill = class))
# ggplot(data = car_data, mapping = aes(x = buying_cost, y = doors)) + geom_tile(mapping = aes(fill = class))
# ggplot(data = car_data, mapping = aes(x = buying_cost, y = persons)) + geom_tile(mapping = aes(fill = class))
# ggplot(data = car_data, mapping = aes(x = buying_cost, y = lug_boot)) + geom_tile(mapping = aes(fill = class))
# ggplot(data = car_data, mapping = aes(x = buying_cost, y = safety)) + geom_tile(mapping = aes(fill = class))
# ggplot(data = car_data, mapping = aes(x = maint_cost, y = lug_boot)) + geom_tile(mapping = aes(fill = class))

ggplot(data = car_data, mapping = aes(x = maint_cost, y = lug_boot)) + geom_tile(mapping = aes(fill = class))



set.seed(579676)
trainIndex <- createDataPartition(car_data$class, list = FALSE, times = 1, p = 0.67)
carTrain <- car_data[trainIndex,]
carTest <- car_data[-trainIndex,]

# myModel <- train(class ~ buying_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ maint_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ doors, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ persons, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ lug_boot, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ safety, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)

# #phase2
# myModel <- train(class ~ buying_cost + persons, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + safety, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + maint_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + lug_boot, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)

# #phase3
# myModel <- train(class ~ buying_cost + safety + persons, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + safety + maint_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + safety + lug_boot, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)

# #phase4
# myModel <- train(class ~ buying_cost + safety + persons + maint_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)
# 
# myModel <- train(class ~ buying_cost + safety + persons + lug_boot, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)

# #phase5
# myModel <- train(class ~ buying_cost + safety + persons + maint_cost + lug_boot, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)


# # best model is this in phase 4
# myModel <- train(class ~ buying_cost + safety + persons + maint_cost, data = carTrain, method = 'knn')
# carPredctns <- predict(myModel, carTest)
# confusionMatrix(carPredctns, carTest$class)


# ?confusionMatrix
# ?train
# ?predict

