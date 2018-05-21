#data source - https://archive.ics.uci.edu/ml/datasets/Computer+Hardware

library(tidyverse)
library(caret)

cpu_data <- read_csv('C:/Users/Lherbeur/Documents/Projects/R/9.CPU Performance analysis/machine.csv', col_names = c('VENDOR','MODEL','MYCT','MMIN','MMAX','CACH','CHMIN','CHMAX','PRP','ERP'))

cpu_data$VENDOR = as.factor(cpu_data$VENDOR)

scaler <- preProcess(cpu_data, method = c("center", "scale"))
cpu_data <- predict(scaler,cpu_data)

# head(cpu_data)
# summary(cpu_data)

vendor_levels <- levels(cpu_data$VENDOR)
vendor_level1 <- vendor_levels[1:6]
vendor_level2 <- vendor_levels[7:12]
vendor_level3 <- vendor_levels[13:18]
vendor_level4 <- vendor_levels[19:24]
vendor_level5 <- vendor_levels[25:30]

vendor_data1 <- cpu_data[cpu_data$VENDOR  %in% vendor_level1, ]
vendor_data2 <- cpu_data[cpu_data$VENDOR  %in% vendor_level2, ]
vendor_data3 <- cpu_data[cpu_data$VENDOR  %in% vendor_level3, ]
vendor_data4 <- cpu_data[cpu_data$VENDOR  %in% vendor_level4, ]
vendor_data5 <- cpu_data[cpu_data$VENDOR  %in% vendor_level5, ]

# plot the vendor in 5 groups of 6 each
ggplot(data = vendor_data1, mapping = aes(x = VENDOR)) + geom_bar()
ggplot(data = vendor_data2, mapping = aes(x = VENDOR)) + geom_bar() 
ggplot(data = vendor_data3, mapping = aes(x = VENDOR)) + geom_bar() 
ggplot(data = vendor_data4, mapping = aes(x = VENDOR)) + geom_bar() 
ggplot(data = vendor_data5, mapping = aes(x = VENDOR)) + geom_bar() 
# find a better way to displ all plots, at once... maybe by grouping
# high count vendors - amdahl, burroughs,crd, cdc, dg, ibm, nas, ncr, siemens, sperry

# remove any vendor with less than 10 observations
# filtered_data <- cpu_data[count(cpu_data[as.character(cpu_data$VENDOR)]) >= 10,]

# do plts of the continuous vars - myct and mmin
ggplot(data = vendor_data1, mapping = aes(x = MYCT, y = MMIN)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data2, mapping = aes(x = MYCT, y = MMIN)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data3, mapping = aes(x = MYCT, y = MMIN)) + geom_point() + geom_line()
ggplot(data = vendor_data4, mapping = aes(x = MYCT, y = MMIN)) + geom_point() + geom_line()
ggplot(data = vendor_data5, mapping = aes(x = MYCT, y = MMIN)) + geom_point() + geom_line()
# the mmin andmyct vars have an inverse reltnshp. not necessarily good predictors but show a reltnshp bt d 2 vars.


# do plots of the continuous vars - myct and mmax
ggplot(data = vendor_data1, mapping = aes(x = MYCT, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data2, mapping = aes(x = MYCT, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data3, mapping = aes(x = MYCT, y = MMAX)) + geom_point(aes(colour = VENDOR)) #
ggplot(data = vendor_data4, mapping = aes(x = MYCT, y = MMIN)) + geom_point(aes(colour = VENDOR)) #
ggplot(data = vendor_data5, mapping = aes(x = MYCT, y = MMIN)) + geom_point(aes(colour = VENDOR)) #
# same as above observation

# plot mmin and mmax
ggplot(data = vendor_data1, mapping = aes(x = MMIN, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data2, mapping = aes(x = MMIN, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data3, mapping = aes(x = MMIN, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data4, mapping = aes(x = MMIN, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
ggplot(data = vendor_data5, mapping = aes(x = MMIN, y = MMAX)) + geom_point(aes(colour = VENDOR)) # + geom_line()
# the lower the mmin, the higher d mmax


# cache n vendor
ggplot(data = vendor_data1, mapping = aes(x = VENDOR, y = CACH)) + geom_boxplot()
ggplot(data = vendor_data2, mapping = aes(x = VENDOR, y = CACH)) + geom_boxplot()
ggplot(data = vendor_data3, mapping = aes(x = VENDOR, y = CACH)) + geom_boxplot()
ggplot(data = vendor_data4, mapping = aes(x = VENDOR, y = CACH)) + geom_boxplot()
ggplot(data = vendor_data5, mapping = aes(x = VENDOR, y = CACH)) + geom_boxplot()

# chmin and chmax
ggplot(data = vendor_data1, mapping = aes(x = CHMIN, y = CHMAX, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data2, mapping = aes(x = CHMIN, y = CHMAX, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data3, mapping = aes(x = CHMIN, y = CHMAX, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data4, mapping = aes(x = CHMIN, y = CHMAX, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data5, mapping = aes(x = CHMIN, y = CHMAX, colour = VENDOR)) + geom_point()
# not much info, tho

# vendor and prp
ggplot(data = vendor_data1, mapping = aes(x = VENDOR, y = PRP)) + geom_boxplot()
ggplot(data = vendor_data2, mapping = aes(x = VENDOR, y = PRP)) + geom_boxplot()
ggplot(data = vendor_data3, mapping = aes(x = VENDOR, y = PRP)) + geom_boxplot()
ggplot(data = vendor_data4, mapping = aes(x = VENDOR, y = PRP)) + geom_boxplot()
ggplot(data = vendor_data5, mapping = aes(x = VENDOR, y = PRP)) + geom_boxplot()

# mcyt and prp
ggplot(data = vendor_data1, mapping = aes(x = MYCT, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data2, mapping = aes(x = MYCT, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data3, mapping = aes(x = MYCT, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data4, mapping = aes(x = MYCT, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data5, mapping = aes(x = MYCT, y = PRP, colour = VENDOR)) + geom_point()
# prp is inversely proportional to the myct

ggplot(data = vendor_data1, mapping = aes(x = MYCT, y = CACH, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data2, mapping = aes(x = MYCT, y = CACH, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data3, mapping = aes(x = MYCT, y = CACH, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data4, mapping = aes(x = MYCT, y = CACH, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data5, mapping = aes(x = MYCT, y = CACH, colour = VENDOR)) + geom_point()
# myct and cach may be good predictors

ggplot(data = vendor_data1, mapping = aes(x = CACH, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data2, mapping = aes(x = CACH, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data3, mapping = aes(x = CACH, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data4, mapping = aes(x = CACH, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data5, mapping = aes(x = CACH, y = PRP, colour = VENDOR)) + geom_point()
# gr8a cache, greater performanc

ggplot(data = vendor_data1, mapping = aes(x = CHMAX, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data2, mapping = aes(x = CHMAX, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data3, mapping = aes(x = CHMAX, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data4, mapping = aes(x = CHMAX, y = PRP, colour = VENDOR)) + geom_point()
ggplot(data = vendor_data5, mapping = aes(x = CHMAX, y = PRP, colour = VENDOR)) + geom_point()
# chmax shld be a good predictor


set.seed(579676)
trainIndex <- createDataPartition(cpu_data$VENDOR, list = FALSE, times = 1, p = 0.67)
cpuTrain <- cpu_data[trainIndex,]
cpuTest <- cpu_data[-trainIndex,]

# ???
myModel <- train(PRP ~ MYCT, data = cpuTrain, method = 'knn')
cpuPredctns <- predict(myModel, cpuTest)
confusionMatrix(cpuPredctns, cpuTest$PRP)












