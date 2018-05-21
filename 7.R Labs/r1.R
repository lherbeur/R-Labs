# data source for getdata_data_ss06hid.csv - https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
# data source for epa daily_88101_1999 data - https://aqs.epa.gov/aqsweb/airdata/daily_88101_1999.zip


library("tidyverse")
library('caret')
library('mlbench')
library('lubridate')
library('jpeg')
library('quantmod')


data <- read.table("C://Users/Lherbeur/Documents/Projects/R/7.R Labs/getdata_data_ss06hid.csv", header = TRUE, sep = ",")
data <- as.tibble(data)

hhGt10 <- data$ACR == 3 & data$AGS == 6   #!is.na(data$ACR) & !is.na(data$AGS) &

which(hhGt10)
#=======================


pic <- readJPEG('C://Users/Lherbeur/Documents/Projects/R/7.R Labs/jeff_leek.jpg',  native=TRUE)
quantile(pic, c(0.30, 0.80))  

#=======================

gdpData <- read_csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv', skip = 5, 
                    col_names = c('CountryCode', 'Ranking', 'xxx', 'Country',  'GDP') )
eduData <- read_csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv')
# joined <- jo

gdpData$xxx <- NULL
gdpData$GDP <- as.numeric(gsub('[$,]', '', gdpData$GDP))


merged <- merge(eduData, gdpData, by = 'CountryCode')
merged <- merged[order(-merged$GDP),]

oecdColumn <- merged[merged$`Income Group` == 'High income: OECD', ]
nrow(merged[grep('Fiscal year end: Jun',merged$`Special Notes`), ] )




quantiled_data <- quantile(merged$GDP, c(0.20, 0.40, 0.60, 0.80, 1.0), na.rm = TRUE)

ctrvs <-merged[, c('Country', 'Income Group')]
mapply(quantiled_data,merged)


#=======================
data <- read_csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv')
                    

splitted <- strsplit (names(data), 'wgtp')
splitted[123]

#=======================
gdpData <- read_csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv', skip = 5, 
                    col_names = c('CountryCode', 'Ranking', 'xxx', 'Country',  'GDP') )

gdpData$xxx <- NULL
gdpData$GDP <- as.numeric(gsub('[$,]', '', gdpData$GDP))

grep("^United",gdpData$Country)

#======================

amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
yearsInt <- as.integer(year(sampleTimes))
length(yearsInt[yearsInt == 2012])


#=======================


# # reading geo-spatial data
# library(rgdal)
# data.shape<-readOGR(dsn="C://Users/Lherbeur/Downloads/NGA_Roads_v1/Nigeria_roads_version1.shp")
# head(data.shape)

# 
# ymd(c("2010-10-10", "bananas"))
# 
# data('diamonds')
# ?diamonds

# select(LetterRecognition, contains("COT"))  #weird bhaviour. cr8s dataset with 0 columns n 20k rows
# 
# ggplot2::diamonds %>% 
#   print(n = 10, width = Inf)
#   
# ggplot(data = diamonds) +
#   geom_count(mapping = aes(x = cut, y = color))
# parse_date("01/02/15", "01/02/15", "01/02/15", "01/02/15", "01/02/15")
# summary(iris)
# 
# # 
# # ?mpg
# # ?geom_point
# tail(mpg)
# 
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# ggplot(data = mpg)
# dim(mpg)
# 
# ggplot(data = mpg) +
#   geom_point(mapping = aes(x = displ, y = hwy, color= class)) +
#   facet_wrap(~ class, nrow = 2)
# ggplot(data = mpg) + 
#   geom_point(mapping = aes(x = displ, y = hwy)) + 
#   facet_grid( ~ model )
# ggplot(data = mpg) +
#   geom_point(mapping = aes(x = displ, y = hwy)) +
#   facet_grid(cyl ~ .)




# swirl exercises
library(swirl)
library(dplyr)
dpath2csv <-"C:/Users/Lherbeur/Documents/R/win-library/3.4/swirl/Courses/Getting_and_Cleaning_Data/Manipulating_Data_with_dplyr/2014-07-08.csv"
mydf <- read.csv(path2csv, stringsAsFactors = FALSE)

# dim(mydf)
# packageVersion('dplyr')

cran <- tbl_df(mydf) #converts to a tibble niii..mschew!
rm(mydf)

select(cran, ip_id, package, country)
#select is a good way to do a select n  arrange, without the arrange. 

# select(cran, country:r_arch)
# select (cran, -time, -country, -version, ip_id, -package)
# select (cran, -(X:size))
# -5:20
# -

filter(cran, r_version <= "3.0.2" & country == "IN")
filter(cran, country == "US" | country == "IN")
?Comparison


filter(cran, size > 100500, r_os == "linux-gnu")
filter(cran, !is.na(r_version))


cran2 <-  select(cran, size:ip_id)
arrange(cran2, -ip_id)
arrange(cran2, country,desc(r_version), desc(ip_id))

cran3 <- select(cran, ip_id, package, size)
mutate(cran3, correct_size = size + 1000)

summarize(cran, avg_bytes = mean(size))

        
cran <- tbl_df(mydf)
rm(mydf)


cran
by_package <- group_by(cran, package)
by_package

summarize(by_package, mean(size))

quantile(pack_sum$count, probs = 0.99)
top_counts <- filter(pack_sum, count > 679)
View(top_counts)
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)

quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
View(top_unique)

top_unique_sorted <- arrange(top_unique, desc(unique))
View(top_unique_sorted)

View(result3)

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5)



# ==============================================
library(tidyr)

students

?gather

gather(students, sex, count, -grade)
students2

res <- gather(students2, sex_class, count, -grade)
res

?separate

# separate(res, sex_class, c('sex', 'class'), sep = 'e_')  
#if sep not stated, uses a non-alphanumeric char to sep

students3


?spread

library(readr)
parse_number('class5')

students4
?unique

passed
failed

passed <- passed %>%
  mutate(status = 'passed')

failed <- failed %>%
  mutate(status = 'failed')

bind_rows(passed, failed)


sat

?contains

#==================================================
Sys.getlocale("LC_TIME")

library(lubridate)

help(package = lubridate)


this_day <- today()
this_day


year(this_day)
month(this_day)
day(this_day)
wday(this_day, label = TRUE)


this_moment <- now()
this_moment

hour(this_moment)

my_date <- ymd("1989-05-17")
my_date

class(my_date)

ymd("1989 17 May")
mdy('March 12, 1975')
# dmy(25081985) or dmy('25081985')

ymd("192012")
ymd("1920-1-2")

dt1

ymd_hms(dt1)


hms("03:22:14")

dt2
ymd(dt2)

class(ymd(dt2))

update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment <- update(this_moment, hours = hour(now()), minutes = minute(now()), seconds = seconds(now()))


nyc <- now(tzone = 'America/New_York')
depart <- nyc + days(2)
depart

depart <- update(depart, hours = 17, minutes = 34)
depart

arrive <- depart + hours(15) + minutes(50)
arrive

?with_tz

arrive <- with_tz(arrive, tzone = 'Asia/Hong_Kong')
arrive

last_time <- mdy("June 17, 2008", tz = 'Singapore')
last_time

?interval

how_long <- interval(last_time, arrive)
as.period(how_long)

# =============================================
# remove all objects from global environment
# rm(list=ls(all=TRUE))

# =============================================

library(nlme)
library(lattice)
library(tidyverse)
library(datasets)

data(airquality)

class(xyplot(airquality$Ozone ~ airquality$Solar.R, airquality))


qplot(airquality$Wind, airquality$Ozone, data = airquality, geom = "smooth")

x<- as.tibble(airquality)
head(x)


# =============================================hierarchical clustering
# 
library(swirl)


x <- dist(dataFrame)

hc <- hclust(distxy)
plot (hc)
plot (x = as.dendrogram(hc))
# abline(h=1.5, col = 'blue')
# abline(h=0.4, col = 'red')
# abline(h=0.08, col = 'red')


abline(h=0.05, col = 'green')

dist(dFsm)
hclust(d = distxy)
heatmap(dataMatrix, col = cm.colors(25))
heatmap(mt)
mt

plot(denmt)
distmt


# =========================================k-means clustering
library(swirl)

points(cx,cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd = 2) #plot centroid points 


mdist(x, y, cx, cy)
class(distTmp)

apply(distTmp, 2, which.min)

points(x, y, pch = 19, cex = 2, col = cols1[newClust]) #plot points coloured by with cluster colour

tapply(y, newClust, mean)
y
x
newClust

points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2) #plot new centroid points 
newCx

mdist(x, y, newCx, newCy)

distTmp2


apply(distTmp2, 2, which.min)

points(x, y, pch = 19, cex = 2,  col = cols1[newClust2]) #plot points coloured by with cluster colour

tapply(x, newClust2, mean)
tapply(y, newClust2, mean)

points(finalCx, finalCy, col = cols1, pch = 9,  cex = 2, lwd=2) #plot new centroid points 

kmeans(dataFrame, centers = 3)
kmObj$iter
kmObj$cluster

plot(x, y, col = kmObj$cluster, pch=19, cex = 2)
points(kmObj$centers, col=c("black","red","green"), pch = 3, cex = 3, lwd = 3 )

plot(x, y, col =kmeans(dataFrame,6)$cluster, pch=19, cex = 2)


# =========================================dimension reduction
head(dataMatrix)

heatmap(dataMatrix)
myedit("addPatt.R")

source ("addPatt.R", local = TRUE)

mat
svd(mat)


matu %*% diag %*% t(matv)  #recombine svd split vectors to get prgi matrix mat

svd(scale(mat))
prcomp(scale(mat))

svd1$v[,1]
svd1$d

head(constantMatrix)

svd2$d

svd2$v[, 1:2]

dim(faceData)

a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])

myImage(a1)

a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])


myImage(a2)

myImage(svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]))

myImage(svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))


# =========================================clustering example
dim(ssd)
  
names(ssd[,562:563])

table(ssd$subject)
sum(table(ssd$subject))

table(ssd$activity)
sum(table(ssd$activity))

sub1 <- subset(ssd, subject == 1)
dim(sub1)
names(sub1[,1:12])

myedit("showXY.R")

showMe(1:6)


mdist <- dist(sub1[, 1:3])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))  # i guess ds is custom to swirl and not R

mdist <- dist(sub1[, 10:12])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))


svd1 <- svd(scale(sub1[,-c(562,563)]))
dim(svd1$u)

maxCon <- which.max(svd1$v[, 2])
mdist <- dist(sub1[, c(10:12, maxCon)])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))


names(sub1[maxCon]) #the maxCon - fBodyAcc.meanFreq...Z is the most contributor

kClust <- kmeans(sub1[, -c(562:563)], centers = 6)

table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562:563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)


dim(kClust$centers)
laying <- which(kClust$size==29)
plot(kClust$centers[laying,1:12], pch = 19, ylab = "Laying Cluster")
names(sub1[, 1:3]) # "tBodyAcc.mean...X" "tBodyAcc.mean...Y" "tBodyAcc.mean...Z" have the greatest effect

walkdown <- which(kClust$size==49)
plot(kClust$centers[walkdown, 1:12], pch = 19, ylab = "Walkdown Cluster")



# =========================================case study
library(tidyverse)
library(swirl)
library(dotCall64)

pm_data <- read.table('C://Users/Lherbeur/Documents/Projects/R/7.R Labs/daily_88101_1999/daily_88101_1999.csv')


# =========================================case study 2
library(tidyverse)
library(swirl)

library(dotCall64)


dim(pm0)
cnames
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]][wcol])
head(pm0)


x0 <- pm0$Sample.Value
str(x0)

mean(is.na(x0))  #gives the % of na values in the column...how?

names(pm1) <- make.names(cnames[[1]][wcol])
dim(pm1)
head(pm1)

x1 <- pm1$Sample.Value

mean(is.na(x1))
summary(x0)
summary(x1)


boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

negative <- x1<0

sum(negative, na.rm = TRUE) #no of negatv values
mean(negative, na.rm = TRUE)


dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), format = "%Y%m%d")

head(dates)

hist(dates[negative], "month")

str(site0)

both <- intersect(site0, site1)
both

head(pm0)
?intersect



cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

sapply(split(cnt0, cnt0$county.site), nrow) #number of readings taken by monitor
sapply(split(cnt1, cnt1$county.site), nrow)

pm0sub <- subset(cnt0, County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(cnt1, County.Code == 63 & Site.ID == 2008)

x0sub <- pm0sub$Sample.Value
x1sub <- pm1sub$Sample.Value

dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")



par(mfrow = c(1,2),  mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = TRUE), lwd = 2)

plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = TRUE), lwd = 2)


rng <- range(x0sub, x1sub, na.rm = TRUE)
rng


mn0 <- with (pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
str(mn0)

mn1 <- with (pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
str(mn1)


summary(mn0)
summary(mn1)


d0 <- data.frame(state =names(mn0), mean = mn0)
d1 <- data.frame(state =names(mn1), mean = mn1)


mrg <- merge(d0, d1, by= "state")
dim(mrg)
head(mrg)


with(mrg, plot(rep(1,52), mrg[,2], xlim = c(.5, 2.5)))
# We see a column of points at x=1 which represent the 1999 state means. For the second column
# | of points, again call with with 2 arguments. As before, the first is mrg. The second,
# | however, is a call to the function points with 2 arguments. We need to do this since we're
# | adding points to an already existing plot. The first argument to points is the set of x
# | values, rep(2,52). The second argument is the set of y values, mrg[,3]. Of course, this is
# | the third column of mrg. (We don't need to specify the range of x values again.)

with(mrg, points(rep(2,52), mrg[,3]))
# | We see a shorter column of points at x=2. Now let's connect the dots. Use the R function
# | segments with 4 arguments. The first 2 are the x and y coordinates of the 1999 points and
# | the last 2 are the x and y coordinates of the 2012 points. As in the previous calls specify
# | the x coordinates with calls to rep and the y coordinates with references to the appropriate
# | columns of mrg.

segments(rep(1,52), mrg[,2], rep(2,52), mrg[,3])
# We see from the plot that the vast majority of states have indeed improved their particulate
# | matter counts so the general trend is downward. There are a few exceptions. (The topmost
# | point in the 1999 column is actually two points that had very close measurements.)


mrg[mrg$mean.x < mrg$mean.y, ]


# =========================================Statistical inference
# =========================================swirl - Introduction 
library(swirl)



deck
mypdf
integrate(mypdf, lower = 0, upper = 1.6)



# =========================================

library(swirl)

expect_dice

dice_high

expect_dice(dice_high)
expect_dice(dice_low)

0.5 *(expect_dice(dice_high) + expect_dice(dice_low))

integrate(myfunc, 0, 2)

spop
mean(spop)

allsam

apply(allsam, 1, mean)

smeans

mean(smeans)




?qunif 

x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp
mean(temp['X'])
class(temp)



#=======================practical machine learning
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(tidyverse)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

concrete <- as_tibble(concrete)
head(concrete)
featurePlot(concrete, concrete$CompressiveStrength, labels = c('Cement', 'BlastFurnaceSlag', 'FlyAsh',
                                            'Water', 'Superplasticizer', 'CoarseAggregate',
                                            'FineAggregate', 'Age'))



hist(log(concrete$Superplasticizer), plot = T )

#=================

library(caret)
library(AppliedPredictiveModeling)

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


IL_cols <- grep("^IL", colnames(training), value = TRUE)
filtered_columns_ <- training[,IL_cols]
# pca = prcomp(filtered_columns_)
# 
# size(pca)
# pca[,1]

procd_data <- preProcess(filtered_columns_, method = "pca", thresh = 0.8)
procd_data$rotation

#we see here that there are 9 components required to achieve 90% of the variance



# =================================
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_cols <- grep("^IL", colnames(training), value = TRUE)
IL_cols <- c(IL_cols, "diagnosis")
filtered_columns_ <- training[,IL_cols]
df <- data.frame(IL_cols)

# scaler <- preProcess(filtered_columns_, method = c('center', 'scale', 'knnImpute'))
# filtered_columns_ <- predict(scaler, filtered_columns_)
# 
# filtered_columns__ <- testing[,IL_cols]
# scaler <- preProcess(filtered_columns__, method = c('center', 'scale', 'knnImpute'))
# filtered_columns__ <- predict(scaler, filtered_columns__)
# 
# model <- train(diagnosis ~ ., data=training, method = "glm", preProcess(""knnIMpute))
# predctns <- predict(model, training, na.action = na.pass)
# confusionMatrix(predctns, testing$diagnosis)$overall['Accuracy']
# 
# 
# 
# 


modelFit <- train(diagnosis ~ ., method = "glm", preProcess = c("pca"),  
                  data = filtered_columns_, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, filtered_columns__))
print(C2)












modelFit <- train(diagnosis ~ ., method = "glm", data = training)
predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)



modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)




library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)


data(segmentationOriginal)

set.seed(125)
trainIndex <- createDataPartition(segmentationOriginal$Case, p=0.8, list = FALSE, times=1)
trainData <- segmentationOriginal[trainIndex, ]
testData <- segmentationOriginal[-trainIndex, ]

myModel <- train(Case ~ ., data = trainData, method = 'rpart')
predctns <- predict(myModel, testData)
confusionMatrix(predctns, testData$Case)$overall['Accuracy']

print(myModel)
testData <- as.data.frame(list(23000, 10, 2))
names(testData) <- c('TotalIntench2', 'FiberWidthCh1', 'listPerimStatusCh1')
predctns <- predict(myModel, testData)



library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
head(newdata)

myModel <- train(Area  ~ ., data = olive, method = 'rpart')
predctns <- predict(myModel, newdata)
confusionMatrix(predctns, olive$Area)$overall['Accuracy']
predctns



library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# trainSA$chd <- as.integer(trainSA$chd)
# testSA$chd <- as.integer(testSA$chd)


set.seed(13234)
myModel <- train(chd  ~ age + alcohol + obesity + tobacco + typea + ldl
                 , data = trainSA, method = 'glm', family="binomial")
predctns <- predict(myModel, trainSA)
# confusionMatrix(predctns, testSA$chd)$overall['Accuracy']

missClass(trainSA$chd, predctns)



myModel <- train(chd  ~ age + alcohol + obesity + tobacco + typea + ldl
                 , data = testSA, method = 'glm', family="binomial")
predctns <- predict(myModel, testSA)
# confusionMatrix(predctns, testSA$chd)$overall['Accuracy']


missClass(testSA$chd, predctns)


missClass = function(values,prediction)
  {sum(((prediction > 0.5)*1) != values)/length(values)}




library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

myModel <- train(y ~ ., data = vowel.train, method = 'rf')

varImp(myModel)


#========================================
library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)

data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

myRFModel <- train(y ~ ., data = vowel.train, method = 'rf')
myGBModel <- train(y ~ ., data = vowel.train, method = 'gbm')

rfPredctns <- predict(myRFModel, vowel.test)
gbPredctns <- predict(myGBModel, vowel.test)


confusionMatrix(rfPredctns, vowel.test$y)$overall['Accuracy']
confusionMatrix(gbPredctns, vowel.test$y)$overall['Accuracy']







set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)
mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(mod_rf, testing)
pred_gbm <- predict(mod_gbm, testing)
pred_lda <- predict(mod_lda, testing)
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)


confusionMatrix(pred_rf, testing$diagnosis)$overall['Accuracy']
confusionMatrix(pred_gbm, testing$diagnosis)$overall['Accuracy']
confusionMatrix(pred_lda, testing$diagnosis)$overall['Accuracy']
confusionMatrix(combPred, testing$diagnosis)$overall['Accuracy']






set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)
library(elasticnet)
mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)









library(lubridate) # For year() function below

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)


library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]





set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)












