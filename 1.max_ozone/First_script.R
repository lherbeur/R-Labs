# data1 <- file("hw1_data.csv", "r")
# x <- readLines(data1)
# x


data_link <- "hw1_data.csv"
data <-read.csv(data_link, header = TRUE)
x <- nrow(data)

data[(x - 1) : x, ]

na_values <- c(is.na(data$Ozone))
total_nas <- sum(na_values)
#nrow()
sum (data$Ozone, na.rm = TRUE) / (x-total_nas)

subset1 <- subset(data, data$Ozone > 31 & data$Temp > 90)

subset1_mean <- sum(subset1$Solar.R) /nrow(subset1)

month_subset <- subset(data, data$Month == 6)
month_subset_mean <- sum(month_subset$Temp) /nrow(month_subset)

month5_subset <- subset(data, na.rm = TRUE, data$Month == 5)
max_ozone <- max(month5_subset$Ozone, na.rm = TRUE)
                  
factor_vars <- factor(c("satisfactory", "not satisfactory", "very satisfactory",
                             "very unsatisfactory", "very unsatisfactory", 
                             "not satisfactory"), 
                      levels = c("satisfactory", "not satisfactory"))

classes <- sapply(data, class)
classes




#x <- c('a', 'b', 'c', 'd', 'e')
#
#if (length(x[is.na(x)]) == 0){
#  print('is 0')
#}
# else{
#   print('Not 0')  
# }

