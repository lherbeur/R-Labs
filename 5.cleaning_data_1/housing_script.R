
data_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\5.cleaning_data_1\\idaho_housing_data.csv'
housing <- read.csv(data_path, header = TRUE)
x <- subset(housing, na.rm = TRUE, housing$VAL == 24, select = c('VAL'))
nrow(x)
