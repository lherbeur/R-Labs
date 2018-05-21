library("xlsx")

data_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\5.cleaning_data_1\\natural_gas_data.xlsx'
dat <- read.xlsx(data_path, sheetIndex = 0, sheetName = 'NGAP Sample Data', startRow = 18, endRow = 23, colIndex = c(7:15))
# head(dat)

sum(dat$Zip*dat$Ext,na.rm=T)