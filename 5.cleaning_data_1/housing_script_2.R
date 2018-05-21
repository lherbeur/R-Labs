library(data.table)

dest_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\5.cleaning_data_1\\housing_data_2.csv'
# file <- download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv', destfile = dest_path)
DT <- fread(dest_path, sep=",", sep2="auto")
# mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
# 
# mean(DT$pwgtp15,by=DT$SEX)
# 
# tapply(DT$pwgtp15,DT$SEX,mean)
# 
# DT[,mean(pwgtp15),by=SEX]
# 
# sapply(split(DT$pwgtp15,DT$SEX),mean)
# 
# rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]