# install.packages("XML")
library(XML)
data_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\5.cleaning_data_1\\restaurants.xml'
dat <- xmlParse(data_path)

df_name <- xmlToDataFrame(dat['//name'])
df_zipcode <- xmlToDataFrame(dat['//zipcode'])
df <- data.frame(df_name, df_zipcode, colnames('name', 'zipcode'))

df1 <- subset(df, na.rm = TRUE, df$text.1 == 21231)
nrow(df1)
       


# length(root1)
# 
# df <- data.frame(root1,row.names=NULL)
# df

# f <- xmlChildren(root)
# f
# length(f)
# # f1 subset(f, na.rm = TRUE, f$zipcode == 21231, select = c('zipcode'))
# length(f1)
# class(f1)
# 
# my_func <- function (f)
# {
#   list <- list()
#   org <- xmlSApply(x, xmlValue)
# 
#   class(org)
# }

