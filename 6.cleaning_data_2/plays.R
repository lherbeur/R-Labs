# install.packages('rmysql')

library('RMySQL')
# conn <- dbConnect(MySQL(), user = 'genome', host='genome-mysql.cse.ucsc.edu')
# result <- dbGetQuery(conn, 'show databases;')
# result

conn <- dbConnect(MySQL(), user = 'genome', host='genome-mysql.cse.ucsc.edu', db = 'hg19')
result <- dbGetQuery(conn, 'show tables;')
dbClearResult(result)
result1 <- dbListFields(conn, 'HInv')
dbClearResult(result1)
query <- dbSendQuery(conn, 'select * from affyU133Plus2 where mismatches between 1 and 3') #where mrnaAcc 
affyMis <- dbFetch(query)
dbClearResult(affyMis)
quantile(affyMis$misMatches)

affyMisSmaller <- dbFetch(query, n = 10)

dim(affyMisSmaller)

dbDisconnect(conn)