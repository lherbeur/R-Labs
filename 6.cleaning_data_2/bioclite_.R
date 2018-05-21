# 
# source('http://bioconductor.org/bioclite.R')
# bioclite('rhdf5')



con <- url ('https://www.facebook.com/')
htmlCode <- readLines(con)
close(con)
htmlCode