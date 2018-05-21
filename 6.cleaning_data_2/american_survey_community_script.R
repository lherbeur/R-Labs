
##======================sql df package
# library('sqldf')
# 
# data_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\6.cleaning_data_2\\american_survey_community.csv'
# acs <- read.csv(data_path, header = TRUE)

# unique(acs$AGEP)
# sqldf("select distinct AGEP from acs")





##==================count chars
# con <- url('http://biostat.jhsph.edu/~jleek/contact.html')
# html_text <- readLines(con, n=-1L)
# nchar(html_text[10])
# nchar(html_text[20])
# nchar(html_text[30])
# nchar(html_text[100])

##==================gov_indices number
# cat(file=ff, "123456", "987654", sep="\n")
data_path <- 'C:\\Users\\Lherbeur\\Documents\\R\\6.cleaning_data_2\\gov_indices.for'
column_4 <- read.fwf(data_path, width=c(-10,-5,-4,-4,-5,4,-4,-5,-4,-4,-5,-4,-4), header = TRUE, skip = 3)   
column_4_list <-  lapply(k, as.character)
column_4_list <- as.numeric(unlist(column_4_list))
# column_4_list
class(column_4_list)
sum(column_4_list, na.rm = TRUE)





