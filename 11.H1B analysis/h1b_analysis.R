library(dplyr)
# data source h-1b- https://www.foreignlaborcert.doleta.gov/pdf/PerformanceData/2018/H-1B_FY2018.xlsx
# data source green card - https://www.foreignlaborcert.doleta.gov/pdf/PerformanceData/2018/PERM_FY2018.xlsx

library(plyr)
library(tidyverse)
library(caret)
library(lubridate)
library(gridExtra)


# read in data from converted csv sheet
h1b_data <-  read.csv('C://Users/Lherbeur/Documents/Projects/R/11.H1B analysis/H-1B_FY2018.csv',header = T)
h1b_data <- as.tibble(h1b_data)


h1b_data$CASE_SUBMITTED <- as.Date(as.character(h1b_data$CASE_SUBMITTED), format = "%d/%m/%Y")

# number of 2017 & 2018
nrow(h1b_data[year(h1b_data$CASE_SUBMITTED) == 2017 | year(h1b_data$CASE_SUBMITTED) == 2018,])
h1b_data = h1b_data[year(h1b_data$CASE_SUBMITTED) == 2017 | year(h1b_data$CASE_SUBMITTED) == 2018,]

# select only the non-special h1-b class - shld not be
# h1b_data <- h1b_data[h1b_data$VISA_CLASS == 'H-1B',]

h1b_data <- select(h1b_data, 'CASE_STATUS', 'CASE_SUBMITTED', 'PW_WAGE_LEVEL', 'SOC_CODE', 'EMPLOYER_NAME', 'EMPLOYER_CITY',
                   'EMPLOYER_STATE', 'AGENT_REPRESENTING_EMPLOYER', 'AGENT_ATTORNEY_NAME',
                   'AGENT_ATTORNEY_STATE', 'FULL_TIME_POSITION','PW_WAGE_LEVEL',
                   'WILLFUL_VIOLATOR','H.1B_DEPENDENT')

# function for categorisation
getJobCategory <- function(soc_code)
{
  category <- sapply(soc_code, function(x)
  { 
    switch(x,
           '11' = 'MANAGERIAL, ADMIN',
           '13' = 'FINANCIALS, COMPLIANCE',
           '15' = 'COMPUTING, STATISTICIANS',
           '17' = 'ENGINEERING EXCEPT COMPUTERS',
           '19' = 'SCIENTISTS',
           '21' = 'PSYCHOLOGY, COUNSELLING, SOCIAL WORKS',
           '23' = 'LEGAL',
           '25' = 'EDUCATORS, CURATORS',
           '27' = 'DESIGNERS, COACHES',
           '29' = 'MEDICALS',
           '31' = 'HEALTHCARE ASSTS',
           '33' = 'SECURITY',
           '35' = 'CULINARY',
           '37' = 'CLEANING, KEEPING',
           '39' = 'RECEIPTIONISTS, SERVICE ATTENDANTS',
           '40' = 'ENGINEERING EXCEPT COMPUTERS',
           '41' = 'TRADERS, SALES REPS',
           '43' = 'QUALITY, STATISTICAL ASSTS',
           '45' = 'AGRICULTURAL',
           '47' = 'ARTISANS',
           '49' = 'SERVICE TECHNICIANS',
           '51' = 'MACHINISTS',
           '53' = 'TRANSPORT',
           '71' = 'ENGINEERING EXCEPT COMPUTERS',
           as.character(x)
    )
    
  })
}

# regroup to a wider classification, based on SOC_CODE
h1b_data <- h1b_data[grepl('^[0-9]{2}-', h1b_data$SOC_CODE),]
h1b_data$SOC_CODE <- substr(h1b_data$SOC_CODE, start = 1, stop = 2)
h1b_data <- h1b_data %>% 
            mutate(JOB_CATEGORY = getJobCategory(h1b_data$SOC_CODE)) 


# plots
# case statuses
ggplot(h1b_data, mapping = aes(x = CASE_STATUS, fill = CASE_STATUS)) +
  geom_bar(aes(y = ..count../1000)) + 
  labs(title = 'Case Status Proportions', x = 'Case Status', y =  'Count (000s)') 


nrow(h1b_data[h1b_data$CASE_STATUS == 'CERTIFIED',])
nrow(h1b_data[h1b_data$CASE_STATUS == 'CERTIFIED-WITHDRAWN',])
nrow(h1b_data[h1b_data$CASE_STATUS == 'WITHDRAWN',])
nrow(h1b_data[h1b_data$CASE_STATUS == 'DENIED',])

# wage levels
ggplot(h1b_data, mapping = aes(x = PW_WAGE_LEVEL, fill = PW_WAGE_LEVEL)) + 
  geom_bar(aes(y = ..count../1000)) +
  labs(title = 'Wage Level Proportions', x = 'Wage Level', y =  'Count (000s)')


wage_grouped_data <- h1b_data %>%
                      group_by(PW_WAGE_LEVEL) %>%
                      dplyr::summarize(count = n())

View(wage_grouped_data)

# findings
# Top job categories
job_grouped_data <- h1b_data %>%
  group_by(JOB_CATEGORY) %>%
  dplyr::summarize(count = n(), percent = round(100 * count / nrow(h1b_data), 3))

job_grouped_data <-  job_grouped_data[order(-job_grouped_data$count), ]

View(job_grouped_data)

# Top states
state_grouped_data <- h1b_data %>%
  group_by(EMPLOYER_STATE) %>%
  dplyr::summarize(count = n(), percent = round(100 * count / nrow(h1b_data), 3))

state_grouped_data <-  state_grouped_data[order(-state_grouped_data$count), ]

View(state_grouped_data)


# Top cities
city_grouped_data <- h1b_data %>%
  group_by(EMPLOYER_CITY, EMPLOYER_STATE) %>%
  dplyr::summarize(count = n(), percent = round(100 * count / nrow(h1b_data), 3)) 

city_grouped_data <-  city_grouped_data[order(-city_grouped_data$count), ]

View(city_grouped_data)


# Top employers
employer_grouped_data <- h1b_data %>%
  group_by(EMPLOYER_NAME) %>%
  dplyr::summarize(count = n(), percent = round(100 * count / nrow(h1b_data), 3)) 

employer_grouped_data <-  employer_grouped_data[order(-employer_grouped_data$count), ]

View(employer_grouped_data)




generatePdf <- function(data, file_name)
{
  pdf(height = 17, onefile = FALSE, file = paste("C://Users/Lherbeur/Documents/Projects/R/11.H1B analysis/", file_name, ".pdf", sep = ""))
  grid.table(data, cols = colnames(data))
  dev.off()
}


# summary(h1b_data, maxsum = 100)
# summary of statuses...certd, uncertd.


# plots
# plot(time_grouped_data$count ~ time_grouped_data$CASE_SUBMITTED, type = "l",
# ylab = "Number of applications", xlab = "Month of submission")

# ggplot(time_grouped_data, mapping = aes(x = format(time_grouped_data$CASE_SUBMITTED, "%m"), y = count)) +
#   geom_line() +
#   scale_x_datetime(date_breaks = "1 months") 

