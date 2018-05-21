library(dplyr)
library(plyr)
library(tidyverse)
library(caret)
library(lubridate)


# read in data from xlsx sheet
perm_data <- readxl::read_xlsx('C://Users/Lherbeur/Documents/Projects/R/11.H1B analysis/PERM_FY2018.xlsx')
names(perm_data)
# select just a few data variables
perm_data <- select(perm_data,
                   'COUNTRY_OF_CITIZENSHIP', 'FW_INFO_BIRTH_COUNTRY')

# birth country - saw usa in it. processing h1-b for us citizen. how?
emp_birth_country_grouped_data <- perm_data %>%
  group_by(COUNTRY_OF_CITIZENSHIP) %>%
  dplyr::summarize(count = n(), percent =  round(100 * count / nrow(perm_data), 3))

emp_birth_country_grouped_data <-  emp_birth_country_grouped_data[order(-emp_birth_country_grouped_data$count), ]

View(emp_birth_country_grouped_data)

# attorney- the bulk of applying companies have attorneys represntng them
attorney_grouped_data <- perm_data %>%
  group_by(AGENT_FIRM_NAME) %>%
  dplyr::summarize(count = n(), percent = 100 * count / nrow(perm_data))

attorney_grouped_data <-  attorney_grouped_data[order(-attorney_grouped_data$count), ]

# attorney state
attorney_state_grouped_data <- perm_data %>%
  group_by(AGENT_STATE) %>%
  dplyr::summarize(count = n(), percent = 100 * count / nrow(perm_data))

attorney_state_grouped_data <-  attorney_state_grouped_data[order(-attorney_state_grouped_data$count), ]
 

# class of admission
edu_level_grouped_data <- perm_data %>%
  group_by(FOREIGN_WORKER_INFO_EDUCATION) %>%
  dplyr::summarize(count = n(), percent = 100 * count / nrow(perm_data))

edu_level_grouped_data <-  edu_level_grouped_data[order(-edu_level_grouped_data$count), ]


