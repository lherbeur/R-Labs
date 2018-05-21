# data source - https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip



library(tidyverse)
library(dplyr)



pm_data <- read_rds('C://Users/Lherbeur/Documents/Projects/R/12.EDA 1/pm_analysis/exdata_data_NEI_data/summarySCC_PM25.rds')
pm_data <- as.tibble(pm_data)

pm_data_classification <- read_rds('C://Users/Lherbeur/Documents/Projects/R/12.EDA 1/pm_analysis/exdata_data_NEI_data/Source_Classification_Code.rds')
pm_data_classification <- as.tibble(pm_data_classification)


# pm_data$year <- as.Date(as.character(pm_data$year), "%Y")
head(pm_data)

data_1999 <- pm_data[pm_data$year == 1999,]
data_2002 <- pm_data[pm_data$year == 2002,]
data_2005 <- pm_data[pm_data$year == 2005,]
data_2008 <- pm_data[pm_data$year == 2008,]

total_emissions_1999 <- sum(data_1999$Emissions, na.rm = T) 
total_emissions_2008 <- sum(data_2008$Emissions, na.rm = T) 

# since 1999 total emission is greater than 2008's,  total emissions decreased

rng <- range(data_1999$Emissions, data_2002$Emissions)
boxplot(data_1999$Emissions, data_2002$Emissions, ylim = rng)
        # data_2005$Emissions, data_2008$Emissions)

year_sum <- pm_data %>%
  group_by(year)  %>%
  summarise(total = sum(Emissions, na.rm = T))


ggplot(data = year_sum, aes(x = year, y = total)) + 
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Total Emissions", title = "Total emissions for the US")
  


# baltimore - decreased, as well. tho, went up and then came down
data_baltimore <- pm_data[pm_data$fips == "24510",]

data_baltimore_total <- data_baltimore %>%
  group_by(year)  %>%
  summarise(total = sum(Emissions, na.rm = T))

ggplot(data = data_baltimore_total, aes(x = year, y = total)) + 
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Total Emissions", title = "Total emissions for Baltimore City")



# 3. 
# All have seen consistent decreases; however, d point type saw an increase
# and then a decrease
pm_data$type <- as.factor(pm_data$type)


data_baltimore_total <- data_baltimore %>%
  group_by(year, type)  %>%
  summarise(total = sum(Emissions, na.rm = T))


p <- ggplot(data = data_baltimore_total, aes(x = year, y = total)) + geom_line() + geom_point()
p + facet_wrap(~type) +
labs(title = "Total emissions for Baltimore based on Type")



# coal combustion related sources
full_data <- merge(pm_data, pm_data_classification)  #both av the SCC, so that's d default


coal_data <- full_data[grepl("coal", full_data$Short.Name), c('Emissions', 'year')]
coal_data_grouped <- coal_data %>%
                      group_by(year) %>%
                      summarise(total = sum(Emissions, na.rm = T))

ggplot(data = coal_data_grouped, aes(x = year, y = total)) + geom_line() + geom_point() +
labs(x = "Year", y = "Total Emissions", title = "Total emissions from Coal combustion")



# motor vehicle...baltimore
veh_baltimore_data <- full_data[grepl("Veh", full_data$Short.Name) & full_data$fips == "24510", 
                       c('Emissions', 'year')]
veh_baltimore_data_grouped <- veh_baltimore_data %>%
  group_by(year) %>%
  summarise(total = sum(Emissions, na.rm = T))

ggplot(data = veh_baltimore_data_grouped, aes(x = year, y = total)) + geom_line() + geom_point() +
labs(x = "Year", y = "Total Emissions", title = "Total emissions from Vehicles in Baltimore City")


# motor california
veh_california_data <- full_data[grepl("Veh", full_data$Short.Name) & full_data$fips == "06037", 
c('Emissions', 'year')]
veh_california_data_grouped <- veh_california_data %>%
  group_by(year) %>%
  summarise(total = sum(Emissions, na.rm = T))


names(veh_baltimore_data_grouped) <- paste("baltimore", names(veh_baltimore_data_grouped), sep= ".")
names(veh_california_data_grouped) <- paste("california", names(veh_california_data_grouped), sep= ".")

merged_data <- data.frame(veh_baltimore_data_grouped, veh_california_data_grouped)
  

ggplot(data = merged_data) + 
  geom_point(aes(x = baltimore.year, y = baltimore.total), colour = "blue") +
  geom_line(aes(x = baltimore.year, y = baltimore.total),  colour = "blue") + 
  geom_point(aes(x = california.year, y = california.total), colour = "red") +
  geom_line(aes(x = california.year, y = california.total), colour = "red") + 
  labs(x = "Year", y = "Total Emissions", title = "Total emissions from Vehicles in Baltimore City and California") 
 
# # adding legends 
# scale_colour_manual( values=c(California="red", Baltimore="blue"))
# legend("bottomleft", c('Baltimore', 'California'), col = c ("blue", "red"))





# sum_for_years <- tapply(pm_data$Emissions, pm_data$year, sum)
# sum_for_years
# years <- names(sum_for_years)
# sum <- sum_for_years[1]
# sum_for_years[[1]]
# x <- as.list(sum_for_years[1])
# names(x)

