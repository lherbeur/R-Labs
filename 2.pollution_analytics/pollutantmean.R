library (stringi)


############################# first question - pollutant mean
# pollutantmean(c("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata"), "nitrate", 23)
pollutantmean("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata", "nitrate")

pollutantmean <- function(directory = c("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata"), pollutant = c("nitrate"), ids = 1:332)
{
  alldata <- list()
  
  for (id in ids)
  {
    if (stri_length(id) < 3)
    {
      id <- getPaddedId(id)
    }
    
    file_path <- paste(paste(directory[1], id, sep = "/"), "csv", sep = ".")
    alldata[[length(alldata) + 1]] <- getFileData(file_path, pollutant[1])

  }
  
  sum <- 0 
  row_count <- 0 
  
  for (data in alldata)
  {
    sum <- sum + data[[1]]
    row_count <- row_count + data[[2]]
  }
  
  mean <- sum/ row_count
  print(mean)
}


getPaddedId <- function(id)
{
  while (stri_length(id) < 3)
  {
    id <- paste("0", id, sep = "")
  }
  id
}


getFileData <- function(file_path, pollutant)
{
  pm_data <- read.table(file_path, header = TRUE, sep = ",")
  clean_data <- subset(pm_data, !is.na(pm_data[pollutant]),  select = pollutant)
  
  list(sum(clean_data), nrow(clean_data))
}

