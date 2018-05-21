
#fix the column names in d result later

######Question 
# set.seed(42)
# cc <- complete("specdata", 332:1)
# use <- sample(332, 10)
# print(cc[use, ])


# complete("specdata", 54)

complete <- function(directory = c("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata"), ids = 1:332)
{
  complete_data <- data.frame("Id" , "Count")
  data <- list()
  
  for (id in ids)
  {
    if (stri_length(id) < 3)
    {
      id <- getPaddedId(id)
    }
    file_path <- paste(paste(directory[1], id, sep = "/"), "csv", sep = ".")
    data[[length(data) + 1]] <- getFileData(file_path, id)
    
  }
  
  
  # names(data) <- c("Id" , "Count") 
  # complete_data <- as.data.frame(data, col.names("Id" , "Count")) 
  
  complete_data <- do.call(rbind, data) 
  # print (complete_data)
}


getPaddedId <- function(id)
{
  while (stri_length(id) < 3)
  {
    id <- paste("0", id, sep = "")
  }
  id
}

getFileData <- function(file_path, id)
{
  pm_data <- read.table(file_path, header = TRUE, sep = ",")
  clean_data <- na.omit(pm_data)
  
  list (id, nrow(clean_data))
}

