cr <- corr("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata", 2000)                
cr <- sort(unlist(cr))
n <- length(cr)  

cr <- corr("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata", 1000)             
cr <- sort(unlist(cr))
print(c(n, round(cr, 4)))
              


corr <- function(directory = c("/Users/olawunmi.george/Documents/R/pollution_analytics/specdata"), threshold = 0)
{
   corr_coefs <- list()
   file_paths <- list.files(directory)
   
   for (file_path in file_paths)
   {
     full_path <- paste(directory, file_path, sep = "/")
     
     corr_coefs [[length(corr_coefs) + 1]] <- getFileData(full_path, threshold)
   }
    # print (head(corr_coefs))
    # summary(corr_coefs)
   corr_coefs
}

getFileData <- function(file_path, threshold)
{
  pm_data <- read.table(file_path, header = TRUE, sep = ",")
  clean_data <- na.omit(pm_data)
  
  if (nrow(clean_data) > threshold)
  {
    sulfate_data <- subset(clean_data, select = "sulfate")
    nitrate_data <- subset(clean_data, select = "nitrate")
    
    corr_coeff <- cor(sulfate_data[,1], nitrate_data[,1])
  }
  
}




