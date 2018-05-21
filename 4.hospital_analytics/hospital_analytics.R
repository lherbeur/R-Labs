
library(stringi)
setwd("~/Downloads/ProgAssignment3-data")

hosp_data <- read.table("outcome-of-care-measures.csv", header = TRUE, sep = ",")
# sapply(hosp_data, class)
# head(hosp_data)
# nrow(hosp_data)
# ncol(hosp_data)
# names(hosp_data)
# colClasses = c("factor", "factor",  "factor",  "logical", "logical", "factor",  "factor",  "integer", "factor",  "numeric",
#                "numerical", "factor",  "factor",  "factor",  "factor",  "factor",  "numeric", "factor",  "factor",  "factor",
#                "factor",  "factor" , "numeric",  "factor", "factor",  "factor",  "factor",  "factor",  "factor",  "factor", 
#                "factor",  "factor",  "factor",  "factor",  "factor",  "factor",  "factor",  "factor",  "factor",  "factor",
#                "factor" , "factor",  "factor",  "factor",  "factor",  "factor") 

# numeric_death_rate <- as.numeric(hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
# hist(numeric_death_rate, plot = TRUE)
# min(hosp_data$outcome_name, na.rm = TRUE)


best <- function(state_name = "AZ", outcome_name = "heart attack")
{
  ## Read outcome data
  outcome_options <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state_name %in% hosp_data$State)
  {
    return("invalid state")
  }
  if (!outcome_name %in% outcome_options)
  {
    return("invalid outcome")
  }
  
  if (outcome_name == "heart attack")
  {
    hospitals <- hosp_data[hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(as.numeric(as.character(hosp_data[hosp_data$State == state_name, 11])), na.rm = TRUE) & hosp_data$State == state_name, 2]
    best <- sort(as.vector(hospitals))
    return(best[1])
  }
  else if (outcome_name == "heart failure")
  {
    hospitals <- hosp_data[hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(as.numeric(as.character(hosp_data[hosp_data$State == state_name, 17])), na.rm = TRUE) & hosp_data$State == state_name, 2]
    best <- sort(as.vector(hospitals))
    return(best[1])
  }
  else if (outcome_name == "pneumonia")
  {
    hospitals <- hosp_data[hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(as.numeric(as.character(hosp_data[hosp_data$State == state_name, 23])), na.rm = TRUE) & hosp_data$State == state_name, 2]
    best <- sort(as.vector(hospitals))
    return(best[1])
  }
 
}

rankhospital <- function(state_name = "AZ", outcome_name = "heart attack", ranking = 2)
{
  ## Read outcome data
  outcome_options <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state_name %in% hosp_data$State)
  {
    return("invalid state")
  }
  if (!outcome_name %in% outcome_options)
  {
    return("invalid outcome")
  }
  
  by_state <- split(hosp_data, hosp_data$State)[[state_name]]
  
  if (outcome_name == "heart attack")
  {
    ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                       by_state$Hospital.Name, na.last = FALSE), ]
   
    # print(ordered_by_state[70:90,])
    # print(nrow(ordered_by_state))
    
    if (class(ranking) == "character")
    {
      ranking <- if (ranking == "best")
      {
        1
      }
      else if (ranking == "worst")
      {
        nrow(ordered_by_state)
      }
    }
    return (ordered_by_state[ranking, ])
  }
  else if (outcome_name == "heart failure")
  {
    ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                       by_state$Hospital.Name, na.last = FALSE), ]
    
    # print(ordered_by_state[1:20,])
    
    
    if (class(ranking) == "character")
    {
      ranking <- if (ranking == "best")
      {
        1
      }
      else if (ranking == "worst")
      {
        nrow(ordered_by_state)
      }
    }
   
    return (ordered_by_state[ranking, ])
  }
  else if (outcome_name == "pneumonia")
  {
    ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                       by_state$Hospital.Name, na.last = FALSE),]
    
    print(ordered_by_state[1:11,])
    
     if (class(ranking) == "character")
     {
      ranking <- if (ranking == "best")
      {
        1
      }
      else if (ranking == "worst")
      {
        nrow(ordered_by_state)
      }
     }
    
    return (ordered_by_state[ranking, ])
  }
  
}

rankall <- function(outcome_name = "heart attack", ranking = 2)
{
  ## Read outcome data
  outcome_options <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that outcome is valid 
  if (!outcome_name %in% outcome_options)
  {
    return("invalid outcome")
  }
  
  df <- data.frame(hospital = character(), state = character())
  state_split <- split(hosp_data, hosp_data$State)

  for (state in levels(hosp_data$State))
  {
    by_state <- state_split[[state]]
    print(state)
    
    if (outcome_name == "heart attack")
    {
      ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                         by_state$Hospital.Name, na.last = FALSE), ]
      if (class(ranking) == "character")
      {
        ranking <- if (ranking == "best")
        {
          1
        }
        else if (ranking == "worst")
        {
          nrow(ordered_by_state)
        }
      }
      
      best_hospital <- ordered_by_state[ranking, 2]
      print(best_hospital)
      # rank <- hospState[nrow(hospState), 2]
      
      # rank <- hospState[nrow(hospState), 2]
    }
    else if (outcome_name == "heart failure")
    {
      ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                         by_state$Hospital.Name, na.last = FALSE), ]

      if (class(ranking) == "character")
      {
        ranking <- if (ranking == "best")
        {
          1
        }
        else if (ranking == "worst")
        {
          nrow(ordered_by_state)
        }
      }

      best_hospital <- ordered_by_state[ranking, 2]
      print(best_hospital)
    }
    else if (outcome_name == "pneumonia")
    {
      ordered_by_state <- by_state[order(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                         by_state$Hospital.Name, na.last = FALSE),]

      if (class(ranking) == "character")
      {
        ranking <- if (ranking == "best")
        {
          1
        }
        else if (ranking == "worst")
        {
          nrow(ordered_by_state)
        }
      }

      best_hospital <- ordered_by_state[ranking, 2]
      print(best_hospital)
    }
    
    df <- rbind(df, data.frame(hospital = best_hospital, state = state))
  }
  print(df)
}

# best("KK", "pneumonia")
# rankhospital("TX", "pneumonia", ranking = 1)  #sort out d Not availables not coerced to NA
# rankall(outcome_name = "pneumonia", ranking = 2 )


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)