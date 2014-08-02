rankhospital <- function(state, outcome, num = "best") {
  
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  
  #Map columns of interest
  columns <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  suppressWarnings(outcomedata[,columns]<- as.numeric(as.matrix(outcomedata[,columns])))
  
  options(error = expression(NULL), warn = -0)
  
  
  #Get hospital with specified rank/state
  if (state %in% outcomedata$State == TRUE &
        outcome %in% valid.outcomes == TRUE){
          requiredcolumns <- c("Hospital.Name",columns[[outcome]])
          statehospitaldata <- subset(outcomedata[,requiredcolumns], outcomedata$State == state & !is.na(outcomedata[,columns[outcome]]))
          if (num == "best") num <- 1
          if (num == "worst") num <- nrow(statehospitaldata)
          if (nrow(statehospitaldata) < num){
            return(NA)}
          statehospitaldata <- statehospitaldata[order(statehospitaldata[columns[outcome]], statehospitaldata["Hospital.Name"]),]
          rankedhospitalname <- statehospitaldata[num, "Hospital.Name"]  
    
  }else if (state %in% outcomedata$State != TRUE){
        stop("invalid state")
  }else{
        stop("invalid outcome")
  }

print(rankedhospitalname)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}