#This function finds the name of the hospital ranked num(th) for a given outcome and state.
#Uses outcome-of-care-measures.csv in the working directory
#Takes a state (2 letter abreviation), outcome ("heart attack","heart failure", "pneumonia")
#and rank ("best","worst", n) as input
#Outputs a character vector containing hospital name


rankhospital <- function(state, outcome, num = "best") {
  
  #Read data file
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Define valid outcome values
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  
  #Relate outcome values to their data file columns
  columns <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  
  #Convert data file column values to numerics (supress coersion warnings)
  suppressWarnings(outcomedata[,columns]<- as.numeric(as.matrix(outcomedata[,columns])))
  
  #Enable stop() error messages
  options(error = expression(NULL), warn = -0)
  
  #Check if input values are valid
  if (state %in% outcomedata$State == TRUE & outcome %in% valid.outcomes == TRUE){
          #Determine which columns will be needed from the data(depends on selected outcomes)
          requiredcolumns <- c("Hospital.Name",columns[[outcome]])
          #Subset the data to get only columns of interest from hospitals in the required state,
          #ignore records with NAs for outcome data
          statehospitaldata <- subset(outcomedata[,requiredcolumns], outcomedata$State == state & !is.na(outcomedata[,columns[outcome]]))
          #Convert text values of num to numerics, which correspond to rank when outcome values are arranged in ascending order
          if (num == "best") num <- 1
          if (num == "worst") num <- nrow(statehospitaldata)
          #If the requested rank does not exist (e.g. rank 5 in a set of 4 records) return NA
          if (nrow(statehospitaldata) < num){
            return(NA)}
          #Order data in the subset according to outcome measure(mortality rate), lowest value first
          statehospitaldata <- statehospitaldata[order(statehospitaldata[columns[outcome]], statehospitaldata["Hospital.Name"]),]
          #Get the name of the hospital with the required rank within the state
          rankedhospitalname <- statehospitaldata[num, "Hospital.Name"]  
    
  #Handle invalid input
  }else if (state %in% outcomedata$State != TRUE){
        stop("invalid state")
  }else{
        stop("invalid outcome")
  }
  #Output name of the hospital within the selected state which is ranked (num)th for outcome measure
return(rankedhospitalname)
}