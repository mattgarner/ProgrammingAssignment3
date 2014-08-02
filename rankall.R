#This function finds the name of the hospital in each state ranked (num)th for a given outcome and state.
#Uses outcome-of-care-measures.csv in the working directory
#Takes an outcome ("heart attack","heart failure", "pneumonia") and rank ("best", "worst", n) as input
#Outputs a data frame containing hospital and state


rankall <- function(outcome, num = "best") {
  
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
  
  #Vector of all states featured in data
  allstates <- unique(outcomedata$State)
  
  #Check if input values are valid
  if (outcome %in% valid.outcomes == TRUE){
      
      #Dataframe to holdd results
      results <- data.frame(hospital=factor(levels=unique(outcomedata$Hospital.Name)), state = factor(levels=unique(outcomedata$State)))
      
      
      for (state in allstates){    
            #Determine which columns will be needed from the data(depends on selected outcomes)      
            requiredcolumns <- c("Hospital.Name",columns[[outcome]])
            
            #Subset the data to get only columns of interest from hospitals in the required state,
            #ignore records with NAs for outcome data
            statehospitaldata <- subset(outcomedata[,requiredcolumns], outcomedata$State == state & !is.na(outcomedata[,columns[outcome]]))
            
            #Convert text values of num to numerics, which correspond to rank when outcome values are arranged in ascending order
            if (num == "best"){
                rank <- 1
            }
            else if (num == "worst"){
                rank <- nrow(statehospitaldata)
            }
            else {
              rank <- num
            }
            
                #If the requested rank does not exist (e.g. rank 5 in a set of 4 records) return NA
            if (nrow(statehospitaldata) < rank){
                rankedhospitalname <- NA
            }else {
                #Order data in the subset according to outcome measure(mortality rate), lowest value first
                statehospitaldata <- statehospitaldata[order(statehospitaldata[columns[outcome]], statehospitaldata["Hospital.Name"]),]
                #Get the name of the hospital with the required rank within the state
                rankedhospitalname <- statehospitaldata[rank, "Hospital.Name"]
            }
            #For each state, add a row to the results table containing state and the hospital with the specified rank
            results[nrow(results)+1, ] <- c(rankedhospitalname, state)      
      }
  
  #Handle invalid input
  }else if (state %in% outcomedata$State != TRUE){
        stop("invalid state")
  }else{
        stop("invalid outcome")
  }
  
#Order results alphabetically by state
results <- results[order(as.character(results$state)),]

#Return data frame containing hospital name and state. Each state gets one entry for the hospital ranked num(th)
#for the selected outcome
return(results)
}