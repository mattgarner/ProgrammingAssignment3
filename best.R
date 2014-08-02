#This function finds the name of the hospital with the lowest mortality rate for a given outcome and state.
#Uses outcome-of-care-measures.csv in the working directory
#Takes a state (2 letter abreviation) and outcome ("heart attack","heart failure", "pneumonia")as input
#Outputs a character vector containing hospital name


best <- function(state, outcome){
	
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
		    #Get the name of the hospital within the subset which contains the lowest mortality value        
        besthospitalname <- statehospitaldata[which.min(statehospitaldata[[columns[outcome]]]), "Hospital.Name"]

  #Handle invalid input
	}else if (state %in% outcomedata$State != TRUE){
    		stop("invalid state")
	}else{
		    stop("invalid outcome")
	}

#Output name of the hospital within the selected state with lowest mortality for the selected outcome
return(besthospitalname)
}