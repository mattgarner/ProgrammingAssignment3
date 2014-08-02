best <- function(state, outcome){
	
	outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	valid.outcomes <- c("heart attack","heart failure","pneumonia")
  
  #Map columns of interest
  columns <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  
  
  #Convert numerics
	suppressWarnings(outcomedata[,columns]<- as.numeric(as.matrix(outcomedata[,columns])))

	options(error = expression(NULL), warn = -0)
	
	if (state %in% outcomedata$State == TRUE &
		outcome %in% valid.outcomes == TRUE){
		
		## Return hospital name in that state with lowest 30-day death
		## rate
		requiredcolumns <- c("Hospital.Name",columns[[outcome]])
		statehospitaldata <- subset(outcomedata[,requiredcolumns], outcomedata$State == state & !is.na(outcomedata[,columns[outcome]]))
		besthospitalname <- statehospitaldata[which.min(statehospitaldata[[columns[outcome]]]), "Hospital.Name"]

	}else if (state %in% outcomedata$State != TRUE){
		stop("invalid state")
	}else{
		stop("invalid outcome")
	}
print(besthospitalname)
}