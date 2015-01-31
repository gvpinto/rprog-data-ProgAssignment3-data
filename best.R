best <- function(state, outcome) {
    
    ##setwd('/Users/gvpinto/R/rprog-data-ProgAssignment3-data')
	## source('best.R')
	## best("TX", "heart attack")
    
    selectedOutcomeCols <- c()
    if (!hasArg(outcome)) {
        outcome <- c("NA")
	} else if (!hasArg(state)) {
		state <- c("NA")
	}
    ## msg <- paste(c("Error in best(\"", state, "\", \"", outcome, "\") : invalid "), collapse = "")
    careMeasures <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
        
    if (outcome == "heart attack") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if(outcome == "heart failure") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else if (outcome == "pneumonia") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    } else {
		stop("invalid outcome")
    }

	if (!any(careMeasures$State == state)) {
		stop("invalid state")	
	}
	
    careMeasures <- careMeasures[columns]
    careMeasures[, columns[3]] <- as.numeric(careMeasures[, columns[3]])
    careMeasures <- careMeasures[complete.cases(careMeasures), ]
    careMeasures <- careMeasures[careMeasures[columns[2]] == state, ] 
    careMeasures <- careMeasures[(careMeasures[columns[3]] == min(careMeasures[columns[3]])) , ]
	## careMeasures <- subset(careMeasures, (careMeasures[columns[3]] == min(careMeasures[columns[3]])) & (careMeasures[columns[2]] == state), drop=FALSE)
	
    if (nrow(careMeasures) > 1) {
		print('Found more than 1 row, Ordering')
    	careMeasures <- careMeasures[order(careMeasures[, columns[1]]), ]
    }
	## careMeasures
	## TODO check for nrows() > 1 and then sort if necessary
   	careMeasures[, columns[1]]
	
	
    ## filteredOutcome <- outcome[selectedOutcomeCols]
    
    ##setwd('/Users/gvpinto')
    
}


fileload <- function (columns, state) {
    ##careMeasures[columns[1]]
} 