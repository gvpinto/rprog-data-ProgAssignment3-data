rankhospital <- function(state, outcome, rank) {
    
    ##setwd('/Users/gvpinto/R/rprog-data-ProgAssignment3-data')
	## source('best.R')
	## best("TX", "heart attack")
	
	## Validate the rank
    aRank <- as.numeric()
	maxRanks <- as.numeric()
	
    if (is.character(rank) && (rank == "best" || rank == "worst")) {
        if (rank == "best") {
            aRank <- 1
        } else {
            aRank <- 0 
        }
    } else if(is.numeric(rank)) {
        aRank <- rank
    } else {
        stop("invalid rank")
    }

	## Check if there are arguments
    if (!hasArg(outcome)) {
        outcome <- c("NA")
	} else if (!hasArg(state)) {
		state <- c("NA")
	}
    
	## Setting up the Columns
    if (outcome == "heart attack") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if(outcome == "heart failure") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else if (outcome == "pneumonia") {
        columns <- c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    } else {
		stop("invalid outcome")
    }
	
	
	## Data Load and Cleanup
    measures <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
	
	## Validate the State
	if (!any(measures$State == state)) {
		stop("invalid state")	
	}
	
	measures <- measures[columns]
	measures <- subset(measures, measures[columns[2]] == state, select=columns)
	measures[, columns[3]] <- as.numeric(measures[, columns[3]])
	measures <- measures[complete.cases(measures), ]
	maxRanks <- nrow(measures)
			
	if (aRank == 0) {
		aRank <- maxRanks
	}
	
	if (aRank > maxRanks) {
		return("NA")
	} else {
		measures <- measures[order(measures[columns[3]], measures[columns[1]]), ]
		measures[aRank, columns[1]]
	}
	
    
}
