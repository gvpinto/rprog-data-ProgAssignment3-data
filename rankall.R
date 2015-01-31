rankall <- function(outcome, num = "best") {
    
    ##setwd('/Users/gvpinto/R/rprog-data-ProgAssignment3-data')
	## source('best.R')
	## best("TX", "heart attack")
	
	## Validate the num
    aRank <- as.numeric()
	maxRanks <- as.numeric()
	noRank <- FALSE
	
    if (is.character(num) && (num == "best" || num == "worst")) {
        if (num == "best") {
            aRank <- 1
        } else {
            aRank <- 0
			noRank <- TRUE 
        }
    } else if(is.numeric(num)) {
        aRank <- num
    } else {
        stop("invalid num")
    }

	## Check if there are arguments
    if (!hasArg(outcome)) {
        outcome <- c("NA")
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
	
	## Get a list of data.frames split by State
	splitByState <- split(measures, measures$State)
	
	## Let total count in the list i.e. total states	
	stateCount <- length(splitByState)
	
	
	## Pre Allocating
	resultDf <- data.frame(hospital = character(stateCount), state = character(stateCount), stringsAsFactors=FALSE)
	
	
	for (i in 1:stateCount) {
				
		## Get the State
		state <- names(splitByState[i])
		hospital <- as.character()
		
		## Get the data.frame for the state
		measures <- splitByState[[i]]
		
		## Filter the Columns
		measures <- measures[columns]
		
		## Convert to NA for non numeric
		measures[, columns[3]] <- as.numeric(measures[, columns[3]])
		
		## Remove NA's
		measures <- measures[complete.cases(measures), ]
		
		## Get the Final Count
		maxRank <- nrow(measures)
		
		## If worst is requested then set the last record
		
		if (noRank == TRUE) {
			aRank <- maxRank
		}
	
	
		if (aRank > maxRank) {
			hospital <- "NA"
		} else {
			measures <- measures[order(measures[columns[3]], measures[columns[1]]), ]			
			hospital <- measures[aRank, columns[1]]
						
		}
		
		resultDf[i, "hospital"] <- hospital
		resultDf[i, "state"] <- state
		
	}
	

	## Sort by State
	resultDf <- resultDf[order(resultDf["state"]), ]
	rownames(resultDf) <- resultDf[, "state"]
	resultDf
    
}


