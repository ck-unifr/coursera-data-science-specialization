find_name <- function(data, outcome) {
    outcome <- gsub(" ", ".", outcome)
    for (name in names(data)){
        if (startsWith(name, "Hospital.30.Day.Death..Mortality..Rates") 
            & endsWith(tolower(name), outcome)) {
            return(name)
        }
    }
    return("")
}

## Finding the best hospital in a state
best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death rate
    
    data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    
    if (!(state %in% data[ ,7])) {
        stop(paste("state ", state, " does not exist."))
    }
    outcome <- find_name(data, outcome)
    if (!(outcome %in% names(data))) {
        stop(paste("outcome ", outcome, " does not exist."))
    }
    data[, outcome] <- as.numeric(data[, outcome])
    newdata <- data[data$State==state,]
    newdata <- newdata[order(newdata[outcome]),]
    #min <- min(newdata, na.rm=T)
    #print(min)
    #min_index <- which(newdata == min)
    
    #return(newdata[,2][min_index])
    return(newdata[,2][1])
}


