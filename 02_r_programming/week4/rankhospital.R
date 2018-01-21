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

## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
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
    newdata <- newdata[order(newdata[outcome], newdata[,2]),]
    
    if (tolower(num) == "best") {
        num <- 1
    } else if (tolower(num) == "worst") {
        num <- nrow(newdata)
    } 
    
    return(newdata[,2][num])
}


