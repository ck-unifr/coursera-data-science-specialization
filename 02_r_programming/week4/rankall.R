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

## Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    
    outcome <- find_name(data, outcome)
    if (!(outcome %in% names(data))) {
        stop(paste("invalid outcome ", outcome))
    }
    data[, outcome] <- as.numeric(data[, outcome])
    vec_state <- unique(sort(data$State))
    #vec_hospital <- rep("",length(vec_state))
    vec_hospital <- character(length(vec_state))
    i = 0
    for (state in vec_state) {
        newdata <- data[data$State==state,]
        newdata <- newdata[order(newdata[outcome], newdata["Hospital.Name"]),]
        if (tolower(num) == "best") {
            num <- 1
        } else if (tolower(num) == "worst") {
            num <- nrow(newdata)
        }
        vec_hospital[i] = newdata[,2][num]
        i <- i + 1
    }
    
    df <- data.frame(hospital=vec_hospital, state=vec_state)
    rownames(df) <- df$state
    return(df)
}


