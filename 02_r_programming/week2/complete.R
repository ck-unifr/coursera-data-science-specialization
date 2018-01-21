#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating 
    ## the name of the pollutant for which we will calculate the 
    ## mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nods is the
    ## number of complete cases

    
    complete <- rep(0, length(id))
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    j <- 1 
    for (i in id) {
        data <- read.csv(files_list[i])
        complete[j] <- sum(complete.cases(data))
        j <- j + 1
    }
    
    return(data.frame(id = id, nobs = complete))
}