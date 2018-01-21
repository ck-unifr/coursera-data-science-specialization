#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating 
    ## the name of the pollutant for which we will calculate the 
    ## mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## Note: Do not round the result!
    
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    #print(files_list)
    dat <- data.frame()                                   #creates an empty data frame
    for (i in id) {    
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
    #print(head(dat))
    #print(tail(dat))
    result = mean(dat[, pollutant], na.rm=TRUE)
    return(result)
    #return(round(result, 3)) 
}