#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicates the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    complete <- complete(directory, 1:332)
    # complete <- complete(directory, 1:10)
    ids <- complete$id[complete$nobs > threshold]
    ids <- as.numeric(ids)
    
    # print(head(complete))
    corr_vector <- rep(0, length(ids))
    j <- 1
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    #print(files_list)
    # dat <- data.frame()                                   #creates an empty data frame
    for (i in ids) {    
        # data <- read.csv(files_list[i])
        # data <- data[which(!is.na(data$sulfate) & !is.na(data$nitrate)), ]
        # corr_vector[j] <- cor(data$sulfate, data$nitrate)
        current_file <- read.csv(files_list[i])
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
        j <- j + 1
    }
    
    return(corr_vector)
}