setwd(dir="/home/adarsh/Rclass/ProgrammingAssignment3/")

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- data[c(2, 7, 11, 17, 23)]
data[, c(3, 4, 5)] <- sapply(data[, c(3, 4, 5)], as.numeric)

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    states <- unique(data$State)
    states <- sort(states)
    
    ranks <- data.frame(hospital=NA, state=NA)
    
    for (i in 1:length(states)) {
        ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
    }
    
    ranks
}

data <- data[order(data[, 4]), ]
data <- data[!is.na(data[, 4]), ]
