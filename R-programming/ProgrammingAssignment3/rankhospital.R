setwd(dir="/home/adarsh/Rclass/ProgrammingAssignment3/")

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- data[c(2, 7, 11, 17, 23)]
data[, c(3, 4, 5)] <- sapply(data[, c(3, 4, 5)], as.numeric)

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    valid_states <- unique(data$State)
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% valid_states)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% valid_outcomes)) {
        stop("invalid outcome")
    }
    
    data2 <- data[data$State == state, ]
    
    if (outcome == "heart attack") {
        data2 <- data2[order(data2[, 3], data2[, 1]), ]
        data2 <- data2[!is.na(data2[, 3]), ]
    }
    else if (outcome == "heart failure") {
        data2 <- data2[order(data2[, 4], data2[, 1]), ]
        data2 <- data2[!is.na(data2[, 4]), ]
    }
    else {
        data2 <- data2[order(data2[, 5], data2[, 1]), ]
        data2 <- data2[!is.na(data2[, 5]), ]
    }
    
    if (num == "best") {
        num <- 1L
    }  
    else if (num == "worst") {
        num <- nrow(data2)
    }
    else {
        num <- as.numeric(num)
    }
    
    data2[num, 1]
}