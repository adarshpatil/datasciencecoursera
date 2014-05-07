setwd(dir="/home/adarsh/Rclass/ProgrammingAssignment3/")

best <- function(state, outcome) {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    valid_states <- unique(data$State)
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% valid_states)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% valid_outcomes)) {
        stop("invalid outcome")
    }
    
    data2 <- data[data$State == state, ]
    data2[, c(11, 17, 23)] <- sapply(data2[, c(11, 17, 23)], as.numeric)
    data2 <- data2[order(data2[, 2]), ]
    
    if (outcome == "heart attack") {
        best <- data2[which.min(data2[, 11]), "Hospital.Name"]
    }
    else if (outcome == "heart failure") {
        best <- data2[which.min(data2[, 17]), "Hospital.Name"]
    }
    else {
        best <- data2[which.min(data2[, 23]), "Hospital.Name"]
    }
    
    best
}