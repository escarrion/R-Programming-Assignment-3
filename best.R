
# This function allows a user to specify a state and an outcome
# and it will return the best hospital in a state for that particular
# outcome

# pertinent Columns (name:2, state:7, attack:11, failure:17, pneumonia:23, rank:47)


best <- function(state, outcome){
    valid_outcomes <- list('heart attack', 'heart failure', 'pneumonia')
    
    
    #read in the csv
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = 'character', na.strings = c('Not Available'))
    
    
    #check validity of the input
    if(state %in% outcomes[,7] == FALSE){
        stop('invalid state')
    }
    if(outcome %in% valid_outcomes == FALSE){
        stop('invalid outcome')
    }
    
    
    # find the best hospital for the given outcome in the state
   
    state_outcome <- subset(outcomes, State == as.character(state))
    
    attack <- as.numeric(state_outcome[,11])
    failure <- as.numeric(state_outcome[,17])
    pneumonia <- as.numeric(state_outcome[,23])
    
    # sort by given outcome
    
    if(outcome == valid_outcomes[1]){
        state_outcome <- state_outcome[order(attack),]
    }
    else if(outcome == valid_outcomes[2]){
        state_outcome <- state_outcome[order(failure),]
    }
    else
        state_outcome <- state_outcome[order(pneumonia),]

           
    # return the hospital with the best measure
    state_outcome[1,2]
    
}
