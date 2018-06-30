# this functio allows a user to specify a state, an outcome and a ranking
# and it returns the hospital in that state ranked at the level specified by the user


# pertinent Columns (name:2, state:7, attack:11, failure:17, pneumonia:23, rank:47)


rankhospital <- function(state, outcome, num = 'best'){
    valid_outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
   
    #read in the csv
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = 'character', na.strings = c('Not Available'), stringsAsFactors = FALSE)
    
    #check validity of the input
    if(state %in% outcomes[,7] == FALSE){
        stop('invalid state')
    }
    if(outcome %in% names(valid_outcomes) == FALSE){
        stop('invalid outcome')
    }
    
    
    # subset the frame for given state & rename columns
    state_data <- subset(outcomes, State == as.character(state))
    colnames(state_data)[c(2,7,11,17,23)] <- c('Name', 'State', 'Heart Attack', "Heart Failure", 'Pneumonia')
    
    # Rank & order based on the chosen outcome
    
    for(name in names(valid_outcomes)){
        
        if(outcome == name){
            state_data.2col <- state_data[, c(2,valid_outcomes[name])]
            
            state_data.complete <- state_data.2col[complete.cases(state_data.2col),]
            
            ordered_data <- state_data.complete[order(as.numeric(state_data.complete[,2]), state_data.complete[1]),]
            
            ranking <- 1:nrow(ordered_data)
            ordered_data$Rank <- ranking
            
        }    
    
    }    
    
    # at this point, the data is ordered and ranked by the outcome passed to the function
    # so we need to use that outcome as the filter for printing the rate we want.
     
    
    if(num =='best'){
        print(ordered_data[1, c(2, valid_outcomes[outcome], 47)])
    }
    else if(num == 'worst'){
        print(ordered_data[nrow(ordered_data), c(2, valid_outcomes[outcome], 47)])
    }
    else if(num > nrow(ordered_data)){
        return(NA)
    }
    else print(ordered_data[num, c(2, valid_outcomes[outcome], 47)])
      
}

     
   
        
          
       
