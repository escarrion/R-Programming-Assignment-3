# rank all allows a user to find the best hospital in all states
# for a given outcome
# pertinent Columns (name:2, state:7, attack:11, failure:17, pneumonia:23)

rankall <- function(outcome, num = 'best'){
    valid_outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
    
    #read in the csv
    outcomes <-read.csv('outcome-of-care-measures.csv',colClasses = 'character',na.strings = c('Not Available'),stringsAsFactors = FALSE)
    
    #check validity of the input
    #if(state %in% outcomes[,7] == FALSE){
        #stop('invalid state')
    #}
    if(outcome %in% names(valid_outcomes) == FALSE){
        stop('invalid outcome')
    }
    
    # Initialize the data frame the final data will be added to. 

    result_frame <- data.frame(matrix(nrow=0,ncol=2))
    colnames(result_frame) <- c('Hospital.Name', 'State')
   
    # order the data, then remove NA's for each of the state dataframes.  
    for(name in names(valid_outcomes)){
        
        if(outcome == name){
            split_outcomes <- split(outcomes, outcomes$State)
            
            for(i in 1:length(split_outcomes)){
                state_data <- split_outcomes[[i]]
                
                state_data_3col <- state_data[ ,c(2,7,valid_outcomes[name])]
     
                state.complete <- state_data_3col[complete.cases(state_data_3col), ]
                
                state_ordered <- state.complete[order(as.numeric(state.complete[,3]), state.complete[1]), ]
                
                # at this point, the new DF is just three columns - name, state, and outcome
                # na's have been removed and it's been ordered by outcome and hospital name
                # now, we just need to append the data to the result_frame
                
                
                if(num == 'best'){
                    result_frame <- rbind(result_frame, state_ordered[1,c(1,2)])
                }
                else if(num == 'worst'){
                    result_frame <- rbind(result_frame, state_ordered[nrow(state_ordered),c(1,2)])
                    
                }  
                else result_frame <- rbind(result_frame, state_ordered[num, c(1,2)])           
                
            }
        }    
     }
    return(result_frame)
}  

