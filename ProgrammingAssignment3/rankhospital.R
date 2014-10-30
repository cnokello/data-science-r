options(warn = -1)
source('best.R')
#
# state = 2-character of state
# outcome = name of outcome
# num = ranking of the hospital for outcome 'outcome'
#
rankhospital <- function(state, outcome, num = 'best') {
        state <- toupper(state)
        
        ## Read outcome data
        all_outcomes <- loadOutcomes()
        performRanking(all_outcomes, state, outcome, num)
}

performRanking <- function(all_outcomes, state, outcome, num) {
        if(!isValidState(state, all_outcomes$State)) stop('invalid state')
        
        state_outcomes <- all_outcomes[all_outcomes$State == state, ]
        matched_outcome_colname <- isValidOutcome(outcome, state_outcomes)
        if(matched_outcome_colname == 'F') stop('invalid outcome')
        
        ## Subset the data frame to only attributes we are interested in
        f_state_outcomes <- state_outcomes[, c('Hospital.Name', eval(quote(matched_outcome_colname)))]
        f_state_outcomes[, eval(quote(matched_outcome_colname))] <- 
                as.numeric(f_state_outcomes[, eval(quote(matched_outcome_colname))], ignoreWarnings = T)
        colnames(f_state_outcomes) <- c('Hospital.Name', 'outcome')
        f_state_outcomes <- f_state_outcomes[!is.na(f_state_outcomes$outcome), ]
        
        f_state_outcomes <- f_state_outcomes[with(f_state_outcomes, order(outcome, Hospital.Name)), ]
        f_state_outcomes <- cbind(f_state_outcomes, rank = 1:length(f_state_outcomes$outcome))
        
        hospital <- f_state_outcomes[f_state_outcomes$rank == 
                                             getRankNum(num, length(f_state_outcomes$outcome)), 
                                     'Hospital.Name']
        
        if(length(hospital) == 0) hospital <- NA
        return(hospital)
}

getRankNum <- function(num, size) {
        rank <- num
        if(is.na(as.numeric(num, ignoreWarnings = T))) {
                if(num == 'best') rank <- 1
                else if(num == 'worst') rank <- size
        }
        rank
}