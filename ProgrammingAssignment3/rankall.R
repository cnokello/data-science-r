options(warn = -1)
source('best.R')

# outcome = name of outcome
# num = rank of hospital. Can be an integer, 'best' or 'worst'
rankall <- function(outcome, num = 'best') {
        
        ## Load outcome data
        all_outcomes <- loadOutcomes()
        
        all_states <- unique(all_outcomes$State)
        states <- character()
        ranks <- character()
        for(state in all_states) {
                # print(all_outcomes[all_outcomes$State == state, ])
                states <- c(states, state)
                hospital_rank <- getRankedHospital(all_outcomes[all_outcomes$State == state, ], state, outcome, num)
                ranks <- c(ranks, hospital_rank)
        }
        
        df_state_ranking <- data.frame(hospital = ranks, state = states, row.names = states)
        df_state_ranking[order(rownames(df_state_ranking)), ]
        # print(df_state_ranking)
        
}

getRankedHospital <- function(all_outcomes, state, outcome, num) {
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