## Reads 'outcome-of-care-measures.csv
## Returns a character vector of the name of the hospital with the 
## best/lowest 30-day mortality for the specified outcome
##
## state = 2-character abbreviation of a state
## outcome = name of outcome
best <- function(state, outcome) {
        state <- toupper(state)
        
        ## Read outcome data
        outcomes <- loadOutcomes()
        
        ## Check that state is valid
        state_valid <- F
        state_outcomes <- NULL
        if(isValidState(state, outcomes$State)) state_outcomes <- outcomes[outcomes$State == state, ]
        else stop('invalid state')
        
        
        ## Stop if coutcome column name specified is invalid
        matched_outcome_colname <- isValidOutcome(outcome, state_outcomes)
        if(matched_outcome_colname == 'F') stop('invalid outcome')
        
        
        
        ## Format the data
        f_state_outcomes <- state_outcomes[, c('Hospital.Name', eval(quote(matched_outcome_colname)))]
        f_state_outcomes[, eval(quote(matched_outcome_colname))] <- as.numeric(f_state_outcomes[, eval(quote(matched_outcome_colname))])
        colnames(f_state_outcomes) <- c('hospital', 'outcome')
        
        ## Remove NAs
        f_state_outcomes <- f_state_outcomes[!is.na(f_state_outcomes$outcome), ]
        f_hospital_lowest_mortality <- f_state_outcomes[f_state_outcomes$outcome == min(f_state_outcomes$outcome, na.rm = TRUE), ]
        f_hospitals <- f_hospital_lowest_mortality$hospital
        
        best_hospital <- NULL;
        if(length(f_hospitals) > 1) {
                f_hospitals <- sort(f_hospitals, decreasing = F)        
                best_hospital <- f_hospitals[1]
                
        } else best_hospital <- f_hospitals
        
        return(best_hospital)
}

loadOutcomes <- function() {
        outcomes <- read.csv('outcome-of-care-measures.csv', colClass = 'character')
}

isValidState <- function(state, allStates) {
        valid_state <- F
        if(toupper(state) %in% allStates) valid_state <- T
        valid_state
}

isValidOutcome <- function(outcome, state_outcomes) {
        cleanColumnName <- function(colname) {
                colname <- tolower(gsub('\\s+', '', colname))
                colname <- gsub('\\.', '', colname)
        }
        
        valid_outcome <- 'F'
        column_names <- names(state_outcomes)
        c_column_names <- lapply(column_names, cleanColumnName)
        df_colnames <- data.frame(cbind(column_names, c_column_names))
        colnames(df_colnames) <- c('real', 'clean')
        # print(class(df_colnames))
        
        ## Strip spaces from outcome
        matched_outcome_colname <- NULL
        outcome <- cleanColumnName(outcome)
        # print(outcome)
        
        for(outcome_colname in c_column_names) {
                if(grepl(outcome, outcome_colname)) {
                        #valid_outcome <- T                        
                        valid_outcome <- unlist(df_colnames[df_colnames$clean == outcome_colname, 'real'])
                        # print(matched_outcome_colname)
                        break
                }        
        }
        
        valid_outcome
}
