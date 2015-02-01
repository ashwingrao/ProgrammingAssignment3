options(stringsAsFactors = FALSE)
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check for valid outcomes
  supported_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% supported_outcomes )) {
    stop ("invalid outcome")
  }
  
  outcome_col <- NA
  if (outcome == "heart attack") {
    outcome_col <- 11
  }else if (outcome == "heart failure") {
    outcome_col <- 17
  }else if (outcome == "pneumonia") {
    outcome_col <- 23
  }
  
  ## Check validness of the positional value
  
  if (!(num %in% c("best","worst"))) {
    if (!is.numeric(num)) {
      stop("invalid num")
    }
  }
  
  ## Change all values labeled as "Not Available" as NA
  outcomeFile[outcomeFile[,outcome_col] == "Not Available",][outcome_col] <- NA
  
  #Remove all NAs
  #Identify all instances with NO N/As 
  no_nas <- complete.cases(outcomeFile[,outcome_col])
  
  ## Create a new dataframe with three relevant columns... 
  ## Col 1: Hospital.Name
  ## Col 2: State
  ## Col 3: The desired outcome
  mod_outcomeFile <- outcomeFile[no_nas,c(2,7,outcome_col)]
  
  ## Factor the resulting dataFrame by States
  factor_by_state <- factor(mod_outcomeFile$State)
  
  ret <- data.frame()                             #creates an empty data frame
  states <- levels(factor_by_state)
  #print(class(states))
  ## print(states)
  
  ## For each state, find the hospital of the given rank
  for(st in states) {
    val <- NA
    ## Get a subset of items that are from the state
    st_frame <- mod_outcomeFile[which (mod_outcomeFile$State == st),]
    rank_frame <- st_frame[order(as.numeric(st_frame[,3]),st_frame[,1], na.last=TRUE),]
    ## Return the right value
    if (num == "best") {
      val <- c(rank_frame[1,]$Hospital.Name, st)
    } else if (num == "worst") {
      val <-  c(tail(rank_frame, n=1 )$Hospital.Name,st)
    } else if (is.numeric(num) && num <= nrow(rank_frame)) {
      val <-  c(rank_frame[num,]$Hospital.Name,st)
    }else {
      val <-  c(NA,st)
    }
    ret <-  rbind(ret, val)
    
  }
  names(ret) <- c("hospital", "state")
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ret
}
