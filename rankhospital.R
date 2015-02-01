rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ## Check for validness of input values
  
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid by looking the State vector in table
  if (!(state %in% outcomeFile$State )) {
    stop ("invalid state")
  }
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
  
  ## Tried this out after one of my colleagues recommended it but it gets an warning
  ## Warning message:
  ## In rankhospital("MN", "heart attack", 5000) : NAs introduced by coercion
  
  ## outcomeFile[,outcome_col] <- as.numeric(outcomeFile[,outcome_col])
              
  #Remove all NAs
  #Identify all instances with NO N/As 
  no_nas <- complete.cases(outcomeFile[,outcome_col])
  outcomeFile <- outcomeFile[no_nas,]
  
  
  ## Get a subset of items that are from the state
  st_frame <- outcomeFile[which (outcomeFile[,7] == state),]
  # attach(st_frame)
  rank_frame <- st_frame[order(as.numeric(st_frame[,outcome_col]),st_frame[,2], na.last=TRUE),]
  # detach(st_frame)
  
  ## Return the right value
  if (num == "best") {
    rank_frame[1,]$Hospital.Name
  } else if (num == "worst") {
    tail(rank_frame, n=1 )$Hospital.Name
  } else if (is.numeric(num) && num <= nrow(rank_frame)) {
    rank_frame[num,]$Hospital.Name
  }else {
    NA
  }
  
}

rankhospital("MN", "heart attack", 5000)