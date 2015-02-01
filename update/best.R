

best <- function(state, outcome) {
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
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
  
  ## Identify all "Complete Cases"
  #good_list <- complete.cases(outcomeFile$Hospital.Name, outcomeFile[,outcome_col])
  #length(good_list)
  #range(outcomeFile[,outcome_col], na.rm = TRUE)
  #min_index <- which.min(outcomeFile[,outcome_col])
  
  ## Change all values labeled as "Not Available" as NA
  outcomeFile[outcomeFile[,outcome_col] == "Not Available",][outcome_col] <- NA
  ## Get a subset of items that are from the state
  st_frame <- outcomeFile[which (outcomeFile[,7] == state),]
  
  ## Get the minimum value of the outcome_col for that state
  min_val <- min(as.numeric(st_frame[,outcome_col]),na.rm=TRUE)
  
  min_frame <- st_frame[which (as.numeric(st_frame[,outcome_col]) == min_val),]
  
  ## min_index <- which((as.numeric(outcomeFile[,outcome_col]) == min(as.numeric(outcomeFile[,outcome_col]),na.rm = TRUE)) & (outcomeFile[,7] == state)) 
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hospitalName <- NA
  for (i in 1:nrow(min_frame)) {
    ## print(i)
    ## print(min_frame[i,2])
    if (is.na(hospitalName) || hospitalName > min_frame[i,2]) {
      hospitalName <- min_frame[i,2]
    }
  }
  hospitalName
}