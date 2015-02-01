setwd("/Users/ashwingrao/Dropbox/Git/coursera/Assignment 2/Assignment 3/rprog-data-ProgAssignment3-data.zip Folder/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome,1)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
par(mar = rep(2, 4))
hist(outcome[, 11])

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
 
  #print (outcome_col)
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

  if (!(num %in% c("best","worst")) {
    if (!is.numeric(num)) {
      stop("invalid num")
    }
  }
  
  ## Change all values labeled as "Not Available" as NA
  outcomeFile[outcomeFile[,outcome_col] == "Not Available",][outcome_col] <- NA
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
   stop("invalid num")
 }
  
}

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
  dat <- data.frame()                             #creates an empty data frame
  states <- levels(factor_by_state)
  ## print(states)
  
  
  ## For each state, find the hospital of the given rank
  for(st in states) {
  
    ## print (st)
    ## Get a subset of items that are from the state
    st_frame <- mod_outcomeFile[which (mod_outcomeFile$State == st),]
    rank_frame <- st_frame[order(as.numeric(st_frame[,3]),st_frame$State, na.last=TRUE),]
    ## Return the right value
    if (num == "best") {
      print(c(rank_frame[1,]$Hospital.Name, st))
      dat <- rbind(dat,c(rank_frame[1,]$Hospital.Name, st))
      #dat <- rbind.data.frame(dat, c("this", st))
    } else if (num == "worst") {
      dat <-  rbind(dat,c(tail(rank_frame, n=1 )$Hospital.Name,st))
    } else if (is.numeric(num) && num <= nrow(rank_frame)) {
      dat <-  rbind(dat,c(rank_frame[num,]$Hospital.Name,st))
    }else {
      dat <-  rbind(dat,c(NA,st))
    }
    #names(dat) <- c("hospital", "state")
    #print (nrow(dat))
    #print (tail(dat,n=1))
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  dat
}