source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA


rankhospital("TX", "heart attack") 
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
rankhospital("TX", "heart failure") 
# [1] "FORT DUNCAN MEDICAL CENTER"
rankhospital("MD", "heart attack") 
# [1] "JOHNS HOPKINS HOSPITAL, THE"
rankhospital("MD", "pneumonia") 
#  [1] "GREATER BALTIMORE MEDICAL CENTER"
rankhospital("BB", "heart attack") 
# Error in best("BB", "heart attack") : invalid state
rankhospital("NY", "hert attack") 
# Error in best("NY", "hert attack") : invalid outcome

