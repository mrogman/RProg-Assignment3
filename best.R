best <- function(state, outcome) {
  
  #read outcome measures csv
  dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## validate state
  if(length(grep(state, dataSet$State)) == 0) stop('invalid state')
  
  ## validate outcome
  validOutcomes = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcomes) stop("invalid outcome")
  
  ## get full column name for indicated outcome
  colFullNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcomeColName <- colFullNames[match(outcome,validOutcomes)]

  ## get state data, find lowest mortality rate in outcome column, return hospital name
  stateData <- dataSet[dataSet$State==state,]
  mortRateMin <- which.min(as.double(stateData[,outcomeColName]))
  
  stateData[mortRateMin,"Hospital.Name"]
}