rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## validate state
  if(length(grep(state, dataSet$State)) == 0) stop('invalid state')
  
  ## validate outcome
  validOutcomes = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcomes) stop("invalid outcome")
  
  ## get full column name for indicated outcome
  colFullNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcomeColName <- colFullNames[match(outcome,validOutcomes)]
  
  ## subset state data and order by outcome
  stateData <- dataSet[dataSet$State==state,]
  stateDataSorted <- stateData[order(as.numeric(stateData[[outcomeColName]]),stateData[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  ## num argument: assign 1 if passed 'best', assign total number of rows if 'worst'. Otherwise, leave num as is.
  if (num=="best") num = 1
  if (num=='worst') num = nrow(stateDataSorted)
  
  ## return hospital name of rank
  stateDataSorted[num,"Hospital.Name"]
}