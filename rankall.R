rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## validate outcome
  validOutcomes = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcomes) stop("invalid outcome")
  
  ## get valid states
  validStates = sort(unique(dataSet[,7]))
  
  #get full column name for indicated outcome
  colFullNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcomeColName <- colFullNames[match(outcome,validOutcomes)]
  
  ## Declare hospital vector
  hospital<-character(0)
  
  ## iterate over each state
  for (i in seq_along(validStates)) {
    
    ## subset state data and order by outcome
    stateData <- dataSet[dataSet$State==validStates[i],]
    stateDataSorted <- stateData[order(as.numeric(stateData[[outcomeColName]]),stateData[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    ## assign num argument to this.num. overwrite with 1 if passed 'best' or total number of rows if 'worst'
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(stateDataSorted)
    
    ## record hospital name for this iteration
    hospital[i] <- stateDataSorted[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validStates,row.names=validStates)
}