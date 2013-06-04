best = function(state, outcome) {
  #Read in data, coerce data columns
  hospital = read.csv("outcome-of-care-measures.csv",colClasses="character")
  hospital[,11] = as.double(hospital[,11])
  hospital[,17] = as.double(hospital[,17])
  hospital[,23] = as.double(hospital[,23])
  
  all_states = as.character(hospital$State)
  outcomes = c("heart attack","heart failure","pneumonia")
  
  #Error testing
  if (!(state %in% all_states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    num = 11
  }
  
  else if (outcome == "heart failure") {
    num = 17
  }
  
  else {
    num = 23
  }
  
  #Subset by state and remove not available data
  one_state = subset(hospital,hospital$State == state )
 
  one_state = subset(one_state,one_state[,num] != "Not Available")
  
  #Reorder
  one_state = one_state[order(one_state[,num],one_state[,2]),]
  
  return (as.character(one_state[1,2]))
  
}