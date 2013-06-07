# Reads in outcome data and finds
# the hospital with given rank (either best, worst, or a number)
# in each state. Outputs a data frame of the hospital name and the state

rankall = function(outcome, num = "best") {
  
  hospital = read.csv("outcome-of-care-measures.csv", colClasses="character")
  hospital[,11] = as.numeric(hospital[,11])
  hospital[,17] = as.numeric(hospital[,17])
  hospital[,23] = as.numeric(hospital[,23])
  
  all_states = as.character(hospital$State)
  outcomes = c("heart attack","heart failure","pneumonia")
  
  #Error testing
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    col = 11
  }
  
  else if (outcome == "heart failure") {
    col = 17
  }
  
  else {
    col = 23
  }
  
  #Remove NAs and sort from lowest to highest based on outcome
  one_state = subset(hospital,hospital[,col] != "Not Available")
  one_state = one_state[order(one_state[,col],one_state[,2]),]
  
  data = data.frame(hospital=c(),state=c())

  if (num == "best") {
    new_data = lapply(split(one_state,one_state$State), function(x) {
      return (c(x[1,2],x[1,7]))
    })
  }
  
  else if (num == "worst") {
    new_data = lapply(split(one_state,one_state$State), function(x) {
      return (c(tail(x[,2],n=1),tail(x[,7],n=1)))
    })
  }
  
  else if (as.numeric(num) > length(one_state)) {
    new_data = lapply(split(one_state,one_state$State), function(x) {
      return (c(NA,NA))
    })
  }
  
  else {
    new_data = lapply(split(one_state,one_state$State), function(x) {
      return (c(x[as.numeric(num),2],x[as.numeric(num),7]))
    })
  }
  
  table = lapply(new_data,function(x){
    add = data.frame(hospital=c(x[[1]][1]),state=c(x[[2]][1]))
    return (add)
  })
  
  table = do.call("rbind",table)
  
  print(table)

  

  
}
