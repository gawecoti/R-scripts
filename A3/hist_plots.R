part2 = function() {
  outcome[,11] = as.numeric(outcome[,11])
  outcome[,17] = as.numeric(outcome[,17])
  outcome[,23] = as.numeric(outcome[,23])
  
  par(mfrow=c(3,1))
  xllim_value=range(c(outcome[, 11],outcome[, 17],outcome[, 23]),na.rm=TRUE) 
  hist(outcome[,11],main="Heart Attack",xlab="30-day death rate",xlim=xllim_value,prob=TRUE)
  abline(v=median(outcome[,11],na.rm=T))
  lines(density(outcome[,11],na.rm=T))
  lines(density(outcome[,11],na.rm=T, adjust=2), lty="dotted")
  hist(outcome[,17],main="Heart Failure",xlab="30-day death rate",xlim=xllim_value,prob=TRUE)
  abline(v=median(outcome[,17],na.rm=T))
  lines(density(outcome[,17],na.rm=T))
  lines(density(outcome[,17],na.rm=T, adjust=2), lty="dotted")
  hist(outcome[,23],main="Pneumonia",xlab="30-day death rate",xlim=xllim_value,prob=TRUE) 
  abline(v=median(outcome[,23],na.rm=T))
  lines(density(outcome[,23],na.rm=T))
  lines(density(outcome[,23],na.rm=T, adjust=2), lty="dotted")
}