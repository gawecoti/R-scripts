# Calculates the correlation between sulfate 
# and nitrate levels

corr <- function(directory, threshold = 0) {
  source("complete.R")
  data = complete(directory)
  corr_list = numeric()
  success = FALSE
  
  for (i in data$id) {
    if (data[i,]$nobs >= threshold) {
      file = getmonitor(i, directory)
      complete = complete.cases(file)
      correlation = cor(file[complete,]$sulfate, file[complete,]$nitrate)
      print (correlation)
      corr_list = append(corr_list,correlation)
      success = TRUE
    }
  }
  
  if (success == FALSE) {
    return (c())
  }
  
  return (corr_list)
}
