complete <- function(directory, id = 1:332) {
  source("getmonitor.R")
  list_num_nas = c()
  for (i in id) {
    num_nas = sum(complete.cases(getmonitor(i,directory)))
    list_num_nas = append(list_num_nas,num_nas)
  }
  
  return (data = data.frame(id=id,nobs=list_num_nas))
}