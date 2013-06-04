getmonitor <- function(id, directory, summarize=FALSE) {
  id = as.integer(id)
  init_dir = getwd()
  setwd(paste0(init_dir,"/specdata"))
  if (id < 10) {
    id = paste0(0,0,id,".csv")
  }
  else if (id < 100) {
    id = paste0(0,id,".csv")
  }
  else {
    id = paste0(id,".csv")
  }
  data = read.csv(id)
  if (summarize) {
    print(data)
  }
  setwd(init_dir)
  return (as.data.frame(data))
}