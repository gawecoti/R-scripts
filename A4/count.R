count = function(cause = NULL) {
  homicides = readLines("homicides.txt")
  causes = c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
  
  if (is.null(cause)) {
    stop("no input")
  }
  
  if (!(cause %in% causes)){
    stop("invalid input")
  }
  
  print(head(homicides))
  str.1 = "<dd>[Ccause]: "
  str.2 = paste0(str.1,cause)
  print(str.2)
  result = length(grep(str.2, homicides, ignore.case = TRUE))
  return (result)
  
}