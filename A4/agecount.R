agecount = function(age = NULL) {
  
  homicides = readLines("homicides.txt")
  
  if (is.null(age)) {
    stop()
  }
  
  new_query = paste0(age, " years old")
  print(new_query)
  
  return(length(grep(new_query,homicides,ignore.case = TRUE)))
  
}