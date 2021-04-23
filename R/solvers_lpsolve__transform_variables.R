## Transforming our variables
##
## Eio Gjerga, 2020

transformVariables <- function(variables = variables, measurements = measurements){
  
  vars1 <- variables
  measSpecies = names(measurements)
  vars2 <- rep("", length(measurements))
  
  for(jj in seq_len(length(measurements))){
    idx = which(variables$exp == paste0("Species ", measurements[jj]))
    vars2[jj] <- paste0("absDiff", idx)
  }
  
  mappingTable = matrix(data = , nrow = length(c(vars1, vars2)), ncol = 2)
  mappingTable[, 1] = c(vars1, vars2)
  mappingTable[, 2] = paste0("x", seq_len(length(c(vars1, vars2))))
  
  return(mappingTable)
  
}