## Transforming our variables
##
## Eio Gjerga, 2020

transformVariables <- function(variables = variables, measObj = measObj){
  
  vars1 <- variables[[1]][[1]]
  
  measSpecies = colnames(measObj)
  vars2 <- rep("", length(measSpecies))
  for(jj in seq_len(length(measSpecies))){
    idx = which(variables[[1]]$exp==paste0("Species ",
                                            measSpecies[jj],
                                            " in experiment ", 1))
    vars2[jj] <- paste0("absDiff", idx, "_", 1)
  }
  
  mappingTable = matrix(data = , nrow = length(c(vars1, vars2)), ncol = 2)
  mappingTable[, 1] = c(vars1, vars2)
  mappingTable[, 2] = paste0("x", seq_len(length(c(vars1, vars2))))
  
  return(mappingTable)
  
}