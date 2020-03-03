#'\code{transformVariables}
#'
#' Transforming our variables
#'
#'Eio Gjerga, 2020

transformVariables <- function(variables = variables, measObj = measObj){
  
  allVariables <- c()
  for(ii in 1:length(variables)){
    allVariables <- c(allVariables, variables[[ii]][[1]])
  }
  
  measSpecies = colnames(measObj)
  for(ii in 1:length(variables)){
    for(jj in 1:length(measSpecies)){
      idx = which(variables[[ii]]$exp==paste0("Species ", 
                                              measSpecies[jj], 
                                              " in experiment ", ii))
      if(length(idx)>0){
        allVariables <- c(allVariables, paste0("absDiff", idx, "_", ii))
      }
    }
  }
  
  mappingTable = matrix(data = , nrow = length(allVariables), ncol = 2)
  mappingTable[, 1] = allVariables
  mappingTable[, 2] = paste0("x", 1:length(allVariables))
  
  return(mappingTable)
  
}