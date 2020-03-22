## Appending the of formulation for each condition
##
## Enio Gjerga, 2020

append_objective_function = function(oF = oF, variables = variables, 
                                     deltaWeight = 0.0001){
  
  for(ii in 1:(length(variables)-1)){
    
    objectiveFunctionUpVec <- 
      paste0(" + ", deltaWeight, " ", 
             variables[[ii]]$variables[variables[[ii]]$idxEdgesUp])
    objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
    
    objectiveFunctionDownVec <- 
      paste0(" + ", deltaWeight, " ", 
             variables[[ii]]$variables[variables[[ii]]$idxEdgesDown])
    objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
    
    oF <- paste0(oF, objectiveFunctionUp)
    oF <- paste0(oF, objectiveFunctionDown)
    
  }
  
  return(oF)
  
}