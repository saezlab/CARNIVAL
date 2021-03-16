## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

solveCarnivalSingleRun <- function(perturbations, 
                                   measurements, 
                                   pathwayWeights, 
                                   priorKnowledgeNetwork, 
                                   carnivalOptions) {
  message("Writing constraints...")
  
  #N.B. Don't remove the line below, it breaks cplex runs
  priorKnowledgeNetwork <- as.data.frame(priorKnowledgeNetwork)
  
  variables <- writeLpFile(perturbations, 
                           measurements, 
                           pathwayWeights, 
                           priorKnowledgeNetwork, 
                           carnivalOptions)
  
  message("Solving LP problem...")
  
  result <- c()
  if(carnivalOptions$solver == supportedSolvers$cplex){
    writeCplexCommandFile(carnivalOptions)
    result <- solveWithCplex(carnivalOptions$solverPath,
                             carnivalOptions$dirName, 
                             variables,
                             priorKnowledgeNetwork, 
                             perturbations, 
                             measurements) 
    
  } else if(carnivalOptions$solver == supportedSolvers$cbc) {
    result <- solveWithCbc(variables = variables, 
                           carnivalOptions = carnivalOptions,
                           pknList = priorKnowledgeNetwork, 
                           inputObj = perturbations, 
                           measurements = measurements)
  } else { #default solver
    result <- solveWithLpSolve(variables = variables, 
                               measurements = measurements,
                               inputObj = perturbations, 
                               pknList = priorKnowledgeNetwork,
                               dirName = carnivalOptions$dirName)
  }
  
  writeFigure(carnivalOptions$dirName, perturbations, measurements, result)
  return(result)
  
}

writeFigure <- function(dirName, inputObj, measurements, result) {
  if (!is.null(result)) {
    if(!is.null(dirName)){
      if(dir.exists(dirName)){
        WriteDOTfig(result = result,
                    dir_name = dirName,
                    inputs = inputObj,
                    measurements = measObj,
                    UP2GS = FALSE)
      } else {
        warning("Specified directory does not exist. DOT figure not saved.")
      }
    }
  } else {
    message("No result to be written")
    return(NULL)
  }
}


