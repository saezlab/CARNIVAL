## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

solveCarnivalSingleFromLp <- function(#lpFile = "", 
                                      parsedDataFile = "",
                                      carnivalOptions) {
  load(parsedDataFile) 
  result <- sendTaskToSolver(variables,
                             perturbations, 
                             measurements, 
                             pathwayWeights, 
                             priorKnowledgeNetwork, 
                             carnivalOptions)
  
  return(result)
}

solveCarnivalSingleRun <- function(variables,
                                   perturbations, 
                                   measurements, 
                                   pathwayWeights, 
                                   priorKnowledgeNetwork, 
                                   carnivalOptions) {
  
  #N.B. Don't remove the line below, it breaks cplex runs
  priorKnowledgeNetwork <- as.data.frame(priorKnowledgeNetwork)
  
  runId <- createRunId()
  carnivalOptions$runId <- runId
  
  #TODO should be in run carnival ? 
  variables <- writeLpFile(perturbations, 
                           measurements, 
                           pathwayWeights, 
                           priorKnowledgeNetwork, 
                           carnivalOptions)
  

  message("Solving LP problem...")
  
  result <- sendTaskToSolver(variables,
                            perturbations, 
                            measurements, 
                            pathwayWeights, 
                            priorKnowledgeNetwork, 
                            carnivalOptions)
  
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
        }
    }
  }
}

    
sendTaskToSolver <- function(variables,
                             perturbations, 
                             measurements, 
                             pathwayWeights, 
                             priorKnowledgeNetwork, 
                             carnivalOptions) {
  
  result <- c()
  
  if(carnivalOptions$solver == supportedSolvers$cplex){
    writeCplexCommandFile(carnivalOptions)
    result <- solveWithCplex(carnivalOptions$solverPath,
                             carnivalOptions$dirName, 
                             carnivalOptions$runId,
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
}

createRunId <- function() {
  datetime <- format(Sys.time(), "t%H_%M_%Sd%d_%m_%Y")
  salt <- sample(1:100, 1)
  runId <- paste(datetime, salt, sep="n")
  return(runId)
}