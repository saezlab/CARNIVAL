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
  
  if (!is.null(result)) {
    WriteDOTfig(result = result,
                dir_name = outputFolder,
                inputs = perturbations,
                measurements = measurements,
                UP2GS = FALSE)
  }
  
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
  
  result <- sendTaskToSolver(variables,
                            perturbations, 
                            measurements, 
                            pathwayWeights, 
                            priorKnowledgeNetwork, 
                            carnivalOptions)
  
  #TODO results with diagnostics is never null, think how to implement it better
  #if (!is.null(result)) {
  #  WriteDOTfig(result = result,
  #              dir_name = outputFolder,
  #              inputs = perturbations,
  #              measurements = measurements,
  #              UP2GS = FALSE)
  #}
  
  return(result)
}
    
sendTaskToSolver <- function(variables,
                             perturbations, 
                             measurements, 
                             pathwayWeights, 
                             priorKnowledgeNetwork, 
                             carnivalOptions) {
  
  message("Solving LP problem...")
  result <- c()
  
  if(carnivalOptions$solver == supportedSolvers$cplex){
    cplexCommandFilename <- writeCplexCommandFile(carnivalOptions)
    
    result <- solveWithCplex(carnivalOptions$solverPath,
                             cplexCommandFilename,
                             carnivalOptions$dirName, 
                             carnivalOptions$runId,
                             carnivalOptions$outputFolder,
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
                               perturbations = perturbations, 
                               priorKnowledgeNetwork = priorKnowledgeNetwork, 
                               carnivalOptions = carnivalOptions)
  }
}

