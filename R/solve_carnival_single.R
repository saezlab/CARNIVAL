## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

solveCarnivalSingleRun <- function(perturbations, 
                                   measurements, 
                                   pathwayWeights, 
                                   priorKnowledgeNetwork, 
                                   carnivalOptions) {
  message("Writing constraints...")
  
  #Don't remove the line below, it breaks cplex runs
  priorKnowledgeNetwork <- as.data.frame(priorKnowledgeNetwork)
  
  measurementsSign <- sign(measurements)
  measurementsWeights <- abs(measurements)
  
  variables <- writeLPFile(perturbations, 
                           measurements, 
                           measurementsSign, 
                           measurementsWeights, 
                           pathwayWeights, 
                           priorKnowledgeNetwork, 
                           carnivalOptions)
  
  
  message("Solving LP problem...")
  
  result <- c()
  #TODO rewrite with dep inj? 
  if(carnivalOptions$solver == supportedSolvers$cplex){
    result <- solveWithCplex(carnivalOptions$solverPath,
                             carnivalOptions$dirName, 
                             variables,
                             priorKnowledgeNetwork, 
                             perturbations, 
                             measurements) 
    
  } else if(carnivalOptions$solver == supportedSolvers$cbc) {
    #TODO add params
    result <- solveWithCbc()
  } else {
    #TODO add params
    result <- solveWithLpSolve()
  }
  
  return(result)
  
}


