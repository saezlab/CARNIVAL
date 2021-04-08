## This code writes the boundaries of each variable.
##
## Enio Gjerga, 2020

createBoundaries <- function(variables = variables, 
                             objectiveFunction = objectiveFunction){
  
  M <- 100
  cc1 <- paste0("\t", 
                     "-1 <= ", 
                     variables$variables[variables$idxNodes], 
                     " <= 1")
  cc2 <- paste0("\t", 
                     "0 <= ", 
                     variables$variables[variables$idxNodesUp], 
                     " <= 1")
  cc3 <- paste0("\t", 
                     "0 <= ", 
                     variables$variables[variables$idxNodesDown], 
                     " <= 1")
  cc4 <- paste0("\t", 
                     "0 <= ", 
                     variables$variables[variables$idxEdgesUp], 
                     " <= 1")
  cc5 <- paste0("\t", 
                     "0 <= ",  
                     variables$variables[variables$idxEdgesDown], 
                     " <= 1")
  
  cc6 <- paste0("\t", "-1 <= ", variables$variables[variables$idxB], 
                             " <= 1")
  cc7 <- paste0("\t", "0 <= ", variables$variables[variables$idxDist], 
                             " <= ", M)
  cc8 <- paste0("\t", "0 <= ", unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
                               pattern = "absDiff", 
                               x = strsplit(objectiveFunction, split = " ")[[1]])]), 
                             " <= 2")
  cc9 <- paste0("\t", "0 <= ", variables$variables[variables$idxDist], 
                             " <= ", M)
  
  return(c(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9))
  
}

createBoundary <- function(lowLimit, variable, upperLimit) {
 boundary <- paste0("\t", lowLimit, " <= ", variables$variables[variables$idxB], 
         " <= ", upperLimit)
 return(boundary)
}

createBoundaries_newIntRep <- function(variables){
  distanceConstant <- 100 
  
  b1 <- createBoundary(-1, variables$nodesDf$nodes, 1)
  b2 <- createBoundary(-1, variables$nodesDf$nodesUpVars, 1)
  b3 <- createBoundary(-1, variables$nodesDf$nodesDownVars, 1)
  
  b4 <- createBoundary(0, variables$edgesDf$edgesUpVars, 1)
  b5 <- createBoundary(0, variables$edgesDf$edgesDownVars, 1)
  
  b6 <- createBoundary(0, variables$measurementsDf$measurementsVars, 2)
  b7 <- createBoundary(-1, variables$nodesDf$nodesActStateVars, 1)
  
  b8 <- createBoundary(0, variables$nodesDf$nodesDistanceVars, distanceConstant)
  
  return(c(b1, b2, b3, b4, b5, b6, b7, b8))
}