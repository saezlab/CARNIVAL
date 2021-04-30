## Transforming variables for lpSolve solver.
##
## Eio Gjerga, Olga Ivanova 2020-2021l

transformVariables_v2 <- function(variables, measurements){
  
  allVariables <- c( variables$nodesDf$nodesVars, 
                     variables$nodesDf$nodesUpVars,
                     variables$nodesDf$nodesDownVars,
                     variables$nodesDf$nodesActStateVars, 
                     variables$nodesDf$nodesDistanceVars,
                     variables$edgesDf$edgesUpVars,
                     variables$edgesDf$edgesDownVars,
                     variables$measurementsDf$measurementsVars )
  
  mappingTable <- matrix(data = , nrow = length(allVariables), ncol = 2)
  mappingTable[, 1] <- allVariables
  mappingTable[, 2] <- paste0( "x", seq_len(length(allVariables)) )
  
  return(mappingTable)
  
}

#The function is obsolete, kept here for old data representation compatibility
transformVariables <- function(variables, measurements){
  
  vars1 <- variables$variables
  measSpecies <- colnames(measurements)
  vars2 <- rep("", length(measSpecies))
  
  for(jj in seq_len(length(measSpecies))){
    idx <- which(variables$exp == paste0("Species ",
                                           measSpecies[jj]) )
    vars2[jj] <- paste0("absDiff", idx, "_", 1)
  }
  
  mappingTable <- matrix(data = , nrow = length(c(vars1, vars2)), ncol = 2)
  mappingTable[, 1] <- c(vars1, vars2)
  mappingTable[, 2] <- paste0("x", seq_len(length(c(vars1, vars2))))
  
  return(mappingTable)
  
}
