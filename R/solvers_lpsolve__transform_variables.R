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
