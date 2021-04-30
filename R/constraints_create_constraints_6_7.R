## This code writes the list of constraints (6) and (7) of the ILP problem. 
## 
## Enio Gjerga, Olga Ivanova 2020-2021

createConstraints_6_7_v2 <- function(variables, constraintName = c("c6", "c7")) {
  
  parentNodes <- setdiff(variables$edgesDf$Node1, variables$edgesDf$Node2)
  
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  parentNodesEdges <- variablesMerged[variablesMerged$Node1 %in% parentNodes, ]
  
  if ( length(parentNodes) > 0 ) {
    constraints_6 <- createConstraintFreeForm(unique(parentNodesEdges$nodesUpVars), "<=", 0) 
    constraints_7 <- createConstraintFreeForm(unique(parentNodesEdges$nodesDownVars), "<=", 0) 
  } else {
    constraints_6 <- c()
    constraints_7 <- c()
  }
  
  variablesMergedTwoNodes <- merge(variablesMerged, variables$nodesDf, by.x="Node2", by.y="nodes")
  
  lapply(unique(variablesMergedTwoNodes$Node2), function(x) {
    allIncomingEdges <- variablesMergedTwoNodes[variablesMergedTwoNodes$Node2 == x, ]
    
    nodeVar <- unique(allIncomingEdges$nodesUpVars.y)
    edgesVars <- paste0(" - ", allIncomingEdges$edgesUpVars)
    constraintLeft <- c(nodeVar, edgesVars)
    constraintLeft <- paste(constraintLeft, collapse='')
    
    constraints_6 <<- c(constraints_6, createConstraintFreeForm(constraintLeft, "<=", 0))
    
    nodeVar <- unique(allIncomingEdges$nodesDownVars.y)
    edgesVars <- paste0(" - ", allIncomingEdges$edgesDownVars)
    constraintLeft <- c(nodeVar, edgesVars)
    constraintLeft <- paste(constraintLeft, collapse='')
    
    constraints_7 <<- c(constraints_7, createConstraintFreeForm(constraintLeft, "<=", 0))
    
  })
  
  constraints6_7 <- list("c6" = constraints_6, "c7" = constraints_7)
  names(constraints6_7) <- constraintName
  return(constraints6_7)
}

