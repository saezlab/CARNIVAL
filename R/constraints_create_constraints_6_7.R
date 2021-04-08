createConstraints_6_newIntRep <- function(variables, priorKnowledgeNetwork) {
  
  parentNodes <- setdiff(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2)
  
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  parentNodesEdges <- variablesMerged[variablesMerged$Node1 %in% parentNodes, ]
  
  if ( length(parentNodes) > 0 ) {
    constraints_6 <- createConstraintFreeForm( parentNodesEdges$nodesUpVars, "<=", 0) 
    constraints_7 <- createConstraintFreeForm( parentNodesEdges$nodesDownVars, "<=", 0) 
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
  
  return(list("c6"=constraints_6, "c7"=constraint7))
}

