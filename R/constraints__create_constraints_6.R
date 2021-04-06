##  This code writes the list of constraints (6) of the ILP problem for one condition.
##
## Enio Gjerga, 2020

createConstraints_6 <- function(variables = variables,
                                priorKnowledgeNetwork = priorKnowledgeNetwork) {
  
  vars <- variables$variables
  
  source <- unique(variables$reactionSource)
  target <- unique(variables$reactionTarget)
  
  gg <- igraph::graph_from_data_frame(d = priorKnowledgeNetwork[, c(3, 1)])
  adj <- igraph::get.adjacency(gg)
  adj <- as.matrix(adj)
  
  idx1 <- which(rowSums(adj) == 0)
  idx2 <- setdiff(seq_len(nrow(adj)), idx1)
  
  cc1 <- NULL
  
  if( length(idx1) > 0 ){
    
    cc1 <-
      paste0(vars[which(
          variables$exp %in% paste0(
            "SpeciesUP ",
            rownames(adj)[idx1]))], " <= 0")
    
  } else {
    cc1 <- NULL
  }
  
  cc2 <- rep("", length(idx2))
  
  for(i in seq_len(length(idx2))){
    
    cc2[i] <- paste0(
      vars[which(
        variables$exp == paste0(
          "SpeciesUP ",
          rownames(adj)[idx2[i]]))],
      paste(
        paste0(
          " - ",
          vars[which(
            variables$exp %in% paste0(
              "ReactionUp ",
              colnames(adj)[which(adj[idx2[i], ]>0)],
              "=",
              rownames(adj)[idx2[i]]))]), collapse = ""), " <= 0")
    
  }

  return(c(cc1, cc2))
}

createConstraints_6_newIntRep <- function(variables, priorKnowledgeNetwork) {
  
  parentNodes <- setdiff(priorKnowledgeNetwork$Node1, priorKnowledgeNetwork$Node2)
  
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  parentNodesEdges <- variablesMerged[variablesMerged$Node1 %in% parentNodes, ]
  
  if ( length(parentNodes) > 0 ) {
    constraints_6 <- createConstraintFreeForm( parentNodesEdges$nodesUpVars, "<=", 0) 
  } else {
    constraints_6 <- c()
  }
  
  variablesMergedTwoNodes <- merge(variablesMerged, variables$nodesDf, by.x="Node2", by.y="nodes")
  
  lapply(unique(variablesMergedTwoNodes$Node2), function(x) {
     allIncomingEdges <- variablesMergedTwoNodes[variablesMergedTwoNodes$Node2 == x, ]
     nodeVar <- unique(allIncomingEdges$nodesUpVars.y)
     edgesVars <- paste0(" - ", allIncomingEdges$edgesUpVars)
     constraintLeft <- c(nodeVar, edgesVars)
     constraintLeft <- paste(temp, collapse='')
     
     constraints_6 <<- c(constraints_6, createConstraintFreeForm(constraintLeft, "<=", 0))
    
  })
 
  return(constraints_6)
}
