## This code writes the list of constraints (2) of the ILP problem for one 
## condition.
## 
## Enio Gjerga, 2020

createConstraints_2 <- function(variables = variables){
  
  vars <- variables$variables
  
  selectEdges <- function(idx, reactionType) {
    gsub(variables$exp[idx], 
         pattern = reactionType, 
         replacement = "")
  }
  
  findMatchingVariable <- function(selectedEdges) {
    nodesFromEdge <- unlist(strsplit(selectedEdges, split = "="))
    varsToFind <- paste0("Species ", nodesFromEdge)[c(TRUE, FALSE)]
    vars[match(varsToFind, variables$exp)]
  }
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesDown1 <- variables$idxEdgesDown[idx1]
  idxEdgesDown2 <- variables$idxEdgesDown[idx2]
  
  selectedEdges1 <- selectEdges(idxEdgesDown1, "ReactionDown ")
  selectedEdges2 <- selectEdges(idxEdgesDown2, "ReactionDown ")
  
  constraints1[idx1] <- createConstraint( vars[idxEdgesDown1], 
                                          "+",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0)
  
  constraints1[idx2] <- createConstraint( vars[idxEdgesDown2], 
                                          "-",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0)
  
  return(constraints1)
  
}

createConstraints_2_newIntRep <- function(variables, priorKnowledgeNetwork) {
  variablesMerged <- merge(variables$edgesDf, variables$nodesDf, by.x="Node1", by.y="nodes")
  
  edgesUpActivation <- variablesMerged[variablesMerged$Sign == 1, ]
  sourceNodes <- edgesUpActivation$nodesVars
  
  constraints_2 <- createConstraint(edgesUpActivation$edgesDownVars, "+", 
                                    sourceNodes, ">=", 0) 
  
  edgesUpInhibition <- variablesMerged[variablesMerged$Sign == -1, ]
  sourceNodes <- edgesUpInhibition$nodesVars
  
  constraints_2 <- c(constraints_2, createConstraint(edgesUpInhibition$edgesDownVars, "-", 
                                                     sourceNodes, ">=", 0))
  return(constraints_2)
}
