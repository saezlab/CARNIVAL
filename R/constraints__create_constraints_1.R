## This code writes the list of constraints (1) of the ILP problem for one 
## conditions.
## 
## Enio Gjerga, 2020

createConstraints_1<- function(variables = variables){
  
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
  
  vars <- variables$variables 
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesUp1 <- variables$idxEdgesUp[idx1]
  idxEdgesUp2 <- variables$idxEdgesUp[idx2]
  
  selectedEdges1 <- selectEdges(idxEdgesUp1, "ReactionUp ")
  selectedEdges2 <- selectEdges(idxEdgesUp2, "ReactionUp ")
  
  constraints1[idx1] <- createConstraint( vars[idxEdgesUp1], 
                                          "-",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0 )
  
  constraints1[idx2] <- createConstraint( vars[idxEdgesUp2], 
                                          "+",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0 )
  
  return(constraints1)
  
}

