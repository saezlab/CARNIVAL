## This code writes the list of constraints (2) of the ILP problem for one 
## condition.
## 
## Enio Gjerga, 2020

createConstraints_2 <- function(variables = variables){
  
  selectEdgesUp <- function(idx) {
      gsub(variables$exp[idx], 
              pattern = "ReactionDown ", 
              replacement = "")
  }
  
  findMatchingVariable <- function(selectedEdges) {
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(selectedEdges, split = "="))[c(TRUE, FALSE)]), 
      variables$exp)]
  }
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesDown1 <- variables$idxEdgesDown[idx1]
  idxEdgesDown2 <- variables$idxEdgesDown[idx2]
  
  selectedEdges1 <- selectEdgesUp(idxEdgesDown1)
  selectedEdges2 <- selectEdgesUp(idxEdgesDown2)
  
  constraints1[idx1] <- createConstraint( variables$variables[idxEdgesDown1], 
                                          "+",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0)
  
  constraints1[idx2] <- createConstraint( variables$variables[idxEdgesDown2], 
                                          "-",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0)
  
  return(constraints1)
  
}
