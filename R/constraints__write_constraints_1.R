## This code writes the list of constraints (1) of the ILP problem for one 
## conditions.
## 
## Enio Gjerga, 2020

createConstraints_1<- function(variables = variables){
  
  selectEdgesUp <- function(idx) {
    gsub(variables$exp[idx], 
              pattern = "ReactionUp ", 
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
  
  idxEdgesUp1 <- variables$idxEdgesUp[idx1]
  idxEdgesUp2 <- variables$idxEdgesUp[idx2]
  
  selectedEdges1 <- selectEdgesUp(idxEdgesUp1)
  selectedEdges2 <- selectEdgesUp(idxEdgesUp2)
  
  constraints1[idx1] <- createConstraint( variables$variables[idxEdgesUp1], 
                                            "-",
                                            findMatchingVariable(selectedEdges1), 
                                            ">=", 0)
  
  constraints1[idx2] <- createConstraint( variables$variables[idxEdgesUp2], 
                                            "+",
                                            findMatchingVariable(selectedEdges2), 
                                            ">=", 0)
  
  return(constraints1)
  
}

