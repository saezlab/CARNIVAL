## This code writes the list of constraints (2) of the ILP problem for one 
## condition.
## 
## Enio Gjerga, 2020

createConstraints_2 <- function(variables=variables, 
                                conditionIDX=1){
  
  constraints1 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(
    variables$variables[variables$idxEdgesDown[idx1]], 
    " + ",
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(
          gsub(
            gsub(
              variables$exp[variables$idxEdgesDown[idx1]],
              pattern = "ReactionDown ", 
              replacement = ""), 
            pattern = paste0(
              " in experiment ", 
              conditionIDX), 
            replacement = ""), split = "="))[c(TRUE, FALSE)],
        " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  constraints1[idx2] <- paste0(
    variables$variables[variables$idxEdgesDown[idx2]], 
    " - ",
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(
          gsub(
            gsub(
              variables$exp[variables$idxEdgesDown[idx2]], 
              pattern = "ReactionDown ", 
              replacement = ""), 
            pattern = paste0(
              " in experiment ", 
              conditionIDX), 
            replacement = ""), 
          split = "="))[c(TRUE, FALSE)],
        " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  return(constraints1)
  
}

createConstraints_2_v2_1 <- function(variables = variables){
  
  selectEdgesUp <- function(idx) {
    gsub(gsub(variables$exp[idx], 
              pattern = "ReactionDown ", 
              replacement = ""), 
         pattern = " in experiment 1", 
         replacement = "")
  }
  
  findMatchingVariable <- function(selectedEdges) {
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(selectedEdges, split = "="))[c(TRUE, FALSE)],
        " in experiment 1"), variables$exp)]
  }
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesDown1 <- variables$idxEdgesDown[idx1]
  idxEdgesDown2 <- variables$idxEdgesDown[idx2]
  
  selectedEdges1 <- selectEdgesUp(idxEdgesDown1)
  selectedEdges2 <- selectEdgesUp(idxEdgesDown1)
  
  constraints1[idx1] <- createConstraint( variables$variables[idxEdgesUp1], 
                                          "+",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0)
  
  constraints1[idx2] <- createConstraint( variables$variables[idxEdgesUp2], 
                                          "-",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0)
  
  return(constraints1)
  
}
