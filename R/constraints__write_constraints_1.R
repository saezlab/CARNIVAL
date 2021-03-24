## This code writes the list of constraints (1) of the ILP problem for one 
## conditions.
## 
## Enio Gjerga, 2020

write_constraints_1 <- function(variables=variables, 
                                conditionIDX=1) {
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1)
  
  constraints1[idx1] <- paste0(
    variables$variables[variables$idxEdgesUp[idx1]], 
    " - ",
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx1]], 
                                  pattern = "ReactionUp ", 
                                  replacement = ""), 
                             pattern = paste0(
                               " in experiment ",
                               conditionIDX), 
                             replacement = ""), 
                        split = "="))[c(TRUE, FALSE)],
        " in experiment ", conditionIDX), variables$exp)], 
    " >= 0")
  
  constraints1[idx2] <- paste0(
    variables$variables[variables$idxEdgesUp[idx2]], 
    " + ",
    variables$variables[match(
      paste0("Species ",
             unlist(
               strsplit(gsub(
                 gsub(
                   variables$exp[variables$idxEdgesUp[idx2]], 
                   pattern = "ReactionUp ", 
                   replacement = ""), 
                 pattern = paste0(
                   " in experiment ", 
                   conditionIDX), 
                 replacement = ""), 
                 split = "="))[c(TRUE, FALSE)],
             " in experiment ", 
             conditionIDX), variables$exp)], 
    " >= 0")
  
  return(constraints1)
  
}

write_constraints_1_v2_1 <- function(variables = variables){
  
  selectEdgesUp <- function(idx) {
    gsub(gsub(variables$exp[idx], 
              pattern = "ReactionUp ", 
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

createConstraint <- function(variable1, sign, variable2, inequality, rightPart) { 
  constraint <- paste(variable1, sign, variable2, inequality, rightPart, sep = " ")  
  return(constraint)
}
