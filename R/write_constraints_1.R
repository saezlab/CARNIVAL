#'\code{write_constraints_1}
#'
#' This code writes the list of constraints (1) of the ILP problem for one 
#' conditions.
#' 
#' Enio Gjerga, 2020

write_constraints_1 <- function(variables=variables, 
                                conditionIDX=conditionIDX) {
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
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
        " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
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
             conditionIDX), variables$exp)], " >= 0")
  
  return(constraints1)
  
}