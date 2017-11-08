write_constraints_4 <- function(variables=variables, conditionIDX=conditionIDX) {
  
  constraints4 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints4[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ", 
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = "ReactionDown "), 
                                                         variables$exp)],
                               " <= 0")
  
  constraints4[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ",
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = "ReactionDown "), 
                                                         variables$exp)],
                               " >= 0")
  
  return(constraints4)
  
}