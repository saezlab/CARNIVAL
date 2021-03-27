## This code writes the list of constraints (8) of the ILP problem for one condition.
## 
## Enio Gjerga, 2020

createConstraints_8 <- function(variables = variables, 
                                perturbations = perturbations, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork){
  vars <- variables$variables
  
  cc1 <- paste0(
    vars[variables$idxNodesUp], 
    " - ", 
    vars[variables$idxNodesDown], 
    " + ", vars[variables$idxB], 
    " - ", vars[variables$idxNodes], " = 0")
  
  kk <- paste0("Species ", names(perturbations))
  cc2 <- paste0(
    vars[variables$idxB[which(
      !(variables$exp[variables$idxNodes] %in% kk))]], " = 0")

  kk <- paste0("Species ", names(perturbations))
  cc3 <- c()
  for(jj in seq_len(length(kk))){
    cName <- strsplit(x = kk[jj], split = " ")[[1]][2]
    cc3 <- c(cc3, paste0(
      vars[which(
        variables$exp == paste0(
          "Species ", cName))], " = ", perturbations[jj]))
  }
 
  cc4 <- c()
  if (length(
    setdiff(
      as.character(priorKnowledgeNetwork[, 1]), 
      as.character(priorKnowledgeNetwork[, 3]))) > 0) {
    kk <- paste0(
      "Species ", 
      setdiff(as.character(priorKnowledgeNetwork[, 1]), 
              as.character(priorKnowledgeNetwork[, 3])))
    cc4 <- paste0(
      vars[variables$idxNodes[which(
        variables$exp[variables$idxNodes] %in% kk)]], " - ", 
      vars[variables$idxB[which(
        variables$exp[variables$idxNodes] %in% kk)]], 
      " = 0")
  }
  
  constraints8 <- c(cc1, cc2, cc3, cc4)
  
  return(constraints8)
}
