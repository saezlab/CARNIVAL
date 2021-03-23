## This code writes the list of constraints (8) of the ILP problem for one condition.
## 
## Enio Gjerga, 2020

write_constraints_8 <- function(variables = variables, 
                                perturbations = perturbations, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork){
  
  ii=1
  #TODO tmp, remove after cleaning up for exp condition
  perturbations <- t(as.data.frame(perturbations))
  
  cc1 <- paste0(
    variables[[ii]]$variables[variables[[ii]]$idxNodesUp], 
    " - ", 
    variables[[ii]]$variables[variables[[ii]]$idxNodesDown], 
    " + ", variables[[ii]]$variables[variables[[ii]]$idxB], 
    " - ", variables[[ii]]$variables[variables[[ii]]$idxNodes], " = 0")
  
  kk <- paste0("Species ", colnames(perturbations), " in experiment ", ii)
  cc2 <- paste0(
    variables[[ii]]$variables[variables[[ii]]$idxB[which(
      !(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk))]], " = 0")

  kk <- paste0("Species ", colnames(perturbations), " in experiment ", ii)
  cc3 <- c()
  for(jj in seq_len(length(kk))){
    cName <- strsplit(x = kk[jj], split = " ")[[1]][2]
    cc3 <- c(cc3, paste0(
      variables[[ii]]$variables[which(
        variables[[ii]]$exp == paste0(
          "Species ", cName, " in experiment ", ii))], " = ", perturbations[ii, jj]))
  }
 
  cc4 <- c()
  if (length(
    setdiff(
      as.character(priorKnowledgeNetwork[, 1]), 
      as.character(priorKnowledgeNetwork[, 3])))>0) {
    kk <- paste0(
      "Species ", 
      setdiff(as.character(priorKnowledgeNetwork[, 1]), 
              as.character(priorKnowledgeNetwork[, 3])), " in experiment ", ii)
    cc4 <- paste0(
      variables[[ii]]$variables[variables[[ii]]$idxNodes[which(
        variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " - ", 
      variables[[ii]]$variables[variables[[ii]]$idxB[which(
        variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], 
      " = 0")
  }
  
  constraints8 <- c(cc1, cc2, cc3, cc4)
  
  return(constraints8)
}
