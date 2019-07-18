#'\code{write_constraints_8}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indices.
#'@param inputs Contains the list of targets as inputs.
#'@param pknList Contains the background network which serves as a prior knowledge and which we train.
#'
#'@return This code writes the list of constraints (8) of the ILP problem for all the conditions.

write_constraints_8 <- function(variables=variables, inputs=inputs, pknList=pknList){
  
  constraints8 <- c()
  
  for(ii in 1:length(variables)){
    
    ##
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodesUp], " - ", variables[[ii]]$variables[variables[[ii]]$idxNodesDown], 
                 " + ", variables[[ii]]$variables[variables[[ii]]$idxB], " - ", variables[[ii]]$variables[variables[[ii]]$idxNodes], " = 0")
    
    constraints8 <- c(constraints8, cc)
    
    ##
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxB[which(!(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk))]], " = 0")
    constraints8 <- c(constraints8, cc)
    
    ##
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    cc= c()
    for(jj in 1:length(kk)){
      cName = strsplit(x = kk[jj], split = " ")[[1]][2]
      cc = c(cc, paste0(variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("Species ", cName, " in experiment ", ii))], " = ", inputs[ii, jj]))
    }
    # cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " = ", inputs[ii, ])
    constraints8 <- c(constraints8, cc)
    
    ##
    # kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    if (length(setdiff(as.character(pknList[, 1]), as.character(pknList[, 3])))>0) {
      kk <- paste0("Species ", setdiff(as.character(pknList[, 1]), as.character(pknList[, 3])), " in experiment ", ii)
      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " - ", 
                   variables[[ii]]$variables[variables[[ii]]$idxB[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " = 0")
      constraints8 <- c(constraints8, cc)
    }
    
  }
  
  return(constraints8)
  
}
