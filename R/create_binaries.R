## This code writes the list of binary variables (xp, xm, up & um).
## 
## Enio Gjerga, 2020

createBinaries <- function(variables = variables){
  
  cc1 <- paste0("\t", variables$variables[variables$idxNodesUp])
  cc2 <- paste0("\t", variables$variables[variables$idxNodesDown])
  cc3 <- paste0("\t", variables$variables[variables$idxEdgesUp])
  cc4 <- paste0("\t", variables$variables[variables$idxEdgesDown])
  
  return(c(cc1, cc2, cc3, cc4))
  
}

createBinaries_newIntRep <- function(variables = variables) {
  binaries <- paste(c(variables$nodesDf$nodesUpVars, variables$nodesDf$nodesDownVars,
                      variables$edgesDf$edgesUpVars, variables$edgesDf$edgesDownVars),
                    sep = "\t")
  return(binaries)
}
