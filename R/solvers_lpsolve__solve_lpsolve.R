
solveWithLpSolve <- function( variables = variables, 
                              carnivalOptions,
                              dataPreprocessed ) {
  
  lpForm <- prepareLPMatrixSingle(variables = variables, 
                                  measurements = measurements, 
                                  carnivalOptions = carnivalOptions)
  
  lpSolutionResults <- lp(direction = "min", objective.in = lpForm$obj, 
                          const.mat = lpForm$con, const.dir = lpForm$dir, 
                          const.rhs = lpForm$rhs, int.vec = lpForm$ints, 
                          binary.vec = lpForm$bins)$solution
  
  lpSolution <- list("lpForm" = lpForm, "lpSolutionResults" = lpSolutionResults)
  
  return(lpSolution)
}

getSolutionMatrixLpSolve <- function(lpSolution) {
  
  solMatrix <- lpSolution[["lpForm"]]$matrix
  solMatrix[, 2] <- lpSolution[["lpSolutionResults"]]
  colnames(solMatrix) <- c("name", "var")
  solMatrix <- as.data.frame(solMatrix)
  solMatrix$name <- as.character(solMatrix$name)
  solMatrix$var <- as.character(solMatrix$var)
  
  return(solMatrix)
}
