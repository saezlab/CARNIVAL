solveWithLpSolve <- function(lpMatrix, carnivalOptions) {
  
  lpForm <- prepareLPMatrixSingle(lpMatrix, carnivalOptions)
  
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
  rownames(solMatrix) <- solMatrix[, 1] 
  solMatrix <- as.matrix(solMatrix[, 2])
  
  return(solMatrix)
}
