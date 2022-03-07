solveWithLpSolve <- function(lpMatrix, carnivalOptions) {
  
  lpForm <- prepareLPMatrixSingle(lpMatrix, carnivalOptions)
  # lp solve assumes that all decision variables are positive. We should shift
  # the problem to the positive plane. 
  
  
  # lpSolutionResults <- lp(direction = "min", objective.in = lpForm$obj,
  #                         const.mat = lpForm$con, const.dir = lpForm$dir,
  #                         const.rhs = lpForm$rhs, int.vec = lpForm$ints,
  #                         binary.vec = lpForm$bins)$solution
  # 

  lpForm_tr <- shiftConstraintSpace(lpForm)
  lpSolutionResults_tr <- lp(direction = "min", objective.in = lpForm_tr$obj, 
                          const.mat = lpForm_tr$con, const.dir = lpForm_tr$dir, 
                          const.rhs = lpForm_tr$rhs, int.vec = sort(c(lpForm_tr$bins, lpForm_tr$ints))
                          )$solution
  
  lpSolutionResults <- shiftConstraintSpaceBack(lpSolutionResults_tr)
  
  
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



# Shifts all the variables and solve the problem for the transformed variable y. 
# y = x + 1 , which means x = y - 1 has to inserted
#  obj   \alpha*x
# s.t. C*x < rhs    
#
# becomes
#
#  obj   \alpha*(y-1)  => \alpha*y can be used if we dont care about the objective function value
# s.t. C * (y - 1) < rhs => C * y < rhs + C*1 = rhs'
# i.e. we have to transform the right hand side function
# @author Attila Gabor 2021
shiftConstraintSpace <- function(lpForm, shift = 1){
  rhs <- as.numeric(lpForm$rhs)
  lpFormLhsMatrix <- lpForm$con
  one <- rep(shift, ncol(lpFormLhsMatrix))
  
  add <- lpFormLhsMatrix%*%one
  rhs <- rhs + add
  
  lpForm$rhs <- rhs
  return(lpForm)
}

# transform back the optimal solution. 
# @author Attila Gabor 2021
shiftConstraintSpaceBack <- function(solution, shift = 1){
  solution <- as.numeric(solution)
  return(solution - shift)
}
