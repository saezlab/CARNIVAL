#TODO add docs
#TODO params
solveWithLpSolve <- function(variables=variables, 
                             measurements=measurements,
                             inputObj=inputObj, 
                             pknList=pknList,
                             dirName=dirName) {
  
  lpForm <- prepareLPMatrixSingle(variables = variables, measurements = measurements)
  
  lpSolution <- lp(direction = "min", objective.in = lpForm$obj, 
                   const.mat = lpForm$con, const.dir = lpForm$dir, 
                   const.rhs = lpForm$rhs, int.vec = lpForm$ints, 
                   binary.vec = lpForm$bins)$solution
  
  res <- exportResultLPSolve(variables = variables,
                             pknList = pknList, 
                             inputs = inputObj,
                             measurements = measObj,
                             lpSolution = lpSolution, 
                             mt = lpForm$mt,
                             conditionIDX = 1)
  return(res)
}