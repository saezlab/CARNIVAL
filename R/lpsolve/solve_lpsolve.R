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
                             pknList = pknList, inputs = inputObj,
                             measurements = measObj,
                             lpSolution = lpSolution, mt = lpForm$mt,
                             conditionIDX = 1)
  
  if (!is.null(res)) {
    if(!is.null(dirName)){
      if(dir.exists(dirName)){
        WriteDOTfig(res=res,
                    dir_name=dirName,
                    inputs=inputObj,
                    measurements=measObj,
                    UP2GS=FALSE)
      } else {
        warning("Specified directory does not exist. DOT figure not saved.")
      }
    }
  } else {
    message("No result to be written")
    return(NULL)
  }
  
  return(res)
}