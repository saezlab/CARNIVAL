#TODO add docs
#TODO params
solveWithLpSolve <- function(variables = variables, 
                             measurements = measurements,
                             perturbations = perturbations, 
                             priorKnowledgeNetwork = priorKnowledgeNetwork, 
                             carnivalOptions) {
  
  lpForm <- prepareLPMatrixSingle(variables = variables, 
                                  measurements = measurements, 
                                  carnivalOptions = carnivalOptions)
  
  lpSolution <- lp(direction = "min", objective.in = lpForm$obj, 
                   const.mat = lpForm$con, const.dir = lpForm$dir, 
                   const.rhs = lpForm$rhs, int.vec = lpForm$ints, 
                   binary.vec = lpForm$bins)$solution
  
  result <- exportResultLPSolve(variables = variables,
                                priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                perturbations = perturbations,
                                measurements = measurements,
                                lpSolution = lpSolution, 
                                matrix = lpForm$mt)
  return(result)
}