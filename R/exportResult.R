#'\code{exportResult}
#'
#' Extract and export the optimisation results from the cplex solution file 
#' (XML) as files and variables for further plotting functions
#'
#'Enio Gjerga, 2020

exportResult <- function(cplexSolutionFileName = cplexSolutionFileName, 
                         variables = variables, conditionIDX = conditionIDX,
                         pknList = pknList, inputs=inputs, 
                         measurements=measurements, solver = "cplex", 
                         lpSolution = NULL, mt = NULL){
  
  if(solver=="cplex"){
    
    RES <- exportResultCPLEX(cplexSolutionFileName = cplexSolutionFileName, 
                             variables = variables, conditionIDX = conditionIDX, 
                             pknList = pknList, inputs = inputs, 
                             measurements = measurements)
    
  } else {
    
    if(solver=="cbc"){
      
      RES <- exportResultCBC(cplexSolutionFileName = cplexSolutionFileName, 
                             variables = variables, conditionIDX = conditionIDX, 
                             pknList = pknList, inputs = inputs, 
                             measurements = measurements)
      
      return(RES)
      
    } else {
      
      RES <- exportResultLPSolve(variables = variables, 
                                 conditionIDX = conditionIDX, pknList = pknList,
                                 inputs = inputs, measurements = measurements, 
                                 lpSolution = lpSolution, mt = mt)
      
      return(RES)
      
    }
    
  }
  
}