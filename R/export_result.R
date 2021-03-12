## Extract and export the optimisation results from the cplex solution file 
## (XML) as files and variables for further plotting functions
##
## Enio Gjerga, 2020

exportResult <- function(solutionFileName = solutionFileName, 
                         variables = variables, 
                         conditionIDX = conditionIDX,
                         pknList = pknList, 
                         inputs = inputs, 
                         measurements = measurements, 
                         solver = "cplex", 
                         lpSolution = NULL, 
                         mt = NULL){
  
  if( solver == supportedSolvers$cplex ){
    
    results <- exportResultCPLEX(cplexSolutionFileName = solutionFileName, 
                             variables = variables, conditionIDX = conditionIDX, 
                             pknList = pknList, inputs = inputs, 
                             measurements = measurements)
    
  } else {
    
    if( solver == supportedSolvers$cbc ){
      
      results <- exportResultCBC(cplexSolutionFileName = solutionFileName, 
                             variables = variables, conditionIDX = conditionIDX, 
                             pknList = pknList, inputs = inputs, 
                             measurements = measurements)
      
      return(RES)
      
    } else {
      
      results <- exportResultLPSolve(variables = variables, 
                                 conditionIDX = conditionIDX, pknList = pknList,
                                 inputs = inputs, measurements = measurements, 
                                 lpSolution = lpSolution, mt = mt)
      
      return(results)
      
    }
  }
  
}