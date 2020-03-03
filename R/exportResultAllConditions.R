#'\code{exportResultAllConditions}
#'
#' Extract and export the optimisation results from the cplex solution file 
#' (XML) as files and variables for further plotting functions
#'
#'Enio Gjerga, 2020

exportResultAllConditions <- function(cplexSolutionFileName = 
                                        cplexSolutionFileName, 
                                      variables = variables,
                                      pknList = pknList, 
                                      inputs=inputs, 
                                      measurements=measurements, 
                                      solver=solver, 
                                      lpSolution=NULL, 
                                      mt=NULL){
  
  if(solver=="cplex"){
    
    RES <- exportResultAllConditionsCPLEX(cplexSolutionFileName = 
                                              cplexSolutionFileName, 
                                          variables = variables, 
                                          pknList = pknList, 
                                          inputs = inputs, 
                                          measurements = measurements, 
                                          solver = "cplex")
    
    return(RES)
    
  } else {
    
    if(solver=="cbc"){
      
      RES <- exportResultAllConditionsCBC(cplexSolutionFileName = 
                                             cplexSolutionFileName, 
                                           variables = variables, 
                                           pknList = pknList, 
                                           inputs = inputs, 
                                           measurements = measurements, 
                                           solver = "cbc")
      
      return(RES)
      
    } else {
      
      
    }
  }
  
}
