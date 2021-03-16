#TODO add docs
#TODO params
solveWithCbc <- function(variables=variables, 
                         carnivalOptions=carnivalOptions,
                         pknList=pknList, 
                         inputObj=inputObj, 
                         measurements=measurements) {
  
  resFile = paste0("results_cbc", ".txt")
  
  cbc_command <- paste0(carnivalOptions$solverPath, " testFile", 
                        ".lp -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resFile)
  
  system(cbc_command)
  
  res <- exportResultCBC(solutionFileName = resFile, 
                        variables = variables, 
                        conditionIDX = 1,
                        pknList = pknList, 
                        inputs = inputObj, 
                        measurements = measurements)

  return(res)
}