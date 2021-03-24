#TODO add docs
#TODO params
solveWithCbc <- function(variables = variables, 
                         carnivalOptions = carnivalOptions,
                         priorKnowledgeNetwork = priorKnowledgeNetwork, 
                         perturbations = perturbations, 
                         measurements = measurements) {
  
  resultFile <- carnivalOptions$filenames$resultFile
  lpFile <- carnivalOptions$filenames$lpFilename
  
  cbc_command <- paste0(carnivalOptions$solverPath, " ", lpFile, 
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resultFile)
 
  system(cbc_command)
  
  result <- exportResultCbc(solutionFileName = resultFile, 
                            variables = variables, 
                            priorKnowledgeNetwork = priorKnowledgeNetwork, 
                            perturbations = perturbations, 
                            measurements = measurements)

  return(result)
}