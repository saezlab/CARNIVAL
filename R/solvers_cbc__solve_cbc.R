#TODO add docs
#TODO params
solveWithCbc <- function(variables = variables, 
                         carnivalOptions = carnivalOptions,
                         priorKnowledgeNetwork = priorKnowledgeNetwork, 
                         perturbations = perturbations, 
                         measurements = measurements) {
  
  outputFolder <- carnivalOptions$outputFolder
  resultsFile <- paste0(outputFolder, "results_cbc", "_", carnivalOptions$runId, ".txt")
  lpFile <- paste0(outputFolder, "lpFile", "_", carnivalOptions$runId,".lp")
  
  cbc_command <- paste0(carnivalOptions$solverPath, " ", lpFile, 
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resultsFile)
 
  system(cbc_command)
  
  result <- exportResultCbc(solutionFileName = resultsFile, 
                            variables = variables, 
                            priorKnowledgeNetwork = priorKnowledgeNetwork, 
                            perturbations = perturbations, 
                            measurements = measurements)

  return(result)
}