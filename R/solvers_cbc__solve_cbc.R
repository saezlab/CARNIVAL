solveWithCbc <- function(variables = variables, 
                         carnivalOptions = carnivalOptions,
                         dataPreprocessed) {
  
  resultFile <- carnivalOptions$filenames$resultFile
  lpFile <- carnivalOptions$filenames$lpFilename
  
  cbc_command <- paste0(carnivalOptions$solverPath, " ", lpFile, 
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resultFile)
 
  system(cbc_command)
  
  solutionFileName <- carnivalOptions$filenames$resultFile
  solMatrix <- read_csv(solutionFileName)
  
  return(solution)
}

getSolutionMatrixCbc <- function(solution) {
  return(solution)
}
