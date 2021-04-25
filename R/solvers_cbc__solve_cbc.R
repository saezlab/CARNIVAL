solveWithCbc <- function( variables, dataPreprocessed, carnivalOptions) {
  
  resultFile <- carnivalOptions$filenames$resultFile
  lpFile <- carnivalOptions$filenames$lpFilename
  
  cbc_command <- paste0(carnivalOptions$solverPath, " ", lpFile, 
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resultFile)
 
  system(cbc_command)
  
  solutionFileName <- carnivalOptions$filenames$resultFile
  solMatrix <- read.csv2(solutionFileName, sep = ",")
  
  return(solMatrix)
}

getSolutionMatrixCbc <- function(solutionMatrix) {
  variablesNames <- solutionMatrix$name
  solutionMatrix <- as.matrix(solutionMatrix$solution)
  rownames(solutionMatrix) <- variablesNames
  
  return(solutionMatrix)
}
