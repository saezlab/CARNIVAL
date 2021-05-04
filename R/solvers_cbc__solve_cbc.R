#' Executes cbc solver on provided .lp file. 
#'
#' @param carnivalOptions 
#'
#' @return
#' @keywords internal
#'
solveWithCbc <- function(carnivalOptions) {
  
  resultFile <- carnivalOptions$filenames$resultFile
  lpFile <- carnivalOptions$filenames$lpFilename
  
  cbc_command <- paste0(carnivalOptions$solverPath, " ", lpFile, 
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resultFile)
 
  system(cbc_command)
  
  solutionFileName <- carnivalOptions$filenames$resultFile
  solutionMatrix <- read.csv2(solutionFileName, sep = ",")
  
  return(solutionMatrix)
}

getSolutionMatrixCbc <- function(solutionMatrix) {
  variablesNames <- solutionMatrix$name
  solutionMatrix <- as.matrix(solutionMatrix$solution)
  rownames(solutionMatrix) <- variablesNames
  
  return(solutionMatrix)
}
