#' Executes cbc solver on provided .lp file.
#'
#' @param carnivalOptions
#'
#' @return
#' @keywords internal
#'
solveWithCbc <- function(carnivalOptions) {

  resultFile <- carnivalOptions$filenames$resultFile
  resultFile <- stringr::str_replace(resultFile, ".txt", ".csv")
  lpFile <- carnivalOptions$filenames$lpFilename

  cbc_command <- paste0(carnivalOptions$solverPath,
                        " -import ", lpFile,
                        " -seconds ", carnivalOptions$timelimit,
                        " -ratioGap ", carnivalOptions$poolrelGap,
                        " -threads ", carnivalOptions$threads,
                        " -solve", # Make sure to solve before exporting!!!
                        " -printi csv ",
                        " -solution ", resultFile
                        )

  system(cbc_command)

  solutionMatrix <- read.csv2(resultFile, sep = ",")

  return(solutionMatrix)
}

getSolutionMatrixCbc <- function(solutionMatrix) {
  variablesNames <- solutionMatrix$name
  solutionMatrix <- as.matrix(solutionMatrix$solution)
  rownames(solutionMatrix) <- variablesNames

  return(solutionMatrix)
}
