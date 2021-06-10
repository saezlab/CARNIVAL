#' Executes gurobi solver on provided .lp file.
#'
#' @param carnivalOptions
#'
#' @return
#' @keywords internal
#'
solveWithGurobi <- function(carnivalOptions) {

  resultFile <- carnivalOptions$filenames$resultFile
  resultFile <- stringr::str_replace(resultFile, ".txt", ".sol")
  lpFile <- carnivalOptions$filenames$lpFilename

  gurobi_command <- paste0(carnivalOptions$solverPath,
                           " MIPGAP=", carnivalOptions$mipGap,
                           " TimeLimit=", carnivalOptions$timelimit,
                           " PoolGap=", carnivalOptions$poolrelGap,
                           " SolutionLimit=", carnivalOptions$limitPop,
                           " PoolSolutions=", carnivalOptions$poolCap,
                           " Threads=", carnivalOptions$threads,
                           " ResultFile=", resultFile,
                           " ", lpFile)

  system(gurobi_command)

  solutionMatrix <- read.csv2(resultFile, sep = ",", comment.char="#",
                              col.names=c("name", "solution"), header=F)

  return(solutionMatrix)
}

getSolutionMatrixGurobi <- function(solutionMatrix) {
  variablesNames <- solutionMatrix$name
  solutionMatrix <- as.matrix(solutionMatrix$solution)
  rownames(solutionMatrix) <- variablesNames

  return(solutionMatrix)
}
