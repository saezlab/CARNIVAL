createRunId <- function() {
  datetime <- format(Sys.time(), "t%H_%M_%Sd%d_%m_%Y")
  salt <- sample(1:100, 1)
  runId <- paste(datetime, salt, sep="n")

  return(runId)
}

createFilenames <- function(carnivalOptions) {
  outputFolder <- carnivalOptions$outputFolder
  lpFilename <- paste0(outputFolder, "lpFile", "_", carnivalOptions$runId, ".lp")
  parsedData <- paste0(outputFolder, "parsedData_", carnivalOptions$runId, ".RData")
  resultFile <- paste0(outputFolder, "result", "_", carnivalOptions$runId, ".txt")
  
  return(list("lpFilename" = lpFilename, "parsedData" = parsedData, 
              "resultFile" = resultFile))
}

createSolverSpecificFiles <- function(carnivalOptions) {
  if( carnivalOptions$solver == supportedSolvers$cplex ) {
    cplexCommandFile <- paste0(outputFolder, "cplexCommand", "_", carnivalOptions$runId, ".txt")
    carnivalOptions$filenames <- c(carnivalOptions$filenames, "cplexCommandFile" = cplexCommandFile)
  }
  
  return(carnivalOptions$filenames)
}

#TODO Olga
trackTime <- function() {
  return(NULL)
}
 