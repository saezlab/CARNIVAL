collectMetaInfo <- function(carnivalOptions) {
  runId <- createRunId()
  carnivalOptions$runId <- runId

  filenames <- createFilenames(carnivalOptions)
  carnivalOptions$filenames <- filenames
  carnivalOptions$startTime <- getTime()

  return(carnivalOptions)
}

createRunId <- function() {
  datetime <- format(Sys.time(), "t%H_%M_%Sd%d_%m_%Y")
  salt <- sample(1:100, 1)
  runId <- paste(datetime, salt, sep="n")

  return(runId)
}

createFilenames <- function(carnivalOptions) {
  #TODO add windows support with .Platform
  if ( is.null(carnivalOptions$outputFolder) ) {
    outputFolder <- file.path(getwd(), "")
  } else if ( carnivalOptions$outputFolder == "" ){
    outputFolder <- file.path(getwd(), "")
  } else {
    outputFolder <- file.path(carnivalOptions$outputFolder, "")  
  }
  
  lpFilename <- file.path(outputFolder, paste0("lpFile", "_", carnivalOptions$runId, ".lp"))
  parsedData <- file.path(outputFolder, paste0("parsedData_", carnivalOptions$runId, ".RData"))
  resultFile <- file.path(outputFolder, paste0("result", "_", carnivalOptions$runId, ".txt"))

  filenames <- list("lpFilename" = lpFilename, "parsedData" = parsedData,
                    "resultFile" = resultFile)

  if(carnivalOptions$solver == getSupportedSolvers()$cplex) {
    filenames <- createSolverSpecificFiles(carnivalOptions, filenames)
  }

  return(filenames)
}

createSolverSpecificFiles <- function(carnivalOptions, filenames) {
  outputFolder <- carnivalOptions$outputFolder
  if( carnivalOptions$solver == getSupportedSolvers()$cplex ) {
    cplexCommandFile <- file.path(outputFolder, paste0("cplexCommand", "_", carnivalOptions$runId, ".txt"))
    cplexLog <- file.path(outputFolder, paste0("cplexLog", "_", carnivalOptions$runId, ".txt"))
    filenames <- c(filenames, "cplexCommandFile" = cplexCommandFile,
                              "cplexLog" = cplexLog)
  }

  return(filenames)
}

getTime <- function() {
  time <- format(Sys.time(), "%H:%M:%S %d.%m.%Y")
  return(time)
}