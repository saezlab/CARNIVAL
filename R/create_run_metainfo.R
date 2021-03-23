createRunId <- function() {
  datetime <- format(Sys.time(), "t%H_%M_%Sd%d_%m_%Y")
  salt <- sample(1:100, 1)
  runId <- paste(datetime, salt, sep="n")
  return(runId)
}

trackTime <- function() {
  return(NULL)
}

createFilenames <- function() {
  
}