#TODO add docs
#TODO params
solveWithCbc <- function(variables=variables, carnivalOptions=carnivalOptions,
                         pknList=pknList, inputObj=inputObj, measurements=measurements) {
  
  resFile = paste0("results_cbc", ".txt")
  
  cbc_command <- paste0(carnivalOptions$solverPath, " testFile_", 1, "_", 
                        1, ".lp -seconds ", carnivalOptions$timelimit,
                        " -ratio ", carnivalOptions$poolrelGap, 
                        " solve printi csv solu ", resFile)
  
  system(cbc_command)
  
  res <- exportResultCBC(solutionFileName = resFile, 
                      variables = variables, 
                      conditionIDX = 1,
                      pknList = pknList, 
                      inputs = inputObj, 
                      measurements = measObj)
  
  if (!is.null(res)) {
    if(!is.null(dirName)){
      if(dir.exists(dirName)){
        WriteDOTfig(res=res,
                    dir_name = dirName,
                    inputs = inputObj,
                    measurements = measObj,
                    UP2GS = FALSE)
      } else {
        warning("Specified directory does not exist. DOT figure not saved.")
      }
    }
  } else {
    message("No result to be written")
    return(NULL)
  }
  
  return(res)
}