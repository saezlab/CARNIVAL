## Returning error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkMeasObj <- function(measObj = measObj, netObj = netObj){
  
  nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), 
                      as.character(as.matrix(netObj)[, 3])))
  
  if (is.null(measObj)) {
    stop("Please provide a valid measurement object.")
  } else {
    ## allowedClass = c("matrix", "data.frame")
    if((!is(measObj, "matrix")) && (!is(measObj, "data.frame"))){
      stop("Measurement object should either be of matrix or data.frame class")
    } else {
      if(ncol(measObj)>0){
        mSpecies = colnames(measObj)
        
        idx = which(mSpecies%in%nSpecies)
        idx2rem = setdiff(1:length(mSpecies), idx)
        
        if(length(idx2rem)==length(mSpecies)){
          stop("Something wrong with your measurements object/network object. 
               No measurements is present in the network")
        } else {
          if(length(idx2rem)>0){
            if((nrow(measObj)==1) && (is(measObj, "matrix"))){
              measObj = measObj[, -idx2rem]
              measObj = t(as.matrix(measObj))
            } else {
              measObj = measObj[, -idx2rem]
            }
          }
        }
      } else {
        stop("Something wrong with your measurements object. Please check.")
      }
    }
  }
  
  colnames(measObj) <- gsub(pattern = "-", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = "+", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = "*", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = "/", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = "<", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = ">", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = "=", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  colnames(measObj) <- gsub(pattern = " ", replacement = "_", 
                            x = colnames(measObj), fixed = TRUE)
  
  return(measObj)
  
}