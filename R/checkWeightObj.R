## Returning error message in case of errors in the weights
##
## Enio Gjerga, 2020

checkWeightObj <- function(weightObj = weightObj, netObj = netObj){
  
  nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), 
                      as.character(as.matrix(netObj)[, 3])))
  
  if (is.null(weightObj)) {
    weightObj = "NULL"
  } else {
    ## allowedClass = c("matrix", "data.frame")
    if((!is(weightObj, "matrix")) && (!is(weightObj, "data.frame"))){
      stop("Weight object should either be of matrix or data.frame class")
    } else {
      if(ncol(weightObj)>0){
        
        colnames(weightObj) <- gsub(pattern = "-", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = "+", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = "*", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = "/", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = "<", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = ">", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = "=", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        colnames(weightObj) <- gsub(pattern = " ", replacement = "_", 
                                   x = colnames(weightObj), fixed = TRUE)
        
        ## now checking for allowed values
        absVals <- as.numeric(abs(weightObj[1, ]))
        if(any(absVals>1)){
          stop("Error on weightObj. Weights should be between -1 and 1.")
        }
        
        mSpecies = colnames(weightObj)
        
        idx = which(mSpecies%in%nSpecies)
        idx2rem = setdiff(1:length(mSpecies), idx)
        
        if(length(idx2rem)==length(mSpecies)){
          stop("Something wrong with your weight object/network object. 
               No weighted node is present in the network. 
               You can set the weightObj to NULL.")
        } else {
          if(length(idx2rem)>0){
            if((nrow(weightObj)==1) && (is(weightObj, "matrix"))){
              weightObj = weightObj[, -idx2rem]
              weightObj = t(as.matrix(weightObj))
            } else {
              weightObj = weightObj[, -idx2rem]
            }
          }
        }
    } else {
      stop("Something wrong with your weight object. Please check or set it to 
           NULL.")
    }
  }
  }
  
  return(weightObj)
}
