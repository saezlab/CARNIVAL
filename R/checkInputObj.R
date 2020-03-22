## Returning error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkInputObj <- function(inputObj = inputObj, netObj = netObj){
  
  nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), 
                      as.character(as.matrix(netObj)[, 3])))
  
  returnList = list()
  
  if(is.null(inputObj)){
    print("inputObj set to NULL -- running InvCARNIVAL")
    MappedPertNode <- AddPerturbationNode(network = as.matrix(netObj))
    returnList = MappedPertNode
  } else {
    if(ncol(inputObj)>0){
      
      colnames(inputObj) <- gsub(pattern = "-", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = "+", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = "*", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = "/", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = "<", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = ">", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = "=", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      colnames(inputObj) <- gsub(pattern = " ", replacement = "_", 
                                x = colnames(inputObj), fixed = TRUE)
      
      mSpecies = colnames(inputObj)
    } else {
      stop("Something wrong with your measurements object. Please check.")
    }
    
    idx = which(mSpecies%in%nSpecies)
    idx2rem = setdiff(1:length(mSpecies), idx)
    
    if(length(idx2rem)==length(mSpecies)){
      stop("Something wrong with your measurements object/network object. 
           No input is present in the network")
    } else {
      if(length(idx2rem)>0){
        if((nrow(inputObj)==1) && (is(inputObj, "matrix"))){
          inputObj = inputObj[, -idx2rem]
          inputObj = t(as.matrix(inputObj))
        } else {
          inputObj = inputObj[, -idx2rem]
        }
      }
    }
    returnList[[length(returnList)+1]] = inputObj
    returnList[[length(returnList)+1]] = netObj
    names(returnList) = c("inputs", "network")
  }
  
  return(returnList)
  
}