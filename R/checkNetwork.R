#'\code{checkNetwork}
#'
#'@param netObj Network object
#'
#'@return Error message in case of errors in the inputs
#'
#'@export
#'
#'Enio Gjerga, 2020

checkNetwork <- function(netObj = netObj){
  
  ## allowedClass = c("matrix", "data.frame")
  if((!is(netObj, "matrix")) && (!is(netObj, "data.frame"))){
    stop("Network object should either be of matrix or data.frame class")
  } else {
    
    colnames(netObj) = c("source", "Interaction", "target")
    if("data.frame"%in%is(netObj)){
      netObj$source = as.character(netObj$source)
      netObj$Interaction = as.numeric(as.character(netObj$Interaction))
      netObj$target = as.character(netObj$target)
      
    } else {
      netObj = as.data.frame(netObj)
      netObj$source = as.character(netObj$source)
      netObj$Interaction = as.numeric(as.character(netObj$Interaction))
      netObj$target = as.character(netObj$target)
    }
    
    idx2rem = which(duplicated(netObj[, c(1, 3)]))
    if(length(idx2rem)>0){
      print("There are duplicated interactions in the network. Removing the 
            duplications..")
      netObj = netObj[-idx2rem, ]
    }
    
    if(ncol(netObj)!=3){
      stop("network object should have three columns: source node, interaction 
            sign and target node")
    } else {
      if(((!is(netObj$source, "character")) 
          || (!is(netObj$Interaction, "numeric")) 
          || (!is(netObj$target, "character")))){
        stop("Source and target node columns (1 & 3) should be of a 
              character type, while interaction column (2) should be of a
              numeric type")
      } else {
        if(!all(unique(netObj$Interaction)%in%c(-1, 1))){
          stop("Interactions column should contain either 1 or -1")
        }
      }
    }
  }
  
  netObj = controlNodeIdentifiers(netObj = netObj)
  
  return(netObj)
  
}