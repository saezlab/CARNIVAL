##Error message in case of errors in the inputs
##
##Enio Gjerga, 2020

checkNetwork <- function(netObj = netObj){
  
  if((!is(netObj, "matrix")) && (!is(netObj, "data.frame"))){
    stop("Network object should either be of matrix or data.frame class")
  } else {
    
    colnames(netObj) = c("source", "interaction", "target")
    if("data.frame"%in%is(netObj)){
      netObj$source = as.character(netObj$source)
      netObj$interaction = as.numeric(as.character(netObj$interaction))
      netObj$target = as.character(netObj$target)
      
    } else {
      netObj = as.data.frame(netObj)
      netObj$source = as.character(netObj$source)
      netObj$interaction = as.numeric(as.character(netObj$interaction))
      netObj$target = as.character(netObj$target)
    }
    
    if(ncol(netObj)!=3){
      stop("Network object should have three columns: source node, interaction 
            sign and target node")
    } else {
      if(((!is(netObj$source, "character")) 
          || (!is(netObj$interaction, "numeric")) 
          || (!is(netObj$target, "character")))){
        stop("Source and target node columns (1 & 3) should be of a 
              character type, while interaction column (2) should be of a
              numeric type")
      } else {
        if(!all(unique(netObj$interaction)%in%c(-1, 1))){
          stop("Interactions column should contain either 1 or -1")
        }
      }
    }
  }
  
  netObj = controlNodeIdentifiers(netObj = netObj)
  
  return(netObj)
  
}