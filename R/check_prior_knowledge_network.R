##Error message in case of errors in the inputs
##
##Enio Gjerga, 2020

checkPriorKnowledgeNetwork <- function(priorKnowledgeNetwork = priorKnowledgeNetwork){
  
  incorrectDataTypeError <- "Network object should either be of matrix or data.frame class"
  incorrectColumnsInNetworkError <- "Network object should have three columns: source node ('source'), interaction 
                                      sign ('interaction') and target node('target')"
  
  
  stopifnot(incorrectDataTypeError = is.data.frame(priorKnowledgeNetwork))
  stopifnot(incorrectColumnsInNetworkError = all( c("source","interaction", "target") %in% names(priorKnowledgeNetwork)))
  stopifnot(ncol(priorKnowledgeNetwork) == 3)
  
  return(TRUE)
}

preprocessPriorKnowledgeNetwork <- function(priorKnowledgeNetwork = priorKnowledgeNetwork) {
  incorrectInteractionValue <- "Interactions column should contain either 1 or -1"
  
  priorKnowledgeNetwork <- correctNodeIdentifiersInNetwork(network = priorKnowledgeNetwork)
  priorKnowledgeNetwork$source = as.character(priorKnowledgeNetwork$source)
  priorKnowledgeNetwork$target = as.character(priorKnowledgeNetwork$target)
  
  tryCatch({
    priorKnowledgeNetwork$interaction = as.numeric(as.character(priorKnowledgeNetwork$interaction))  
  }, warning = function(w) {
    stop("Check the interaction column in your prior knowledge network: contains non numeric values!")
  })
  
  stopifnot( incorrectInteractionValue = all(unique(priorKnowledgeNetwork$interaction) %in% c(-1, 1)) )  
  
  #this renaming is used during export of results of each solver (see export_result.R)
  colnames(priorKnowledgeNetwork) <- c("Node1", "Sign", "Node2")
 
  return(priorKnowledgeNetwork)           
}
