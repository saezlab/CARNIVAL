#' Checks prior knowledge network for correct format. 
#'
#' @param priorKnowledgeNetwork a network with 3 columns: 
#' source node ('source'), interaction 
#' sign ('interaction') and target node('target').
#'
#' @return TRUE if everything is correct. Stops pipeline if not.
#' @keywords internal 
#' 
#' @author Enio Gjerga, Olga Ivanova 2020-2021
checkPriorKnowledgeNetwork <- function(priorKnowledgeNetwork){
  
  incorrectDataTypeError <- "Network object should either be of matrix or 
                              data.frame class."
  incorrectColumnsInNetworkError <- "Network object should have 3 columns: 
                                      source node ('source'), interaction 
                                      sign ('interaction') and target node('target')."
  incorrectInteractionValue <- "Interactions column should contain either 1 or -1."
  
  if (!is.data.frame(priorKnowledgeNetwork)) {
    stop(incorrectDataTypeError)
  }
  
  isCorrectColumns <- all( c("source","interaction", "target") %in% 
                             names(priorKnowledgeNetwork))
  
  if (!isCorrectColumns || ncol(priorKnowledgeNetwork) != 3) {
    stop(incorrectColumnsInNetworkError)
  }
  
  isCorrectValues <- all(unique(priorKnowledgeNetwork$interaction) %in% c(-1, 1))
  if (!isCorrectValues) {
    stop(incorrectInteractionValue)
  }
  
  return(TRUE)
}

#' Preprocesses prior knowledge network: correct nodes identifiers for symbols 
#' that might break solvers runs, assigns the types for each column: 
#' Node1 (character), Sign (numeric), Node2 (character). 
#' Stops if interaction/sign column has non-numeric value 
#'
#' @inheritParams checkPriorKnowledgeNetwork
#'
#' @return preprocessed prior knowledge network with corrected nodes identifiers
#' add 3 columns: Node1, Sign, Node2
#' @keywords internal
#'
#' @author Enio Gjerga, Olga Ivanova 2020-2021
preprocessPriorKnowledgeNetwork <- function(priorKnowledgeNetwork) {
  
  priorKnowledgeNetwork <- correctNodeIdentifiersInNetwork(priorKnowledgeNetwork)
  priorKnowledgeNetwork$source <- as.character(priorKnowledgeNetwork$source)
  priorKnowledgeNetwork$target <- as.character(priorKnowledgeNetwork$target)
  priorKnowledgeNetwork$interaction <- as.numeric(priorKnowledgeNetwork$interaction)
  
  tryCatch({
    priorKnowledgeNetwork$interaction <- as.numeric(as.character(priorKnowledgeNetwork$interaction))  
  }, warning = function(w) {
    stop("Check the interaction column in your prior knowledge network: 
         contains non numeric values!")
  })

  #this renaming is used during export of results of each solver 
  #(see solvers_export_result.R) and in all constraints generation 
  colnames(priorKnowledgeNetwork) <- c("Node1", "Sign", "Node2")
  
  #N.B. Don't remove the line, it breaks cplex runs
  priorKnowledgeNetwork <- as.data.frame(priorKnowledgeNetwork)
  
  return(priorKnowledgeNetwork)           
}