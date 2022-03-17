## Providing functions for fixing special characters in node identifiers
##
## Enio Gjerga, Olga Ivanova 2020-2021


correctIdentifiers <- function(nodesIds, replacementSymbol = "_",
                               verbose = FALSE){
  
    nodesIds_new <- make.names(nodesIds)
    nodesIds_new <- gsub(pattern = ".",replacement = replacementSymbol,x = nodesIds_new,fixed = TRUE)
    
    changedIdsindx <- which(nodesIds!=nodesIds_new)
    
    if(length(changedIdsindx)>0){
        
        # check if 2 names ended up to be the same after change
        map_table <- data.frame(user.defined.id =  nodesIds[changedIdsindx],
                                mapped.id = nodesIds_new[changedIdsindx])
        map_table <- unique(map_table)
        if (verbose) {
            message("The following names changed in the network:")  
            print(map_table)
        }
        
        duplicated_corrected_nodeIds <- duplicated(map_table$mapped.id) | duplicated(map_table$mapped.id,fromLast = TRUE)
        
        if(any(duplicated_corrected_nodeIds)){
            message("The following node names maps to the same ID:")
            print(map_table[duplicated_corrected_nodeIds,])
            stop("Multiple nodes in the PKN map to the same name after correcting
                  their identifiers, check the table above and correct the names in the PKN manually (check ?make.names).")
        } 
    }  
  
  return(nodesIds_new) 
}

correctNodeIdentifiersInNetwork <- function( network, replacementSymbol = "_", 
                                             verbose = FALSE){
  
  network$source <- correctIdentifiers(nodesIds = network$source, replacementSymbol, 
                                       verbose)
  network$target <- correctIdentifiers(network$target, replacementSymbol, 
                                       verbose)
  
  return(network) 
}


